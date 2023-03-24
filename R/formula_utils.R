#' Construct a model matrix and expand model formula suitable for lme4 and MixedModels
#'
#' @param fm Formula
#' @param df Dataframe
#'
#' @return A list of R formula for lme4, Julia formula for MixedModels, and the model matrix as a data frame
#'
#' @export
jlmer_model_matrix <- function(fm, df, drop_terms = NULL) {

  # TODO: look into model.matrix(lme4::subbars(fm), df)
  # TODO: Do complete.cases() over response and predictor cols
  # TODO: Deal with categorical variables separately?

  fm_env <- attr(fm, ".Environment")
  fm_response <- deparse1(fm[[2]])
  fm_terms <- terms.formula(lme4::subbars(fm), keep.order = TRUE)
  attr(fm_terms, "intercept") <- as.logical(attr(terms(lme4::nobars(fm)), "intercept"))
  fm_cols <- vapply(as.list(attr(fm_terms, "variables")[-1]), deparse1, character(1))
  df_subset <- df[,fm_cols]
  na_rows <- !complete.cases(df_subset)
  if (any(na_rows)) {
    df_subset <- df_subset[!na_rows,]
    warning("Dropping ", na_rows, "row(s) with missing values.")
  }
  model_matrix <- model.matrix(fm_terms, df_subset)

  terms_compact <- attr(fm_terms, "term.labels")
  terms_expanded <- colnames(model_matrix)
  terms_grouping <- setNames(attr(model_matrix, "assign"), colnames(model_matrix))
  has_intercept <- 0 %in% terms_grouping
  terms_grouping <- terms_grouping[terms_grouping != 0]
  terms_dict <- split(names(terms_grouping), terms_grouping)
  names(terms_dict) <- terms_compact

  # Renaming
  # cross reference fm_cols
  renamed_terms_dict <- lapply(seq_along(terms_dict), function(i) {
    nm <- names(terms_dict)[i]
    x <- terms_dict[[i]]
    if (grepl(":", nm)) {
      renamed <- standardize_interaction_term(x)
      # if (length(x) > 1) {
      #   nm_split <- strsplit(nm, ":", fixed = TRUE)[[1]]
      #   renamed_split <- sapply(renamed, function(x) strsplit(x, "__", fixed = TRUE)[[1]])
      #   renamed_terms_split <- sapply(seq_along(nm_split), function(j) {
      #     split_fct_term(renamed_split[j,], nm_split[j])
      #   })
      #   renamed <- apply(renamed_terms_split, 1, paste0, collapse = "__")
      # }
    } else if (length(x) > 1) {
      renamed <- x
      # renamed <- split_fct_term(x, nm)
    } else {
      renamed <- x
    }
    unname(renamed)
  })
  names(renamed_terms_dict) <- names(terms_dict)
  colnames(model_matrix) <- gsub(":", "__", colnames(model_matrix), fixed = TRUE)
  model_matrix_df <- as.data.frame(model_matrix)[setdiff(colnames(model_matrix), "(Intercept)")]

  re <- lme4::findbars(fm)
  has_re <- !is.null(re)
  if (!has_re) {
    fe_terms_renamed <- unlist(renamed_terms_dict, recursive = TRUE, use.names = FALSE)
    if (!is.null(drop_terms)) fe_terms_renamed[!fe_terms_renamed %in% drop_terms]
    fe_terms_renamed <- c(as.integer(has_intercept), fe_terms_renamed)
    fe_fm <- reformulate(fe_terms_renamed, fm_response, env = fm_env)
    r_fm <- jl_fm <- fe_fm
  } else {
    fe <- lme4::nobars(fm)
    fe_expanded <- terms(fe, keep.order = TRUE)
    fe_terms <- attr(fe_expanded, "term.labels")
    fe_terms_renamed <- unlist(renamed_terms_dict[fe_terms], use.names = FALSE)
    if (!is.null(drop_terms)) fe_terms_renamed[!fe_terms_renamed %in% drop_terms]
    fe_terms_renamed <- c(as.integer(has_intercept), fe_terms_renamed)
    fe_fm <- reformulate(fe_terms_renamed)[[2]]
    lfm <- lme4::lFormula(fm, df)
    re_terms_raw <- lapply(lfm$reTrms$cnms, function(x) {
      if ("(Intercept)" %in% x) {
        replace(x, x == "(Intercept)", 1)
      } else {
        c(0, x)
      }
    })
    re_terms <- with(stack(re_terms_raw), split(values, ind))
    re_terms_renamed <- lapply(re_terms, function(x) {
      renamed <- standardize_interaction_term(x)
      renamed <- renamed[renamed %in% c("0", "1", fe_terms_renamed)]
      if ("0" %in% fe_terms_renamed) renamed <- renamed[renamed != "1"]
      if (all(c("1", "0") %in% renamed)) renamed <- renamed[renamed != "0"]
      if (!is.null(drop_terms)) renamed <- renamed[!renamed %in% drop_terms]
      unique(renamed)
    })
    re_bars <- ifelse(table(sapply(re, function(x) deparse1(x[[3]]))) > 1, "||", "|")
    re_groups <- lapply(seq_along(re_terms_renamed), function(i) {
      rhs <- names(re_terms)[i]
      bar <- re_bars[[rhs]]
      lhs <- re_terms_renamed[[i]]
      lhs <- gsub(":", "__", lhs, fixed = TRUE)
      lhs <- reformulate(lhs)[[2]]
      call(bar, lhs, as.symbol(rhs))
    })

    re_fm_r <- lapply(re_groups, function(x) call("(", x))
    re_fm_jl <- lapply(re_groups, function(x) {
      if (identical(x[[1]], quote(`|`))) {
        call("(", x)
      } else {
        x[[1]] <- quote(`|`)
        call("zerocorr", x)
      }
    })
    combine_fm <- function(re_str) {
      expanded <- Reduce(function(x, y) call("+", x, y), c(fe_fm, re_str))
      as.formula(call("~", as.symbol(fm_response), expanded), fm_env)
    }
    r_fm <- combine_fm(re_fm_r)
    jl_fm <- combine_fm(re_fm_jl)
  }

  model_matrix_df <- cbind(
    model_matrix_df,
    df[!na_rows, setdiff(colnames(df), colnames(model_matrix_df)), drop = FALSE]
  )

  if ("tibble" %in% rownames(installed.packages())) {
    model_matrix_df <- asNamespace("tibble")$as_tibble(model_matrix_df)
  }

  groupings <- unname(renamed_terms_dict[lengths(renamed_terms_dict) > 1])
  jl_fm <- structure(jl_fm, class = c("jl_formula", class(jl_fm)), groupings = groupings)

  list(
    formula = r_fm,
    jl_formula = jl_fm,
    data = model_matrix_df
  )
}

standardize_interaction_term <- function(x) {
  vapply(x, gsub, character(1), pattern = ":", replacement = "__", fixed = TRUE)
}

split_fct_term <- function(x, y) {
  ifelse(x == y, x, vapply(x, gsub, character(1), pattern = paste0(y, ""), replacement = paste0(y, "."), fixed = TRUE))
}

print.jl_formula <- function(x, ...) {
  attributes(x) <- NULL
  print(x)
}
