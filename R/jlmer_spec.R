#' Create a specifications object for fitting regression models in Julia
#'
#' @param formula Model formula in R syntax
#' @param data A data frame
#' @param subject Column for subjects in the data.
#' @param trial Column for trials in the data. Must uniquely identify a time series within subject
#'  (for example, the column for items in a counterbalanced design where each subject sees exactly one item).
#' @param time Column for time in the data.
#' @param drop_terms (Optional) any terms to drop from the reconstructed model formula
#' @param ... Unused, for extensibility.
#'
#' @srrstats {G2.5} Inputs are never strictly expected as factor, but users are expected to be aware of the concept of contrast coding,
#'  such that they can further control the numerical coding of their categorical variables with `contrasts()`.
#' @srrstats {G2.7} Package accepts any tabular structure that is `model.matrix()`-able.
#' @srrstats {G2.8} Package applies a strict gateway via `make_jlmer_spec()` where all CPA-related functions require a pre-processed `<jlmer_spec>` object.
#' @srrstats {G2.9} Messages dropped rows and invalid names on `model.matrix()` conversion.
#' @srrstats {G2.10} Handled via `model.matrix()`
#' @srrstats {G2.11} Handled via `model.matrix()`
#' @srrstats {G2.12} Handled via `model.matrix()`
#' @srrstats {G2.13} Handled via `model.matrix()` and informs on dropping NA rows.
#' @srrstats {G2.14} `make_jlmer_spec()` informs about NA rows. Imputation not implemented as it does not make sense for this package.
#' @srrstats {G5.8} Edge conditions on data are caught firstly by `model.matrix()` (inside `make_jlmer_spec()`) and also in Julia when handing the data off to GLM/MixedModels
#' @srrstats {RE1.1} Handled standardly via `model.matrix()`
#' @srrstats {RE1.3} Handled standardly via `model.matrix()`. Users can see the model matrix with contrasts spelled-out upon printing a `<jlmer_spec>` object
#'
#' @examples
#' # Bare specification object (minimal spec for fitting a global model)
#' spec <- make_jlmer_spec(weight ~ 1 + Diet, ChickWeight)
#' spec
#'
#' # Constraints on specification for CPA:
#' # 1) The combination of `subject`, `trial`, and `time` must uniquely identify rows in the data
#' # 2) `time` must have constant sampling rate (i.e., evenly spaced values)
#' spec_wrong <- make_jlmer_spec(
#'   weight ~ 1 + Diet, ChickWeight,
#'   time = "Time"
#' )
#' unique(ChickWeight$Time)
#'
#' # Corrected specification for the above
#' spec_correct <- make_jlmer_spec(
#'   weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
#'   subject = "Chick", time = "Time"
#' )
#' spec_correct
#'
#' @return An object of class `jlmer_spec`.
#' @export
make_jlmer_spec <- function(formula, data, subject = NULL, trial = NULL, time = NULL, drop_terms = NULL, ...) {
  # Old names
  fm <- formula
  df <- data

  # Validation
  check_arg_class(formula, "formula")
  check_arg_class(data, "data.frame", "data")
  special_cols <- c(subject, trial, time)
  # Validate grouping structure
  if (!is.null(special_cols)) {
    if (!all(special_cols %in% colnames(data))) {
      cli::cli_abort("Column{?s} {.val {special_cols[!special_cols %in% colnames(data)]}} not found in {.arg data}")
    }
    if (nrow(data) != nrow(unique(data[, special_cols, drop = FALSE]))) {
      cli::cli_alert_warning(c(
        "Grouping column{?s} {.val {special_cols}} do{?es/} not uniquely identify rows in the data"
      ))
    }
  }
  if (!is.null(time)) {
    time_diffs <- diff(sort(unique(data[[time]])))
    if (!all(time_diffs == time_diffs[1])) {
      cli::cli_alert_warning(c(
        "Sampling rate for the {.arg time} column {.val {time}} is not constant - ",
        "may affect interpretability of results."
      ))
    }
  }

  # Formula reconstruction
  fm_env <- attr(fm, ".Environment")
  fm_response <- deparse1(fm[[2]])
  fm_terms <- stats::terms(lme4::subbars(fm), keep.order = TRUE)
  attr(fm_terms, "intercept") <- as.logical(attr(stats::terms(lme4::nobars(fm)), "intercept"))
  fm_cols <- vapply(as.list(attr(fm_terms, "variables")[-1]), deparse1, character(1))
  df_subset <- df[, fm_cols, drop = FALSE]
  na_rows <- !stats::complete.cases(df_subset)
  if (any(na_rows)) {
    df_subset <- df_subset[!na_rows, ]
    cli::cli_alert_warning("Dropping {.val {sum(na_rows)}} row{?s} with missing values.")
  }

  re <- lme4::findbars(fm)
  has_re <- !is.null(re)
  if (has_re) {
    lfm <- lme4::lFormula(fm, df)
    fe_term_labels <- attr(stats::terms(lme4::nobars(fm)), "term.labels")
    model_matrix <- lfm$X
  } else {
    fe_term_labels <- attr(fm_terms, "term.labels")
    model_matrix <- stats::model.matrix(fm_terms, df_subset)
  }

  terms_compact <- fe_term_labels
  terms_expanded <- colnames(model_matrix)
  terms_grouping <- setNames(attr(model_matrix, "assign"), colnames(model_matrix))
  has_intercept <- 0 %in% terms_grouping
  if (has_intercept) {
    terms_compact <- c(1, terms_compact)
  }
  terms_dict <- split(names(terms_grouping), terms_grouping)
  names(terms_dict) <- terms_compact

  # Renaming
  # split_fct_term <- function(x, y) {
  #   ifelse(x == y, x, vapply(x, gsub, character(1), pattern = paste0(y, ""), replacement = paste0(y, "."), fixed = TRUE))
  # }
  renamed_terms_dict <- lapply(seq_along(terms_dict), function(i) {
    nm <- names(terms_dict)[i]
    x <- terms_dict[[i]]
    if (grepl(":", nm)) {
      renamed <- vapply(x, gsub, character(1), pattern = ":", replacement = "__", fixed = TRUE)
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
    out <- unname(renamed)
    out[!out %in% drop_terms]
  })
  names(renamed_terms_dict) <- names(terms_dict)
  colnames(model_matrix) <- gsub(":", "__", colnames(model_matrix), fixed = TRUE)
  model_matrix_df <- as.data.frame(model_matrix)[setdiff(colnames(model_matrix), "(Intercept)")]
  model_matrix_df <- cbind(df_subset[, fm_response, drop = FALSE], model_matrix_df)

  if (!has_re) {
    fe_terms_renamed <- unlist(renamed_terms_dict, recursive = TRUE, use.names = FALSE)
    fe_terms_renamed <- fe_terms_renamed[fe_terms_renamed != "(Intercept)"]
    if (!is.null(drop_terms)) fe_terms_renamed <- fe_terms_renamed[!fe_terms_renamed %in% drop_terms]
    fe_terms_renamed <- as.character(c(as.integer(has_intercept), fe_terms_renamed))
    fe_fm <- stats::reformulate(fe_terms_renamed, fm_response, env = fm_env)
    r_fm <- jl_fm <- fe_fm
    re_groups <- NULL
  } else {
    fe <- lme4::nobars(fm)
    fe_expanded <- stats::terms(fe, keep.order = TRUE)
    fe_terms <- attr(fe_expanded, "term.labels")
    renamed_fe_terms_dict <- renamed_terms_dict[fe_terms]
    fe_terms_renamed <- unlist(renamed_fe_terms_dict, use.names = FALSE)
    if (!is.null(drop_terms)) fe_terms_renamed <- fe_terms_renamed[!fe_terms_renamed %in% drop_terms]
    fe_terms_renamed <- as.character(c(as.integer(has_intercept), fe_terms_renamed))
    fe_fm <- stats::reformulate(fe_terms_renamed)[[2]]
    re_terms_renamed <- lapply(re, function(x) {
      terms <- stats::terms.formula(call("~", x[[2]]))
      renamed <- unlist(renamed_terms_dict[attr(terms, "term.labels")], use.names = FALSE)
      if ("0" %in% fe_terms_renamed) renamed <- renamed[renamed != "1"]
      if (!is.null(drop_terms)) renamed <- renamed[!renamed %in% drop_terms]
      unique(c(attr(terms, "intercept"), renamed))
    })
    re_rhs_deparsed <- vapply(re, function(x) deparse1(x[[3]]), character(1))
    re_terms_regrouped <- lapply(split(re_terms_renamed, re_rhs_deparsed), unlist, use.names = FALSE)
    re_bars <- ifelse(table(re_rhs_deparsed) > 1, "||", "|")
    re_groups <- lapply(names(re_terms_regrouped), function(x) {
      bar <- re_bars[[x]]
      lhs <- re_terms_regrouped[[x]]
      lhs <- gsub(":", "__", lhs, fixed = TRUE)
      if (all(c("1", "0") %in% lhs)) lhs <- lhs[lhs != "0"]
      lhs <- stats::reformulate(lhs)[[2]]
      call(bar, lhs, as.symbol(x))
    })
    re_fm_r <- lapply(re_groups, function(x) call("(", x))
    re_fm_jl <- lapply(re_groups, function(x) {
      if (identical(x[[1]], quote(`||`))) {
        x[[1]] <- quote(`|`)
        call("zerocorr", x)
      } else {
        call("(", x)
      }
    })
    combine_fm <- function(re_str) {
      expanded <- Reduce(function(x, y) call("+", x, y), c(fe_fm, re_str))
      stats::as.formula(call("~", as.symbol(fm_response), expanded), fm_env)
    }
    re_groups <- names(re_bars)
    r_fm <- combine_fm(re_fm_r)
    jl_fm <- combine_fm(re_fm_jl)
    re_cols <- as.data.frame.list(lapply(df[!na_rows, names(re_bars), drop = FALSE], as.character))
    model_matrix_df <- cbind(model_matrix_df, re_cols)
  }

  cols_keep <- c(subject, trial, time)

  model_matrix_df <- cbind(
    model_matrix_df,
    df[!na_rows, setdiff(cols_keep, colnames(model_matrix_df)), drop = FALSE]
  )

  model_matrix_df <- maybe_as_tibble(model_matrix_df)

  out <- list(
    formula = list(r = r_fm, jl = jl_fm, original = fm),
    data = model_matrix_df,
    meta = list(
      term_groups = renamed_terms_dict, re_groups = re_groups,
      subject = subject, trial = trial, time = time,
      is_mem = has_re
    )
  )

  structure(out, class = "jlmer_spec")
}

#' @export
print.jlmer_spec <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
format.jlmer_spec <- function(x, ...) {
  cli::cli_format_method(
    {
      cli::cli_rule(left = "{.strong jlmer specification}", right = "{.cls jlmer_spec}")
      # Formula
      cli::cli_text("{.el Formula}: {.fm {deparse1(x$formula$jl)}}")
      # Terms
      cli::cli_text("{.el Predictors}:")
      terms <- Filter(function(term) !identical(term, "(Intercept)"), x$meta$term_groups)
      cli::cli_ul()
      cli::cli_dl(lapply(terms, paste, collapse = ", "), paste0("{.emph ", names(terms), "}"))
      cli::cli_end()
      # Grouping
      groupings <- x$meta[c("subject", "trial", "time")]
      if (!is.null(unlist(groupings))) {
        cli::cli_text("{.el Groupings}:")
        cli::cli_ul()
        cli::cli_dl(groupings, paste0("{.emph ", c("Subject", "Trial", "Time"), "}"))
        cli::cli_end()
      }
      # Data
      cli::cli_text("{.el Data}:")
      if (inherits(x$data, "tbl_df")) {
        old_pillar.advice <- options(pillar.advice = FALSE)
        print(x$data, n = 3)
        options(old_pillar.advice)
      } else {
        print(x$data, max = 3 * ncol(x$data))
      }
      cli::cli_rule()
    },
    theme = .jlmerclusterperm$cli_theme
  )
}

# update_spec_data <- function(jlmer_spec, data, reform = FALSE) {
#   .jlmer_spec <- jlmer_spec
#   if (is.function(data)) {
#     .data <- data(jlmer_spec$data)
#   } else {
#     .data <- data
#   }
#   used_cols <- get_spec_used_cols(jlmer_spec)
#   if (!all(used_cols %in% colnames(.data))) {
#     missing_cols <- used_cols[!used_cols %in% colnames(.data)]
#     cli::cli_abort("Necessary column{?s} {.val {missing_cols}} missing from {.arg data}")
#   }
#   .jlmer_spec$data <- .data
#   .jlmer_spec
# }
#
# get_spec_used_cols <- function(jlmer_spec) {
#   all_cols <- unlist(c(
#     jlmer_spec$meta$term_groups,
#     jlmer_spec$meta$re_groups,
#     jlmer_spec$meta[c("subject", "trial", "time")]
#   ), use.names = FALSE)
#   all_cols[all_cols != "(Intercept)"]
# }
