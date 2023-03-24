#' Construct a model matrix and expand model formula suitable for lme4 and MixedModels
#'
#' @param fm Formula
#' @param df Dataframe
#' @param drop_terms A character vector of individual terms to drop from the reformulated model formula.
#'
#' @return A list of R formula for lme4, Julia formula for MixedModels, and the model matrix as a data frame
#'
#' @export
jlmer_model_matrix <- function(fm, df, drop_terms = NULL) {
  fm_env <- attr(fm, ".Environment")
  response <- fm[[2]]
  bars <- lme4::findbars(fm)
  has_re <- !is.null(bars)
  if (has_re) {
    lfm <- lme4::lFormula(fm, df)
    model_matrix <- lfm$X
  } else {
    model_matrix <- model.matrix(fm, df)
  }
  colnames(model_matrix) <- gsub(":", "__", colnames(model_matrix), fixed = TRUE)

  fe <- colnames(model_matrix)
  if (!is.null(drop_terms)) {
    model_matrix <- model_matrix[, !fe %in% drop_terms, drop = FALSE]
  }
  reconstruct_fm <- function(fm_terms) {
    has_intercept <- "(Intercept)" %in% fm_terms
    if (has_intercept) fm_terms <- fm_terms[fm_terms != "(Intercept)"]
    if (!is.null(drop_terms)) fm_terms <- fm_terms[!fm_terms %in% drop_terms]
    stats::reformulate(c(as.double(has_intercept), fm_terms))[[2]]
  }
  fe_fm <- reconstruct_fm(fe)
  model_matrix_df <- as.data.frame(model_matrix)[setdiff(colnames(model_matrix), "(Intercept)")]

  if (has_re) {
    re <- lfm$reTrms$cnms
    re_bars <- ifelse(table(sapply(bars, function(x) deparse1(x[[3]]))) > 1, "||", "|")
    re_groups <- lapply(seq_along(re), function(i) {
      rhs <- names(re)[i]
      bar <- re_bars[[rhs]]
      lhs <- re[[i]]
      lhs <- replace(lhs, lhs == "(Intercept)", "1")
      lhs <- gsub(":", "__", lhs, fixed = TRUE)
      lhs <- lhs[!lhs %in% drop_terms]
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
      as.formula(call("~", response, expanded), fm_env)
    }
    r_fm <- combine_fm(re_fm_r)
    jl_fm <- combine_fm(re_fm_jl)
    model_matrix_df <- cbind(lfm$fr[deparse1(response)], model_matrix_df, lfm$reTrms$flist)
  } else {
    r_fm <- jl_fm <- as.formula(call("~", response, fe_fm), fm_env)
  }

  model_matrix_df <- cbind(
    model_matrix_df,
    df[!is.na(df[[response]]), setdiff(colnames(df), colnames(model_matrix_df)), drop = FALSE]
  )

  if ("tibble" %in% rownames(installed.packages())) {
    model_matrix_df <- asNamespace("tibble")$as_tibble(model_matrix_df)
  }

  list(
    formula = r_fm,
    julia_formula = jl_fm,
    data = model_matrix_df
  )
}
