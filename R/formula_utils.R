#' Construct a model matrix and expand model formula suitable for lme4 and MixedModels
#'
#' @param fm Formula
#' @param df Dataframe
#' @param cols_keep A character vector of additional columns to keep from `df` or
#'  `FALSE` to drop all and `TRUE` to drop none. Defaults to `FALSE`.
#' @param drop_terms A character vector of terms to drop from the model formula.
#'
#' @return A list of R formula for lme4, Julia formula for MixedModels, and the model matrix as a data frame
#'
#' @export
jlmer_model_matrix <- function(fm, df, cols_keep = FALSE, drop_terms = NULL) {
  fm_env <- attr(fm, ".Environment")
  response <- fm[[2]]
  lfm <- lme4::lFormula(fm, df)
  model_matrix <- lfm$X
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

  re <- lfm$reTrms$cnms
  re_bars <- ifelse(table(sapply(lme4::findbars(fm), function(x) deparse1(x[[3]]))) > 1, "||", "|")
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
    expanded_fm <- eval(call("~", response, expanded))
    attr(expanded_fm, ".Environment") <- fm_env
    expanded_fm
  }
  model_matrix_df <- as.data.frame(model_matrix)[setdiff(colnames(model_matrix), "(Intercept)")]
  colnames(model_matrix_df) <- gsub(":", "__", colnames(model_matrix_df), fixed = TRUE)
  model_matrix_df <- cbind(lfm$fr[deparse1(response)], model_matrix_df, lfm$reTrms$flist)
  if (!isFALSE(cols_keep)) {
    if (isTRUE(cols_keep)) cols_keep <- setdiff(colnames(df), colnames(model_matrix_df))
    model_matrix_df <- cbind(model_matrix_df, df[!is.na(df[[response]]), cols_keep, drop = FALSE])
  }
  list(
    formula = combine_fm(re_fm_r),
    julia_formula = combine_fm(re_fm_jl),
    data = model_matrix_df
  )
}
