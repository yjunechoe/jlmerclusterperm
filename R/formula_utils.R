#' Construct a model matrix and expand model formula suitable for lme4 and MixedModels
#'
#' @param fm Formula
#' @param df Dataframe
#' @param drop_terms A character vector of terms to drop from the model matrix
#' @param zcr Whether to force zero correlation in the random effects structure. Defaults to `TRUE`.
#'
#' @return A list of R formula for lme4, Julia formula for MixedModels, and the model matrix as a data frame
#'
#' @export
jlmer_model_matrix <- function(fm, df, drop_terms = NULL, zcr = TRUE) {
  fm_env <- attr(fm, ".Environment")
  fm <- stats::as.formula(gsub("\\|\\|", "\\|", deparse1(fm)))
  response <- fm[[2]]
  lfm <- lme4::lFormula(fm, df)
  model_matrix <- lfm$X
  fe <- colnames(model_matrix)
  re <- lfm$reTrms$cnms
  if (!is.null(drop_terms)) {
    drop_terms_regex <- paste0("(^|:)(", paste(drop_terms, collapse = "|"), ")(:|$)")
    model_matrix <- model_matrix[,!grepl(drop_terms_regex, fe), drop = FALSE]
  }
  reconstruct_fm <- function(fm_terms) {
    has_intercept <- "(Intercept)" %in% fm_terms
    if (has_intercept) fm_terms <- fm_terms[fm_terms != "(Intercept)"]
    if (!is.null(drop_terms)) {
      fm_terms <- fm_terms[!grepl(drop_terms_regex, fm_terms)]
    }
    fm_terms <- gsub(":", "__", fm_terms, fixed = TRUE)
    stats::reformulate(c(as.double(has_intercept), fm_terms))[[2]]
  }
  fe_fm <- reconstruct_fm(fe)
  re_fm <- lapply(re, reconstruct_fm)
  re_fm_r <- lapply(seq_along(re_fm), function(i) {
    call("(", call(ifelse(lengths(re_fm)[i] == 1, "|", "||"), re_fm[[i]], as.symbol(names(re_fm)[i])))
  })
  re_fm_jl <- lapply(seq_along(re_fm), function(i) {
    zcr <- call("|", re_fm[[i]], as.symbol(names(re_fm)[i]))
    if (lengths(re_fm)[i] > 1) { zcr <- call("zerocorr", zcr) }
    zcr
  })
  combine_fm <- function(re_str) {
    expanded <- Reduce(function(x, y) call("+", x, y), c(fe_fm, re_str))
    expanded_fm <- eval(call("~", response, expanded))
    attr(expanded_fm, ".Environment") <- fm_env
    expanded_fm
  }
  model_matrix_df <- as.data.frame(model_matrix)[setdiff(colnames(model_matrix), "(Intercept)")]
  colnames(model_matrix_df) <- gsub(":", "__", colnames(model_matrix_df), fixed = TRUE)
  list(
    formula = combine_fm(re_fm_r),
    julia_formula = combine_fm(re_fm_jl),
    data = cbind(
      lfm$fr[deparse1(response)],
      model_matrix_df,
      as.data.frame(lfm$reTrms$flist)
    )
  )
}
