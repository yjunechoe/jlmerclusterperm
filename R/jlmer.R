#' Fit a Julia mixed model
#'
#' @param julia_formula A formula in MixedModels.jl syntax
#' @param data DataFrame
#' @param family A GLM family. Currently supports "gaussian" and "binomial".
#' @param ... Optional arguments to fit (`fast`, `nAGQ`, etc.)
#'
#' @seealso jlmer_model_matrix
#'
#' @export
jlmer <- function(julia_formula, data, family = c("gaussian", "binomial"), ...) {

  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- df_to_NT(data)

  jmler_family <- JuliaConnectoR::juliaCall(switch(
    match.arg(family), gaussian = "Normal", binomial = "Bernoulli"
  ))

  grouping_vars <- lapply(lme4::findbars(julia_formula), `[[`, 3)
  jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = grouping_vars)

  out <- .jlmerclusterperm$jlmer(jlmer_fm, jlmer_df, jmler_family, jlmer_groupings, ...)

  out

}

#' Fit a Julia mixed model using lmer syntax
#'
#' @param formula Model formula in lme4 syntax
#' @param data Dataframe
#' @param reformulate_opts Other of options passed to `jlmer_model_matrix()`
#' @inheritParams jlmer
#'
#' @seealso jlmer_model_matrix
#'
#' @export
to_jlmer <- function(formula, data, family = c("gaussian", "binomial"), reformulate_opts = list(), ...) {

  mm <- do.call(jlmer_model_matrix, modifyList(reformulate_opts, list(fm = formula, df = data)))
  jlmer(julia_formula = mm$julia_formula, data = mm$data, family, ...)

}

#' Fit Julia mixed models to each time point of a time series data
#'
#' @param time Column representing time in `data`
#' @inheritParams jlmer
#' @param ... Optional arguments to fit. Defaults to `fast = TRUE` and `progress = FALSE`.
#'
#' @seealso jlmer_model_matrix
#'
#' @return A Predictor x Time matrix of t-values
#' @export
jlmer_by_time <- function(julia_formula, data, time, family = c("gaussian", "binomial"), ...) {

  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- JuliaConnectoR::juliaCall("DataFrame", data)

  jmler_family <- JuliaConnectoR::juliaCall(switch(
    match.arg(family), gaussian = "Normal", binomial = "Bernoulli"
  ))

  jlmer_time <- as.character(substitute(time))

  grouping_vars <- lapply(lme4::findbars(julia_formula), `[[`, 3)
  jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = grouping_vars)

  opts <- list(...)
  opts <- modifyList(list(progress = FALSE), opts)
  if (match.arg(family) == "binomial") {
    opts <- modifyList(list(fast = TRUE), opts)
  }

  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$jlmer_by_time,
    c(list(jlmer_fm, jlmer_df, jlmer_time, jmler_family, jlmer_groupings), opts)
  ))

  out$Predictors <- replace(out$Predictors, out$Predictors == "1", "(Intercept)")
  dimnames(out$z_matrix) <- out[c("Predictors", "Time")]

  out$z_matrix

}
