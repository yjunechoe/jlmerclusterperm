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

  family <- match.arg(family)
  args <- prep_for_jlmer(julia_formula, data, time = NULL, family, ...)[-3]

  out <- do.call(.jlmerclusterperm$jlmer, c(args, ...))
  out

}

#' Fit a Julia mixed model using lmer syntax
#'
#' @param formula Model formula in lme4 syntax
#' @param data Dataframe
#' @param reformulate_opts Other options passed to `jlmer_model_matrix()`
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
#' @return A Predictor x Time matrix of z-values
#' @export
jlmer_by_time <- function(julia_formula, data, time, family = c("gaussian", "binomial"), ...) {

  family <- match.arg(family)
  args <- prep_for_jlmer(julia_formula, data, time, family, ...)

  opts <- list(...)
  opts <- modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- modifyList(list(fast = TRUE), opts)
  }

  is_mem <- !is.null(lme4::findbars(julia_formula))

  out <- JuliaConnectoR::juliaGet(do.call(.jlmerclusterperm$jlmer_by_time, c(args, is_mem, opts)))

  out$Predictors <- replace(out$Predictors, out$Predictors == "1", "(Intercept)")
  dimnames(out$z_matrix) <- out[c("Predictors", "Time")]

  out$z_matrix

}
