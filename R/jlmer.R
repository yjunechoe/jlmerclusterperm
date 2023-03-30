#' Fit a Julia mixed model
#'
#' @param jlmer_spec Data prepped for jlmer from `make_jlmer_spec()`
#' @param family A GLM family. Currently supports "gaussian" and "binomial".
#' @param ... Optional arguments to fit (`fast`, `nAGQ`, etc.)
#'
#' @seealso make_jlmer_spec
#'
#' @export
jlmer <- function(jlmer_spec, family = c("gaussian", "binomial"), ...) {

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_spec$formula$jl, jlmer_spec$data, time = NULL, family, ...)[-3]

  out <- do.call(.jlmerclusterperm$jlmer, c(args, jlmer_spec$meta$is_mem, ...))
  out

}

#' Fit a Julia mixed model using lmer syntax
#'
#' @param formula Model formula in lme4 syntax
#' @param data Dataframe
#' @param jlmer_spec_opts Other options passed to `make_jlmer_spec()`
#' @inheritParams jlmer
#'
#' @seealso make_jlmer_spec
#'
#' @export
to_jlmer <- function(formula, data, family = c("gaussian", "binomial"), jlmer_spec_opts = list(), ...) {

  jlmer_spec <- do.call(make_jlmer_spec, utils::modifyList(jlmer_spec_opts, list(fm = formula, df = data)))
  jlmer(jlmer_spec, family, ...)

}
