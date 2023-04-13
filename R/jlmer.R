#' Fit a Julia regression model using lme4 syntax
#'
#' @param jlmer_spec_opts List of options passed to `make_jlmer_spec()`
#' @inheritParams jlmer
#' @inheritParams make_jlmer_spec
#'
#' @seealso [jlmer()], [make_jlmer_spec()]
#'
#' @return A `jlmer_mod` object.
#' @export
to_jlmer <- function(formula, data, family = c("gaussian", "binomial"), jlmer_spec_opts = list(), ..., progress = FALSE) {

  jlmer_spec <- do.call(make_jlmer_spec, utils::modifyList(jlmer_spec_opts, list(formula = formula, data = data)))
  jlmer(jlmer_spec, family, ...)

}

#' Fit a Julia regression model using jlmer specifications
#'
#' @param jlmer_spec Data prepped for jlmer from `make_jlmer_spec()`
#' @param family A GLM family. Currently supports "gaussian" and "binomial".
#' @param ... Optional arguments passed to Julia for model fitting.
#' @param progress If `TRUE`, prints the timing of iterations.
#'
#' @seealso [make_jlmer_spec()]
#'
#' @return A `jlmer_mod` object.
#' @export
jlmer <- function(jlmer_spec, family = c("gaussian", "binomial"), ..., progress = FALSE) {

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_spec$formula$jl, jlmer_spec$data, time = NULL, family, ...)[-3]

  mod <- do.call(.jlmerclusterperm$jlmer, c(args, jlmer_spec$meta$is_mem, progress = progress, ...))
  structure(mod, class = c("jlmer_mod", class(mod)))

}

#' @export
print.jlmer_mod <- function(x, ...) {
  cat(format(x, ...))
}

#' @export
format.jlmer_mod <- function(x, ...) {
  cat("<Julia object of type ", JuliaConnectoR::juliaCall("typeof", x), ">\n", sep = "")
  if (JuliaConnectoR::juliaLet("x isa MixedModel", x = x)) {
    re <- gsub("\n\n$", "\n", showobj_reformat(JuliaConnectoR::juliaCall('VarCorr', x)))
    fe <- showobj_reformat(JuliaConnectoR::juliaCall('coeftable', x))
    out <- c(re, fe)
  } else {
    out <- showobj_reformat(JuliaConnectoR::juliaCall('coeftable', x))
  }
  out
}

showobj_reformat <- function(x) {
  paste0(trimws(utils::capture.output(print(x))[-1], whitespace = "[\n]"), collapse = "\n")
}
