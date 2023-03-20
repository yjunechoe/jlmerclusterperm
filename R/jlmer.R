#' Fit a mixed model in Julia
#'
#' @param formula Model formula
#' @param data Dataframe
#' @param family A GLM family. Currently supports "gaussian" and "binomial".
#' @param ... Optional arguments to fit (`fast`, `nAGQ`, etc.)
#'
#' @export
jlmer <- function(formula, data, family = c("gaussian", "binomial"), ...) {

  mm <- jlmer_model_matrix(formula, as.data.frame(data))
  jlmer.fit(julia_formula = mm$julia_formula, model_matrix = as.data.frame(mm$data), family, ...)

}

#' @param julia_formula From output of `jlmer_model_matrix()`
#' @param model_matrix From output of `jlmer_model_matrix()`
#' @param ... Optional arguments to fit (`fast`, `nAGQ`, etc.)
#'
#' @export
#' @rdname jlmer
jlmer.fit <- function(julia_formula, model_matrix, family = c("gaussian", "binomial"), ...) {

  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- JuliaConnectoR::juliaLet("DataFrame(df)", df = model_matrix)

  jmler_family <- JuliaConnectoR::juliaEval(switch(match.arg(family),
    gaussian = "Normal()",
    binomial = "Bernoulli()"
  ))

  grouping_vars <- lapply(lme4::findbars(julia_formula), `[[`, 3)
  jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = grouping_vars)

  .jlmerclusterperm$jlmer(jlmer_fm, jlmer_df, jmler_family, jlmer_groupings, ...)

}
