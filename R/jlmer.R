#' Fit a mixed model in Julia
#'
#' @param julia_formula Model formula in Julia syntax
#' @param data Dataframe
#' @param ... Ignored.
#'
#' @export
jlmer <- function(julia_formula, data, ...) {
  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- JuliaConnectoR::juliaLet("DataFrame(df)", df = data)
  jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = lapply(lme4::findbars(julia_formula), `[[`, 3))

  .jlmerclusterperm$jlmer_jl(jlmer_fm, jlmer_df, jlmer_groupings)
}

#' @param jlmer_mm Output of `jlmer_model_matrix()`
#'
#' @export
#' @rdname jlmer
jlmer.fit <- function(jlmer_mm, ...) {
  jlmer(jlmer_mm$julia_formula, jlmer_mm$data, ...)
}
