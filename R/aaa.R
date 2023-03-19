#' @keywords internal
.jlmerclusterperm <- new.env(parent = emptyenv())

#' Initial setup for jlmerclusterperm package.
#'
#' @param ... Ignored.
#'
#' @examples
#' \dontrun{
#' jlmerclusterperm_setup()
#' }
#'
#' @export
jlmerclusterperm_setup <- function(...) {
  if (!JuliaConnectoR::juliaSetupOk()) stop("Cannot set up Julia.")
  JuliaConnectoR::juliaEval(paste0('include("', system.file("julia/setup.jl", package = "jlmerclusterperm"), '")'))
  .jlmerclusterperm$jlmer_jl <- JuliaConnectoR::juliaEval("jlmer")
}
