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
  if (Sys.getenv("JULIACONNECTOR_SERVER") != "") JuliaConnectoR::stopJulia()
  nthreads <- getOption("jlmerclusterperm.threads") %||%
    Sys.getenv("JULIA_NUM_THREADS") %||%
    (parallel::detectCores() - 1)
  .jlmerclusterperm$nthreads <- nthreads
  if (nthreads > 1) {
    message("Starting Julia with ", nthreads, " workers ...")
    Sys.setenv("JULIA_NUM_THREADS" = nthreads)
    suppressMessages(JuliaConnectoR::startJuliaServer())
    JuliaConnectoR::juliaEval(paste0("using Distributed; addprocs(", nthreads, ");"))
    Sys.unsetenv("JULIA_NUM_THREADS")
  } else {
    message("Starting Julia ...")
    suppressMessages(JuliaConnectoR::startJuliaServer())
  }
  message("Running package setup scripts ...")
  JuliaConnectoR::juliaEval(paste0('include("', system.file("julia/setup.jl", package = "jlmerclusterperm"), '")'))
  populate_fns()
  invisible(TRUE)
}

populate_fns <- function(...) {
  .jlmerclusterperm$jlmer <- JuliaConnectoR::juliaEval("jlmer")
}
