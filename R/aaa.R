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
    # JuliaConnectoR::juliaEval(paste0("using Distributed; addprocs(", nthreads, ");"))
    Sys.unsetenv("JULIA_NUM_THREADS")
  } else {
    message("Starting Julia ...")
    suppressMessages(JuliaConnectoR::startJuliaServer())
  }
  message("Running package setup scripts ...")
  source_fns()
  invisible(TRUE)
}

source_fns <- function(...) {
  jl_scripts <- list.files(system.file("julia/", package = "jlmerclusterperm"), pattern = "\\d{2}-.*\\.jl$", full.names = TRUE)
  lapply(jl_scripts, function(x) JuliaConnectoR::juliaCall("include", x))
  .jlmerclusterperm$jlmer <- function(...) JuliaConnectoR::juliaCall("jlmer", ...)
  .jlmerclusterperm$jlmer_by_time <- function(...) JuliaConnectoR::juliaCall("jlmer_by_time", ...)
  .jlmerclusterperm$clusterpermute <- function(...) JuliaConnectoR::juliaCall("clusterpermute", ...)
}
