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
  if (!JuliaConnectoR::juliaSetupOk()) stop("Cannot set up Julia for jlmerclusterperm.")
  if (Sys.getenv("JULIACONNECTOR_SERVER") != "") JuliaConnectoR::stopJulia()
  setup_with_progress()
  invisible(TRUE)
}

setup_with_progress <- function() {
  start_with_threads()
  set_projenv()
  source_fns()
}

start_with_threads <- function(...) {
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  nthreads <- getOption("jlmerclusterperm.threads") %||%
    JULIA_NUM_THREADS %||%
    (max(8, parallel::detectCores()) - 1)
  cli::cli_progress_step("Starting Julia with {nthreads} thread{?s}")
  if (nthreads > 1) {
    Sys.setenv("JULIA_NUM_THREADS" = nthreads)
    suppressMessages(JuliaConnectoR::startJuliaServer())
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  } else {
    suppressMessages(JuliaConnectoR::startJuliaServer())
  }
  .jlmerclusterperm$nthreads <- nthreads
}

set_projenv <- function(...) {
  cli::cli_progress_step("Activating project environment")
  pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  JuliaConnectoR::juliaLet("cd(pkgdir)", pkgdir = pkgdir)
  JuliaConnectoR::juliaEval('using Suppressor, Pkg;')
  JuliaConnectoR::juliaEval('@suppress Pkg.activate(".");')
  .jlmerclusterperm$pkgdir <- pkgdir
}

source_fns <- function(...) {
  cli::cli_progress_step("Running package setup scripts")
  jl_scripts <- list.files(system.file("julia/", package = "jlmerclusterperm"), pattern = "\\d{2}-.*\\.jl$", full.names = TRUE)
  for (i in seq_along(jl_scripts)) {
    JuliaConnectoR::juliaCall("include", jl_scripts[i])
  }
  .jlmerclusterperm$jlmer <- function(...) JuliaConnectoR::juliaCall("jlmer", ...)
  .jlmerclusterperm$jlmer_by_time <- function(...) JuliaConnectoR::juliaCall("jlmer_by_time", ...)
  .jlmerclusterperm$clusterpermute <- function(...) JuliaConnectoR::juliaCall("clusterpermute", ...)
}


