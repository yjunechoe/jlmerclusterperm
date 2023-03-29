#' @keywords internal
.jlmerclusterperm <- new.env(parent = emptyenv())

#' Initial setup for jlmerclusterperm package.
#'
#' @param ... Ignored.
#' @param verbose Print progress and messages from Julia in the console
#'
#' @examples
#' \dontrun{
#' jlmerclusterperm_setup()
#' }
#'
#' @export
jlmerclusterperm_setup <- function(..., verbose = TRUE) {
  if (!JuliaConnectoR::juliaSetupOk()) cli::cli_abort("Cannot set up Julia for {.pkg jlmerclusterperm}.")
  if (Sys.getenv("JULIACONNECTOR_SERVER") != "") JuliaConnectoR::stopJulia()
  setup_with_progress(verbose = verbose)
  invisible(TRUE)
}

setup_with_progress <- function(..., verbose) {
  start_with_threads(verbose = verbose)
  set_projenv(verbose = verbose)
  source_fns(verbose = verbose)
}

start_with_threads <- function(..., verbose = TRUE) {
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  nthreads <- getOption("jlmerclusterperm.threads") %||%
    JULIA_NUM_THREADS %||%
    (max(8, parallel::detectCores()) - 1)
  if (verbose) cli::cli_progress_step("Starting Julia with {nthreads} thread{?s}")
  if (nthreads > 1) {
    Sys.setenv("JULIA_NUM_THREADS" = nthreads)
    suppressMessages(JuliaConnectoR::startJuliaServer())
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  } else {
    suppressMessages(JuliaConnectoR::startJuliaServer())
  }
  .jlmerclusterperm$opts$nthreads <- nthreads
}

set_projenv <- function(..., verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Activating project environment")
  pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  JuliaConnectoR::juliaLet("cd(pkgdir)", pkgdir = pkgdir)
  JuliaConnectoR::juliaEval('using Suppressor, Pkg;')
  JuliaConnectoR::juliaEval('@suppress Pkg.activate(".");')
  JuliaConnectoR::juliaCall('Pkg.instantiate')
  .jlmerclusterperm$opts$pkgdir <- pkgdir
}

source_fns <- function(..., verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Running package setup scripts")
  # Not recycling `.jlmerclusterperm$opts$pkgdir` here for easy debugging
  jl_scripts <- list.files(system.file("julia/", package = "jlmerclusterperm"), pattern = "\\d{2}-.*\\.jl$", full.names = TRUE)
  for (i in seq_along(jl_scripts)) {
    JuliaConnectoR::juliaCall("include", jl_scripts[i])
  }
  fns_exported <- c("jlmer", "jlmer_by_time", "clusterpermute")
  .jlmerclusterperm$jlmer <- function(...) JuliaConnectoR::juliaCall("jlmer", ...)
  .jlmerclusterperm$jlmer_by_time <- function(...) JuliaConnectoR::juliaCall("jlmer_by_time", ...)
  .jlmerclusterperm$clusterpermute <- function(...) JuliaConnectoR::juliaCall("clusterpermute", ...)
  fns_exported
}
