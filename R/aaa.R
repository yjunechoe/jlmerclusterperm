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
  source_jl(verbose = verbose)
}

start_with_threads <- function(..., max_threads = 7, verbose = TRUE) {
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  nthreads <- getOption("jlmerclusterperm.nthreads") %||%
    JULIA_NUM_THREADS %||%
    (min(max_threads, parallel::detectCores() - 1))
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
  if (verbose) cli::cli_progress_step("Activating package environment")
  pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  JuliaConnectoR::juliaCall("cd", pkgdir)
  JuliaConnectoR::juliaEval('using Suppressor, Pkg;')
  JuliaConnectoR::juliaEval('@suppress Pkg.activate(".");')
  JuliaConnectoR::juliaCall('Pkg.instantiate')
  JuliaConnectoR::juliaCall("cd", getwd())
  JuliaConnectoR::juliaEval(paste0('using Random123; rng = Threefry2x((', getOption("jlmerclusterperm.seed") %||% 1, ', 20))'))
  .jlmerclusterperm$opts$pkgdir <- pkgdir
}

source_jl <- function(..., verbose = TRUE) {
  jl_pkgs <- readLines(file.path(.jlmerclusterperm$opts$pkgdir, "load-pkgs.jl"))
  jl_scripts <- list.files(.jlmerclusterperm$opts$pkgdir, pattern = "\\d{2}-.*\\.jl$", full.names = TRUE)
  load_steps <- c(jl_pkgs, jl_scripts)
  i <- 1L
  if (verbose) cli::cli_progress_step("Running package setup scripts ({i}/{length(load_steps)})")
  for (i in seq_along(load_steps)) {
    if (verbose) cli::cli_progress_update()
    jl_load <- load_steps[i]
    if (grepl("^using ", jl_load)) {
      JuliaConnectoR::juliaEval(jl_load)
    } else {
      JuliaConnectoR::juliaCall("include", jl_load)
    }
  }
  .jlmerclusterperm$jlmer <- function(...) JuliaConnectoR::juliaCall("jlmer", ...)
  .jlmerclusterperm$jlmer_by_time <- function(...) JuliaConnectoR::juliaCall("jlmer_by_time", ...)
  .jlmerclusterperm$clusterpermute <- function(...) JuliaConnectoR::juliaCall("clusterpermute", ...)
  .jlmerclusterperm$guess_shuffle_as <- function(...) JuliaConnectoR::juliaCall("guess_shuffle_as", ...)
  .jlmerclusterperm$permute_by_predictor <- function(...) JuliaConnectoR::juliaCall("permute_by_predictor", ...)
  .jlmerclusterperm$exported_fns <- c("jlmer", "jlmer_by_time", "clusterpermute", "guess_shuffle_as", "permute_by_predictor")
}

#' @keywords internal
dev_source <- function() {
  .jlmerclusterperm$opts$pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  source_jl()
}
