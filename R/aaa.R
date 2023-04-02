#' @keywords internal
.jlmerclusterperm <- new.env(parent = emptyenv())
.jlmerclusterperm$cli_theme <- list(
  h1 = list(`font-weight` = "regular", `margin-top` = 0, fmt = function(x) cli::rule(x, line_col = "white")),
  span.lemph = list(color = "grey", `font-style` = "italic"),
  span.el = list(color = "cyan"),
  span.fm = list(color = "blue")
)

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
  nthreads <- getOption("jlmerclusterperm.nthreads", JULIA_NUM_THREADS) %|0|%
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
  JuliaConnectoR::juliaEval(paste0('using Random123; const rng = Threefry2x((', getOption("jlmerclusterperm.seed", 1L), ', 20))'))
  .jlmerclusterperm$set_counter <- function(i) JuliaConnectoR::juliaLet("set_counter!(rng, i)", i)
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
  exported_fns <- c(
    "jlmer", "compute_timewise_statistics",
    "extract_clusters", "permute_timewise_statistics",
    "guess_shuffle_as", "permute_by_predictor"
  )
  for (jl_fn in exported_fns) {
    .jlmerclusterperm[[jl_fn]] <- wrap_jl_fn(jl_fn)
  }
  .jlmerclusterperm$exported_fns <- exported_fns
}

#' @keywords internal
dev_source <- function() {
  .jlmerclusterperm$opts$pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  source_jl()
}

wrap_jl_fn <- function(jl_fn) {
  force(jl_fn)
  function(...) JuliaConnectoR::juliaCall(jl_fn, ...)
}
