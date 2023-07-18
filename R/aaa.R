#' @keywords internal
.jlmerclusterperm <- new.env(parent = emptyenv())
.jlmerclusterperm$cli_theme <- list(
  h1 = list(
    `font-weight` = "regular",
    `margin-top` = 0,
    fmt = function(x) cli::rule(x, line_col = "white")
  ),
  span.lemph = list(color = "grey", `font-style` = "italic"),
  span.el = list(color = "green"),
  span.fm = list()
)

# Helpers
julia_version_compatible <- function() {
  julia_version <- gsub("^.*(\\d+\\.\\d+\\.\\d+).*$", "\\1", system2("julia", "--version", stdout = TRUE))
  as.package_version(julia_version) >= "1.8"
}
is_setup <- function() isTRUE(.jlmerclusterperm$is_setup)

#' Check Julia setup requirements for jlmerclusterperm
#'
#' @return Boolean
#' @export
#' @examples
#' julia_setup_ok()
julia_setup_ok <- function() {
  JuliaConnectoR::juliaSetupOk() && julia_version_compatible()
}

#' Initial setup for the jlmerclusterperm package
#'
#' @param ... Ignored.
#' @param restart Whether to set up a fresh Julia session, given that one is already running.
#'   If `FALSE` and `jlmerclusterperm_setup()` has already been called, nothing happens.
#' @param verbose Print progress and messages from Julia in the console
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(verbose = FALSE)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @export
#' @return TRUE
jlmerclusterperm_setup <- function(..., restart = TRUE, verbose = TRUE) {
  if (!JuliaConnectoR::juliaSetupOk()) cli::cli_abort("No Julia installation detected.")
  if (!julia_version_compatible()) cli::cli_abort("Julia version >=1.8 required.")
  if (restart || !is_setup()) {
    JuliaConnectoR::stopJulia()
    setup_with_progress(verbose = verbose)
    .jlmerclusterperm$is_setup <- TRUE
  } else {
    if (verbose) cli::cli_inform("Julia instance already running - skipping setup.")
  }
  invisible(TRUE)
}

setup_with_progress <- function(..., verbose = TRUE) {
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
  seed <- as.integer(getOption("jlmerclusterperm.seed", 1L))
  JuliaConnectoR::juliaCall("cd", pkgdir)
  JuliaConnectoR::juliaEval("using Pkg")
  JuliaConnectoR::juliaEval('Pkg.activate(".", io = devnull)')
  JuliaConnectoR::juliaEval("Pkg.instantiate()") # io = devnull
  JuliaConnectoR::juliaEval("Pkg.resolve(io = devnull)")
  JuliaConnectoR::juliaCall("cd", getwd())
  JuliaConnectoR::juliaEval(paste0("pg_width = ", max(1L, cli::console_width() - 44L)))
  JuliaConnectoR::juliaEval("pg_io = stderr")
  JuliaConnectoR::juliaEval(paste0("using Random123; const rng = Threefry2x((", seed, ", 20))"))
  .jlmerclusterperm$opts$seed <- seed
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
    .jlmerclusterperm[[jl_fn]] <- JuliaConnectoR::juliaFun(jl_fn)
  }
  .jlmerclusterperm$exported_fns <- exported_fns
}

#' @keywords internal
dev_source <- function() { # nocov start
  .jlmerclusterperm$opts$pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  source_jl()
} # nocov end
