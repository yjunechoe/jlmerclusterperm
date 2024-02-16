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

# Setup helpers
julia_cli <- function(x) {
  utils::tail(system2("julia", x, stdout = TRUE), 1L)
}
parse_julia_version <- function(version) {
  gsub("^julia.version .*(\\d+\\.\\d+\\.\\d+).*$", "\\1", version)
}
julia_version <- function() {
  parse_julia_version(julia_cli("--version"))
}
julia_version_compatible <- function() {
  as.package_version(julia_version()) >= "1.8"
}
julia_detect_cores <- function() {
  as.integer(julia_cli('-q -e "println(Sys.CPU_THREADS);"'))
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
#' @param cache_dir The location to write out package cache files (namely, Manifest.toml).
#'   If `NULL` (default), attempts to write to the package's cache directory discovered via
#'   `R_user_dir()` and falls back to `tempdir()`.
#' @param restart Whether to set up a fresh Julia session, given that one is already running.
#'   If `FALSE` and `jlmerclusterperm_setup()` has already been called, nothing happens.
#' @param verbose Print progress and messages from Julia in the console
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @export
#' @return TRUE
jlmerclusterperm_setup <- function(..., cache_dir = NULL, restart = TRUE, verbose = TRUE) {
  if (!JuliaConnectoR::juliaSetupOk()) cli::cli_abort("No Julia installation detected.")
  if (!julia_version_compatible()) cli::cli_abort("Julia version >=1.8 required.")
  if (restart || !is_setup()) {
    JuliaConnectoR::stopJulia()
    setup_with_progress(cache_dir = cache_dir, verbose = verbose)
    .jlmerclusterperm$is_setup <- TRUE
  } else {
    cli::cli_inform("Julia instance already running - skipping setup.")
  }
  invisible(TRUE)
}

setup_with_progress <- function(..., cache_dir = NULL, verbose = TRUE) {
  start_with_threads(verbose = verbose)
  set_projenv(cache_dir = cache_dir, verbose = verbose)
  source_jl(verbose = verbose)
  define_globals()
  cleanup_jl()
  invisible(TRUE)
}

start_with_threads <- function(..., max_threads = 7L, verbose = TRUE) {
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  nthreads <- getOption("jlmerclusterperm.nthreads", JULIA_NUM_THREADS) %|0|%
    (min(max_threads, julia_detect_cores() - 1L))
  if (verbose) cli::cli_progress_step("Starting Julia with {nthreads} thread{?s}")
  if (nthreads > 1) {
    Sys.setenv("JULIA_NUM_THREADS" = nthreads)
    suppressMessages(JuliaConnectoR::startJuliaServer())
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  } else {
    suppressMessages(JuliaConnectoR::startJuliaServer())
  }
  .jlmerclusterperm$opts$nthreads <- nthreads
  invisible(TRUE)
}

set_projenv <- function(..., cache_dir = NULL, verbose = TRUE) {
  if (verbose) cli::cli_progress_step("Activating package environment")
  userdir <- if (!is.null(cache_dir)) {
    cache_dir
  } else {
    pkg_cache_dir <- R_user_dir("jlmerclusterperm", which = "cache")
    if (dir.exists(dirname(pkg_cache_dir))) pkg_cache_dir else tempdir()
  }
  projdir <- file.path(userdir, "julia")
  manifest <- file.path(projdir, "Manifest.toml")
  manifest_cached <- file.exists(manifest) &&
    (julia_version() == parse_julia_version(readLines(manifest)[3]))
  if (!dir.exists(userdir)) {
    dir.create(userdir)
  }
  if (!manifest_cached) {
    unlink(manifest)
  }
  pkgdir <- system.file("julia/", package = "jlmerclusterperm")
  file.copy(from = pkgdir, to = userdir, recursive = TRUE)
  JuliaConnectoR::juliaCall("cd", projdir)
  JuliaConnectoR::juliaEval("using Pkg")
  JuliaConnectoR::juliaEval('Pkg.activate(".", io = devnull)')
  JuliaConnectoR::juliaEval('Pkg.develop(path = "JlmerClusterPerm", io = devnull)')
  io <- if (manifest_cached || !verbose) "io = devnull" else ""
  JuliaConnectoR::juliaEval(sprintf("Pkg.instantiate(%s)", io))
  JuliaConnectoR::juliaEval("Pkg.resolve(io = devnull)")
  JuliaConnectoR::juliaCall("cd", getwd())
  .jlmerclusterperm$opts$projdir <- projdir
  invisible(TRUE)
}

define_globals <- function(...) {
  seed <- as.integer(getOption("jlmerclusterperm.seed", 1L))
  JuliaConnectoR::juliaEval(sprintf(
    "pg = Dict(:width => %i, :io => stderr);
    using Random123;
    const rng = Threefry2x((%i, 20));",
    max(1L, cli::console_width() - 44L),
    as.integer(seed)
  ))
  .jlmerclusterperm$opts$seed <- seed
  .jlmerclusterperm$get_jl_opts <- function(x) {
    list(JuliaConnectoR::juliaEval("(pg = pg, rng = rng)"))
  }
  invisible(TRUE)
}

source_jl <- function(..., verbose = TRUE) {
  jl_pkgs <- readLines(file.path(.jlmerclusterperm$opts$projdir, "load-pkgs.jl"))
  i <- 1L
  if (verbose) cli::cli_progress_step("Running package setup scripts ({i}/{length(jl_pkgs) + 1})")
  for (i in (seq_along(jl_pkgs) + 1)) {
    if (verbose) cli::cli_progress_update()
    if (i <= length(jl_pkgs)) {
      JuliaConnectoR::juliaEval(jl_pkgs[i])
    } else {
      .jlmerclusterperm$jl <- JuliaConnectoR::juliaImport("JlmerClusterPerm")
    }
  }
  invisible(TRUE)
}

cleanup_jl <- function(...) {
  unlink(dir(.jlmerclusterperm$opts$projdir, pattern = "[^Manifest.toml]", full.names = TRUE), recursive = TRUE)
}
