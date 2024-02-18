#' Interface to the Julia RNG
#'
#' @name julia_rng
#' @examplesIf julia_setup_ok()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' # RNG initializes to seed=1 counter=0
#' get_rng_seed()
#' get_rng_state()
#'
#' # setter/getter for RNG counter
#' set_rng_state(123)
#' get_rng_state()
#'
#' # setter/getter for RNG seed
#' set_rng_seed(2)
#' get_rng_seed()
#'
#' # restore to initial setting (seed=1, counter=0)
#' set_rng_seed(1)
#' set_rng_state(0)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return The current seed or counter
NULL

#' @param i Counter number
#'
#' @rdname julia_rng
#' @export
set_rng_state <- function(i) {
  JuliaConnectoR::juliaLet("set_counter!(rng, Int(i))", i = i)
  invisible(i)
}

#' @rdname julia_rng
#' @export
reset_rng_state <- function() {
  JuliaConnectoR::juliaEval("set_counter!(rng, 0)")
  invisible(0)
}

#' @rdname julia_rng
#' @export
get_rng_state <- function() {
  as.double(JuliaConnectoR::juliaEval("Int(rng.ctr1)"))
}

#' @rdname julia_rng
#' @param seed Seed
#' @export
set_rng_seed <- function(seed) {
  seed_missing <- missing(seed)
  if (seed_missing) {
    seed <- as.double(JuliaConnectoR::juliaEval("Int(Random123.gen_seed(UInt32, 1)[1])"))
  }
  JuliaConnectoR::juliaEval(paste0("Random123.seed!(rng, (Int(", seed, "), 20))"))
  .jlmerclusterperm$opts$seed <- seed
  if (seed_missing) {
    cli::cli_alert_info("Using randomly generated seed")
    seed
  } else {
    invisible(seed)
  }
}

#' @rdname julia_rng
#' @export
get_rng_seed <- function() {
  .jlmerclusterperm$opts$seed
}
