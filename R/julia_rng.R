#' Interface to the Julia RNG
#'
#' @name julia_rng
NULL

#' @param i Counter number
#'
#' @rdname julia_rng
#' @export
set_rng_state <- function(i) {
  counterL <- as.integer(i)
  JuliaConnectoR::juliaLet("set_counter!(rng, i)", i = counterL)
  invisible(counterL)
}

#' @rdname julia_rng
#' @export
reset_rng_state <- function() {
  JuliaConnectoR::juliaEval("set_counter!(rng, 0)")
  invisible(0L)
}

#' @rdname julia_rng
#' @export
get_rng_state <- function() {
  JuliaConnectoR::juliaEval("Int(rng.ctr1)")
}

#' @rdname julia_rng
#' @param seed Seed
#' @export
set_rng_seed <- function(seed) {
  seed_supplied <- !missing(seed)
  if (seed_supplied) {
    seedL <- as.integer(seed)
  } else {
    seedL <- as.integer(JuliaConnectoR::juliaEval("Int(Random123.gen_seed(UInt32, 1)[1])"))
  }
  JuliaConnectoR::juliaEval(paste0("Random123.seed!(rng, (", seedL, ", 20))"))
  .jlmerclusterperm$opts$seed <- seedL
  if (seed_supplied) {
    invisible(seedL)
  } else {
    cli::cli_alert_info("Using randomly generated seed")
    seedL
  }
}

#' @rdname julia_rng
#' @export
get_rng_seed <- function() {
  .jlmerclusterperm$opts$seed
}
