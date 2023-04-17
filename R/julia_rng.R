#' RNG seed and counter for the permutation algorithm
#'
#' Experimental.
#'
#' @name julia_rng
#' @keywords internal
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

#' @param seed Seed
#'
#' @rdname julia_rng
#' @export
set_rng_seed <- function(seed) {
  seedL <- as.integer(seed)
  JuliaConnectoR::juliaEval(paste0("Random123.seed!(rng, (", seedL, ", 20))"))
  invisible(seedL)
}
