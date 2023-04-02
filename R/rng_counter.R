#' RNG seed and counter for the permutation algorithm
#'
#' @name rng_counter
#' @keywords internal
NULL

#' @param i Counter number
#'
#' @rdname rng_counter
#' @export
set_rng_counter <- function(i) {
  iL <- as.integer(i)
  JuliaConnectoR::juliaLet("set_counter!(rng, i)", i = iL)
  iL
}

#' @rdname rng_counter
#' @export
reset_rng_counter <- function() {
  JuliaConnectoR::juliaEval("set_counter!(rng, 0)")
  0L
}
