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
  JuliaConnectoR::juliaLet("set_counter!(rng, i)", i = as.integer(i))
  invisible(i)
}

#' @rdname rng_counter
#' @export
reset_rng_counter <- function() {
  JuliaConnectoR::juliaEval("set_counter!(rng, 0)")
  invisible(0L)
}
