#' RNG seed and counter for the permutation algorithm
#'
#' @name julia_rng
#' @keywords internal
NULL

#' @param i Counter number
#'
#' @rdname julia_rng
#' @export
set_rng_counter <- function(counter) {
  counterL <- as.integer(counter)
  JuliaConnectoR::juliaLet("set_counter!(rng, i)", i = counterL)
  counterL
}

#' @rdname julia_rng
#' @export
reset_rng_counter <- function() {
  JuliaConnectoR::juliaEval("set_counter!(rng, 0)")
  0L
}

get_rng_counter <- function() {
  JuliaConnectoR::juliaEval("Int(rng.ctr1)")
}

#' @rdname julia_rng
#' @export
set_rng_seed <- function(seed) {
  seedL <- as.integer(seed)
  JuliaConnectoR::juliaEval("Random123.seed!(rng, (", seedL, ", 20))")
  seedL
}
