#' Fit Julia mixed models to each time point of a time series data
#'
#' @param nsim Number of simulations description
#' @inheritParams jlmer_by_time
#'
#' @seealso jlmer_model_matrix
#'
#' @return A list of Predictor x Time matrix of z-values
#' @export
clusterpermute <- function(julia_formula, data, time, family = c("gaussian", "binomial"),
                           nsim = 100L, threshold = 1.5, participant_col, binned = TRUE, ...) {

  family <- match.arg(family)
  args <- prep_for_jlmer(julia_formula, data, time, family, ...)
  nsim <- as.integer(nsim)

  opts <- list(...)
  opts <- modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- modifyList(list(fast = TRUE), opts)
  }

  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$clusterpermute,
    c(args, nsim, threshold, participant_col, binned, opts)
  ))

  out

}
