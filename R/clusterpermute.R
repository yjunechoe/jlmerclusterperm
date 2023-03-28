#' Fit Julia mixed models to each time point of a time series data
#'
#' @param nsim Number of simulations description
#' @param predictors A subset of predictors to test. Defaults to `NULL` which tests all predictors.
#' @inheritParams jlmer_by_time
#'
#' @seealso prep_jlmer_data
#'
#' @return A list of Predictor x Time matrix of z-values
#' @export
clusterpermute <- function(jlmer_data, family = c("gaussian", "binomial"),
                           nsim = 100L, predictors = NULL, ...) {

  is_mem <- jlmer_data$meta$is_mem
  participant_col <- jlmer_data$meta$subject
  term_groups <- augment_term_groups(jlmer_data$meta$term_groups)
  predictors_subset <- list(as.list(predictors))

  trial_col <- jlmer_data$meta$item %||% ""
  # if (is.null(jlmer_data$meta$item)) {
  #   trial_col <- ""
  # } else {
  #   trials <- interaction(jlmer_data$data[unlist(jlmer_data$meta[c("subject", "item")])], drop = TRUE)
  #   jlmer_data$data$JLMER_TRIAL_COL <- as.integer(trials)
  #   trial_col <- "JLMER_TRIAL_COL"
  # }

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_data$formula$jl, jlmer_data$data, jlmer_data$meta$time, family, ...)
  nsim <- as.integer(nsim)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }

  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$clusterpermute,
    c(args, nsim, participant_col, trial_col, term_groups, predictors_subset, is_mem, opts)
  ))

  dimnames(out$z_array) <- list(
    Sim = sprintf(paste0("%0", floor(log10(nsim)), "d"), 1:nsim),
    Time = sort(unique(jlmer_data$data[[jlmer_data$meta$time]])),
    Predictor = unlist(out$predictors)
  )

  if (!is.null(predictors)) {
    out$z_array <- out$z_array[,,predictors, drop = FALSE]
  }

  out$z_array

}

augment_term_groups <- function(term_groups) {
  grp_idx <- split(seq_len(sum(lengths(term_groups))), rep(seq_along(term_groups), times = lengths(term_groups)))
  JuliaConnectoR::juliaLet("Tuple(x)", x = lapply(seq_along(term_groups), function (i) {
    JuliaConnectoR::juliaLet("(p = p, i = i)", p = as.list(term_groups[[i]]), i = as.list(grp_idx[[i]]))
  }))
}
