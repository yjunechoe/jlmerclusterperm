#' Fit Julia mixed models to each time point of a time series data
#'
#' @param nsim Number of simulations description
#' @param predictors A subset of predictors to test. Defaults to `NULL` which tests all predictors.
#' @inheritParams compute_timewise_statistics
#'
#' @seealso [make_jlmer_spec()]
#'
#' @return A simulation-by-time-by-predictor 3D array of cluster statistics.
#' @export
permute_timewise_statistics <- function(jlmer_spec, family = c("gaussian", "binomial"),
                                        statistic = c("t", "chisq"),
                                        nsim = 100L, predictors = NULL, ...) {

  statistic <- match.arg(statistic)
  is_mem <- jlmer_spec$meta$is_mem
  participant_col <- jlmer_spec$meta$subject
  trial_col <- jlmer_spec$meta$trial %|0|% ""
  term_groups <- augment_term_groups(jlmer_spec$meta$term_groups)
  predictors_subset <- list(as.list(predictors))

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_spec$formula$jl, jlmer_spec$data, jlmer_spec$meta$time, family, ...)
  nsim <- as.integer(nsim)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }

  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$permute_timewise_statistics,
    c(args, nsim, participant_col, trial_col, term_groups, predictors_subset, statistic, is_mem, opts)
  ))

  # shuffle_predictor_groups <- Filter(function(x) !identical(x, "(Intercept)"), jlmer_spec$meta$term_groups)
  # counter_states <- split(out$counter_states, rep(names(shuffle_predictor_groups), each = nsim))
  # counter_states[] <- lapply(names(counter_states), function(x) {
  #   list(predictors = shuffle_predictor_groups[[x]], counter = counter_states[[x]])
  # })

  dimnames(out$z_array) <- list(
    Sim = sprintf(paste0("%0", floor(log10(nsim)), "d"), 1:nsim),
    Time = sort(unique(jlmer_spec$data[[jlmer_spec$meta$time]])),
    Predictor = unlist(out$predictors)
  )

  if (!is.null(predictors)) {
    out$z_array <- out$z_array[, , predictors, drop = FALSE]
  }

  structure(out$z_array, class = "timewise_statistics", statistic = statistic)

}

augment_term_groups <- function(term_groups) {
  grp_idx <- split(seq_len(sum(lengths(term_groups))), rep(seq_along(term_groups), times = lengths(term_groups)))
  JuliaConnectoR::juliaLet("Tuple(x)", x = lapply(seq_along(term_groups), function (i) {
    JuliaConnectoR::juliaLet("(p = p, i = i)", p = as.list(term_groups[[i]]), i = as.list(grp_idx[[i]]))
  }))
}

#' @export
print.timewise_statistics <- function(x, ...) {
  x_dim <- dim(x)
  x_dimnames <- dimnames(x)
  attributes(x) <- NULL
  dim(x) <- x_dim
  dimnames(x) <- x_dimnames
  print(x, ...)
}
