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
  term_groups <- augment_term_groups(jlmer_spec$meta$term_groups, statistic)
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
    c(args, nsim, participant_col, trial_col, term_groups$jl, predictors_subset, statistic, is_mem, opts)
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
  if (statistic == "chisq") {
    Predictors <- jlmer_spec$meta$term_groups
    Predictors <- Predictors[names(Predictors) != "1"]
    pruned <- which(!duplicated(rep(names(Predictors), lengths(Predictors))))
    out$z_array <- out$z_array[, , pruned, drop = FALSE]
    dimnames(out$z_array)$Predictor <- names(Predictors)
  }

  if (!is.null(predictors)) {
    predictors_keep <- names(Filter(function(x) predictors %in% x, jlmer_spec$meta$term_groups))
    out$z_array <- out$z_array[, , predictors_keep, drop = FALSE]
  }

  structure(out$z_array, class = "timewise_statistics",
            statistic = statistic, term_groups = term_groups$r)

}

augment_term_groups <- function(term_groups, statistic) {
  term_levels <- lengths(term_groups)
  grp_idx <- split(seq_len(sum(lengths(term_groups))), rep(seq_along(term_groups), times = term_levels))
  term_groups_jl <- JuliaConnectoR::juliaLet("Tuple(x)", x = lapply(seq_along(term_groups), function (i) {
    JuliaConnectoR::juliaLet("(p = p, i = i)", p = as.list(term_groups[[i]]), i = as.list(grp_idx[[i]]))
  }))
  if (statistic == "chisq" && !all(term_levels == 1)) {
    # cli::cli_abort(c(
    #   "Using {.val chisq} statistic for multi-level categorical variables is not supported.",
    #   x = "Predictor{?s} {.val {names(term_groups)[term_levels != 1]}} {?is/are} expressed by more than one model term.",
    #   i = "Try {.arg statistic = {.val t}} instead for a more interpretable result over individual terms."
    # ))
    term_groups <- term_groups[names(term_groups) != "1"]
    term_dfs <- stats::setNames(lengths(term_groups), names(term_groups))
    term_groups <- stats::setNames(nm = names(term_groups))
    attr(term_groups, "dfs") <- term_dfs
  }
  list(r = term_groups, jl = term_groups_jl)
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
