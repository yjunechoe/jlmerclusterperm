#' Simulate cluster-mass statistics via bootstrapped permutations
#'
#' @param nsim Number of simulations description
#' @param predictors (Optional) a subset of predictors to test. Defaults to `NULL` which tests all predictors.
#' @inheritParams compute_timewise_statistics
#'
#' @seealso [make_jlmer_spec()]
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Specification object
#' spec <- make_jlmer_spec(
#'   weight ~ 1 + Diet, filter(ChickWeight, Time <= 20),
#'   subject = "Chick", time = "Time"
#' )
#' spec
#'
#' # Simulation x Time x Predictor array of t-statistics from regression output
#' reset_rng_state()
#' null_statistics <- permute_timewise_statistics(spec, nsim = 3)
#' round(null_statistics, 2)
#'
#' # Collect as dataframe with `tidy()`
#' permuted_timewise_stats_df <- tidy(null_statistics)
#' permuted_timewise_stats_df
#'
#' # Permutations ran under the same RNG state are identical
#' reset_rng_state()
#' null_statistics2 <- permute_timewise_statistics(spec, nsim = 3)
#' identical(null_statistics, null_statistics2)
#'
#' get_rng_state()
#' null_statistics3 <- permute_timewise_statistics(spec, nsim = 3)
#' identical(null_statistics, null_statistics3)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return A simulation-by-time-by-predictor 3D array of cluster statistics, of class `timewise_statistics`.
#' @export
permute_timewise_statistics <- function(jlmer_spec, family = c("gaussian", "binomial"),
                                        statistic = c("t", "chisq"),
                                        nsim = 100L, predictors = NULL, ...) {
  check_arg_class(jlmer_spec, "jlmer_spec")
  statistic <- match.arg(statistic)
  is_mem <- jlmer_spec$meta$is_mem
  participant_col <- jlmer_spec$meta$subject
  trial_col <- jlmer_spec$meta$trial %|0|% ""
  term_groups <- augment_term_groups(jlmer_spec, statistic)
  predictors_subset <- validate_predictors_subset(predictors, term_groups$r)

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_spec, family = family, ...)
  nsim <- as.integer(nsim)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }

  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$jl$permute_timewise_statistics,
    c(args, nsim, participant_col, trial_col, term_groups$jl,
      predictors_subset, statistic, is_mem, .jlmerclusterperm$get_jl_opts(), opts)
  ))

  dimnames(out$z_array) <- list(
    Sim = as.factor(zero_pad(1:nsim)),
    Time = sort(unique(jlmer_spec$data[[jlmer_spec$meta$time]])),
    Predictor = unlist(out$predictors)
  )
  if (statistic == "t") {
    # if (!is.null(predictors)) {
    #   predictors_keep <- names(Filter(function(x) predictors %in% x, jlmer_spec$meta$term_groups))
    #   out$z_array <- out$z_array[, , predictors_keep, drop = FALSE]
    # }
  } else if (statistic == "chisq") {
    predictors <- Filter(function(x) any(dimnames(out$z_array)$Predictor %in% x), jlmer_spec$meta$term_groups)
    pruned <- which(!duplicated(rep(names(predictors), lengths(predictors))))
    out$z_array <- out$z_array[, , pruned, drop = FALSE]
    dimnames(out$z_array)$Predictor <- names(predictors)
  }

  if (is.null(dimnames(out$z_array)$Predictor)) {
    cli::cli_abort(c(
      "No predictors to permute.",
      i = if (!is.null(predictors)) "{.val {predictors}} {?is/are} not among model terms."
    ))
  }

  convergence_failures <- check_convergence_failures(out$z_array)

  structure(out$z_array,
    class = "timewise_statistics",
    statistic = statistic, term_groups = term_groups$r, convergence_failures = convergence_failures
  )
}

check_convergence_failures <- function(z_array) {
  convergence_failures_pos <- is.nan(z_array)
  if (any(convergence_failures_pos)) {
    convergence_failures <- unique(which(convergence_failures_pos, arr.ind = TRUE)[, c("Predictor", "Sim"), drop = FALSE])
    convergence_failure_table <- table(convergence_failures[, "Predictor"])
    names(convergence_failure_table) <- dimnames(z_array)$Predictor[as.integer(names(convergence_failure_table))]
    cli::cli_alert_info("Convergence errors encountered (out of {.arg nsim = {.val {nrow(z_array)}}}) while bootstrapping the following {cli::qty(names(convergence_failure_table))}predictor{?s}:")
    cli::cli_div(theme = .jlmerclusterperm$cli_theme)
    cli::cli_ul()
    cli::cli_dl(as.list(convergence_failure_table), labels = paste0("{.el ", names(convergence_failure_table), "}"))
    cli::cli_end()
    cli::cli_end()
    convergence_failures[do.call(order, asplit(convergence_failures, 2)), ]
  }
}

validate_predictors_subset <- function(predictors, r_term_groups) {
  predictors_set <- unlist(r_term_groups, use.names = FALSE)
  if (!is.null(predictors) && !any(predictors %in% predictors_set)) {
    cli::cli_abort(c(
      "Invalid selection passed to the {.arg predictor} argument.",
      "x" = "Must choose among {.val {predictors_set}}."
    ))
  } else {
    list(predictors)
  }
}

augment_term_groups <- function(jlmer_spec, statistic) {
  if (!is.null(jlmer_spec$.backdoor$augmented_term_groups)) {
    return(jlmer_spec$.backdoor$augmented_term_groups)
  }

  term_groups <- jlmer_spec$meta$term_groups
  term_levels <- lengths(term_groups)
  grp_idx <- split(seq_len(sum(lengths(term_groups))), rep(seq_along(term_groups), times = term_levels))
  term_groups_jl <- JuliaConnectoR::juliaLet("Tuple(x)", x = lapply(seq_along(term_groups), function(i) {
    JuliaConnectoR::juliaLet("(P = P, p = p, i = i)", P = names(term_groups)[i], p = as.list(term_groups[[i]]), i = as.list(grp_idx[[i]]))
  }))
  term_groups <- term_groups[names(term_groups) != "1"]
  if (statistic == "chisq") {
    # cli::cli_abort(c(
    #   "Using {.val chisq} statistic for multi-level categorical variables is not supported.",
    #   x = "Predictor{?s} {.val {names(term_groups)[term_levels != 1]}} {?is/are} expressed by more than one model term.",
    #   i = "Try {.arg statistic = {.val t}} instead for a more interpretable result over individual terms."
    # ))
    term_dfs <- setNames(lengths(term_groups), names(term_groups))
    term_groups <- setNames(nm = names(term_groups))
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
