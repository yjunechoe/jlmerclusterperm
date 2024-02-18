#' Fit Julia regression models to each time point of a time series data
#'
#' @inheritParams jlmer
#' @param statistic Test statistic for calculating cluster mass.
#'  Can be one of `"t"` (default) from the regression model output or
#'  `"chisq"` from a likelihood ratio test (takes about twice as long to calculate).
#' @param ... Optional arguments passed to Julia for model fitting.
#'  Defaults to `fast = TRUE` (when `family = "binomial"`) and `progress = FALSE`.
#'
#' @seealso [jlmer()], [make_jlmer_spec()]
#'
#' @srrstats {RE3.0} Issues singularity messages and excludes runs with convergence failures in permutation testing (and informs of this)
#' @srrstats {RE3.1} Convergence failures can be retrieved from function outputs, but users are encouraged to watch out for warnings and messages.
#'  These can be suppressed via the `suppress*()` functions.
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
#' # Predictor x Time matrix of t-statistics from regression output
#' empirical_statistics <- compute_timewise_statistics(spec)
#' round(empirical_statistics, 2)
#'
#' # Collect as dataframe with `tidy()`
#' empirical_statistics_df <- tidy(empirical_statistics)
#' empirical_statistics_df
#'
#' # Timewise statistics are from regression models fitted to each time point
#' # - Note the identical statistics at `Time == 0`
#' empirical_statistics_df %>%
#'   filter(time == 0)
#' to_jlmer(weight ~ 1 + Diet, filter(ChickWeight, Time == 0)) %>%
#'   tidy() %>%
#'   select(term, statistic)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return A predictor-by-time matrix of cluster statistics, of class `timewise_statistics`.
#' @export
compute_timewise_statistics <- function(jlmer_spec, family = c("gaussian", "binomial"), statistic = c("t", "chisq"), ...) {
  check_arg_class(jlmer_spec, "jlmer_spec")
  family <- match.arg(family)
  statistic <- match.arg(statistic)
  is_mem <- jlmer_spec$meta$is_mem
  term_groups <- augment_term_groups(jlmer_spec, statistic)
  args <- prep_for_jlmer(jlmer_spec, family = family, ...)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }


  out <- JuliaConnectoR::juliaGet(do.call(
    .jlmerclusterperm$jl$compute_timewise_statistics,
    c(args, term_groups$jl, statistic, is_mem, .jlmerclusterperm$get_jl_opts(), opts)
  ))

  alert_diagnostics(jlmer_spec, out)

  if (statistic == "t") {
    dimnames(out$t_matrix) <- out[c("Predictor", "Time")]
    out$t_matrix <- out$t_matrix[out$Predictor != "1", , drop = FALSE]
  } else {
    predictors <- names(term_groups$r)
    dimnames(out$t_matrix) <- c(list(Predictor = predictors[predictors != "1"]), out["Time"])
  }

  structure(out$t_matrix,
    class = "timewise_statistics",
    statistic = statistic, term_groups = term_groups$r
  )
}

alert_diagnostics <- function(jlmer_spec, out) {
  if (any(out$convergence_failures)) {
    cli::cli_alert_danger(c(
      "{.val {sum(out$convergence_failures)}} convergence failure{?s} at the following timepoint{?s}: ",
      "{.val {out$Time[out$convergence_failures]}}."
    ))
  }
  if (jlmer_spec$meta$is_mem) {
    singular_fits <- out$singular_fits
    if (any(singular_fits)) {
      cli::cli_alert_info("{.val {sum(singular_fits)}} singular fit{?s} ({round(mean(singular_fits) * 100, 2)}%).")
    }
    re_n_terms <- sapply(lme4::findbars(jlmer_spec$formula$jl), function(x) {
      setNames(length(x[[2]]), deparse1(x[[3]]))
    })
    if (mean(singular_fits) > .2 && any(re_n_terms > 1)) {
      cli::cli_alert_info("Average number of components estimated to capture 95% of RE variance:")
      rePCs <- rowMeans(out$rePCA_95_matrix)
      cli::cli_ul()
      lapply(seq_along(out$Grouping), function(i) {
        if (re_n_terms[out$Grouping[i]] > 1) cli::cli_li("{out$Grouping[i]}: {sprintf('%.01f', rePCs[i])}")
      })
      cli::cli_end()
    }
  }
}
