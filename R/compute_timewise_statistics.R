#' Fit Julia mixed models to each time point of a time series data
#'
#' @inheritParams jlmer
#' @param statistic Test statistic for calculating cluster mass.
#'  Can be one of `"t"` (default) from the regression model output or
#'  `"chisq"` from a likelihood ratio test (takes about twice as long to calculate).
#' @param ... Optional arguments passed to Julia for model fitting.
#'  Defaults to `fast = TRUE` and `progress = FALSE`.
#'
#' @seealso [jlmer()], [make_jlmer_spec()]
#'
#' @return A predictor-by-time matrix of cluster statistics.
#' @export
compute_timewise_statistics <- function(jlmer_spec, family = c("gaussian", "binomial"), statistic = c("t", "chisq"), ...) {

  family <- match.arg(family)
  statistic <- match.arg(statistic)
  is_mem <- jlmer_spec$meta$is_mem
  term_groups <- augment_term_groups(jlmer_spec$meta$term_groups, statistic)
  args <- prep_for_jlmer(jlmer_spec$formula$jl, jlmer_spec$data, time = jlmer_spec$meta$time, family, ...)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }


  out <- JuliaConnectoR::juliaGet(do.call(.jlmerclusterperm$compute_timewise_statistics,
                                          c(args, term_groups, statistic, is_mem, opts)))

  alert_diagnostics(jlmer_spec, out)

  dimnames(out$t_matrix) <- out[c("Predictor", "Time")]
  out$t_matrix <- out$t_matrix[out$Predictor != "1", , drop = FALSE]

  structure(out$t_matrix, class = "timewise_statistics",
            statistic = statistic, term_groups = jlmer_spec$meta$term_groups)

}

alert_diagnostics <- function(jlmer_spec, out) {
  if (jlmer_spec$meta$is_mem) {
    if (any(out$convergence_failures)) {
      cli::cli_alert_warning("{.val {sum(out$convergence_failures)}} convergence failure{?s}.")
    }
    singular_fits <- out$singular_fits
    cli::cli_alert_info("{.val {sum(singular_fits)}} singular fit{?s} ({round(mean(singular_fits) * 100, 2)}%).")
    re_n_terms <- sapply(lme4::findbars(jlmer_spec$formula$jl), function(x) stats::setNames(length(x[[2]]), deparse1(x[[3]])))
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
