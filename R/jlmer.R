#' Fit a Julia mixed model
#'
#' @param jlmer_data Data prepped for jlmer from `prep_for_jlmer()`
#' @param family A GLM family. Currently supports "gaussian" and "binomial".
#' @param ... Optional arguments to fit (`fast`, `nAGQ`, etc.)
#'
#' @seealso prep_jlmer_data
#'
#' @export
jlmer <- function(jlmer_data, family = c("gaussian", "binomial"), ...) {

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_data$formula$jl, jlmer_data$data, time = NULL, family, ...)[-3]

  out <- do.call(.jlmerclusterperm$jlmer, c(args, jlmer_data$meta$is_mem, ...))
  out

}

#' Fit a Julia mixed model using lmer syntax
#'
#' @param formula Model formula in lme4 syntax
#' @param data Dataframe
#' @param prep_jlmer_opts Other options passed to `prep_jlmer_data()`
#' @inheritParams jlmer
#'
#' @seealso prep_jlmer_data
#'
#' @export
to_jlmer <- function(formula, data, family = c("gaussian", "binomial"), prep_jlmer_opts = list(), ...) {

  jlmer_data <- do.call(prep_jlmer_data, utils::modifyList(prep_jlmer_opts, list(fm = formula, df = data)))
  jlmer(jlmer_data, family, ...)

}

#' Fit Julia mixed models to each time point of a time series data
#'
#' @inheritParams jlmer
#' @param ... Optional arguments to fit. Defaults to `fast = TRUE` and `progress = FALSE`.
#'
#' @seealso prep_jlmer_data
#'
#' @return A Predictor x Time matrix of z-values
#' @export
jlmer_by_time <- function(jlmer_data, family = c("gaussian", "binomial"), ...) {

  family <- match.arg(family)
  args <- prep_for_jlmer(jlmer_data$formula$jl, jlmer_data$data, time = jlmer_data$meta$time, family, ...)

  opts <- list(...)
  opts <- utils::modifyList(list(progress = FALSE), opts)
  if (family == "binomial") {
    opts <- utils::modifyList(list(fast = TRUE), opts)
  }

  is_mem <- jlmer_data$meta$is_mem

  out <- JuliaConnectoR::juliaGet(do.call(.jlmerclusterperm$jlmer_by_time, c(args, is_mem, opts)))

  alert_diagnostics(jlmer_data, out)

  dimnames(out$z_matrix) <- out[c("Predictors", "Time")]
  out$z_matrix <- out$z_matrix[out$Predictors != "1", , drop = FALSE]

  out$z_matrix

}

alert_diagnostics <- function(jlmer_data, out) {
  if (jlmer_data$meta$is_mem) {
    if (any(out$convergence_failures)) {
      cli::cli_alert_warning("{.val {sum(out$convergence_failures)}} convergence failure{?s}.")
    }
    singular_fits <- out$singular_fits
    cli::cli_alert_info("{.val {sum(singular_fits)}} singular fit{?s} ({round(mean(singular_fits) * 100, 2)}%).")
    re_n_terms <- sapply(lme4::findbars(jlmer_data$formula$jl), function(x) stats::setNames(length(x[[2]]), deparse1(x[[3]])))
    if (mean(singular_fits) > .2 && any(re_n_terms > 1)) {
      cli::cli_alert_info("Average number of components estimated to capture 95% of RE variance:")
      rePCs <- rowMeans(out$rePCA_95_matrix)
      rePCs_info <- cli::cli_ul()
      lapply(seq_along(out$Grouping), function(i) {
        if (re_n_terms[out$Grouping[i]] > 1) cli::cli_li("{out$Grouping[i]}: {sprintf('%.01f', rePCs[i])}")
      })
      cli::cli_end(rePCs_info)
    }
  }
}
