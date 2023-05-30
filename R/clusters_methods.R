#' @export
print.empirical_clusters <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

#' @export
format.empirical_clusters <- function(x, ...) {
  pvalues <- attr(x, "pvalues")
  missing_clusters <- attr(x, "missing_clusters")
  term_groups <- attr(x, "term_groups")
  predictor_dfs <- attr(term_groups, "dfs")
  time <- attr(x, "time")
  statistic <- attr(x, "statistic")
  threshold <- attr(x, "threshold")
  binned <- attr(x, "binned")
  zero_clusters <- x[missing_clusters]
  valid_clusters <- x[!missing_clusters]
  if (!is.null(pvalues)) valid_clusters <- valid_clusters[names(pvalues)]
  cli::cli_format_method(
    {
      cli::cli_rule(left = paste("{.strong Empirical clusters}", format_threshold(statistic)), right = "{.cls empirical_clusters}")
      for (i in seq_along(valid_clusters)) {
        predictor <- names(valid_clusters)[[i]]
        cli::cli_text("{.el {predictor}}", if (statistic == "chisq") " ({.emph df = {predictor_dfs[[predictor]]}}){?*}")
        cluster_df <- valid_clusters[[i]]
        clusters <- split(cluster_df, seq_len(nrow(cluster_df)))
        names(clusters) <- paste0("[", time[cluster_df$cluster_start], ", ", time[cluster_df$cluster_end], "]")
        clusters <- lapply(clusters, function(cluster) sprintf("%0.3f", cluster$statistic))
        if (clusters[[1]] != "0.00") {
          if (!is.null(pvalues)) {
            clusters[] <- lapply(seq_along(clusters), function(cluster_ind) {
              pval <- pvalues[[i]][[cluster_ind]]
              paste(clusters[[cluster_ind]], sprintf(paste0("{.", ifelse(pval < 0.05, "emph", "lemph"), " (p=%0.4f)}"), round(pval, 4)))
            })
          }
          cli::cli_ul()
          cli::cli_dl(clusters)
          cli::cli_end()
        }
      }
      cli::cli_rule()
      if (length(zero_clusters) > 0) {
        cli::cli_alert_warning("No clusters found for {.el {names(zero_clusters)}}")
      }
      if (statistic == "chisq" && any(predictor_dfs > 1)) {
        cli::cli_alert_info(c("* The {.val chisq} statistic for multi-level factors are unsigned. Use {.arg statistic = {.val t}} for a more interpretable result."))
      }
    },
    theme = .jlmerclusterperm$cli_theme
  )
}

#' @export
print.null_cluster_dists <- function(x, levels = 0.95, ...) {
  cat(format(x, levels, ...), sep = "\n")
}

#' @export
format.null_cluster_dists <- function(x, levels = 0.95, ...) {
  term_groups <- attr(x, "term_groups")
  predictor_dfs <- attr(term_groups, "dfs")
  statistic <- attr(x, "statistic")
  threshold <- attr(x, "threshold")
  binned <- attr(x, "binned")
  cluster_stats <- lapply(x, extract_null_cluster_stats, levels)
  cli::cli_format_method(
    {
      cli::cli_rule(left = paste("{.strong Null cluster-mass distribution}", format_threshold(statistic)), right = "{.cls null_cluster_dists}")
      for (i in seq_along(cluster_stats)) {
        predictor <- names(x)[[i]]
        cli::cli_text("{.el {predictor}} (n = {cluster_stats[[i]]$n}", if (statistic == "chisq") ", {.emph df = {predictor_dfs[[predictor]]}}{?*}", ")")
        cli::cli_ul()
        cli::cli_dl(cluster_stats[[i]][1:2])
        cli::cli_end()
      }
      cli::cli_rule()
      if (statistic == "chisq" && any(predictor_dfs > 1)) {
        cli::cli_alert_info(c("* The {.val chisq} statistic for multi-level factors are unsigned. Use {.arg statistic = {.val t}} for a more interpretable result."))
      }
    },
    theme = .jlmerclusterperm$cli_theme
  )
}

extract_null_cluster_stats <- function(x, levels) {
  statistics <- x$statistic
  mean_se <- do.call(sprintf, c("%0.3f (%0.2f)", lapply(list(mean, stats::sd), function(f) f(statistics))))
  cis <- paste(vapply(levels, function(prob) {
    percent <- paste0(prob * 100, "%")
    bounds <- ((1 - prob) / 2) + c(0, prob)
    interval <- stats::quantile(statistics, bounds)
    paste(percent, sprintf("[%0.3f, %0.3f]", interval[1], interval[2]))
  }, character(1)), collapse = ", ")
  list("Mean (SD)" = mean_se, "Coverage intervals" = cis, n = nrow(x))
}

format_threshold <- function(statistic) {
  if (statistic == "t") {
    "(t > {.val {threshold}})"
  } else if (statistic == "chisq") {
    "(chisq p < {.val {threshold}})"
  }
}
