#' @export
print.null_clusters <- function(x, levels = 0.95, ...) {
  cat(format(x, levels, ...), sep = "\n")
}

#' @export
format.null_clusters <- function(x, levels, ...) {
  cluster_stats <- lapply(x, extract_null_cluster_stats, levels)
  cli::cli_format_method({
    cli::cli_h1("null cluster statistics")
    for (i in seq_along(cluster_stats)) {
      cli::cli_text(names(x)[[i]])
      cli::cli_ul()
      cli::cli_dl(cluster_stats[[i]], labels = paste0("{.el ", names(cluster_stats[[i]]), "}"))
      cli::cli_end()
    }
    cli::cli_rule()
  }, theme = .jlmerclusterperm$cli_theme)
}

extract_null_cluster_stats <- function(x, levels) {
  statistics <- x$statistic
  n_zeros <- sum(near_zero(statistics))
  zeros <- paste0(n_zeros, " (of ", length(statistics), ")")
  mean_se <- do.call(sprintf, c("%0.3f (%0.2f)", lapply(list(mean, stats::sd), function(f) f(statistics))))
  cis <- paste(sapply(levels, function(prob) {
    percent <- paste0(prob * 100,"%")
    bounds <- ((1 - prob) / 2) + c(0, prob)
    interval <- stats::quantile(statistics, bounds)
    paste(percent, sprintf("[%0.3f, %0.3f]", interval[1], interval[2]))
  }), collapse = ", ")
  list("Mean (SD)" = mean_se, "Zero clusters" = zeros, "Coverage intervals" = cis)
}
