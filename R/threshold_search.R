#' Test the probability of cluster-mass statistics over a range of threshold values
#'
#' @param threshold_steps A vector of threshold values to test
#' @inheritParams extract_empirical_clusters
#' @inheritParams extract_null_cluster_dists
#' @inheritParams calculate_clusters_pvalues
#' @param progress Whether to display a progress bar
#'
#' @return A data frame of predictor clusters-mass statistics by threshold.
#' @export
walk_threshold_steps <- function(empirical_statistics, null_statistics, threshold_steps,
                                 top_n = Inf, binned = FALSE, add1 = TRUE, progress = TRUE) {
  test_threshold <- function(threshold) {
    empirical <- extract_empirical_clusters(empirical_statistics, threshold = threshold, binned = binned, top_n = top_n)
    if (all(attr(empirical, "missing_clusters"))) {
      return(NULL)
    }
    null <- extract_null_cluster_dists(null_statistics, threshold = threshold, binned = binned)
    out <- tidy(calculate_clusters_pvalues(empirical, null, add1 = TRUE))
    out[!is.na(out$pvalue), ]
  }
  if (progress) {
    i_vec <- cli::cli_progress_along(threshold_steps)
  } else {
    i_vec <- seq_along(threshold_steps)
  }
  res <- lapply(i_vec, function(i) {
    out <- test_threshold(threshold_steps[i])
    if (!is.null(out)) out$threshold <- threshold_steps[i]
    out
  })

  res_df <- do.call(rbind.data.frame, res)
  res_df[, c("threshold", "predictor", "id", "start", "end", "length", "sum_statistic", "pvalue")]
}
