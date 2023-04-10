#' Test cluster-mass probabilities over a range of threshold values
#'
#' @param threshold_steps A vector of threshold values to test
#' @inheritParams extract_empirical_clusters
#' @inheritParams extract_null_cluster_dists
#' @inheritParams calculate_clusters_pvalues
#'
#' @return A data frame of clusters-mass statistics
#' @export
walk_threshold_steps <- function(empirical_statistics, null_statistics, threshold_steps,
                                 top_n = Inf, binned = FALSE, add1 = TRUE) {
  test_threshold <- function(threshold) {
    empirical <- extract_empirical_clusters(empirical_statistics, threshold = threshold, binned = binned, top_n = top_n)
    if (all(attr(empirical, "missing_clusters"))) return(NULL)
    null <- extract_null_cluster_dists(null_statistics, threshold = threshold, binned = binned)
    out <- tidy(calculate_clusters_pvalues(empirical, null, add1 = TRUE))
    out[!is.na(out$pvalue), ]
  }
  res <- vector("list", length(threshold_steps))
  i <- 1
  cli::cli_progress_step("Testing {.arg threshold = {.val {threshold_steps[i]}}} ({i}/{length(threshold_steps)})")
  for (i in seq_along(threshold_steps)) {
    cli::cli_progress_update()
    out <- test_threshold(threshold_steps[i])
    if (!is.null(out)) {
      out$threshold <- threshold_steps[i]
      res[[i]] <- out
    }
  }
  res_df <- do.call(rbind.data.frame, res)
  res_df$length <- res_df$cluster_end - res_df$cluster_start + 1
  colnames(res_df)[colnames(res_df) %in% c("cluster_start", "cluster_end")] <- c("start", "end")
  res_df[, c("threshold", "predictor", "cluster_id", "start", "end", "length", "statistic", "pvalue")]
}
