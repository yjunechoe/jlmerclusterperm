#' Detect largest clusters from a time sequence of predictor statistics
#'
#' @param empirical_statistics A predictor-by-time matrix of empirical timewise statistics.
#' @param threshold The threshold value that the statistic must pass to contribute to cluster mass.
#'  Interpretation differs on the choice of statistic (more below):
#'  * If `statistic = "t"`, the threshold for t-value (beta/std.err) from the regression model.
#'  * If `statistic = "chisq"`, the threshold for the p-value of chi-squared statistics from likelihood ratio tests.
#' @param binned Whether the data has been aggregated/collapsed into time bins. Defaults to `FALSE`,
#'  which requires a cluster to span at least two time points. If `TRUE`, allows length-1 clusters to exist.
#' @param top_n How many clusters to return, in the order of the size of the cluster-mass statistic.
#'  Defaults to `Inf` which return all detected clusters.
#'
#' @seealso [compute_timewise_statistics()]
#'
#' @return An `empirical_clusters` object.
#' @export
extract_empirical_clusters <- function(empirical_statistics, threshold, binned = FALSE, top_n = Inf) {
  time <- dimnames(empirical_statistics)$Time
  statistic <- attr(empirical_statistics, "statistic")
  empirical_statistics <- apply_threshold(empirical_statistics, statistic, threshold)
  predictors <- rownames(empirical_statistics)
  n <- as.integer(max(min(top_n, ncol(empirical_statistics)), 1))
  largest_clusters <- .jlmerclusterperm$extract_clusters(empirical_statistics, binned, n)
  cluster_dfs <- df_from_DF(largest_clusters)
  empirical_clusters <- split(cluster_dfs[, -5], predictors[cluster_dfs$id])[predictors]
  empirical_clusters <- lapply(empirical_clusters, function(cluster_df) {
    cluster_df[order(cluster_df$cluster_id), ]
  })
  missing_clusters <- sapply(empirical_clusters, function(x) all(near_zero(x$statistic)))
  structure(empirical_clusters,
    class = "empirical_clusters",
    missing_clusters = missing_clusters, statistic = statistic, threshold = threshold,
    binned = binned, time = time,
    term_groups = attr(empirical_statistics, "term_groups")
  )
}

#' Construct a null distribution of cluster-mass statistics
#'
#' @param null_statistics A simulation-by-time-by-predictor 3D array of null (permuted) timewise statistics.
#' @inheritParams extract_empirical_clusters
#'
#' @seealso [permute_timewise_statistics()]
#'
#' @return A `null_cluster_dists` object.
#' @export
extract_null_cluster_dists <- function(null_statistics, threshold, binned = FALSE) {
  time <- dimnames(null_statistics)$Time
  statistic <- attr(null_statistics, "statistic")
  null_statistics <- apply_threshold(null_statistics, statistic, threshold)
  null_cluster_dists <- apply(null_statistics, 3, function(t_matrix) {
    t_matrix <- t_matrix[!is.nan(rowSums(t_matrix)), ]
    largest_clusters <- df_from_DF(.jlmerclusterperm$extract_clusters(t_matrix, binned, 1L))
  }, simplify = FALSE)
  structure(null_cluster_dists,
    class = "null_cluster_dists",
    statistic = statistic, threshold = threshold, binned = binned, time = time,
    term_groups = attr(null_statistics, "term_groups")
  )
}

#' @keywords internal
apply_threshold <- function(timewise_statistics, statistic, threshold) {
  if (statistic == "t") {
    timewise_statistics[abs(timewise_statistics) <= abs(threshold)] <- 0
  } else if (statistic == "chisq") {
    threshold_dict <- chisq_threshold_dict(attr(timewise_statistics, "term_groups"), threshold)
    predictors <- dimnames(timewise_statistics)$Predictor
    for (predictor in predictors) {
      predictor_ind <- slice.index(timewise_statistics, "Predictor") == match(predictor, predictors)
      predictor_statistics <- timewise_statistics[predictor_ind]
      predictor_statistics[abs(predictor_statistics) <= abs(threshold_dict[threshold_dict$term == predictor, "threshold"])] <- 0
      timewise_statistics[predictor_ind] <- predictor_statistics
    }
  }
  timewise_statistics
}

#' @keywords internal
chisq_threshold_dict <- function(term_groups, threshold) {
  if (threshold < 0 || threshold > 1) {
    cli::cli_abort("{.arg threshold} must be between {.val {0}} and {.val {1}} when {.arg statistic = {.val chisq}}")
  }
  threshold_dict <- utils::stack(attr(term_groups, "dfs"))
  colnames(threshold_dict) <- c("df", "term")
  threshold_dict$df <- as.integer(threshold_dict$df)
  threshold_dict$threshold <- stats::qchisq(p = 1 - threshold, df = threshold_dict$df)
  threshold_dict
}
