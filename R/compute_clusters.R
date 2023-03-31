#' Detect largest clusters from the time series data
#'
#' @param t_matrix A predictor-by-time matrix of cluster statistics.
#' @param threshold z-value threshold. Defaults to `1.5`.
#' @param binned Whether time points have been aggregated. Defaults to `TRUE` which allows length-1 clusters.
#' @param top_n How many clusters to return, ordered by the size of the cluster statistic.
#'   Defaults to `1L` which returns the largest cluster. Use `NULL` to return all clusters.
#'
#' @seealso [jlmer_by_time()]
#'
#' @export
compute_empirical_clusters <- function(t_matrix, threshold = 1.5, binned = TRUE, top_n = 1L) {
  t_matrix[abs(t_matrix) <= abs(threshold)] <- 0
  predictors <- rownames(t_matrix)
  n <- as.integer(max(top_n %|0|% ncol(t_matrix), 1))
  largest_clusters <- .jlmerclusterperm$compute_largest_clusters(t_matrix, binned, n)
  cluster_dfs <- df_from_DF(largest_clusters)
  empirical_clusters <- split(cluster_dfs[, -5], predictors[cluster_dfs$id])
  structure(empirical_clusters, class = "empirical_clusters")
}

#' Construct a distribution of the largest cluster statistics from bootstrapped permutations
#'
#' @param t_array A simulation-by-time-by-predictor 3D array of cluster statistics.
#' @inheritParams compute_empirical_clusters
#'
#' @seealso [clusterpermute()]
#'
#' @export
compute_largest_null_clusters <- function(t_array, threshold = 1.5, binned = TRUE) {
  t_array[abs(t_array) <= abs(threshold)] <- 0
  null_clusters <- apply(t_array, 3, function(t_matrix) {
    t_matrix <- t_matrix[!is.nan(rowSums(t_matrix)),]
    largest_clusters <- df_from_DF(.jlmerclusterperm$compute_largest_clusters(t_matrix, binned, 1L))
  }, simplify = FALSE)
  structure(null_clusters, class = "null_clusters")
}

#' Calculate bootstrapped p-values of clusters
#'
#' @param empirical_clusters A `empirical_clusters` object
#' @param null_clusters A `null_clusters` object
#'
#' @seealso [compute_empirical_clusters()], [compute_largest_null_clusters()]
#'
#' @export
clusters_pvalue <- function(empirical_clusters, null_clusters) {
  empirical_statistics <- lapply(empirical_clusters, `[[`, "statistic")
  null_distribution <- lapply(null_clusters, `[[`, "statistic")
  sapply(names(null_distribution), function(x) mean(abs(null_distribution[[x]]) >= abs(empirical_statistics[[x]])))
}
