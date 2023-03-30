#' Detect largest clusters from the time series data
#'
#' @param t_matrix A predictor-by-time matrix of cluster statistics.
#' @param threshold z-value threshold. Defaults to `1.5`.
#' @param binned Whether time points have been aggregated. Defaults to `TRUE` which allows length-1 clusters.
#' @param top_n How many clusters to return, ordered by the size of the cluster statistic.
#'   Defaults to `1L` which returns the largest cluster. Use `NULL` to return all clusters.
#'
#' @seealso jlmer_by_time
#'
#' @export
compute_empirical_clusters <- function(t_matrix, threshold = 1.5, binned = TRUE, top_n = 1L) {
  t_matrix[abs(t_matrix) <= abs(threshold)] <- 0
  predictors <- rownames(t_matrix)
  n <- as.integer(top_n %|0|% ncol(t_matrix))
  largest_clusters <- .jlmerclusterperm$compute_largest_clusters(t_matrix, binned, n)
  cluster_dfs <- rbind_DFs(JuliaConnectoR::juliaGet(largest_clusters))
  empirical_clusters <- split(cluster_dfs[, -1], predictors[cluster_dfs$.id])
  structure(empirical_clusters, class = "empirical_clusters")
}

#' Construct a distribution of the largest cluster statistics from bootstrapped permutations
#'
#' @param t_array A simulation-by-time-by-predictor 3D array of cluster statistics.
#' @inheritParams compute_empirical_clusters
#'
#' @seealso clusterperm
#'
#' @export
compute_largest_null_clusters <- function(t_array, threshold = 1.5, binned = TRUE) {
  null_clusters <- apply(t_array, 3, function(t_matrix) {
    t_matrix <- t_matrix[!is.nan(rowSums(t_matrix)),]
    rbind_DFs(JuliaConnectoR::juliaGet(.jlmerclusterperm$compute_largest_clusters(t_matrix, 1.5, 1L)))
  }, simplify = FALSE)
  structure(null_clusters, class = "null_clusters")
}
