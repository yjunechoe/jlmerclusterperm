#' Run a complete cluster-based permutation analysis
#'
#' @inheritParams permute_timewise_statistics
#' @inheritParams extract_empirical_clusters
#' @inheritParams calculate_clusters_pvalues
#'
#' @seealso [compute_timewise_statistics()], [permute_timewise_statistics()],
#'  [extract_empirical_clusters()], [extract_null_cluster_dists()],
#'  [calculate_clusters_pvalues()]
#' @export
clusterpermute <- function(jlmer_spec,
                           family, statistic, threshold,
                           nsim, predictors,
                           binned, top_n,
                           add1,
                           ...) {
  empirical_statistics <- compute_timewise_statistics(jlmer_spec, family, statistic, ...)
  empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold, binned, top_n)
  null_statistics <- permute_timewise_statistics(jlmer_spec, family, statistic, nsim, predictors, ...)
  null_clusters <- extract_null_cluster_dists(null_statistics, threshold, binned)
  calculate_clusters_pvalues(empirical_clusters, null_clusters, add1)
}
