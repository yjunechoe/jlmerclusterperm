#' Conduct a cluster-based permutation test
#'
#' @inheritParams permute_timewise_statistics
#' @inheritParams extract_empirical_clusters
#' @inheritParams calculate_clusters_pvalues
#'
#' @seealso [compute_timewise_statistics()], [permute_timewise_statistics()],
#'  [extract_empirical_clusters()], [extract_null_cluster_dists()],
#'  [calculate_clusters_pvalues()]
#'
#' @export
#' @return A list of `null_cluster_dists` and `empirical_clusters` with p-values
clusterpermute <- function(jlmer_spec,
                           family = c("gaussian", "binomial"),
                           statistic = c("t", "chisq"),
                           threshold,
                           nsim = 100L,
                           predictors = NULL,
                           binned = FALSE,
                           top_n = Inf,
                           add1 = TRUE,
                           ...) {
  family <- match.arg(family)
  statistic <- match.arg(statistic)
  jlmer_spec$.backdoor$prepped_for_jlmer <- prep_for_jlmer(jlmer_spec, family, ...)
  jlmer_spec$.backdoor$augmented_term_groups <- augment_term_groups(jlmer_spec, statistic)
  cli::cli_progress_step("Detecting empirical clusters and calculating cluster-mass statistics.")
  old_opts <- julia_progress(show = FALSE)
  empirical_statistics <- suppressMessages(compute_timewise_statistics(jlmer_spec, family, statistic, ...))
  julia_progress(show = old_opts$show)
  empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold, binned, top_n)
  cli::cli_progress_step("Sampling cluster-mass statistics from a bootstrapped null distribution.")
  null_statistics <- permute_timewise_statistics(jlmer_spec, family, statistic, nsim, predictors, ...)
  null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold, binned)
  cli::cli_progress_step("Calculating the probability of the observed cluster-mass statistics.")
  empirical_clusters_p <- calculate_clusters_pvalues(empirical_clusters, null_cluster_dists, add1)
  list(null_cluster_dists = null_cluster_dists, empirical_clusters = empirical_clusters_p)
}
