#' Conduct a cluster-based permutation test
#'
#' @inheritParams permute_timewise_statistics
#' @inheritParams extract_empirical_clusters
#' @inheritParams calculate_clusters_pvalues
#' @param progress Defaults to `TRUE`, which prints progress on each step of the cluster permutation test.
#'
#' @seealso [compute_timewise_statistics()], [permute_timewise_statistics()],
#'  [extract_empirical_clusters()], [extract_null_cluster_dists()],
#'  [calculate_clusters_pvalues()]
#'
#' @examplesIf JuliaConnectoR::juliaSetupOk()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Specification object
#' spec <- make_jlmer_spec(
#'   weight ~ 1 + Diet, filter(ChickWeight, Time <= 20),
#'   subject = "Chick", time = "Time"
#' )
#' spec
#'
#' # Should minimally provide `threshold` and `nsim`, in addition to the spec object
#' reset_rng_state()
#' CPA <- clusterpermute(spec, threshold = 2, nsim = 100, progress = FALSE)
#' CPA
#'
#' # CPA is a list of `<null_cluster_dists>` and `<empirical_clusters>` objects
#' sapply(CPA, class)
#'
#' # You can extract the individual components for further inspection
#' CPA$null_cluster_dists
#' CPA$empirical_clusters
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
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
                           ...,
                           progress = TRUE) {
  family <- match.arg(family)
  statistic <- match.arg(statistic)
  jlmer_spec$.backdoor$prepped_for_jlmer <- prep_for_jlmer(jlmer_spec, family, ...)
  jlmer_spec$.backdoor$augmented_term_groups <- augment_term_groups(jlmer_spec, statistic)
  if (progress) cli::cli_progress_step("Detecting empirical clusters and calculating cluster-mass statistics.")
  old_opts <- julia_progress(show = FALSE)
  empirical_statistics <- suppressMessages(compute_timewise_statistics(jlmer_spec, family, statistic, ...))
  julia_progress(show = old_opts$show)
  empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold, binned, top_n)
  if (progress) cli::cli_progress_step("Sampling cluster-mass statistics from a bootstrapped null distribution.")
  null_statistics <- permute_timewise_statistics(jlmer_spec, family, statistic, nsim, predictors, ...)
  null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold, binned)
  if (progress) cli::cli_progress_step("Calculating the probability of the observed cluster-mass statistics.")
  empirical_clusters_p <- calculate_clusters_pvalues(empirical_clusters, null_cluster_dists, add1)
  structure(list(null_cluster_dists = null_cluster_dists, empirical_clusters = empirical_clusters_p), class = "CPA_out")
}

#' @export
print.CPA_out <- function(x, ...) {
  cat(
    "$null_cluster_dists",
    format(x$null_cluster_dists, ...),
    "\n$empirical_clusters",
    format(x$empirical_clusters, ...),
    sep = "\n"
  )
}
