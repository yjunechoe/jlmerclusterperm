#' Calculate bootstrapped p-values of cluster-mass statistics
#'
#' @param empirical_clusters A `empirical_clusters` object
#' @param null_cluster_dists A `null_cluster_dists` object
#' @param add1 Whether to add 1 to the numerator and denominator when calculating the p-value.
#'   Use `TRUE` to effectively count the observed statistic as part of the permuted
#'   null distribution (recommended with larger `nsim` prior to publishing results).
#'
#' @seealso [extract_empirical_clusters()], [extract_null_cluster_dists()]
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
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
#' # Make empirical clusters
#' empirical_statistics <- compute_timewise_statistics(spec)
#' empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold = 2)
#' empirical_clusters
#'
#' # Make null cluster-mass distribution
#' reset_rng_state()
#' null_statistics <- permute_timewise_statistics(spec, nsim = 100)
#' null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold = 2)
#'
#' # Significance test the empirical cluster(s) from each predictor against the simulated null
#' calculate_clusters_pvalues(empirical_clusters, null_cluster_dists)
#'
#' # Set `add1 = TRUE` to normalize by adding 1 to numerator and denominator
#' calculate_clusters_pvalues(empirical_clusters, null_cluster_dists, add1 = TRUE)
#'
#' # This sequence of procedures is equivalent to `clusterpermute()`
#' reset_rng_state()
#' clusterpermute(spec, threshold = 2, nsim = 100, progress = FALSE)
#'
#' # The empirical clusters and the null cluster-mass distribution must be comparable
#' empirical_clusters2 <- extract_empirical_clusters(empirical_statistics, threshold = 3)
#' # For example, below code errors because thresholds are different (2 vs. 3)
#' try( calculate_clusters_pvalues(empirical_clusters2, null_cluster_dists) )
#'
#' # Check for compatibility with `clusters_are_comparable()`
#' clusters_are_comparable(empirical_clusters, null_cluster_dists)
#' clusters_are_comparable(empirical_clusters2, null_cluster_dists)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @export
#' @return An `empirical_clusters` object augmented with p-values.
calculate_clusters_pvalues <- function(empirical_clusters, null_cluster_dists, add1 = FALSE) {
  clusters_are_comparable(empirical_clusters, null_cluster_dists, error = TRUE)
  empirical <- lapply(empirical_clusters, `[[`, "statistic")
  empirical <- Filter(function(x) !near_zero(x[1]), empirical)
  null <- lapply(null_cluster_dists, `[[`, "statistic")
  predictors_to_test <- setNames(nm = names(empirical)[names(empirical) %in% names(null)])
  pvalues <- lapply(predictors_to_test, function(x) {
    vapply(empirical[[x]], function(cluster_statistic) {
      mean(c(abs(null[[x]]) >= abs(cluster_statistic), if (add1) TRUE))
    }, numeric(1))
  })
  augmented <- empirical_clusters
  attr(augmented, "pvalues") <- pvalues
  augmented
}

#' @param error Whether to throw an error if incompatible
#' @rdname calculate_clusters_pvalues
#' @export
clusters_are_comparable <- function(empirical_clusters, null_cluster_dists, error = FALSE) {
  if (!inherits(empirical_clusters, "empirical_clusters") || !inherits(null_cluster_dists, "null_cluster_dists")) {
    cli::cli_abort("Can only compare object of class {.cls empirical_clusters} against object of class {.cls null_cluster_dists}")
  }
  empirical_attrs <- attributes(empirical_clusters)
  null_attrs <- attributes(null_cluster_dists)
  if (!identical(empirical_attrs[c("statistic", "threshold")], null_attrs[c("statistic", "threshold")])) {
    cluster_properties <- sapply(list(empirical_attrs, null_attrs), `[`, c("statistic", "threshold"))
    mismatch_info <- apply(cluster_properties, 1L, function(x) {
      if (x[[1]] != x[[2]]) {
        x <- unlist(x, use.names = FALSE)
        if (is.numeric(x)) x <- paste0("{", x, "}")
        paste0("empirical uses {.val ", x[1], "} but null uses {.val ", x[2], "}.")
      }
    })
    mismatch_info <- Filter(Negate(is.null), mismatch_info)
    mismatch_info <- setNames(paste0("{.strong ", names(mismatch_info), "}: ", mismatch_info), rep("x", length(mismatch_info)))
    if (error) {
      cli::cli_abort(c(
        "Cluster-mass statistics between empirical and null are not comparable.",
        "i" = "{.arg empirical_clusters} and {.arg null_cluster_dists} must share the same {.code statistic} and {.code threshold}.",
        mismatch_info
      ))
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
