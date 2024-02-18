#' Test the probability of cluster-mass statistics over a range of threshold values
#'
#' @param steps A vector of threshold values to test
#' @inheritParams extract_empirical_clusters
#' @inheritParams extract_null_cluster_dists
#' @inheritParams calculate_clusters_pvalues
#' @param progress Whether to display a progress bar
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' # Specification object
#' spec <- make_jlmer_spec(
#'   weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
#'   subject = "Chick", time = "Time"
#' )
#' spec
#'
#' # Compute timewise statistics for the observed and permuted data
#' empirical_statistics <- compute_timewise_statistics(spec)
#' null_statistics <- permute_timewise_statistics(spec, nsim = 100)
#'
#' # Test cluster mass/probability under different threshold values
#' walk_threshold_steps(empirical_statistics, null_statistics, steps = 1:3,
#'                      progress = FALSE)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return A data frame of predictor clusters-mass statistics by threshold.
#' @export
walk_threshold_steps <- function(empirical_statistics, null_statistics, steps,
                                 top_n = Inf, binned = FALSE, add1 = TRUE, progress = TRUE) {
  test_threshold <- function(threshold) {
    empirical <- extract_empirical_clusters(empirical_statistics, threshold = threshold, binned = binned, top_n = top_n)
    if (all(attr(empirical, "missing_clusters"))) {
      return(NULL)
    }
    null <- extract_null_cluster_dists(null_statistics, threshold = threshold, binned = binned)
    out <- tidy(calculate_clusters_pvalues(empirical, null, add1 = add1))
    out[!is.na(out$pvalue), ]
  }
  if (progress) {
    i_vec <- cli::cli_progress_along(steps)
  } else {
    i_vec <- seq_along(steps)
  }
  res <- lapply(i_vec, function(i) {
    out <- test_threshold(steps[i])
    if (!is.null(out)) out$threshold <- steps[i]
    out
  })

  res_df <- do.call(rbind.data.frame, res)
  res_df[, c("threshold", "predictor", "id", "start", "end", "length", "sum_statistic", "pvalue")]
}
