skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

#' @srrstats {G5.0} Uses the built-in `ChickWeight` dataset for tests
spec <- make_jlmer_spec(
  weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
  subject = "Chick", time = "Time"
)

#' @srrstats {G5.5} Uses shared Julia RNG state to test correctness
#' @srrstats {G5.9} Tests for stochastic nature of the CPA under different RNG states
test_that("CPAs under the same RNG state are identical", {
  reset_rng_state()
  a <- clusterpermute(spec, threshold = 2, progress = FALSE)
  reset_rng_state()
  b <- clusterpermute(spec, threshold = 2, progress = FALSE)
  expect_equal(a, b)
})

# piecemeal vs. wholesale
reset_rng_state()
wholesale <- clusterpermute(spec, threshold = 2, progress = FALSE)
reset_rng_state()
empirical_statistics <- compute_timewise_statistics(spec)
empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold = 2)
null_statistics <- permute_timewise_statistics(spec)
null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold = 2)
empirical_clusters_tested <- calculate_clusters_pvalues(empirical_clusters, null_cluster_dists, add1 = TRUE)
reset_rng_state()

test_that("Piecemeal and wholesale CPAs are identical", {
  expect_equal(wholesale$null_cluster_dists, null_cluster_dists)
  expect_equal(tidy(wholesale$null_cluster_dists), tidy(null_cluster_dists))
  expect_equal(wholesale$empirical_clusters, empirical_clusters_tested)
  expect_equal(tidy(wholesale$empirical_clusters), tidy(empirical_clusters_tested))
})

test_that("Errors on incompatible clusters", {
  expect_error(calculate_clusters_pvalues(extract_empirical_clusters(empirical_statistics, threshold = 3), null_cluster_dists))
  expect_error(calculate_clusters_pvalues(extract_empirical_clusters(compute_timewise_statistics(spec, statistic = "chisq"), threshold = .05), null_cluster_dists))
  expect_error(calculate_clusters_pvalues(empirical_clusters, extract_null_cluster_dists(null_statistics, threshold = 3)))
})

test_that("Errors on no predictors", {
  spec_intercept <- make_jlmer_spec(
    weight ~ 1, subset(ChickWeight, Time <= 20),
    subject = "Chick", time = "Time"
  )
  expect_error(clusterpermute(spec_intercept, threshold = 1.5, nsim = 1), "No predictors to permute")
  expect_error(permute_timewise_statistics(spec_intercept, threshold = 1.5, nsim = 1), "No predictors to permute")
})

# Coverage
lapply(list(empirical_statistics, empirical_clusters, null_statistics, null_cluster_dists, empirical_clusters_tested), tidy)
lapply(list(spec, empirical_statistics, empirical_clusters, null_statistics, null_cluster_dists, empirical_clusters_tested), print)
print(wholesale)
