skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

spec <- make_jlmer_spec(
  weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
  subject = "Chick", time = "Time"
)

empirical_ts <- compute_timewise_statistics(spec, statistic = "t")
empirical_chisqs <- compute_timewise_statistics(spec, statistic = "chisq")

test_that("chisq bound by 0-1", {
  expect_error(extract_empirical_clusters(empirical_chisqs, threshold = 2))
})

test_that("dims of empirical stats", {
  expect_equal(dim(empirical_ts), c(3, 11))
  expect_equal(dim(empirical_chisqs), c(1, 11))
})

test_that("tidy dims of empirical stats", {
  expect_equal(dim(tidy(empirical_ts)), c(33, 3))
  expect_equal(dim(tidy(empirical_chisqs)), c(11, 3))
})

null_ts <- permute_timewise_statistics(spec, statistic = "t")
null_chisqs <- permute_timewise_statistics(spec, statistic = "chisq")

test_that("dims of null stats", {
  expect_equal(dim(null_ts), c(100, rev(dim(empirical_ts))))
  expect_equal(dim(null_chisqs), c(100, rev(dim(empirical_chisqs))))
})

test_that("tidy dims of null stats", {
  expect_equal(dim(tidy(null_ts)), c(nrow(tidy(empirical_ts)) * 100, 4))
  expect_equal(dim(tidy(null_chisqs)), c(nrow(tidy(empirical_chisqs)) * 100, 4))
})
