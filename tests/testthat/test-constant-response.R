testthat::skip_on_cran()

jlmerclusterperm_setup(cache_dir = tempdir(), restart = FALSE, verbose = FALSE)

# Eyetracking-like data where nobody fixates the target at the first two time points
set.seed(1)
constant_df <- expand.grid(Subject = 1:10, Trial = 1:8, Time = 1:6)
constant_df$Condition <- ifelse(constant_df$Trial %% 2 == 0, 0.5, -0.5)
constant_df$Target <- rbinom(nrow(constant_df), 1, plogis(constant_df$Condition * constant_df$Time / 2))
constant_df$Target[constant_df$Time <= 2] <- 0

constant_spec <- make_jlmer_spec(
  Target ~ 1 + Condition + (1 | Subject), constant_df,
  subject = "Subject", trial = "Trial", time = "Time"
)

test_that("Mixed models tolerate time points with a constant response", {
  empirical_statistics <- expect_no_error(
    suppressMessages(compute_timewise_statistics(constant_spec, family = "binomial"))
  )
  expect_equal(dim(empirical_statistics), c(1, 6))
  expect_equal(unname(empirical_statistics[, 1:2]), c(-Inf, -Inf))
  expect_true(all(is.finite(empirical_statistics[, 3:6])))
})

test_that("Constant response time points do not count as convergence failures", {
  expect_no_message(
    compute_timewise_statistics(constant_spec, family = "binomial"),
    message = "convergence failure"
  )
})

test_that("CPA runs end-to-end on mixed models with a constant response", {
  reset_rng_state()
  CPA <- expect_no_error(
    suppressMessages(clusterpermute(constant_spec, family = "binomial", threshold = 1.5, nsim = 5, progress = FALSE))
  )
  # Infinite statistics contribute zero mass but must not coerce the cluster statistic to a list
  expect_type(CPA$empirical_clusters$Condition$statistic, "double")
  expect_type(tidy(CPA$null_cluster_dists)$sum_statistic, "double")
})

test_that("GLMs tolerate time points with a constant response", {
  glm_spec <- make_jlmer_spec(
    Target ~ 1 + Condition, constant_df,
    subject = "Subject", trial = "Trial", time = "Time"
  )
  reset_rng_state()
  expect_no_error(
    suppressMessages(clusterpermute(glm_spec, family = "binomial", threshold = 1.5, nsim = 5, progress = FALSE))
  )
})
