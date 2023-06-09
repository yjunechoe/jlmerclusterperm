skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)
reset_rng_state()

spec <- make_jlmer_spec(
  weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
  subject = "Chick", time = "Time"
)
empirical_statistics <- compute_timewise_statistics(spec)
null_statistics <- permute_timewise_statistics(spec, nsim = 100)

test_that("tests all steps", {
  out <- walk_threshold_steps(empirical_statistics, null_statistics, steps = 1:3)
  expect_equal(sort(unique(out$threshold)), 1:3)
})
