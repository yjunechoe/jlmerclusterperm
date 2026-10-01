testthat::skip_on_cran()

jlmerclusterperm_setup(cache_dir = tempdir(), restart = FALSE, verbose = FALSE)

spec <- make_jlmer_spec(
  weight ~ 1 + Diet, subset(ChickWeight, Time <= 20),
  subject = "Chick", time = "Time"
)

test_that("is_setup() is TRUE after setup", {
  expect_true(is_setup())
})

test_that("Julia functions error informatively after the session is stopped", {
  JuliaConnectoR::stopJulia()
  expect_false(is_setup())
  err <- expect_error(clusterpermute(spec, threshold = 2, nsim = 2), "jlmerclusterperm_setup")
  expect_match(deparse(conditionCall(err))[1], "clusterpermute")
})

test_that("Julia functions error informatively in a Julia session not started by setup", {
  JuliaConnectoR::juliaEval("1")
  expect_false(is_setup())
  expect_error(compute_timewise_statistics(spec), "jlmerclusterperm_setup")
  expect_error(set_rng_state(1), "jlmerclusterperm_setup")
})

test_that("setup with `restart = FALSE` sets up a session that is not ready", {
  jlmerclusterperm_setup(cache_dir = tempdir(), restart = FALSE, verbose = FALSE)
  expect_true(is_setup())
})

# Leave a working session for the remaining test files
if (!is_setup()) jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
