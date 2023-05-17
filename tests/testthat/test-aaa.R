test_that("Setup with seed works", {
  seed_opts <- options("jlmerclusterperm.nthreads" = 2)
  expect_true(jlmerclusterperm_setup(verbose = FALSE))
  expect_equal(JuliaConnectoR::juliaEval("Threads.nthreads()"), 2)
  options(seed_opts)
})

test_that("Restart as default", {
  expect_true(jlmerclusterperm_setup(verbose = FALSE))
})

test_that("Starts with default seed of N-1", {
  expect_equal(JuliaConnectoR::juliaEval("Threads.nthreads()"), min(8, parallel::detectCores() - 1))
})

test_that("Don't restart if FALSE", {
  expect_message(jlmerclusterperm_setup(restart = FALSE))
})

test_that("RNG initializes to seed=1 counter=0", {
  expect_equal(get_rng_seed(), 1)
  expect_equal(get_rng_state(), 0)
})
