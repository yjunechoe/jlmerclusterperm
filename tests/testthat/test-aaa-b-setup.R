skip_conditionally()

#' @srrstats {G5.2} Appropriate message/warning/error tests are in `/tests/testthat`

# test_that("Setup works", {
#   expect_true(jlmerclusterperm_setup())
# })

test_that("Setup with seed works (use 2 for testing)", {
  options("jlmerclusterperm.nthreads" = 2)
  expect_true(jlmerclusterperm_setup(cache_dir = tempdir(), restart = TRUE))
  expect_true(file.exists(file.path(tempdir(), "julia", "Manifest.toml")))
  expect_equal(JuliaConnectoR::juliaEval("Threads.nthreads()"), 2)
})

test_that("RNG initializes to seed=1 counter=0", {
  expect_equal(get_rng_seed(), 1)
  expect_equal(get_rng_state(), 0)
})
