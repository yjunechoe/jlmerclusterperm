skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

test_that("RNG counter setter/getter", {
  expect_equal(set_rng_state(123), 123)
  expect_equal(get_rng_state(), 123)
})

test_that("RNG seed setter/getter", {
  expect_equal(set_rng_seed(2), 2)
  expect_equal(get_rng_seed(), 2)
})

test_that("RNG restore", {
  set_rng_seed(1)
  expect_equal(reset_rng_state(), 0)
  expect_equal(get_rng_seed(), 1)
  expect_equal(get_rng_state(), 0)
})
