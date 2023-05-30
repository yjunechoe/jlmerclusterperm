skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

test_that("No side effects when called empty", {
  expect_equal(julia_progress(), julia_progress())
})

old_opts <- julia_progress()

test_that("Side effect by show, width, or both", {
  julia_progress(show = FALSE)
  expect_equal(julia_progress()$show, FALSE)
  julia_progress(width = 100)
  expect_equal(julia_progress()$width, 100)
  julia_progress(old_opts)
  expect_equal(julia_progress(), old_opts)
})

test_that("Restore startup option", {
  julia_progress(show = TRUE, width = "auto")
  expect_equal(old_opts, julia_progress())
})
