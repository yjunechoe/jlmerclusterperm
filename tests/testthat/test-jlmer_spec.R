test_that("Minimal specification without grouping structures allowed", {
  expect_equal(names(make_jlmer_spec(weight ~ 1 + Diet, ChickWeight)), c("formula", "data", "meta"))
  expect_equal(names(make_jlmer_spec(weight ~ 1 + Diet + (1 | Chick), ChickWeight)), c("formula", "data", "meta"))
})

test_that("Inform misspecifications in grouping structure", {
  expect_message(make_jlmer_spec(weight ~ 1 + Diet, subset(ChickWeight, Time <= 20), time = "Time"))
  expect_message(make_jlmer_spec(weight ~ 1 + Diet, ChickWeight, subject = "Chick", time = "Time"))
})

test_that("Warn uneven time sampling rate", {
  expect_message(make_jlmer_spec(weight ~ 1 + Diet, ChickWeight, subject = "Chick", time = "Time"), "Sampling rate")
})
