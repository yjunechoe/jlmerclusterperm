skip_conditionally()

#' @srrstats {G5.3} Tests for correctness but no explicit test for NA
#' @srrstats {G5.4} Tests compare to R `lm()` output.

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

spec_lm <- make_jlmer_spec(weight ~ 1 + Diet, ChickWeight)
jlm1 <- to_jlmer(weight ~ 1 + Diet, ChickWeight)
jlm2 <- jlmer(spec_lm)

#' @srrstats {RE7.3} Tests for `print()`, `tidy()`, and `glance()`
test_that("direct and indirect fits identical", {
  expect_equal(tidy(jlm1), tidy(jlm2))
  expect_equal(glance(jlm1), glance(jlm2))
})

test_that("returns julia object", {
  expect_s3_class(jlm1, "jlmer_mod")
  expect_s3_class(jlm2, "jlmer_mod")
})

spec_lme <- make_jlmer_spec(weight ~ 1 + Diet + (1 | Chick), ChickWeight)
jlme1 <- to_jlmer(weight ~ 1 + Diet + (1 | Chick), ChickWeight)
jlme2 <- jlmer(spec_lme)

test_that("direct and indirect fits identical - mixed", {
  expect_equal(tidy(jlme1), tidy(jlme2))
  expect_equal(glance(jlme1), glance(jlme2))
})

print(jlm1)
print(jlme1)
