skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

# Example 1
chickweights_df <- ChickWeight
chickweights_df <- chickweights_df[chickweights_df$Time <= 20, ]
chickweights_df$DietInt <- as.integer(chickweights_df$Diet)
chickweights_spec1 <- make_jlmer_spec(
  formula = weight ~ 1 + DietInt,
  data = chickweights_df,
  subject = "Chick", time = "Time"
)
chickweights_spec1

test_that("preserves participant structure", {
  p_str_original <- table(unique(chickweights_spec1$data[, c("Chick", "DietInt")])$DietInt)
  permuted <- permute_by_predictor(chickweights_spec1, predictors = "DietInt", predictor_type = "between_participant")
  p_str_permuted <- table(unique(permuted[, c("Chick", "DietInt")])$DietInt)
  expect_equal(p_str_original, p_str_permuted)
})

test_that("preserves temporal structure", {
  t_str_original <- unname(sort(tapply(chickweights_spec1$data$weight, chickweights_spec1$data$Chick, toString)))
  permuted <- permute_by_predictor(chickweights_spec1, predictors = "DietInt", predictor_type = "between_participant")
  t_str_permuted <- unname(sort(tapply(permuted$weight, permuted$Chick, toString)))
  expect_equal(t_str_original, t_str_permuted)
})

test_that("guesses type", {
  reset_rng_state()
  expect_message(spec1_perm1 <- permute_by_predictor(chickweights_spec1, predictors = "DietInt"), "between_participant")
})

test_that("increments counter", {
  expect_true(get_rng_state() > 0)
})

test_that("shuffling reproducibility", {
  reset_rng_state()
  spec1_perm1 <- permute_by_predictor(chickweights_spec1, predictors = "DietInt", predictor_type = "between_participant")
  reset_rng_state()
  spec1_perm2 <- permute_by_predictor(chickweights_spec1, predictors = "DietInt", predictor_type = "between_participant")
  expect_equal(spec1_perm1, spec1_perm2)
})

test_that("levels of a category shuffled together", {
  chickweights_spec2 <- make_jlmer_spec(
    formula = weight ~ 1 + Diet,
    data = chickweights_df,
    subject = "Chick", time = "Time"
  )
  reset_rng_state()
  expect_message(spec2_perm1 <- permute_by_predictor(chickweights_spec2, predictors = "Diet2", predictor_type = "between_participant"), "Diet3")
  reset_rng_state()
  spec2_perm2 <- permute_by_predictor(chickweights_spec2, predictors = c("Diet2", "Diet3", "Diet4"), predictor_type = "between_participant")
  expect_equal(spec2_perm1, spec2_perm2)
})
