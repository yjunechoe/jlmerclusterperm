testthat::skip_on_cran()

jlmerclusterperm_setup(cache_dir = tempdir(), restart = FALSE, verbose = FALSE)

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

# Example 2: a within-participant predictor constant within (Chick, Trial) units
chickweights_df2 <- transform(chickweights_df, Trial = ifelse(Time %% 4 < 2, "a", "b"))
chickweights_df2$Half <- ifelse(xor(chickweights_df2$Trial == "a", chickweights_df2$DietInt %% 2 == 0), -0.5, 0.5)
chickweights_spec3 <- make_jlmer_spec(
  formula = weight ~ 1 + Half,
  data = chickweights_df2,
  subject = "Chick", trial = "Trial", time = "Time"
)

test_that("within-participant shuffling preserves trial and participant structure", {
  expect_message(permuted <- permute_by_predictor(chickweights_spec3, predictors = "Half", n = 5), "within_participant")
  for (sim in split(permuted, permuted$id)) {
    # each trial keeps a single value across its time series
    expect_true(all(tapply(sim$Half, paste(sim$Chick, sim$Trial), function(x) length(unique(x))) == 1))
    # each participant keeps the same set of values
    original <- chickweights_spec3$data
    values_by_chick <- function(df) {
      out <- lapply(split(df$Half, as.character(df$Chick)), function(x) sort(unique(x)))
      out[order(names(out))]
    }
    expect_equal(values_by_chick(sim), values_by_chick(original))
  }
})

test_that("informative errors when a predictor is not constant within shuffled units", {
  # within-participant predictor forced to be shuffled between participants
  expect_error(
    permute_by_predictor(chickweights_spec3, predictors = "Half", predictor_type = "between_participant"),
    "values vary within Chick"
  )
  # predictor varies over time within a trial
  chickweights_df3 <- transform(chickweights_df2, Half = ifelse(Time %% 8 < 4, -0.5, 0.5))
  chickweights_spec4 <- make_jlmer_spec(
    formula = weight ~ 1 + Half,
    data = chickweights_df3,
    subject = "Chick", trial = "Trial", time = "Time"
  )
  expect_error(
    permute_by_predictor(chickweights_spec4, predictors = "Half", predictor_type = "within_participant"),
    "values vary within Chick x Trial"
  )
  # within-participant shuffling without a trial column
  chickweights_spec5 <- make_jlmer_spec(
    formula = weight ~ 1 + Half,
    data = chickweights_df2,
    subject = "Chick", time = "Time"
  )
  expect_error(
    permute_by_predictor(chickweights_spec5, predictors = "Half", predictor_type = "within_participant"),
    "requires a column for `trial`"
  )
  expect_error(permute_by_predictor(chickweights_spec5, predictors = "Half"), "no column for `trial`")
})
