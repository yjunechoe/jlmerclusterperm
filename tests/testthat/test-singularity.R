skip_conditionally()

jlmerclusterperm_setup(restart = FALSE, verbose = FALSE)

singularity_spec <- make_jlmer_spec(
  formula = weight ~ 1 + Diet + (1 + Diet | Chick),
  data = subset(transform(ChickWeight, Diet = as.integer(Chick)), Chick %in% 1:5 & Time <= 20),
  subject = "Chick", time = "Time"
)

test_that("Informs on singularity", {
  expect_message(expect_message(expect_message(
    compute_timewise_statistics(singularity_spec),
    "singular fits"
  ), "number of components"), "Chick")
})
