#' Permute data while respecting grouping structure(s) of observations
#'
#' @inheritParams jlmer
#' @param predictors A vector of terms from the model. If multiple, the must form the levels of one predictor.
#' @param predictor_type Whether the predictor is `"between_participant"` or `"within_participant"`. Defaults to `"guess"`.
#' @param n Number of permuted samples of the data to generate. Defaults to `1L`.
#'
#' @examplesIf julia_setup_ok()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' # Example data setup
#' chickweights_df <- ChickWeight
#' chickweights_df <- chickweights_df[chickweights_df$Time <= 20, ]
#' chickweights_df$DietInt <- as.integer(chickweights_df$Diet)
#' head(chickweights_df)
#'
#' # Example 1: Spec object using the continuous `DietInt` predictor
#' chickweights_spec1 <- make_jlmer_spec(
#'   formula = weight ~ 1 + DietInt,
#'   data = chickweights_df,
#'   subject = "Chick", time = "Time"
#' )
#' chickweights_spec1
#'
#' # Shuffling `DietInt` values guesses `predictor_type = "between_participant"`
#' reset_rng_state()
#' spec1_perm1 <- permute_by_predictor(chickweights_spec1, predictors = "DietInt")
#' # This calls the same shuffling algorithm for CPA in Julia, so counter is incremented
#' get_rng_state()
#'
#' # Shuffling under shared RNG state reproduces the same permutation of the data
#' reset_rng_state()
#' spec1_perm2 <- permute_by_predictor(chickweights_spec1, predictors = "DietInt")
#' identical(spec1_perm1, spec1_perm2)
#'
#' # Example 2: Spec object using the multilevel `Diet` predictor
#' chickweights_spec2 <- make_jlmer_spec(
#'   formula = weight ~ 1 + Diet,
#'   data = chickweights_df,
#'   subject = "Chick", time = "Time"
#' )
#' chickweights_spec2
#'
#' # Levels of a category are automatically shuffled together
#' reset_rng_state()
#' spec2_perm1 <- permute_by_predictor(chickweights_spec2, predictors = "Diet2")
#' reset_rng_state()
#' spec2_perm2 <- permute_by_predictor(chickweights_spec2, predictors = c("Diet2", "Diet3", "Diet4"))
#' identical(spec2_perm1, spec2_perm2)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return A long dataframe of permuted re-samples with `.id` column representing replication IDs.
#' @export
permute_by_predictor <- function(jlmer_spec, predictors, predictor_type = c("guess", "between_participant", "within_participant"), n = 1L) {
  df <- jlmer_spec$data
  df_jl <- JuliaConnectoR::juliaCall("DataFrame", as.data.frame(df))
  subject <- jlmer_spec$meta$subject
  trial <- jlmer_spec$meta$trial
  predictor_type <- match.arg(predictor_type)
  if (predictor_type == "guess") {
    predictor_type <- .jlmerclusterperm$jl$guess_shuffle_as(df_jl, predictors, subject, trial)
    cli::cli_alert_info("Guessed {.arg predictor_type} to be {.val {predictor_type}}")
  }
  predictor_group <- Filter(function(x) any(predictors %in% x), jlmer_spec$meta$term_groups)
  if (length(predictor_group) > 1) {
    cli::cli_abort("You can only shuffle levels of one factor at a time.")
  } else {
    predictor_group <- predictor_group[[1]]
    if (!all(predictor_group %in% predictors)) {
      cli::cli_alert_info("Shuffling all levels of the factor together ({.val {predictor_group}})")
    }
    predictors <- predictor_group
  }
  shuffled <- df_from_DF(.jlmerclusterperm$jl$permute_by_predictor(
    df_jl, predictor_type, predictors, subject, trial, as.integer(n), .jlmerclusterperm$get_jl_opts()[[1]]
  ))
  class(shuffled) <- class(df)
  shuffled
}
