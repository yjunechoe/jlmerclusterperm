#' Permute data by predictor
#'
#' @inheritParams jlmer
#' @param predictors A vector of terms from the model. If multiple, the must form the levels of one predictor.
#' @param predictor_type Whether the predictor is `"between_participant"` or `"within_participant"`. Defaults to `"guess"`.
#' @param n Number of permuted data to generate. Defaults to `1L`.
#'
#' @return A long dataframe with `.id` column representing replication IDs.
#' @export
permute_by_predictor <- function(jlmer_spec, predictors, predictor_type = c("guess", "between_participant", "within_participant"), n = 1L) {
  df <- jlmer_spec$data
  df_jl <- JuliaConnectoR::juliaCall("DataFrame", as.data.frame(df))
  subject <- jlmer_spec$meta$subject
  trial <- jlmer_spec$meta$trial
  time <- jlmer_spec$meta$time
  predictor_type <- match.arg(predictor_type)
  if (predictor_type == "guess") {
    predictor_type <- .jlmerclusterperm$guess_shuffle_as(df_jl, predictors, subject, trial)
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
  shuffled <- df_from_DF(.jlmerclusterperm$permute_by_predictor(df_jl, predictor_type, predictors, subject, trial, as.integer(n)))
  class(shuffled) <- class(df)
  shuffled
}
