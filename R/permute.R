#' Permute data by predictor
#'
#' @inheritParams jlmer
#' @param predictors A vector of terms from the model. If multiple, the must form the levels of one predictor.
#' @param predictor_type Whether the predictor is between or within participant. Default is to guess.
#' @param n Number of permuted data to generate
#'
#' @return A long dataframe with `.id` column representing replication IDs.
#' @export
permute_by_predictor <- function(jlmer_data, predictors, predictor_type = c("guess", "between_participant", "within_participant"), n = 1L) {
  df <- jlmer_data$data
  df_jl <- JuliaConnectoR::juliaCall("DataFrame", as.data.frame(df))
  subject <- jlmer_data$meta$subject
  item <- jlmer_data$meta$item
  time <- jlmer_data$meta$time
  predictor_type <- match.arg(predictor_type)
  if (predictor_type == "guess") {
    predictor_type <- .jlmerclusterperm$guess_shuffle_as(df_jl, predictors, subject, item)
    cli::cli_alert_info("Guessed {.arg predictor_type} to be {.val {predictor_type}}")
  }
  predictor_group <- Filter(function(x) any(predictors %in% x), jlmer_data$meta$term_groups)
  if (length(predictor_group) > 1) {
    cli::cli_abort("You can only shuffle levels of one factor at a time.")
  } else {
    predictor_group <- predictor_group[[1]]
    if (!all(predictor_group %in% predictors)) {
      cli::cli_alert_info("Shuffling all levels of the factor together ({.val {predictor_group}})")
    }
    predictors <- predictor_group
  }
  shuffled <- JuliaConnectoR::juliaGet(.jlmerclusterperm$permute_by_predictor(df_jl, predictor_type, predictors, subject, item, n))
  col_names <- as.character(shuffled[[1]]$colindex$names)
  shuffled_dfs <- lapply(seq_along(shuffled), function(i) {
    shuffled_df <- as.data.frame.list(shuffled[[i]]$columns, col.names = col_names)
    shuffled_df$.id <- i
    shuffled_df
  })
  shuffled_long <- do.call(rbind, shuffled_dfs)
  class(shuffled_long) <- class(df)
  shuffled_long[, c(".id", colnames(df))]
}
