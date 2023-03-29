permute_by_predictor <- function(jlmer_data, predictors, n = 1L, predictor_type = c("guess", "between_participant", "within_participant")) {
  df <- jlmer_data$data
  df_jl <- JuliaConnectoR::juliaCall("DataFrame", as.data.frame(df))
  subject <- jlmer_data$meta$subject
  item <- jlmer_data$meta$item
  time <- jlmer_data$meta$time
  predictor_type <- match.arg(predictor_type)
  if (predictor_type == "guess") {
    predictor_type <- JuliaConnectoR::juliaCall("guess_shuffle_as", df_jl, predictors, subject, item)
    cli::cli_alert_info("Guessed {.arg predictor_type} to be {.val {predictor_type}}.")
  }
  predictor_group <- Filter(function(x) any(predictors %in% x), jlmer_data$meta$term_groups)
  if (length(predictor_group) > 1) {
    cli::cli_abort("You can only shuffle one factor at a time.")
  } else {
    predictor_group <- predictor_group[[1]]
    if (!all(predictor_group %in% predictors)) {
      cli::cli_alert_info("Shuffling all levels of the factor together ({.arg predictors} = {.val {predictor_group}})")
    }
    predictors <- predictor_group
  }
  shuffled <- JuliaConnectoR::juliaGet(.jlmerclusterperm$permute_by_predictor(df_jl, predictor_type, predictors, subject, item, n))
  col_names <- as.character(shuffled[[1]]$keys)
  df_class <- class(df)
  out <- vector("list", length(shuffled))
  for (i in seq_along(out)) {
    shuffled_df <- as.data.frame.list(shuffled[[i]]$values, col.names = col_names)
    shuffled_df$.id <- i
    class(shuffled_df) <- class(df)
    out[[i]] <- shuffled_df
  }
  out_df <- do.call(rbind, out)
  out_df[, c(".id", colnames(df))]
}
