df_to_NT <- function(df) {
  JuliaConnectoR::juliaLet("NamedTuple{Tuple(Symbol.(x))}(y)", x = colnames(df), y = unname(as.list(df)))
}

rbind_DFs <- function(DFs) {
  col_names <- as.character(DFs[[1]]$colindex$names)
  dfs_list <- lapply(seq_along(DFs), function(i) {
    df <- as.data.frame.list(DFs[[i]]$columns, col.names = col_names)
    df$.id <- i
    df
  })
  df_long <- do.call(rbind, dfs_list)
  df_long[, c(".id", col_names)]
}

df_from_DF <- function(DF) {
  df_str <- JuliaConnectoR::juliaGet(DF)
  as.data.frame(df_str$columns, col.names = unlist(df_str$colindex$names, use.names = FALSE))
}

prep_for_jlmer <- function(julia_formula, data, time, family, ...) {
  opts <- list(...)
  if (is.null(opts) || any(names(opts) == "")) {
    cli::cli_abort("All optional arguments to fit in {.arg ...} must be named.")
  }

  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- JuliaConnectoR::juliaCall("DataFrame", data)

  jmler_family <- JuliaConnectoR::juliaCall(switch(family, gaussian = "Normal", binomial = "Bernoulli"))

  jlmer_groupings <- if (!is.null(lme4::findbars(julia_formula))) {
    grouping_vars <- lapply(lme4::findbars(julia_formula), `[[`, 3)
    jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = grouping_vars)
  } else {
    list()
  }

  list(jlmer_fm, jlmer_df, time, jmler_family, jlmer_groupings)
}
