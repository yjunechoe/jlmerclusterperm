strip_JLTYPE <- function(x) {
  attr(x, "JLTYPE") <- NULL
  x
}

df_from_DF <- function(DF) {
  df_str <- JuliaConnectoR::juliaGet(DF)
  as.data.frame(df_str$columns, col.names = unlist(df_str$colindex$names, use.names = FALSE))
}

prep_for_jlmer <- function(jlmer_spec, family, ...) {
  if (!is.null(jlmer_spec$.backdoor$prepped_for_jlmer)) {
    return(jlmer_spec$.backdoor$prepped_for_jlmer)
  }

  julia_formula <- jlmer_spec$formula$jl
  data <- jlmer_spec$data
  time <- jlmer_spec$meta$time

  opts <- list(...)
  if (is.null(opts) || any(names(opts) == "")) {
    cli::cli_abort("All optional arguments to fit in {.arg ...} must be named.")
  }

  jlmer_fm <- JuliaConnectoR::juliaEval(paste0("@formula(", deparse1(julia_formula), ")"))
  jlmer_df <- JuliaConnectoR::juliaCall("DataFrame", data)

  jmler_family <- JuliaConnectoR::juliaCall(switch(family,
    gaussian = "Normal",
    binomial = "Bernoulli"
  ))

  jlmer_groupings <- if (!is.null(lme4::findbars(julia_formula))) {
    grouping_vars <- lapply(lme4::findbars(julia_formula), `[[`, 3)
    jlmer_groupings <- JuliaConnectoR::juliaLet("Dict(x .=> [Grouping()])", x = grouping_vars)
  } else {
    list()
  }

  list(jlmer_fm, jlmer_df, time, jmler_family, jlmer_groupings)
}
