`%|0|%` <- function(lhs, rhs) if (is.null(lhs) || identical(lhs, "") || length(lhs) == 0 || is.na(lhs)) rhs else lhs

near_zero <- function(x) {
  abs(x) < .Machine$double.eps^0.5
}

maybe_as_tibble <- function(x) {
  if ("tibble" %in% rownames(utils::installed.packages())) {
    asNamespace("tibble")$as_tibble(x)
  } else {
    x
  }
}

backtrans_interaction <- function(x) {
  gsub("__", ":", x, fixed = TRUE)
}

replace_as_na <- function(x, y) {
  x[x == y] <- NA
  x
}

zero_pad <- function(x, y) {
  if (missing(y)) y <- max(x)
  sprintf(paste0("%0", floor(log10(y)) + 1, "d"), x)
}

check_arg_class <- function(x, x_class, x_arg = x_class) {
  if (!inherits(x, x_class)) {
    cli::cli_abort(c(
      "{.arg {x_arg}} must be a {.cls {x_class}} object, not a {.cls {class(x)}}"
    ))
  }
  invisible(TRUE)
}
