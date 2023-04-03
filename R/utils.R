`%|0|%` <- function(lhs, rhs) if (is.null(lhs) || identical(lhs, "") || length(lhs) == 0) rhs else lhs

runs_id <- function(...) {
  uq <- interaction(..., drop = TRUE)
  runs <- rle(as.character(uq))$lengths
  rep(seq_along(runs), times = runs)
}

near_zero <- function(x) {
  abs(x) < .Machine$double.eps ^ 0.5
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
