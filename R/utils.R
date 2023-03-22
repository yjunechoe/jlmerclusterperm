`%||%` <- function(lhs, rhs) if (is.null(lhs) || identical(lhs, "") || length(lhs) == 0) rhs else lhs

runs_id <- function(...) {
  uq <- interaction(..., drop = TRUE)
  runs <- rle(as.character(uq))$lengths
  rep(seq_along(runs), times = runs)
}
