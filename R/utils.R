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

replace_as_na <- function(x, y) {
  x[x == y] <- NA
  x
}

make_threshold_dict <- function(term_groups, threshold) {
  if (threshold < 0 || threshold > 1) {
    cli::cli_abort(c(
      '{.arg threshold} must be between {.val {0}} and {.val {1}} when {.arg statistic = {.val chisq}}'
    ))
  }
  threshold_dict <- utils::stack(stats::setNames(term_groups, lengths(term_groups)))
  colnames(threshold_dict) <- c("term", "df")
  threshold_dict$df <- as.integer(threshold_dict$df)
  threshold_dict$threshold <- stats::qchisq(p = 1 - threshold, df = threshold_dict$df)
  threshold_dict
}
