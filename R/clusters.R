#' Calculate largest clusters by predictor from a z-matrix
#'
#' @param z_matrix A predictor-by-time matrix of z-values
#' @param threshold z-value threshold. Defaults to `1.5`.
#'
#' @seealso jlmer_by_time
#'
#' @export
largest_clusters <- function(z_matrix, threshold = 1.5) {
  times <- as.numeric(colnames(z_matrix))
  sign_matrix <- sign(replace(z_matrix, abs(z_matrix) < threshold, 0))
  all_clusters <- lapply(seq_len(nrow(z_matrix)), function(i) {
    run_ids <- runs_id(xx[i,])
    clusters <- vapply(split(x[i,], run_ids), function(z_seq) {
      z_seq_sign <- sign(z_seq[1])
      if (z_seq_sign == 0) {
        sum_z <- 0
      } else {
        sum_z <- sum(replace(z_seq, is.infinite(z_seq), 0))
      }
      sum_z
    }, double(1), USE.NAMES = FALSE)
    largest_cluster <- which.max(abs(clusters))
    out <- list(
      time = times[run_ids == largest_cluster],
      sum_z = clusters[largest_cluster]
    )
    if (length(out$time) < 2) out <- NULL
    out
  })
  names(all_clusters) <- rownames(z_matrix)
  all_clusters
}
