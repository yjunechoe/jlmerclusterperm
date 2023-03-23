#' Calculate largest clusters by predictor from a z-matrix
#'
#' @param z_matrix A predictor-by-time matrix of z-values
#' @param threshold z-value threshold. Defaults to `1.5`.
#' @param binned Whether time points have been aggregated. Defaults to `TRUE`
#'
#' @seealso jlmer_by_time
#'
#' @export
largest_clusters <- function(z_matrix, threshold = 1.5, binned = TRUE) {
  z_matrix[abs(z_matrix) < threshold] <- 0
  predictors <- rownames(z_matrix)
  clusters <- JuliaConnectoR::juliaGet(JuliaConnectoR::juliaLet(
    "map(x -> find_largest_cluster(x, time_is_point), eachrow(z_matrix))",
    z_matrix = z_matrix, time_is_point = !binned
  ))
  clusters <- lapply(clusters, function(x) {
    attributes(x) <- NULL
    names(x) <- c("cluster", "sum_z")
    x
  })
  attributes(clusters) <- NULL
  names(clusters) <- predictors
  clusters
  # times <- as.numeric(colnames(z_matrix))
  # sign_matrix <- sign(replace(z_matrix, abs(z_matrix) < threshold, 0))
  # all_clusters <- lapply(seq_len(nrow(z_matrix)), function(i) {
  #   run_ids <- runs_id(sign_matrix[i,])
  #   clusters <- vapply(split(z_matrix[i,], run_ids), function(z_seq) {
  #     z_seq_sign <- sign(z_seq[1])
  #     if (z_seq_sign == 0) {
  #       sum_z <- 0
  #     } else {
  #       sum_z <- sum(replace(z_seq, is.infinite(z_seq), 0))
  #     }
  #     sum_z
  #   }, double(1), USE.NAMES = FALSE)
  #   largest_cluster <- which.max(abs(clusters))
  #   out <- list(
  #     time = times[run_ids == largest_cluster],
  #     sum_z = clusters[largest_cluster]
  #   )
  #   if (length(out$time) < 2) out <- NULL
  #   out
  # })
  # names(all_clusters) <- rownames(z_matrix)
  # all_clusters
}
