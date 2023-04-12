#' Calculate bootstrapped p-values of clusters
#'
#' @param empirical_clusters A `empirical_clusters` object
#' @param null_cluster_dists A `null_cluster_dists` object
#' @param add1 Whether to add 1 to the numerator before calculating probability.
#'   Use `TRUE` to effectively count the observed statistic as part of the permuted
#'   null distribution (recommended with larger `nsim` prior to publishing results).
#'
#' @seealso [extract_empirical_clusters()], [extract_null_cluster_dists()]
#'
#' @export
#' @return An `empirical_clusters` object augmented with p-values.
calculate_clusters_pvalues <- function(empirical_clusters, null_cluster_dists, add1 = FALSE) {
  clusters_are_comparable(empirical_clusters, null_cluster_dists)
  empirical <- lapply(empirical_clusters, `[[`, "statistic")
  empirical <- Filter(function(x) !near_zero(x[1]), empirical)
  null <- lapply(null_cluster_dists, `[[`, "statistic")
  predictors_to_test <- stats::setNames(nm = names(empirical)[names(empirical) %in% names(null)])
  pvalues <- lapply(predictors_to_test, function(x) {
    sapply(empirical[[x]], function(cluster_statistic) {
      mean(c(abs(null[[x]]) >= abs(cluster_statistic), if (add1) TRUE))
    })
  })
  augmented <- empirical_clusters
  attr(augmented, "pvalues") <- pvalues
  augmented
}

#' @rdname calculate_clusters_pvalues
#' @export
clusters_are_comparable <- function(empirical_clusters, null_cluster_dists) {
  if (!inherits(empirical_clusters, "empirical_clusters") || !inherits(null_cluster_dists, "null_cluster_dists")) {
    cli::cli_abort("Can only compare object of class {.cls empirical_clusters} against object of class {.cls null_cluster_dists}")
  }
  empirical_attrs <- attributes(empirical_clusters)
  null_attrs <- attributes(null_cluster_dists)
  if (!identical(empirical_attrs[c("statistic", "threshold")], null_attrs[c("statistic", "threshold")])) {
    cluster_properties <- sapply(list(empirical_attrs, null_attrs), `[`, c("statistic", "threshold"))
    mismatch_info <- apply(cluster_properties, 1L, function(x) {
      if(x[[1]] != x[[2]]) {
        x <- unlist(x, use.names = FALSE)
        if (is.numeric(x)) x <- paste0("{", x, "}")
        paste0("empirical uses {.val ", x[1], "} but null uses {.val ", x[2], "}.")
      }
    })
    mismatch_info <- Filter(Negate(is.null), mismatch_info)
    mismatch_info <- stats::setNames(paste0("{.strong ", names(mismatch_info), "}: ", mismatch_info), rep("x", length(mismatch_info)))
    cli::cli_abort(c(
      "Cluster-mass statistics between empirical and null are not comparable.",
      "i" = "{.arg empirical_clusters} and {.arg null_cluster_dists} must share the same {.code statistic} and {.code threshold}.",
      mismatch_info
    ))
  } else {
    TRUE
  }
}
