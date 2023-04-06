#' Detect largest clusters from a time sequence of cluster statistics
#'
#' @param t_matrix A predictor-by-time matrix of cluster statistics.
#' @param threshold Cluster statistic threshold. If statistic is `"chisq"`, this represents the p-value threshold from a chi-squared test.
#' @param binned Whether time points have been aggregated. Defaults to `TRUE` which allows length-1 clusters.
#' @param top_n How many clusters to return, ordered by the size of the cluster statistic.
#'   Defaults to `1L` which returns the largest cluster. Use `NULL` to return all clusters.
#'
#' @seealso [compute_timewise_statistics()]
#'
#' @export
extract_empirical_clusters <- function(t_matrix, threshold, binned = TRUE, top_n = 1L) {
  time <- dimnames(t_matrix)$Time
  statistic <- attr(t_matrix, "statistic")
  if (statistic == "t") {
    t_matrix[abs(t_matrix) <= abs(threshold)] <- 0
  } else if (statistic == "chisq") {
    threshold_dict <- make_threshold_dict(attr(t_matrix, "term_groups"), threshold)
    for (predictor in dimnames(t_matrix)$Predictors) {
      predictor_statistics <- t_matrix[predictor,]
      t_matrix[predictor, abs(predictor_statistics) <= abs(threshold_dict[threshold_dict$term == predictor, "threshold"])] <- 0
    }
  }
  predictors <- rownames(t_matrix)
  n <- as.integer(max(min(top_n, ncol(t_matrix)), 1))
  largest_clusters <- .jlmerclusterperm$extract_clusters(t_matrix, binned, n)
  cluster_dfs <- df_from_DF(largest_clusters)
  empirical_clusters <- split(cluster_dfs[, -5], predictors[cluster_dfs$id])[predictors]
  empirical_clusters <- lapply(empirical_clusters, function(cluster_df) {
    cluster_df[order(cluster_df$cluster_id), ]
  })
  missing_clusters <- sapply(empirical_clusters, function(x) all(near_zero(x$statistic)))
  structure(empirical_clusters, class = "empirical_clusters", missing_clusters = missing_clusters,
            statistic = statistic, threshold = threshold, binned = binned, time = time)
}

#' Construct a distribution of the largest cluster statistics from bootstrapped permutations
#'
#' @param t_array A simulation-by-time-by-predictor 3D array of cluster statistics.
#' @inheritParams extract_empirical_clusters
#'
#' @seealso [permute_timewise_statistics()]
#'
#' @export
extract_null_cluster_dists <- function(t_array, threshold, binned = TRUE) {
  time <- dimnames(t_array)$Time
  statistic <- attr(t_array, "statistic")
  if (statistic == "t") {
    t_array[abs(t_array) <= abs(threshold)] <- 0
  } else if (statistic == "chisq") {
    threshold_dict <- make_threshold_dict(attr(t_array, "term_groups"), threshold)
    for (predictor in dimnames(t_array)$Predictor) {
      predictor_statistics <- t_array[, , predictor]
      predictor_statistics[abs(predictor_statistics) <= abs(threshold_dict[threshold_dict$term == predictor, "threshold"])] <- 0
      t_array[, , predictor] <- predictor_statistics
    }
  }
  null_clusters <- apply(t_array, 3, function(t_matrix) {
    t_matrix <- t_matrix[!is.nan(rowSums(t_matrix)),]
    largest_clusters <- df_from_DF(.jlmerclusterperm$extract_clusters(t_matrix, binned, 1L))
  }, simplify = FALSE)
  structure(null_clusters, class = "null_clusters",
            statistic = statistic, threshold = threshold, binned = binned, time = time)
}

#' Calculate bootstrapped p-values of clusters
#'
#' @param empirical_clusters A `empirical_clusters` object
#' @param null_clusters A `null_clusters` object
#' @param add1 Whether to add 1 to the numerator before calculating probability.
#'   Defaults to `FALSE`. Use `TRUE` to effectively include the observed statistic
#'   as part of the null distribution (recommended with larger `nsim` prior to publishing results).
#'
#' @seealso [extract_empirical_clusters()], [extract_null_cluster_dists()]
#'
#' @export
calculate_clusters_pvalues <- function(empirical_clusters, null_clusters, add1 = FALSE) {
  empirical_attrs <- attributes(empirical_clusters)
  null_attrs <- attributes(null_clusters)
  if (!identical(empirical_attrs[c("statistic", "threshold")], null_attrs[c("statistic", "threshold")])) {
    cli::cli_abort("Cluster statistics between empirical and null are incomparable.")
  }
  empirical <- lapply(empirical_clusters, `[[`, "statistic")
  empirical <- Filter(function(x) !near_zero(x[1]), empirical)
  null <- lapply(null_clusters, `[[`, "statistic")
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


