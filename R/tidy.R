#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidier methods for Julia regression models
#'
#' @param x An object of class `jlmer_mod`
#' @param effects One of "var_model", "ran_pars", or "fixed"
#' @param ... Unused
#'
#' @srrstats {RE4.2} Model coefficients via `tidy()`
#'
#' @examplesIf JuliaConnectoR::juliaSetupOk()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' # Fixed-effects only model
#' mod1 <- to_jlmer(weight ~ 1 + Diet, ChickWeight)
#' tidy(mod1)
#' glance(mod1)
#'
#' # Mixed model
#' mod2 <- to_jlmer(weight ~ 1 + Diet + (1 | Chick), ChickWeight)
#' tidy(mod2)
#' glance(mod2)
#'
#' # Select which of fixed/random effects to return
#' tidy(mod2, effects = "fixed")
#' tidy(mod2, effects = "ran_pars")
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @name julia_model_tidiers
#' @return A data frame
NULL

#' @rdname julia_model_tidiers
#' @method tidy jlmer_mod
#' @export
tidy.jlmer_mod <- function(x, effects = c("var_model", "ran_pars", "fixed"), ...) {
  out <- df_from_DF(JuliaConnectoR::juliaLet("DataFrame(coeftable(x))", x = x))[, 1:5]
  colnames(out) <- c("term", "estimate", "std.error", "statistic", "p.value")
  out$term <- backtrans_interaction(out$term)
  is_mem <- JuliaConnectoR::juliaLet("typeof(x) <: MixedModel", x = x)
  effects <- match.arg(effects)
  if (is_mem && effects != "fixed") {
    vc <- JuliaConnectoR::juliaGet(JuliaConnectoR::juliaCall("VarCorr", x))[[1]]
    re_flatten <- lapply(vc, function(g) lapply(g, unlist))
    re_sd <- lapply(re_flatten, function(g) {
      setNames(g[[1]], paste0("sd__", backtrans_interaction(names(g[[1]]))))
    })
    re_cor <- lapply(re_flatten, function(g) {
      re_terms <- backtrans_interaction(names(g[[1]]))
      re_term_matrix <- outer(re_terms, re_terms, function(i, j) {
        vapply(seq_along(i), function(ind) {
          paste0(sort(c(i[ind], j[ind]))[1], ".", sort(c(i[ind], j[ind]))[2])
        }, character(1))
      })
      re_cor_terms <- re_term_matrix[lower.tri(re_term_matrix)]
      if (!is.null(g[[2]])) {
        setNames(g[[2]], paste0("cor__", re_cor_terms))
      }
    })
    re <- Filter(Negate(is.null), c(re_sd, re_cor))
    re_dfs <- lapply(seq_along(re), function(i) {
      data.frame(group = names(re)[i], term = names(re[[i]]), estimate = unname(re[[i]]))
    })
    re_df <- do.call(rbind, re_dfs)
    sigma <- JuliaConnectoR::juliaLet("x.sigma", x = x)
    if (!is.na(sigma)) {
      re_df <- rbind(re_df, data.frame(group = "Residual", term = "sd__Observation", estimate = sigma))
    }
    if (effects == "ran_pars") {
      out <- re_df
    } else {
      out$effect <- "fixed"
      out$group <- NA_character_
      re_df$effect <- "ran_pars"
      re_df[, setdiff(names(out), names(re_df))] <- NA
      out <- rbind(out, re_df)[, c("effect", "group", "term", "estimate", "std.error", "statistic", "p.value")]
    }
  }
  maybe_as_tibble(out)
}

#' Tidiers for cluster permutation test objects
#'
#' @param x An object of class `<timewise_statistics>`, `<empirical_clusters>`, or `<null_cluster_dists>`
#' @param ... Unused
#'
#' @examplesIf JuliaConnectoR::juliaSetupOk()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(verbose = FALSE)
#' julia_progress(show = FALSE)
#' }
#'
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # Specification object
#' spec <- make_jlmer_spec(
#'   weight ~ 1 + Diet, filter(ChickWeight, Time <= 20),
#'   subject = "Chick", time = "Time"
#' )
#' spec
#'
#' # Method for `<timewise_statistics>`
#' empirical_statistics <- compute_timewise_statistics(spec)
#' class(empirical_statistics)
#' tidy(empirical_statistics)
#'
#' reset_rng_state()
#' null_statistics <- permute_timewise_statistics(spec, nsim = 100)
#' class(null_statistics)
#' tidy(null_statistics)
#'
#' # Method for `<empirical_clusters>`
#' empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold = 2)
#' class(empirical_clusters)
#' tidy(empirical_clusters)
#'
#' # Method for `<null_cluster_dists>`
#' null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold = 2)
#' class(null_cluster_dists)
#' tidy(null_cluster_dists)
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @name cluster_permutation_tidiers

#' @rdname cluster_permutation_tidiers
#' @method tidy timewise_statistics
#' @export
#' @return A data frame
tidy.timewise_statistics <- function(x, ...) {
  stacked <- as.data.frame.table(x, responseName = "statistic")
  colnames(stacked) <- tolower(colnames(stacked))
  stacked$time <- as.numeric(levels(stacked$time))[stacked$time]
  stacked$predictor <- as.character(stacked$predictor)
  stacked <- stacked[do.call(order, stacked[c("time", "predictor")]), intersect(c("predictor", "time", "statistic", "sim"), colnames(stacked))]
  maybe_as_tibble(stacked)
}

#' @rdname cluster_permutation_tidiers
#' @method tidy empirical_clusters
#' @export
tidy.empirical_clusters <- function(x, ...) {
  pvalues <- attr(x, "pvalues")
  cluster_dfs <- lapply(seq_along(x), function(i) {
    predictor <- names(x)[i]
    cluster_df <- cbind(predictor = predictor, x[[i]])
    if (!is.null(pvalues[[predictor]])) {
      cluster_df$pvalue <- pvalues[[predictor]]
    } else if (!is.null(pvalues)) {
      cluster_df$pvalue <- NA
    }
    cluster_df
  })
  clusters_df <- do.call(rbind, cluster_dfs)
  clusters_df <- clusters_df[clusters_df$cluster_id != 0, ]
  time <- as.numeric(attr(x, "time"))
  clusters_df$id <- factor(zero_pad(clusters_df$cluster_id))
  clusters_df$length <- clusters_df$cluster_end - clusters_df$cluster_start + 1
  clusters_df$start <- time[clusters_df$cluster_start]
  clusters_df$end <- time[clusters_df$cluster_end]
  clusters_df$sum_statistic <- clusters_df$statistic
  clusters_df <- clusters_df[intersect(c("predictor", "id", "start", "end", "length", "sum_statistic", "pvalue"), names(clusters_df))]
  maybe_as_tibble(clusters_df)
}

#' @rdname cluster_permutation_tidiers
#' @method tidy null_cluster_dists
#' @export
tidy.null_cluster_dists <- function(x, ...) {
  cluster_dfs <- lapply(seq_along(x), function(i) {
    cbind(predictor = names(x)[i], x[[i]])
  })
  clusters_df <- do.call(rbind, cluster_dfs)
  time <- as.numeric(attr(x, "time"))
  clusters_df$length <- clusters_df$cluster_end - clusters_df$cluster_start + 1
  clusters_df$start <- time[replace_as_na(clusters_df$cluster_start, 0)]
  clusters_df$end <- time[replace_as_na(clusters_df$cluster_end, 0)]
  clusters_df$length[is.na(clusters_df$start)] <- NA
  clusters_df$sim <- as.factor(zero_pad(clusters_df$id))
  clusters_df$sum_statistic <- clusters_df$statistic
  clusters_df <- clusters_df[c("predictor", "start", "end", "length", "sum_statistic", "sim")]
  maybe_as_tibble(clusters_df)
}

#' @importFrom generics glance
#' @export
generics::glance

#' @rdname julia_model_tidiers
#'
#' @srrstats {RE4.5} Numbers of observations via `glance()`
#' @srrstats {RE4.10} Model Residuals via `glance()`. Users are assumed to be familiar.
#' @srrstats {RE4.11} Goodness-of-fit and other statistics via `glance()`
#'
#' @method glance jlmer_mod
#' @export
glance.jlmer_mod <- function(x, ...) {
  is_mixed <- JuliaConnectoR::juliaLet("x isa MixedModel", x = x)
  is_REML <- is_mixed && JuliaConnectoR::juliaLet("x.optsum.REML", x = x)
  nobs <- JuliaConnectoR::juliaCall("nobs", x)
  sigma <- if (is_mixed) {
    JuliaConnectoR::juliaLet("x.sigma", x = x) %|0|% NA
  } else {
    has_dispersion <- JuliaConnectoR::juliaCall("GLM.dispersion_parameter", x)
    if (has_dispersion) JuliaConnectoR::juliaLet("dispersion(x.model)", x = x) else NA
  }
  ll <- if (is_REML) {
    list(logLik = NA, AIC = NA, BIC = NA)
  } else {
    list(
      logLik = JuliaConnectoR::juliaCall("loglikelihood", x),
      AIC = JuliaConnectoR::juliaCall("aic", x),
      BIC = JuliaConnectoR::juliaCall("bic", x)
    )
  }
  deviance <- JuliaConnectoR::juliaCall("deviance", x)
  dof <- JuliaConnectoR::juliaCall("dof", x)
  out <- data.frame(
    nobs = nobs, df = dof, sigma = sigma,
    logLik = ll$logLik, AIC = ll$AIC, BIC = ll$BIC,
    deviance = deviance, df.residual = nobs - dof
  )
  maybe_as_tibble(out)
}
