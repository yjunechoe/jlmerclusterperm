#' @importFrom generics tidy
#' @export
generics::tidy

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
      stats::setNames(g[[1]], paste0("sd__", backtrans_interaction(names(g[[1]]))))
    })
    re_cor <- lapply(re_flatten, function(g) {
      re_terms <- backtrans_interaction(names(g[[1]]))
      re_term_matrix <- outer(re_terms, re_terms, function(i, j) paste0(i, ".", j))
      re_cor_terms <- re_term_matrix[lower.tri(re_term_matrix)]
      if (!is.null(g[[2]])) {
        stats::setNames(g[[2]], paste0("cor__", re_cor_terms))
      }
    })
    re <- Filter(Negate(is.null), c(re_sd, re_cor))
    re_dfs <- lapply(seq_along(re), function(i) {
      data.frame(group = names(re)[i], term = names(re[[i]]), estimate = unname(re[[i]]))
    })
    re_df <- do.call(rbind, re_dfs)
    re_df <- re_df[do.call(order, re_df[, c("group", "term")]), ]
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

#' @importFrom generics glance
#' @export
generics::glance

#' @export
glance.jlmer_mod <- function(x, ...) {
  nobs <- JuliaConnectoR::juliaCall("nobs", x)
  AIC <- JuliaConnectoR::juliaCall("aic", x)
  BIC <- JuliaConnectoR::juliaCall("bic", x)
  deviance <- JuliaConnectoR::juliaCall("deviance", x)
  dof <- JuliaConnectoR::juliaCall("dof", x)
  out <- data.frame(
    nobs = nobs, AIC = AIC, BIC = BIC,
    deviance = deviance, df.residual = nobs - dof
  )
  maybe_as_tibble(out)
}

