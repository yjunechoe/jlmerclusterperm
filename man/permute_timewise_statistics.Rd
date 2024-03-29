% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permute_timewise_statistics.R
\name{permute_timewise_statistics}
\alias{permute_timewise_statistics}
\title{Simulate cluster-mass statistics via bootstrapped permutations}
\usage{
permute_timewise_statistics(
  jlmer_spec,
  family = c("gaussian", "binomial"),
  statistic = c("t", "chisq"),
  nsim = 100L,
  predictors = NULL,
  ...
)
}
\arguments{
\item{jlmer_spec}{Data prepped for jlmer from \code{make_jlmer_spec()}}

\item{family}{A GLM family. Currently supports "gaussian" and "binomial".}

\item{statistic}{Test statistic for calculating cluster mass.
Can be one of \code{"t"} (default) from the regression model output or
\code{"chisq"} from a likelihood ratio test (takes about twice as long to calculate).}

\item{nsim}{Number of simulations description}

\item{predictors}{(Optional) a subset of predictors to test. Defaults to \code{NULL} which tests all predictors.}

\item{...}{Optional arguments passed to Julia for model fitting.
Defaults to \code{fast = TRUE} (when \code{family = "binomial"}) and \code{progress = FALSE}.}
}
\value{
A simulation-by-time-by-predictor 3D array of cluster statistics, of class \code{timewise_statistics}.
}
\description{
Simulate cluster-mass statistics via bootstrapped permutations
}
\examples{
\dontshow{if (julia_setup_ok()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
\dontshow{
options("jlmerclusterperm.nthreads" = 2)
jlmerclusterperm_setup(cache_dir = tempdir(), verbose = FALSE)
julia_progress(show = FALSE)
}

library(dplyr, warn.conflicts = FALSE)

# Specification object
spec <- make_jlmer_spec(
  weight ~ 1 + Diet, filter(ChickWeight, Time <= 20),
  subject = "Chick", time = "Time"
)
spec

# Simulation x Time x Predictor array of t-statistics from regression output
reset_rng_state()
null_statistics <- permute_timewise_statistics(spec, nsim = 3)
round(null_statistics, 2)

# Collect as dataframe with `tidy()`
permuted_timewise_stats_df <- tidy(null_statistics)
permuted_timewise_stats_df

# Permutations ran under the same RNG state are identical
reset_rng_state()
null_statistics2 <- permute_timewise_statistics(spec, nsim = 3)
identical(null_statistics, null_statistics2)

get_rng_state()
null_statistics3 <- permute_timewise_statistics(spec, nsim = 3)
identical(null_statistics, null_statistics3)

\dontshow{
JuliaConnectoR::stopJulia()
}
}
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=make_jlmer_spec]{make_jlmer_spec()}}
}
