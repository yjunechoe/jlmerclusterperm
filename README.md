
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jlmerclusterperm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/yjunechoe/jlmerclusterperm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yjunechoe/jlmerclusterperm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Julia [GLM.jl](https://github.com/JuliaStats/GLM.jl) and
[MixedModels.jl](https://github.com/JuliaStats/MixedModels.jl)
implementation of bootstrapped cluster-based permutation analysis for
time series data, powered by
[`JuliaConnectoR`](https://github.com/stefan-m-lenz/JuliaConnectoR).

![](man/figures/clusterpermute_slice.png)

## Installation

You can install the development version of jlmerclusterperm from
[GitHub](https://github.com/yjunechoe/jlmerclusterperm) with:

``` r
# install.packages("remotes")
remotes::install_github("yjunechoe/jlmerclusterperm")
```

Using `jlmerclusterperm` requires a prior installation of Julia, which
can be downloaded from either the [official
website](https://julialang.org/) or using the command line utility
[juliaup](https://github.com/JuliaLang/juliaup). Julia version \>=1.8 is
required and \>=1.9 is preferred for its substantial compiler/runtime
improvements.

Before using functions from `jlmerclusterperm`, an initial setup step is
required via calling `jlmerclusterperm_setup()`. This will launch Julia
with some number of threads (controlled via
`options("jlmerclusterperm.nthreads")`) and install necessary package
dependencies (this only happens once and should take about 15-30
minutes).

Subsequent calls to `jlmerclusterperm_setup()` incur a small overhead of
around 30 seconds and there will be slight delays for first-time
function calls due to Julia’s [just-in-time
compilation](https://docs.julialang.org/en/v1/). Afterwards you can
enjoy blazingly-fast functions from the package.

``` r
# Both lines must be run
library(jlmerclusterperm)
system.time(jlmerclusterperm_setup(verbose = FALSE))
#>    user  system elapsed 
#>    0.02    0.01   28.00
```

See the
[Articles](https://yjunechoe.github.io/jlmerclusterperm/articles/) page
for in-depth tutorials and case study vignettes.

## Quick tour of package functionalities

### Complete CPA with `clusterpermute()`

A time series data:

``` r
chickweights <- ChickWeight
chickweights$Time <- as.integer(factor(chickweights$Time))
matplot(
  tapply(chickweights$weight, chickweights[c("Time", "Diet")], mean),
  type = "b", lwd = 3, ylab = "Weight", xlab = "Time"
)
```

<img src="man/figures/README-chickweight-1.png" width="100%" />

Preparing a specification object:

``` r
chickweights_spec <- make_jlmer_spec(
  formula = weight ~ 1 + Diet,
  data = chickweights,
  subject = "Chick", time = "Time"
)
chickweights_spec
#> ── jlmer specification ───────────────────────────────────────── <jlmer_spec> ──
#> Formula: weight ~ 1 + Diet2 + Diet3 + Diet4
#> Predictors:
#>   Diet: Diet2, Diet3, Diet4
#> Groupings:
#>   Subject: Chick
#>   Trial:
#>   Time: Time
#> Data:
#> # A tibble: 578 × 6
#>   weight Diet2 Diet3 Diet4 Chick  Time
#>    <dbl> <dbl> <dbl> <dbl> <ord> <int>
#> 1     42     0     0     0 1         1
#> 2     51     0     0     0 1         2
#> 3     59     0     0     0 1         3
#> # ℹ 575 more rows
#> ────────────────────────────────────────────────────────────────────────────────
```

Cluster-based permutation test:

``` r
set_rng_state(123L)
clusterpermute(
  chickweights_spec,
  threshold = 2.5,
  nsim = 100,
  progress = FALSE
)$empirical_clusters
#> ── Empirical clusters (t > 2.5) ──────────────────────── <empirical_clusters> ──
#> Diet2
#>   [3, 4]: 6.121 (p=0.0495)
#> Diet3
#>   [3, 12]: 35.769 (p=0.0099)
#> Diet4
#>   [2, 8]: 32.442 (p=0.0099)
#> ────────────────────────────────────────────────────────────────────────────────
```

Using custom contrast coding:

``` r
chickweights_helm <- chickweights
contrasts(chickweights_helm$Diet) <- contr.helmert(4)
colnames(contrasts(chickweights_helm$Diet)) <- c("1v2", "12v3", "123v4")
contrasts(chickweights_helm$Diet)
#>   1v2 12v3 123v4
#> 1  -1   -1    -1
#> 2   1   -1    -1
#> 3   0    2    -1
#> 4   0    0     3
```

``` r
chickweights_helm_spec <- make_jlmer_spec(
  formula = weight ~ 1 + Diet,
  data = chickweights_helm,
  subject = "Chick", time = "Time"
)
set_rng_state(123L)
clusterpermute(
  chickweights_helm_spec,
  threshold = 2.5,
  nsim = 100,
  progress = FALSE
)$empirical_clusters
#> ── Empirical clusters (t > 2.5) ──────────────────────── <empirical_clusters> ──
#> Diet1v2
#>   [3, 4]: 6.121 (p=0.0495)
#> Diet12v3
#>   [3, 5]: 8.904 (p=0.0297)
#>   [9, 12]: 12.061 (p=0.0099)
#> Diet123v4
#>   [3, 6]: 15.029 (p=0.0495)
#> ────────────────────────────────────────────────────────────────────────────────
```

With random effects:

``` r
chickweights_re_spec <- make_jlmer_spec(
  formula = weight ~ 1 + Diet + (1 | Chick),
  data = chickweights,
  subject = "Chick", time = "Time"
)
set_rng_state(123L)
clusterpermute(
  chickweights_re_spec,
  threshold = 2.5,
  nsim = 100,
  progress = FALSE
)$empirical_clusters
#> ── Empirical clusters (t > 2.5) ──────────────────────── <empirical_clusters> ──
#> Diet2
#>   [3, 4]: 6.387 (p=0.0594)
#> Diet3
#>   [2, 12]: 39.919 (p=0.0099)
#> Diet4
#>   [2, 8]: 33.853 (p=0.0099)
#> ────────────────────────────────────────────────────────────────────────────────
```

### Components of a CPA

Time-wise statistics of the data:

``` r
empirical_statistics <- compute_timewise_statistics(chickweights_spec)
tidy(empirical_statistics)
#> # A tibble: 36 × 3
#>    predictor  time statistic
#>    <chr>     <dbl>     <dbl>
#>  1 Diet2         1    -1.60 
#>  2 Diet3         1    -1.37 
#>  3 Diet4         1    -0.916
#>  4 Diet2         2     1.67 
#>  5 Diet3         2     2.45 
#>  6 Diet4         2     3.53 
#>  7 Diet2         3     2.60 
#>  8 Diet3         3     4.48 
#>  9 Diet4         3     6.27 
#> 10 Diet2         4     3.52 
#> # ℹ 26 more rows
```

``` r
matplot(t(empirical_statistics), type = "l", ylab = "t-value")
abline(h = 2.5, lty = 3)
```

<img src="man/figures/README-plot-empirical_statistics-1.png" width="100%" />

Empirical clusters:

``` r
empirical_clusters <- extract_empirical_clusters(empirical_statistics, threshold = 2.5)
empirical_clusters
#> ── Empirical clusters (t > 2.5) ──────────────────────── <empirical_clusters> ──
#> Diet2
#>   [3, 4]: 6.121
#> Diet3
#>   [3, 12]: 35.769
#> Diet4
#>   [2, 8]: 32.442
#> ────────────────────────────────────────────────────────────────────────────────
```

Simulating the null:

``` r
set_rng_state(123L)
null_statistics <- permute_timewise_statistics(chickweights_spec, nsim = 100)
null_cluster_dists <- extract_null_cluster_dists(null_statistics, threshold = 2.5)
```

Significance testing:

``` r
calculate_clusters_pvalues(empirical_clusters, null_cluster_dists, add1 = TRUE)
#> ── Empirical clusters (t > 2.5) ──────────────────────── <empirical_clusters> ──
#> Diet2
#>   [3, 4]: 6.121 (p=0.0495)
#> Diet3
#>   [3, 12]: 35.769 (p=0.0099)
#> Diet4
#>   [2, 8]: 32.442 (p=0.0099)
#> ────────────────────────────────────────────────────────────────────────────────
```

Testing a range of thresholds values:

``` r
walk_threshold_steps(
  empirical_statistics,null_statistics,
  threshold_steps = c(2, 2.5, 3)
)
#> # A tibble: 9 × 8
#>   threshold predictor id    start   end length sum_statistic  pvalue
#>       <dbl> <chr>     <fct> <dbl> <dbl>  <dbl>         <dbl>   <dbl>
#> 1       2   Diet2     1         3     5      3          8.50 0.0792 
#> 2       2   Diet3     1         2    12     11         38.2  0.00990
#> 3       2   Diet4     1         2    12     11         41.7  0.00990
#> 4       2.5 Diet2     1         3     4      2          6.12 0.0495 
#> 5       2.5 Diet3     1         3    12     10         35.8  0.00990
#> 6       2.5 Diet4     1         2     8      7         32.4  0.00990
#> 7       3   Diet3     1         3     5      3         12.7  0.00990
#> 8       3   Diet3     2         9    12      4         14.0  0.00990
#> 9       3   Diet4     1         2     7      6         29.7  0.00990
```
