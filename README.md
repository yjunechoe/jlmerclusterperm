
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jlmerclusterperm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Julia [MixedModels.jl](https://github.com/JuliaStats/MixedModels.jl)
implementation of cluster-based permutation test for time series data,
powered by
[{JuliaConnectoR}](https://github.com/stefan-m-lenz/JuliaConnectoR)

## Installation

You can install the development version of jlmerclusterperm from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yjunechoe/jlmerclusterperm")
```

## Setup

``` r
library(jlmerclusterperm)
system.time({jlmerclusterperm_setup()})
#> Starting Julia with 7 workers ...
#> Running package setup scripts ...
#>    user  system elapsed 
#>    0.03    0.00   27.77
```

## Basic example

Example data:

``` r
head(ChickWeight)
#>   weight Time Chick Diet
#> 1     42    0     1    1
#> 2     51    2     1    1
#> 3     59    4     1    1
#> 4     64    6     1    1
#> 5     76    8     1    1
#> 6     93   10     1    1
fm <- weight ~ 1 + Diet + (1 | Chick)
```

``` r
chickweights_m <- tapply(ChickWeight$weight, ChickWeight[c("Time", "Diet")], mean)
matplot(chickweights_m, type = "b", lwd = 3)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="70%" />

Run Julia mixed model using lme4 syntax, using the convenience form
`to_jlmer()`:

``` r
library(lme4)
#> Loading required package: Matrix
summary(lmer(fm, ChickWeight))
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: weight ~ 1 + Diet + (1 | Chick)
#>    Data: ChickWeight
#> 
#> REML criterion at convergence: 6506.8
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -1.8495 -0.7499 -0.1835  0.6363  3.0852 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev.
#>  Chick    (Intercept)  305.8   17.49   
#>  Residual             4526.6   67.28   
#> Number of obs: 578, groups:  Chick, 50
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)   101.63       6.04  16.826
#> Diet2          20.98      10.24   2.050
#> Diet3          41.32      10.24   4.036
#> Diet4          33.40      10.27   3.253
#> 
#> Correlation of Fixed Effects:
#>       (Intr) Diet2  Diet3 
#> Diet2 -0.590              
#> Diet3 -0.590  0.348       
#> Diet4 -0.588  0.347  0.347

to_jlmer(fm, ChickWeight)
#> <Julia object of type LinearMixedModel{Float64}>
#> Linear mixed model fit by maximum likelihood
#>  weight ~ 1 + Diet2 + Diet3 + Diet4 + (1 | Chick)
#>    logLik   -2 logLik     AIC       AICc        BIC    
#>  -3265.1439  6530.2877  6542.2877  6542.4348  6568.4452
#> 
#> Variance components:
#>             Column    Variance Std.Dev.
#> Chick    (Intercept)   247.2559 15.7244
#> Residual              4527.9661 67.2902
#>  Number of obs: 578; levels of grouping factors: 50
#> 
#>   Fixed-effects parameters:
#> ──────────────────────────────────────────────────
#>                 Coef.  Std. Error      z  Pr(>|z|)
#> ──────────────────────────────────────────────────
#> (Intercept)  101.781      5.78499  17.59    <1e-68
#> Diet2         20.8353     9.79412   2.13    0.0334
#> Diet3         41.1687     9.79412   4.20    <1e-04
#> Diet4         33.284      9.82881   3.39    0.0007
#> ──────────────────────────────────────────────────
```

Run Julia mixed model directly in `jlmer()` after explicitly
reformulating with `jlmer_model_matrix()`:

``` r
mm <- jlmer_model_matrix(fm, ChickWeight)
mm$julia_formula
#> weight ~ 1 + Diet2 + Diet3 + Diet4 + (1 | Chick)
#> <environment: 0x00000248eb8b2d48>
mm$data
#> # A tibble: 578 × 7
#>    weight Diet2 Diet3 Diet4 Chick  Time Diet 
#>     <dbl> <dbl> <dbl> <dbl> <ord> <dbl> <fct>
#>  1     42     0     0     0 1         0 1    
#>  2     51     0     0     0 1         2 1    
#>  3     59     0     0     0 1         4 1    
#>  4     64     0     0     0 1         6 1    
#>  5     76     0     0     0 1         8 1    
#>  6     93     0     0     0 1        10 1    
#>  7    106     0     0     0 1        12 1    
#>  8    125     0     0     0 1        14 1    
#>  9    149     0     0     0 1        16 1    
#> 10    171     0     0     0 1        18 1    
#> # … with 568 more rows

jlmer(mm$julia_formula, mm$data)
#> <Julia object of type LinearMixedModel{Float64}>
#> Linear mixed model fit by maximum likelihood
#>  weight ~ 1 + Diet2 + Diet3 + Diet4 + (1 | Chick)
#>    logLik   -2 logLik     AIC       AICc        BIC    
#>  -3265.1439  6530.2877  6542.2877  6542.4348  6568.4452
#> 
#> Variance components:
#>             Column    Variance Std.Dev.
#> Chick    (Intercept)   247.2559 15.7244
#> Residual              4527.9661 67.2902
#>  Number of obs: 578; levels of grouping factors: 50
#> 
#>   Fixed-effects parameters:
#> ──────────────────────────────────────────────────
#>                 Coef.  Std. Error      z  Pr(>|z|)
#> ──────────────────────────────────────────────────
#> (Intercept)  101.781      5.78499  17.59    <1e-68
#> Diet2         20.8353     9.79412   2.13    0.0334
#> Diet3         41.1687     9.79412   4.20    <1e-04
#> Diet4         33.284      9.82881   3.39    0.0007
#> ──────────────────────────────────────────────────
```

Fit jlmer at each timepoint and get a z-matrix back with
`jlmer_by_time`:

``` r
by_time <- jlmer_by_time(mm$julia_formula, mm$data, time = "Time")
by_time
#>              Time
#> Predictors              0         2         4         6         8        10
#>   (Intercept) 171.1679337 66.283563 78.451405 48.530261 27.990931 20.435061
#>   Diet2        -1.6709347  1.741333  2.713433  3.673967  2.478554  1.992062
#>   Diet3        -1.4322297  2.551256  4.671226  4.740676  3.860595  3.101100
#>   Diet4        -0.9548198  3.685147  6.547445  7.300775  5.345773  4.248827
#>              Time
#> Predictors           12        14        16        18        20        21
#>   (Intercept) 16.459364 15.624203 14.347003 13.257500 12.593491 11.642034
#>   Diet2        2.028207  1.400797  1.210453  1.459871  1.582567  1.500884
#>   Diet3        3.194883  3.111014  3.184318  3.764490  3.979699  3.759318
#>   Diet4        3.818298  2.906696  2.254730  2.231461  2.759932  2.389537
```

Find largest cluster in the observed data:

``` r
empirical_clusters <- largest_clusters(by_time, threshold = 1.5)
empirical_clusters
#> $`(Intercept)`
#> $`(Intercept)`$cluster
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12
#> 
#> $`(Intercept)`$sum_z
#> [1] 496.7828
#> 
#> 
#> $Diet2
#> $Diet2$cluster
#> [1] 2 3 4 5 6 7
#> 
#> $Diet2$sum_z
#> [1] 14.62756
#> 
#> 
#> $Diet3
#> $Diet3$cluster
#>  [1]  2  3  4  5  6  7  8  9 10 11 12
#> 
#> $Diet3$sum_z
#> [1] 39.91857
#> 
#> 
#> $Diet4
#> $Diet4$cluster
#>  [1]  2  3  4  5  6  7  8  9 10 11 12
#> 
#> $Diet4$sum_z
#> [1] 43.48862
```

We can think of the data as having a within-participant design, since
each chick is assigned one diet. `clusterpermute()` conducts a
cluster-based permutation test by randomly shuffling the assignment of
diet between chicks to generate a null distribution of the largest sum-z
cluster by predictor.

``` r
system.time({
  null_dist_clusters <- clusterpermute(
    julia_formula = mm$julia_formula, data = mm$data, time = "Time",
    nsim = 1000, participant_col = "Chick", threshold = 1.5,
    binned = FALSE # For non-aggregated data
  )
})
#>    user  system elapsed 
#>    0.06    0.02   25.27
```

Test empirical clusters against the simulated null:

``` r
sapply(c("Diet2", "Diet3", "Diet4"), function(predictor) {
  empirical <- empirical_clusters[[predictor]]$sum_z
  null_dist <- null_dist_clusters[[predictor]]
  mean(abs(null_dist) > empirical)
})
#> Diet2 Diet3 Diet4 
#> 0.058 0.000 0.000
```

## Formula utilities

``` r
jlmer_model_matrix(mpg ~ wt * qsec, head(mtcars))
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec
#> <environment: 0x00000248eb8b2d48>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec
#> <environment: 0x00000248eb8b2d48>
#> 
#> $data
#> # A tibble: 6 × 12
#>      wt  qsec wt__qsec   mpg   cyl  disp    hp  drat    vs    am  gear  carb
#>   <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  2.62  16.5     43.1  21       6   160   110  3.9      0     1     4     4
#> 2  2.88  17.0     48.9  21       6   160   110  3.9      0     1     4     4
#> 3  2.32  18.6     43.2  22.8     4   108    93  3.85     1     1     4     1
#> 4  3.22  19.4     62.5  21.4     6   258   110  3.08     1     0     3     1
#> 5  3.44  17.0     58.5  18.7     8   360   175  3.15     0     0     3     2
#> 6  3.46  20.2     70.0  18.1     6   225   105  2.76     1     0     3     1
jlmer_model_matrix(mpg ~ wt * qsec + (1 + wt | vs), head(mtcars))
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 + wt | vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 + wt | vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $data
#> # A tibble: 6 × 12
#>     mpg    wt  qsec wt__qsec vs      cyl  disp    hp  drat    am  gear  carb
#>   <dbl> <dbl> <dbl>    <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21    2.62  16.5     43.1 0         6   160   110  3.9      1     4     4
#> 2  21    2.88  17.0     48.9 0         6   160   110  3.9      1     4     4
#> 3  22.8  2.32  18.6     43.2 1         4   108    93  3.85     1     4     1
#> 4  21.4  3.22  19.4     62.5 1         6   258   110  3.08     0     3     1
#> 5  18.7  3.44  17.0     58.5 0         8   360   175  3.15     0     3     2
#> 6  18.1  3.46  20.2     70.0 1         6   225   105  2.76     0     3     1
jlmer_model_matrix(mpg ~ wt * qsec + (1 + wt || vs), head(mtcars))
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 || vs) + (wt || vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec + zerocorr(1 | vs) + zerocorr(wt | 
#>     vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $data
#> # A tibble: 6 × 12
#>     mpg    wt  qsec wt__qsec vs      cyl  disp    hp  drat    am  gear  carb
#>   <dbl> <dbl> <dbl>    <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21    2.62  16.5     43.1 0         6   160   110  3.9      1     4     4
#> 2  21    2.88  17.0     48.9 0         6   160   110  3.9      1     4     4
#> 3  22.8  2.32  18.6     43.2 1         4   108    93  3.85     1     4     1
#> 4  21.4  3.22  19.4     62.5 1         6   258   110  3.08     0     3     1
#> 5  18.7  3.44  17.0     58.5 0         8   360   175  3.15     0     3     2
#> 6  18.1  3.46  20.2     70.0 1         6   225   105  2.76     0     3     1
jlmer_model_matrix(mpg ~ wt * qsec + (1 + wt | vs), head(mtcars), drop_terms = "wt__qsec")
#> $formula
#> mpg ~ 1 + wt + qsec + (1 + wt | vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + (1 + wt | vs)
#> <environment: 0x00000248eb8b2d48>
#> 
#> $data
#> # A tibble: 6 × 11
#>     mpg    wt  qsec vs      cyl  disp    hp  drat    am  gear  carb
#>   <dbl> <dbl> <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21    2.62  16.5 0         6   160   110  3.9      1     4     4
#> 2  21    2.88  17.0 0         6   160   110  3.9      1     4     4
#> 3  22.8  2.32  18.6 1         4   108    93  3.85     1     4     1
#> 4  21.4  3.22  19.4 1         6   258   110  3.08     0     3     1
#> 5  18.7  3.44  17.0 0         8   360   175  3.15     0     3     2
#> 6  18.1  3.46  20.2 1         6   225   105  2.76     0     3     1
```
