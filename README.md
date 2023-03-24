
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
#>    0.01    0.02   24.03
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
#> attr(,"groupings")
#> attr(,"groupings")[[1]]
#> [1] "Diet2" "Diet3" "Diet4"
#> 
#> attr(,"groupings")[[2]]
#>  [1] "Chick.L"  "Chick.Q"  "Chick.C"  "Chick^4"  "Chick^5"  "Chick^6" 
#>  [7] "Chick^7"  "Chick^8"  "Chick^9"  "Chick^10" "Chick^11" "Chick^12"
#> [13] "Chick^13" "Chick^14" "Chick^15" "Chick^16" "Chick^17" "Chick^18"
#> [19] "Chick^19" "Chick^20" "Chick^21" "Chick^22" "Chick^23" "Chick^24"
#> [25] "Chick^25" "Chick^26" "Chick^27" "Chick^28" "Chick^29" "Chick^30"
#> [31] "Chick^31" "Chick^32" "Chick^33" "Chick^34" "Chick^35" "Chick^36"
#> [37] "Chick^37" "Chick^38" "Chick^39" "Chick^40" "Chick^41" "Chick^42"
#> [43] "Chick^43" "Chick^44" "Chick^45" "Chick^46" "Chick^47" "Chick^48"
#> [49] "Chick^49"
#> 
#> <environment: 0x0000013d9a4d1da8>
mm$data
#> # A tibble: 578 × 56
#>    Diet2 Diet3 Diet4 Chick.L Chick.Q Chick.C `Chick^4` `Chick^5` Chick…¹ Chick…²
#>    <dbl> <dbl> <dbl>   <dbl>   <dbl>   <dbl>     <dbl>     <dbl>   <dbl>   <dbl>
#>  1     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  2     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  3     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  4     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  5     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  6     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  7     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  8     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#>  9     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#> 10     0     0     0  -0.103 -0.0745   0.167   -0.0643    -0.113   0.160 -0.0217
#> # … with 568 more rows, 46 more variables: `Chick^8` <dbl>, `Chick^9` <dbl>,
#> #   `Chick^10` <dbl>, `Chick^11` <dbl>, `Chick^12` <dbl>, `Chick^13` <dbl>,
#> #   `Chick^14` <dbl>, `Chick^15` <dbl>, `Chick^16` <dbl>, `Chick^17` <dbl>,
#> #   `Chick^18` <dbl>, `Chick^19` <dbl>, `Chick^20` <dbl>, `Chick^21` <dbl>,
#> #   `Chick^22` <dbl>, `Chick^23` <dbl>, `Chick^24` <dbl>, `Chick^25` <dbl>,
#> #   `Chick^26` <dbl>, `Chick^27` <dbl>, `Chick^28` <dbl>, `Chick^29` <dbl>,
#> #   `Chick^30` <dbl>, `Chick^31` <dbl>, `Chick^32` <dbl>, `Chick^33` <dbl>, …

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
#>    0.01    0.00   23.78
```

Test empirical clusters against the simulated null:

``` r
sapply(c("Diet2", "Diet3", "Diet4"), function(predictor) {
  empirical <- empirical_clusters[[predictor]]$sum_z
  null_dist <- null_dist_clusters[[predictor]]
  mean(abs(null_dist) > empirical)
})
#> Diet2 Diet3 Diet4 
#> 0.045 0.000 0.000
```

## Formula utilities

``` r
mtcars_sm <- mtcars[, c(1, 6:10)]
mtcars_sm$gear <- as.factor(mtcars_sm$gear)
head(mtcars_sm)
#>                    mpg    wt  qsec vs am gear
#> Mazda RX4         21.0 2.620 16.46  0  1    4
#> Mazda RX4 Wag     21.0 2.875 17.02  0  1    4
#> Datsun 710        22.8 2.320 18.61  1  1    4
#> Hornet 4 Drive    21.4 3.215 19.44  1  0    3
#> Hornet Sportabout 18.7 3.440 17.02  0  0    3
#> Valiant           18.1 3.460 20.22  1  0    3

jlmer_model_matrix(mpg ~ wt * qsec, mtcars_sm)
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec
#> attr(,"groupings")
#> list()
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $data
#> # A tibble: 32 × 7
#>       wt  qsec wt__qsec   mpg    vs    am gear 
#>    <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <fct>
#>  1  2.62  16.5     43.1  21       0     1 4    
#>  2  2.88  17.0     48.9  21       0     1 4    
#>  3  2.32  18.6     43.2  22.8     1     1 4    
#>  4  3.22  19.4     62.5  21.4     1     0 3    
#>  5  3.44  17.0     58.5  18.7     0     0 3    
#>  6  3.46  20.2     70.0  18.1     1     0 3    
#>  7  3.57  15.8     56.5  14.3     0     0 3    
#>  8  3.19  20       63.8  24.4     1     0 4    
#>  9  3.15  22.9     72.1  22.8     1     0 4    
#> 10  3.44  18.3     63.0  19.2     1     0 4    
#> # … with 22 more rows
jlmer_model_matrix(mpg ~ wt * qsec + (wt * qsec | am), mtcars_sm)
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 + wt + qsec + wt__qsec | 
#>     am)
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 + wt + qsec + wt__qsec | 
#>     am)
#> attr(,"groupings")
#> list()
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $data
#> # A tibble: 32 × 7
#>       wt  qsec wt__qsec    am   mpg    vs gear 
#>    <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <fct>
#>  1  2.62  16.5     43.1     1  21       0 4    
#>  2  2.88  17.0     48.9     1  21       0 4    
#>  3  2.32  18.6     43.2     1  22.8     1 4    
#>  4  3.22  19.4     62.5     0  21.4     1 3    
#>  5  3.44  17.0     58.5     0  18.7     0 3    
#>  6  3.46  20.2     70.0     0  18.1     1 3    
#>  7  3.57  15.8     56.5     0  14.3     0 3    
#>  8  3.19  20       63.8     0  24.4     1 4    
#>  9  3.15  22.9     72.1     0  22.8     1 4    
#> 10  3.44  18.3     63.0     0  19.2     1 4    
#> # … with 22 more rows
jlmer_model_matrix(mpg ~ wt * qsec + (wt * qsec || am), mtcars_sm)
#> $formula
#> mpg ~ 1 + wt + qsec + wt__qsec + (1 + wt + qsec + wt__qsec || 
#>     am)
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $julia_formula
#> mpg ~ 1 + wt + qsec + wt__qsec + zerocorr(1 + wt + qsec + wt__qsec | 
#>     am)
#> attr(,"groupings")
#> list()
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $data
#> # A tibble: 32 × 7
#>       wt  qsec wt__qsec    am   mpg    vs gear 
#>    <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <fct>
#>  1  2.62  16.5     43.1     1  21       0 4    
#>  2  2.88  17.0     48.9     1  21       0 4    
#>  3  2.32  18.6     43.2     1  22.8     1 4    
#>  4  3.22  19.4     62.5     0  21.4     1 3    
#>  5  3.44  17.0     58.5     0  18.7     0 3    
#>  6  3.46  20.2     70.0     0  18.1     1 3    
#>  7  3.57  15.8     56.5     0  14.3     0 3    
#>  8  3.19  20       63.8     0  24.4     1 4    
#>  9  3.15  22.9     72.1     0  22.8     1 4    
#> 10  3.44  18.3     63.0     0  19.2     1 4    
#> # … with 22 more rows
jlmer_model_matrix(mpg ~ wt * qsec + (wt * qsec || am), mtcars_sm, drop_terms = "wt")
#> $formula
#> mpg ~ 1 + qsec + wt__qsec + (1 + qsec + wt__qsec || am)
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $julia_formula
#> mpg ~ 1 + qsec + wt__qsec + zerocorr(1 + qsec + wt__qsec | am)
#> attr(,"groupings")
#> list()
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $data
#> # A tibble: 32 × 7
#>       wt  qsec wt__qsec    am   mpg    vs gear 
#>    <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <fct>
#>  1  2.62  16.5     43.1     1  21       0 4    
#>  2  2.88  17.0     48.9     1  21       0 4    
#>  3  2.32  18.6     43.2     1  22.8     1 4    
#>  4  3.22  19.4     62.5     0  21.4     1 3    
#>  5  3.44  17.0     58.5     0  18.7     0 3    
#>  6  3.46  20.2     70.0     0  18.1     1 3    
#>  7  3.57  15.8     56.5     0  14.3     0 3    
#>  8  3.19  20       63.8     0  24.4     1 4    
#>  9  3.15  22.9     72.1     0  22.8     1 4    
#> 10  3.44  18.3     63.0     0  19.2     1 4    
#> # … with 22 more rows
jlmer_model_matrix(mpg ~ gear * qsec + (gear + qsec | am), mtcars_sm)
#> $formula
#> mpg ~ 1 + gear4 + gear5 + qsec + gear4__qsec + gear5__qsec + 
#>     (1 + gear4 + gear5 + qsec | am)
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $julia_formula
#> mpg ~ 1 + gear4 + gear5 + qsec + gear4__qsec + gear5__qsec + 
#>     (1 + gear4 + gear5 + qsec | am)
#> attr(,"groupings")
#> attr(,"groupings")[[1]]
#> [1] "gear4" "gear5"
#> 
#> attr(,"groupings")[[2]]
#> [1] "gear4__qsec" "gear5__qsec"
#> 
#> <environment: 0x0000013d9a4d1da8>
#> 
#> $data
#> # A tibble: 32 × 10
#>    gear4 gear5  qsec gear4__qsec gear5__qsec    am   mpg    wt    vs gear 
#>    <dbl> <dbl> <dbl>       <dbl>       <dbl> <dbl> <dbl> <dbl> <dbl> <fct>
#>  1     1     0  16.5        16.5           0     1  21    2.62     0 4    
#>  2     1     0  17.0        17.0           0     1  21    2.88     0 4    
#>  3     1     0  18.6        18.6           0     1  22.8  2.32     1 4    
#>  4     0     0  19.4         0             0     0  21.4  3.22     1 3    
#>  5     0     0  17.0         0             0     0  18.7  3.44     0 3    
#>  6     0     0  20.2         0             0     0  18.1  3.46     1 3    
#>  7     0     0  15.8         0             0     0  14.3  3.57     0 3    
#>  8     1     0  20          20             0     0  24.4  3.19     1 4    
#>  9     1     0  22.9        22.9           0     0  22.8  3.15     1 4    
#> 10     1     0  18.3        18.3           0     0  19.2  3.44     1 4    
#> # … with 22 more rows
```
