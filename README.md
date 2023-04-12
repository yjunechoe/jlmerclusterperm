
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jlmerclusterperm

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Julia [GLM.jl](https://github.com/JuliaStats/GLM.jl) and
[MixedModels.jl](https://github.com/JuliaStats/MixedModels.jl)
implementation of bootstrapped cluster-based permutation analysis for
time series data, powered by
[`JuliaConnectoR`](https://github.com/stefan-m-lenz/JuliaConnectoR)

## Installation

You can install the development version of jlmerclusterperm from
[GitHub](https://github.com/yjunechoe/jlmerclusterperm) with:

``` r
# install.packages("remotes")
remotes::install_github("yjunechoe/jlmerclusterperm")
```

Using `jlmerclusterperm` requires a prior installation of Julia, which
can be downloaded from either the [official
website](https://julialang.org/) or via the command line utility
[juliaup](https://github.com/JuliaLang/juliaup). Julia version \>=1.8 is
required and \>=1.9 is preferred for its substantial compiler/runtime
improvements.

Before using functions from `jlmerclusterperm`, an initial setup step is
required via calling `jlmerclusterperm_setup()`. This will launch Julia
with some number of threads (controlled via
`options("jlmerclusterperm.nthreads")`) and install necessary Julia
package dependencies (this only happens once and should take about 15-30
minutes).

Calls to `jlmerclusterperm_setup()` after the initial setup incurs an
overhead of around 30 seconds and you will experience small delays at
first from Julia’s [just-in-time
compilation](https://docs.julialang.org/en/v1/). Afterwards you can
enjoy the blazingly-fast functions from the package.

``` r
# Both lines must be run
library(jlmerclusterperm)
system.time(jlmerclusterperm_setup(verbose = FALSE))
#>    user  system elapsed 
#>    0.02    0.02   23.40
```

## Example walkthrough

WIP…

## Notes on R-Julia interoperability

All `jlmerclusterperm` functions collect Julia objects as R objects,
except `jlmer` and `to_jlmer` which return GLM.jl or MixedModels.jl
fitted model objects.

``` r
jmod <- to_jlmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
jmod
#> Variance components:
#>             Column    Variance Std.Dev.   Corr.
#> Subject  (Intercept)  565.51066 23.78047
#>          Days          32.68212  5.71683 +0.08
#> Residual              654.94145 25.59182
#>  ──────────────────────────────────────────────────
#>                 Coef.  Std. Error      z  Pr(>|z|)
#> ──────────────────────────────────────────────────
#> (Intercept)  251.405      6.63226  37.91    <1e-99
#> Days          10.4673     1.50224   6.97    <1e-11
#> ──────────────────────────────────────────────────
glance(jmod) # see also `tidy()`
#> # A tibble: 1 × 7
#>    nobs sigma logLik   AIC   BIC deviance df.residual
#>   <int> <dbl>  <dbl> <dbl> <dbl>    <dbl>       <int>
#> 1   180  25.6  -876. 1764. 1783.    1752.         174
```

Model output should be comparable to `{lme4}` models:

``` r
library(lme4)
#> Loading required package: Matrix
rmod <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(rmod)
#> Linear mixed model fit by REML ['lmerMod']
#> Formula: Reaction ~ Days + (Days | Subject)
#>    Data: sleepstudy
#> 
#> REML criterion at convergence: 1743.6
#> 
#> Scaled residuals: 
#>     Min      1Q  Median      3Q     Max 
#> -3.9536 -0.4634  0.0231  0.4634  5.1793 
#> 
#> Random effects:
#>  Groups   Name        Variance Std.Dev. Corr
#>  Subject  (Intercept) 612.10   24.741       
#>           Days         35.07    5.922   0.07
#>  Residual             654.94   25.592       
#> Number of obs: 180, groups:  Subject, 18
#> 
#> Fixed effects:
#>             Estimate Std. Error t value
#> (Intercept)  251.405      6.825  36.838
#> Days          10.467      1.546   6.771
#> 
#> Correlation of Fixed Effects:
#>      (Intr)
#> Days -0.138
```

You can use functions from
[`JuliaConnectoR`](https://github.com/stefan-m-lenz/JuliaConnectoR) to
interact with these (pointers to) Julia objects:

``` r
library(JuliaConnectoR)
juliaLet("x.rePCA", x = jmod)
#> <Julia object of type NamedTuple{(:Subject,), Tuple{Vector{Float64}}}>
#> (Subject = [0.5406660465696776, 1.0],)
juliaCall("issingular", jmod)
#> [1] FALSE
```

You can also call functions from other Julia packages that you already
have installed. For example,
[Effects.jl](https://github.com/beacon-biosignals/Effects.jl) may be
useful for
[`marginaleffects`](https://github.com/vincentarelbundock/marginaleffects)/[`emmeans`](https://github.com/rvlenth/emmeans)
features:

``` r
juliaEval("using Effects")
juliaLet("effects(x.namedelements, y)", x = list(Days = unique(sleepstudy$Days)), y = jmod)
#> <Julia object of type DataFrame>
#> 10×5 DataFrame
#>  Row │ Days     Reaction  err       lower    upper
#>      │ Float64  Float64   Float64   Float64  Float64
#> ─────┼───────────────────────────────────────────────
#>    1 │     0.0   251.405   6.63226  244.773  258.037
#>    2 │     1.0   261.872   6.59566  255.277  268.468
#>    3 │     2.0   272.34    6.89435  265.445  279.234
#>    4 │     3.0   282.807   7.48832  275.319  290.295
#>    5 │     4.0   293.274   8.31451  284.96   301.589
#>    6 │     5.0   303.742   9.31132  294.43   313.053
#>    7 │     6.0   314.209  10.4299   303.779  324.639
#>    8 │     7.0   324.676  11.6353   313.041  336.311
#>    9 │     8.0   335.143  12.9031   322.24   348.046
#>   10 │     9.0   345.611  14.2167   331.394  359.827
```

The following packages are loaded into the Julia environment via
`jlmerclusterperm_setup()`:

``` r
cat(readLines(system.file(package = "jlmerclusterperm", "julia", "Project.toml")), sep = "\n")
#> [deps]
#> DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
#> GLM = "38e38edf-8417-5370-95a0-9cbb8c7f171a"
#> MixedModels = "ff71e718-51f3-5ec2-a782-8ffcbfa3c316"
#> ProgressMeter = "92933f4c-e287-5a05-a399-4b506db050ca"
#> Random = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"
#> Random123 = "74087812-796a-5b5d-8853-05524746bad3"
#> StatsBase = "2913bbd2-ae8a-5f71-8c99-4fb6c76f3a91"
#> StatsModels = "3eaba693-59b7-5ba5-a881-562e759f1c8d"
#> Suppressor = "fd094767-a336-5f1f-9728-57cf17d0bbfb"
```

If you wish to use other Julia packages not listed here like Effects.jl,
it is recommend to install them in the global library in a fresh Julia
session as opposed to `Pkg.add()`-ing from R using the `JuliaConnectoR`
interface, as that may pollute the `jlmerclusterperm` project
environment (if this happens, just re-install `jlmerclusterperm`).

Version info:

``` r
R.version
#>                _                                
#> platform       x86_64-w64-mingw32               
#> arch           x86_64                           
#> os             mingw32                          
#> crt            ucrt                             
#> system         x86_64, mingw32                  
#> status                                          
#> major          4                                
#> minor          2.2                              
#> year           2022                             
#> month          10                               
#> day            31                               
#> svn rev        83211                            
#> language       R                                
#> version.string R version 4.2.2 (2022-10-31 ucrt)
#> nickname       Innocent and Trusting
juliaCall("versioninfo")
#> Julia Version 1.9.0-rc2
#> Commit 72aec423c2 (2023-04-01 10:41 UTC)
#> Platform Info:
#>   OS: Windows (x86_64-w64-mingw32)
#>   CPU: 8 × 11th Gen Intel(R) Core(TM) i7-1165G7 @ 2.80GHz
#>   WORD_SIZE: 64
#>   LIBM: libopenlibm
#>   LLVM: libLLVM-14.0.6 (ORCJIT, tigerlake)
#>   Threads: 7 on 8 virtual cores
#> Environment:
#>   JULIA_NUM_THREADS = 7
```
