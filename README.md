
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

![Animation of a cluster-based permutation test where each frame steps
through a range of
t-thresholds.](man/figures/clusterpermute_animation.gif)

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
#>    0.02    0.02   23.07
```

## Example walkthrough

WIP…
