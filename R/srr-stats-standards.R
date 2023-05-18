#' srr_stats
#'
#' All of the following standards initially have `@srrstats` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstats` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' @srrstats {G1.0} References Maris & Oostenveld (2007) which originally proposed the cluster-based permutation analysis.
#' @srrstats {G1.1} Package is an improvement over existing implementations in R (mainly in speed and interpretability).
#'  This is explained in the readme and the case study vignettes.
#' @srrstats {G1.2} Lifecycle is active and stable.
#' @srrstats {G1.3} The many moving parts are explained across the readme, the function documentation, and the topics/case study vignettes.
#'  Users are assumed to already have familiarity with (genearlized, mixed-effects) regression to use this package.
#' @srrstats {G1.4} `roxygen2` is used throughout the package.
#' @srrstats {G1.4a}
#' @srrstats {G1.6} Some performance comparisons are available in the case study vignettes upon following the links to original tutorials they replicate.
#' @srrstats {G2.0} Function arguments passed to Julia are appropriately handled in Julia. Outputs from Julia to R are checked on type and length.
#' @srrstats {G2.0a}
#' @srrstats {G2.1} Function arguments passed to Julia are appropriately handled in Julia. Type conversion is applied where R and Julia disagree.
#' @srrstats {G2.1a}
#' @srrstats {G2.2} Package checks length of parameters where appropriate.
#' @srrstats {G2.3} Functions use `match.arg()` where appropriate. Character parameters are documented as lower-case and functions do not implement implicit `tolower()` conversion.
#' @srrstats {G2.3a}
#' @srrstats {G2.3b}
#' @srrstats {G2.4} The occasional type conversions happen in transporting data between R and Julia and these are handled appropriately.
#' @srrstats {G2.4a}
#' @srrstats {G2.4b}
#' @srrstats {G2.4c}
#' @srrstats {G2.4d}
#' @srrstats {G2.4e}
#' @srrstats {G2.5} Inputs are never strictly expected as factor, but users are expected to be aware of the concept of contrast coding,
#'  such that they can further control the numerical coding of their categorical variables with `contrasts()`.
#' @srrstats {G2.6} Values are appropriately pre-processed regardless of class structures where appropriate
#' @srrstats {G2.7} Package accepts any tabular structure that is `model.matrix()`-able.
#' @srrstats {G2.8} Package applies a strict gateway via `make_jlmer_spec()` where all CPA-related functions require a pre-processed `<jlmer_spec>` object.
#' @srrstats {G2.9} Messages dropped rows and invalid names on `model.matrix()` conversion.
#' @srrstats {G2.10} Handled via `model.matrix()`
#' @srrstats {G2.11} Handled via `model.matrix()`
#' @srrstats {G2.12} Handled via `model.matrix()`
#' @srrstats {G2.13} Handled via `model.matrix()` and informs on dropping NA rows.
#' @srrstats {G2.14} `make_jlmer_spec()` informs about NA rows. Imputation not implemented as it does not make sense for this package.
#' @srrstats {G2.14a}
#' @srrstats {G2.14b}
#' @srrstats {G2.14c}
#' @srrstats {G2.15} Functions check for missinginess in output where appropriate.
#' @srrstats {G2.16} Undefined values are caught during model fitting in Julia.
#' @srrstats {G3.0} Equality comparison between floats use tolerance (e.g., the internal function `near_zero()`)
#' @srrstats {G5.0} Uses the built-in `ChickWeight` dataset for tests
#' @srrstats {G5.2} Appropriate message/warning/error tests are in `/tests/testthat`
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G5.3} Tests for correctness but no explicit test for NA
#' @srrstats {G5.4} Tests compare to R `lm()` output.
#' @srrstats {G5.4a}
#' @srrstats {G5.4b}
#' @srrstats {G5.4c}
#' @srrstats {G5.5} Uses shared Julia RNG state to test correctness
#' @srrstats {G5.8} Edge conditions on data are caught firstly by `model.matrix()` (inside `make_jlmer_spec()`) and also in Julia when handing the data off to GLM/MixedModels
#' @srrstats {G5.8a}
#' @srrstats {G5.8b}
#' @srrstats {G5.8c}
#' @srrstats {G5.8d}
#' @srrstats {G5.9} Tests for stochastic nature of the CPA under different RNG states
#' @srrstats {G5.9a}
#' @srrstats {G5.9b}
#' @srrstats {G5.10} Tests ran on codecov.
#' @srrstats {RE1.0} Uses R formula interface
#' @srrstats {RE1.1} Handled standardly via `model.matrix()`
#' @srrstats {RE1.2} See G2.5
#' @srrstats {RE1.3} Handled standardly via `model.matrix()`. Users can see the model matrix with contrasts spelled-out upon printing a `<jlmer_spec>` object
#' @srrstats {RE1.3a}
#' @srrstats {RE2.1} See G2.14
#' @srrstats {RE3.0} Issues singularity messages and excludes runs with convergence failures in permutation testing (and informs of this)
#' @srrstats {RE3.1} Convergence failures can be retrieved from function outputs, but users are encouraged to watch out for warnings and messages.
#'  These can be suppressed via the `suppress*()` functions.
#' @srrstats {RE3.3} Convergence thresholds can be explicitly set by passing the appropriate argument to the `...` of functions that call GLM/MixedModels
#' @srrstats {RE4.0} `jlmer()` and `to_jlmer()` return pointers to Julia model objects.
#' @srrstats {RE4.2} Model coefficients via `tidy()`
#' @srrstats {RE4.3} Confidence intervals printed in `print.jlmer_mod()` for LMs, not implemented for LMEMs.
#' @srrstats {RE4.4} Formula printed via `print.jlmer_mod()` or evaluating Julia code on model object via JuliaConnectoR
#' @srrstats {RE4.5} Numbers of observations via `glance()`
#' @srrstats {RE4.6} The variance-covariance matrix of the model parameters via `print.jlmer_mod()` or evaluating Julia code on model object via JuliaConnectoR
#' @srrstats {RE4.7} Convergence statistics can be retrieved from evaluating Julia code on model object via JuliaConnectoR
#' @srrstats {RE4.8} Response variable and metadata printed via `print.jlmer_mod()` or evaluating Julia code on model object via JuliaConnectoR
#' @srrstats {RE4.10} Model Residuals via `glance()`. Users are assumed to be familiar.
#' @srrstats {RE4.11} Goodness-of-fit and other statistics via `glance()`
#' @srrstats {RE4.13} Predictor variables and metadata printed via `print.jlmer_mod()` or evaluating Julia code on model object via JuliaConnectoR
#' @srrstats {RE4.17} Print method defined for all custom S3 objects.
#' @srrstats {RE7.3} Tests for `print()`, `tidy()`, and `glance()`
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstats`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#' @srrstatsNA {G1.5} There are no associated publications with the package.
#' @srrstatsNA {G3.1} Package does not rely on covariance calculations
#' @srrstatsNA {G3.1a}
#' @srrstatsNA {G4.0} Package does not enable outputs to be written to local files
#' @srrstatsNA {G5.1} Package does not create any data sets to export.
#' @srrstatsNA {G5.6} No parameter recovery tests, but handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {G5.6a}
#' @srrstatsNA {G5.6b}
#' @srrstatsNA {G5.7} No Algorithm performance tests on regression itself, but handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {G5.11} Tests only require built-in datasets
#' @srrstatsNA {G5.11a}
#' @srrstatsNA {G5.12} All tests are trivial checks on quality and correctness.
#' @srrstatsNA {RE1.4} No data transformations applied beyond numerical coding of categorical variables via `model.matrix()`
#' @srrstatsNA {RE2.0} No data transformations applied beyond numerical coding of categorical variables via `model.matrix()`
#' @srrstatsNA {RE2.2} Missing data in response variable not allowed.
#' @srrstatsNA {RE2.3} Centering data not within scope of the package.
#' @srrstatsNA {RE2.4} Colinearity checks not within scope of the package.
#' @srrstatsNA {RE2.4a}
#' @srrstatsNA {RE2.4b}
#' @srrstatsNA {RE3.2} Handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {RE4.1} Generating a model object without actually fitting values not within the scope of the package.
#' @srrstatsNA {RE4.9} Modelled values of response variables not within scope of the package
#' @srrstatsNA {RE4.12} Transformation functions not within the scope of the package.
#' @srrstatsNA {RE4.14} Extrapolation or forecast *errors* capability not within the scope of the package.
#' @srrstatsNA {RE4.15} Forecase-related functionalities not within the scope of the package.
#' @srrstatsNA {RE4.16} No feature for modelling distinct responses for different categorical groups.
#' @srrstatsNA {RE4.18} No `summary()` method implemented for Julia model objects but see `tidy.jlmer_mod()` and `glance.jlmer_mod()`
#' @srrstatsNA {RE5.0} Scaling relationships on speed not checked, but handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {RE6.0} No default `plot()` methods but users may interface with Julia-specific visualization tools via JuliaConnectoR.
#' @srrstatsNA {RE6.1} See RE6.0
#' @srrstatsNA {RE6.2} See RE6.0
#' @srrstatsNA {RE6.3} See RE4.15
#' @srrstatsNA {RE7.0} Tests with noiseless, exact relationships between predictor (independent) data handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {RE7.0a}
#' @srrstatsNA {RE7.1} Tests with noiseless, exact relationships between predictor (independent) and response (dependent) data handled in GLM.jl and MixedModels.jl
#' @srrstatsNA {RE7.1a}
#' @srrstatsNA {RE7.2} Output objects are designed to strip meta-data attributes input data for R-Julia interoperability
#' @srrstatsNA {RE7.4} See RE4.15
#' @noRd
NULL
