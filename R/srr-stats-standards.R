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
#' @srrstats {G2.6} Values are appropriately pre-processed regardless of class structures where appropriate
#' @srrstats {G2.14a}
#' @srrstats {G2.14b}
#' @srrstats {G2.14c}
#' @srrstats {G2.15} Functions check for missinginess in output where appropriate.
#' @srrstats {G5.2a}
#' @srrstats {G5.2b}
#' @srrstats {G5.4a}
#' @srrstats {G5.4b}
#' @srrstats {G5.4c}
#' @srrstats {G5.8a}
#' @srrstats {G5.8b}
#' @srrstats {G5.8c}
#' @srrstats {G5.8d}
#' @srrstats {G5.9a}
#' @srrstats {G5.9b}
#' @srrstats {G5.10} Tests ran on codecov.
#' @srrstats {RE1.2} See G2.5
#' @srrstats {RE1.3a}
#' @srrstats {RE2.1} See G2.14
#' @srrstats {RE4.7} Convergence statistics can be retrieved from evaluating Julia code on model object via JuliaConnectoR
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
