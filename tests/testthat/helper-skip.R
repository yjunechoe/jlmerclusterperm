no_julia <- !JuliaConnectoR::juliaSetupOk()
skip_conditionally <- function() {
  # skip_on_cran()
  if (no_julia) {
    testthat::skip("No Julia installation detected.")
  }
  if (!julia_version_compatible()) {
    testthat::skip("Julia version >=1.8 required.")
  }
  invisible()
}
