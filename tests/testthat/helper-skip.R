no_julia <- !JuliaConnectoR::juliaSetupOk()
skip_conditionally <- function() {
  # skip_on_cran()
  if (no_julia) {
    skip("No Julia installation detected.")
  }
  if (!julia_version_compatible()) {
    skip("Julia version >=1.8 required.")
  }
  invisible()
}
