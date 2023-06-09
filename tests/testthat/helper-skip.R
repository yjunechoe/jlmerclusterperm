no_julia <- !JuliaConnectoR::juliaSetupOk()
skip_conditionally <- function() {
  # skip_on_cran()
  if (no_julia) {
    skip("Julia setup not okay.")
  }
  invisible()
}
