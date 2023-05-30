skip_conditionally <- function() {
  skip_on_cran()
  if (!JuliaConnectoR::juliaSetupOk()) {
    skip("Julia setup not okay.")
  }
  invisible()
}
