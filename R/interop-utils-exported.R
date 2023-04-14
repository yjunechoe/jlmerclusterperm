#' Set/get options for Julia progress bar
#'
#' @param show Whether to show the progress bar. You may also pass in a list of `"show"` and `"width"`.
#' @param width Width of the progress bar.
#'
#' @return Previous values for `show` and `width`
#' @export
julia_progress <- function(show, width) {
  old_opts <- JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval("(show = !(pg_io isa Base.DevNull), width = pg_width)"))
  if (!missing(show)) {
    if (is.list(show) && identical(names(show), c("show", "width"))) {
      width <- show$width
      show <- show$show
    }
    JuliaConnectoR::juliaEval(paste0("pg_io = ", if (show) "stderr" else "devnull"))
  }
  if (!missing(width)) JuliaConnectoR::juliaEval(paste0("pg_width = ", width - 50))
  invisible(strip_JLTYPE(old_opts))
}
