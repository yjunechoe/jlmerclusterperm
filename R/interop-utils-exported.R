#' Set/get options for Julia progress bar
#'
#' @param show Whether to show the progress bar. You may also pass in a list of `"show"` and `"width"`.
#' @param width Width of the progress bar.
#'
#' @examples
#' \dontrun{
#' jlmerclusterperm_setup()
#'
#' # Set Julia progress options and save old state
#' old_progress_opts <- julia_progress(show = FALSE, width = 30)
#' old_progress_opts
#'
#' # Restored old state
#' julia_progress(old_progress_opts)
#' identical(old_progress_opts, julia_progress())
#' }
#'
#' @return Previous values for `show` and `width`
#' @export
julia_progress <- function(show, width) {
  show_missing <- missing(show)
  width_missing <- missing(width)
  old_opts <- JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval("(show = !(pg_io isa Base.DevNull), width = pg_width)"))
  if (!show_missing) {
    if (is.list(show) && identical(names(show), c("show", "width"))) {
      width <- show$width
      show <- show$show
    }
    JuliaConnectoR::juliaEval(paste0("pg_io = ", if (show) "stderr" else "devnull"))
  }
  if (!width_missing) JuliaConnectoR::juliaEval(paste0("pg_width = ", width))
  if (show_missing && width_missing) {
    strip_JLTYPE(old_opts)
  } else {
    invisible(strip_JLTYPE(old_opts))
  }
}
