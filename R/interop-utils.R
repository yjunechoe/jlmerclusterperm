#' Set/get options for Julia progress bar
#'
#' @param show Whether to show the progress bar. You may also pass in a list of `"show"` and `"width"`.
#' @param width Width of the progress bar. If `"auto"`, adjusts the progress bar width to fit the console.
#'
#' @examplesIf JuliaConnectoR::juliaSetupOk()
#' \donttest{
#' \dontshow{
#' options("jlmerclusterperm.nthreads" = 2)
#' jlmerclusterperm_setup(verbose = FALSE)
#' }
#'
#' # Show current progress options
#' julia_progress()
#'
#' # Set options and save previous options
#' old_progress_opts <- julia_progress(show = FALSE, width = 100)
#' julia_progress()
#'
#' # Restoring progress settings by passing a list of old options
#' old_progress_opts
#' julia_progress(old_progress_opts)
#' identical(julia_progress(), old_progress_opts)
#'
#' # Alternatively, reset to default settings using this syntax:
#' julia_progress(show = TRUE, width = "auto")
#'
#' \dontshow{
#' JuliaConnectoR::stopJulia()
#' }
#' }
#'
#' @return Previous values for `show` and `width`
#' @export
julia_progress <- function(show, width) {
  show_missing <- missing(show)
  both_missing <- show_missing && missing(width)
  opts_is_list <- !show_missing && is.list(show) && identical(names(show), c("show", "width"))
  old_opts <- JuliaConnectoR::juliaGet(JuliaConnectoR::juliaEval("(show = !(pg_io isa Base.DevNull), width = pg_width)"))
  if (!show_missing) {
    if (opts_is_list) {
      width <- show$width
      show <- show$show
    }
    JuliaConnectoR::juliaEval(paste0("pg_io = ", if (show) "stderr" else "devnull"))
  }
  if (!missing(width)) {
    if (width == "auto") {
      width <- max(1L, cli::console_width() - 44L)
    }
    JuliaConnectoR::juliaEval(paste0("pg_width = ", width))
  }
  out <- strip_JLTYPE(old_opts)
  if (both_missing) {
    out
  } else {
    invisible(out)
  }
}
