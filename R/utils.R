# utils.R ----

#' Timestamped console logging, CPB style
#'
#' A small timestamped wrapper around [cat()], matching the logging
#' convention used throughout CPB scripts. Mainly intended for progress
#' messages in scripts, build pipelines and vignette/example code; used
#' internally by [save_cpb()] to confirm the path it wrote to.
#'
#' @param ... Passed to [cat()] and concatenated after the timestamp.
#' @return Invisibly, `NULL`. Called for its side effect of printing to
#'   the console.
#' @examples
#' tcat("starting export")
#' @export
tcat <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), ..., "\n", sep = "")
  invisible(NULL)
}
