# zzz.R ----

.onLoad <- function(libname, pkgname) {
  # font registration must never fail package load; cpb_register_fonts()
  # already reports problems via a single warning() rather than an error,
  # so only guard against an unexpected hard error here.
  tryCatch(
    cpb_register_fonts(),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}
