# zzz.R ----

.onLoad <- function(libname, pkgname) {
  # Font registration must never fail package load; cpb_register_fonts()
  # already reports problems via a single warning() rather than an error,
  # so only guard against an unexpected hard error here.
  #
  # enable_showtext = FALSE: attaching a package must not change how the
  # rest of the session draws text. Users who want showtext on every
  # device call cpb_register_fonts() themselves.
  tryCatch(
    cpb_register_fonts(enable_showtext = FALSE),
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  # CPB figures are labelled in Dutch, so accented characters are
  # routine ("reele", "geindexeerd" with their diaereses). Outside a
  # UTF-8 locale R reads each one as two undefined bytes and the
  # graphics devices draw them as missing glyphs -- silently, so the
  # chart looks finished and reads as mojibake. Say so on attach
  # rather than let it reach a published figure.
  #
  # Deliberately only a message: resetting the session's locale as a
  # side effect of library() would be a surprising global change.
  if (!cpb_utf8_locale()) {
    packageStartupMessage(
      "ggcpb: LC_CTYPE is '", Sys.getlocale("LC_CTYPE"), "', not UTF-8, so ",
      "accented\n  characters will be drawn as missing glyphs. Fix with e.g.\n",
      '  Sys.setlocale("LC_CTYPE", "C.UTF-8")',
      "  (or start R with LC_ALL=C.UTF-8)."
    )
  }
  invisible(NULL)
}

#' Is the session in a UTF-8 locale?
#'
#' @return `TRUE` when `LC_CTYPE` can represent the accented characters
#'   that Dutch figure labels rely on.
#' @keywords internal
#' @noRd
cpb_utf8_locale <- function() {
  ctype <- Sys.getlocale("LC_CTYPE")
  grepl("UTF-?8", ctype, ignore.case = TRUE) || identical(l10n_info()$`UTF-8`, TRUE)
}
