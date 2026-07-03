# fonts.R ----
#
# Registration of the bundled RijksoverheidSansText font family. Loading
# ggcpb must never fail because of fonts: registration is wrapped in
# tryCatch() and falls back to the ggplot2 default family ("") with a
# single warning if a backend or a font file is missing.

.ggcpb_env <- new.env(parent = emptyenv())
.ggcpb_env$font_registered <- FALSE

#' Paths to the bundled RijksoverheidSansText font files
#'
#' @return A named list of four file paths (`plain`, `bold`, `italic`,
#'   `bolditalic`). Paths are resolved with [system.file()] and may be
#'   `""` if a file is not found.
#' @noRd
cpb_font_files <- function() {
  list(
    plain = system.file(
      "fonts", "RijksoverheidSansText-Regular_2_0.ttf", package = "ggcpb"
    ),
    bold = system.file(
      "fonts", "RijksoverheidSansText-Bold_2_0.ttf", package = "ggcpb"
    ),
    italic = system.file(
      "fonts", "RijksoverheidSansText-Italic_2_0.ttf", package = "ggcpb"
    ),
    bolditalic = system.file(
      "fonts", "RijksoverheidSansText-BoldItalic_2_0.ttf", package = "ggcpb"
    )
  )
}

#' Register the CPB "RijksoverheidSansText" font family
#'
#' Registers the bundled Rijksoverheid Sans Text font family with both
#' \pkg{systemfonts} (so `ragg`/`ggplot2` output picks it up) and
#' \pkg{sysfonts} (so a `showtext::showtext_auto()` rendering path picks
#' it up as well). This is run automatically when ggcpb is loaded, but
#' can be re-run on demand -- e.g. after toggling `showtext_auto()`, or
#' in a fresh parallel worker that does not inherit the main session's
#' font registry.
#'
#' Registration never raises an error: if the bundled font files cannot
#' be found, or a backend fails to register them, a single warning is
#' issued and [cpb_font_family()] subsequently returns `""`, letting
#' [theme_cpb()] fall back to the ggplot2 default font.
#'
#' @return Invisibly, `TRUE` if at least one backend registered the
#'   font successfully, `FALSE` otherwise.
#' @examples
#' cpb_register_fonts()
#' cpb_font_family()
#' @export
cpb_register_fonts <- function() {
  files <- cpb_font_files()

  # RijksoverheidSansText 2.0 ships regular, bold and italic but no
  # bold-italic face. theme_cpb() only ever asks for those three (bold
  # titles/strips, italic subtitle/axis/legend titles) and never combines
  # bold + italic, so only these three are required.
  required <- c("plain", "bold", "italic")
  have_required <- all(vapply(
    files[required], function(f) nzchar(f) && file.exists(f), logical(1)
  ))

  if (!have_required) {
    warning(
      "ggcpb: could not find the bundled RijksoverheidSansText font files ",
      "in inst/fonts/; falling back to the default ggplot2 font family. ",
      "See the ggcpb README for the path-based fallback.",
      call. = FALSE
    )
    .ggcpb_env$font_registered <- FALSE
    return(invisible(FALSE))
  }

  # No bold-italic file: reuse the bold face for that slot so a stray
  # bold + italic request keeps the bold weight instead of erroring.
  bolditalic <- if (nzchar(files$bolditalic) && file.exists(files$bolditalic)) {
    files$bolditalic
  } else {
    files$bold
  }

  ok_systemfonts <- tryCatch(
    {
      systemfonts::register_font(
        name       = "RijksoverheidSansText",
        plain      = files$plain,
        bold       = files$bold,
        italic     = files$italic,
        bolditalic = bolditalic
      )
      TRUE
    },
    error = function(e) FALSE
  )

  ok_sysfonts <- tryCatch(
    {
      sysfonts::font_add(
        family     = "RijksoverheidSansText",
        regular    = files$plain,
        bold       = files$bold,
        italic     = files$italic,
        bolditalic = bolditalic
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!ok_systemfonts && !ok_sysfonts) {
    warning(
      "ggcpb: font registration failed for both the systemfonts and ",
      "sysfonts backends; falling back to the default ggplot2 font family.",
      call. = FALSE
    )
    .ggcpb_env$font_registered <- FALSE
    return(invisible(FALSE))
  }

  .ggcpb_env$font_registered <- TRUE
  invisible(TRUE)
}

#' The CPB font family name for use in `theme_cpb()`
#'
#' @return `"RijksoverheidSansText"` if the font was registered
#'   successfully (see [cpb_register_fonts()]), or `""` otherwise -- an
#'   empty string tells ggplot2 to use its built-in default family, so
#'   plots still render correctly even without the CPB font.
#' @examples
#' cpb_font_family()
#' @export
cpb_font_family <- function() {
  if (isTRUE(.ggcpb_env$font_registered)) "RijksoverheidSansText" else ""
}
