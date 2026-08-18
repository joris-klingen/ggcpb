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

#' Which `font_add()` slot a font file belongs in
#'
#' @param italic,bold Logical flags read from the font's own metadata.
#' @return One of `"plain"`, `"bold"`, `"italic"`, `"bolditalic"`.
#' @noRd
cpb_font_face <- function(bold, italic) {
  if (isTRUE(bold) && isTRUE(italic)) "bolditalic"
  else if (isTRUE(bold)) "bold"
  else if (isTRUE(italic)) "italic"
  else "plain"
}

#' Guess family and face from a font file name
#'
#' Only used when the file carries no usable metadata: a name like
#' `RijksoverheidSansText-BoldItalic_2_0.ttf` still tells us both parts.
#' @noRd
cpb_font_from_name <- function(file) {
  stem <- tools::file_path_sans_ext(basename(file))
  # drop a trailing version suffix such as "_2_0"
  stem <- sub("[_-][0-9]+([_.-][0-9]+)*$", "", stem)
  face_pat <- "[ _-]?(BoldItalic|BoldOblique|Italic|Oblique|Bold|Regular|Book|Roman)$"
  m <- regmatches(stem, regexpr(face_pat, stem, ignore.case = TRUE))
  token <- tolower(gsub("[ _-]", "", m))
  face <- cpb_font_face(
    bold   = grepl("bold", token),
    italic = grepl("italic|oblique", token)
  )
  list(family = sub(face_pat, "", stem, ignore.case = TRUE), face = face)
}

#' Scan a directory for font files and read their family and face
#'
#' @inheritParams cpb_register_fonts
#' @return A data frame with one row per readable font file (`file`,
#'   `family`, `face`).
#' @noRd
cpb_scan_fonts <- function(path, recursive = FALSE) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single directory path.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("`path` is not an existing directory: ", path, call. = FALSE)
  }
  files <- list.files(
    path, pattern = "\\.(ttf|otf|ttc)$", ignore.case = TRUE,
    full.names = TRUE, recursive = isTRUE(recursive)
  )
  if (!length(files)) {
    return(data.frame(file = character(), family = character(),
                      face = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(files, function(f) {
    # the font's own metadata is more reliable than its file name: the
    # same face is shipped as -Regular, -Book and -Roman by different
    # foundries. Fall back to the name only when the file cannot be read.
    info <- tryCatch(systemfonts::font_info(path = f, index = 0),
                     error = function(e) NULL)
    if (is.null(info) || !nzchar(info$family[[1]])) {
      guess <- cpb_font_from_name(f)
      return(data.frame(file = f, family = guess$family, face = guess$face,
                        stringsAsFactors = FALSE))
    }
    data.frame(
      file   = f,
      family = info$family[[1]],
      # font_info() returns weight as an *ordered factor*, so compare on
      # the character value; bold and anything heavier maps to the bold
      # slot, which is the only weight theme_cpb() asks for
      face   = cpb_font_face(
        bold   = as.character(info$weight[[1]]) %in%
                   c("bold", "ultrabold", "heavy"),
        italic = isTRUE(info$italic[[1]])
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Register one family with the Windows base device
#'
#' A no-op off Windows. Note that [grDevices::windowsFont()] maps a
#' name onto a font *already known to Windows itself* -- it cannot load
#' a file -- so this only helps for families installed in the OS. For a
#' font that is merely bundled with a package, `showtext` is the route
#' that works.
#' @return `TRUE` if the mapping was created.
#' @noRd
cpb_register_windows_font <- function(family) {
  if (!identical(.Platform$OS.type, "windows")) return(FALSE)
  tryCatch({
    # fetched at run time so the call does not exist on other platforms
    windowsFonts <- get("windowsFonts", envir = asNamespace("grDevices"))
    windowsFont  <- get("windowsFont",  envir = asNamespace("grDevices"))
    args <- stats::setNames(list(windowsFont(family)), family)
    do.call(windowsFonts, args)
    TRUE
  }, error = function(e) FALSE)
}

#' Register a single family with every available backend
#'
#' @param family Family name.
#' @param faces Named character vector of file paths, names being
#'   `plain`/`bold`/`italic`/`bolditalic`.
#' @return A one-row data frame describing what happened.
#' @noRd
cpb_register_family <- function(family, faces, windows_device = TRUE) {
  slot <- function(nm) if (!is.na(faces[[nm]])) faces[[nm]] else NA_character_
  plain <- slot("plain")
  # a family with no regular face is still usable: fall back to whatever
  # face we do have rather than dropping it
  if (is.na(plain)) plain <- faces[!is.na(faces)][[1]]
  bold       <- if (!is.na(slot("bold"))) slot("bold") else plain
  italic     <- if (!is.na(slot("italic"))) slot("italic") else plain
  # no bold-italic file: reuse bold so a stray bold+italic request keeps
  # the bold weight instead of erroring
  bolditalic <- if (!is.na(slot("bolditalic"))) slot("bolditalic") else bold

  ok_systemfonts <- tryCatch({
    systemfonts::register_font(name = family, plain = plain, bold = bold,
                               italic = italic, bolditalic = bolditalic)
    TRUE
  }, error = function(e) FALSE)

  ok_sysfonts <- tryCatch({
    sysfonts::font_add(family = family, regular = plain, bold = bold,
                       italic = italic, bolditalic = bolditalic)
    TRUE
  }, error = function(e) FALSE)

  ok_windows <- if (isTRUE(windows_device)) {
    cpb_register_windows_font(family)
  } else {
    FALSE
  }

  data.frame(
    family      = family,
    faces       = paste(names(faces)[!is.na(faces)], collapse = ", "),
    n_files     = sum(!is.na(faces)),
    systemfonts = ok_systemfonts,
    sysfonts    = ok_sysfonts,
    windows     = ok_windows,
    stringsAsFactors = FALSE
  )
}

#' Register the CPB "RijksoverheidSansText" font family
#'
#' Registers the bundled Rijksoverheid Sans Text font family with
#' \pkg{systemfonts} (so `ragg`/`svglite`/`ggplot2` output picks it up)
#' and \pkg{sysfonts} (the registry \pkg{showtext} draws from), and
#' optionally turns \pkg{showtext} on so the font also renders on
#' devices that do their own font lookup. Run automatically when ggcpb
#' is loaded, and safe to re-run on demand -- e.g. in a fresh parallel
#' worker that does not inherit the main session's font registry.
#'
#' Registration never raises an error: if the bundled font files cannot
#' be found, or a backend fails to register them, a single warning is
#' issued and [cpb_font_family()] subsequently returns `""`, letting
#' [theme_cpb()] fall back to the ggplot2 default font.
#'
#' @section Why registration alone is not enough:
#' `sysfonts::font_add()` adds a font to showtext's own registry; it
#' does **not** teach a graphics device about it. On the base Windows
#' device that surfaces as
#' `"font family not found in Windows font database"` at draw time even
#' though `sysfonts::font_families()` lists the font. Something has to
#' actually draw the glyphs:
#'
#' * `enable_showtext = TRUE` (the default) calls
#'   [showtext::showtext_auto()], which makes showtext render text on
#'   *any* device, including the base Windows one and the RStudio pane.
#'   This is the route that reliably works for a bundled font.
#' * `windows_device = TRUE` additionally maps the family through
#'   [grDevices::windowsFonts()]. This is a no-op off Windows, and it
#'   only helps for fonts **installed in Windows itself** --
#'   `windowsFont()` maps a name onto a font the OS already knows and
#'   cannot load a file. Install the font system-wide to use this route
#'   without showtext.
#' * The modern devices (`ragg::agg_png()`, `svglite`, the RStudio
#'   `AGG` pane) consult \pkg{systemfonts} and need neither.
#'
#' @section The showtext DPI gotcha:
#' showtext rasterises text at a DPI it is told about, *not* the one the
#' device is using. If the two disagree, text comes out too large or too
#' small while the rest of the plot is fine -- most visibly when saving
#' at high resolution:
#'
#' ```r
#' showtext::showtext_opts(dpi = 300)
#' ggplot2::ggsave("fig.png", p, dpi = 300)
#' showtext::showtext_opts(dpi = 96)    # back to screen
#' ```
#'
#' Keep `showtext_opts(dpi =)` equal to the device `res`/`dpi` for every
#' save. [save_cpb()] and the `ragg` devices go through
#' \pkg{systemfonts} instead, so they size text correctly regardless and
#' are the simpler path when you are only exporting files.
#'
#' @param path Optional single directory holding font files (`.ttf`,
#'   `.otf`, `.ttc`) to register in addition to the bundled family. Each
#'   file's family and face are read from its own metadata where
#'   possible, falling back to the file name, and the regular/bold/
#'   italic/bold-italic variants of one family are mapped onto the
#'   matching [sysfonts::font_add()] slots. `NULL` (default) registers
#'   only the bundled font.
#' @param recursive If `TRUE`, search `path` recursively. Defaults to
#'   `FALSE`.
#' @param enable_showtext If `TRUE` (default), call
#'   [showtext::showtext_auto()] so the registered fonts render on every
#'   device. Set to `FALSE` to leave the session's rendering path
#'   untouched -- this is what package load does, so merely attaching
#'   ggcpb never changes how your other plots draw text. Ignored with a
#'   warning if \pkg{showtext} is not installed.
#' @param windows_device If `TRUE` (default), also map each family
#'   through [grDevices::windowsFonts()]. No effect off Windows; see
#'   the section above for when it helps.
#'
#' @return Invisibly, a data frame with one row per family registered
#'   and columns `family`, `faces`, `n_files`, `systemfonts`,
#'   `sysfonts` and `windows`, so you can check what actually took. It
#'   carries a `showtext` attribute recording whether showtext
#'   rendering is on.
#' @examples
#' cpb_register_fonts(enable_showtext = FALSE)
#' cpb_font_family()
#'
#' # register every font in a directory as well
#' \dontrun{
#' cpb_register_fonts(path = "~/fonts", recursive = TRUE)
#' }
#' @export
cpb_register_fonts <- function(path = NULL,
                               recursive = FALSE,
                               enable_showtext = TRUE,
                               windows_device = TRUE) {
  files <- cpb_font_files()

  # RijksoverheidSansText 2.0 ships regular, bold and italic but no
  # bold-italic face. theme_cpb() only ever asks for those three (bold
  # titles/strips, italic subtitle/axis/legend titles) and never combines
  # bold + italic, so only these three are required.
  required <- c("plain", "bold", "italic")
  have_required <- all(vapply(
    files[required], function(f) nzchar(f) && file.exists(f), logical(1)
  ))

  registered <- NULL

  if (have_required) {
    faces <- vapply(
      c("plain", "bold", "italic", "bolditalic"),
      function(nm) if (nzchar(files[[nm]]) && file.exists(files[[nm]])) {
        files[[nm]]
      } else {
        NA_character_
      },
      character(1)
    )
    registered <- cpb_register_family("RijksoverheidSansText", faces,
                                      windows_device = windows_device)
    if (!registered$systemfonts && !registered$sysfonts) {
      warning(
        "ggcpb: font registration failed for both the systemfonts and ",
        "sysfonts backends; falling back to the default ggplot2 font family.",
        call. = FALSE
      )
      .ggcpb_env$font_registered <- FALSE
    } else {
      .ggcpb_env$font_registered <- TRUE
    }
  } else {
    warning(
      "ggcpb: could not find the bundled RijksoverheidSansText font files ",
      "in inst/fonts/; falling back to the default ggplot2 font family. ",
      "See the ggcpb README for the path-based fallback.",
      call. = FALSE
    )
    .ggcpb_env$font_registered <- FALSE
  }

  # `path` adds to the bundled family rather than replacing it, so the
  # house font keeps working whatever else the user points us at
  if (!is.null(path)) {
    found <- cpb_scan_fonts(path, recursive = recursive)
    if (!nrow(found)) {
      warning("ggcpb: no font files found in '", path, "'.", call. = FALSE)
    }
    for (fam in unique(found$family)) {
      rows <- found[found$family == fam, , drop = FALSE]
      faces <- vapply(
        c("plain", "bold", "italic", "bolditalic"),
        function(nm) {
          hit <- rows$file[rows$face == nm]
          if (length(hit)) hit[[1]] else NA_character_
        },
        character(1)
      )
      registered <- rbind(
        registered,
        cpb_register_family(fam, faces, windows_device = windows_device)
      )
    }
  }

  showtext_on <- FALSE
  if (isTRUE(enable_showtext)) {
    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext_on <- tryCatch({
        showtext::showtext_auto()
        TRUE
      }, error = function(e) FALSE)
    } else {
      warning(
        "ggcpb: `enable_showtext = TRUE` but the showtext package is not ",
        "installed; fonts will only render on devices that consult ",
        "systemfonts (ragg, svglite). Install showtext, or see ",
        "?cpb_register_fonts.",
        call. = FALSE
      )
    }
  }

  if (is.null(registered)) {
    registered <- data.frame(
      family = character(), faces = character(), n_files = integer(),
      systemfonts = logical(), sysfonts = logical(), windows = logical(),
      stringsAsFactors = FALSE
    )
  }
  # a `path` pointing at the bundled font re-registers the house family;
  # report each family once, keeping the registration that actually took
  # effect (the later one wins in both backends)
  registered <- registered[!duplicated(registered$family, fromLast = TRUE), ,
                           drop = FALSE]
  rownames(registered) <- NULL
  attr(registered, "showtext") <- showtext_on
  invisible(registered)
}

#' The CPB font family name for use in `theme_cpb()`
#'
#' @return `"RijksoverheidSansText"` if the font was registered
#'   successfully (see [cpb_register_fonts()]), or `""` otherwise -- an
#'   empty string tells ggplot2 to use its built-in default family, so
#'   plots still render correctly even without the CPB font.
#'
#' @section Devices without TTF lookup:
#' The registration goes through \pkg{systemfonts}, which the modern
#' devices (`ragg::agg_png()`, `svglite`, the RStudio device) consult.
#' The base `pdf()` and `postscript()` devices instead look families up
#' in their own Type1 font database and *error at draw time* on a
#' family they do not know. When one of those devices is currently
#' active, `cpb_font_family()` therefore also returns `""`, so plots
#' built with the default `theme_cpb()` still render (in the device's
#' default face) instead of failing. If text on such a device is
#' handled elsewhere -- e.g. `showtext::showtext_auto()` is on, which
#' draws text itself on any device -- set
#' `options(ggcpb.force_font_family = TRUE)` to always get the CPB
#' family name.
#' @examples
#' cpb_font_family()
#' @export
cpb_font_family <- function() {
  if (!isTRUE(.ggcpb_env$font_registered)) return("")
  if (!isTRUE(getOption("ggcpb.force_font_family", FALSE))) {
    dev <- names(grDevices::dev.cur())
    if (dev %in% c("pdf", "postscript")) return("")
  }
  "RijksoverheidSansText"
}
