# fonts.R ----
#
# Font registration. The house font, RijksoverheidSansText, may not be
# published, so it is looked up where CPB keeps it.
# Without it, Source Sans 3 (SIL OFL 1.1) stands in. Loading
# ggcpb must never fail because of fonts: registration is wrapped in
# tryCatch() and falls back to the ggplot2 default family ("") with a
# single warning if a backend or a font file is missing.

.ggcpb_env <- new.env(parent = emptyenv())
.ggcpb_env$font_family <- ""

cpb_house_family    <- "RijksoverheidSansText"
cpb_fallback_family <- "Source Sans 3"

# theme_cpb() uses bold (titles, strips) and italic (subtitle, axis and
# legend titles), never both at once
cpb_required_faces <- c("plain", "bold", "italic")

#' Folders to search for the Rijksoverheid font
#'
#' The M share on Windows and Linux, and the user font
#' folder on macOS. `options(ggcpb.font_path =)` replaces the list.
#' @noRd
cpb_font_paths <- function() {
  getOption("ggcpb.font_path", c(
    "M:/p_james/fonts",
    "/Volumes/data/M/p_james/fonts",
    "/nas/mdk/M/p_james/fonts",
    "/cifs/p_james/fonts",
    "~/Library/Fonts"
  ))
}

#' The font files to register
#'
#' @param paths Folders to search, in order.
#' @return A list with `family` and `faces`, a named character vector
#'   (`plain`, `bold`, `italic`, `bolditalic`; `NA` where missing). The
#'   Rijksoverheid font from the first folder that has it, else the
#'   bundled Source Sans 3.
#' @noRd
cpb_font_files <- function(paths = cpb_font_paths()) {
  for (p in path.expand(paths)) {
    if (!dir.exists(p)) next
    files <- list.files(p, pattern = "^Rijksoverheid.*\\.(ttf|otf)$",
                        ignore.case = TRUE, full.names = TRUE)
    found <- cpb_read_fonts(files)
    found <- found[found$family == cpb_house_family, , drop = FALSE]
    faces <- cpb_family_faces(found)
    if (!anyNA(faces[cpb_required_faces])) {
      return(list(family = cpb_house_family, faces = faces))
    }
  }
  bundled <- function(file) {
    f <- system.file("fonts", file, package = "ggcpb")
    if (nzchar(f)) f else NA_character_
  }
  list(
    family = cpb_fallback_family,
    faces = c(
      plain      = bundled("SourceSans3-Regular.ttf"),
      bold       = bundled("SourceSans3-Bold.ttf"),
      italic     = bundled("SourceSans3-It.ttf"),
      bolditalic = NA_character_
    )
  )
}

#' One family's files as `font_add()` slots
#'
#' @param found A data frame from `cpb_read_fonts()`, one family only.
#' @return A named character vector (`plain`, `bold`, `italic`,
#'   `bolditalic`), `NA` where a face is missing.
#' @noRd
cpb_family_faces <- function(found) {
  vapply(
    c("plain", "bold", "italic", "bolditalic"),
    function(nm) {
      hit <- found$file[found$face == nm]
      if (length(hit)) hit[[1]] else NA_character_
    },
    character(1)
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
  cpb_read_fonts(files)
}

#' Read family and face from font files
#'
#' @param files Font file paths.
#' @return A data frame with one row per file (`file`, `family`, `face`).
#' @noRd
cpb_read_fonts <- function(files) {
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

#' Register the Rijkshuisstijl font family
#'
#' Registers Rijksoverheid Sans Text, with
#' \pkg{systemfonts} (so `ragg`/`svglite`/`ggplot2` output picks it up)
#' and \pkg{sysfonts} (the registry \pkg{showtext} draws from), and
#' optionally turns \pkg{showtext} on so the font also renders on
#' devices that do their own font lookup. Run automatically when ggcpb
#' is loaded, and safe to re-run on demand -- e.g. in a fresh parallel
#' worker that does not inherit the main session's font registry.
#'
#' The default font is the Rijksoverheid font. The package will search 
#' in certain directories for this font. If the package can not find the font,
#' Source Sans 3 serves as an open source default font. Its text runs about
#' 10-15% wider, so a title or legend that only just fits may not, and
#' such figures do not meet the Rijkshuisstijl: [save_cpb()] warns
#' about that.
#'
#' If no font files can be found, a single warning is issued and
#' [cpb_font_family()] subsequently returns `""`, letting [theme_cpb()]
#' fall back to the ggplot2 default font.
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
#'   `.otf`, `.ttc`) to register in addition to the Rijkshuisstijl. Each
#'   file's family and face are read from its own metadata where
#'   possible, falling back to the file name, and the regular/bold/
#'   italic/bold-italic variants of one family are mapped onto the
#'   matching [sysfonts::font_add()] slots. `NULL` (default) registers
#'   only the Rijkshuisstijl font.
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
  font <- cpb_font_files()
  registered <- NULL
  .ggcpb_env$font_family <- ""

  if (!anyNA(font$faces[cpb_required_faces])) {
    registered <- cpb_register_family(font$family, font$faces,
                                      windows_device = windows_device)
    if (!registered$systemfonts && !registered$sysfonts) {
      warning(
        "ggcpb: font registration failed for both the systemfonts and ",
        "sysfonts backends; falling back to the default ggplot2 font family.",
        call. = FALSE
      )
    } else {
      .ggcpb_env$font_family <- font$family
    }
  } else {
    warning(
      "ggcpb: could not find the Rijksoverheid font or ",
      "Source Sans 3 files in inst/fonts/; falling back to the default ",
      "ggplot2 font family.",
      call. = FALSE
    )
  }

  # `path` adds to the family rather than replacing it, so the
  # Rijkshuisstijl font keeps working whatever else the user points us at
  if (!is.null(path)) {
    found <- cpb_scan_fonts(path, recursive = recursive)
    if (!nrow(found)) {
      warning("ggcpb: no font files found in '", path, "'.", call. = FALSE)
    }
    for (fam in unique(found$family)) {
      faces <- cpb_family_faces(found[found$family == fam, , drop = FALSE])
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
  # a `path` pointing at the font's folder re-registers that family;
  # report each family once, keeping the registration that actually took
  # effect (the later one wins in both backends)
  registered <- registered[!duplicated(registered$family, fromLast = TRUE), ,
                           drop = FALSE]
  rownames(registered) <- NULL
  attr(registered, "showtext") <- showtext_on
  invisible(registered)
}

#' The font name for use in `theme_cpb()`
#'
#' @return The registered family (see [cpb_register_fonts()]):
#'   `"RijksoverheidSansText"`, or `"Source Sans 3"` where that is not
#'   available. `""` if neither can be found. An empty string tells
#'   ggplot2 to use its built-in default family, so plots still render
#'   correctly even without the Rijkshuisstijl font.
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
  family <- .ggcpb_env$font_family
  if (!nzchar(family)) return("")
  if (!isTRUE(getOption("ggcpb.force_font_family", FALSE))) {
    dev <- names(grDevices::dev.cur())
    if (dev %in% c("pdf", "postscript")) return("")
  }
  family
}

#' Warn when a plot does not use the Rijksoverheid font
#'
#' Called by save_cpb() and print.cpb_plot(). Off with
#' `options(ggcpb.font_warning = FALSE)`.
#' @param plot A ggplot.
#' @param once Warn once per session: print() runs on every redraw.
#' @return Invisibly `TRUE` if the plot uses the font.
#' @noRd
cpb_check_house_font <- function(plot, once = FALSE) {
  if (!isTRUE(getOption("ggcpb.font_warning", TRUE))) return(invisible(TRUE))
  family <- tryCatch(
    ggplot2::calc_element("text", ggplot2::theme_get() + plot$theme)$family,
    error = function(e) NULL
  )
  registered <- identical(.ggcpb_env$font_family, cpb_house_family)
  if (identical(family, cpb_house_family) && registered) {
    return(invisible(TRUE))
  }
  rlang::warn(
    paste0(
      "ggcpb: this plot does not meet the Rijkshuisstijl requirements, ",
      "because it does not use the Rijkshuisstijl font (", cpb_house_family, ").",
      if (!registered) " The font was not found; see ?cpb_register_fonts."
    ),
    .frequency = if (once) "once" else "always",
    .frequency_id = "cpb_house_font"
  )
  invisible(FALSE)
}
