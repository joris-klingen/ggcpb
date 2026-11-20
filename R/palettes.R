# palettes.R ----
#
# Palette generator functions and the swatch accessor cpb_cols(). These
# operate on the internal token vectors defined in tokens.R via the
# cpb_palette_colours() helper, which excludes the trailing NA colour.

#' A CPB colour palette generator
#'
#' Returns a function of `n` that produces `n` colours from one of the
#' three CPB palettes, for use as the `palette` argument of a ggplot2
#' discrete scale. The trailing NA colour (`"lightgrey"`) is always
#' excluded from the cycled colours; use `na.value` on the scale
#' functions to set it instead.
#'
#' @param palette One of `"qualitative"` (the primary 9-colour discrete
#'   palette), `"discr"` (the alternate blue/pink-led 7-colour discrete
#'   palette), `"sequential"` (the 6-colour light-to-dark pink ramp), or
#'   `"blues"` (the 6-colour light-to-dark blue ramp for classed fills),
#'   or `"extended"` (20 categorical colours: the nine of
#'   `"qualitative"` first, in the same positions as in [cpb_cols()],
#'   then eleven more). Use `"extended"` for testing only, not
#'   for publication: it gives a warning.
#'   Ramps are interpolated to any number of levels.
#' @details For `"sequential"`, up to six levels take the darkest
#'   swatches of the ramp: `n` levels get the `n` darkest, still ordered
#'   light to dark, so the palest pink is only used once all six are
#'   needed. More than six levels are interpolated along the full ramp.
#' @param reverse If `TRUE`, reverse the palette order before drawing
#'   colours from it.
#' @return A function `function(n)` returning a character vector of
#'   `n` hex colours. For `"qualitative"` and `"discr"`, colours are
#'   recycled (with a warning) if `n` exceeds the palette length. For
#'   `"sequential"`, the `n` darkest swatches (see Details), interpolated
#'   along the full ramp beyond six. For `"blues"`, `n` colours are
#'   interpolated along the ramp.
#' @examples
#' cpb_pal("qualitative")(3)
#' cpb_pal("sequential")(5)
#' cpb_pal("qualitative", reverse = TRUE)(3)
#' @export
cpb_pal <- function(palette = c("qualitative", "discr", "sequential",
                                "blues", "extended"),
                    reverse = FALSE) {
  palette <- match.arg(palette)
  cpb_warn_extended(palette)
  cols <- cpb_palette_colours(palette)
  # the qualitative swatches are stored in their own (CPB source-script)
  # order; series cycle through them in the order the published figures
  # use, which leads with the primary blue rather than the pale pink.
  # cpb_cols() below deliberately keeps the raw order, because it and
  # the `index =` argument address swatches by position.
  if (identical(palette, "qualitative")) {
    cols <- cols[cpb_series_order[cpb_series_order <= length(cols)]]
  }
  # the extended palette cycles through its first nine the same way
  if (identical(palette, "extended")) {
    rest <- seq_along(cols)[-seq_along(cpb_series_order)]
    cols <- cols[c(cpb_series_order, rest)]
  }
  if (identical(palette, "sequential")) {
    # the pink ramp's lightest swatches are too pale to stand on their
    # own in a presentation, so a small number of levels is taken from
    # the dark end: n levels get the n darkest swatches (kept in
    # light-to-dark order), and the lightest one only appears once all
    # six are needed. Beyond six the full ramp is interpolated.
    return(function(n) {
      out <- if (n <= length(cols)) {
        cols[seq.int(length(cols) - n + 1L, length.out = n)]
      } else {
        grDevices::colorRampPalette(cols)(n)
      }
      out <- unname(out)
      if (isTRUE(reverse)) rev(out) else out
    })
  }
  if (isTRUE(reverse)) cols <- rev(cols)

  function(n) {
    if (identical(palette, "blues")) {
      grDevices::colorRampPalette(cols)(n)
    } else {
      if (n > length(cols)) {
        warning(
          "cpb_pal(): the '", palette, "' palette only has ", length(cols),
          " colours; recycling to fill ", n, " requested levels.",
          if (!identical(palette, "extended")) {
            ' Use palette = "extended" (for testing only) for up to 20 colours.'
          },
          call. = FALSE
        )
        cols <- rep_len(cols, n)
      }
      unname(cols[seq_len(n)])
    }
  }
}

# The extended palette is for testing, not for publication. The warning is
# given where a user picks the palette, not where the package reads it.
cpb_warn_extended <- function(palette) {
  if (identical(palette, "extended")) {
    warning("palette = \"extended\" is only for testing, not for ",
            "publication. Use the \"qualitative\" palette for a published ",
            "figure.", call. = FALSE)
  }
  invisible(palette)
}

#' Pull specific CPB palette swatches by position
#'
#' A convenience accessor for pulling one or more hex colours out of a
#' CPB palette by integer position, e.g. for a one-off
#' `scale_fill_manual(values = ...)` call. Equivalent to indexing the
#' underlying palette vector directly (as in `cpb_colors[c(6, 2)]` in
#' CPB source scripts), but without needing to know the internal
#' object names.
#'
#' @param ... One or more integer positions into `palette`, or colour
#'   names such as `"siren"` (for `"qualitative"` and `"extended"`). If
#'   empty, the full palette is returned in its original order.
#' @inheritParams cpb_pal
#' @param reverse If `TRUE`, reverse the palette before indexing.
#' @return A character vector of hex colours, named by the position
#'   (or colour name) they were drawn from.
#' @examples
#' cpb_cols(6, 2)
#' cpb_cols(1:3, palette = "discr")
#' cpb_cols("apple", "sun", palette = "extended")
#' @export
cpb_cols <- function(...,
                     palette = c("qualitative", "discr", "sequential",
                                 "extended"),
                     reverse = FALSE) {
  palette <- match.arg(palette)
  cpb_warn_extended(palette)
  cols <- cpb_palette_colours(palette)
  if (isTRUE(reverse)) cols <- rev(cols)

  idx <- c(...)
  # the full named palette keeps its colour names
  if (length(idx) == 0 && !is.null(names(cols))) return(cols)
  if (length(idx) == 0) idx <- seq_along(cols)
  # a colour name finds its colour, also in the qualitative palette, which
  # has no names in its own vector
  named <- cols
  if (is.null(names(named)) && !is.null(cpb_palette_names(palette))) {
    pn <- cpb_palette_names(palette)
    names(named) <- if (isTRUE(reverse)) rev(pn) else pn
  }
  if (is.character(idx) && !is.null(names(named))) {
    unknown <- setdiff(idx, names(named))
    if (length(unknown)) {
      stop("cpb_cols(): unknown colour name(s) ",
           paste0("'", unknown, "'", collapse = ", "),
           "; the '", palette, "' palette has ",
           paste(names(named), collapse = ", "), ".",
           call. = FALSE)
    }
    return(named[idx])
  }
  if (!is.numeric(idx)) {
    stop("cpb_cols(): indices passed via ... must be numeric positions ",
         "into the palette",
         if (!is.null(names(cols))) " or colour names" else "", ".",
         call. = FALSE)
  }
  if (any(idx < 1 | idx > length(cols))) {
    stop(
      "cpb_cols(): index out of range; the '", palette, "' palette has ",
      length(cols), " colours.",
      call. = FALSE
    )
  }

  out <- cols[idx]
  names(out) <- idx
  out
}

# Colour/fill index resolution ----

# The colour names of a palette, in the order of its positions. The nine
# colours of the qualitative palette have the first nine names of the
# extended palette. The ramps have no names (NULL).
cpb_palette_names <- function(palette) {
  nms <- names(cpb_colors_extended)
  nms <- nms[nms != "na"]
  switch(palette,
         qualitative = nms[1:9],
         extended = nms,
         NULL)
}

# The keyword vocabulary for `colour_index`. "discrete"
# and "continuous" are the two the house style is described in; the raw
# palette names are accepted as synonyms so there are not two competing
# vocabularies for the same thing.
cpb_index_keywords <- c(
  discrete    = "qualitative",
  continuous  = "sequential",
  qualitative = "qualitative",
  sequential  = "sequential",
  discr       = "discr",
  blues       = "blues",
  extended    = "extended"
)

#' Resolve a `colour_index` argument
#'
#' The argument does double duty: a numeric vector picks swatches out of
#' the palette by position, while a keyword picks the palette itself.
#' `NULL` defers to `palette`, so existing calls keep their behaviour.
#'
#' @param value The `colour_index`/`colour_index` value.
#' @param palette The wrapper's `palette` argument.
#' @param palette_supplied Was `palette` given explicitly by the caller?
#' @param arg Argument name, for error messages.
#' @return A list with `index` (integer vector or `NULL`) and `palette`.
#' @noRd
cpb_resolve_index <- function(value, palette, palette_supplied,
                              arg = "colour_index") {
  if (is.null(value)) return(list(index = NULL, palette = palette))

  if (is.character(value)) {
    if (!length(value) || anyNA(value)) {
      stop("`", arg, "` must be a keyword, colour names or a vector of ",
           "palette positions.", call. = FALSE)
    }
    is_keyword <- length(value) == 1L && value %in% names(cpb_index_keywords)
    if (!is_keyword) {
      # colour names, such as c("siren", "cold_turkey"), give palette positions
      known <- cpb_palette_names(palette)
      if (is.null(known)) {
        stop("`", arg, "` \"", paste(value, collapse = "\", \""),
             "\" is not recognised: the palette '", palette,
             "' has no colour names. Use palette positions, or ",
             "palette = \"qualitative\" for colour names.", call. = FALSE)
      }
      pos <- match(value, known)
      if (anyNA(pos)) {
        stop("`", arg, "` \"", paste(value[is.na(pos)], collapse = "\", \""),
             "\" is not recognised. Use a keyword (",
             paste0("\"", names(cpb_index_keywords), "\"", collapse = ", "),
             "), colour names of the palette (",
             paste(known, collapse = ", "),
             "), or a vector of palette positions.",
             if (identical(palette, "qualitative") &&
                   all(value[is.na(pos)] %in% cpb_palette_names("extended"))) {
               paste0(" The other colours need palette = \"extended\", ",
                      "which is for testing only.")
             },
             call. = FALSE)
      }
      return(list(index = pos, palette = palette))
    }
    resolved <- unname(cpb_index_keywords[[value]])
    # a keyword *is* a palette choice, so it cannot also be given as one
    if (isTRUE(palette_supplied) && !identical(resolved, palette)) {
      stop("`", arg, " = \"", value, "\"` and `palette = \"", palette,
           "\"` both set the palette, and they disagree. Pass one of them.",
           call. = FALSE)
    }
    return(list(index = NULL, palette = resolved))
  }

  if (is.numeric(value)) {
    if (!length(value) || anyNA(value) || any(value < 1) ||
          any(value != round(value))) {
      stop("`", arg, "` must be positive whole numbers giving palette ",
           "positions, e.g. c(2, 5, 6).", call. = FALSE)
    }
    return(list(index = as.integer(value), palette = palette))
  }

  stop("`", arg, "` must be a keyword or a vector of palette positions.",
       call. = FALSE)
}
