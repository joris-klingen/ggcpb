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
#'   `"blues"` (the 6-colour light-to-dark blue ramp for classed fills).
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
cpb_pal <- function(palette = c("qualitative", "discr", "sequential", "blues"), reverse = FALSE) {
  palette <- match.arg(palette)
  cols <- cpb_palette_colours(palette)
  # the qualitative swatches are stored in their own (CPB source-script)
  # order; series cycle through them in the order the published figures
  # use, which leads with the primary blue rather than the pale pink.
  # cpb_cols() below deliberately keeps the raw order, because it and
  # the `index =` argument address swatches by position.
  if (identical(palette, "qualitative")) {
    cols <- cols[cpb_series_order[cpb_series_order <= length(cols)]]
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
          call. = FALSE
        )
        cols <- rep_len(cols, n)
      }
      unname(cols[seq_len(n)])
    }
  }
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
#' @param ... One or more integer positions into `palette`. If empty,
#'   the full palette is returned in its original order.
#' @param palette One of `"qualitative"` (default), `"discr"`,
#'   `"sequential"` (pink ramp), or `"blues"` (blue ramp).
#' @param reverse If `TRUE`, reverse the palette before indexing.
#' @return A character vector of hex colours, named by the position
#'   they were drawn from.
#' @examples
#' cpb_cols(6, 2)
#' cpb_cols(1:3, palette = "discr")
#' @export
cpb_cols <- function(..., palette = c("qualitative", "discr", "sequential"), reverse = FALSE) {
  palette <- match.arg(palette)
  cols <- cpb_palette_colours(palette)
  if (isTRUE(reverse)) cols <- rev(cols)

  idx <- c(...)
  if (length(idx) == 0) idx <- seq_along(cols)
  if (!is.numeric(idx)) {
    stop("cpb_cols(): indices passed via ... must be numeric positions into the palette.", call. = FALSE)
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

# The keyword vocabulary for `colour_index` / `fill_index`. "discrete"
# and "continuous" are the two the house style is described in; the raw
# palette names are accepted as synonyms so there are not two competing
# vocabularies for the same thing.
cpb_index_keywords <- c(
  discrete    = "qualitative",
  continuous  = "sequential",
  qualitative = "qualitative",
  sequential  = "sequential",
  discr       = "discr",
  blues       = "blues"
)

#' Resolve a `colour_index` / `fill_index` argument
#'
#' The argument does double duty: a numeric vector picks swatches out of
#' the palette by position, while a keyword picks the palette itself.
#' `NULL` defers to `palette`, so existing calls keep their behaviour.
#'
#' @param value The `colour_index`/`fill_index` value.
#' @param legacy The deprecated `index` value, if any.
#' @param palette The wrapper's `palette` argument.
#' @param palette_supplied Was `palette` given explicitly by the caller?
#' @param arg Argument name, for error messages.
#' @return A list with `index` (integer vector or `NULL`) and `palette`.
#' @noRd
cpb_resolve_index <- function(value, legacy, palette, palette_supplied,
                              arg = "colour_index") {
  if (!is.null(legacy)) {
    if (!is.null(value)) {
      stop("`index` and `", arg, "` cannot both be set. `index` is ",
           "deprecated -- use `", arg, "` on its own.", call. = FALSE)
    }
    warning("`index` is deprecated; use `", arg, "` instead.", call. = FALSE)
    value <- legacy
  }
  if (is.null(value)) return(list(index = NULL, palette = palette))

  if (is.character(value)) {
    if (length(value) != 1L || is.na(value)) {
      stop("`", arg, "` must be a single keyword (", 
           paste0("\"", names(cpb_index_keywords)[1:2], "\"", collapse = " or "),
           ") or a vector of palette positions.", call. = FALSE)
    }
    if (!value %in% names(cpb_index_keywords)) {
      stop("`", arg, "` keyword \"", value, "\" is not recognised. Use one of ",
           paste0("\"", names(cpb_index_keywords), "\"", collapse = ", "),
           ", or a vector of palette positions.", call. = FALSE)
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
