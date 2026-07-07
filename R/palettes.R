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
#' @param reverse If `TRUE`, reverse the palette order before drawing
#'   colours from it.
#' @return A function `function(n)` returning a character vector of
#'   `n` hex colours. For `"qualitative"` and `"discr"`, colours are
#'   recycled (with a warning) if `n` exceeds the palette length. For
#'   `"sequential"`, `n` colours are interpolated along the ramp.
#' @examples
#' cpb_pal("qualitative")(3)
#' cpb_pal("sequential")(5)
#' cpb_pal("qualitative", reverse = TRUE)(3)
#' @export
cpb_pal <- function(palette = c("qualitative", "discr", "sequential", "blues"), reverse = FALSE) {
  palette <- match.arg(palette)
  cols <- cpb_palette_colours(palette)
  if (isTRUE(reverse)) cols <- rev(cols)

  function(n) {
    if (palette %in% c("sequential", "blues")) {
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
