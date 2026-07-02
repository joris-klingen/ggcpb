# scales.R ----
#
# Discrete and continuous ggplot2 scales built on the CPB palettes, plus
# manual/ordinal convenience scales for picking specific palette
# positions. All discrete and manual scales default na.value = cpb_na.

# discrete scales ----

#' Call ggplot2::discrete_scale() across the scale_name deprecation
#'
#' ggplot2 3.4.x requires a `scale_name` argument to
#' [ggplot2::discrete_scale()]; ggplot2 >= 3.5.0 deprecated and removed
#' it. This internal helper detects which signature is in use so the
#' CPB discrete scales work unchanged on either version.
#'
#' @noRd
cpb_discrete_scale <- function(aesthetics, palette, na.value, ...) {
  args <- list(aesthetics = aesthetics, palette = palette, na.value = na.value, ...)
  if ("scale_name" %in% names(formals(ggplot2::discrete_scale))) {
    args$scale_name <- "cpb"
  }
  do.call(ggplot2::discrete_scale, args)
}

#' CPB discrete fill/colour scales
#'
#' Discrete ggplot2 scales drawing from a CPB palette via [cpb_pal()].
#'
#' @param palette One of `"qualitative"` (default), `"discr"`, or
#'   `"sequential"`.
#' @param reverse If `TRUE`, reverse the palette order.
#' @param na.value Colour used for `NA` values; defaults to the CPB NA
#'   colour (`"lightgrey"`).
#' @param ... Passed through to [ggplot2::discrete_scale()].
#' @return A ggplot2 `Scale` object.
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_cpb_d()
#' @export
scale_fill_cpb_d <- function(palette = c("qualitative", "discr", "sequential"),
                              reverse = FALSE,
                              na.value = cpb_na,
                              ...) {
  palette <- match.arg(palette)
  cpb_discrete_scale(
    aesthetics = "fill",
    palette    = cpb_pal(palette, reverse = reverse),
    na.value   = na.value,
    ...
  )
}

#' @rdname scale_fill_cpb_d
#' @export
scale_colour_cpb_d <- function(palette = c("qualitative", "discr", "sequential"),
                                reverse = FALSE,
                                na.value = cpb_na,
                                ...) {
  palette <- match.arg(palette)
  cpb_discrete_scale(
    aesthetics = "colour",
    palette    = cpb_pal(palette, reverse = reverse),
    na.value   = na.value,
    ...
  )
}

#' @rdname scale_fill_cpb_d
#' @export
scale_color_cpb_d <- scale_colour_cpb_d

# continuous scales ----

#' CPB continuous fill/colour scales
#'
#' Continuous ggplot2 scales built as a full gradient across the CPB
#' sequential palette (`cpb_tokens()$colors_scale`), from its lightest
#' to its darkest entry.
#'
#' @param reverse If `TRUE`, reverse the ramp (dark -> light).
#' @param na.value Colour used for `NA` values; defaults to the CPB NA
#'   colour (`"lightgrey"`).
#' @param ... Passed through to [ggplot2::scale_fill_gradientn()] /
#'   [ggplot2::scale_colour_gradientn()].
#' @return A ggplot2 `Scale` object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = hp)) +
#'   geom_point() +
#'   scale_colour_cpb_c()
#' @export
scale_fill_cpb_c <- function(reverse = FALSE, na.value = cpb_na, ...) {
  cols <- cpb_palette_colours("sequential")
  if (isTRUE(reverse)) cols <- rev(cols)
  ggplot2::scale_fill_gradientn(colours = cols, na.value = na.value, ...)
}

#' @rdname scale_fill_cpb_c
#' @export
scale_colour_cpb_c <- function(reverse = FALSE, na.value = cpb_na, ...) {
  cols <- cpb_palette_colours("sequential")
  if (isTRUE(reverse)) cols <- rev(cols)
  ggplot2::scale_colour_gradientn(colours = cols, na.value = na.value, ...)
}

#' @rdname scale_fill_cpb_c
#' @export
scale_color_cpb_c <- scale_colour_cpb_c

# manual / ordinal convenience scales ----

#' CPB manual fill/colour scales, selecting specific palette positions
#'
#' A `scale_*_manual()` built from specific, ordered positions in a CPB
#' palette, for the common case where a plot needs a small, deliberately
#' ordered subset of the palette (e.g. `cpb_colors[c(6, 2)]` in CPB
#' source scripts, or `cpb_colors_scale[5:1]` for a reversed subset of
#' the sequential ramp).
#'
#' @param index Integer vector of positions into `palette`, in the
#'   order the resulting scale should use them. If `NULL` (default),
#'   the full palette is used in its original order.
#' @param palette One of `"qualitative"` (default), `"discr"`, or
#'   `"sequential"`.
#' @param na.value Colour used for `NA` values; defaults to the CPB NA
#'   colour (`"lightgrey"`).
#' @param ... Passed through to [ggplot2::scale_fill_manual()] /
#'   [ggplot2::scale_colour_manual()].
#' @return A ggplot2 `Scale` object.
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(class, fill = drv)) +
#'   geom_bar() +
#'   scale_fill_cpb_manual(index = c(6, 2, 5))
#' @export
scale_fill_cpb_manual <- function(index = NULL,
                                   palette = c("qualitative", "discr", "sequential"),
                                   na.value = cpb_na,
                                   ...) {
  palette <- match.arg(palette)
  cols <- cpb_palette_colours(palette)
  if (!is.null(index)) cols <- cols[index]
  ggplot2::scale_fill_manual(values = unname(cols), na.value = na.value, ...)
}

#' @rdname scale_fill_cpb_manual
#' @export
scale_colour_cpb_manual <- function(index = NULL,
                                     palette = c("qualitative", "discr", "sequential"),
                                     na.value = cpb_na,
                                     ...) {
  palette <- match.arg(palette)
  cols <- cpb_palette_colours(palette)
  if (!is.null(index)) cols <- cols[index]
  ggplot2::scale_colour_manual(values = unname(cols), na.value = na.value, ...)
}

#' @rdname scale_fill_cpb_manual
#' @export
scale_color_cpb_manual <- scale_colour_cpb_manual
