# theme.R ----
#
# The CPB house-style ggplot2 theme, built on ggplot2::theme_minimal().
# There is one style -- the look of published CPB figures -- with each
# element exposed as an argument for per-figure deviations. Font sizes
# are strict, fixed point sizes; they are never scaled by a base_size
# argument or rel().

#' The CPB house-style ggplot2 theme
#'
#' Applies the CPB house style on top of [ggplot2::theme_minimal()]:
#' a left-aligned bold title, italic right-aligned axis titles,
#' hairline black gridlines at labelled breaks on the value axis only
#' (by default), black tick marks with an axis line on the category
#' axis, a flush-left vertical legend at the bottom, and the CPB-blue
#' plot background.
#'
#' Text sizes are absolute and are never scaled -- there is no
#' `base_size` argument. `plot.title` is 9 pt bold; `axis.title`,
#' `plot.subtitle`, `axis.text`, `legend.title`, `legend.text` and
#' `strip.text` are 7 pt.
#'
#' @param base_family Font family for all text. Defaults to
#'   [cpb_font_family()], which resolves to `"RijksoverheidSansText"`
#'   if the bundled font registered successfully, or `""` (the
#'   ggplot2 default family) otherwise.
#' @param background If `TRUE` (default), `plot.background` is filled
#'   with the CPB background colour (`cpb_tokens()$bg`). If `FALSE`,
#'   the plot background is left blank/transparent.
#' @param orientation Either `"vertical"` (default; the value axis is
#'   y, e.g. a normal column chart) or `"horizontal"` (the value axis
#'   is x, e.g. a chart built with `coord_flip()`). Determines which
#'   gridlines the default `grid = "value"` shows and which axis gets
#'   the category ticks.
#' @param grid Which gridlines to draw: `"value"` (default) draws
#'   gridlines only on the value axis implied by `orientation`;
#'   `"both"` draws both x and y gridlines; `"none"` draws none;
#'   `"x"`/`"y"` draw gridlines on that axis explicitly, regardless of
#'   `orientation`.
#' @param legend Passed through to `legend.position`; accepts the
#'   usual `"bottom"` (default) /`"right"`/`"left"`/`"top"`/`"none"`
#'   strings, or a two-element numeric vector of plot-relative
#'   coordinates.
#' @param minor If `TRUE`, minor gridlines are drawn (matching the
#'   major gridlines). Defaults to `FALSE`: gridlines appear only at
#'   labelled breaks.
#' @param ticks If `TRUE` (default), draw black axis tick marks, and
#'   the axis line they hang from, on the *category* axis (the x axis
#'   when `orientation = "vertical"`, the y axis when
#'   `"horizontal"`).
#' @param flush_legend If `TRUE` (default), anchor the legend to the
#'   left edge of the full plot area (`legend.location = "plot"` plus
#'   a left justification) and stack its keys vertically -- the fixed
#'   bottom-left legend block of CPB figures.
#' @param axis_text_size Axis text size in points. Defaults to `7`.
#' @param legend_key_size Legend key size in cm. `NULL` (default)
#'   keeps the house 0.25 x 0.30 cm keys.
#' @param grid_colour Gridline colour. Defaults to `"black"`.
#' @param grid_linewidth Gridline linewidth (mm). Defaults to the
#'   `0.1` house hairline; `NULL` keeps the ggplot2 default.
#'
#' @return A ggplot2 `theme` object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   theme_cpb()
#'
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   coord_flip() +
#'   theme_cpb(orientation = "horizontal")
#' @export
theme_cpb <- function(base_family = cpb_font_family(),
                       background = TRUE,
                       orientation = c("vertical", "horizontal"),
                       grid = c("value", "both", "none", "x", "y"),
                       legend = "bottom",
                       minor = FALSE,
                       ticks = TRUE,
                       flush_legend = TRUE,
                       axis_text_size = 7,
                       legend_key_size = NULL,
                       grid_colour = "black",
                       grid_linewidth = 0.1) {
  orientation <- match.arg(orientation)
  grid <- match.arg(grid)

  value_axis <- if (orientation == "vertical") "y" else "x"

  show_grid_x <- switch(grid,
    value = value_axis == "x",
    both  = TRUE,
    none  = FALSE,
    x     = TRUE,
    y     = FALSE
  )
  show_grid_y <- switch(grid,
    value = value_axis == "y",
    both  = TRUE,
    none  = FALSE,
    x     = FALSE,
    y     = TRUE
  )

  gridline <- if (is.null(grid_linewidth)) {
    ggplot2::element_line(colour = grid_colour)
  } else {
    ggplot2::element_line(colour = grid_colour, linewidth = grid_linewidth)
  }
  blankline <- ggplot2::element_blank()

  minorline <- if (isTRUE(minor)) gridline else blankline

  plot_bg <- if (isTRUE(background)) {
    ggplot2::element_rect(fill = cpb_bg, colour = NA)
  } else {
    ggplot2::element_blank()
  }

  # The title/subtitle are both aligned to the plot edge via
  # plot.title.position = "plot" (which governs the subtitle too), so no
  # separate plot.subtitle.position is set -- that element does not
  # exist in ggplot2 and setting it only warns.
  theme_args <- list(
    plot.title.position = "plot",

    plot.title    = ggplot2::element_text(face = "bold", hjust = 0, size = 9),
    plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0, size = 7),

    axis.title = ggplot2::element_text(face = "italic", hjust = 1, size = 7),
    axis.text  = ggplot2::element_text(colour = "black", size = axis_text_size),

    legend.position   = legend,
    legend.title      = ggplot2::element_text(face = "italic", size = 7),
    # the label sits close to its key (~3.5 pt vs the ggplot2 default
    # of ~5.5 pt), matching published output
    legend.text       = ggplot2::element_text(
      face = "italic", size = 7, margin = ggplot2::margin(l = 3.5)
    ),
    legend.key.height = grid::unit(
      if (is.null(legend_key_size)) 0.25 else legend_key_size, "cm"),
    legend.key.width  = grid::unit(
      if (is.null(legend_key_size)) 0.30 else legend_key_size, "cm"),
    legend.key.spacing.y = grid::unit(0.05, "cm"),
    legend.margin        = ggplot2::margin(0, 0, 0, 0),
    legend.box.spacing   = grid::unit(6, "pt"),

    strip.text       = ggplot2::element_text(face = "bold", hjust = 0, size = 7),
    strip.background = ggplot2::element_blank(),

    panel.grid.major.x = if (show_grid_x) gridline else blankline,
    panel.grid.minor.x = if (show_grid_x) minorline else blankline,
    panel.grid.major.y = if (show_grid_y) gridline else blankline,
    panel.grid.minor.y = if (show_grid_y) minorline else blankline,

    plot.margin = ggplot2::margin(10, 10, 8, 10),

    plot.background = plot_bg
  )

  if (isTRUE(ticks)) {
    tickline <- ggplot2::element_line(colour = "black", linewidth = 0.2)
    # the axis line the ticks hang from, so the tick strip never floats
    # when the lowest break is off the panel edge
    axisline <- ggplot2::element_line(colour = "black", linewidth = 0.1)
    if (orientation == "vertical") {
      theme_args$axis.ticks.x <- tickline
      theme_args$axis.line.x  <- axisline
    } else {
      theme_args$axis.ticks.y <- tickline
      theme_args$axis.line.y  <- axisline
    }
    theme_args$axis.ticks.length <- grid::unit(2.2, "pt")
  }

  if (isTRUE(flush_legend)) {
    theme_args$legend.justification <- "left"
    theme_args$legend.direction     <- "vertical"
    theme_args$legend.location      <- "plot"
  }

  ggplot2::theme_minimal(base_family = base_family) +
    do.call(ggplot2::theme, theme_args)
}
