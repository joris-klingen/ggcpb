# theme.R ----
#
# The CPB house-style ggplot2 theme. Built on ggplot2::theme_minimal(),
# parameterised over two style presets: "cpb_default" (the published
# nplot() look) and "ggplot" (the hand-rolled "energiecrisis_2026"
# script look). Font sizes are strict, fixed point sizes -- they are
# never scaled by a base_size argument or rel().

#' The CPB house-style ggplot2 theme
#'
#' Applies the CPB house style on top of [ggplot2::theme_minimal()]:
#' a left-aligned bold title, italic right-aligned axis titles,
#' gridlines on the value axis only (by default), the CPB-blue plot
#' background, and the `style` preset's gridline/tick/legend
#' treatment.
#'
#' Text sizes are absolute and are never scaled -- there is no
#' `base_size` argument. `plot.title` is 9 pt bold; `axis.title`,
#' `plot.subtitle`, `legend.title`, `legend.text` and `strip.text`
#' are 7 pt;
#' `axis.text` is 7 pt (`"cpb_default"`) or 6 pt (`"ggplot"`).
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
#'   gridlines the default `grid = "value"` shows.
#' @param grid Which gridlines to draw: `"value"` (default) draws
#'   gridlines only on the value axis implied by `orientation`;
#'   `"both"` draws both x and y gridlines; `"none"` draws none;
#'   `"x"`/`"y"` draw gridlines on that axis explicitly, regardless of
#'   `orientation`. Gridlines are drawn in `cpb_tokens()$grid`, and
#'   minor gridlines always match the major gridlines.
#' @param style Style preset that sets the defaults of the knobs below.
#'   `"cpb_default"` (the default) is the CPB house look of published
#'   figures (historically produced with the internal base-R `nplot()`
#'   plotter): hairline black gridlines at labelled breaks only, black
#'   tick marks (with an axis line) on the category axis, 7 pt axis
#'   text, a flush-left vertical legend at the bottom, and tight
#'   bottom margins. `"ggplot"` is the lighter hand-rolled CPB ggplot2
#'   look: CPB-grey gridlines with minors, no axis ticks, 6 pt axis
#'   text, legend on the right. Any knob set explicitly overrides the
#'   preset.
#' @param legend Passed through to `legend.position`; accepts the
#'   usual `"right"`/`"left"`/`"top"`/`"bottom"`/`"none"` strings, or a
#'   two-element numeric vector of plot-relative coordinates. `NULL`
#'   (default) resolves by `style`: `"bottom"` for `"cpb_default"`,
#'   `"right"` for `"ggplot"`.
#' @param minor If `TRUE`, minor gridlines are drawn (matching the
#'   major gridlines); if `FALSE`, gridlines appear only at labelled
#'   breaks. `NULL` (default) resolves by `style` (`FALSE` for
#'   `"cpb_default"`, `TRUE` for `"ggplot"`).
#' @param ticks If `TRUE`, draw black axis tick marks on the *category*
#'   axis (the x axis when `orientation = "vertical"`, the y axis when
#'   `"horizontal"`), as `nplot()` does. `NULL` (default) resolves by
#'   `style` (`TRUE` for `"cpb_default"`, `FALSE` for `"ggplot"`).
#' @param flush_legend If `TRUE`, anchor the legend to the left edge of
#'   the full plot area (`legend.location = "plot"` plus a left
#'   justification) and stack its keys vertically -- the `nplot()`
#'   bottom-left legend block. Most useful with `legend = "bottom"`.
#'   `NULL` (default) resolves by `style` (`TRUE` for `"cpb_default"`,
#'   `FALSE` for `"ggplot"`). Requires ggplot2 >= 3.5.0 for the
#'   `legend.location` part; on older versions only the justification
#'   is applied.
#' @param axis_text_size Axis text size in points. `NULL` (default)
#'   resolves by `style`: `7` for `"cpb_default"`, `6` for `"ggplot"`
#'   (the hand-rolled CPB scripts).
#' @param legend_key_size Legend key size in cm. `NULL` (default)
#'   keeps the house 0.25 x 0.30 cm keys (both styles; the published
#'   figures use the same size).
#' @param grid_colour Gridline colour. `NULL` (default) resolves by
#'   `style`: `"black"` for `"cpb_default"`, `cpb_tokens()$grid`
#'   (`"#c9d1da"`) for `"ggplot"`.
#' @param grid_linewidth Gridline linewidth (mm). `NULL` (default)
#'   resolves by `style`: a `0.1` hairline for `"cpb_default"`, the
#'   ggplot2 default for `"ggplot"`.
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
                       style = c("cpb_default", "ggplot"),
                       legend = NULL,
                       minor = NULL,
                       ticks = NULL,
                       flush_legend = NULL,
                       axis_text_size = NULL,
                       legend_key_size = NULL,
                       grid_colour = NULL,
                       grid_linewidth = NULL) {
  orientation <- match.arg(orientation)
  grid <- match.arg(grid)
  style <- match.arg(style)

  # resolve the style-dependent defaults; explicit arguments always win
  nplot <- style == "cpb_default"   # the look of the legacy nplot() plotter
  if (is.null(legend))          legend          <- if (nplot) "bottom" else "right"
  if (is.null(minor))           minor           <- !nplot
  if (is.null(ticks))           ticks           <- nplot
  if (is.null(flush_legend))    flush_legend    <- nplot
  if (is.null(axis_text_size))  axis_text_size  <- if (nplot) 7 else 6
  # nplot keys measure 0.25 x 0.30 cm in the published references (p20),
  # the same as the classic keys, so the preset leaves the size alone
  if (is.null(grid_colour))     grid_colour     <- if (nplot) "black" else cpb_grid
  if (is.null(grid_linewidth) && nplot) grid_linewidth <- 0.1

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

  # legend.key.spacing.y was only added in ggplot2 3.5.0; guard it so
  # theme_cpb() degrades gracefully (silently dropping that setting) on an
  # older ggplot2 instead of erroring at render time. The title/subtitle are
  # both aligned to the plot edge via plot.title.position = "plot" (which
  # governs the subtitle too), so no separate plot.subtitle.position is set --
  # that element does not exist in ggplot2 and setting it only warns.
  theme_args <- list(
    plot.title.position = "plot",

    plot.title    = ggplot2::element_text(face = "bold", hjust = 0, size = 9),
    plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0, size = 7),

    axis.title = ggplot2::element_text(face = "italic", hjust = 1, size = 7),
    axis.text  = ggplot2::element_text(colour = "black", size = axis_text_size),

    legend.position   = legend,
    legend.title      = ggplot2::element_text(face = "italic", size = 7),
    # nplot sets the label close to its key (~3.5 pt vs the ggplot2
    # default of ~5.5 pt)
    legend.text       = ggplot2::element_text(
      face = "italic", size = 7,
      margin = if (nplot) ggplot2::margin(l = 3.5) else NULL
    ),
    legend.key.height = grid::unit(
      if (is.null(legend_key_size)) 0.25 else legend_key_size, "cm"),
    legend.key.width  = grid::unit(
      if (is.null(legend_key_size)) 0.30 else legend_key_size, "cm"),

    strip.text       = ggplot2::element_text(face = "bold", hjust = 0, size = 7),
    strip.background = ggplot2::element_blank(),

    panel.grid.major.x = if (show_grid_x) gridline else blankline,
    panel.grid.minor.x = if (show_grid_x) minorline else blankline,
    panel.grid.major.y = if (show_grid_y) gridline else blankline,
    panel.grid.minor.y = if (show_grid_y) minorline else blankline,

    # the classic 25 pt bottom margin makes room for the snippet-style
    # overlay legends (legend.position = c(x, -0.2)); nplot uses a real
    # bottom legend, so the panel gets that space back
    plot.margin = if (nplot) {
      ggplot2::margin(10, 10, 8, 10)
    } else {
      ggplot2::margin(10, 10, 25, 10)
    },

    plot.background = plot_bg
  )

  if (nplot) {
    theme_args$legend.margin      <- ggplot2::margin(0, 0, 0, 0)
    theme_args$legend.box.spacing <- grid::unit(6, "pt")
  }

  if (isTRUE(ticks)) {
    tickline <- ggplot2::element_line(colour = "black", linewidth = 0.2)
    # nplot (base R) draws the axis line the ticks hang from, so the
    # tick strip never floats when the lowest break is off the panel edge
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
  }

  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    theme_args$legend.key.spacing.y <- grid::unit(0.05, "cm")
    if (isTRUE(flush_legend)) {
      theme_args$legend.location <- "plot"
    }
  }

  ggplot2::theme_minimal(base_family = base_family) +
    do.call(ggplot2::theme, theme_args)
}

#' A minimal CPB theme with no background or gridlines
#'
#' A convenience wrapper around [theme_cpb()] with `background = FALSE`
#' and `grid = "none"`, useful as a starting point when composing a
#' custom theme, or for small multiples where gridlines and a filled
#' background would be too heavy.
#'
#' @inheritParams theme_cpb
#' @return A ggplot2 `theme` object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   theme_cpb_min()
#' @export
theme_cpb_min <- function(base_family = cpb_font_family(),
                           orientation = c("vertical", "horizontal"),
                           legend = "right") {
  orientation <- match.arg(orientation)
  theme_cpb(
    base_family = base_family,
    background  = FALSE,
    orientation = orientation,
    grid        = "none",
    legend      = legend
  )
}
