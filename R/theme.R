# theme.R ----
#
# The CPB house-style ggplot2 theme. Built on ggplot2::theme_minimal(),
# reproducing the "energiecrisis_2026" theme overrides, but parameterised
# rather than hardcoded. Font sizes are strict, fixed point sizes (9/8/7)
# -- they are never scaled by a base_size argument or rel().

#' The CPB house-style ggplot2 theme
#'
#' Applies the CPB house style on top of [ggplot2::theme_minimal()]:
#' fixed 9/8/7 pt text sizes, a left-aligned bold title, italic
#' right-aligned axis titles, CPB-coloured gridlines on the value axis
#' only (by default), and an optional CPB-blue plot background.
#'
#' Text sizes are absolute and are never scaled -- there is no
#' `base_size` argument. `plot.title` is 9 pt bold; `axis.title`,
#' `plot.subtitle`, `legend.text` and `strip.text` are 8 pt;
#' `axis.text` is 7 pt.
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
#' @param legend Passed through to `legend.position`; accepts the
#'   usual `"right"`/`"left"`/`"top"`/`"bottom"`/`"none"` strings, or a
#'   two-element numeric vector of plot-relative coordinates.
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
                       legend = "right") {
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

  gridline  <- ggplot2::element_line(colour = cpb_grid)
  blankline <- ggplot2::element_blank()

  plot_bg <- if (isTRUE(background)) {
    ggplot2::element_rect(fill = cpb_bg, colour = NA)
  } else {
    ggplot2::element_blank()
  }

  # plot.subtitle.position and legend.key.spacing.y were only added in
  # ggplot2 3.5.0; guard them so theme_cpb() degrades gracefully (silently
  # dropping those two settings) on an older ggplot2 instead of erroring
  # at render time.
  theme_args <- list(
    plot.title.position = "plot",

    plot.title    = ggplot2::element_text(face = "bold", hjust = 0, size = 9),
    plot.subtitle = ggplot2::element_text(face = "italic", hjust = 0, size = 8),

    axis.title = ggplot2::element_text(face = "italic", hjust = 1, size = 8),
    axis.text  = ggplot2::element_text(colour = "black", size = 7),

    legend.position   = legend,
    legend.text       = ggplot2::element_text(face = "italic", size = 8),
    legend.key.height = grid::unit(0.25, "cm"),
    legend.key.width  = grid::unit(0.30, "cm"),

    strip.text       = ggplot2::element_text(face = "bold", hjust = 0, size = 8),
    strip.background = ggplot2::element_blank(),

    panel.grid.major.x = if (show_grid_x) gridline else blankline,
    panel.grid.minor.x = if (show_grid_x) gridline else blankline,
    panel.grid.major.y = if (show_grid_y) gridline else blankline,
    panel.grid.minor.y = if (show_grid_y) gridline else blankline,

    plot.margin = ggplot2::margin(10, 10, 25, 10),

    plot.background = plot_bg
  )

  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    theme_args$plot.subtitle.position <- "plot"
    theme_args$legend.key.spacing.y <- grid::unit(0.05, "cm")
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
