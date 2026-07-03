# wrappers.R ----
#
# Thin, high-level wrapper functions: data.frame in, finished-styled
# ggplot object out. Each wrapper applies theme_cpb() and a CPB scale
# and returns a real ggplot object -- it never saves or prints as a
# side effect, so users can keep adding layers with `+`. Columns are
# selected with tidy evaluation, so a plain data.frame or a
# data.table works transparently (both inherit data.frame).

# columns / bars ----

#' One-sided value-axis expansion for the nplot look
#'
#' nplot draws bars and areas sitting directly on the zero axis: the
#' panel edge *is* the axis line, so the zero side of the value scale
#' gets no padding when the data does not cross zero. Returns an
#' [ggplot2::expansion()] spec, or `NULL` (keep the ggplot2 default)
#' for mixed-sign or non-numeric data.
#'
#' @noRd
cpb_zero_flush_expand <- function(values) {
  if (!is.numeric(values) || !length(values) || all(is.na(values))) return(NULL)
  lo <- min(values, na.rm = TRUE)
  hi <- max(values, na.rm = TRUE)
  if (lo >= 0) {
    ggplot2::expansion(mult = c(0, 0.05))
  } else if (hi <= 0) {
    ggplot2::expansion(mult = c(0.05, 0))
  } else {
    NULL
  }
}

#' A CPB-styled column (bar) chart
#'
#' Thin wrapper around [ggplot2::geom_col()] with CPB theming and
#' colour scale applied. Returns a real ggplot object that can be
#' extended further with `+`.
#'
#' @param data A data.frame or data.table with one row per bar segment.
#' @param x,y Columns mapped to the x and y aesthetics (tidy eval).
#' @param fill Optional column mapped to the fill aesthetic (tidy
#'   eval); if omitted, bars are drawn in a single colour (`fill_colour`).
#' @param fill_colour Constant bar fill used when no `fill` column is
#'   mapped. Defaults to `NULL`, which resolves to the CPB primary blue
#'   (`cpb_cols(6)`, `"#005faf"`). Ignored when `fill` is supplied.
#' @param position One of `"stack"` (default), `"dodge"`, or `"fill"`.
#' @param orientation `"vertical"` (default) or `"horizontal"` (adds
#'   [ggplot2::coord_flip()] and is forwarded to [theme_cpb()]).
#' @param value_limits Optional length-2 numeric vector giving the
#'   value-axis range (the `y` axis, or the flipped axis when
#'   `orientation = "horizontal"`). Applied as a coordinate-system zoom
#'   ([ggplot2::coord_cartesian()] / [ggplot2::coord_flip()] `ylim`), so
#'   bars are clipped for display but not dropped. `NULL` (default) lets
#'   ggplot2 pick the range.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()] instead of the default
#'   [scale_fill_cpb_d()] when supplied.
#' @param pct_axis If `TRUE`, format the value axis with
#'   [label_pct_nl()]. Uses `scale = 100` automatically when
#'   `position = "fill"` (proportions), and `scale = 1` otherwise
#'   (values already in percentage points).
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_labels If `TRUE`, add [ggplot2::geom_text()] value
#'   labels using `y`, positioned to match `position`.
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)` -- stacking otherwise
#'   makes the legend order counter-intuitive.
#' @param style Style preset, forwarded to [theme_cpb()]: `"ggplot"`
#'   (default, the hand-rolled CPB ggplot2 look) or `"nplot"` (the
#'   legacy CPB plotter look: hairline black gridlines at labelled
#'   breaks only, category-axis ticks, 7 pt axis text, 0.45 cm legend
#'   keys, flush-left bottom legend, and a black zero line on the
#'   value axis). Any knob set explicitly overrides the preset.
#' @param legend Legend position, forwarded to [theme_cpb()]; accepts
#'   `"right"`, `"bottom"`, `"left"`, `"top"`, `"none"`, or a
#'   two-element numeric vector of plot-relative coordinates. `NULL`
#'   (default) resolves by `style`.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis on top of the bars, as the CPB `nplot()` house style
#'   does. `NULL` (default) resolves by `style`: `TRUE` for `"nplot"`,
#'   `FALSE` for `"ggplot"`.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()]; `NULL` (the default) resolves by
#'   `style`.
#' @param title Plot title.
#' @param ylab Label for the **vertical** axis. Following CPB house
#'   style it is rendered as the plot *subtitle* -- a left-aligned
#'   italic caption at the top -- not as a rotated axis title. In a
#'   horizontal bar chart (`orientation = "horizontal"`) the vertical
#'   axis is the category axis; in a vertical one it is the value axis.
#' @param xlab Label for the **horizontal** axis (bottom, right-aligned
#'   italic). It is attached to the correct ggplot2 aesthetic
#'   automatically: the value (`y`) aesthetic when
#'   `orientation = "horizontal"` (after `coord_flip()`), the category
#'   (`x`) aesthetic otherwise.
#' @param filllab Legend title override; defaults to `NULL` (no legend
#'   title), matching CPB house style.
#' @param ... Further arguments passed to [ggplot2::geom_col()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   year = rep(2021:2023, each = 2),
#'   group = rep(c("huishoudens", "bedrijven"), 3),
#'   value = c(10, 15, 12, 18, 14, 20)
#' )
#' cpb_col(df, x = year, y = value, fill = group)
#' @export
cpb_col <- function(data, x, y, fill = NULL,
                     fill_colour = NULL,
                     position = c("stack", "dodge", "fill"),
                     orientation = c("vertical", "horizontal"),
                     palette = "qualitative",
                     index = NULL,
                     pct_axis = FALSE,
                     value_breaks = NULL,
                     value_limits = NULL,
                     value_labels = FALSE,
                     reverse_legend = TRUE,
                     style = c("ggplot", "nplot"),
                     legend = NULL,
                     zeroline = NULL,
                     minor = NULL,
                     ticks = NULL,
                     flush_legend = NULL,
                     axis_text_size = NULL,
                     legend_key_size = NULL,
                     grid_colour = NULL,
                     grid_linewidth = NULL,
                     title = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     filllab = NULL,
                     ...) {
  position <- match.arg(position)
  orientation <- match.arg(orientation)
  style <- match.arg(style)
  if (is.null(zeroline)) zeroline <- style == "nplot"

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  has_fill <- !rlang::quo_is_null(fill)

  if (has_fill) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, fill = !!fill)
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_col(position = position, ...)
  } else {
    # No fill mapping: draw one flat house-style colour (CPB primary blue
    # by default) rather than ggplot2's grey.
    single_fill <- if (is.null(fill_colour)) unname(cpb_cols(6)) else fill_colour
    mapping <- ggplot2::aes(x = !!x, y = !!y)
    p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_col(position = position, fill = single_fill, ...)
  }

  # The zero line sits on the value axis (the y aesthetic even under
  # coord_flip()) and is drawn on top of the bars, matching nplot().
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  if (orientation == "horizontal") {
    p <- p + if (!is.null(value_limits)) {
      ggplot2::coord_flip(ylim = value_limits)
    } else {
      ggplot2::coord_flip()
    }
  } else if (!is.null(value_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = value_limits)
  }

  # the value-axis scale is assembled once, so labels, breaks and the
  # nplot zero-flush expansion can coexist
  scale_args <- list()
  if (isTRUE(pct_axis)) {
    pct_scale <- if (position == "fill") 100 else 1
    scale_args$labels <- label_pct_nl(scale = pct_scale)
  }
  if (!is.null(value_breaks)) {
    scale_args$breaks <- value_breaks
  }
  if (style == "nplot") {
    expand <- cpb_zero_flush_expand(rlang::eval_tidy(y, data))
    if (!is.null(expand)) scale_args$expand <- expand
  }
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  if (isTRUE(value_labels)) {
    label_position <- switch(position,
      stack = ggplot2::position_stack(vjust = 0.5),
      fill  = ggplot2::position_fill(vjust = 0.5),
      dodge = ggplot2::position_dodge2(width = 0.9)
    )
    p <- p + ggplot2::geom_text(
      mapping  = ggplot2::aes(label = !!y),
      position = label_position,
      size     = 7 / ggplot2::.pt,
      colour   = "black"
    )
  }

  if (has_fill) {
    p <- p + if (!is.null(index)) {
      scale_fill_cpb_manual(index = index, palette = palette)
    } else {
      scale_fill_cpb_d(palette = palette)
    }
    if (isTRUE(reverse_legend)) {
      p <- p + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
    }
  }

  # CPB convention: the vertical-axis label is the plot subtitle (`ylab`), and
  # the horizontal-axis label (`xlab`) is the ordinary axis title. Under
  # coord_flip() the value sits on the y aesthetic but is drawn horizontally,
  # so `xlab` attaches to y when horizontal and to x when vertical.
  if (orientation == "horizontal") {
    lab_x <- NULL
    lab_y <- xlab
  } else {
    lab_x <- xlab
    lab_y <- NULL
  }

  p +
    ggplot2::labs(title = title, subtitle = ylab, x = lab_x, y = lab_y, fill = filllab) +
    theme_cpb(
      orientation     = orientation,
      style           = style,
      legend          = legend,
      minor           = minor,
      ticks           = ticks,
      flush_legend    = flush_legend,
      axis_text_size  = axis_text_size,
      legend_key_size = legend_key_size,
      grid_colour     = grid_colour,
      grid_linewidth  = grid_linewidth
    )
}

# stacked area ----

#' A CPB-styled stacked area chart
#'
#' Thin wrapper around [ggplot2::geom_area()] with CPB theming and
#' colour scale applied, for the recurring "share of total over time"
#' chart.
#'
#' @param data A data.frame or data.table with one row per time x group
#'   combination.
#' @param x,y Columns mapped to the x and y aesthetics (tidy eval);
#'   typically a time variable and a value or share.
#' @param fill Column mapped to the fill aesthetic (tidy eval), i.e.
#'   the grouping variable being stacked.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()] instead of the default
#'   [scale_fill_cpb_d()] when supplied.
#' @param pct_axis If `TRUE`, format the y axis with [label_pct_nl()].
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)`.
#' @param style Style preset, forwarded to [theme_cpb()]; see
#'   [cpb_col()].
#' @param legend Legend position, forwarded to [theme_cpb()]; `NULL`
#'   (default) resolves by `style`.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis on top of the areas, as the CPB `nplot()` house style
#'   does. `NULL` (default) resolves by `style`.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()]; `NULL` (the default) resolves by
#'   `style`.
#' @param title,subtitle Plot title/subtitle.
#' @param xlab,filllab Axis and legend title overrides; default
#'   to `NULL` (no axis title), matching CPB house style.
#' @param ylab Label for the value (y) axis. Following CPB house style
#'   it is rendered as the plot *subtitle* -- a left-aligned italic
#'   caption above the panel -- unless an explicit `subtitle` is also
#'   given, in which case it falls back to a rotated y-axis title.
#' @param ... Further arguments passed to [ggplot2::geom_area()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   year = rep(2020:2023, each = 2),
#'   bron = rep(c("gas", "elektriciteit"), 4),
#'   aandeel = c(60, 40, 55, 45, 50, 50, 48, 52)
#' )
#' cpb_area(df, x = year, y = aandeel, fill = bron, pct_axis = TRUE)
#' @export
cpb_area <- function(data, x, y, fill,
                      palette = "qualitative",
                      index = NULL,
                      pct_axis = FALSE,
                      reverse_legend = TRUE,
                      style = c("ggplot", "nplot"),
                      legend = NULL,
                      zeroline = NULL,
                      minor = NULL,
                      ticks = NULL,
                      flush_legend = NULL,
                      axis_text_size = NULL,
                      legend_key_size = NULL,
                      grid_colour = NULL,
                      grid_linewidth = NULL,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      filllab = NULL,
                      ...) {
  style <- match.arg(style)
  if (is.null(zeroline)) zeroline <- style == "nplot"

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, fill = !!fill)) +
    ggplot2::geom_area(...)

  # on top of the areas, matching nplot()
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  # assembled once, as in cpb_col()
  scale_args <- list()
  if (isTRUE(pct_axis)) {
    scale_args$labels <- label_pct_nl()
  }
  if (style == "nplot") {
    expand <- cpb_zero_flush_expand(rlang::eval_tidy(y, data))
    if (!is.null(expand)) scale_args$expand <- expand
  }
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  p <- p + if (!is.null(index)) {
    scale_fill_cpb_manual(index = index, palette = palette)
  } else {
    scale_fill_cpb_d(palette = palette)
  }

  if (isTRUE(reverse_legend)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }

  # CPB convention: the value-axis label doubles as the subtitle (an
  # italic caption above the panel) rather than a rotated axis title
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    theme_cpb(
      style           = style,
      legend          = legend,
      minor           = minor,
      ticks           = ticks,
      flush_legend    = flush_legend,
      axis_text_size  = axis_text_size,
      legend_key_size = legend_key_size,
      grid_colour     = grid_colour,
      grid_linewidth  = grid_linewidth
    )
}

# lines ----

#' A CPB-styled line chart
#'
#' Thin wrapper around [ggplot2::geom_line()] with CPB theming and
#' colour scale applied.
#'
#' @param data A data.frame or data.table with one row per x x group
#'   combination.
#' @param x,y Columns mapped to the x and y aesthetics (tidy eval).
#' @param colour Optional column mapped to the colour aesthetic (tidy
#'   eval); if omitted, a single line is drawn in `line_colour`.
#' @param line_colour Constant line colour used when no `colour` column
#'   is mapped. Defaults to `NULL`, which resolves to the CPB primary
#'   blue (`cpb_cols(6)`, `"#005faf"`). Ignored when `colour` is
#'   supplied.
#' @param linewidth Line width. `NULL` (default) resolves by `style`:
#'   `1.2` for `"ggplot"` (matching CPB source scripts), `0.55` for
#'   `"nplot"` (matching the published nplot figures).
#' @param palette CPB palette to use for `colour`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_colour_cpb_manual()] instead of the default
#'   [scale_colour_cpb_d()] when supplied.
#' @param pct_axis If `TRUE`, format the y axis with [label_pct_nl()].
#' @param style Style preset, forwarded to [theme_cpb()]; see
#'   [cpb_col()].
#' @param legend Legend position, forwarded to [theme_cpb()]; `NULL`
#'   (default) resolves by `style`.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis underneath the data lines, as the CPB `nplot()` house
#'   style does. `NULL` (default) resolves by `style`: for `"nplot"`
#'   the line is drawn when the `y` data spans (or touches) zero,
#'   mirroring nplot's bold-axis-if-zero behaviour; for `"ggplot"` it
#'   is not drawn.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()]; `NULL` (the default) resolves by
#'   `style`.
#' @param title,subtitle Plot title/subtitle.
#' @param xlab,colourlab Axis and legend title overrides; default
#'   to `NULL` (no axis title), matching CPB house style.
#' @param ylab Label for the value (y) axis. Following CPB house style
#'   it is rendered as the plot *subtitle* -- a left-aligned italic
#'   caption above the panel (e.g. the unit, `"%"`) -- unless an
#'   explicit `subtitle` is also given, in which case it falls back to
#'   a rotated y-axis title.
#' @param ... Further arguments passed to [ggplot2::geom_line()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   jaar = rep(2018:2022, 2),
#'   raming = rep(c("CEP", "MEV"), each = 5),
#'   bbp_groei = c(2.1, 1.8, -3.8, 4.9, 4.5, 2.4, 1.6, -3.6, 4.6, 4.2)
#' )
#' cpb_line(df, x = jaar, y = bbp_groei, colour = raming)
#' @export
cpb_line <- function(data, x, y, colour = NULL,
                      line_colour = NULL,
                      linewidth = NULL,
                      palette = "qualitative",
                      index = NULL,
                      pct_axis = FALSE,
                      style = c("ggplot", "nplot"),
                      legend = NULL,
                      zeroline = NULL,
                      minor = NULL,
                      ticks = NULL,
                      flush_legend = NULL,
                      axis_text_size = NULL,
                      legend_key_size = NULL,
                      grid_colour = NULL,
                      grid_linewidth = NULL,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      colourlab = NULL,
                      ...) {
  style <- match.arg(style)
  if (is.null(linewidth)) linewidth <- if (style == "nplot") 0.55 else 1.2

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colour <- rlang::enquo(colour)
  has_colour <- !rlang::quo_is_null(colour)

  # nplot bolds the zero line only when zero is actually on the axis, so
  # the auto setting checks whether the data spans (or touches) zero --
  # an unconditional hline at 0 would stretch the y range of an
  # all-positive chart (e.g. an index series) down to zero
  if (is.null(zeroline)) {
    yvals <- rlang::eval_tidy(y, data)
    zeroline <- style == "nplot" &&
      is.numeric(yvals) &&
      min(yvals, na.rm = TRUE) <= 0 && max(yvals, na.rm = TRUE) >= 0
  }

  if (has_colour) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, colour = !!colour)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }

  p <- ggplot2::ggplot(data, mapping)

  # underneath the data lines, matching nplot()
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  p <- p + if (has_colour) {
    ggplot2::geom_line(linewidth = linewidth, ...)
  } else {
    # no colour mapping: draw one flat house-style colour (CPB primary
    # blue by default) rather than black
    single_colour <- if (is.null(line_colour)) unname(cpb_cols(6)) else line_colour
    ggplot2::geom_line(linewidth = linewidth, colour = single_colour, ...)
  }

  if (isTRUE(pct_axis)) {
    p <- p + ggplot2::scale_y_continuous(labels = label_pct_nl())
  }

  if (has_colour) {
    p <- p + if (!is.null(index)) {
      scale_colour_cpb_manual(index = index, palette = palette)
    } else {
      scale_colour_cpb_d(palette = palette)
    }
  }

  # CPB convention: the value-axis label doubles as the subtitle (an
  # italic caption above the panel, typically the unit) rather than a
  # rotated axis title
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, colour = colourlab) +
    theme_cpb(
      style           = style,
      legend          = legend,
      minor           = minor,
      ticks           = ticks,
      flush_legend    = flush_legend,
      axis_text_size  = axis_text_size,
      legend_key_size = legend_key_size,
      grid_colour     = grid_colour,
      grid_linewidth  = grid_linewidth
    )
}

# quantile box/errorbar combo ----

#' A CPB-styled quantile box-and-errorbar chart
#'
#' Reproduces the p5/p25/p50/p75/p95 errorbar-plus-boxplot combination
#' used in CPB energy-crisis distributional figures: a thin errorbar
#' spanning the p5-p95 range, with a box spanning the p25-p75
#' interquartile range and a median line at p50 drawn on top. Both
#' layers use `stat = "identity"` -- pass precomputed quantile columns
#' rather than raw observations.
#'
#' @param data A data.frame or data.table with one row per box, holding
#'   precomputed quantile columns.
#' @param x Column mapped to the x aesthetic (tidy eval), i.e. the
#'   category each box belongs to.
#' @param p5,p25,p50,p75,p95 Columns holding the precomputed 5th,
#'   25th, 50th (median), 75th and 95th percentiles (tidy eval).
#' @param fill Optional column mapped to the fill aesthetic (tidy
#'   eval), e.g. for grouped boxes side by side.
#' @param fill_colour Constant box fill used when no `fill` column is
#'   mapped. Defaults to `NULL`, which resolves to the CPB primary blue
#'   (`cpb_cols(6)`, `"#005faf"`). Ignored when `fill` is supplied.
#' @param width Box width; the errorbar width is drawn at half this
#'   value. Defaults to `0.5`.
#' @param linewidth Stroke width of the box outlines, median line and
#'   errorbars. Defaults to `0.25`, matching the thin strokes of the
#'   published CPB distributional figures.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()] instead of the default
#'   [scale_fill_cpb_d()] when supplied.
#' @param orientation `"vertical"` (default) or `"horizontal"` (adds
#'   [ggplot2::coord_flip()] and is forwarded to [theme_cpb()]).
#' @param style Style preset, forwarded to [theme_cpb()]; see
#'   [cpb_col()].
#' @param legend Legend position, forwarded to [theme_cpb()]; `NULL`
#'   (default) resolves by `style`.
#' @param reverse_legend If `TRUE`, reverse the fill legend order via
#'   `guide_legend(reverse = TRUE)`. Defaults to `FALSE`; useful when
#'   the fill levels were reversed to control the dodge order under
#'   `coord_flip()`.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis underneath the boxes, as the CPB `nplot()` house style
#'   does. `NULL` (default) resolves by `style`: for `"nplot"` the
#'   line is drawn when the p5-p95 data spans (or touches) zero; for
#'   `"ggplot"` it is not drawn.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()]; `NULL` (the default) resolves by
#'   `style`.
#' @param title,subtitle Plot title/subtitle.
#' @param xlab,filllab Axis and legend title overrides; default
#'   to `NULL` (no axis title), matching CPB house style.
#' @param ylab Label for the value axis (the `y` aesthetic). When
#'   `orientation = "horizontal"` it is drawn as the bottom axis title
#'   (after `coord_flip()`). When `"vertical"`, CPB house style renders
#'   it as the plot *subtitle* -- unless an explicit `subtitle` is also
#'   given, in which case it falls back to a rotated y-axis title.
#' @param ... Further arguments passed to both [ggplot2::geom_errorbar()]
#'   and [ggplot2::geom_boxplot()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   groep = c("laag inkomen", "midden inkomen", "hoog inkomen"),
#'   p5  = c(-8, -6, -4),
#'   p25 = c(-4, -3, -2),
#'   p50 = c(-2, -1, 0),
#'   p75 = c(0, 1, 2),
#'   p95 = c(3, 4, 5)
#' )
#' cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
#' @export
cpb_box <- function(data, x, p5, p25, p50, p75, p95,
                     fill = NULL,
                     fill_colour = NULL,
                     width = 0.5,
                     linewidth = 0.25,
                     palette = "qualitative",
                     index = NULL,
                     orientation = c("vertical", "horizontal"),
                     style = c("ggplot", "nplot"),
                     legend = NULL,
                     reverse_legend = FALSE,
                     zeroline = NULL,
                     minor = NULL,
                     ticks = NULL,
                     flush_legend = NULL,
                     axis_text_size = NULL,
                     legend_key_size = NULL,
                     grid_colour = NULL,
                     grid_linewidth = NULL,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     filllab = NULL,
                     ...) {
  orientation <- match.arg(orientation)
  style <- match.arg(style)

  x <- rlang::enquo(x)
  p5  <- rlang::enquo(p5)
  p25 <- rlang::enquo(p25)
  p50 <- rlang::enquo(p50)
  p75 <- rlang::enquo(p75)
  p95 <- rlang::enquo(p95)
  fill <- rlang::enquo(fill)
  has_fill <- !rlang::quo_is_null(fill)

  # as in cpb_line(): only bold the zero line when zero is on the axis
  if (is.null(zeroline)) {
    lo <- rlang::eval_tidy(p5, data)
    hi <- rlang::eval_tidy(p95, data)
    zeroline <- style == "nplot" &&
      is.numeric(lo) && is.numeric(hi) &&
      min(lo, na.rm = TRUE) <= 0 && max(hi, na.rm = TRUE) >= 0
  }

  if (has_fill) {
    # group (not fill) drives the errorbar dodge: errorbars have no fill
    # aesthetic, and mapping one only triggers a warning
    mapping_errorbar <- ggplot2::aes(x = !!x, ymin = !!p5, ymax = !!p95, group = !!fill)
    mapping_box <- ggplot2::aes(
      x = !!x, ymin = !!p25, lower = !!p25, middle = !!p50, upper = !!p75,
      ymax = !!p75, fill = !!fill
    )
  } else {
    mapping_errorbar <- ggplot2::aes(x = !!x, ymin = !!p5, ymax = !!p95)
    mapping_box <- ggplot2::aes(
      x = !!x, ymin = !!p25, lower = !!p25, middle = !!p50, upper = !!p75,
      ymax = !!p75
    )
  }

  p <- ggplot2::ggplot(data)

  # underneath the boxes, matching nplot()
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  # key_glyph = "rect": CPB legends show plain colour squares, not
  # miniature boxplots. Without a fill mapping the boxes are drawn in
  # one flat house-style colour (CPB primary blue by default).
  box_args <- list(mapping = mapping_box, stat = "identity", width = width,
                   linewidth = linewidth, key_glyph = "rect", ...)
  if (!has_fill) {
    box_args$fill <- if (is.null(fill_colour)) unname(cpb_cols(6)) else fill_colour
  }
  p <- p +
    ggplot2::geom_errorbar(mapping = mapping_errorbar, width = width / 2,
                           linewidth = linewidth, ...) +
    do.call(ggplot2::geom_boxplot, box_args)

  if (orientation == "horizontal") {
    p <- p + ggplot2::coord_flip()
  }

  if (has_fill) {
    p <- p + if (!is.null(index)) {
      scale_fill_cpb_manual(index = index, palette = palette)
    } else {
      scale_fill_cpb_d(palette = palette)
    }
    if (isTRUE(reverse_legend)) {
      p <- p + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
    }
  }

  # CPB convention for vertical charts: the value-axis label doubles as
  # the subtitle; horizontally the value axis is drawn at the bottom
  # after coord_flip(), where a real axis title is appropriate
  lab_y <- ylab
  if (orientation == "vertical" && is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    theme_cpb(
      orientation     = orientation,
      style           = style,
      legend          = legend,
      minor           = minor,
      ticks           = ticks,
      flush_legend    = flush_legend,
      axis_text_size  = axis_text_size,
      legend_key_size = legend_key_size,
      grid_colour     = grid_colour,
      grid_linewidth  = grid_linewidth
    )
}
