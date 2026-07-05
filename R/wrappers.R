# wrappers.R ----
#
# Thin, high-level wrapper functions: data.frame in, finished-styled
# ggplot object out. Each wrapper applies theme_cpb() and a CPB scale
# and returns a real ggplot object -- it never saves or prints as a
# side effect, so users can keep adding layers with `+`. Columns are
# selected with tidy evaluation, so a plain data.frame or a
# data.table works transparently (both inherit data.frame).

# columns / bars ----

#' One-sided value-axis expansion for the house look
#'
#' CPB figures draw bars and areas sitting directly on the zero axis: the
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

#' Forecast-window annotation layers
#'
#' The house convention for marking the forecast part of a time axis
#' (nicknamed the "raming" window): a translucent white rectangle from
#' `forecast_x` to the right panel edge, drawn *underneath* the data,
#' plus an italic grey label centred in the window at the top of the
#' panel, drawn on top. Split in two helpers so the wrappers can layer
#' them on either side of their geoms.
#'
#' @noRd
cpb_forecast_rect <- function(forecast_x) {
  ggplot2::annotate("rect", xmin = forecast_x, xmax = Inf,
                    ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.45)
}

#' @noRd
cpb_forecast_label <- function(forecast_x, xvals, label) {
  if (is.null(label) || !nzchar(label)) return(NULL)
  x_max <- suppressWarnings(max(as.numeric(xvals), na.rm = TRUE))
  if (is.finite(x_max) && x_max > forecast_x) {
    # centred in the window, as the legacy plotter does
    label_x <- (forecast_x + x_max) / 2
    hjust <- 0.5
  } else {
    label_x <- forecast_x
    hjust <- -0.15
  }
  ggplot2::annotate("text", x = label_x, y = Inf, label = label,
                    vjust = 1.8, hjust = hjust, size = 2.2,
                    colour = "#666666", family = cpb_font_family(),
                    fontface = "italic")
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
#' @param forecast_x Optional x value where the forecast window starts
#'   (vertical charts with a numeric/time x axis). Everything to its
#'   right is overlaid with a translucent white rectangle underneath
#'   the bars and labelled with `forecast_label`. Pick a value between
#'   two bars (e.g. `2025.5`) so no bar is cut.
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)` -- stacking otherwise
#'   makes the legend order counter-intuitive.
#' @param legend Legend position, forwarded to [theme_cpb()]; accepts
#'   `"bottom"` (default), `"right"`, `"left"`, `"top"`, `"none"`, or
#'   a two-element numeric vector of plot-relative coordinates.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis on top of the bars, as the CPB house style does.
#'   Defaults to `TRUE` (bars are anchored at zero).
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
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
                     forecast_x = NULL,
                     forecast_label = "raming",
                     reverse_legend = TRUE,
                     legend = "bottom",
                     zeroline = TRUE,
                     minor = FALSE,
                     ticks = TRUE,
                     flush_legend = TRUE,
                     axis_text_size = 7,
                     legend_key_size = NULL,
                     grid_colour = "black",
                     grid_linewidth = 0.1,
                     title = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     filllab = NULL,
                     ...) {
  position <- match.arg(position)
  orientation <- match.arg(orientation)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  has_fill <- !rlang::quo_is_null(fill)

  if (has_fill) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, fill = !!fill)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }
  p <- ggplot2::ggplot(data, mapping)

  # the forecast window sits underneath the bars
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(forecast_x)
  }

  p <- p + if (has_fill) {
    ggplot2::geom_col(position = position, ...)
  } else {
    # No fill mapping: draw one flat house-style colour (CPB primary blue
    # by default) rather than ggplot2's grey.
    single_fill <- if (is.null(fill_colour)) unname(cpb_cols(6)) else fill_colour
    ggplot2::geom_col(position = position, fill = single_fill, ...)
  }

  # The zero line sits on the value axis (the y aesthetic even under
  # coord_flip()) and is drawn on top of the bars.
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(forecast_x, rlang::eval_tidy(x, data), forecast_label)
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
  # zero-flush expansion can coexist
  scale_args <- list()
  if (isTRUE(pct_axis)) {
    pct_scale <- if (position == "fill") 100 else 1
    scale_args$labels <- label_pct_nl(scale = pct_scale)
  }
  if (!is.null(value_breaks)) {
    scale_args$breaks <- value_breaks
  }
  expand <- cpb_zero_flush_expand(rlang::eval_tidy(y, data))
  if (!is.null(expand)) scale_args$expand <- expand
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

  # a titled figure always reserves the subtitle line, so the gap
  # between title and panel is stable whether or not a subtitle is set
  subtitle <- ylab
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = lab_x, y = lab_y, fill = filllab) +
    theme_cpb(
      orientation     = orientation,
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
#' @param forecast_x Optional x value where the forecast window
#'   starts; overlaid and labelled as in [cpb_line()].
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param zeroline If `TRUE` (default), draw a solid black line at
#'   zero on the value axis on top of the areas, as the CPB house
#'   style does.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
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
                      forecast_x = NULL,
                      forecast_label = "raming",
                      reverse_legend = TRUE,
                      legend = "bottom",
                      zeroline = TRUE,
                      minor = FALSE,
                      ticks = TRUE,
                      flush_legend = TRUE,
                      axis_text_size = 7,
                      legend_key_size = NULL,
                      grid_colour = "black",
                      grid_linewidth = 0.1,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      filllab = NULL,
                      ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, fill = !!fill))

  # the forecast window sits underneath the areas
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(forecast_x)
  }

  p <- p + ggplot2::geom_area(...)

  # on top of the areas
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(forecast_x, rlang::eval_tidy(x, data), forecast_label)
  }

  # assembled once, as in cpb_col()
  scale_args <- list()
  if (isTRUE(pct_axis)) {
    scale_args$labels <- label_pct_nl()
  }
  expand <- cpb_zero_flush_expand(rlang::eval_tidy(y, data))
  if (!is.null(expand)) scale_args$expand <- expand
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
  # italic caption above the panel) rather than a rotated axis title.
  # A titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    theme_cpb(
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
#' @param linewidth Line width; defaults to `0.55`, matching the
#'   published CPB figures.
#' @param palette CPB palette to use for `colour`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_colour_cpb_manual()] instead of the default
#'   [scale_colour_cpb_d()] when supplied.
#' @param pct_axis If `TRUE`, format the y axis with [label_pct_nl()].
#' @param ymin,ymax Optional columns (tidy eval) bounding an
#'   uncertainty band, drawn as a translucent ribbon underneath the
#'   line(s). With a `colour` mapping each series gets a band in its
#'   own colour; otherwise the band uses the line colour.
#' @param forecast_x Optional x value where the forecast window
#'   starts. Everything to its right is overlaid with a translucent
#'   white rectangle (drawn underneath the data) and labelled with
#'   `forecast_label`, the house convention for marking predicted
#'   values.
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis underneath the data lines. `NULL` (default) draws it
#'   automatically when the `y` data spans (or touches) zero, the
#'   house bold-axis-if-zero convention.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
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
                      linewidth = 0.55,
                      palette = "qualitative",
                      index = NULL,
                      pct_axis = FALSE,
                      ymin = NULL,
                      ymax = NULL,
                      forecast_x = NULL,
                      forecast_label = "raming",
                      legend = "bottom",
                      zeroline = NULL,
                      minor = FALSE,
                      ticks = TRUE,
                      flush_legend = TRUE,
                      axis_text_size = 7,
                      legend_key_size = NULL,
                      grid_colour = "black",
                      grid_linewidth = 0.1,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      colourlab = NULL,
                      ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colour <- rlang::enquo(colour)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  has_colour <- !rlang::quo_is_null(colour)
  has_band <- !rlang::quo_is_null(ymin) && !rlang::quo_is_null(ymax)

  # the house style bolds the zero line only when zero is on the axis, so
  # the auto setting checks whether the data spans (or touches) zero --
  # an unconditional hline at 0 would stretch the y range of an
  # all-positive chart (e.g. an index series) down to zero
  if (is.null(zeroline)) {
    yvals <- rlang::eval_tidy(y, data)
    zeroline <- is.numeric(yvals) &&
      min(yvals, na.rm = TRUE) <= 0 && max(yvals, na.rm = TRUE) >= 0
  }

  if (has_colour) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, colour = !!colour)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }

  p <- ggplot2::ggplot(data, mapping)

  # background layers first: the forecast window, then the zero line,
  # then the uncertainty band, so the data lines stay on top
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(forecast_x)
  }
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  single_colour <- if (is.null(line_colour)) unname(cpb_cols(6)) else line_colour

  if (has_band) {
    if (has_colour) {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = !!ymin, ymax = !!ymax, fill = !!colour),
        alpha = 0.25, colour = NA
      ) +
        (if (!is.null(index)) {
          scale_fill_cpb_manual(index = index, palette = palette)
        } else {
          scale_fill_cpb_d(palette = palette)
        }) +
        ggplot2::guides(fill = "none")
    } else {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = !!ymin, ymax = !!ymax),
        fill = single_colour, alpha = 0.25, colour = NA
      )
    }
  }

  p <- p + if (has_colour) {
    ggplot2::geom_line(linewidth = linewidth, ...)
  } else {
    # no colour mapping: draw one flat house-style colour (CPB primary
    # blue by default) rather than black
    ggplot2::geom_line(linewidth = linewidth, colour = single_colour, ...)
  }

  # the label sits on top of everything
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(forecast_x, rlang::eval_tidy(x, data), forecast_label)
  }

  # the panel is drawn tight around the data/limits, so the axis line
  # and ticks meet the outermost gridlines instead of floating beyond
  # them
  p <- p + ggplot2::coord_cartesian(expand = FALSE)

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
  # rotated axis title. A titled figure always reserves the subtitle
  # line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, colour = colourlab) +
    theme_cpb(
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
#' used in CPB distributional figures: a thin errorbar
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
#' @param reverse_legend If `TRUE`, reverse the fill legend order via
#'   `guide_legend(reverse = TRUE)`. Defaults to `FALSE`; useful when
#'   the fill levels were reversed to control the dodge order under
#'   `coord_flip()`.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis underneath the boxes. `NULL` (default) draws it
#'   automatically when the p5-p95 data spans (or touches) zero, the
#'   house bold-axis-if-zero convention.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
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
                     legend = "bottom",
                     reverse_legend = FALSE,
                     zeroline = NULL,
                     minor = FALSE,
                     ticks = TRUE,
                     flush_legend = TRUE,
                     axis_text_size = 7,
                     legend_key_size = NULL,
                     grid_colour = "black",
                     grid_linewidth = 0.1,
                     title = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     filllab = NULL,
                     ...) {
  orientation <- match.arg(orientation)

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
    zeroline <- is.numeric(lo) && is.numeric(hi) &&
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

  # underneath the boxes
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
  # after coord_flip(), where a real axis title is appropriate. A
  # titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (orientation == "vertical" && is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    theme_cpb(
      orientation     = orientation,
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

# scatter ----

#' A CPB-styled scatter plot
#'
#' Thin wrapper around [ggplot2::geom_point()] with CPB theming and
#' colour scale applied. Returns a real ggplot object that can be
#' extended further with `+`.
#'
#' @param data A data.frame or data.table with one row per point.
#' @param x,y Columns mapped to the x and y aesthetics (tidy eval).
#' @param colour Optional column mapped to the colour aesthetic (tidy
#'   eval). A numeric column gets the continuous CPB gradient
#'   ([scale_colour_cpb_c()]); a discrete column gets the discrete CPB
#'   palette. If omitted, points are drawn in `point_colour`.
#' @param point_colour Constant point colour used when no `colour`
#'   column is mapped. Defaults to `NULL`, which resolves to the CPB
#'   primary blue (`cpb_cols(6)`, `"#005faf"`).
#' @param size Point size; defaults to `0.8`.
#' @param palette CPB palette used for a *discrete* `colour` column;
#'   one of `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions for a
#'   discrete `colour` column, forwarded to
#'   [scale_colour_cpb_manual()].
#' @param legend Legend position, forwarded to [theme_cpb()].
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis underneath the points. `NULL` (default) draws it
#'   automatically when the `y` data spans (or touches) zero.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
#' @param title,subtitle Plot title/subtitle.
#' @param xlab,colourlab Axis and legend title overrides; default to
#'   `NULL`, matching CPB house style.
#' @param ylab Label for the value (y) axis. Following CPB house style
#'   it is rendered as the plot *subtitle* -- a left-aligned italic
#'   caption above the panel -- unless an explicit `subtitle` is also
#'   given, in which case it falls back to a rotated y-axis title.
#' @param ... Further arguments passed to [ggplot2::geom_point()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(inkomen = rlnorm(100, log(2500), 0.3))
#' df$energie <- 100 + 0.03 * df$inkomen + rnorm(100, 0, 30)
#' cpb_scatter(df, x = inkomen, y = energie,
#'   title = "Energierekening naar inkomen",
#'   ylab  = "energierekening (euro per maand)",
#'   xlab  = "besteedbaar inkomen (euro per maand)")
#' @export
cpb_scatter <- function(data, x, y, colour = NULL,
                         point_colour = NULL,
                         size = 0.8,
                         palette = "qualitative",
                         index = NULL,
                         legend = "bottom",
                         zeroline = NULL,
                         minor = FALSE,
                         ticks = TRUE,
                         flush_legend = TRUE,
                         axis_text_size = 7,
                         legend_key_size = NULL,
                         grid_colour = "black",
                         grid_linewidth = 0.1,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         colourlab = NULL,
                         ...) {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colour <- rlang::enquo(colour)
  has_colour <- !rlang::quo_is_null(colour)

  if (is.null(zeroline)) {
    yvals <- rlang::eval_tidy(y, data)
    zeroline <- is.numeric(yvals) &&
      min(yvals, na.rm = TRUE) <= 0 && max(yvals, na.rm = TRUE) >= 0
  }

  if (has_colour) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, colour = !!colour)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }

  p <- ggplot2::ggplot(data, mapping)

  # underneath the points
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  p <- p + if (has_colour) {
    ggplot2::geom_point(size = size, ...)
  } else {
    single_colour <- if (is.null(point_colour)) unname(cpb_cols(6)) else point_colour
    ggplot2::geom_point(size = size, colour = single_colour, ...)
  }

  # a numeric colour column gets the continuous gradient, anything
  # else the discrete palette
  if (has_colour) {
    colvals <- rlang::eval_tidy(colour, data)
    p <- p + if (is.numeric(colvals)) {
      scale_colour_cpb_c()
    } else if (!is.null(index)) {
      scale_colour_cpb_manual(index = index, palette = palette)
    } else {
      scale_colour_cpb_d(palette = palette)
    }
  }

  # CPB convention: the value-axis label doubles as the subtitle. A
  # titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, colour = colourlab) +
    theme_cpb(
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

# histogram ----

#' A CPB-styled histogram
#'
#' Thin wrapper around [ggplot2::geom_histogram()] with CPB theming
#' applied: house-blue bars with white outlines, a black zero line and
#' a count axis that starts on the axis line. Returns a real ggplot
#' object that can be extended further with `+`.
#'
#' @param data A data.frame or data.table with one row per observation.
#' @param x Column with the observations to bin (tidy eval).
#' @param fill Optional column mapped to the fill aesthetic (tidy
#'   eval) for grouped histograms; if omitted, bars are drawn in
#'   `fill_colour`.
#' @param fill_colour Constant bar fill used when no `fill` column is
#'   mapped. Defaults to `NULL`, which resolves to the CPB primary
#'   blue (`cpb_cols(6)`, `"#005faf"`).
#' @param binwidth,bins Passed to [ggplot2::geom_histogram()]; set one
#'   of them (ggplot2 defaults to `bins = 30` with a warning
#'   otherwise).
#' @param outline Bar outline colour; defaults to `"white"`, the house
#'   look for histograms.
#' @param position Position adjustment for grouped histograms;
#'   defaults to `"stack"`.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()].
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)`.
#' @param legend Legend position, forwarded to [theme_cpb()].
#' @param zeroline If `TRUE` (default), draw a solid black line at
#'   zero on the count axis on top of the bars.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
#' @param title,subtitle Plot title/subtitle.
#' @param xlab,filllab Axis and legend title overrides; default to
#'   `NULL`, matching CPB house style.
#' @param ylab Label for the count (y) axis, rendered as the plot
#'   *subtitle* (e.g. `"aantal"`) unless an explicit `subtitle` is
#'   also given.
#' @param ... Further arguments passed to [ggplot2::geom_histogram()].
#' @return A `ggplot` object.
#' @examples
#' library(ggplot2)
#' df <- data.frame(duur = rgamma(1000, 8, 0.6))
#' cpb_hist(df, x = duur, binwidth = 2,
#'   title = "Verdeling van de duur",
#'   ylab  = "aantal",
#'   xlab  = "duur (maanden)")
#' @export
cpb_hist <- function(data, x, fill = NULL,
                      fill_colour = NULL,
                      binwidth = NULL,
                      bins = NULL,
                      outline = "white",
                      position = "stack",
                      palette = "qualitative",
                      index = NULL,
                      reverse_legend = TRUE,
                      legend = "bottom",
                      zeroline = TRUE,
                      minor = FALSE,
                      ticks = TRUE,
                      flush_legend = TRUE,
                      axis_text_size = 7,
                      legend_key_size = NULL,
                      grid_colour = "black",
                      grid_linewidth = 0.1,
                      title = NULL,
                      subtitle = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      filllab = NULL,
                      ...) {
  x <- rlang::enquo(x)
  fill <- rlang::enquo(fill)
  has_fill <- !rlang::quo_is_null(fill)

  if (has_fill) {
    mapping <- ggplot2::aes(x = !!x, fill = !!fill)
  } else {
    mapping <- ggplot2::aes(x = !!x)
  }

  p <- ggplot2::ggplot(data, mapping)

  p <- p + if (has_fill) {
    ggplot2::geom_histogram(binwidth = binwidth, bins = bins, position = position,
                            colour = outline, linewidth = 0.2, ...)
  } else {
    single_fill <- if (is.null(fill_colour)) unname(cpb_cols(6)) else fill_colour
    ggplot2::geom_histogram(binwidth = binwidth, bins = bins, position = position,
                            colour = outline, linewidth = 0.2, fill = single_fill, ...)
  }

  # counts are anchored at zero: black zero line on top of the bars and
  # a count axis flush with the axis line
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))

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

  # CPB convention: the count-axis label doubles as the subtitle. A
  # titled figure always reserves the subtitle line for a stable gap.
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    ylab <- NULL
  }
  if (!is.null(title) && is.null(subtitle)) subtitle <- " "

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab, fill = filllab) +
    theme_cpb(
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
