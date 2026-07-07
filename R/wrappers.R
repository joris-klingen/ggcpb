# wrappers.R ----
#
# Thin, high-level wrapper functions: data.frame in, finished-styled
# ggplot object out. Each wrapper applies theme_cpb() and a CPB scale
# and returns a real ggplot object -- it never saves or prints as a
# side effect, so users can keep adding layers with `+`. Columns are
# selected with tidy evaluation, so a plain data.frame or a
# data.table works transparently (both inherit data.frame).


# shared wrapper tail ----

# Every cpb_*() wrapper ends the same way: forward the shared theme
# knobs to theme_cpb(). The knobs are read *by name* from the wrapper's
# own frame, so a wrapper that misses one of them fails loudly here
# (mget() errors on a missing name) instead of silently dropping the
# argument -- the mechanism that keeps the wrapper signatures from
# drifting apart.
cpb_wrapper_theme <- function(env = parent.frame()) {
  args <- mget(
    c("legend", "minor", "ticks", "flush_legend", "axis_text_size",
      "legend_key_size", "grid_colour", "grid_linewidth"),
    envir = env
  )
  args$orientation <- mget("orientation", envir = env,
                           ifnotfound = list("vertical"))[[1]]
  do.call(theme_cpb, args)
}

# Two-level category axis without facets: categories keep one shared
# value axis, but are laid out in blocks with a gap between groups (see
# the grouped published figures, e.g. "geslacht" vs "opleiding ouders").
# Returns one row per category with its numeric axis position plus the
# group centres for the bold group labels.
cpb_group_positions <- function(cats, groups, gap = 0.8) {
  cats <- as.factor(cats)
  groups <- as.factor(groups)
  # one group per category (categories nested in groups)
  map <- unique(data.frame(cat = cats, grp = groups))
  if (anyDuplicated(map$cat)) {
    stop("each `x` category must belong to exactly one `group`.", call. = FALSE)
  }
  # lay the groups out in factor-level order, categories in level order
  # within their group
  map <- map[order(as.integer(map$grp), as.integer(map$cat)), , drop = FALSE]
  offset <- (as.integer(factor(map$grp, levels = unique(map$grp))) - 1) * gap
  map$pos <- seq_len(nrow(map)) + offset
  centres <- vapply(split(map$pos, map$grp), mean, numeric(1))
  list(map = map, centres = centres[unique(as.character(map$grp))])
}

# Grouped category slots with a heading row per group (the vertical
# grouping of the published distributional figures: a bold group name
# on its own row above its categories, all sharing one value axis).
# Returns one row per slot: heading rows have heading = TRUE. Positions
# descend from the first slot, so under coord_flip() the first group
# reads from the top.
cpb_group_heading_positions <- function(cats, groups, gap = 0.7) {
  map <- cpb_group_positions(cats, groups, gap = 0)$map
  out <- NULL
  pos <- 0
  for (g in unique(as.character(map$grp))) {
    if (!is.null(out)) pos <- pos - gap
    cts <- as.character(map$cat[as.character(map$grp) == g])
    if (length(cts) == 1 && cts == g) {
      # a single-category group named after itself collapses onto its
      # heading row (e.g. the "Alle huishoudens" total)
      pos <- pos - 1
      out <- rbind(out, data.frame(label = g, cat = cts,
                                   heading = TRUE, pos = pos))
      next
    }
    pos <- pos - 1
    out <- rbind(out, data.frame(label = g, cat = NA_character_,
                                 heading = TRUE, pos = pos))
    for (ct in cts) {
      pos <- pos - 1
      out <- rbind(out, data.frame(label = ct, cat = ct,
                                   heading = FALSE, pos = pos))
    }
  }
  out$pos <- out$pos - min(out$pos) + 1
  out
}

# Faceting in house style: the facet title is a bold strip *below*
# its panel (the legacy nicerplot placement) and every panel is a
# complete mini-figure with its own axes and axis labels.
cpb_add_facet <- function(p, facet, facet_ncol = NULL, facet_scales = "fixed") {
  if (rlang::quo_is_null(facet)) return(p)
  p + ggplot2::facet_wrap(ggplot2::vars(!!facet), ncol = facet_ncol,
                          scales = facet_scales,
                          strip.position = "bottom",
                          axes = "all", axis.labels = "all")
}

# a titled figure always reserves the subtitle line, so the gap between
# title and panel is stable whether or not a subtitle is set
cpb_reserve_subtitle <- function(title, subtitle) {
  if (!is.null(title) && is.null(subtitle)) " " else subtitle
}

# The value-axis scale arguments are assembled once, so percentage
# labels, custom breaks and the zero-flush expansion can coexist in a
# single scale_y_continuous() -- adding a second y scale would replace
# the first with a message.
cpb_value_scale_args <- function(values = NULL, pct_axis = FALSE, pct_scale = 1,
                                 value_breaks = NULL) {
  args <- list()
  if (isTRUE(pct_axis)) args$labels <- label_pct_nl(scale = pct_scale)
  if (!is.null(value_breaks)) args$breaks <- value_breaks
  if (!is.null(values)) {
    expand <- cpb_zero_flush_expand(values)
    if (!is.null(expand)) args$expand <- expand
  }
  args
}

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
#' @param group Optional column (tidy eval) assigning each `x` category
#'   to a group, for the published two-level category axis: categories
#'   are laid out in blocks with a gap between groups on *one shared
#'   value axis* (no facets), and the group names are printed in bold
#'   under the category labels. Each category must belong to exactly
#'   one group; groups and categories follow their factor-level order.
#'   The group labels occupy the x-axis-title line, so `xlab` is not
#'   available; vertical charts only.
#' @param group_gap Gap between group blocks, in category widths;
#'   defaults to `0.8`.
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
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
#' @param legend Legend position, forwarded to [theme_cpb()]; accepts
#'   `"bottom"` (default), `"right"`, `"left"`, `"top"`, `"none"`, or
#'   a two-element numeric vector of plot-relative coordinates.
#' @param zeroline If `TRUE`, draw a solid black line at zero on the
#'   value axis on top of the bars, as the CPB house style does.
#'   Defaults to `TRUE` (bars are anchored at zero).
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults.
#' @param title,subtitle Plot title/subtitle. `subtitle` is normally
#'   left `NULL`: CPB house style fills the subtitle line with `ylab`
#'   (the value-axis caption). An explicit `subtitle` wins, in which
#'   case a vertical chart's `ylab` falls back to a rotated y-axis
#'   title.
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
                     group = NULL,
                     group_gap = 0.8,
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
                     facet = NULL,
                     facet_ncol = NULL,
                     facet_scales = "fixed",
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
  position <- match.arg(position)
  orientation <- match.arg(orientation)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  has_fill <- !rlang::quo_is_null(fill)
  has_group <- !rlang::quo_is_null(group)

  if (has_group) {
    # a two-level category axis: categories in gapped group blocks on
    # one shared value axis, group names in bold under the categories
    if (orientation == "horizontal") {
      stop("`group` is only supported for vertical column charts; for ",
           "horizontal grouped categories see the `group` argument of ",
           "cpb_box().", call. = FALSE)
    }
    if (!is.null(forecast_x)) {
      stop("`group` and `forecast_x` cannot be combined: the grouped ",
           "category axis is not a time axis.", call. = FALSE)
    }
    grp <- cpb_group_positions(rlang::eval_tidy(x, data),
                               rlang::eval_tidy(group, data),
                               gap = group_gap)
    data <- as.data.frame(data)
    data[["cpb__x"]] <- grp$map$pos[match(as.character(rlang::eval_tidy(x, data)),
                                             as.character(grp$map$cat))]
    x <- rlang::quo(.data[["cpb__x"]])
  }

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

  if (has_group) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = grp$map$pos,
        labels = as.character(grp$map$cat),
        expand = ggplot2::expansion(add = 0.7)
      ) +
      # the group names sit in bold under the category labels, on the
      # line the x-axis title would otherwise use; clip is off so the
      # text can be drawn below the panel
      ggplot2::annotate("text",
        x = unname(grp$centres), y = -Inf, label = names(grp$centres),
        vjust = 5.1, fontface = "bold", size = 7 / ggplot2::.pt,
        family = cpb_font_family()
      ) +
      ggplot2::coord_cartesian(ylim = value_limits, clip = "off")
  } else if (orientation == "horizontal") {
    p <- p + if (!is.null(value_limits)) {
      ggplot2::coord_flip(ylim = value_limits)
    } else {
      ggplot2::coord_flip()
    }
  } else if (!is.null(value_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = value_limits)
  }

  scale_args <- cpb_value_scale_args(
    values       = rlang::eval_tidy(y, data),
    pct_axis     = pct_axis,
    pct_scale    = if (position == "fill") 100 else 1,
    value_breaks = value_breaks
  )
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
  # the bold group labels occupy the axis-title line, so it is always
  # reserved (an explicit xlab would collide with them)
  if (has_group) lab_x <- " "

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  if (is.null(subtitle)) {
    subtitle <- ylab
  } else if (!is.null(ylab) && orientation == "vertical") {
    # an explicit subtitle occupies the caption line, so the value-axis
    # label falls back to a rotated axis title, as in the other wrappers
    lab_y <- ylab
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = lab_x, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()
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
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 limits for the value axis,
#'   applied through the coordinate system (zoom) so no data is
#'   dropped.
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)`.
#' @param forecast_x Optional x value where the forecast window
#'   starts; overlaid and labelled as in [cpb_line()].
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
#' @param legend Legend position, forwarded to [theme_cpb()].
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
                      value_breaks = NULL,
                      value_limits = NULL,
                      forecast_x = NULL,
                      forecast_label = "raming",
                      reverse_legend = TRUE,
                      facet = NULL,
                      facet_ncol = NULL,
                      facet_scales = "fixed",
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
  facet <- rlang::enquo(facet)

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

  scale_args <- cpb_value_scale_args(
    values       = rlang::eval_tidy(y, data),
    pct_axis     = pct_axis,
    value_breaks = value_breaks
  )
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }
  if (!is.null(value_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = value_limits)
  }

  p <- p + if (!is.null(index)) {
    scale_fill_cpb_manual(index = index, palette = palette)
  } else {
    scale_fill_cpb_d(palette = palette)
  }

  if (isTRUE(reverse_legend)) {
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB convention: the value-axis label doubles as the subtitle (an
  # italic caption above the panel) rather than a rotated axis title.
  # A titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()
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
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 limits for the value axis,
#'   applied through the coordinate system (zoom) so no data is
#'   dropped.
#' @param reverse_legend If `TRUE`, reverse the colour legend order
#'   via `guide_legend(reverse = TRUE)`. Defaults to `FALSE`: unlike
#'   the stacked wrappers, line order carries no stacking convention.
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
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
#' @param legend Legend position, forwarded to [theme_cpb()].
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
                      value_breaks = NULL,
                      value_limits = NULL,
                      ymin = NULL,
                      ymax = NULL,
                      forecast_x = NULL,
                      forecast_label = "raming",
                      reverse_legend = FALSE,
                      facet = NULL,
                      facet_ncol = NULL,
                      facet_scales = "fixed",
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
  facet <- rlang::enquo(facet)
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
  p <- p + ggplot2::coord_cartesian(ylim = value_limits, expand = FALSE)

  # no zero-flush expansion here: the tight coord above already pins
  # the panel to the data/limits
  scale_args <- cpb_value_scale_args(pct_axis = pct_axis,
                                     value_breaks = value_breaks)
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  if (has_colour) {
    p <- p + if (!is.null(index)) {
      scale_colour_cpb_manual(index = index, palette = palette)
    } else {
      scale_colour_cpb_d(palette = palette)
    }
    if (isTRUE(reverse_legend)) {
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
    }
  }

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB convention: the value-axis label doubles as the subtitle (an
  # italic caption above the panel, typically the unit) rather than a
  # rotated axis title. A titled figure always reserves the subtitle
  # line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, colour = colourlab) +
    cpb_wrapper_theme()
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
#'   eval), e.g. for grouped boxes side by side. Only supported by
#'   `box_style = "ggcpb"`.
#' @param fill_colour Constant box fill used when no `fill` column is
#'   mapped. Defaults to `NULL`, which resolves to the CPB primary blue
#'   (`cpb_cols(6)`, `"#005faf"`) for `"ggcpb"`/`"james"` and the CPB
#'   light blue (`cpb_cols(5)`, `"#87d2ff"`) for `"modern"`. Ignored
#'   when `fill` is supplied. For the `"james"`/`"modern"` styles it
#'   may also be a *vector* with one colour per row of `data` (e.g.
#'   one colour per `group`), recycled if shorter.
#' @param group Optional column (tidy eval) assigning each `x` category
#'   to a group, for the published vertically grouped layout: every
#'   group gets its name as a bold heading row on the category axis
#'   above its categories, all boxes share one value axis. A group
#'   containing exactly one category with the same name as the group
#'   collapses onto its heading row (e.g. an "Alle huishoudens" total).
#'   Cannot be combined with a `fill` mapping. Typically used with
#'   `orientation = "horizontal"`. The category rows keep the house
#'   category ticks; the bold headings carry none and are outdented.
#' @param group_gap Extra gap between group blocks, in category
#'   widths; defaults to `0.7`.
#' @param box_style How the boxes are constructed:
#'   * `"ggcpb"` (default): the style already used in CPB
#'     distributional figures -- capped errorbar whiskers plus an
#'     outlined box with a median line.
#'   * `"james"`: the legacy `nplot()` box -- a borderless filled box,
#'     plain (capless) whiskers in the box colour, a black median line
#'     extending slightly beyond the box, and the median value printed
#'     above it.
#'   * `"modern"`: the designer variant of `"james"` -- light-blue box
#'     and whiskers, a thick dark-blue median line, the median value
#'     in bold above it and the p25/p75 values printed below the box
#'     ends.
#'
#'   `"james"` and `"modern"` follow the house convention of
#'   horizontal boxes; combine them with
#'   `orientation = "horizontal"`.
#' @param box_labels Whether to print the value labels of the
#'   `"james"`/`"modern"` styles. `NULL` (default) resolves by
#'   `box_style` (`TRUE` for `"james"`/`"modern"`, which always
#'   ignore it under a `fill` mapping); ignored for `"ggcpb"`.
#' @param label_accuracy Rounding of the printed value labels, passed
#'   to [label_number_nl()]; defaults to `0.1` (one decimal, Dutch
#'   comma).
#' @param width Box width; the errorbar width is drawn at half this
#'   value. Defaults to `0.5`.
#' @param linewidth Stroke width of the box outlines, median line and
#'   errorbars in the `"ggcpb"` style. Defaults to `0.25`, matching
#'   the thin strokes of the published CPB distributional figures.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, or `"sequential"`.
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()] instead of the default
#'   [scale_fill_cpb_d()] when supplied.
#' @param pct_axis If `TRUE`, format the value axis with
#'   [label_pct_nl()].
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 limits for the value axis,
#'   applied through the coordinate system (zoom) so no data is
#'   dropped.
#' @param orientation `"vertical"` (default) or `"horizontal"` (adds
#'   [ggplot2::coord_flip()] and is forwarded to [theme_cpb()]).
#' @param reverse_legend If `TRUE`, reverse the fill legend order via
#'   `guide_legend(reverse = TRUE)`. Defaults to `FALSE`; useful when
#'   the fill levels were reversed to control the dodge order under
#'   `coord_flip()`.
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
#' @param legend Legend position, forwarded to [theme_cpb()].
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
                     group = NULL,
                     group_gap = 0.7,
                     box_style = c("ggcpb", "james", "modern"),
                     box_labels = NULL,
                     label_accuracy = 0.1,
                     width = 0.5,
                     linewidth = 0.25,
                     palette = "qualitative",
                     index = NULL,
                     pct_axis = FALSE,
                     value_breaks = NULL,
                     value_limits = NULL,
                     orientation = c("vertical", "horizontal"),
                     facet = NULL,
                     facet_ncol = NULL,
                     facet_scales = "fixed",
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
  box_style <- match.arg(box_style)

  x <- rlang::enquo(x)
  p5  <- rlang::enquo(p5)
  p25 <- rlang::enquo(p25)
  p50 <- rlang::enquo(p50)
  p75 <- rlang::enquo(p75)
  p95 <- rlang::enquo(p95)
  fill <- rlang::enquo(fill)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  has_fill <- !rlang::quo_is_null(fill)
  has_group <- !rlang::quo_is_null(group)

  if (has_group && has_fill) {
    stop("`group` organises single boxes under bold group headings and ",
         "cannot be combined with a `fill` mapping.", call. = FALSE)
  }
  slots <- NULL
  if (has_group) {
    # vertical grouping: every group gets a bold heading row above its
    # categories; all boxes share one value axis
    slots <- cpb_group_heading_positions(rlang::eval_tidy(x, data),
                                         rlang::eval_tidy(group, data),
                                         gap = group_gap)
    data <- as.data.frame(data)
    data[["cpb__x"]] <- slots$pos[match(as.character(rlang::eval_tidy(x, data)),
                                           slots$cat)]
    x <- rlang::quo(.data[["cpb__x"]])
  }

  if (has_fill && box_style != "ggcpb") {
    stop("box_style = \"", box_style, "\" draws single-colour boxes and does ",
         "not support a `fill` mapping; use box_style = \"ggcpb\" for ",
         "fill-grouped boxes.", call. = FALSE)
  }
  if (is.null(box_labels)) box_labels <- box_style != "ggcpb"

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
    # the explicit group keeps one box per category when the category
    # axis is numeric (the grouped-slots layout)
    mapping_errorbar <- ggplot2::aes(x = !!x, ymin = !!p5, ymax = !!p95,
                                     group = !!x)
    mapping_box <- ggplot2::aes(
      x = !!x, ymin = !!p25, lower = !!p25, middle = !!p50, upper = !!p75,
      ymax = !!p75, group = !!x
    )
  }

  # A vector fill_colour gives every box its own colour (e.g. one
  # colour per group in the grouped layout). It is carried as a data
  # column and mapped with I(), because ggplot2 reorders rows by axis
  # position and a plain parameter vector would not travel with them.
  row_cols <- NULL
  if (box_style != "ggcpb" && length(fill_colour) > 1) {
    data <- as.data.frame(data)
    data[["cpb__boxcol"]] <- rep_len(fill_colour, nrow(data))
    row_cols <- TRUE
  }

  p <- ggplot2::ggplot(data)

  # underneath the boxes
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  if (box_style == "ggcpb") {
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
  } else {
    # "james" (the legacy nplot() box) and "modern" (its designer
    # variant) share one construction: a borderless filled box over
    # p25-p75, plain capless whiskers in the box colour, and a median
    # line extending slightly beyond the box. They differ in colours,
    # weights and which value labels are printed.
    sty <- switch(box_style,
      james = list(
        box_col   = if (is.null(fill_colour)) unname(cpb_cols(6)) else fill_colour,
        whisk_lw  = 0.4,
        med_col   = "black", med_lw = 0.4, med_ext = 0.15,
        med_lab_col = "black", med_lab_face = "plain", med_lab_size = 2.2,
        q_labels  = FALSE
      ),
      modern = list(
        box_col   = if (is.null(fill_colour)) unname(cpb_cols(5)) else fill_colour,
        whisk_lw  = 0.55,
        med_col   = unname(cpb_cols(6)), med_lw = 1.3, med_ext = 0.2,
        med_lab_col = unname(cpb_cols(6)), med_lab_face = "bold", med_lab_size = 2.6,
        q_labels  = TRUE, q_lab_col = "#00a5ff", q_lab_size = 2.2
      )
    )
    fmt <- label_number_nl(accuracy = label_accuracy)

    # plain whiskers: capless (width = 0) segments p5-p25 and p75-p95;
    # then the borderless box (colour = NA also hides the boxplot's own
    # median line, which is drawn separately so it can extend past the
    # box). With per-row colours the colour/fill ride along as I()
    # (asis) aesthetics.
    whisk_lo <- ggplot2::aes(x = !!x, ymin = !!p5, ymax = !!p25)
    whisk_hi <- ggplot2::aes(x = !!x, ymin = !!p75, ymax = !!p95)
    whisk_args <- list(width = 0, linewidth = sty$whisk_lw)
    box_args2 <- list(mapping = mapping_box, stat = "identity",
                      width = width, colour = NA, key_glyph = "rect")
    if (is.null(row_cols)) {
      whisk_args$colour <- sty$box_col
      box_args2$fill <- sty$box_col
    } else {
      col_aes <- ggplot2::aes(colour = I(.data[["cpb__boxcol"]]))
      whisk_lo <- utils::modifyList(whisk_lo, col_aes)
      whisk_hi <- utils::modifyList(whisk_hi, col_aes)
      box_args2$mapping <- utils::modifyList(
        mapping_box, ggplot2::aes(fill = I(.data[["cpb__boxcol"]]))
      )
    }

    p <- p +
      do.call(ggplot2::geom_errorbar,
              c(list(mapping = whisk_lo), whisk_args, list(...))) +
      do.call(ggplot2::geom_errorbar,
              c(list(mapping = whisk_hi), whisk_args, list(...))) +
      do.call(ggplot2::geom_boxplot, c(box_args2, list(...))) +
      # the median: a zero-span errorbar whose cap IS the median line,
      # slightly wider than the box
      ggplot2::geom_errorbar(ggplot2::aes(x = !!x, ymin = !!p50, ymax = !!p50),
                             width = width * (1 + 2 * sty$med_ext),
                             linewidth = sty$med_lw, colour = sty$med_col, ...)

    if (isTRUE(box_labels)) {
      # labels are offset along the category axis: the median value
      # above the box, the quartile values (modern) below it
      p <- p + ggplot2::geom_text(
        ggplot2::aes(x = !!x, y = !!p50, label = fmt(!!p50)),
        nudge_x = width * 0.95, size = sty$med_lab_size,
        colour = sty$med_lab_col, fontface = sty$med_lab_face,
        family = cpb_font_family()
      )
      if (isTRUE(sty$q_labels)) {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(x = !!x, y = !!p25, label = fmt(!!p25)),
            nudge_x = -width * 0.85, hjust = 0.8, size = sty$q_lab_size,
            colour = sty$q_lab_col, family = cpb_font_family()
          ) +
          ggplot2::geom_text(
            ggplot2::aes(x = !!x, y = !!p75, label = fmt(!!p75)),
            nudge_x = -width * 0.85, hjust = 0.2, size = sty$q_lab_size,
            colour = sty$q_lab_col, family = cpb_font_family()
          )
      }
    }
  }

  # the grouped layout draws its bold headings outside the panel, so
  # clipping is turned off for that case
  clip <- if (has_group) "off" else "on"
  if (orientation == "horizontal") {
    p <- p + if (!is.null(value_limits)) {
      ggplot2::coord_flip(ylim = value_limits, clip = clip)
    } else {
      ggplot2::coord_flip(clip = clip)
    }
  } else if (!is.null(value_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = value_limits, clip = clip)
  } else if (has_group) {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }

  # no zero-flush expansion: boxes do not grow from the axis
  scale_args <- cpb_value_scale_args(pct_axis = pct_axis,
                                     value_breaks = value_breaks)
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  if (has_group) {
    # only the plain category rows are axis breaks, so the house
    # category ticks land on them (and not on the bold group-heading
    # rows). The heading names are drawn separately as bold text.
    cat_rows <- slots[!slots$heading, , drop = FALSE]
    head_rows <- slots[slots$heading, , drop = FALSE]
    p <- p + ggplot2::scale_x_continuous(
      breaks = cat_rows$pos,
      labels = cat_rows$label,
      # keep the heading-only rows inside the panel range
      limits = range(slots$pos) + c(-0.9, 0.9),
      expand = ggplot2::expansion(add = 0)
    )
    # the bold headings are drawn as text on the axis side (no tick),
    # right-aligned like the category labels; for horizontal boxes they
    # sit at the value-axis minimum (the left edge after coord_flip),
    # for vertical boxes just below the category labels
    if (nrow(head_rows)) {
      p <- p + if (orientation == "horizontal") {
        ggplot2::annotate("text", x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 1.03, vjust = 0.5,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family())
      } else {
        ggplot2::annotate("text", x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 0.5, vjust = 2.6,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family())
      }
    }
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

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB convention for vertical charts: the value-axis label doubles as
  # the subtitle; horizontally the value axis is drawn at the bottom
  # after coord_flip(), where a real axis title is appropriate. A
  # titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (orientation == "vertical" && is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()
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
#' @param forecast_x Optional x value where the forecast window
#'   starts; overlaid and labelled as in [cpb_line()].
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param reverse_legend If `TRUE`, reverse the colour legend order
#'   via `guide_legend(reverse = TRUE)` (discrete `colour` only).
#'   Defaults to `FALSE`.
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
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
                         forecast_x = NULL,
                         forecast_label = "raming",
                         reverse_legend = FALSE,
                         facet = NULL,
                         facet_ncol = NULL,
                         facet_scales = "fixed",
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
  facet <- rlang::enquo(facet)
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

  # underneath the points: first the forecast window, then the zero line
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(forecast_x)
  }
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  p <- p + if (has_colour) {
    ggplot2::geom_point(size = size, ...)
  } else {
    single_colour <- if (is.null(point_colour)) unname(cpb_cols(6)) else point_colour
    ggplot2::geom_point(size = size, colour = single_colour, ...)
  }

  # the label sits on top of everything
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(forecast_x, rlang::eval_tidy(x, data), forecast_label)
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
    if (!is.numeric(colvals) && isTRUE(reverse_legend)) {
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
    }
  }

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB convention: the value-axis label doubles as the subtitle. A
  # titled figure always reserves the subtitle line for a stable gap.
  lab_y <- ylab
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, colour = colourlab) +
    cpb_wrapper_theme()
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
#' @param facet Optional column (tidy eval) to facet by. Facets follow
#'   the house (legacy nicerplot) convention: the facet title is a bold
#'   strip *below* each panel, and every panel is a complete
#'   mini-figure with its own axes and axis labels.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()] (`"fixed"` default, or `"free"`,
#'   `"free_x"`, `"free_y"`).
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
                      facet = NULL,
                      facet_ncol = NULL,
                      facet_scales = "fixed",
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
  facet <- rlang::enquo(facet)
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

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB convention: the count-axis label doubles as the subtitle. A
  # titled figure always reserves the subtitle line for a stable gap.
  if (is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    ylab <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab, fill = filllab) +
    cpb_wrapper_theme()
}
