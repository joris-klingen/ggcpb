# boxplot_extended.R ----
#
# cpb_boxplot_extended(): a fixed CPB "extended" box-plot look (a
# light blue panel, white major gridlines, a bold value-axis line on
# the panel edge it is drawn against, and a centred-or-full-width
# title depending on whether the figure is faceted) wrapped around the
# ordinary cpb_box(), so a caller fills it in exactly like cpb_box()
# itself but never has to reach for a manual theme() call, or the
# gtable/layer surgery a faceted title and a background zero-line
# otherwise need, to get that look.

#' A CPB box-plot with the "extended" published look baked in
#'
#' Everything about [cpb_box()] -- same arguments, same meaning, same
#' data shape -- with a fixed set of visual choices layered on top
#' that would otherwise need a manual `theme()` call repeated on every
#' figure: a light blue panel background, white major gridlines
#' instead of black, no axis line except a bold one on the value
#' axis's own edge, and a title that spans the full figure width when
#' faceted but sits centred over the panel when it is not (matching
#' the two ways these figures are actually published: one box per
#' category, or several years/panels side by side as facets).
#'
#' A handful of [cpb_box()]'s own defaults are changed to match, since
#' this look was built around them: `box_style = "modern"`,
#' `orientation = "horizontal"`, `value_axis = "top"`, `width = 0.45`,
#' `grid_colour = "white"`, `ticks = FALSE`, and `zeroline = FALSE`
#' (the zero reference is instead drawn as a thick white line under
#' every layer, see `zero_indicator` below, which reads better than a
#' black one against the light blue panel). Every one of them can
#' still be overridden like any other argument.
#'
#' `orientation = "vertical"` is supported, mapped the same way, but
#' only `"horizontal"` (the default, and the only orientation the
#' published figures this look comes from actually use) has been
#' visually checked.
#'
#' @inheritParams cpb_box
#' @param box_style One of `"modern"` (default here), `"ggcpb"`,
#'   `"james"`, or `"dot"`; see [cpb_box()].
#' @param value_axis Where the value axis is drawn, `"top"` or
#'   `"bottom"`; see [cpb_box()]. `NULL` (default) resolves to `"top"`
#'   normally, matching the published look this function is built
#'   around, or `"bottom"` when `sec_y` is given, since [cpb_box()]
#'   does not allow `sec_y` together with `value_axis = "top"` (both
#'   would claim the same edge of the panel) -- resolved here so that
#'   detail does not have to be worked around by hand every time.
#' @param orientation `"horizontal"` (default here) or `"vertical"`;
#'   see [cpb_box()].
#' @param grid_colour Major gridline colour; defaults to `"white"`
#'   here (CPB legends show plain colour squares, not miniature
#'   boxplots) to read against `panel_fill`'s light background instead
#'   of the usual white one.
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_linewidth
#'   Forwarded to [theme_cpb()] for per-figure deviations from the
#'   house defaults -- documented here rather than left to
#'   `@inheritParams cpb_box` above, since that tag shares a single
#'   combined `@param` entry with `grid_colour` in [cpb_box()]'s own
#'   docs, and `grid_colour` is documented separately just above with
#'   a different default; inheriting only part of a combined entry
#'   isn't something roxygen2 does, so the rest of that entry needs
#'   repeating here instead of silently going undocumented.
#' @param panel_fill Panel background colour. Defaults to `"#eef8ff"`,
#'   the light blue this look is built around.
#' @param value_axis_linewidth Line width of the bold value-axis edge
#'   (`axis.line.x.top`/`axis.line.x.bottom` when `orientation` is
#'   `"horizontal"`, `axis.line.y` when `"vertical"`); defaults to
#'   `0.7`.
#' @param zero_indicator If `TRUE` (default), draw a thick white
#'   reference line at zero underneath every other layer, standing in
#'   for [cpb_box()]'s own black `zeroline` (forced off here, see
#'   `zeroline` below) -- a black line reads poorly against
#'   `panel_fill`'s light background, and this is drawn first so nothing
#'   else is covered by it.
#' @param zero_indicator_linewidth Line width of the `zero_indicator`
#'   line; defaults to `2`. [cpb_box()]'s own `zeroline` is not
#'   exposed here at all (always off): it draws a black line, which
#'   reads poorly against `panel_fill`'s light background, and the two
#'   would otherwise draw one on top of the other.
#' @param ylab_position Where the title (and, tied to it, the `ylab`
#'   caption -- ggplot2 has no way to place them separately, since both
#'   read off the same `plot.title.position` anchor) are placed:
#'   `"left"`, flush with the full figure width, matching the plain
#'   [cpb_box()]/[cpb_col()] house convention, or `"middle"`, centred
#'   over just the panel, matching the boxed-in look `panel_fill`
#'   gives this style. `NULL` (default) picks `"left"` when `facet` is
#'   set and `"middle"` otherwise, matching every reference figure this
#'   look has been checked against; pass one explicitly to override
#'   that for a single figure.
#' @param ... Further arguments passed to both [ggplot2::geom_errorbar()]
#'   and [ggplot2::geom_boxplot()], as in [cpb_box()].
#' @return A `ggplot` object.
#' @examples
#' df <- data.frame(
#'   groep = c("laag inkomen", "midden inkomen", "hoog inkomen"),
#'   p5 = c(-8, -6, -4), p25 = c(-4, -3, -2), p50 = c(-2, -1, 0),
#'   p75 = c(0, 1, 2), p95 = c(3, 4, 5)
#' )
#' cpb_boxplot_extended(df, x = groep, p5 = p5, p25 = p25, p50 = p50,
#'   p75 = p75, p95 = p95, ylab = "%-punt verandering")
#' @export
cpb_boxplot_extended <- function(data, x, p5, p25, p50, p75, p95,
                                  mean = NULL,
                                  fill = NULL,
                                  fill_colour = NULL,
                                  group = NULL,
                                  group_gap = 0.7,
                                  box_style = c("modern", "ggcpb", "james", "dot"),
                                  dot_labels = NULL,
                                  box_labels = NULL,
                                  label_accuracy = 0.1,
                                  width = 0.45,
                                  linewidth = 0.25,
                                  palette = "qualitative",
                                  fill_index = NULL,
                                  index = NULL,
                                  pct_axis = FALSE,
                                  value_accuracy = NULL,
                                  value_breaks = NULL,
                                  value_limits = NULL,
                                  value_axis = NULL,
                                  x_lim = NULL,
                                  x_lim_follow_data = TRUE,
                                  orientation = c("horizontal", "vertical"),
                                  sec_y = NULL,
                                  sec_type = c("line", "point", "col"),
                                  sec_limits = NULL,
                                  sec_label = NULL,
                                  sec_ylab = NULL,
                                  sec_colour = NULL,
                                  sec_linewidth = 0.55,
                                  sec_points = FALSE,
                                  sec_point_size = 1.6,
                                  sec_col_width = 0.3,
                                  sec_accuracy = NULL,
                                  facet = NULL,
                                  facet_ncol = NULL,
                                  facet_scales = "fixed",
                                  legend = "bottom",
                                  reverse_legend = FALSE,
                                  legend_ncol = NULL,
                                  minor = FALSE,
                                  ticks = FALSE,
                                  flush_legend = TRUE,
                                  axis_text_size = 7,
                                  legend_key_size = NULL,
                                  grid_colour = "white",
                                  grid_linewidth = 0.8,
                                  title = NULL,
                                  subtitle = NULL,
                                  xlab = NULL,
                                  ylab = NULL,
                                  filllab = NULL,
                                  panel_fill = "#eef8ff",
                                  value_axis_linewidth = 0.7,
                                  zero_indicator = TRUE,
                                  zero_indicator_linewidth = 2,
                                  ylab_position = NULL,
                                  ...) {
  box_style <- match.arg(box_style)
  orientation <- match.arg(orientation)
  has_facet <- !rlang::quo_is_null(rlang::enquo(facet))
  has_sec <- !rlang::quo_is_null(rlang::enquo(sec_y))
  if (is.null(value_axis)) {
    value_axis <- if (has_sec) "bottom" else "top"
  } else {
    value_axis <- match.arg(value_axis, c("top", "bottom"))
  }
  if (is.null(ylab_position)) {
    # matches the look every current reference figure was checked
    # against: a faceted figure keeps the flush-left full-width title,
    # a single-panel one gets the centred-over-panel title
    ylab_position <- if (has_facet) "left" else "middle"
  } else {
    ylab_position <- match.arg(ylab_position, c("left", "middle"))
  }

  p <- cpb_box(data,
    x = {{ x }}, p5 = {{ p5 }}, p25 = {{ p25 }}, p50 = {{ p50 }},
    p75 = {{ p75 }}, p95 = {{ p95 }}, mean = {{ mean }},
    fill = {{ fill }}, fill_colour = fill_colour,
    group = {{ group }}, group_gap = group_gap,
    box_style = box_style, dot_labels = dot_labels, box_labels = box_labels,
    label_accuracy = label_accuracy, width = width, linewidth = linewidth,
    palette = palette, fill_index = fill_index, index = index,
    pct_axis = pct_axis, value_accuracy = value_accuracy,
    value_breaks = value_breaks, value_limits = value_limits,
    value_axis = value_axis, x_lim = x_lim,
    x_lim_follow_data = x_lim_follow_data, orientation = orientation,
    sec_y = {{ sec_y }}, sec_type = sec_type, sec_limits = sec_limits,
    sec_label = sec_label, sec_ylab = sec_ylab, sec_colour = sec_colour,
    sec_linewidth = sec_linewidth, sec_points = sec_points,
    sec_point_size = sec_point_size, sec_col_width = sec_col_width,
    sec_accuracy = sec_accuracy,
    facet = {{ facet }}, facet_ncol = facet_ncol, facet_scales = facet_scales,
    legend = legend, reverse_legend = reverse_legend, legend_ncol = legend_ncol,
    # cpb_box()'s own zeroline is always off: zero_indicator below
    # replaces it, see the `zero_indicator_linewidth` @param for why
    zeroline = FALSE,
    minor = minor, ticks = ticks, flush_legend = flush_legend,
    axis_text_size = axis_text_size, legend_key_size = legend_key_size,
    grid_colour = grid_colour, grid_linewidth = grid_linewidth,
    title = title, subtitle = subtitle, xlab = xlab, ylab = ylab,
    filllab = filllab, ...
  )

  # the value axis is x post-coord_flip() when horizontal, y when not
  # -- so is the axis the bold "value_axis = ..." edge and the major
  # gridlines belong to; the category axis gets neither, since a bold
  # line/gridlines there would compete with the category labels
  # instead of the data
  if (orientation == "horizontal") {
    value_axis_line <- if (value_axis == "top") {
      ggplot2::element_line(colour = "black", linewidth = value_axis_linewidth)
    } else {
      ggplot2::element_blank()
    }
    theme_extra <- ggplot2::theme(
      axis.line.x.top    = value_axis_line,
      axis.line.x.bottom = if (value_axis == "bottom") value_axis_line else ggplot2::element_blank(),
      axis.line.y        = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = grid_colour, linewidth = grid_linewidth),
      panel.grid.major.y = ggplot2::element_blank()
    )
  } else {
    # the vertical case has no published example to check this
    # against; mapped by the same left-right/top-bottom symmetry as
    # the horizontal case above, on a best-effort basis
    value_axis_line <- ggplot2::element_line(colour = "black", linewidth = value_axis_linewidth)
    theme_extra <- ggplot2::theme(
      axis.line.y         = value_axis_line,
      axis.line.x         = ggplot2::element_blank(),
      panel.grid.major.y  = ggplot2::element_line(colour = grid_colour, linewidth = grid_linewidth),
      panel.grid.major.x  = ggplot2::element_blank()
    )
  }

  p <- p + ggplot2::theme(
    panel.background    = ggplot2::element_rect(fill = panel_fill, colour = NA),
    plot.background      = ggplot2::element_rect(fill = "white", colour = NA),
    strip.text           = ggplot2::element_text(hjust = 0.5),
    strip.placement       = "outside"
  ) + theme_extra

  # ggplot2 has no separate `plot.subtitle.position`: title, subtitle
  # and caption always share the one `plot.title.position` anchor
  # (`"plot"`, flush with the full figure width, or `"panel"`, flush
  # with just the panel) -- so `ylab_position` moves the title and the
  # `ylab` caption together, not `ylab` alone. `"left"` keeps the house
  # full-width, left-aligned title (theme_cpb()'s own default -- kept
  # explicit here rather than relied on, so this look does not silently
  # change if that default ever does); `"middle"` centres the title
  # over just the panel, which reads better once the panel is visually
  # boxed in by panel_fill.
  p <- p + if (ylab_position == "left") {
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title          = ggplot2::element_text(hjust = 0)
    )
  } else {
    ggplot2::theme(
      plot.title.position = "panel",
      plot.title          = ggplot2::element_text(hjust = 0.5)
    )
  }

  if (has_facet) {
    # strip.position is a facet_wrap() construction argument, not a
    # theme setting -- cpb_add_facet() (see wrappers.R) always draws
    # it at the bottom (the legacy nicerplot convention every other
    # wrapper follows), so it is moved here instead of adding a
    # second, conflicting facet_wrap() layer just to change one of its
    # own arguments
    p$facet$params$strip.position <- "top"
  }

  if (isTRUE(zero_indicator)) {
    p$layers <- c(
      ggplot2::geom_hline(
        yintercept = 0, colour = "white", linewidth = zero_indicator_linewidth
      ),
      p$layers
    )
  }

  p
}
