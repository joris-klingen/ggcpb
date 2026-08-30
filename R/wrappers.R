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
    c(
      "legend", "minor", "ticks", "flush_legend", "axis_text_size",
      "legend_key_size", "grid_colour", "grid_linewidth"
    ),
    envir = env
  )
  args$orientation <- mget("orientation",
    envir = env,
    ifnotfound = list("vertical")
  )[[1]]
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
      out <- rbind(out, data.frame(
        label = g, cat = cts,
        heading = TRUE, pos = pos
      ))
      next
    }
    pos <- pos - 1
    out <- rbind(out, data.frame(
      label = g, cat = NA_character_,
      heading = TRUE, pos = pos
    ))
    for (ct in cts) {
      pos <- pos - 1
      out <- rbind(out, data.frame(
        label = ct, cat = ct,
        heading = FALSE, pos = pos
      ))
    }
  }
  out$pos <- out$pos - min(out$pos) + 1
  out
}

# Faceting in house style: the facet title is a bold strip *below*
# its panel (the legacy nicerplot placement) and every panel is a
# complete mini-figure with its own axes and axis labels.
cpb_add_facet <- function(p, facet, facet_ncol = NULL, facet_scales = "fixed") {
  if (rlang::quo_is_null(facet)) {
    return(p)
  }
  p + ggplot2::facet_wrap(ggplot2::vars(!!facet),
    ncol = facet_ncol,
    scales = facet_scales,
    strip.position = "bottom",
    axes = "all", axis.labels = "all"
  )
}

# reverse_legend and legend_ncol both configure the same guide_legend(),
# so they are resolved together: setting one through a wrapper argument
# must not silently drop the other (which a bare guides() call appended
# after the wrapper would do).
cpb_add_legend_guide <- function(p, aesthetic, reverse = FALSE, ncol = NULL) {
  if (!isTRUE(reverse) && is.null(ncol)) {
    return(p)
  }
  args <- list(ggplot2::guide_legend(reverse = isTRUE(reverse), ncol = ncol))
  names(args) <- aesthetic
  p + do.call(ggplot2::guides, args)
}

# geom_errorbar()'s default legend key is a bare line -- draw_key_path(),
# with no whiskers -- so a capped interval in the plot shows an
# uncapped line in the legend. Returns a key_glyph function that draws
# a capped bar matching the geom's own orientation instead, for use
# where an errorbar layer gets its own named legend key (box_style =
# "dot" does; the plain box styles map fill on the box, not the
# errorbar, so they are unaffected).
cpb_key_errorbar <- function(orientation = c("horizontal", "vertical")) {
  orientation <- match.arg(orientation)
  function(data, params, size) {
    colour <- if (!is.null(data$colour)) data$colour else "black"
    alpha <- if (!is.null(data$alpha)) data$alpha else NA
    colour <- scales::alpha(colour, alpha)
    linewidth <- if (!is.null(data$linewidth)) data$linewidth else 0.5
    linetype <- if (!is.null(data$linetype)) data$linetype else 1
    gp <- grid::gpar(
      col = colour, lwd = linewidth * ggplot2::.pt,
      lty = linetype, lineend = "butt"
    )
    if (orientation == "horizontal") {
      grid::grobTree(
        grid::segmentsGrob(0.15, 0.5, 0.85, 0.5, gp = gp),
        grid::segmentsGrob(0.15, 0.2, 0.15, 0.8, gp = gp),
        grid::segmentsGrob(0.85, 0.2, 0.85, 0.8, gp = gp)
      )
    } else {
      grid::grobTree(
        grid::segmentsGrob(0.5, 0.15, 0.5, 0.85, gp = gp),
        grid::segmentsGrob(0.2, 0.15, 0.8, 0.15, gp = gp),
        grid::segmentsGrob(0.2, 0.85, 0.8, 0.85, gp = gp)
      )
    }
  }
}

# a titled figure always reserves the subtitle line, so the gap between
# title and panel is stable whether or not a subtitle is set
cpb_reserve_subtitle <- function(title, subtitle) {
  if (!is.null(title) && is.null(subtitle)) " " else subtitle
}

# The discrete fill/colour scale every wrapper falls back to: an
# index-based manual palette when `index` is supplied, the ordinary
# discrete CPB palette otherwise. One source of truth for this choice
# so it can't drift apart between the fill wrappers and the colour
# ones.
cpb_discrete_scale <- function(aesthetic = c("fill", "colour"), index = NULL,
                               palette = "qualitative", labels = ggplot2::waiver()) {
  aesthetic <- match.arg(aesthetic)
  if (aesthetic == "fill") {
    if (!is.null(index)) {
      scale_fill_cpb_manual(index = index, palette = palette, labels = labels)
    } else {
      scale_fill_cpb_d(palette = palette, labels = labels)
    }
  } else {
    if (!is.null(index)) {
      scale_colour_cpb_manual(index = index, palette = palette, labels = labels)
    } else {
      scale_colour_cpb_d(palette = palette, labels = labels)
    }
  }
}

# x_lim_follow_data's flush: zero expansion on the x scale, whichever
# type it turns out to be. Scale-level, not coord's own expand = FALSE
# (which is blanket across both axes and would strip the value axis's
# own flush expansion too -- the exact bug fixed in cpb_line()
# earlier this session).
cpb_flush_expand_x <- function(p, x, data) {
  xvals <- rlang::eval_tidy(x, data)
  p + if (is.numeric(xvals)) {
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0))
  } else {
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = 0))
  }
}

# cpb_box() and cpb_dot() share the same orientation-aware coord +
# x_lim_follow_data handling byte for byte; kept as one function so
# the two can't drift apart when either one changes. clip is passed
# in rather than computed here because what drives it (box_labels,
# box_style) only exists on cpb_box()'s side.
cpb_apply_coord <- function(p, orientation, x_lim, value_limits, clip,
                            x, data, x_lim_follow_data, has_group) {
  if (orientation == "horizontal") {
    p <- p + if (!is.null(value_limits) || !is.null(x_lim)) {
      ggplot2::coord_flip(xlim = x_lim, ylim = value_limits, clip = clip)
    } else {
      ggplot2::coord_flip(clip = clip)
    }
  } else if (!is.null(value_limits) || !is.null(x_lim)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, ylim = value_limits, clip = clip)
  } else if (clip == "off") {
    # covers every other reason the caller asked for clip = "off"
    # (has_group, box_labels, box_style = "dot", has_sec, ...) even
    # when there's no x_lim/value_limits to otherwise trigger a
    # coord_cartesian() call at all
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }
  if (isTRUE(x_lim_follow_data) && is.null(x_lim) && !has_group) {
    p <- cpb_flush_expand_x(p, x, data)
  }
  p
}

# The house single-colour fallback used wherever a wrapper draws one
# flat colour in the absence of a fill/colour mapping: an explicit
# override if supplied, else the given CPB palette position. One
# source of truth for the "no mapping" default across every wrapper.
cpb_single_colour <- function(value, index = 6) {
  if (is.null(value)) unname(cpb_cols(index)) else value
}

# The house style bolds the zero line only when zero actually falls
# within the lo-hi span -- an unconditional hline at 0 would stretch
# an all-positive chart (e.g. an index series) down to zero. Used
# wherever `zeroline` defaults to auto-detecting from the data instead
# of a fixed TRUE/FALSE.
cpb_zeroline_auto <- function(lo, hi) {
  is.numeric(lo) && is.numeric(hi) &&
    min(lo, na.rm = TRUE) <= 0 && max(hi, na.rm = TRUE) >= 0
}

# ---- sec_y helpers ----
#
# These five functions are the shared machinery behind `sec_y`: a
# second series drawn on its own right-hand axis, in wrappers like
# cpb_col(). Every wrapper that supports sec_y calls all five in the
# same order, so they are kept here as one shared implementation
#
# The core idea: the secondary series has its own value range (say,
# 0-3), but it has to be drawn on the SAME panel as the primary bars,
# whose value range is different (say, 0-20). So every secondary value
# gets converted to a "primary axis position" before it is drawn --
# and the right-hand axis then shows the reverse conversion, so its
# labels read in the secondary series' own units again.

# Step 1: work out that conversion. Takes the secondary column's own
# values (to guess a sensible default range if the user didn't set
# `sec_limits`), plus the primary axis's own flush min/max (the exact
# range the panel is already being drawn to), and returns a small list
# describing the conversion. Also checks for the two ways this can go
# wrong: a non-numeric sec_y column, or a primary/secondary range with
# no actual width to map onto.
cpb_sec_map <- function(sec_vals, sec_limits, prim_min, prim_max) {
  if (!is.numeric(sec_vals)) {
    stop("`sec_y` must be a numeric column.", call. = FALSE)
  }
  if (is.null(sec_limits)) {
    sec_limits <- c(
      min(c(0, sec_vals), na.rm = TRUE),
      max(c(0, sec_vals), na.rm = TRUE)
    )
  }
  if (length(sec_limits) != 2 || !is.numeric(sec_limits) ||
    sec_limits[[2]] == sec_limits[[1]]) {
    stop("`sec_limits` must be a length-2 numeric vector spanning a ",
      "non-zero range.",
      call. = FALSE
    )
  }
  if (prim_max == prim_min) {
    stop("the primary value axis has no range for `sec_y` to map onto.",
      call. = FALSE
    )
  }
  list(
    prim_min = prim_min, prim_max = prim_max,
    sec_min = sec_limits[[1]], sec_max = sec_limits[[2]]
  )
}

# Step 2: given the conversion from step 1, turn one secondary value
# into the position it should actually be drawn at on the primary
# axis. This is what places the secondary line/points/bars.
cpb_sec_to_primary <- function(v, sec_map) {
  (v - sec_map$sec_min) / (sec_map$sec_max - sec_map$sec_min) *
    (sec_map$prim_max - sec_map$prim_min) + sec_map$prim_min
}

# Step 3: the right-hand axis itself. It needs to undo step 2's
# conversion, so that even though the secondary series is physically
# drawn using primary-axis positions, the axis labels next to it show
# the secondary series' real values -- always plain Dutch numbers,
# regardless of whether the primary axis is a percentage. sec_y is
# usually a different kind of quantity from the primary series (e.g.
# a price alongside a percentage share), so it must never silently
# inherit the primary axis's own pct_axis formatting.
#
# Left to compute its own breaks, ggplot2's sec_axis() would pick
# "nice" numbers independently in the secondary series' own units --
# which almost never line up with the primary axis's horizontal
# gridlines (and can even leave the top gridline without a right-hand
# label at all, if its own nicest break falls just short of the data
# max). Every CPB figure with two axes shares one set of gridlines, so
# the two axes' ticks must land at the same heights. The fix is to
# tell sec_axis() to use these exact `primary_breaks` -- but its
# `breaks` argument is read in the SECONDARY axis's own units, not the
# primary ones (it inverse-transforms whatever it's given back onto
# the panel to decide where to draw it), so each primary break first
# has to be run through this same `transform` to get the matching
# secondary-space number to hand it.
cpb_sec_axis <- function(sec_map, primary_breaks) {
  to_sec <- function(v) {
    (v - sec_map$prim_min) / (sec_map$prim_max - sec_map$prim_min) *
      (sec_map$sec_max - sec_map$sec_min) + sec_map$sec_min
  }
  ggplot2::sec_axis(
    transform = to_sec,
    breaks = to_sec(primary_breaks),
    labels = label_number_nl()
  )
}

# Step 4: actually draw the secondary series (as a line, points, or
# thin bars -- see `sec_type`) at the positions from step 2, and give
# it its own legend key. The key's label always gets " (rechteras)"
# ("right axis" in Dutch) appended, which is the CPB house convention
# for showing the reader which axis a legend entry belongs to.
cpb_sec_layer <- function(p, data, x, sec_vals, sec_map, sec_type,
                          sec_col, sec_lab, sec_linewidth, sec_points) {
  sec_df <- as.data.frame(data)
  sec_df[["cpb__sec"]] <- cpb_sec_to_primary(sec_vals, sec_map)
  sec_df <- sec_df[!duplicated(rlang::eval_tidy(x, data)), , drop = FALSE]

  if (sec_type == "col") {
    # drawn narrower than the primary bars (whose default width is
    # about 0.9), so the two don't just fully overlap each other
    p <- p + ggplot2::geom_col(
      data = sec_df,
      ggplot2::aes(x = !!x, y = .data[["cpb__sec"]], colour = sec_lab),
      fill = sec_col, width = 0.3
    )
  } else if (sec_type == "point") {
    p <- p + ggplot2::geom_point(
      data = sec_df,
      ggplot2::aes(x = !!x, y = .data[["cpb__sec"]], colour = sec_lab),
      size = 1.6
    )
  } else {
    p <- p + ggplot2::geom_line(
      data = sec_df,
      ggplot2::aes(
        x = !!x, y = .data[["cpb__sec"]], colour = sec_lab,
        group = 1
      ),
      linewidth = sec_linewidth
    )
    if (isTRUE(sec_points)) {
      p <- p + ggplot2::geom_point(
        data = sec_df,
        ggplot2::aes(x = !!x, y = .data[["cpb__sec"]], colour = sec_lab),
        size = 1.1
      )
    }
  }
  sec_values <- sec_col
  names(sec_values) <- sec_lab
  p + ggplot2::scale_colour_manual(
    values = sec_values, name = NULL,
    labels = function(b) paste0(b, " (rechteras)")
  )
}

# Step 5: the primary series needs the mirror-image treatment -- once
# there's a second axis on the chart, its own legend label(s) get
# " (linkeras)" ("left axis") appended too, so every entry in the
# legend says which axis it belongs to, not just sec_y's. When there
# is no secondary axis at all, this is a no-op (ggplot2::waiver()
# tells the scale "just use your normal default labels").
cpb_linkeras_labels <- function(has_sec) {
  if (isTRUE(has_sec)) function(b) paste0(b, " (linkeras)") else ggplot2::waiver()
}

# Scale args assembled once so pct labels, custom breaks, and flush
# limits land in a single scale_y_continuous() -- a second call would
# silently replace the first.
#
# Flush via pretty() breaks as explicit limits: first and last
# gridline land exactly on the axis edge. pretty() is used over
# extended_breaks() because it's guaranteed to cover its input range;
# extended_breaks() can silently drop data when used as limits.
# Caller-supplied value_breaks/value_limits always wins.
cpb_flush_scale_args <- function(axis_values, pct_axis = FALSE, pct_scale = 1,
                                 value_accuracy = NULL,
                                 value_breaks = NULL, value_limits = NULL) {
  if (isTRUE(pct_axis) && !is.null(value_accuracy)) {
    stop("`pct_axis` and `value_accuracy` cannot be combined: `pct_axis` ",
      "already controls the value-axis label format.",
      call. = FALSE
    )
  }
  args <- list()
  # CPB figures are Dutch-language throughout: the value axis uses a
  # decimal comma (and a point as thousands separator), never the
  # ggplot2 default "0.5". Only whole-number breaks hide the difference.
  args$labels <- if (isTRUE(pct_axis)) {
    label_pct_nl(scale = pct_scale)
  } else if (!is.null(value_accuracy)) {
    label_number_nl(accuracy = value_accuracy)
  } else {
    label_number_nl()
  }
  breaks_final <- if (!is.null(value_breaks)) {
    value_breaks
  } else if (!is.null(value_limits)) {
    # the forced limits are what the visible breaks must span, not the
    # raw data: pretty()'ing the full (possibly wider, possibly
    # narrower) data range can pick breaks that miss one or both of
    # the forced limits entirely -- e.g. a caller-supplied c(0, 40)
    # with data running 8-41 would otherwise get pretty(8, 41)'s own
    # breaks (5, 10, ..., 45), which never include the 0 the caller
    # explicitly asked the axis to start at
    pretty(value_limits)
  } else {
    pretty(range(axis_values, na.rm = TRUE))
  }
  args$breaks <- breaks_final
  args$limits <- if (!is.null(value_limits)) value_limits else range(breaks_final)
  args$expand <- ggplot2::expansion(mult = c(0, 0))
  args
}

# columns / bars ----

#' Forecast-window annotation layers
#'
#' The house convention for marking the forecast part of a time axis
#' (nicknamed the "raming" window): a translucent white rectangle from
#' `forecast_x` to the right panel edge, drawn *underneath* the data,
#' plus an italic grey label centred in the window at the top of the
#' panel, drawn on top. Split in two helpers so the wrappers can layer
#' them on either side of their geoms.
#'
#' Resolve `forecast_x` to a numeric position on the panel
#'
#' A discrete scale places category `i` at position `i`, so a category
#' name has to become its level index before any arithmetic. The window
#' then starts at that category's left edge rather than its centre, so
#' the shaded band covers the whole bar. Numeric x axes pass straight
#' through.
#' @noRd
cpb_forecast_pos <- function(forecast_x, xvals) {
  if (is.numeric(forecast_x)) return(forecast_x)
  levs <- if (is.factor(xvals)) levels(xvals) else sort(unique(as.character(xvals)))
  pos <- match(as.character(forecast_x), levs)
  if (is.na(pos)) {
    stop("`forecast_x` (\"", forecast_x, "\") is not one of the values on ",
         "the x axis.", call. = FALSE)
  }
  pos - 0.5
}

#' @noRd
cpb_forecast_rect <- function(forecast_x) {
  ggplot2::annotate("rect",
    xmin = forecast_x, xmax = Inf,
    ymin = -Inf, ymax = Inf, fill = "white", alpha = 0.45
  )
}

#' @noRd
cpb_forecast_label <- function(forecast_x, xvals, label) {
  if (is.null(label) || !nzchar(label)) {
    return(NULL)
  }
  x_max <- suppressWarnings(max(as.numeric(xvals), na.rm = TRUE))
  if (is.finite(x_max) && x_max > forecast_x) {
    # centred in the window, as the legacy plotter does
    label_x <- (forecast_x + x_max) / 2
    hjust <- 0.5
  } else {
    label_x <- forecast_x
    hjust <- -0.15
  }
  ggplot2::annotate("text",
    x = label_x, y = Inf, label = label,
    vjust = 1.8, hjust = hjust, size = 2.2,
    colour = "#666666", family = cpb_font_family(),
    fontface = "italic"
  )
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
#'   eval); if omitted, bars are drawn in a single colour (`fill_colour`)
#'   with no legend of its own -- unless `sec_y` is also set, in which
#'   case the bars still get a one-level legend key (named after `y`)
#'   so both series are always represented, not just `sec_y`. Whenever
#'   `sec_y` is set, every fill legend label (real or the one-level
#'   fallback) is suffixed `"(linkeras)"` (left axis) to tell it apart
#'   from `sec_y`'s own `"(rechteras)"` (right axis) key -- house
#'   convention, applied automatically; do not add it to `fill`'s own
#'   factor levels yourself.
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
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, the house
#'   combination chart (e.g. a stacked wealth total in billions with
#'   the tax raised on it, an order of magnitude smaller, alongside).
#'   One value per `x`. Only supported for vertical, ungrouped columns.
#' @param sec_type How `sec_y` is drawn: `"line"` (default), `"point"`
#'   (markers only, no connecting line), or `"col"` (thin bars,
#'   narrower than the primary columns so the two stay visually
#'   distinct). All three read off the same secondary axis and share
#'   one legend key with the primary fill.
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses zero to the maximum of
#'   `sec_y`. `sec_y` is placed by mapping this range linearly onto
#'   the primary range, so the two axes always start together.
#' @param sec_label Legend label for `sec_y`. `NULL` (default) uses
#'   the `sec_y` column name. Automatically suffixed `"(rechteras)"`
#'   (right axis) -- don't add it yourself, e.g. `sec_label =
#'   "erfbelasting"` shows as `"erfbelasting (rechteras)"`.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_colour Colour for `sec_y`; defaults to `NULL`, which
#'   resolves to the CPB pink (`cpb_cols(2)`, `"#e6006e"`) that sets it
#'   apart from the blue-led column fills.
#' @param sec_linewidth Line width; only used when `sec_type = "line"`.
#'   Defaults to `0.55`, as in [cpb_line()].
#' @param sec_points If `TRUE`, add a marker at every point of the
#'   `sec_y` line. Only used when `sec_type = "line"` -- for markers
#'   without a connecting line, use `sec_type = "point"` instead.
#' @param value_limits Optional length-2 numeric vector giving the
#'   value-axis range (the `y` axis, or the flipped axis when
#'   `orientation = "horizontal"`). Applied as the wrapper-built value
#'   scale's own `limits` (not a coordinate-system zoom), so this is
#'   the hard bound the axis is drawn flush to; a bar/segment that
#'   falls outside it is genuinely dropped, with a warning, the same
#'   as setting `limits` on any ggplot2 scale. `NULL` (default) flushes
#'   to the full data range instead (see `x_lim`/`x_lim_follow_data`
#'   for the category axis's equivalent).
#' @param x_lim Optional length-2 vector zooming the category (`x`)
#'   axis to a range, without dropping data -- applied as a
#'   coordinate-system zoom ([ggplot2::coord_cartesian()] /
#'   [ggplot2::coord_flip()] `xlim`), so a bar just outside the window
#'   still contributes to breaks/totals but is only clipped for
#'   display. `NULL` (default) shows the full range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the category axis so it sits flush to the data's
#'   actual range, at the cost of ggplot2 picking its own breaks
#'   within that (possibly non-round) range instead of the usual
#'   padded, evenly spaced ones. Matches nicerplot's parameter of the
#'   same name. Defaults to `FALSE`. Ignored when `x_lim` is set.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param fill_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_fill_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param index Deprecated. Former name of
#'   `fill_index`. Still accepted, with a warning.
#' @param pct_axis If `TRUE`, format the value axis with
#'   [label_pct_nl()]. Uses `scale = 100` automatically when
#'   `position = "fill"` (proportions), and `scale = 1` otherwise
#'   (values already in percentage points).
#' @param value_accuracy Rounding accuracy for the value axis labels,
#'   passed to [label_number_nl()] (e.g. `0.1` for one decimal place).
#'   `NULL` (default) lets `scales` pick a sensible accuracy from the
#'   breaks. Cannot be combined with `pct_axis`. Use this instead of
#'   adding a second `scale_y_continuous()`, which would discard the
#'   wrapper's flush axis (see `value_breaks`).
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
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
                     sec_y = NULL,
                     sec_type = c("line", "point", "col"),
                     sec_limits = NULL,
                     sec_label = NULL,
                     sec_ylab = NULL,
                     sec_colour = NULL,
                     sec_linewidth = 0.55,
                     sec_points = FALSE,
                     palette = "qualitative",
                     fill_index = NULL,
                     index = NULL,
                     pct_axis = FALSE,
                     value_accuracy = NULL,
                     value_breaks = NULL,
                     value_limits = NULL,
                     value_labels = FALSE,
                     x_lim = NULL,
                     x_lim_follow_data = FALSE,
                     forecast_x = NULL,
                     forecast_label = "raming",
                     reverse_legend = TRUE,
                     legend_ncol = NULL,
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
  .cpb_idx <- cpb_resolve_index(fill_index, index, palette, !missing(palette), "fill_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  position <- match.arg(position)
  orientation <- match.arg(orientation)
  sec_type <- match.arg(sec_type)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  sec_y <- rlang::enquo(sec_y)
  has_fill <- !rlang::quo_is_null(fill)
  has_group <- !rlang::quo_is_null(group)
  has_sec <- !rlang::quo_is_null(sec_y)

  if (has_sec) {
    if (orientation == "horizontal") {
      stop("`sec_y` is only supported for vertical column charts: the ",
        "secondary axis is drawn on the right of the value axis.",
        call. = FALSE
      )
    }
    if (has_group) {
      stop("`sec_y` and `group` cannot be combined: the bold group headings ",
        "and the secondary axis both claim the space beside the panel.",
        call. = FALSE
      )
    }
    if (position == "fill") {
      stop("`sec_y` cannot be combined with position = \"fill\": a ",
        "proportional value axis has no scale for a second series to ",
        "share.",
        call. = FALSE
      )
    }
  }

  # the value axis always spans the full drawn height of the bars, not
  # just the raw y values: a stacked bar's top is the per-category sum,
  # not any single segment, so the flush range (and the secondary-axis
  # mapping below) must be computed the same way the bars are drawn
  yvals <- rlang::eval_tidy(y, data)
  xvals_for_axis <- as.character(rlang::eval_tidy(x, data))
  axis_values <- if (position == "fill") {
    c(0, 1)
  } else if (position == "stack") {
    c(
      tapply(pmax(yvals, 0), xvals_for_axis, sum),
      tapply(pmin(yvals, 0), xvals_for_axis, sum), 0
    )
  } else {
    c(yvals, 0)
  }
  value_breaks_final <- if (!is.null(value_breaks)) {
    value_breaks
  } else {
    pretty(range(axis_values, na.rm = TRUE))
  }
  flush_ylim <- if (!is.null(value_limits)) {
    value_limits
  } else {
    range(value_breaks_final)
  }

  if (has_group) {
    # a two-level category axis: categories in gapped group blocks on
    # one shared value axis, group names in bold under the categories
    if (orientation == "horizontal") {
      stop("`group` is only supported for vertical column charts; for ",
        "horizontal grouped categories see the `group` argument of ",
        "cpb_box().",
        call. = FALSE
      )
    }
    if (!is.null(forecast_x)) {
      stop("`group` and `forecast_x` cannot be combined: the grouped ",
        "category axis is not a time axis.",
        call. = FALSE
      )
    }
    grp <- cpb_group_positions(rlang::eval_tidy(x, data),
      rlang::eval_tidy(group, data),
      gap = group_gap
    )
    data <- as.data.frame(data)
    data[["cpb__x"]] <- grp$map$pos[match(
      as.character(rlang::eval_tidy(x, data)),
      as.character(grp$map$cat)
    )]
    x <- rlang::quo(.data[["cpb__x"]])
  }

  # a secondary axis means two series share the panel: the primary bars
  # need their own legend key too, not just the secondary one, so
  # without a real fill mapping they get a one-level dummy fill (a
  # constant label, the same trick sec_y's own colour legend already
  # uses) instead of an unmapped constant that has no legend at all
  single_fill <- cpb_single_colour(fill_colour, 6)
  primary_lab <- rlang::as_label(y)

  if (has_fill) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, fill = !!fill)
  } else if (has_sec) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, fill = primary_lab)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }
  p <- ggplot2::ggplot(data, mapping)

  # the forecast window sits underneath the bars
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)))
  }

  p <- p + if (has_fill || has_sec) {
    # fill is aes-mapped in both cases (a real column, or the dummy
    # constant label above), so no literal fill= parameter here.
    # show.legend = TRUE forces a key even for a drop = FALSE level
    # with zero rows in this layer's data, which ggplot2 otherwise
    # blanks by default. key_glyph = "rect" is what geom_col() already
    # looks like by default (a plain colour square) -- it needs to be
    # set explicitly here because its *default* polygon key glyph is
    # what lets sec_y's point/line/col legend key bleed into the fill
    # legend's own square when the two guides are stacked (see the
    # "sec_y helpers" block near the top of this file)
    ggplot2::geom_col(position = position, show.legend = TRUE,
                      key_glyph = "rect", ...)
  } else {
    # No fill mapping and no secondary axis: draw one flat house-style
    # colour (CPB primary blue by default) rather than ggplot2's grey,
    # with no legend -- a single-series bar chart needs none.
    ggplot2::geom_col(position = position, fill = single_fill, ...)
  }

  # The secondary series is drawn on the primary scale and read off a
  # right-hand axis; see the "sec_y helpers" block near the top of
  # this file for how the two are kept in sync.
  sec_map <- NULL
  if (has_sec) {
    sec_vals <- rlang::eval_tidy(sec_y, data)
    # map onto the flush axis range computed above (the exact range
    # the panel is drawn to), not the raw data
    sec_map <- cpb_sec_map(sec_vals, sec_limits, flush_ylim[[1]], flush_ylim[[2]])
    sec_lab <- if (is.null(sec_label)) rlang::as_label(sec_y) else sec_label
    sec_col <- cpb_single_colour(sec_colour, 2)
    p <- cpb_sec_layer(p, data, x, sec_vals, sec_map, sec_type,
                       sec_col, sec_lab, sec_linewidth, sec_points)
  }

  # The zero line sits on the value axis (the y aesthetic even under
  # coord_flip()) and is drawn on top of the bars.
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)),
      rlang::eval_tidy(x, data), forecast_label)
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
      ggplot2::coord_cartesian(xlim = x_lim, ylim = value_limits, clip = "off")
  } else if (orientation == "horizontal") {
    p <- p + if (!is.null(value_limits) || !is.null(x_lim)) {
      ggplot2::coord_flip(xlim = x_lim, ylim = value_limits)
    } else {
      ggplot2::coord_flip()
    }
  } else if (has_sec) {
    # sec_y's own markers can legitimately sit exactly on the flush
    # axis boundary (its default range runs to the data's own max), so
    # a point or line there would otherwise be half-cropped by the
    # panel edge; the secondary unit caption, when present, is drawn
    # above the panel and needs the same
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, ylim = value_limits, clip = "off")
  } else if (!is.null(value_limits) || !is.null(x_lim)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, ylim = value_limits)
  }
  # x_lim_follow_data strips the category axis's default margin so it
  # sits flush to the data's own range -- a scale-level expand (not a
  # coord one, which would also strip the value axis's own expansion);
  # ignored for the grouped layout, which needs its own fixed margin
  # for the heading rows, and superseded by an explicit x_lim
  if (isTRUE(x_lim_follow_data) && is.null(x_lim) && !has_group) {
    p <- cpb_flush_expand_x(p, x, data)
  }

  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text",
      x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  scale_args <- cpb_flush_scale_args(
    axis_values  = axis_values,
    pct_axis     = pct_axis,
    pct_scale    = if (position == "fill") 100 else 1,
    value_accuracy = value_accuracy,
    value_breaks = value_breaks,
    value_limits = value_limits
  )
  if (has_sec) {
    scale_args$sec.axis <- cpb_sec_axis(sec_map, scale_args$breaks)
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
    p <- p + cpb_discrete_scale("fill", index, palette,
                                labels = cpb_linkeras_labels(has_sec))
    p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)
  } else if (has_sec) {
    p <- p + ggplot2::scale_fill_manual(
      values = stats::setNames(single_fill, primary_lab), name = NULL,
      labels = cpb_linkeras_labels(TRUE)
    )
    p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)
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

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = lab_x, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()

  # the fill keys and the line key are two guides; stack them into one
  # left-aligned block instead of letting them sit side by side, with
  # the columns named before the line, as in the published figures
  if (has_sec) {
    p <- p +
      ggplot2::guides(
        # override.aes: without this, ggplot2 sometimes carries the
        # sec_y point/line layer's own colour and point shape into the
        # fill guide's key background (a stray dot rendered on top of
        # a fill square) when the two guides are stacked like this --
        # telling the fill guide explicitly to ignore those aesthetics
        # is the documented ggplot2 fix for that
        fill = ggplot2::guide_legend(
          order = 1, reverse = isTRUE(reverse_legend),
          ncol = legend_ncol, override.aes = list(colour = NA, shape = NA)
        ),
        colour = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme(
        legend.box = "vertical",
        legend.box.just = "left"
      )
  }
  p
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
#'   the grouping variable being stacked. Whenever `sec_y` is set,
#'   every fill legend label is suffixed `"(linkeras)"` (left axis) to
#'   tell it apart from `sec_y`'s own `"(rechteras)"` (right axis) key
#'   -- house convention, applied automatically; do not add it to
#'   `fill`'s own factor levels yourself.
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, alongside the
#'   stacked areas. One value per `x`.
#' @param sec_type How `sec_y` is drawn: `"line"` (default), `"point"`
#'   (markers only, no connecting line), or `"col"` (thin bars). All
#'   three read off the same secondary axis and share one legend key
#'   with the primary fill.
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses zero to the maximum of
#'   `sec_y`. `sec_y` is placed by mapping this range linearly onto
#'   the primary range, so the two axes always start together.
#' @param sec_label Legend label for `sec_y`. `NULL` (default) uses
#'   the `sec_y` column name. Automatically suffixed `"(rechteras)"`
#'   (right axis) -- don't add it yourself, e.g. `sec_label =
#'   "erfbelasting"` shows as `"erfbelasting (rechteras)"`.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_colour Colour for `sec_y`; defaults to `NULL`, which
#'   resolves to the CPB pink (`cpb_cols(2)`, `"#e6006e"`) that sets it
#'   apart from the blue-led area fills.
#' @param sec_linewidth Line width; only used when `sec_type = "line"`.
#'   Defaults to `0.55`, as in [cpb_line()].
#' @param sec_points If `TRUE`, add a marker at every point of the
#'   `sec_y` line. Only used when `sec_type = "line"` -- for markers
#'   without a connecting line, use `sec_type = "point"` instead.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param fill_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_fill_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param index Deprecated. Former name of
#'   `fill_index`. Still accepted, with a warning.
#' @param pct_axis If `TRUE`, format the y axis with [label_pct_nl()].
#' @param value_accuracy Rounding accuracy for the value axis labels,
#'   passed to [label_number_nl()] (e.g. `0.1` for one decimal place).
#'   `NULL` (default) lets `scales` pick a sensible accuracy from the
#'   breaks. Cannot be combined with `pct_axis`. Use this instead of
#'   adding a second `scale_y_continuous()`, which would discard the
#'   wrapper's flush axis (see `value_breaks`).
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 limits for the value axis,
#'   applied through the coordinate system (zoom) so no data is
#'   dropped.
#' @param x_lim Optional length-2 vector zooming the `x` axis to a
#'   range, without dropping data -- applied as a coordinate-system
#'   zoom ([ggplot2::coord_cartesian()] `xlim`). `NULL` (default) shows
#'   the full range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the `x` axis so it sits flush to the data's actual
#'   range, at the cost of ggplot2 picking its own breaks within that
#'   (possibly non-round) range instead of the usual padded, evenly
#'   spaced ones. Matches nicerplot's parameter of the same name.
#'   Defaults to `FALSE`. Ignored when `x_lim` is set.
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = ...)`. `NULL` (default) keeps
#'   ggplot2's own single-row/column layout.
#' @param forecast_x Optional x value where the forecast window
#'   starts; overlaid and labelled as in [cpb_line()].
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
                      sec_y = NULL,
                      sec_type = c("line", "point", "col"),
                      sec_limits = NULL,
                      sec_label = NULL,
                      sec_ylab = NULL,
                      sec_colour = NULL,
                      sec_linewidth = 0.55,
                      sec_points = FALSE,
                      palette = "qualitative",
                      fill_index = NULL,
                      index = NULL,
                      pct_axis = FALSE,
                      value_accuracy = NULL,
                      value_breaks = NULL,
                      value_limits = NULL,
                      x_lim = NULL,
                      x_lim_follow_data = FALSE,
                      forecast_x = NULL,
                      forecast_label = "raming",
                      reverse_legend = TRUE,
                      legend_ncol = NULL,
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
  .cpb_idx <- cpb_resolve_index(fill_index, index, palette, !missing(palette), "fill_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  sec_type <- match.arg(sec_type)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  fill <- rlang::enquo(fill)
  facet <- rlang::enquo(facet)
  sec_y <- rlang::enquo(sec_y)
  has_sec <- !rlang::quo_is_null(sec_y)

  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y, fill = !!fill))

  # the forecast window sits underneath the areas
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)))
  }

  # key_glyph = "rect": a plain colour square, CPB house style (and
  # geom_area()'s own default polygon key glyph is what lets sec_y's
  # point/line/col legend key bleed into the fill legend's own square
  # when the two guides are stacked -- see the "sec_y helpers" block
  # near the top of this file). show.legend = TRUE forces a key even
  # for a drop = FALSE level with zero rows in this layer's data.
  p <- p + ggplot2::geom_area(show.legend = TRUE, key_glyph = "rect", ...)

  # on top of the areas
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)),
      rlang::eval_tidy(x, data), forecast_label)
  }

  # geom_area() stacks by default, so the axis must span the per-x
  # total across fill levels, not any single series' raw y values
  yvals <- rlang::eval_tidy(y, data)
  xvals_for_axis <- as.character(rlang::eval_tidy(x, data))
  axis_values <- c(
    tapply(pmax(yvals, 0), xvals_for_axis, sum),
    tapply(pmin(yvals, 0), xvals_for_axis, sum), 0
  )
  scale_args <- cpb_flush_scale_args(
    axis_values  = axis_values,
    pct_axis     = pct_axis,
    value_accuracy = value_accuracy,
    value_breaks = value_breaks,
    value_limits = value_limits
  )

  # sec_y is drawn on top of the areas, mapped onto this same flush
  # range, and read off its own axis on the right; see the "sec_y
  # helpers" block near the top of this file for how that mapping and
  # its axis labels are kept in sync with each other.
  if (has_sec) {
    sec_vals <- rlang::eval_tidy(sec_y, data)
    sec_map <- cpb_sec_map(sec_vals, sec_limits, scale_args$limits[[1]], scale_args$limits[[2]])
    sec_lab <- if (is.null(sec_label)) rlang::as_label(sec_y) else sec_label
    sec_col <- cpb_single_colour(sec_colour, 2)
    p <- cpb_sec_layer(p, data, x, sec_vals, sec_map, sec_type,
                       sec_col, sec_lab, sec_linewidth, sec_points)
    scale_args$sec.axis <- cpb_sec_axis(sec_map, scale_args$breaks)
  }
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }
  # sec_y's own markers can legitimately sit exactly on the flush axis
  # boundary (its default range runs to the data's own max), so a
  # point or line there would otherwise be half-cropped by the panel
  # edge; the secondary unit caption, when present, is drawn above the
  # panel and needs the same
  clip <- if (has_sec) "off" else "on"
  if (!is.null(value_limits) || !is.null(x_lim) || clip == "off") {
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, ylim = value_limits, clip = clip)
  }
  if (isTRUE(x_lim_follow_data) && is.null(x_lim)) {
    p <- cpb_flush_expand_x(p, x, data)
  }

  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  p <- p + cpb_discrete_scale("fill", index, palette,
                              labels = cpb_linkeras_labels(has_sec))

  p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)

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

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()

  # the fill keys and sec_y's own key are two separate guides; stack
  # them into one left-aligned block instead of letting them sit side
  # by side, fill named first, as in the published figures
  if (has_sec) {
    p <- p +
      ggplot2::guides(
        # override.aes: without this, ggplot2 sometimes carries the
        # sec_y point/line layer's own colour and point shape into the
        # fill guide's key background (a stray dot rendered on top of
        # a fill square) when the two guides are stacked like this --
        # telling the fill guide explicitly to ignore those aesthetics
        # is the documented ggplot2 fix for that
        fill = ggplot2::guide_legend(order = 1, reverse = isTRUE(reverse_legend),
                                     ncol = legend_ncol,
                                     override.aes = list(colour = NA, shape = NA)),
        colour = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme(legend.box = "vertical", legend.box.just = "left")
  }
  p
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
#'   Cannot be combined with `sec_y`: both would need the colour
#'   aesthetic and its one legend, so `sec_y` is only for an otherwise
#'   single, unmapped line.
#' @param line_colour Constant line colour used when no `colour` column
#'   is mapped. Defaults to `NULL`, which resolves to the CPB primary
#'   blue (`cpb_cols(6)`, `"#005faf"`). Ignored when `colour` is
#'   supplied.
#' @param linewidth Line width; defaults to `0.55`, matching the
#'   published CPB figures.
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, the house
#'   dual-axis line chart (e.g. two rates in % on the left and an index
#'   on the right). One value per `x`. Unlike [cpb_col()], where the
#'   columns key on `fill` and the secondary line on `colour`, here the
#'   primary series already key on `colour`: the secondary line joins
#'   that same scale and takes the next palette position, so all series
#'   share one legend block. House style names the axis in each label,
#'   e.g. `"inflatie (linkeras)"` and `"reeel loon (rechteras)"`.
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses the range of `sec_y`.
#'   The line is placed by mapping this range linearly onto the primary
#'   range, so the two axes always start together.
#' @param sec_label Legend label for the secondary line. `NULL`
#'   (default) uses the `sec_y` column name.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_linewidth Line width for `sec_y`; `NULL` (default) uses
#'   `linewidth`, so the secondary line matches the primary ones.
#' @param points If `TRUE`, draw a point marker at every observation on
#'   top of the line, the house variant used when the x axis holds a
#'   handful of discrete categories (age brackets, quintiles) rather
#'   than a dense time series. Markers take the line colour, and the
#'   legend key shows a line with a marker on it.
#'   Markers have a radius, so drawing them expands the panel slightly
#'   beyond the data range -- otherwise the first, last and extreme
#'   points are sliced in half by the panel edge. A line on its own
#'   keeps the tight panel, where the axis meets the outermost
#'   gridlines.
#' @param point_size Marker size when `points = TRUE`; defaults to
#'   `1.1`, which reads as the published dot against the default
#'   `linewidth`.
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, alongside the
#'   line. One value per `x`. Requires `colour` to be unset (see
#'   above); the primary line stays a plain, unlabelled line in
#'   `line_colour`, and only `sec_y` gets a legend key.
#' @param sec_type How `sec_y` is drawn: `"line"` (default), `"point"`
#'   (markers only, no connecting line), or `"col"` (thin bars).
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses zero to the maximum of
#'   `sec_y`. `sec_y` is placed by mapping this range linearly onto
#'   the primary range, so the two axes always start together.
#' @param sec_label Legend label for `sec_y`. `NULL` (default) uses
#'   the `sec_y` column name. Automatically suffixed `"(rechteras)"`
#'   (right axis) -- don't add it yourself, e.g. `sec_label =
#'   "erfbelasting"` shows as `"erfbelasting (rechteras)"`.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_colour Colour for `sec_y`; defaults to `NULL`, which
#'   resolves to the CPB pink (`cpb_cols(2)`, `"#e6006e"`).
#' @param sec_linewidth Line width; only used when `sec_type = "line"`.
#'   Defaults to `0.55`.
#' @param sec_points If `TRUE`, add a marker at every point of the
#'   `sec_y` line. Only used when `sec_type = "line"` -- for markers
#'   without a connecting line, use `sec_type = "point"` instead.
#' @param palette CPB palette to use for `colour`; one of
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param colour_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_colour_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param color_index American-spelling alias for `colour_index`; ignored
#'   when `colour_index` is given.
#' @param index Deprecated. Former name of
#'   `colour_index`. Still accepted, with a warning.
#' @param pct_axis If `TRUE`, format the y axis with [label_pct_nl()].
#' @param value_accuracy Rounding accuracy for the value axis labels,
#'   passed to [label_number_nl()] (e.g. `0.1` for one decimal place).
#'   `NULL` (default) lets `scales` pick a sensible accuracy from the
#'   breaks. Cannot be combined with `pct_axis`. Use this instead of
#'   adding a second `scale_y_continuous()`, which would discard the
#'   wrapper's flush axis (see `value_breaks`).
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 numeric vector giving the
#'   value-axis range, applied as the wrapper-built value scale's own
#'   `limits` (not a coordinate-system zoom) -- the hard bound the axis
#'   is drawn flush to; a point outside it is genuinely dropped, with a
#'   warning, the same as setting `limits` on any ggplot2 scale. `NULL`
#'   (default) flushes to the full data range instead.
#' @param x_lim Optional length-2 vector zooming the `x` axis to a
#'   range, without dropping data -- applied as a coordinate-system
#'   zoom ([ggplot2::coord_cartesian()] `xlim`). `NULL` (default) shows
#'   the full range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the `x` axis so it sits flush to the data's actual
#'   range, at the cost of ggplot2 picking its own breaks within that
#'   (possibly non-round) range instead of the usual padded, evenly
#'   spaced ones. Matches nicerplot's parameter of the same name.
#'   Defaults to `FALSE`. Ignored when `x_lim` is set.
#' @param reverse_legend If `TRUE`, reverse the colour legend order
#'   via `guide_legend(reverse = TRUE)`. Defaults to `FALSE`: unlike
#'   the stacked wrappers, line order carries no stacking convention.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = ...)`. `NULL` (default) keeps
#'   ggplot2's own single-row/column layout.
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
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
                      points = FALSE,
                      point_size = 1.1,
                      sec_y = NULL,
                      sec_type = c("line", "point", "col"),
                      sec_limits = NULL,
                      sec_label = NULL,
                      sec_ylab = NULL,
                      sec_colour = NULL,
                      sec_linewidth = NULL,
                      sec_points = FALSE,
                      palette = "qualitative",
                      colour_index = NULL,
                      color_index = NULL,
                      index = NULL,
                      pct_axis = FALSE,
                      value_accuracy = NULL,
                      value_breaks = NULL,
                      value_limits = NULL,
                      x_lim = NULL,
                      x_lim_follow_data = FALSE,
                      ymin = NULL,
                      ymax = NULL,
                      forecast_x = NULL,
                      forecast_label = "raming",
                      reverse_legend = FALSE,
                      legend_ncol = NULL,
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
  if (is.null(colour_index)) colour_index <- color_index
  .cpb_idx <- cpb_resolve_index(colour_index, index, palette, !missing(palette), "colour_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  sec_type <- match.arg(sec_type)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colour <- rlang::enquo(colour)
  ymin <- rlang::enquo(ymin)
  ymax <- rlang::enquo(ymax)
  sec_y <- rlang::enquo(sec_y)
  facet <- rlang::enquo(facet)
  has_colour <- !rlang::quo_is_null(colour)
  has_sec <- !rlang::quo_is_null(sec_y)
  has_band <- !rlang::quo_is_null(ymin) && !rlang::quo_is_null(ymax)

  if (is.null(zeroline)) {
    yvals <- rlang::eval_tidy(y, data)
    zeroline <- cpb_zeroline_auto(yvals, yvals)
  }

  # `group` is set explicitly rather than left to ggplot2, which infers
  # it from every discrete aesthetic: on a categorical x axis (age
  # brackets, quintiles) that puts each observation in a group of its
  # own and the lines disappear entirely. One series per colour level,
  # or a single series when no colour is mapped, is always what is
  # meant here.
  # the secondary series is placed by mapping its range linearly onto
  # the primary one, so the two axes always start together
  if (has_sec) {
    sec_vals <- rlang::eval_tidy(sec_y, data)
    if (!is.numeric(sec_vals)) {
      stop("`sec_y` must be a numeric column.", call. = FALSE)
    }
    prim_vals <- rlang::eval_tidy(y, data)
    prim_min <- min(prim_vals, na.rm = TRUE)
    prim_max <- max(prim_vals, na.rm = TRUE)
    if (!is.null(value_limits)) {
      prim_min <- value_limits[[1]]
      prim_max <- value_limits[[2]]
    }
    if (is.null(sec_limits)) {
      sec_limits <- c(min(sec_vals, na.rm = TRUE), max(sec_vals, na.rm = TRUE))
    }
    if (length(sec_limits) != 2 || !is.numeric(sec_limits) ||
        sec_limits[[2]] == sec_limits[[1]]) {
      stop("`sec_limits` must be a length-2 numeric vector spanning a ",
           "non-zero range.", call. = FALSE)
    }
    if (prim_max == prim_min) {
      stop("the primary value axis has no range for `sec_y` to map onto.",
           call. = FALSE)
    }
    sec_map <- list(prim_min = prim_min, prim_max = prim_max,
                    sec_min = sec_limits[[1]], sec_max = sec_limits[[2]])
    sec_lab <- if (is.null(sec_label)) rlang::as_label(sec_y) else sec_label
    sec_df <- as.data.frame(data)
    sec_df[["cpb__sec"]] <-
      (sec_vals - sec_map$sec_min) / (sec_map$sec_max - sec_map$sec_min) *
      (sec_map$prim_max - sec_map$prim_min) + sec_map$prim_min
    sec_df[["cpb__seclab"]] <- sec_lab
    sec_df <- sec_df[!duplicated(rlang::eval_tidy(x, data)), , drop = FALSE]
  }

  if (has_colour) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, colour = !!colour,
                            group = !!colour)
  } else if (has_sec) {
    # without a colour mapping there would be no key naming the primary
    # line, leaving the legend explaining only the secondary axis. Give
    # the primary line a key of its own, named after the `y` column.
    prim_lab <- if (is.null(ylab)) rlang::as_label(y) else ylab
    data <- as.data.frame(data)
    data[["cpb__primlab"]] <- prim_lab
    mapping <- ggplot2::aes(x = !!x, y = !!y,
                            colour = .data[["cpb__primlab"]], group = 1)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y, group = 1)
  }

  p <- ggplot2::ggplot(data, mapping)

  # background layers first: the forecast window, then the zero line,
  # then the uncertainty band, so the data lines stay on top
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)))
  }
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  single_colour <- cpb_single_colour(line_colour, 6)

  if (has_band) {
    if (has_colour) {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = !!ymin, ymax = !!ymax, fill = !!colour),
        alpha = 0.25, colour = NA
      ) +
        cpb_discrete_scale("fill", index, palette) +
        ggplot2::guides(fill = "none")
    } else {
      p <- p + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = !!ymin, ymax = !!ymax),
        fill = single_colour, alpha = 0.25, colour = NA
      )
    }
  }

  p <- p + if (has_colour || has_sec) {
    ggplot2::geom_line(linewidth = linewidth, show.legend = TRUE, ...)
  } else {
    # no colour mapping: draw one flat house-style colour (CPB primary
    # blue by default) rather than black
    ggplot2::geom_line(linewidth = linewidth, colour = single_colour, ...)
  }

  # markers sit on top of the line, in the same colour. Both layers map
  # the same colour variable, so ggplot2 overlays their glyphs into one
  # key -- a line with a marker on it, as in the published figures.
  if (isTRUE(points)) {
    p <- p + if (has_colour || has_sec) {
      ggplot2::geom_point(size = point_size, show.legend = TRUE)
    } else {
      ggplot2::geom_point(size = point_size, colour = single_colour)
    }
  }

  # the secondary series is rescaled onto the primary range and drawn
  # as one more line. Unlike cpb_col(), where the columns key on fill
  # and the line on colour, here the primary series already key on
  # colour -- so the secondary line joins that same scale and takes the
  # next palette position, which is how the published figures name both
  # axes in a single legend block.
  if (has_sec) {
    p <- p + ggplot2::geom_line(
      data = sec_df,
      ggplot2::aes(x = !!x, y = .data[["cpb__sec"]], colour = .data[["cpb__seclab"]],
                   group = 1),
      linewidth = if (is.null(sec_linewidth)) linewidth else sec_linewidth,
      show.legend = TRUE
    )
    if (isTRUE(points)) {
      p <- p + ggplot2::geom_point(
        data = sec_df,
        ggplot2::aes(x = !!x, y = .data[["cpb__sec"]],
                     colour = .data[["cpb__seclab"]]),
        size = point_size, show.legend = TRUE
      )
    }
  }

  # the label sits on top of everything
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)),
      rlang::eval_tidy(x, data), forecast_label)
  }

  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  # the panel is drawn tight around the data/limits, so the axis line
  # and ticks meet the outermost gridlines instead of floating beyond
  # them. A secondary unit caption is drawn above the panel, so it has
  # to survive clipping.
  #
  # Markers, unlike a line, have a radius: against a panel pinned
  # exactly to the data range the first, last and extreme points get
  # sliced in half by the panel edge. So when points are drawn, fall
  # back to ggplot2's default expansion for the margin they need.
  # Clipping stays on either way, so `value_limits` still crops rather
  # than letting out-of-range data spill over the axis labels.
  p <- p + ggplot2::coord_cartesian(
    clip = if (has_sec && !is.null(sec_ylab)) "off" else "on")

  # A continuous x axis is flushed the same way -- zero expansion --
  # unless points are drawn, in which case it needs the same marker
  # margin as the value axis. A discrete x axis is left alone: it
  # always keeps ggplot2's own categorical padding.
  if (!isTRUE(points) && is.numeric(rlang::eval_tidy(x, data))) {
    p <- p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0)))
  }

  # Use pretty() breaks as scale limits to keep the value axis flush.
  # Keep default coord expansion so discrete x keeps its edge padding.
  axis_values <- rlang::eval_tidy(y, data)
  if (has_band) {
    axis_values <- c(
      axis_values, rlang::eval_tidy(ymin, data),
      rlang::eval_tidy(ymax, data)
    )
  }
  scale_args <- cpb_flush_scale_args(
    axis_values = axis_values, pct_axis = pct_axis,
    value_accuracy = value_accuracy,
    value_breaks = value_breaks,
    value_limits = value_limits
  )

  if (has_sec) {
    # the right-hand axis is the inverse of the map that placed the
    # line, so its labels read in the secondary series' own units
    sm <- sec_map
    scale_args$sec.axis <- ggplot2::sec_axis(
      transform = function(v) {
        (v - sm$prim_min) / (sm$prim_max - sm$prim_min) *
          (sm$sec_max - sm$sec_min) + sm$sec_min
      },
      labels = if (isTRUE(pct_axis)) label_pct_nl() else label_number_nl()
    )
  }
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }

  if (!is.null(x_lim)) {
    # sec_y's own markers can legitimately sit exactly on the flush
    # axis boundary (its default range runs to the data's own max), so
    # a point or line there would otherwise be half-cropped by the
    # panel edge
    p <- p + ggplot2::coord_cartesian(xlim = x_lim, clip = if (has_sec) "off" else "on")
  } else if (has_sec) {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  }
  if (isTRUE(x_lim_follow_data) && is.null(x_lim)) {
    # scale-level, not coord's own expand = FALSE, which is blanket
    # across both axes and would strip the value axis's own flush
    # expansion too (see the discrete-x clipping fix above)
    p <- cpb_flush_expand_x(p, x, data)
  }
  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  if (has_colour || has_sec) {
    p <- p + cpb_discrete_scale("colour", index, palette)
    p <- cpb_add_legend_guide(p, "colour", reverse_legend, legend_ncol)
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
#' @param mean Optional column holding the mean of each distribution
#'   (tidy eval), drawn as a diamond marker. Only used by
#'   `box_style = "dot"`; ignored by the box-drawing styles, which have
#'   no published mean marker.
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
#'   Combines with a `fill` mapping (`"ggcpb"` style): pass a
#'   `position_dodge()` for e.g. two dodged years per category under
#'   the group headings. Typically used with
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
#'   * `"dot"`: the marker variant used for survey distributions --
#'     no box at all. A dashed connector spans p5-p95 with a light dot
#'     at each end, a capped bar spans the p25-p75 range, a filled dot
#'     marks the median and (when `mean` is supplied) a diamond marks
#'     the mean. Unlike the other styles it draws a legend naming each
#'     marker, so the reader can tell the five statistics apart.
#'
#'   `"james"`, `"modern"` and `"dot"` follow the house convention of
#'   horizontal boxes; combine them with
#'   `orientation = "horizontal"`.
#' @param dot_labels Legend labels for `box_style = "dot"`, as a named
#'   character vector with elements `p5`, `iqr`, `p50`, `p95` and
#'   `mean`. `NULL` (default) uses the published Dutch labels. Supply
#'   only the elements you want to change; the rest keep their
#'   defaults. Ignored by the other styles.
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
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param fill_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_fill_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param index Deprecated. Former name of
#'   `fill_index`. Still accepted, with a warning.
#' @param pct_axis If `TRUE`, format the value axis with
#'   [label_pct_nl()].
#' @param value_accuracy Rounding accuracy for the value axis labels,
#'   passed to [label_number_nl()] (e.g. `0.1` for one decimal place).
#'   `NULL` (default) lets `scales` pick a sensible accuracy from the
#'   breaks. Cannot be combined with `pct_axis`. Use this instead of
#'   adding a second `scale_y_continuous()`, which would discard the
#'   wrapper's flush axis (see `value_breaks`).
#' @param value_breaks Optional breaks for the value axis (passed to
#'   the wrapper-built [ggplot2::scale_y_continuous()]). Use this
#'   instead of adding a second y scale, which would discard the
#'   wrapper's axis formatting and expansion.
#' @param value_limits Optional length-2 numeric vector giving the
#'   value-axis range, applied as the wrapper-built value scale's own
#'   `limits` (not a coordinate-system zoom) -- the hard bound the axis
#'   is drawn flush to; a box/whisker outside it is genuinely dropped,
#'   with a warning, the same as setting `limits` on any ggplot2 scale.
#'   `NULL` (default) flushes to the full p5-p95 (and `mean`) range
#'   instead.
#' @param value_axis Where the value axis is drawn: `"bottom"`
#'   (default) or `"top"`. `"top"` places the numeric scale along the
#'   top of the panel, the convention of the CPB koopkracht figures;
#'   it applies to horizontal boxes (the value axis is the flipped
#'   axis).
#' @param x_lim Optional length-2 vector zooming the category (`x`)
#'   axis to a range, without dropping data -- applied as a
#'   coordinate-system zoom ([ggplot2::coord_cartesian()] /
#'   [ggplot2::coord_flip()] `xlim`). `NULL` (default) shows the full
#'   range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the category axis so it sits flush to the data's
#'   actual range, at the cost of ggplot2 picking its own breaks within
#'   that (possibly non-round) range instead of the usual padded,
#'   evenly spaced ones. Matches nicerplot's parameter of the same
#'   name. Defaults to `FALSE`. Ignored when `x_lim` is set, and when
#'   `group` is mapped (the grouped layout needs its own fixed margin
#'   for the heading rows).
#' @param orientation `"vertical"` (default) or `"horizontal"` (adds
#'   [ggplot2::coord_flip()] and is forwarded to [theme_cpb()]).
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, alongside the
#'   boxes. One value per `x`. Only supported for vertical boxes with
#'   no `group` mapping, `box_style` other than `"dot"`, and
#'   `value_axis = "bottom"` -- each of those already claims the space
#'   `sec_y` needs (the right/top edge of the panel, or the colour
#'   aesthetic). With no `fill` mapping the boxes get their own legend
#'   key too (a plain square in `fill_colour`), so both series are
#'   always named in the legend.
#' @param sec_type How `sec_y` is drawn: `"line"` (default), `"point"`
#'   (markers only, no connecting line), or `"col"` (thin bars).
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses zero to the maximum of
#'   `sec_y`. `sec_y` is placed by mapping this range linearly onto
#'   the primary (p5-p95) range, so the two axes always start together.
#' @param sec_label Legend label for `sec_y`. `NULL` (default) uses
#'   the `sec_y` column name. Automatically suffixed `"(rechteras)"`
#'   (right axis) -- don't add it yourself, e.g. `sec_label =
#'   "erfbelasting"` shows as `"erfbelasting (rechteras)"`.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_colour Colour for `sec_y`; defaults to `NULL`, which
#'   resolves to the CPB pink (`cpb_cols(2)`, `"#e6006e"`).
#' @param sec_linewidth Line width; only used when `sec_type = "line"`.
#'   Defaults to `0.55`.
#' @param sec_points If `TRUE`, add a marker at every point of the
#'   `sec_y` line. Only used when `sec_type = "line"` -- for markers
#'   without a connecting line, use `sec_type = "point"` instead.
#' @param reverse_legend If `TRUE`, reverse the fill legend order via
#'   `guide_legend(reverse = TRUE)`. Defaults to `FALSE`; useful when
#'   the fill levels were reversed to control the dodge order under
#'   `coord_flip()`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
#'   p5 = c(-8, -6, -4),
#'   p25 = c(-4, -3, -2),
#'   p50 = c(-2, -1, 0),
#'   p75 = c(0, 1, 2),
#'   p95 = c(3, 4, 5)
#' )
#' cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
#' @export
cpb_box <- function(data, x, p5, p25, p50, p75, p95,
                     mean = NULL,
                     fill = NULL,
                     fill_colour = NULL,
                     group = NULL,
                     group_gap = 0.7,
                     box_style = c("ggcpb", "james", "modern", "dot"),
                     dot_labels = NULL,
                     box_labels = NULL,
                     label_accuracy = 0.1,
                     width = 0.5,
                     linewidth = 0.25,
                     palette = "qualitative",
                     fill_index = NULL,
                     index = NULL,
                     pct_axis = FALSE,
                     value_accuracy = NULL,
                     value_breaks = NULL,
                     value_limits = NULL,
                     value_axis = c("bottom", "top"),
                     x_lim = NULL,
                     x_lim_follow_data = FALSE,
                     orientation = c("vertical", "horizontal"),
                     sec_y = NULL,
                     sec_type = c("line", "point", "col"),
                     sec_limits = NULL,
                     sec_label = NULL,
                     sec_ylab = NULL,
                     sec_colour = NULL,
                     sec_linewidth = 0.55,
                     sec_points = FALSE,
                     facet = NULL,
                     facet_ncol = NULL,
                     facet_scales = "fixed",
                     legend = "bottom",
                     reverse_legend = FALSE,
                     legend_ncol = NULL,
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
  .cpb_idx <- cpb_resolve_index(fill_index, index, palette, !missing(palette), "fill_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  orientation <- match.arg(orientation)
  box_style <- match.arg(box_style)
  value_axis <- match.arg(value_axis)
  sec_type <- match.arg(sec_type)

  x <- rlang::enquo(x)
  p5 <- rlang::enquo(p5)
  p25 <- rlang::enquo(p25)
  p50 <- rlang::enquo(p50)
  p75 <- rlang::enquo(p75)
  p95 <- rlang::enquo(p95)
  mean <- rlang::enquo(mean)
  has_mean <- !rlang::quo_is_null(mean)
  fill <- rlang::enquo(fill)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  sec_y <- rlang::enquo(sec_y)
  has_fill <- !rlang::quo_is_null(fill)
  has_group <- !rlang::quo_is_null(group)
  has_sec <- !rlang::quo_is_null(sec_y)

  if (has_sec) {
    if (orientation == "horizontal") {
      stop("`sec_y` is only supported for vertical box charts: the ",
           "secondary axis is drawn on the right of the value axis.",
           call. = FALSE)
    }
    if (has_group) {
      stop("`sec_y` and `group` cannot be combined: the bold group ",
           "headings and the secondary axis both claim the space beside ",
           "the panel.", call. = FALSE)
    }
    if (box_style == "dot") {
      stop("`sec_y` cannot be combined with box_style = \"dot\": both ",
           "need the colour aesthetic and its one legend.", call. = FALSE)
    }
    if (value_axis == "top") {
      stop("`sec_y` cannot be combined with value_axis = \"top\": both ",
           "claim the right/top edge of the panel.", call. = FALSE)
    }
  }
  # the label used for the boxes' own legend key when sec_y needs one
  # added and there is no real `fill` mapping to name it after instead
  # (see the "always show a legend for both series" comment below)
  primary_lab <- rlang::as_label(p50)

  slots <- NULL
  if (has_group) {
    # vertical grouping: every group gets a bold heading row above its
    # categories; all boxes share one value axis
    slots <- cpb_group_heading_positions(rlang::eval_tidy(x, data),
      rlang::eval_tidy(group, data),
      gap = group_gap
    )
    data <- as.data.frame(data)
    data[["cpb__x"]] <- slots$pos[match(
      as.character(rlang::eval_tidy(x, data)),
      slots$cat
    )]
    x <- rlang::quo(.data[["cpb__x"]])
  }

  if (has_fill && box_style != "ggcpb") {
    stop("box_style = \"", box_style, "\" draws single-colour boxes and does ",
      "not support a `fill` mapping; use box_style = \"ggcpb\" for ",
      "fill-grouped boxes.",
      call. = FALSE
    )
  }
  # the "dot" style carries a legend instead of printed values
  if (is.null(box_labels)) box_labels <- box_style %in% c("james", "modern")
  if (isTRUE(box_labels) && box_style == "dot") {
    stop("box_style = \"dot\" does not print value labels; it names the ",
      "markers in a legend instead (see `dot_labels`).",
      call. = FALSE
    )
  }
  if (has_mean && box_style != "dot") {
    stop("`mean` is only drawn by box_style = \"dot\"; the box styles have ",
      "no published mean marker.",
      call. = FALSE
    )
  }

  if (is.null(zeroline)) {
    zeroline <- cpb_zeroline_auto(rlang::eval_tidy(p5, data), rlang::eval_tidy(p95, data))
  }

  if (has_fill) {
    # the x-fill interaction drives the dodge: errorbars have no fill
    # aesthetic (mapping one only warns), and on the numeric category
    # axis of the grouped layout fill alone would chain across rows
    mapping_errorbar <- ggplot2::aes(
      x = !!x, ymin = !!p5, ymax = !!p95,
      group = interaction(!!x, !!fill)
    )
    mapping_box <- ggplot2::aes(
      x = !!x, ymin = !!p25, lower = !!p25, middle = !!p50, upper = !!p75,
      ymax = !!p75, fill = !!fill, group = interaction(!!x, !!fill)
    )
  } else if (has_sec) {
    # no real fill mapping, but a second axis means the boxes need
    # their own legend key too, not just sec_y's -- see "always show a
    # legend for both series" below
    mapping_errorbar <- ggplot2::aes(
      x = !!x, ymin = !!p5, ymax = !!p95,
      group = !!x
    )
    mapping_box <- ggplot2::aes(
      x = !!x, ymin = !!p25, lower = !!p25, middle = !!p50, upper = !!p75,
      ymax = !!p75, fill = primary_lab, group = !!x
    )
  } else {
    # the explicit group keeps one box per category when the category
    # axis is numeric (the grouped-slots layout)
    mapping_errorbar <- ggplot2::aes(
      x = !!x, ymin = !!p5, ymax = !!p95,
      group = !!x
    )
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

  if (box_style == "dot") {
    # Markers only: a dashed p5-p95 connector with a light dot at each
    # end, a capped p25-p75 bar, a filled median dot and an optional
    # mean diamond. Every layer maps `colour` to a constant label so
    # the markers get named legend keys; because each layer's data
    # holds exactly one level, a key row only ever picks up the glyph
    # of the layer it belongs to.
    labs_default <- c(
      p5 = "5e percentiel", iqr = "25e-75e percentiel",
      p50 = "mediaan", p95 = "95e percentiel",
      mean = "gemiddelde"
    )
    if (!is.null(dot_labels)) {
      if (is.null(names(dot_labels)) ||
        !all(names(dot_labels) %in% names(labs_default))) {
        stop("`dot_labels` must be a named character vector using the names ",
          paste0("\"", names(labs_default), "\"", collapse = ", "), ".",
          call. = FALSE
        )
      }
      labs_default[names(dot_labels)] <- dot_labels
    }
    lab <- as.list(labs_default)

    accent <- cpb_single_colour(fill_colour, 2)[[1]]
    light <- unname(cpb_cols(1))
    meancol <- unname(cpb_cols(5))

    # legend order follows the published figure: the two tails first,
    # then the interquartile range, the median and the mean
    dot_values <- c(light, accent, accent, light, meancol)
    names(dot_values) <- c(lab$p5, lab$iqr, lab$p50, lab$p95, lab$mean)
    dot_breaks <- c(lab$p5, lab$p95, lab$iqr, lab$p50)
    if (has_mean) dot_breaks <- c(dot_breaks, lab$mean)

    p <- p +
      # the connector runs the full p5-p95 span underneath everything
      ggplot2::geom_errorbar(
        ggplot2::aes(x = !!x, ymin = !!p5, ymax = !!p95),
        width = 0, linewidth = 0.25, linetype = "dashed", colour = light
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = !!x, y = !!p5, colour = lab$p5),
        size = 1.1
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = !!x, y = !!p95, colour = lab$p95),
        size = 1.1
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(x = !!x, ymin = !!p25, ymax = !!p75, colour = lab$iqr),
        width = width / 2, linewidth = 0.4,
        key_glyph = cpb_key_errorbar(orientation)
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = !!x, y = !!p50, colour = lab$p50),
        size = 1.6, ...
      )
    if (has_mean) {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(x = !!x, y = !!mean, colour = lab$mean),
        shape = 23, fill = meancol, size = 1.5, stroke = 0.3
      )
    }
    p <- p + ggplot2::scale_colour_manual(
      values = dot_values, breaks = dot_breaks, name = NULL
    )
  } else if (box_style == "ggcpb") {
    # key_glyph = "rect": CPB legends show plain colour squares, not
    # miniature boxplots. Without a fill mapping the boxes are drawn in
    # one flat house-style colour (CPB primary blue by default).
    box_args <- list(mapping = mapping_box, stat = "identity", width = width,
                     linewidth = linewidth, key_glyph = "rect",
                     show.legend = TRUE, ...)
    style_fill_col <- cpb_single_colour(fill_colour, 6)
    if (!has_fill && !has_sec) {
      # fill is a literal constant here; with has_sec it is aes-mapped
      # to primary_lab instead (set above), so a literal value here
      # would silently override that mapping
      box_args$fill <- style_fill_col
    }
    p <- p +
      ggplot2::geom_errorbar(
        mapping = mapping_errorbar, width = width / 2,
        linewidth = linewidth, ...
      ) +
      do.call(ggplot2::geom_boxplot, box_args)
  } else {
    # "james" (the legacy nplot() box) and "modern" (its designer
    # variant) share one construction: a borderless filled box over
    # p25-p75, plain capless whiskers in the box colour, and a median
    # line extending slightly beyond the box. They differ in colours,
    # weights and which value labels are printed.
    sty <- switch(box_style,
      james = list(
        box_col = cpb_single_colour(fill_colour, 6),
        whisk_lw = 0.4,
        med_col = "black", med_lw = 0.4, med_ext = 0.15,
        med_lab_col = "black", med_lab_face = "plain", med_lab_size = 2.2,
        q_labels = FALSE
      ),
      modern = list(
        box_col = cpb_single_colour(fill_colour, 5),
        whisk_lw = 0.55,
        med_col = unname(cpb_cols(6)), med_lw = 1.3, med_ext = 0.2,
        med_lab_col = unname(cpb_cols(6)), med_lab_face = "bold", med_lab_size = 2.6,
        q_labels = TRUE, q_lab_col = "#00a5ff", q_lab_size = 2.2
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
    box_args2 <- list(
      mapping = mapping_box, stat = "identity",
      width = width, colour = NA, key_glyph = "rect"
    )
    style_fill_col <- sty$box_col
    if (is.null(row_cols)) {
      whisk_args$colour <- sty$box_col
      if (!has_sec) {
        # fill is a literal constant here; with has_sec it is
        # aes-mapped to primary_lab instead (set above, in
        # mapping_box), so a literal value here would silently
        # override that mapping
        box_args2$fill <- sty$box_col
      }
    } else {
      col_aes <- ggplot2::aes(colour = I(.data[["cpb__boxcol"]]))
      whisk_lo <- utils::modifyList(whisk_lo, col_aes)
      whisk_hi <- utils::modifyList(whisk_hi, col_aes)
      box_args2$mapping <- utils::modifyList(
        mapping_box, ggplot2::aes(fill = I(.data[["cpb__boxcol"]]))
      )
    }

    p <- p +
      do.call(
        ggplot2::geom_errorbar,
        c(list(mapping = whisk_lo), whisk_args, list(...))
      ) +
      do.call(
        ggplot2::geom_errorbar,
        c(list(mapping = whisk_hi), whisk_args, list(...))
      ) +
      do.call(ggplot2::geom_boxplot, c(box_args2, list(...))) +
      # the median: a zero-span errorbar whose cap IS the median line,
      # slightly wider than the box
      ggplot2::geom_errorbar(ggplot2::aes(x = !!x, ymin = !!p50, ymax = !!p50),
        width = width * (1 + 2 * sty$med_ext),
        linewidth = sty$med_lw, colour = sty$med_col, ...
      )

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

  # boxes do not grow from the axis (no forced zero baseline, unlike
  # cpb_col()), but both ends are still drawn flush to the p5-p95 (and
  # mean, for box_style = "dot") range
  axis_values <- c(rlang::eval_tidy(p5, data), rlang::eval_tidy(p95, data))
  if (has_mean) axis_values <- c(axis_values, rlang::eval_tidy(mean, data))
  scale_args <- cpb_flush_scale_args(
    axis_values = axis_values, pct_axis = pct_axis,
    value_accuracy = value_accuracy,
    value_breaks = value_breaks,
    value_limits = value_limits
  )

  # sec_y is drawn alongside the boxes, mapped onto this same flush
  # range, and read off its own axis on the right; see the "sec_y
  # helpers" block near the top of this file for how that mapping and
  # its axis labels are kept in sync with each other.
  if (has_sec) {
    sec_vals <- rlang::eval_tidy(sec_y, data)
    sec_map <- cpb_sec_map(sec_vals, sec_limits, scale_args$limits[[1]], scale_args$limits[[2]])
    sec_lab <- if (is.null(sec_label)) rlang::as_label(sec_y) else sec_label
    sec_col <- cpb_single_colour(sec_colour, 2)
    p <- cpb_sec_layer(p, data, x, sec_vals, sec_map, sec_type,
                       sec_col, sec_lab, sec_linewidth, sec_points)
    scale_args$sec.axis <- cpb_sec_axis(sec_map, scale_args$breaks)
  }

  # the grouped layout draws its bold headings outside the panel, the
  # james/modern value labels are nudged past their box's own category
  # slot (the median label above it, the modern quartile labels
  # below), the dot style's p5/p95 markers can legitimately land
  # exactly on the flush value-axis boundary (e.g. a p5 of zero on a
  # zero-flush axis), and sec_y's own markers can just as legitimately
  # land on it too (its default range runs to the data's own max) --
  # all would otherwise get half-cropped by the panel edge, so
  # clipping is turned off in all these cases
  clip <- if (has_group || isTRUE(box_labels) || box_style == "dot" ||
             has_sec) "off" else "on"
  p <- cpb_apply_coord(
    p, orientation, x_lim, value_limits, clip,
    x, data, x_lim_follow_data, has_group
  )

  # value_axis = "top" draws the value scale at the top of the panel
  # (the koopkracht-figure convention). The value is the y aesthetic;
  # under coord_flip() its "right" position renders along the top edge.
  if (value_axis == "top") scale_args$position <- "right"
  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }
  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  if (isTRUE(box_labels) && !has_group) {
    # the value labels are nudged along the category axis past their
    # own box's slot (the median label above it, the modern quartile
    # labels below) -- close enough to the first/last category that
    # the ggplot2 discrete default (add = 0.6) can crop them. 0.9
    # matches the margin the grouped layout already uses below for
    # its own heading rows.
    p <- p + ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.9))
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
        ggplot2::annotate("text",
          x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 1.03, vjust = 0.5,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family()
        )
      } else {
        ggplot2::annotate("text",
          x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 0.5, vjust = 2.6,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family()
        )
      }
    }
  }

  if (has_fill) {
    p <- p + cpb_discrete_scale("fill", index, palette, labels = cpb_linkeras_labels(has_sec))
    p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)
  } else if (has_sec) {
    # no real fill mapping, so the boxes' own legend key (added above,
    # via mapping_box's fill = primary_lab) needs a matching one-colour
    # scale -- the same "always show a legend for both series" trick
    # cpb_col() uses
    p <- p + ggplot2::scale_fill_manual(
      values = stats::setNames(style_fill_col, primary_lab), name = NULL,
      labels = cpb_linkeras_labels(TRUE)
    )
    p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)
  }

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # CPB house style has no rotated axis titles: whichever aesthetic
  # coord_flip() leaves drawn vertically gets its label promoted to a
  # caption above the panel instead, matching how the OTHER box
  # examples in this vignette use `subtitle` to name the category axis
  # -- ylab for the value axis when vertical, xlab for the category
  # axis when horizontal. Either keeps a plain, un-rotated axis title
  # when it lands on the horizontal axis instead. A titled figure
  # always reserves the subtitle line for a stable gap.
  lab_x <- xlab
  lab_y <- ylab
  if (orientation == "vertical" && is.null(subtitle) && !is.null(ylab)) {
    subtitle <- ylab
    lab_y <- NULL
  } else if (orientation == "horizontal" && is.null(subtitle) && !is.null(xlab)) {
    subtitle <- xlab
    lab_x <- NULL
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = lab_x, y = lab_y, fill = filllab) +
    cpb_wrapper_theme()

  if (has_sec) {
    # the boxes' fill key and sec_y's own key are two separate guides;
    # stack them into one left-aligned block instead of letting them
    # sit side by side, fill named first, as in the published figures.
    # override.aes: without this, ggplot2 sometimes carries the sec_y
    # point/line layer's own colour and point shape into the fill
    # guide's key background (a stray dot rendered on top of a fill
    # square) when the two guides are stacked like this -- telling the
    # fill guide explicitly to ignore those aesthetics is the
    # documented ggplot2 fix for that
    p <- p +
      ggplot2::guides(
        fill = ggplot2::guide_legend(order = 1, reverse = isTRUE(reverse_legend),
                                     ncol = legend_ncol,
                                     override.aes = list(colour = NA, shape = NA)),
        colour = ggplot2::guide_legend(order = 2)
      ) +
      ggplot2::theme(legend.box = "vertical", legend.box.just = "left")
  }
  p
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
#'   one of `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param colour_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_colour_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param color_index American-spelling alias for `colour_index`; ignored
#'   when `colour_index` is given.
#' @param index Deprecated. Former name of
#'   `colour_index`. Still accepted, with a warning.
#' @param x_lim Optional length-2 vector zooming the `x` axis to a
#'   range, without dropping data. Also recomputes the flush breaks
#'   for that window, rather than the full data's (which would mostly
#'   fall outside it). `NULL` (default) shows the full range.
#' @param x_lim_follow_data If `TRUE`, flush the `x` axis exactly to
#'   the data's own range, at the cost of ggplot2 picking its own
#'   breaks within that (possibly non-round) range instead of the
#'   usual `pretty()` ones. Matches nicerplot's parameter of the same
#'   name. Defaults to `FALSE`. Ignored when `x_lim` is set.
#' @param forecast_x Optional x value where the forecast window
#'   starts; overlaid and labelled as in [cpb_line()].
#' @param forecast_label Label for the forecast window; defaults to
#'   `"raming"`. Use `NULL` (or `""`) for no label.
#' @param reverse_legend If `TRUE`, reverse the colour legend order
#'   via `guide_legend(reverse = TRUE)` (discrete `colour` only).
#'   Defaults to `FALSE`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
                         colour_index = NULL,
                         color_index = NULL,
                         index = NULL,
                         x_lim = NULL,
                         x_lim_follow_data = FALSE,
                         forecast_x = NULL,
                         forecast_label = "raming",
                         reverse_legend = FALSE,
                         legend_ncol = NULL,
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
  if (is.null(colour_index)) colour_index <- color_index
  .cpb_idx <- cpb_resolve_index(colour_index, index, palette, !missing(palette), "colour_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  colour <- rlang::enquo(colour)
  facet <- rlang::enquo(facet)
  has_colour <- !rlang::quo_is_null(colour)

  if (is.null(zeroline)) {
    yvals <- rlang::eval_tidy(y, data)
    zeroline <- cpb_zeroline_auto(yvals, yvals)
  }

  if (has_colour) {
    mapping <- ggplot2::aes(x = !!x, y = !!y, colour = !!colour)
  } else {
    mapping <- ggplot2::aes(x = !!x, y = !!y)
  }

  p <- ggplot2::ggplot(data, mapping)

  # underneath the points: first the forecast window, then the zero line
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_rect(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)))
  }
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }

  p <- p + if (has_colour) {
    ggplot2::geom_point(size = size, show.legend = TRUE, ...)
  } else {
    single_colour <- cpb_single_colour(point_colour, 6)
    ggplot2::geom_point(size = size, colour = single_colour, ...)
  }

  # the label sits on top of everything
  if (!is.null(forecast_x)) {
    p <- p + cpb_forecast_label(
      cpb_forecast_pos(forecast_x, rlang::eval_tidy(x, data)),
      rlang::eval_tidy(x, data), forecast_label)
  }

  # a numeric colour column gets the continuous gradient, anything
  # else the discrete palette
  if (has_colour) {
    colvals <- rlang::eval_tidy(colour, data)
    p <- p + if (is.numeric(colvals)) {
      scale_colour_cpb_c()
    } else {
      cpb_discrete_scale("colour", index, palette)
    }
    if (!is.numeric(colvals)) {
      p <- cpb_add_legend_guide(p, "colour", reverse_legend, legend_ncol)
    }
  }

  # both axes are drawn flush at both ends via pretty() breaks -- kept
  # coord-based (not a scale limits/expand, as in the other wrappers)
  # because a scatter plot's own follow-up scale_x_continuous() (e.g.
  # to add euro labels, a common real usage pattern) would otherwise
  # silently replace and discard the flush; coord survives that. Both
  # axes are always continuous here, so a blanket coord expand = FALSE
  # carries none of the discrete-axis-clipping risk it has elsewhere.
  x_data_range <- range(rlang::eval_tidy(x, data), na.rm = TRUE)
  if (!is.null(x_lim)) {
    # a manual zoom gets its own breaks computed for that window,
    # rather than the full data's (which would mostly fall outside it)
    x_breaks <- pretty(x_lim)
    flush_xlim <- x_lim
  } else if (isTRUE(x_lim_follow_data)) {
    # no rounding: flush exactly to the data, ggplot2 picks its own
    # breaks within that (possibly non-round) range
    x_breaks <- NULL
    flush_xlim <- x_data_range
  } else {
    x_breaks <- pretty(x_data_range)
    flush_xlim <- range(x_breaks)
  }
  y_breaks <- pretty(range(rlang::eval_tidy(y, data), na.rm = TRUE))
  p <- p + ggplot2::coord_cartesian(
    xlim = flush_xlim, ylim = range(y_breaks),
    expand = FALSE
  )
  x_scale_args <- if (!is.null(x_breaks)) list(breaks = x_breaks) else list()
  p <- p + do.call(ggplot2::scale_x_continuous, x_scale_args) +
    ggplot2::scale_y_continuous(breaks = y_breaks)

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
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param fill_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_fill_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param index Deprecated. Former name of
#'   `fill_index`. Still accepted, with a warning.
#' @param x_lim Optional length-2 vector zooming the `x` axis to a
#'   range, without dropping data. Bins are computed from the full
#'   data first, so this only ever changes what is visible, never the
#'   binning itself. `NULL` (default) shows the full range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the `x` axis so it sits flush to the data's actual
#'   range, at the cost of ggplot2 picking its own breaks within that
#'   (possibly non-round) range instead of the usual padded, evenly
#'   spaced ones. Matches nicerplot's parameter of the same name.
#'   Defaults to `FALSE`. Ignored when `x_lim` is set.
#' @param reverse_legend If `TRUE` (default), reverse the fill legend
#'   order via `guide_legend(reverse = TRUE)`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
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
                      fill_index = NULL,
                      index = NULL,
                      x_lim = NULL,
                      x_lim_follow_data = FALSE,
                      reverse_legend = TRUE,
                      legend_ncol = NULL,
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
  .cpb_idx <- cpb_resolve_index(fill_index, index, palette, !missing(palette), "fill_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
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
                            colour = outline, linewidth = 0.2,
                            show.legend = TRUE, ...)
  } else {
    single_fill <- cpb_single_colour(fill_colour, 6)
    ggplot2::geom_histogram(
      binwidth = binwidth, bins = bins, position = position,
      colour = outline, linewidth = 0.2, fill = single_fill, ...
    )
  }

  # counts are anchored at zero: black zero line on top of the bars and
  # a count axis flush with the axis line
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, colour = "black", linewidth = 0.25)
  }
  p <- p + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)))

  # bins are computed from the full data before any x zoom applies, so
  # x_lim only ever changes what is visible, never the binning itself
  if (!is.null(x_lim)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_lim)
  } else if (isTRUE(x_lim_follow_data)) {
    p <- cpb_flush_expand_x(p, x, data)
  }

  if (has_fill) {
    p <- p + cpb_discrete_scale("fill", index, palette)
    p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)
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

# dot-and-interval ----

#' A CPB-styled dot-and-interval chart
#'
#' The coefficient-plot form used for regression output in CPB
#' publications: one point estimate per row with a horizontal interval
#' through it, a black reference line at zero, and (optionally) the
#' rows collected under bold group headings. Both layers use
#' `stat = "identity"` -- pass precomputed estimates and interval
#' bounds rather than raw observations.
#'
#' Unlike [cpb_scatter()], which draws a cloud of observations on two
#' continuous axes, this wrapper puts a categorical axis against a
#' single value axis, and is horizontal by default: the estimate rows
#' read top to bottom.
#'
#' @param data A data.frame or data.table with one row per estimate.
#' @param x Column mapped to the category axis (tidy eval), i.e. the
#'   term each estimate belongs to.
#' @param y Column holding the point estimate (tidy eval).
#' @param lower,upper Columns holding the lower and upper bounds of the
#'   interval around each estimate (tidy eval). Both are required: an
#'   estimate without its uncertainty is a job for [cpb_col()].
#' @param colour Optional column mapped to the colour aesthetic (tidy
#'   eval), e.g. to distinguish two model specifications. If omitted,
#'   everything is drawn in `point_colour`.
#' @param point_colour Constant colour used when no `colour` column is
#'   mapped. Defaults to `NULL`, which resolves to the CPB pink
#'   (`cpb_cols(2)`, `"#e6006e"`) used in the published coefficient
#'   figures.
#' @param group Optional column (tidy eval) assigning each `x` category
#'   to a group; every group gets its name as a bold heading row above
#'   its categories, exactly as in [cpb_box()].
#' @param group_gap Extra gap between group blocks, in category widths;
#'   defaults to `0.7`.
#' @param size Point size; defaults to `1.4`.
#' @param linewidth Interval line width; defaults to `0.4`.
#' @param cap_width Width of the interval end caps, in category widths;
#'   defaults to `0.25`. Use `0` for plain capless intervals.
#' @param orientation `"horizontal"` (default, the published form,
#'   adding [ggplot2::coord_flip()]) or `"vertical"`.
#' @param sec_y Optional column (tidy eval) holding a series to draw
#'   against a **secondary value axis** on the right, alongside the
#'   points. One value per `x`. Requires `orientation = "vertical"`
#'   (the secondary axis is drawn on the right of the value axis), no
#'   `group` mapping (the bold group headings claim the same space),
#'   and `colour` to be unset (both need the colour aesthetic and its
#'   one legend); the primary points stay plain and unlabelled in
#'   `point_colour`, and only `sec_y` gets a legend key.
#' @param sec_type How `sec_y` is drawn: `"line"` (default), `"point"`
#'   (markers only, no connecting line), or `"col"` (thin bars).
#' @param sec_limits Length-2 numeric vector giving the range the
#'   secondary axis spans. `NULL` (default) uses zero to the maximum of
#'   `sec_y`. `sec_y` is placed by mapping this range linearly onto
#'   the primary (lower-upper) range, so the two axes always start
#'   together.
#' @param sec_label Legend label for `sec_y`. `NULL` (default) uses
#'   the `sec_y` column name. Automatically suffixed `"(rechteras)"`
#'   (right axis) -- don't add it yourself, e.g. `sec_label =
#'   "erfbelasting"` shows as `"erfbelasting (rechteras)"`.
#' @param sec_ylab Unit caption for the secondary axis, drawn
#'   right-aligned above the panel to mirror the left-hand unit that
#'   `ylab` puts in the subtitle. `NULL` (default) draws none.
#' @param sec_colour Colour for `sec_y`; defaults to `NULL`, which
#'   resolves to the CPB pink (`cpb_cols(2)`, `"#e6006e"`).
#' @param sec_linewidth Line width; only used when `sec_type = "line"`.
#'   Defaults to `0.55`.
#' @param sec_points If `TRUE`, add a marker at every point of the
#'   `sec_y` line. Only used when `sec_type = "line"` -- for markers
#'   without a connecting line, use `sec_type = "point"` instead.
#' @param palette CPB palette to use for `colour`; one of
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param colour_index Which house colours the series get. Either a vector
#'   of palette positions -- `c(2, 5, 6)`, forwarded to
#'   [scale_colour_cpb_manual()] -- or a keyword naming a palette:
#'   `"discrete"` for the qualitative house palette (blue, magenta,
#'   taupe, ...) and `"continuous"` for the sequential ramp. `NULL`
#'   (default) uses `palette`, which is `"discrete"` for every wrapper
#'   except [cpb_map()]. A keyword and a non-matching `palette` are a
#'   conflict and raise an error, since both set the same thing.
#' @param color_index American-spelling alias for `colour_index`; ignored
#'   when `colour_index` is given.
#' @param index Deprecated. Former name of
#'   `colour_index`. Still accepted, with a warning.
#' @param pct_axis If `TRUE`, format the value axis with
#'   [label_pct_nl()].
#' @param value_accuracy Rounding accuracy for the value axis labels,
#'   passed to [label_number_nl()] (e.g. `0.1` for one decimal place).
#'   `NULL` (default) lets `scales` pick a sensible accuracy from the
#'   breaks. Cannot be combined with `pct_axis`. Use this instead of
#'   adding a second `scale_y_continuous()`, which would discard the
#'   wrapper's flush axis (see `value_breaks`).
#' @param value_breaks Optional breaks for the value axis (passed to
#'   [ggplot2::scale_y_continuous()]).
#' @param value_limits Optional length-2 numeric vector giving the
#'   value-axis range, applied as the wrapper-built value scale's own
#'   `limits` (not a coordinate-system zoom) -- the hard bound the axis
#'   is drawn flush to; an estimate outside it is genuinely dropped,
#'   with a warning, the same as setting `limits` on any ggplot2 scale.
#'   `NULL` (default) flushes to the full lower-upper (and point) range
#'   instead.
#' @param x_lim Optional length-2 vector zooming the category (`x`)
#'   axis to a range, without dropping data -- applied as a
#'   coordinate-system zoom ([ggplot2::coord_cartesian()] /
#'   [ggplot2::coord_flip()] `xlim`). `NULL` (default) shows the full
#'   range.
#' @param x_lim_follow_data If `TRUE`, remove the default margin on
#'   either side of the category axis so it sits flush to the data's
#'   actual range, at the cost of ggplot2 picking its own breaks within
#'   that (possibly non-round) range instead of the usual padded,
#'   evenly spaced ones. Matches nicerplot's parameter of the same
#'   name. Defaults to `FALSE`. Ignored when `x_lim` is set, and when
#'   `group` is mapped (the grouped layout needs its own fixed margin
#'   for the heading rows).
#' @param zeroline If `TRUE` (default), draw a solid black reference
#'   line at zero on the value axis -- the line the intervals are read
#'   against.
#' @param reverse_legend If `TRUE`, reverse the colour legend order.
#'   Defaults to `FALSE`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = )`. `NULL` (default) leaves the
#'   single flush-left column of the house style; `2` and up suit a
#'   legend with many short keys, such as binned classes from
#'   [cpb_cut()], which would otherwise run past the panel.
#' @param facet Optional column (tidy eval) to facet by.
#' @param facet_ncol Number of facet columns, passed to
#'   [ggplot2::facet_wrap()].
#' @param facet_scales Whether facet axis ranges are shared; passed to
#'   [ggplot2::facet_wrap()].
#' @param legend Legend position, forwarded to [theme_cpb()].
#' @param minor,ticks,flush_legend,axis_text_size,legend_key_size,grid_colour,grid_linewidth
#'   Passed through to [theme_cpb()].
#' @param title,subtitle Plot title/subtitle.
#' @param xlab Label for the value axis (drawn at the bottom, where the
#'   value axis lands after `coord_flip()`).
#' @param ylab Label for the category axis. Following CPB house style
#'   this is normally left `NULL`.
#' @param colourlab Legend title override; defaults to `NULL`.
#' @param ... Further arguments passed to [ggplot2::geom_point()].
#' @return A `ggplot` object.
#' @examples
#' df <- data.frame(
#'   term = c(
#'     "Vertrouwen in de politiek", "Succes door hard werken",
#'     "Heeft kinderen", "Vermogenskwintiel"
#'   ),
#'   coef = c(2.9, -2.0, -1.4, -2.5),
#'   lo = c(1.9, -3.0, -3.3, -3.2),
#'   hi = c(3.9, -1.1, 0.6, -1.8)
#' )
#' cpb_dot(df, x = term, y = coef, lower = lo, upper = hi,
#'         xlab = "%-punt verandering")
#' @export
cpb_dot <- function(data, x, y, lower, upper,
                     colour = NULL,
                     point_colour = NULL,
                     group = NULL,
                     group_gap = 0.7,
                     size = 1.4,
                     linewidth = 0.4,
                     cap_width = 0.25,
                     orientation = c("horizontal", "vertical"),
                     sec_y = NULL,
                     sec_type = c("line", "point", "col"),
                     sec_limits = NULL,
                     sec_label = NULL,
                     sec_ylab = NULL,
                     sec_colour = NULL,
                     sec_linewidth = 0.55,
                     sec_points = FALSE,
                     palette = "qualitative",
                     colour_index = NULL,
                     color_index = NULL,
                     index = NULL,
                     pct_axis = FALSE,
                     value_accuracy = NULL,
                     value_breaks = NULL,
                     value_limits = NULL,
                     x_lim = NULL,
                     x_lim_follow_data = FALSE,
                     zeroline = TRUE,
                     reverse_legend = FALSE,
                     legend_ncol = NULL,
                     facet = NULL,
                     facet_ncol = NULL,
                     facet_scales = "fixed",
                     legend = "bottom",
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
  if (is.null(colour_index)) colour_index <- color_index
  .cpb_idx <- cpb_resolve_index(colour_index, index, palette, !missing(palette), "colour_index")
  index <- .cpb_idx$index
  palette <- .cpb_idx$palette
  orientation <- match.arg(orientation)
  sec_type <- match.arg(sec_type)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  lower <- rlang::enquo(lower)
  upper <- rlang::enquo(upper)
  colour <- rlang::enquo(colour)
  group <- rlang::enquo(group)
  facet <- rlang::enquo(facet)
  sec_y <- rlang::enquo(sec_y)
  has_colour <- !rlang::quo_is_null(colour)
  has_group <- !rlang::quo_is_null(group)
  has_sec <- !rlang::quo_is_null(sec_y)

  if (has_sec) {
    if (orientation == "horizontal") {
      stop("`sec_y` is only supported for vertical dot charts (the ",
           "secondary axis is drawn on the right of the value axis); ",
           "pass orientation = \"vertical\".", call. = FALSE)
    }
    if (has_colour) {
      stop("`sec_y` cannot be combined with `colour`: both need the ",
           "colour aesthetic and its one legend, so `sec_y` is only ",
           "supported for otherwise unmapped points.", call. = FALSE)
    }
    if (has_group) {
      stop("`sec_y` and `group` cannot be combined: the bold group ",
           "headings and the secondary axis both claim the space beside ",
           "the panel.", call. = FALSE)
    }
  }

  # the usual default is CPB pink (index 2), but sec_y already claims
  # pink for its own line by default (see cpb_single_colour(sec_colour,
  # 2) below), so the primary points fall back to CPB blue instead --
  # the same two-colour pairing cpb_line() and cpb_box() use
  single_colour <- cpb_single_colour(point_colour, if (has_sec) 6 else 2)

  slots <- NULL
  if (has_group) {
    # same two-level category axis as cpb_box(): bold heading rows
    # above the categories they collect, one shared value axis
    slots <- cpb_group_heading_positions(rlang::eval_tidy(x, data),
      rlang::eval_tidy(group, data),
      gap = group_gap
    )
    data <- as.data.frame(data)
    data[["cpb__x"]] <- slots$pos[match(
      as.character(rlang::eval_tidy(x, data)),
      slots$cat
    )]
    x <- rlang::quo(.data[["cpb__x"]])
  }

  # the interaction keeps one interval per category when the category
  # axis is numeric (the grouped-slots layout) and colour is mapped
  if (has_colour) {
    mapping_interval <- ggplot2::aes(
      x = !!x, ymin = !!lower, ymax = !!upper,
      colour = !!colour,
      group = interaction(!!x, !!colour)
    )
    mapping_point <- ggplot2::aes(
      x = !!x, y = !!y, colour = !!colour,
      group = interaction(!!x, !!colour)
    )
  } else {
    mapping_interval <- ggplot2::aes(
      x = !!x, ymin = !!lower, ymax = !!upper,
      group = !!x
    )
    mapping_point <- ggplot2::aes(x = !!x, y = !!y, group = !!x)
  }

  p <- ggplot2::ggplot(data)

  # the reference line sits underneath the estimates
  if (isTRUE(zeroline)) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0, colour = "black",
      linewidth = 0.25
    )
  }

  interval_args <- list(mapping = mapping_interval, width = cap_width,
                        linewidth = linewidth)
  point_args <- list(mapping = mapping_point, size = size,
                     show.legend = TRUE, ...)
  if (!has_colour) {
    interval_args$colour <- single_colour
    point_args$colour <- single_colour
  }
  p <- p +
    do.call(ggplot2::geom_errorbar, interval_args) +
    do.call(ggplot2::geom_point, point_args)

  # estimates do not grow from the axis (no forced zero baseline), but
  # both ends are still drawn flush to the lower-upper (and point) range
  axis_values <- c(
    rlang::eval_tidy(lower, data), rlang::eval_tidy(upper, data),
    rlang::eval_tidy(y, data)
  )
  scale_args <- cpb_flush_scale_args(
    axis_values = axis_values, pct_axis = pct_axis,
    value_accuracy = value_accuracy,
    value_breaks = value_breaks,
    value_limits = value_limits
  )

  # sec_y is drawn alongside the points, mapped onto this same flush
  # range, and read off its own axis on the right; see the "sec_y
  # helpers" block near the top of this file for how that mapping and
  # its axis labels are kept in sync with each other.
  if (has_sec) {
    sec_vals <- rlang::eval_tidy(sec_y, data)
    sec_map <- cpb_sec_map(sec_vals, sec_limits, scale_args$limits[[1]], scale_args$limits[[2]])
    sec_lab <- if (is.null(sec_label)) rlang::as_label(sec_y) else sec_label
    sec_col <- cpb_single_colour(sec_colour, 2)
    p <- cpb_sec_layer(p, data, x, sec_vals, sec_map, sec_type,
                       sec_col, sec_lab, sec_linewidth, sec_points)
    scale_args$sec.axis <- cpb_sec_axis(sec_map, scale_args$breaks)
  }

  # the grouped layout draws its bold headings outside the panel, and
  # sec_y's own markers can legitimately land exactly on the flush
  # value-axis boundary (its default range runs to the data's own
  # max) -- both would otherwise get half-cropped by the panel edge,
  # so clipping is turned off in both cases
  clip <- if (has_group || has_sec) "off" else "on"
  p <- cpb_apply_coord(
    p, orientation, x_lim, value_limits, clip,
    x, data, x_lim_follow_data, has_group
  )

  if (length(scale_args)) {
    p <- p + do.call(ggplot2::scale_y_continuous, scale_args)
  }
  if (has_sec && !is.null(sec_ylab)) {
    # mirrors the left-hand unit that `ylab` puts in the subtitle:
    # right-aligned, italic, on the line just above the panel
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf, label = sec_ylab,
      hjust = 1, vjust = -0.9, fontface = "italic",
      size = 7 / ggplot2::.pt, family = cpb_font_family()
    )
  }

  if (has_group) {
    cat_rows <- slots[!slots$heading, , drop = FALSE]
    head_rows <- slots[slots$heading, , drop = FALSE]
    p <- p + ggplot2::scale_x_continuous(
      breaks = cat_rows$pos,
      labels = cat_rows$label,
      limits = range(slots$pos) + c(-0.9, 0.9),
      expand = ggplot2::expansion(add = 0)
    )
    if (nrow(head_rows)) {
      p <- p + if (orientation == "horizontal") {
        ggplot2::annotate("text",
          x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 1.03, vjust = 0.5,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family()
        )
      } else {
        ggplot2::annotate("text",
          x = head_rows$pos, y = -Inf,
          label = head_rows$label, hjust = 0.5, vjust = 2.6,
          fontface = "bold", size = 7 / ggplot2::.pt, family = cpb_font_family()
        )
      }
    }
  }

  if (has_colour) {
    p <- p + cpb_discrete_scale("colour", index, palette)
    p <- cpb_add_legend_guide(p, "colour", reverse_legend, legend_ncol)
  }

  p <- cpb_add_facet(p, facet, facet_ncol, facet_scales)

  # as in cpb_box(): vertically the value-axis label doubles as the
  # subtitle, horizontally the value axis is drawn at the bottom after
  # coord_flip(), where a real axis title is appropriate
  lab_x <- ylab
  lab_y <- xlab
  if (orientation == "vertical") {
    lab_y <- NULL
    if (is.null(subtitle) && !is.null(xlab)) subtitle <- xlab
  }
  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(
      title = title, subtitle = subtitle, x = lab_x, y = lab_y,
      colour = colourlab
    ) +
    cpb_wrapper_theme()
}

# donut ----

#' A CPB-styled donut chart
#'
#' Thin wrapper around [ggplot2::geom_col()] and [ggplot2::coord_polar()]
#' with CPB theming and colour scale applied, for a single "share of
#' total" breakdown: one ring, one wedge per category. Draws no value
#' axis -- there is nothing on it to label -- so read wedge sizes off
#' the legend or the data itself.
#'
#' @param data A data.frame or data.table with one row per category.
#' @param fill Column mapped to the fill aesthetic (tidy eval), i.e.
#'   the category each wedge represents.
#' @param y Column holding each wedge's value (tidy eval). Must be
#'   non-negative -- a wedge with a negative angle has no sensible
#'   meaning. Also drives the percentage shown by `wedge_labels` and
#'   `legend_pct`: always computed as this row's share of the sum of
#'   `y`, regardless of what unit `y` itself is in.
#' @param label Optional column (tidy eval) holding a ready-formatted
#'   value string per wedge, e.g. `"5,1 miljoen"` -- printed as-is, not
#'   derived from `y`. `NULL` (default): the wedge label is just its
#'   percentage share; supplying `label` prints `"<label> (<share>%)"`
#'   instead.
#' @param wedge_labels If `TRUE` (default), print a value/percentage
#'   label for every wedge (see `label`). Either style always sits at
#'   its wedge's own angular midpoint, using the same stacking order
#'   as the wedges themselves, so it can't drift out of sync. Very
#'   thin or many similarly-sized wedges are not treated specially --
#'   `"wedge"` labels can overlap their neighbours, and `"leader"`
#'   labels can overlap each other outside the ring.
#' @param label_style Where the label sits: `"wedge"` (default,
#'   printed on the wedge itself) or `"leader"` (printed just outside
#'   the ring, connected to the wedge by a line). `"leader"`'s line
#'   has two parts: a short tick straight out from the wedge's own
#'   outer edge, then a second part that approximates a horizontal run
#'   out to the label, with every same-side label lined up at a shared
#'   distance. Wedges whose ticks would otherwise land close enough to
#'   overlap (several thin wedges bunched together) get nudged apart
#'   into their own row instead, so `"leader"` stays legible with
#'   many, or very unevenly sized, wedges where `"wedge"` would not.
#' @param label_colour Colour of the printed wedge/leader labels.
#'   Defaults to `"black"`; pass `"white"` (or another colour) if your
#'   own `palette`/`index` choice comes out darker/more saturated (for
#'   `"wedge"` labels only -- `"leader"` labels sit outside the ring,
#'   against the plot background, so contrast doesn't depend on the
#'   wedge colours).
#' @param leader_length How far the `"leader"`-style line's first,
#'   radial part extends beyond the ring's outer edge before bending
#'   towards the label, in the same units as `ring_width`. Defaults to
#'   `0.15`. Ignored when `label_style = "wedge"`.
#' @param legend_pct If `TRUE`, suffix every legend entry with its
#'   percentage share too, e.g. `"gas (45%)"`. Independent of
#'   `wedge_labels` -- either, both, or neither can be on. Defaults to
#'   `FALSE`.
#' @param label_accuracy Rounding accuracy for every percentage this
#'   function prints (`wedge_labels` and `legend_pct` alike), passed
#'   to [label_pct_nl()]. Defaults to `1` (whole percentage points).
#' @param ring_width Width of the ring, from just over `0` (a thin
#'   ring around a large hole) to `2` (no hole at all -- a full pie).
#'   Defaults to `0.6`.
#' @param palette CPB palette to use for `fill`; one of
#'   `"qualitative"` (default), `"discr"`, `"sequential"`
#'   (pink ramp), or `"blues"` (blue ramp).
#' @param index Optional integer vector of palette positions, forwarded
#'   to [scale_fill_cpb_manual()] instead of the default
#'   [scale_fill_cpb_d()] when supplied.
#' @param reverse_legend If `TRUE`, reverse the fill legend order via
#'   `guide_legend(reverse = TRUE)`. Defaults to `FALSE`.
#' @param legend_ncol Number of columns to lay the legend keys out in,
#'   passed to `guide_legend(ncol = ...)`. `NULL` (default) keeps
#'   ggplot2's own single-row/column layout.
#' @param legend Legend position, forwarded to [theme_cpb()].
#' @param flush_legend,legend_key_size Forwarded to [theme_cpb()] for
#'   per-figure deviations from the house defaults. Unlike the other
#'   wrappers, `minor`, `ticks`, `axis_text_size`, `grid_colour` and
#'   `grid_linewidth` are not exposed here: a donut draws no axis or
#'   gridlines for them to affect.
#' @param title,subtitle Plot title/subtitle.
#' @param filllab Legend title override; defaults to `NULL` (no
#'   title), matching CPB house style.
#' @param ... Further arguments passed to [ggplot2::geom_col()].
#' @return A `ggplot` object.
#' @examples
#' df <- data.frame(
#'   bron  = c("gas", "elektriciteit", "warmte", "overig"),
#'   share = c(45, 30, 15, 10),
#'   mld   = c("4,5 mld", "3,0 mld", "1,5 mld", "1,0 mld")
#' )
#' cpb_donut(df, fill = bron, y = share, label = mld,
#'   title = "Energiemix",
#'   index = c(6, 2, 5, 1)
#' )
#' @export
cpb_donut <- function(data, fill, y,
                      label = NULL,
                      wedge_labels = TRUE,
                      label_style = c("wedge", "leader"),
                      label_colour = "black",
                      leader_length = 0.15,
                      legend_pct = FALSE,
                      label_accuracy = 1,
                      ring_width = 0.6,
                      palette = "qualitative",
                      index = NULL,
                      reverse_legend = FALSE,
                      legend_ncol = NULL,
                      legend = "bottom",
                      flush_legend = TRUE,
                      legend_key_size = NULL,
                      title = NULL,
                      subtitle = NULL,
                      filllab = NULL,
                      ...) {
  fill <- rlang::enquo(fill)
  y <- rlang::enquo(y)
  label <- rlang::enquo(label)
  has_label <- !rlang::quo_is_null(label)
  label_style <- match.arg(label_style)

  if (ring_width <= 0 || ring_width > 2) {
    stop("`ring_width` must be greater than 0 and at most 2 (2 draws a ",
      "full pie, with no hole).",
      call. = FALSE
    )
  }
  yvals <- rlang::eval_tidy(y, data)
  if (any(yvals < 0, na.rm = TRUE)) {
    stop("`y` must be non-negative: a wedge cannot have a negative angle.",
      call. = FALSE
    )
  }

  fmt_pct <- label_pct_nl(accuracy = label_accuracy)
  pct <- yvals / sum(yvals, na.rm = TRUE) * 100

  # a single constant x stacks every fill level into one bar; wrapping
  # that one stacked bar in coord_polar() turns it into a ring. The
  # hole is the gap between xlim()'s lower bound (0) and the bar's own
  # inner edge (x_pos - ring_width / 2) -- a wider ring_width shrinks
  # that gap, until ring_width = 2 closes it entirely (a full pie).
  x_pos <- 1
  data <- as.data.frame(data)
  data[["cpb__wedge_label"]] <- if (has_label) {
    paste0(rlang::eval_tidy(label, data), " (", fmt_pct(pct), ")")
  } else {
    fmt_pct(pct)
  }

  outer_edge <- x_pos + ring_width / 2

  # the base layer, without a coord/scale yet -- "leader" needs to work
  # out how much radial room its labels actually need before the x
  # scale can be sized correctly (see below), and a plain geom_col()
  # layer's own computed ymin/ymax (needed for that) do not depend on
  # which coord or scale limits eventually get attached to the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = x_pos, y = !!y, fill = !!fill)) +
    ggplot2::geom_col(width = ring_width, key_glyph = "rect", ...)

  leader_data <- NULL
  if (isTRUE(wedge_labels) && label_style == "leader") {
    # Read back geom_col()'s own computed ymin/ymax (rather than
    # recomputing the stacking order by hand) so the leader lines can
    # never disagree with where the wedges actually ended up -- same
    # one-source-of-truth reasoning as position_stack() uses for
    # "wedge" below, just needed as real numbers here instead of left
    # to ggplot2 to draw.
    wedge_data <- ggplot2::ggplot_build(p)$data[[1]]
    ymid <- (wedge_data$ymin + wedge_data$ymax) / 2
    total <- sum(yvals, na.rm = TRUE)
    theta <- ymid / total * 2 * pi
    on_right <- theta >= 0 & theta <= pi

    # The line has two parts: a short radial tick from the wedge's own
    # outer edge (always at the wedge's own true angle, so it always
    # points at exactly the right wedge), then a second part that
    # approximates a horizontal run out to the label.
    #
    # coord_polar()'s own data space is fundamentally angular -- there
    # is no way to ask it for "a horizontal line" directly the way
    # coord_cartesian() would let you. So that second part is built
    # the other way around: first decide the real Cartesian point the
    # label needs to sit at (same height as the tick, out at a shared
    # distance per side -- and nudged apart, see below), then invert
    # coord_polar()'s own transform to find which (x, y) *in its data
    # space* renders at exactly that Cartesian point. Feed that back
    # in as an ordinary aes(x =, y =) position and coord_polar() draws
    # a straight line to it, same as any other segment -- which, by
    # construction, comes out horizontal-ish in the final image.
    r_bend <- outer_edge + leader_length
    bend_x <- r_bend * sin(theta)
    bend_y <- r_bend * cos(theta)

    # Thin wedges bunched close together would otherwise get their
    # labels stacked at (near) the same height. Working top to bottom,
    # separately for each side (a left-side and a right-side label at
    # a similar height aren't competing for the same space), push any
    # label whose tick is too close to the previous one further down.
    min_row_gap <- 0.16
    label_y <- bend_y
    for (side in list(which(on_right), which(!on_right))) {
      if (length(side) < 2) next
      side <- side[order(label_y[side], decreasing = TRUE)]
      for (i in seq_along(side)[-1]) {
        cur <- side[i]
        prev <- side[i - 1]
        if (label_y[prev] - label_y[cur] < min_row_gap) {
          label_y[cur] <- label_y[prev] - min_row_gap
        }
      }
    }
    # every same-side label lines up at one shared horizontal distance
    # (plus a little extra for the text itself, so the line doesn't
    # run straight into the first character)
    horiz_reach <- r_bend + 0.35
    line_x <- ifelse(on_right, horiz_reach, -horiz_reach)
    text_x <- ifelse(on_right, horiz_reach + 0.06, -horiz_reach - 0.06)

    # invert the transform: given the Cartesian point a point needs to
    # end up at, find the (r, y) pair that renders there
    to_polar_data <- function(x_cart, y_cart) {
      r <- sqrt(x_cart^2 + y_cart^2)
      th <- atan2(x_cart, y_cart) %% (2 * pi)
      list(r = r, y = th / (2 * pi) * total)
    }
    line_pos <- to_polar_data(line_x, label_y)
    text_pos <- to_polar_data(text_x, label_y)

    leader_data <- data.frame(
      x_wedge = outer_edge, x_bend = r_bend,
      x_line  = line_pos$r, x_text = text_pos$r,
      y_wedge = ymid, y_line = line_pos$y, y_text = text_pos$y,
      hjust   = ifelse(on_right, 0, 1),
      wedge_label = data[["cpb__wedge_label"]]
    )
  }

  # the panel needs enough radial room for whatever "leader" actually
  # computed above (the ring itself only ever needs a little margin
  # past its own outer edge); xlim()'s limits genuinely drop any point
  # outside them rather than just visually cropping it, so this has to
  # cover the real maximum, not just approximate it
  xlim_max <- if (!is.null(leader_data)) {
    max(c(outer_edge + 0.05, leader_data$x_line, leader_data$x_text)) + 0.05
  } else {
    outer_edge + 0.05
  }
  p <- p +
    ggplot2::coord_polar(theta = "y", clip = if (label_style == "leader") "off" else "on") +
    ggplot2::xlim(0, xlim_max)

  if (isTRUE(wedge_labels) && label_style == "wedge") {
    # position_stack(vjust = 0.5) centres the label on its own wedge's
    # angular span, using the exact same stacking logic as geom_col()
    # itself (same fill aesthetic, same data), so the two can never
    # drift out of sync with each other
    p <- p + ggplot2::geom_text(
      ggplot2::aes(x = x_pos, y = !!y, label = .data[["cpb__wedge_label"]]),
      position = ggplot2::position_stack(vjust = 0.5),
      colour = label_colour, size = 7 / ggplot2::.pt,
      family = cpb_font_family()
    )
  } else if (!is.null(leader_data)) {
    p <- p +
      ggplot2::geom_segment(
        data = leader_data,
        ggplot2::aes(x = x_wedge, xend = x_bend, y = y_wedge, yend = y_wedge),
        inherit.aes = FALSE, colour = "grey30", linewidth = 0.3
      ) +
      ggplot2::geom_segment(
        data = leader_data,
        ggplot2::aes(x = x_bend, xend = x_line, y = y_wedge, yend = y_line),
        inherit.aes = FALSE, colour = "grey30", linewidth = 0.3
      ) +
      ggplot2::geom_text(
        data = leader_data,
        ggplot2::aes(x = x_text, y = y_text, label = wedge_label, hjust = hjust),
        inherit.aes = FALSE, colour = label_colour, size = 7 / ggplot2::.pt,
        family = cpb_font_family()
      )
  }

  fill_labels <- ggplot2::waiver()
  if (isTRUE(legend_pct)) {
    fill_chr <- as.character(rlang::eval_tidy(fill, data))
    pct_by_level <- tapply(pct, fill_chr, sum)
    fill_labels <- function(breaks) {
      paste0(breaks, " (", fmt_pct(pct_by_level[breaks]), ")")
    }
  }
  p <- p + cpb_discrete_scale("fill", index, palette, labels = fill_labels)
  p <- cpb_add_legend_guide(p, "fill", reverse_legend, legend_ncol)

  subtitle <- cpb_reserve_subtitle(title, subtitle)

  p +
    ggplot2::labs(title = title, subtitle = subtitle, fill = filllab) +
    theme_cpb(
      legend = legend, flush_legend = flush_legend,
      legend_key_size = legend_key_size, grid = "none", ticks = FALSE
    ) +
    ggplot2::theme(
      axis.text  = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line  = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
