# save.R ----
#
# Figure export helper. Width is strict and tied to the CPB page format
# (half or full page); height defaults to the CPB report height but has
# a "presentation" preset and can always be overridden explicitly.

# cpb_donut()'s panel_size, cpb_add_sec_ylab()'s sec_ylab caption, and
# cpb_map()'s aspect fit (see cpb_fix_panel_size()/
# cpb_add_sec_ylab_grob() below, and cpb_map_aspect handling in
# save_cpb() itself) are only ever exact through save_cpb() -- a bare
# print(), a knitr chunk that never calls save_cpb(), a Shiny render,
# all fall back to an approximate placement instead, with nothing to
# say so. Every wrapper that sets one of these three attributes also
# tags its plot with the "cpb_plot" class so this fires; save_cpb()
# itself never triggers it, since it never calls print() on the plot.
#
# Warned once per distinct feature combination per session
# (rlang::warn()'s own .frequency_id mechanism), not on every print --
# cpb_donut()/cpb_map() set their attribute unconditionally, so an
# every-time warning would fire on every single donut/map a caller
# glances at in the console, which is normal, expected use, not a
# mistake to flag.
#' @export
print.cpb_plot <- function(x, ...) {
  features <- c(
    if (!is.null(attr(x, "cpb_panel_size"))) "a fixed panel size (cpb_donut())",
    if (!is.null(attr(x, "cpb_sec_ylab"))) "a secondary-axis caption (sec_ylab)",
    if (!is.null(attr(x, "cpb_map_aspect"))) "a geographic aspect fit (cpb_map())"
  )
  if (length(features)) {
    rlang::warn(
      paste0(
        "ggcpb: this plot has ", paste(features, collapse = " and "),
        ", which only render(s) exactly when written out through ",
        "save_cpb() -- a bare print() (this one included) shows an ",
        "approximate placement instead."
      ),
      .frequency = "once",
      .frequency_id = paste("cpb_plot_approx_print", paste(features, collapse = "|"))
    )
  }
  NextMethod()
}

# The tick-label gap is 1.5% of the figure width. theme_cpb() sets the
# half-page gap. It is rescaled to `width_cm` only on a plot that still
# has that gap, so a plot without theme_cpb() keeps its own.
# @noRd
cpb_scale_y_lab_gap <- function(plot, width_cm) {
  el <- plot$theme$axis.text.y.left
  half <- grid::unit(cpb_y_lab_gap_cm(cpb_page_width_cm[["half"]]), "cm")
  if (!inherits(el, "element_text") || length(el$margin) != 4 ||
      !isTRUE(all.equal(grid::convertWidth(el$margin[2], "cm", valueOnly = TRUE),
                        grid::convertWidth(half, "cm", valueOnly = TRUE)))) {
    return(plot)
  }
  gap_cm <- cpb_y_lab_gap_cm(width_cm)
  plot + ggplot2::theme(
    axis.text.y.left  = ggplot2::element_text(margin = ggplot2::margin(r = gap_cm, unit = "cm"), inherit.blank = TRUE),
    axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = gap_cm, unit = "cm"), inherit.blank = TRUE)
  )
}

# ggplot2 sizes the panel from whatever room is left after title and
# legend take what they need -- usually right, but it makes the data
# area grow or shrink with title/legend length. cpb_donut() instead
# wants its ring pinned to a fixed size regardless, with chrome that
# doesn't fit overflowing rather than shrinking it.
#
# The panel is the only "null" (elastic) cell in the plot's gtable --
# the grid of rows/columns ggplot2 lays a plot out into; every other
# cell is already an absolute size via theme_cpb(). A "null" unit only
# resolves to a real number against a specific grid.layout() at a given
# size -- read alone it's just zero. So this pushes the same viewport
# save_cpb() would render into, then reads each column/row back one at
# a time (all at once would resolve them against each other, not the
# viewport), freezing title/legend at their normal size before the
# panel cell gets overridden.
#
# @param plot A ggplot object.
# @param size Panel size in inches: a single number for a square panel
#   (coord_polar() is aspect-locked, so this is what cpb_donut() uses),
#   or `c(width, height)`.
# @param page_width,page_height The size (inches) the plot would
#   otherwise have been saved at -- what every other cell's size gets
#   resolved against.
# @return A gtable with its panel cell(s) fixed to `size`, and every
#   other cell frozen to what it would be at `page_width`/`page_height`.
# @noRd
cpb_resolve_gtable_units <- function(g, page_width, page_height) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp))
  ragg::agg_png(tmp, width = page_width, height = page_height, units = "in", res = 72)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(
      nrow = nrow(g), ncol = ncol(g), widths = g$widths, heights = g$heights
    )
  ))
  g$widths <- grid::unit(vapply(seq_len(ncol(g)), function(i) {
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = i))
    on.exit(grid::popViewport())
    grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  }, numeric(1)), "in")
  g$heights <- grid::unit(vapply(seq_len(nrow(g)), function(i) {
    grid::pushViewport(grid::viewport(layout.pos.row = i, layout.pos.col = 1))
    on.exit(grid::popViewport())
    grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  }, numeric(1)), "in")
  grid::popViewport()
  g
}

cpb_fix_panel_size <- function(plot, size, page_width, page_height) {
  size <- rep_len(size, 2)
  g <- ggplot2::ggplotGrob(plot)
  g <- cpb_resolve_gtable_units(g, page_width, page_height)

  panel <- which(g$layout$name == "panel")
  if (length(panel) == 0) {
    stop("save_cpb(): `panel_size` was given but no \"panel\" cell was ",
      "found in the plot's layout.",
      call. = FALSE
    )
  }
  panel_col <- unique(g$layout$l[panel])
  panel_row <- unique(g$layout$t[panel])
  old_w <- grid::convertWidth(g$widths[panel_col], "in", valueOnly = TRUE)
  old_h <- grid::convertHeight(g$heights[panel_row], "in", valueOnly = TRUE)

  g$widths[panel_col] <- grid::unit(size[[1]], "in")
  g$heights[panel_row] <- grid::unit(size[[2]], "in")

  # Title/subtitle/legend span the plot's full width (theme_cpb() sets
  # plot.title.position = "plot"), so resizing the panel's own
  # column/row directly would narrow them too. Instead the size
  # difference is split between the axis gutter cells flanking the
  # panel (normally near-empty, since a donut turns its axes off) --
  # this centres the fixed-size panel without touching anyone else's
  # span. If the panel needs to grow and there's no gutter left to take
  # from, cpb_ggsave_grob() reports the extra room needed.
  cpb_grow_flank <- function(units, idx, delta, convert) {
    if (length(idx) != 1) return(units)
    cur <- convert(units[idx], "in", valueOnly = TRUE)
    units[idx] <- grid::unit(max(cur + delta, 0), "in")
    units
  }
  g$widths <- cpb_grow_flank(
    g$widths, g$layout$l[g$layout$name == "axis-l"],
    (old_w - size[[1]]) / 2, grid::convertWidth
  )
  g$widths <- cpb_grow_flank(
    g$widths, g$layout$l[g$layout$name == "axis-r"],
    (old_w - size[[1]]) / 2, grid::convertWidth
  )
  g$heights <- cpb_grow_flank(
    g$heights, g$layout$t[g$layout$name == "axis-t"],
    (old_h - size[[2]]) / 2, grid::convertHeight
  )
  g$heights <- cpb_grow_flank(
    g$heights, g$layout$t[g$layout$name == "axis-b"],
    (old_h - size[[2]]) / 2, grid::convertHeight
  )
  g
}

# sec_ylab (see cpb_add_sec_ylab() in wrappers.R) is drawn twice: once
# approximately, as an annotate() layer, so a bare print()/knitr
# display still shows *something*; here save_cpb() lifts that
# placeholder back out and draws an exact replacement
# (cpb_add_sec_ylab_grob()) against the plot's actual rendered gtable
# instead of a build-time guess.
# @return A list with the (possibly unchanged) `plot` and the `label`
#   to re-draw exactly, or `label = NULL` when there was nothing to do.
# @noRd
cpb_take_sec_ylab <- function(plot) {
  info <- attr(plot, "cpb_sec_ylab")
  if (is.null(info)) {
    return(list(plot = plot, label = NULL))
  }
  # matched by identity, not stored position -- a caller who reorders
  # plot$layers afterward (e.g. to draw something underneath
  # everything) would otherwise delete the wrong layer instead of this
  # caption's own placeholder (see cpb_add_sec_ylab() in wrappers.R)
  idx <- which(vapply(plot$layers, identical, logical(1), y = info$layer_obj))
  if (length(idx) == 1) {
    plot$layers[[idx]] <- NULL
  }
  list(plot = plot, label = info$label)
}

# Places sec_ylab on the subtitle row, the row of ylab's caption,
# right-aligned 0.45 cm from the figure's right edge and top-anchored
# like theme_cpb()'s plot.subtitle.
#
# It spans the whole table, so it works before the gtable's elastic
# units are resolved to an output size.
# @return The gtable with the caption added, or NULL if there's no
#   "subtitle" row.
# @noRd
cpb_place_sec_ylab <- function(g, label) {
  subtitle_idx <- which(g$layout$name == "subtitle")
  if (length(subtitle_idx) != 1) return(NULL)
  row <- g$layout$t[subtitle_idx]
  # styled like the subtitle (ylab's caption) it sits next to
  subtitle <- cpb_find_grob(g$grobs[[subtitle_idx]], "text")
  if (is.null(subtitle)) return(NULL)

  grob <- grid::textGrob(
    label, x = grid::unit(1, "npc") - grid::unit(cpb_labels_margin_cm, "cm"),
    y = grid::unit(1, "npc"), hjust = 1, vjust = 1, gp = subtitle$gp
  )
  gtable::gtable_add_grob(g, grob, t = row, l = 1, r = ncol(g), clip = "off", name = "sec-ylab")
}

# With a right axis the right margin is as wide as the left one, so the
# panel ends as far from the right edge as it starts from the left,
# whatever width the right tick labels need (wider ones run into the
# 0.45 cm edge margin).
# @return The gtable with its outer right margin set to mirror the left
#   side, or NULL when it has no right axis.
# @noRd
cpb_mirror_sec_axis_margin <- function(g) {
  is_axis_r <- grepl("^axis-r", g$layout$name) &
    !vapply(g$grobs, inherits, logical(1), what = "zeroGrob")
  if (!any(is_axis_r)) return(NULL)
  is_panel <- grepl("^panel", g$layout$name)
  first <- min(g$layout$l[is_panel])
  last <- max(g$layout$r[is_panel])
  n <- ncol(g)
  g$widths[n] <- sum(g$widths[seq_len(first - 1)]) - sum(g$widths[(last + 1):(n - 1)])
  g
}

# save_cpb()'s way into cpb_place_sec_ylab(). It first resolves `g` to
# the fixed output size, which cpb_fix_panel_size() and the final save
# need anyway. A missing subtitle row is an error here, where print()
# falls back to an approximation: an explicit save should fail clearly.
# @noRd
cpb_add_sec_ylab_grob <- function(g, label, page_width, page_height) {
  g <- cpb_resolve_gtable_units(g, page_width, page_height)
  placed <- cpb_place_sec_ylab(g, label)
  if (is.null(placed)) {
    stop("save_cpb(): could not find the \"subtitle\" row to align ",
      "sec_ylab against; is `plot` a sec_y chart built by one of the ",
      "ggcpb wrappers?",
      call. = FALSE
    )
  }
  placed
}

# Depth-first search through a grob's `children` (gTree) and/or
# `grobs` (gtable) for the first descendant of class `what` -- reaches
# into axis title/tick-label grobs without hardcoding a nesting depth
# or field name that could shift with a ggplot2 version.
# @noRd
cpb_find_grob <- function(x, what) {
  if (is.null(x)) return(NULL)
  if (inherits(x, what)) return(x)
  kids <- list()
  if (!is.null(x$children)) kids <- c(kids, as.list(x$children))
  if (!is.null(x$grobs)) kids <- c(kids, x$grobs)
  for (k in kids) {
    found <- cpb_find_grob(k, what)
    if (!is.null(found)) return(found)
  }
  NULL
}

# ggplot2's "xlab-t"/"xlab-b" cells hold the value-axis title (the
# wrappers' xlab/ylab convention lands it there for a horizontal
# figure, see wrappers.R). theme_cpb()'s axis.title anchors it flush
# with the *panel* edge (hjust = 1), not the axis's own outermost tick
# label. Since every value axis is flush (cpb_flush_scale_args() in
# wrappers.R: highest/lowest break exactly on the panel edge, no
# expansion) and tick text is centred on its own break, that label
# overhangs the panel edge by half its width -- the title lands short
# by exactly that much, worse for a longer label (an extra "%", more
# digits).
#
# Nudges the title out by that measured half-width (same text, same
# font, not assumed), so it keeps working under any title/panel/label
# length. Best-effort: a no-op if the expected cells/grobs aren't found
# (vertical orientation, no title, unstyled plot, a future ggplot2
# change, ...) rather than erroring over a cosmetic detail.
# @return The (possibly unchanged) gtable.
# @noRd
cpb_align_value_axis_title <- function(g) {
  for (side in c("t", "b")) {
    title_idx <- which(g$layout$name == paste0("xlab-", side))
    axis_idx <- which(g$layout$name == paste0("axis-", side))
    if (length(title_idx) != 1 || length(axis_idx) != 1) next

    title_grob <- g$grobs[[title_idx]]
    if (inherits(title_grob, "zeroGrob")) next
    title_text <- cpb_find_grob(title_grob, "text")
    if (is.null(title_text) || length(title_text$hjust) != 1) next
    if (!isTRUE(title_text$hjust %in% c(0, 1))) next

    tick_text <- cpb_find_grob(g$grobs[[axis_idx]], "text")
    if (is.null(tick_text) || is.null(tick_text$label) || is.null(tick_text$x)) next
    xs <- suppressWarnings(as.numeric(tick_text$x))
    if (!length(xs) || anyNA(xs)) next

    pick <- if (title_text$hjust == 1) which.max(xs) else which.min(xs)
    label <- tick_text$label[[pick]]
    if (is.null(label) || !nzchar(label)) next

    label_width_in <- tryCatch(
      grid::convertWidth(
        grid::grobWidth(grid::textGrob(label, gp = tick_text$gp)),
        "in", valueOnly = TRUE
      ),
      error = function(e) NA_real_
    )
    if (!length(label_width_in) || is.na(label_width_in) || label_width_in <= 0) next

    shift <- grid::unit(label_width_in / 2, "in")
    new_x <- if (title_text$hjust == 1) {
      grid::unit(1, "npc") + shift
    } else {
      grid::unit(0, "npc") - shift
    }

    new_title <- grid::editGrob(title_text, x = new_x)
    title_grob <- cpb_replace_grob(title_grob, title_text, new_title)
    if (!is.null(title_grob)) g$grobs[[title_idx]] <- title_grob
  }
  g
}

# Rebuilds `parent` with `old` (matched by identity, like
# cpb_take_sec_ylab()'s layer lookup) replaced by `new`, searched
# through the same children/grobs shape cpb_find_grob() reads. Returns
# NULL if `old` is not reachable from `parent`.
# @noRd
cpb_replace_grob <- function(parent, old, new) {
  if (identical(parent, old)) return(new)
  if (!is.null(parent$children) && length(parent$children)) {
    for (i in seq_along(parent$children)) {
      replaced <- cpb_replace_grob(parent$children[[i]], old, new)
      if (!is.null(replaced)) {
        parent$children[[i]] <- replaced
        return(parent)
      }
    }
  }
  if (!is.null(parent$grobs) && length(parent$grobs)) {
    for (i in seq_along(parent$grobs)) {
      replaced <- cpb_replace_grob(parent$grobs[[i]], old, new)
      if (!is.null(replaced)) {
        parent$grobs[[i]] <- replaced
        return(parent)
      }
    }
  }
  NULL
}

# Draws a gtable straight to a device, since ggplot2::ggsave() only
# accepts a ggplot object, not an already-built grob.
#
# `width`/`height` are an escape hatch for a grob whose panel cell is
# still a "null" unit (sec_ylab-only, no panel_size): it only resolves
# once drawn at a real size. Left NULL (the cpb_fix_panel_size() case,
# every cell already absolute), the device opens at the gtable's own
# natural size instead -- forcing it back to the original page size
# would just be the "shrink the panel to fit" behaviour this avoids.
# @noRd
cpb_ggsave_grob <- function(filename, grob, dpi, device, bg, width = NULL, height = NULL, ...) {
  if (is.null(width)) {
    width <- sum(grid::convertWidth(grob$widths, "in", valueOnly = TRUE))
  }
  if (is.null(height)) {
    height <- sum(grid::convertHeight(grob$heights, "in", valueOnly = TRUE))
  }
  device(
    filename = filename, width = width, height = height,
    units = "in", res = dpi, background = bg, ...
  )
  on.exit(grDevices::dev.off())
  grid::grid.newpage()
  grid::grid.draw(grob)
  invisible(filename)
}

#' Save a plot at CPB page dimensions
#'
#' A wrapper around [ggplot2::ggsave()] that enforces the CPB page
#' widths and renders with the `ragg` device by default (needed for the
#' bundled `RijksoverheidSansText` font to render correctly).
#'
#' Width is strict: it is set by `page`, not free-form. `page = "half"`
#' gives a width of 7.5 cm, `page = "full"` 15.5 cm and `page = "small"`
#' 6.8 cm (the size for a figure in a "kader"). An explicit
#' `width` is only an escape hatch and is validated against these
#' values -- any other width errors, so a stray `width = 8` fails
#' loudly rather than silently producing an off-spec figure.
#'
#' Height defaults to 7.5 cm (the `"report"` preset), or 6.8 cm for
#' `page = "small"`, which is square too. Pass
#' `preset = "presentation"` for the 2.5 in presentation height, or set
#' `height` explicitly for anything else (e.g. a tall stacked-facet
#' export) -- an explicit `height` always wins over `preset`. A
#' `cpb_map()` plot is the one exception: left at its default (no
#' explicit `height`), its panel is instead auto-sized to the
#' boundaries' true geographic aspect ratio, so the map fills the
#' figure exactly rather than sitting letterboxed inside a
#' fixed-height page (pass `height` explicitly to opt back into a
#' fixed height, e.g. to match a neighbouring figure).
#'
#' @param filename Path to write to; passed to [ggplot2::ggsave()].
#' @param plot The plot to save; defaults to [ggplot2::last_plot()].
#' @param page `"half"` (default, 7.5 cm wide), `"full"` (15.5 cm wide)
#'   or `"small"` (6.8 cm wide and tall, for a figure in a "kader").
#'   Ignored if `width` is supplied explicitly.
#' @param preset Either `"report"` (default, 7.5 cm tall, or 6.8 cm
#'   for `page = "small"`) or `"presentation"` (2.5 in tall). Ignored
#'   if `height` is supplied explicitly.
#' @param height Explicit height in inches. `NULL` (default) uses
#'   `preset` to determine the height.
#' @param width Explicit width in inches. Must be `7.5 / 2.54`,
#'   `15.5 / 2.54` or `6.8 / 2.54`.
#'   `NULL` (default) uses `page` to determine the width.
#' @param dpi Resolution in dots per inch; defaults to `300`. CPB tall
#'   exports commonly use `dpi = 800`.
#' @param device Graphics device passed to [ggplot2::ggsave()];
#'   defaults to [ragg::agg_png()] so the bundled CPB font renders
#'   correctly.
#' @param bg Output background colour; defaults to the CPB background
#'   colour so it matches `theme_cpb()`'s on-plot background. Use
#'   `bg = NA` for a transparent background.
#' @param panel_size Pin the plot's own data area (the panel -- the
#'   ring, for `cpb_donut()`) to a constant physical size in inches,
#'   regardless of how much room the title or legend need: a number
#'   for a square panel, or `c(width, height)`. `NULL` (default) uses
#'   whatever size `plot` already asks for (some wrappers, currently
#'   `cpb_donut()`, request one on their own; pass this to override
#'   it). Anything that does not fit around a fixed-size panel -- a
#'   long title, a legend entry -- overflows past the figure's edge
#'   instead of shrinking the panel to make room.
#' @param ... Further arguments passed to [ggplot2::ggsave()] (or, when
#'   `panel_size` applies, to `device` instead).
#' @return Invisibly, the `filename` that was written.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(factor(cyl))) +
#'   geom_bar() +
#'   theme_cpb()
#' save_cpb("cyl_bar.png", p, page = "half")
#' save_cpb("cyl_bar_full.png", p, page = "full", preset = "presentation")
#' }
#' @export
save_cpb <- function(filename,
                      plot = ggplot2::last_plot(),
                      page = c("half", "full", "small"),
                      preset = c("report", "presentation"),
                      height = NULL,
                      width = NULL,
                      dpi = 300,
                      device = ragg::agg_png,
                      bg = cpb_bg,
                      panel_size = NULL,
                      ...) {
  # print.cpb_plot() (above) only exists to catch a bare print()
  # skipping the exact positioning below -- ggplot2::ggsave() itself
  # calls print() internally to render, so without this, the fast path
  # a few lines down would trigger that same warning on every ordinary
  # save_cpb() call for a cpb_donut()/cpb_map()/sec_ylab plot, exactly
  # the false alarm this is meant to avoid
  class(plot) <- setdiff(class(plot), "cpb_plot")

  # cpb_fix_panel_size()/cpb_add_sec_ylab_grob()/the map aspect fit
  # below all measure a grob's width/height via grid::convertWidth() --
  # even "in" to "in", this resolves through whatever device is
  # current. With none open (a plain Rscript run), R silently opens its
  # default -- usually "pdf" -- leaving a blank Rplots.pdf behind. A
  # throwaway device here covers every such call at once; ragg
  # specifically, since grDevices::pdf(NULL) doesn't know the bundled
  # font and warns on every text measurement.
  tmp_measure <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_measure), add = TRUE)
  ragg::agg_png(tmp_measure, width = 1, height = 1, units = "in", res = 72)
  on.exit(grDevices::dev.off(), add = TRUE)

  page <- match.arg(page)
  preset <- match.arg(preset)

  page_widths <- cpb_cm_to_in(cpb_page_width_cm)
  allowed_widths <- unname(page_widths)

  if (is.null(width)) {
    width <- unname(page_widths[[page]])
  } else if (!any(abs(width - allowed_widths) < 1e-6)) {
    stop(
      "save_cpb(): `width` must be one of the CPB page widths (",
      paste(signif(allowed_widths, 6), collapse = " or "),
      " inches); got ", width, ". Use `page = \"half\"`, ",
      "`page = \"full\"` or `page = \"small\"` instead, or pass an ",
      "explicit width matching one of these values.",
      call. = FALSE
    )
  } else {
    # an explicit width decides which page it is, and so its height
    page <- names(page_widths)[which.min(abs(width - allowed_widths))]
  }

  # NULL means height was left to us; decides below whether cpb_map()'s
  # aspect ratio gets to auto-size the panel, or an explicit height
  # wins outright, like an explicit panel_size does
  height_auto <- is.null(height)
  if (is.null(height)) {
    height <- if (preset == "presentation") 2.5 else cpb_cm_to_in(cpb_page_height_cm[[page]])
  }

  plot <- cpb_scale_y_lab_gap(plot, cpb_in_to_cm(width))

  cpb_check_title(plot$labels$title, width)
  cpb_check_half_page(plot, width)
  cpb_check_category_labels(plot, width)

  # an explicit panel_size always wins; failing that, a wrapper (only
  # cpb_donut() so far) may have already asked for one of its own
  if (is.null(panel_size)) {
    panel_size <- attr(plot, "cpb_panel_size")
  }

  # failing that, cpb_map() tags its plot with the boundaries' true
  # aspect ratio: measure the panel width at this page width (doesn't
  # depend on height, since title/legend span the full width) and pin
  # the panel to that width at the matching height, so the map fills it
  # exactly instead of sitting letterboxed. An explicit height opts
  # out, like an explicit panel_size does.
  if (is.null(panel_size) && height_auto) {
    map_aspect <- attr(plot, "cpb_map_aspect")
    if (!is.null(map_aspect)) {
      g0 <- cpb_resolve_gtable_units(ggplot2::ggplotGrob(plot), width, height)
      panel_col <- unique(g0$layout$l[g0$layout$name == "panel"])
      panel_w <- grid::convertWidth(g0$widths[panel_col], "in", valueOnly = TRUE)
      panel_size <- c(panel_w, panel_w * map_aspect)
    }
  }

  # lifts out the approximate sec_ylab layer a wrapper may have added
  # (see cpb_add_sec_ylab() in wrappers.R); sec_ylab$label is NULL, and
  # plot unchanged, when there is nothing to do
  sec_ylab <- cpb_take_sec_ylab(plot)
  plot <- sec_ylab$plot

  # cpb_align_value_axis_title() is a no-op for most figures, but
  # whether it applies can only be told by building the grob and
  # looking (see its own comment). So the common no-op case builds the
  # grob twice -- here to check, again inside ggplot2::ggsave() below
  # -- accepted over guessing eligibility from plot$labels/coordinates,
  # which would drift out of sync with wrappers.R.
  grob <- if (is.null(panel_size)) {
    ggplot2::ggplotGrob(plot)
  } else {
    cpb_fix_panel_size(plot, panel_size, width, height)
  }
  grob_aligned <- cpb_align_value_axis_title(grob)
  title_aligned <- !identical(grob_aligned, grob)
  grob <- grob_aligned

  mirrored <- cpb_mirror_sec_axis_margin(grob)
  if (!is.null(mirrored)) grob <- mirrored

  if (is.null(panel_size) && is.null(sec_ylab$label) && !title_aligned &&
      is.null(mirrored)) {
    ggplot2::ggsave(
      filename = filename,
      plot     = plot,
      width    = width,
      height   = height,
      units    = "in",
      dpi      = dpi,
      device   = device,
      bg       = bg,
      ...
    )
  } else {
    if (!is.null(sec_ylab$label)) {
      grob <- cpb_add_sec_ylab_grob(grob, sec_ylab$label, width, height)
    }
    if (!is.null(panel_size)) {
      # fixing the panel can need a bit more or less room than the
      # page's usual width/height (see cpb_ggsave_grob()), so what's
      # actually written is reported below, not the requested size
      width <- sum(grid::convertWidth(grob$widths, "in", valueOnly = TRUE))
      height <- sum(grid::convertHeight(grob$heights, "in", valueOnly = TRUE))
      cpb_ggsave_grob(
        filename = filename,
        grob     = grob,
        dpi      = dpi,
        device   = device,
        bg       = bg,
        ...
      )
    } else {
      # no panel_size: the panel cell is still a "null" unit, so it
      # must be told the page size to resolve against rather than
      # (wrongly) reading one back off the still-elastic grob
      cpb_ggsave_grob(
        filename = filename,
        grob     = grob,
        dpi      = dpi,
        device   = device,
        bg       = bg,
        width    = width,
        height   = height,
        ...
      )
    }
  }

  tcat("ggcpb: wrote ", filename, " (", round(width, 2), " x ", round(height, 2), " in, ", dpi, " dpi)")

  invisible(filename)
}

#' Warn when a title is too long for the page width
#'
#' The bold 9 pt title is drawn on one line unless it contains explicit
#' `"\n"` breaks. A single line that runs wider than the panel is
#' clipped or shrinks the figure, so this estimates the per-line
#' character budget for the given width (9 pt bold within the house
#' margins) and warns -- once -- when the longest title line exceeds it,
#' suggesting a manual `"\n"` break. Multi-line titles are checked line
#' by line, so a title already broken with `"\n"` passes.
#'
#' @param title The plot title (may be `NULL`, `""`, or contain `"\n"`).
#' @param width Figure width in inches.
#' @return Invisibly `TRUE` if every line fits, `FALSE` otherwise.
#' @noRd
cpb_check_title <- function(title, width) {
  if (is.null(title) || !any(nzchar(title))) return(invisible(TRUE))
  # usable text width: figure width minus the 10 pt left + 10 pt right
  # plot margins, in points; ~5 pt per 9 pt bold glyph on average
  budget <- floor((width * 72 - 20) / 5.0)
  lines <- strsplit(as.character(title), "\n", fixed = TRUE)[[1]]
  longest <- max(nchar(lines))
  if (longest > budget) {
    warning(
      "ggcpb: the title's longest line is ", longest, " characters, which ",
      "is likely too wide for a ", round(width, 2), " in figure (about ",
      budget, " fit). Break it over two lines with \"\\n\".",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' Warn when the category labels are too long to sit side by side
#'
#' Each category on a discrete axis only gets its share of the panel's
#' width, so a handful of long names ("120% wml - mod.") run into each
#' other rather than wrapping or rotating -- the house style keeps them
#' horizontal. The fix is always the label, not the figure: shorten it,
#' or break it over two lines with `"\n"`. Estimated the same way
#' `cpb_check_title()` estimates a title's width (7 pt regular within
#' the house margins), and deliberately generous about how much room a
#' slot has, so this flags labels that genuinely collide rather than
#' ones that merely come close.
#'
#' @param plot The plot passed to `save_cpb()`.
#' @param width Figure width in inches, already resolved from `page`/
#'   `width`.
#' @return Invisibly `TRUE` if the labels fit, `FALSE` otherwise.
#' @noRd
cpb_check_category_labels <- function(plot, width) {
  built <- tryCatch(ggplot2::ggplot_build(plot), error = function(e) NULL)
  if (is.null(built)) return(invisible(TRUE))
  labels <- tryCatch(
    built$layout$panel_params[[1]]$x$get_labels(),
    error = function(e) NULL
  )
  labels <- labels[!is.na(labels)]
  if (length(labels) < 2 || !is.character(labels)) return(invisible(TRUE))

  # a label already broken with "\n" is measured by its longest line,
  # the same way cpb_check_title() measures a title
  widest <- max(vapply(
    strsplit(labels, "\n", fixed = TRUE),
    function(lines) max(nchar(lines)), integer(1)
  ))
  # usable width in points, minus the house left+right plot margins;
  # ~3.5 pt per 7 pt glyph on average
  slot <- (width * 72 - 20) / length(labels)
  if (widest * 3.5 > slot) {
    warning(
      "ggcpb: the longest category label is ", widest, " characters, which ",
      "is too long for ", length(labels), " labels side by side on a ",
      round(width, 2), " in figure (about ", max(floor(slot / 3.5), 1),
      " fit). Text is too long for the category labels -- please shorten ",
      "them, or break them over two lines with \"\\n\".",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' Warn when a plot's own type is a poor fit for a half page
#'
#' Some wrappers tag their own output with a plain-English reason (via
#' the `cpb_half_page_unsuitable` attribute) when their layout
#' fundamentally needs more width than a half page gives -- an extended
#' boxplot split into several facet panels, say, or a donut chart's
#' ring plus its (often long) legend. Checked here, once, rather than
#' in each wrapper: `width` is only known for certain at save time (a
#' bare `print()`/knitr chunk never goes through save_cpb() at all, and
#' `page`/`width` can still be overridden here regardless of what the
#' wrapper itself might otherwise assume).
#'
#' @param plot The plot passed to `save_cpb()`.
#' @param width Figure width in inches, already resolved from `page`/
#'   `width`.
#' @return Invisibly `TRUE` if nothing was warned about, `FALSE`
#'   otherwise.
#' @noRd
cpb_check_half_page <- function(plot, width) {
  reason <- attr(plot, "cpb_half_page_unsuitable")
  # a half page, or the still narrower small figure
  if (is.null(reason) || !isTRUE(width < cpb_cm_to_in(cpb_page_width_cm[["half"]]) + 1e-6)) {
    return(invisible(TRUE))
  }
  # phrased so `reason` sits as the object of "room for", not the
  # subject of a verb -- a plugged-in noun phrase then never needs to
  # agree in number with anything else in the sentence
  warning(
    "ggcpb: this type of plot is not suitable for a half page: it does ",
    "not leave enough room for ", reason, ". Use page = \"full\" instead.",
    call. = FALSE
  )
  invisible(FALSE)
}
