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

# Places `label` in gtable `g` on the subtitle row (same height as
# ylab()'s own left-hand caption) and flush right against the "axis-r"
# cell (both always exist once a wrapper's sec_y has produced a right
# axis, even empty -- ggplot2 always reserves a "subtitle" row).
#
# Top-anchored (vjust = 1), not centred: theme_cpb()'s plot.subtitle is
# itself vjust = 1 with a bottom-only margin, so centring here would
# sit visibly lower than it.
#
# Flush against the cell, not centred on the tick text's own width: an
# earlier version did the latter and read as stopping short of the
# axis rather than aligned with it; flush is simpler and matches the
# published look.
#
# `page_width`/`page_height` resolve the gtable's elastic "null" units
# (see cpb_resolve_gtable_units()) for the row/column lookup below,
# even though the anchor itself no longer needs them.
# @noRd
cpb_add_sec_ylab_grob <- function(g, label, page_width, page_height) {
  row <- g$layout$t[g$layout$name == "subtitle"]
  axis_idx <- which(g$layout$name == "axis-r")
  if (length(row) != 1 || length(axis_idx) != 1) {
    stop("save_cpb(): could not find the \"subtitle\" row and/or the ",
      "\"axis-r\" column to align sec_ylab against; is `plot` a sec_y ",
      "chart built by one of the ggcpb wrappers?",
      call. = FALSE
    )
  }
  col <- g$layout$l[axis_idx]

  g <- cpb_resolve_gtable_units(g, page_width, page_height)

  grob <- grid::textGrob(
    label, x = grid::unit(1, "npc"), y = grid::unit(1, "npc"),
    hjust = 1, vjust = 1,
    gp = grid::gpar(fontface = "italic", fontsize = 7, fontfamily = cpb_font_family())
  )
  gtable::gtable_add_grob(g, grob, t = row, l = col, clip = "off", name = "sec-ylab")
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
#' gives a width of 2.98 in; `page = "full"` gives 5.96 in. An explicit
#' `width` is only an escape hatch and is validated against these two
#' values -- any other width errors, so a stray `width = 8` fails
#' loudly rather than silently producing an off-spec figure.
#'
#' Height defaults to 2.98 in (the `"report"` preset). Pass
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
#' @param page Either `"half"` (default, 2.98 in wide) or `"full"`
#'   (5.96 in wide). Ignored if `width` is supplied explicitly.
#' @param preset Either `"report"` (default, 2.98 in tall) or
#'   `"presentation"` (2.5 in tall). Ignored if `height` is supplied
#'   explicitly.
#' @param height Explicit height in inches. `NULL` (default) uses
#'   `preset` to determine the height.
#' @param width Explicit width in inches; must be `2.98` or `5.96`.
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
                      page = c("half", "full"),
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

  page_widths <- c(half = 2.98, full = 5.96)
  allowed_widths <- unname(page_widths)

  if (is.null(width)) {
    width <- unname(page_widths[[page]])
  } else if (!any(abs(width - allowed_widths) < 1e-6)) {
    stop(
      "save_cpb(): `width` must be one of the CPB page widths (2.98 or ",
      "5.96 inches); got ", width, ". Use `page = \"half\"` or ",
      "`page = \"full\"` instead, or pass an explicit width matching one ",
      "of these two values.",
      call. = FALSE
    )
  }

  # NULL means height was left to us; decides below whether cpb_map()'s
  # aspect ratio gets to auto-size the panel, or an explicit height
  # wins outright, like an explicit panel_size does
  height_auto <- is.null(height)
  if (is.null(height)) {
    height <- if (preset == "presentation") 2.5 else 2.98
  }

  cpb_check_title(plot$labels$title, width)

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

  if (is.null(panel_size) && is.null(sec_ylab$label) && !title_aligned) {
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
