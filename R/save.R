# save.R ----
#
# Figure export helper. Width is strict and tied to the CPB page format
# (half or full page); height defaults to the CPB report height but has
# a "presentation" preset and can always be overridden explicitly.

# ggplot2's own layout gives the panel whatever room is left over once
# the title and legend have taken what they need -- normally the right
# behaviour, but it means the same plot's data area visibly grows or
# shrinks depending on how long the title or legend happens to be. A
# few wrappers (cpb_donut() so far) instead want the data area itself
# pinned to a constant physical size no matter what surrounds it, with
# any chrome that does not fit simply overflowing/being clipped rather
# than eating into it.
#
# The panel is the gtable's only "null" (elastic) cell; every other
# cell -- title, legend, the fixed plot.margin -- is already sized in
# absolute units by theme_cpb(). But a "null" unit only means anything
# relative to a specific grid.layout(), and only actually resolves to
# a real number once something establishes that layout at a specific
# size and asks -- a bare convertWidth() on an unplaced "1null" silently
# reads as zero. So this pushes exactly the viewport theme_cpb()'s own
# layout would use, sized to what save_cpb() would otherwise have
# rendered at, and reads each column/row back through it one at a time
# (querying a whole gtable's worth of null units in one convertWidth()
# call resolves them all against each other rather than the viewport,
# which is not what's wanted here) -- freezing title/legend exactly
# where they would normally land, before the panel cell gets
# overridden.
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

  # title, subtitle and a top/bottom legend all span the plot's full
  # width (theme_cpb() sets plot.title.position = "plot"), so shrinking
  # the panel's own column/row directly would narrow their span along
  # with it, throwing off exactly the alignment cpb_col() and friends
  # share. Instead, half the size difference goes to each of the
  # (normally near-empty) axis gutter cells flanking the panel -- still
  # present, if blank, since a donut always turns its axes off -- which
  # keeps every other cell's span the same width/height it would have
  # had anyway and centres the fixed-size panel within it. If the
  # panel needed to grow rather than shrink, and there is no gutter
  # left to take space back from, cpb_ggsave_grob() reports the extra
  # room the figure ends up needing.
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

# The wrappers' sec_ylab (see cpb_add_sec_ylab() in wrappers.R) is drawn
# twice over: once approximately, as an ordinary annotate() layer, so a
# bare print()/knitr display still shows *something*; save_cpb() lifts
# that layer back out here and draws an exact replacement instead (see
# cpb_add_sec_ylab_grob()), positioned against the plot's own rendered
# gtable rather than guessed at build time.
# @return A list with the (possibly unchanged) `plot` and the `label`
#   to re-draw exactly, or `label = NULL` when there was nothing to do.
# @noRd
cpb_take_sec_ylab <- function(plot) {
  info <- attr(plot, "cpb_sec_ylab")
  if (is.null(info)) {
    return(list(plot = plot, label = NULL))
  }
  plot$layers[[info$layer]] <- NULL
  list(plot = plot, label = info$label)
}

# Places `label` in gtable `g` on the same row as the plot's subtitle
# (so it lands at the same height as the left-hand unit ylab() puts
# there) and horizontally centred on the secondary axis's own tick
# *values* (so, right-aligned from that centre, the label's last
# character sits above their middle and the rest read leftward from
# it) -- both cells always exist once a wrapper's sec_y has produced a
# right axis, whether or not they hold anything yet (ggplot2 always
# reserves a "subtitle" row; it is just a zeroGrob when empty).
#
# The "axis-r" cell is not *only* the tick text: ggplot2 nests a nested
# gtable in there with the tick marks' own reserved length to its left
# (a "null" column, zero-width in this theme since ticks are blank,
# but still part of the cell) and the text flush to the right -- so the
# whole cell's own npc centre sits left of the text's true centre,
# which is why `page_width`/`page_height` are needed here: they let
# the elastic "null" widths resolve to what they really come out to at
# that size (see cpb_resolve_gtable_units()), the same way
# cpb_fix_panel_size() already has to.
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
  cell_width <- grid::convertWidth(g$widths[col], "in", valueOnly = TRUE)

  # the tick text is the only non-zero-width grob in the nested
  # per-axis gtable, flush to the right of the outer "axis-r" cell (any
  # tick-mark length sits to its left) -- found by grob class, not a
  # hardcoded column index, since that shifts with tick/theme settings
  inner <- g$grobs[[axis_idx]]$children$axis
  text_idx <- which(vapply(inner$grobs, inherits, logical(1), what = "titleGrob"))
  anchor_npc <- if (length(text_idx) == 1) {
    # inner$grobs is indexed by placed grob (one per occupied column,
    # skipping empty ones); inner$widths is indexed by column -- map
    # through the layout to convert from one indexing to the other
    text_col <- inner$layout$l[text_idx]
    text_width <- grid::convertWidth(inner$widths[[text_col]], "in", valueOnly = TRUE)
    1 - (text_width / cell_width) / 2
  } else {
    # no tick text found (e.g. axis.text suppressed) -- the whole
    # cell's own centre is the least-wrong fallback
    0.5
  }

  grob <- grid::textGrob(
    label, x = grid::unit(anchor_npc, "npc"), y = grid::unit(0.5, "npc"),
    hjust = 1, vjust = 0.5,
    gp = grid::gpar(fontface = "italic", fontsize = 7, fontfamily = cpb_font_family())
  )
  gtable::gtable_add_grob(g, grob, t = row, l = col, clip = "off", name = "sec-ylab")
}

# Draws a gtable straight to a graphics device, since ggplot2::ggsave()
# only accepts a ggplot object for `plot`, not an already-built grob.
#
# `width`/`height` are only an escape hatch for a grob whose panel cell
# is still a "null" unit (sec_ylab-only, no panel_size -- see
# save_cpb()): a null unit only resolves once drawn against a viewport
# of a real size, so it must be told what that size is. Left NULL (the
# cpb_fix_panel_size() case), every cell in `grob` is already an
# absolute unit, so the device is instead opened at the gtable's own
# natural total size: fixing the panel can only ever make the whole
# figure need a little more or less room than the dynamic layout
# would have, and forcing it back into the original size is exactly
# the "shrink the panel to fit" behaviour this exists to avoid.
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

  # NULL means the caller left height to us; used below to decide
  # whether cpb_map()'s own aspect ratio gets to auto-size the panel,
  # or an explicit height (like an explicit panel_size) wins outright
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
  # (metres) aspect ratio: measure the panel width the plot would get
  # at this page width regardless (title/legend chrome span the full
  # width, so it does not depend on height) and pin the panel to that
  # width at the matching height, so the map fills its panel exactly
  # instead of sitting letterboxed inside a too-tall/too-short one. An
  # explicit height opts out, the same way an explicit panel_size does.
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

  if (is.null(panel_size) && is.null(sec_ylab$label)) {
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
    grob <- if (is.null(panel_size)) {
      ggplot2::ggplotGrob(plot)
    } else {
      cpb_fix_panel_size(plot, panel_size, width, height)
    }
    if (!is.null(sec_ylab$label)) {
      grob <- cpb_add_sec_ylab_grob(grob, sec_ylab$label, width, height)
    }
    if (!is.null(panel_size)) {
      # fixing the panel can leave the figure needing a little more or
      # less room than the page's usual width/height -- see
      # cpb_ggsave_grob() -- so what actually gets written here is
      # reported below rather than the originally requested width/height
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
