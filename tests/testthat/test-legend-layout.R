# test-legend-layout.R ----
#
# The flush-left bottom legend must sit in one fixed spot, independent
# of chart type, category-label width, legend-label length and the
# number of legend items. The original hand-rolled CPB scripts used
# panel-relative coordinates (legend.position = c(x, y)) and even
# computed a label-length correction ("legend_x = 0.2 - max_label_len *
# 0.027", see references/code/reference_plot_snippets.R) because the
# panel edge moves with the category labels. flush_legend anchors the
# legend to the *plot* edge instead, which nothing inside the panel can
# move. These tests render real PNGs and check the pixels.

# Every plot below maps a series to CPB primary blue (cpb_cols(6),
# #005faf) that ends up as the *bottom* legend entry. In-panel marks
# are always indented past the category/axis labels, while the legend
# key sits at the plot margin, so the leftmost blue pixel in the
# rendered image IS the legend key's left edge, and the lowest blue
# pixel marks the bottom key of the bottom-justified legend block.
legend_key_px <- function(p, width = 4, height = 4, dpi = 96) {
  f <- withr::local_tempfile(fileext = ".png", .local_envir = parent.frame())
  ragg::agg_png(f, width = width, height = height, units = "in", res = dpi)
  print(p)
  grDevices::dev.off()
  img <- png::readPNG(f)
  blue <- img[, , 1] < 0.25 & img[, , 3] > 0.5 & img[, , 3] > img[, , 1] + 0.3
  expect_true(any(blue))  # sanity: the blue key must exist
  bottom <- max(which(apply(blue, 1, any)))
  list(
    left       = min(which(apply(blue, 2, any))),
    bottom     = bottom,
    # distance from the image's bottom edge, comparable across canvas sizes
    bottom_off = nrow(img) - bottom
  )
}

# left plot margin (10 pt) and legend key height/width (0.25/0.30 cm) in
# pixels at the 96 dpi the tests render at
margin_px <- 10 / 72 * 96
key_h_px  <- 0.25 / 2.54 * 96
key_w_px  <- 0.30 / 2.54 * 96

test_that("flush legend lands on the same pixel across chart types and label lengths", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("withr")

  short2 <- data.frame(
    x = rep(c("a", "b"), each = 2),
    g = rep(c("s1", "s2"), 2),
    y = c(1, 2, 3, 4)
  )
  # long category labels move the panel edge; long legend labels widen
  # the legend entries; neither may move the key
  long_cats <- data.frame(
    x = rep(c("een hele lange categorienaam (1,4 mln hh)",
              "nog een veel langere categorienaam"), each = 2),
    g = rep(c("een uitzonderlijk lang legenda-label", "kort"), 2),
    y = c(10, 20, 30, 40)
  )
  five <- data.frame(
    x = rep(c("a", "b"), each = 5),
    g = rep(paste("serie", 1:5), 2),
    y = rep(1:5, 2)
  )
  box_df <- data.frame(x = c("a", "b"), g = c("s1", "s2"),
                       p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)

  # reverse_legend = TRUE puts the blue first level at the *bottom* of
  # the legend in every variant, so the lowest blue pixel is comparable
  rect_variants <- list(
    col_vertical   = cpb_col(short2, x = x, y = y, fill = g, position = "dodge",
                             index = c(6, 2), title = "t"),
    col_horizontal = cpb_col(long_cats, x = x, y = y, fill = g, position = "dodge",
                             orientation = "horizontal",
                             index = c(6, 2), title = "t"),
    col_five       = cpb_col(five, x = x, y = y, fill = g, position = "dodge",
                             index = c(6, 2, 3, 4, 5), title = "t"),
    box            = cpb_box(box_df,
                             x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                             fill = g, reverse_legend = TRUE,
                             index = c(6, 2), title = "t")
  )
  # line keys draw a stroke centred in the key, so their lowest blue
  # pixel sits ~half a key above the rect variants'; they are compared
  # within their own family
  line_variants <- list(
    line_short = cpb_line(short2, x = as.integer(factor(x)), y = y, colour = g,
                          index = c(6, 2), reverse_legend = TRUE, title = "t"),
    line_long  = cpb_line(long_cats, x = as.integer(factor(x)), y = y, colour = g,
                          index = c(6, 2), reverse_legend = TRUE, title = "t")
  )

  rect_pos <- lapply(rect_variants, legend_key_px)
  line_pos <- lapply(line_variants, legend_key_px)

  lefts <- vapply(c(rect_pos, line_pos), `[[`, numeric(1), "left")
  rect_bottoms <- vapply(rect_pos, `[[`, numeric(1), "bottom")
  line_bottoms <- vapply(line_pos, `[[`, numeric(1), "bottom")

  # one solid left-justified spot: every variant's key starts on the
  # same pixel column (antialiasing allows 1 px), at the plot margin
  expect_lte(diff(range(lefts)), 1)
  expect_lte(abs(mean(lefts) - margin_px), 2)

  # one solid bottom-justified spot: the bottom key row is identical
  # whether the legend holds 2 or 5 items, per glyph family ...
  expect_lte(diff(range(rect_bottoms)), 2)
  expect_lte(diff(range(line_bottoms)), 2)
  # ... and the families differ by at most half a key (the stroke
  # centring), i.e. the legend *block* itself is anchored
  expect_lte(abs(mean(rect_bottoms) - mean(line_bottoms)), key_h_px / 2 + 2)
})

test_that("legend key pixel is invariant to legend-label length and item count", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("withr")

  # label-length sweep: 1-character labels up to labels longer than the
  # panel is wide, plus a multi-line label (the blue *bottom* entry
  # keeps a single-line label so its key row stays the reference point)
  lab_sets <- list(
    tiny      = c("a", "b"),
    medium    = c("koopkracht", "inflatie"),
    long      = c("mediane koopkrachtontwikkeling van huishoudens",
                  "gemiddelde contractloonstijging in de marktsector"),
    multiline = c("s1", "een legenda-label\nover twee regels")
  )
  lab_variants <- lapply(lab_sets, function(labs) {
    df <- data.frame(
      x = rep(c("a", "b"), each = 2),
      g = factor(rep(labs, 2), levels = labs),
      y = 1:4
    )
    cpb_col(df, x = x, y = y, fill = g, position = "dodge",
            index = c(6, 2), title = "t")
  })

  # item-count sweep: 1 up to 8 legend entries; blue stays the first
  # level, i.e. the bottom entry of the reversed legend
  counts <- c(1, 2, 3, 5, 8)
  count_variants <- lapply(counts, function(n) {
    labs <- paste("serie", seq_len(n))
    df <- data.frame(
      x = rep(c("a", "b"), each = n),
      g = factor(rep(labs, 2), levels = labs),
      y = rep(seq_len(n), 2)
    )
    cpb_col(df, x = x, y = y, fill = g, position = "dodge",
            index = c(6, 2, 3, 4, 5, 1, 7, 8)[seq_len(n)], title = "t")
  })
  names(count_variants) <- paste0("n", counts)

  pos <- lapply(c(lab_variants, count_variants), legend_key_px)
  lefts   <- vapply(pos, `[[`, numeric(1), "left")
  bottoms <- vapply(pos, `[[`, numeric(1), "bottom_off")

  expect_lte(diff(range(lefts)), 1)
  expect_lte(abs(mean(lefts) - margin_px), 2)
  # the bottom key row must not move when labels grow or entries are
  # added: added entries stack *upwards* from the anchored bottom row
  expect_lte(diff(range(bottoms)), 2)
})

test_that("legend key pixel is invariant to chart type", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("withr")

  cat2 <- data.frame(
    x = rep(c("a", "b"), each = 2),
    g = factor(rep(c("s1", "s2"), 2), levels = c("s1", "s2")),
    y = 1:4
  )
  num2 <- data.frame(
    x = rep(2015:2020, each = 2),
    g = factor(rep(c("s1", "s2"), 6), levels = c("s1", "s2")),
    y = c(1, 2, 2, 3, 3, 3, 4, 5, 4, 6, 5, 7)
  )
  hist_df <- data.frame(
    v = rep(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 6), 2),
    g = factor(rep(c("s1", "s2"), each = 10), levels = c("s1", "s2"))
  )
  box_df <- data.frame(x = c("a", "b"), g = c("s1", "s2"),
                       p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)

  # rect keys: their leftmost/lowest blue pixel is the key's own corner
  rect_variants <- list(
    col_dodge      = cpb_col(cat2, x = x, y = y, fill = g, position = "dodge",
                             index = c(6, 2), title = "t"),
    col_stack      = cpb_col(cat2, x = x, y = y, fill = g,
                             index = c(6, 2), title = "t"),
    col_horizontal = cpb_col(cat2, x = x, y = y, fill = g, position = "dodge",
                             orientation = "horizontal",
                             index = c(6, 2), title = "t"),
    area           = cpb_area(num2, x = x, y = y, fill = g,
                              index = c(6, 2), title = "t"),
    hist           = cpb_hist(hist_df, x = v, fill = g, binwidth = 1,
                              index = c(6, 2), title = "t"),
    box            = cpb_box(box_df,
                             x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                             fill = g, reverse_legend = TRUE,
                             index = c(6, 2), title = "t"),
    col_facet      = cpb_col(transform(cat2, f = rep(c("paneel 1", "paneel 2"), 2)),
                             x = x, y = y, fill = g, position = "dodge",
                             facet = f, index = c(6, 2), title = "t")
  )
  # line keys: stroke centred in the key box, so bottoms sit ~half a
  # key higher than rect bottoms
  line_variants <- list(
    line = cpb_line(num2, x = x, y = y, colour = g,
                    index = c(6, 2), reverse_legend = TRUE, title = "t")
  )
  # point keys: the point is centred in the key box in *both*
  # directions, so neither its left nor its bottom pixel is the box edge
  point_variants <- list(
    scatter = cpb_scatter(num2, x = x, y = y, colour = g,
                          index = c(6, 2), reverse_legend = TRUE, title = "t")
  )

  rect_pos  <- lapply(rect_variants, legend_key_px)
  line_pos  <- lapply(line_variants, legend_key_px)
  point_pos <- lapply(point_variants, legend_key_px)

  edge_lefts <- vapply(c(rect_pos, line_pos), `[[`, numeric(1), "left")
  expect_lte(diff(range(edge_lefts)), 1)
  expect_lte(abs(mean(edge_lefts) - margin_px), 2)

  rect_bottoms <- vapply(rect_pos, `[[`, numeric(1), "bottom_off")
  expect_lte(diff(range(rect_bottoms)), 2)
  # line key: same anchored key box, stroke centring explains the shift
  expect_lte(abs(line_pos$line$bottom_off - mean(rect_bottoms)), key_h_px / 2 + 2)

  # scatter key: the point must fall inside the same anchored key box
  expect_gte(point_pos$scatter$left, mean(edge_lefts) - 1)
  expect_lte(point_pos$scatter$left, mean(edge_lefts) + key_w_px + 1)
  expect_gte(point_pos$scatter$bottom_off, mean(rect_bottoms) - 1)
  expect_lte(point_pos$scatter$bottom_off, mean(rect_bottoms) + key_h_px + 1)
})

test_that("legend key pixel is invariant to canvas size and titles/axis titles", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("withr")

  df <- data.frame(
    x = rep(c("a", "b"), each = 2),
    g = factor(rep(c("s1", "s2"), 2), levels = c("s1", "s2")),
    y = 1:4
  )
  mk <- function(...) {
    cpb_col(df, x = x, y = y, fill = g, position = "dodge",
            index = c(6, 2), ...)
  }

  # everything above/around the panel may change, the key may not
  ann_variants <- list(
    plain          = mk(),
    title          = mk(title = "titel"),
    title_ylab = mk(title = "titel", ylab = "mln euro"),
    title_xlab = mk(title = "titel", xlab = "inkomensgroep"),
    everything = mk(title = "een titel die over de volle breedte doorloopt",
                    ylab = "% van het beschikbaar inkomen",
                    xlab = "huishoudtype")
  )
  ann_pos <- lapply(ann_variants, legend_key_px)

  ann_lefts   <- vapply(ann_pos, `[[`, numeric(1), "left")
  ann_bottoms <- vapply(ann_pos, `[[`, numeric(1), "bottom_off")
  expect_lte(diff(range(ann_lefts)), 1)
  expect_lte(abs(mean(ann_lefts) - margin_px), 2)
  expect_lte(diff(range(ann_bottoms)), 2)

  # canvas sizes: the CPB half/full page widths and an off-grid size;
  # margins are absolute, so left and bottom offsets are size-invariant
  sizes <- list(half = c(2.98, 2.98), full = c(5.96, 2.98),
                tall = c(2.98, 4.5), square = c(4, 4))
  size_pos <- lapply(sizes, function(wh) {
    legend_key_px(mk(title = "titel"), width = wh[1], height = wh[2])
  })
  size_lefts   <- vapply(size_pos, `[[`, numeric(1), "left")
  size_bottoms <- vapply(size_pos, `[[`, numeric(1), "bottom_off")
  expect_lte(diff(range(size_lefts)), 1)
  expect_lte(abs(mean(size_lefts) - margin_px), 2)
  expect_lte(diff(range(size_bottoms)), 2)
})
