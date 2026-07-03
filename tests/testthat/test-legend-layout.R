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
  list(
    left   = min(which(apply(blue, 2, any))),
    bottom = max(which(apply(blue, 1, any)))
  )
}

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
                             style = "nplot", index = c(6, 2), title = "t"),
    col_horizontal = cpb_col(long_cats, x = x, y = y, fill = g, position = "dodge",
                             orientation = "horizontal",
                             style = "nplot", index = c(6, 2), title = "t"),
    col_five       = cpb_col(five, x = x, y = y, fill = g, position = "dodge",
                             style = "nplot", index = c(6, 2, 3, 4, 5), title = "t"),
    box            = cpb_box(box_df,
                             x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                             fill = g, reverse_legend = TRUE,
                             style = "nplot", index = c(6, 2), title = "t")
  )
  # line keys draw a stroke centred in the key, so their lowest blue
  # pixel sits ~half a key above the rect variants'; they are compared
  # within their own family
  line_variants <- list(
    line_short = cpb_line(short2, x = as.integer(factor(x)), y = y, colour = g,
                          style = "nplot", index = c(6, 2), title = "t") +
      ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE)),
    line_long  = cpb_line(long_cats, x = as.integer(factor(x)), y = y, colour = g,
                          style = "nplot", index = c(6, 2), title = "t") +
      ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))
  )

  rect_pos <- lapply(rect_variants, legend_key_px)
  line_pos <- lapply(line_variants, legend_key_px)

  lefts <- vapply(c(rect_pos, line_pos), `[[`, numeric(1), "left")
  rect_bottoms <- vapply(rect_pos, `[[`, numeric(1), "bottom")
  line_bottoms <- vapply(line_pos, `[[`, numeric(1), "bottom")

  # one solid left-justified spot: every variant's key starts on the
  # same pixel column (antialiasing allows 1 px), at the plot margin
  expect_lte(diff(range(lefts)), 1)
  margin_px <- 10 / 72 * 96  # left plot margin: 10 pt at 96 dpi
  expect_lte(abs(mean(lefts) - margin_px), 2)

  # one solid bottom-justified spot: the bottom key row is identical
  # whether the legend holds 2 or 5 items, per glyph family ...
  expect_lte(diff(range(rect_bottoms)), 2)
  expect_lte(diff(range(line_bottoms)), 2)
  # ... and the families differ by at most half a key (the stroke
  # centring), i.e. the legend *block* itself is anchored
  key_px <- 0.25 / 2.54 * 96
  expect_lte(abs(mean(rect_bottoms) - mean(line_bottoms)), key_px / 2 + 2)
})
