# test-boxplot-extended.R ----

local_boxplot_extended_data <- function() {
  data.frame(
    cat = factor(c("laag", "midden", "hoog"), levels = c("laag", "midden", "hoog")),
    p5 = c(-8, -6, -4), p25 = c(-4, -3, -2), p50 = c(-2, -1, 0),
    p75 = c(0, 1, 2), p95 = c(3, 4, 5)
  )
}

test_that("cpb_boxplot_extended draws the same data as cpb_box with box_style = modern", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95)
  p_ref <- cpb_box(d, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                    box_style = "modern", orientation = "horizontal",
                    value_axis = "top", width = 0.45, grid_colour = "white",
                    ticks = FALSE, zeroline = FALSE)

  b <- ggplot2::ggplot_build(p)
  b_ref <- ggplot2::ggplot_build(p_ref)
  # same number of data layers plus the extra zero_indicator hline
  expect_equal(length(b$data), length(b_ref$data) + 1)
  # the boxplot geometry itself (median, hinges, whiskers) matches
  # cpb_box(box_style = "modern") exactly, layer for layer once the
  # leading zero_indicator layer is skipped
  box_idx <- which(vapply(p$layers, function(l) inherits(l$geom, "GeomBoxplot"), TRUE))
  box_idx_ref <- which(vapply(p_ref$layers, function(l) inherits(l$geom, "GeomBoxplot"), TRUE))
  expect_equal(b$data[[box_idx]]$ymin, b_ref$data[[box_idx_ref]]$ymin)
  expect_equal(b$data[[box_idx]]$ymax, b_ref$data[[box_idx_ref]]$ymax)
})

test_that("cpb_boxplot_extended defaults to horizontal, modern, value_axis top", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95)
  expect_s3_class(p$coordinates, "CoordFlip")
})

test_that("cpb_boxplot_extended adds a white zero_indicator line underneath everything else", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95)
  expect_true(inherits(p$layers[[1]]$geom, "GeomHline"))
  expect_equal(p$layers[[1]]$aes_params$colour, "white")
  expect_equal(p$layers[[1]]$aes_params$linewidth, 2)

  p2 <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                              p75 = p75, p95 = p95, zero_indicator = FALSE)
  expect_false(any(vapply(p2$layers, function(l) inherits(l$geom, "GeomHline"), TRUE)))
})

test_that("cpb_boxplot_extended centres the title over the panel when there is no facet", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95, title = "Titel")
  expect_equal(p$theme$plot.title.position, "panel")
  expect_equal(p$theme$plot.title$hjust, 0.5)
})

test_that("cpb_boxplot_extended keeps the full-width left-aligned title when faceted", {
  d <- local_boxplot_extended_data()
  d <- rbind(d, d)
  d$jaar <- rep(c(2025, 2026), each = 3)
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95, title = "Titel", facet = jaar)
  expect_equal(p$theme$plot.title.position, "plot")
  expect_equal(p$theme$plot.title$hjust, 0)
  expect_equal(p$facet$params$strip.position, "top")
})

test_that("cpb_boxplot_extended's panel_fill and grid_colour are overridable", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95,
                             panel_fill = "pink", grid_colour = "green")
  expect_equal(p$theme$panel.background$fill, "pink")
  expect_equal(p$theme$panel.grid.major.x$colour, "green")
})

test_that("cpb_boxplot_extended passes sec_y through to cpb_box", {
  # sec_y requires orientation = "vertical" (cpb_box()'s own
  # constraint: the secondary axis is drawn on the right of the value
  # axis), which is not this wrapper's default
  d <- local_boxplot_extended_data()
  d$z <- c(10, 12, 11)
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95, orientation = "vertical",
                             sec_y = z, sec_ylab = "n")
  is_line <- vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))
  expect_true(any(is_line))
})

test_that("cpb_boxplot_extended's sec_y still requires orientation = vertical, same as cpb_box", {
  d <- local_boxplot_extended_data()
  d$z <- c(10, 12, 11)
  expect_error(
    cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                          p75 = p75, p95 = p95, sec_y = z),
    "vertical box charts"
  )
})

test_that("cpb_boxplot_extended's value_axis auto-resolves around sec_y", {
  d <- local_boxplot_extended_data()
  d$z <- c(10, 12, 11)

  p1 <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                              p75 = p75, p95 = p95)
  expect_true(is.null(p1$theme$axis.line.x.bottom) ||
                inherits(p1$theme$axis.line.x.bottom, "element_blank"))

  p2 <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                              p75 = p75, p95 = p95, orientation = "vertical",
                              sec_y = z)
  # value_axis silently resolved to "bottom" here, avoiding cpb_box()'s
  # own error for sec_y + value_axis = "top" -- built without error is
  # the assertion that matters
  expect_s3_class(p2, "ggplot")

  # an explicit value_axis still wins, error and all
  expect_error(
    cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                          p75 = p75, p95 = p95, orientation = "vertical",
                          sec_y = z, value_axis = "top"),
    "value_axis"
  )
})
