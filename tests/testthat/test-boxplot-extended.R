# test-boxplot-extended.R ----

local_boxplot_extended_data <- function() {
  data.frame(
    cat = factor(c("laag", "midden", "hoog"), levels = c("laag", "midden", "hoog")),
    p5 = c(-8, -6, -4), p25 = c(-4, -3, -2), p50 = c(-2, -1, 0),
    p75 = c(0, 1, 2), p95 = c(3, 4, 5)
  )
}

test_that("cpb_boxplot_extended forwards every cpb_box() parameter it doesn't explicitly opt out of", {
  # cpb_boxplot_extended() hand-mirrors cpb_box()'s own signature
  # (a manual {{ }} forward, not automatic ... passthrough for every
  # named argument -- cpb_box() has no ... of its own to fall back
  # on), so nothing keeps the two in sync automatically: a new
  # cpb_box() parameter can silently become unreachable through the
  # extended wrapper unless someone remembers to add it there too.
  # Guards that by name -- every cpb_box() parameter must appear in
  # cpb_boxplot_extended()'s own formals, except the ones explicitly
  # excluded below (zeroline: cpb_boxplot_extended() always turns
  # cpb_box()'s own zeroline off, replacing it with zero_indicator --
  # see its own @param docs for why).
  excluded <- c("zeroline")
  box_args <- names(formals(cpb_box))
  ext_args <- names(formals(cpb_boxplot_extended))
  missing <- setdiff(box_args, c(ext_args, excluded))
  expect_equal(
    missing, character(0),
    info = paste0(
      "cpb_box() gained ", paste(missing, collapse = ", "),
      "; forward it through cpb_boxplot_extended() too, or add it to ",
      "`excluded` above with a comment explaining why not"
    )
  )
})

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

test_that("cpb_boxplot_extended's ylab_position overrides the facet-based default", {
  d <- local_boxplot_extended_data()
  # no facet, forced to "left" (the faceted default) instead of the
  # single-panel default "middle"
  p_left <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                                  p75 = p75, p95 = p95, title = "Titel",
                                  ylab_position = "left")
  expect_equal(p_left$theme$plot.title.position, "plot")
  expect_equal(p_left$theme$plot.title$hjust, 0)

  d2 <- rbind(d, d)
  d2$jaar <- rep(c(2025, 2026), each = 3)
  # faceted, forced to "middle" (the single-panel default) instead of
  # the faceted default "left"
  p_middle <- cpb_boxplot_extended(d2, x = cat, p5 = p5, p25 = p25, p50 = p50,
                                    p75 = p75, p95 = p95, title = "Titel",
                                    facet = jaar, ylab_position = "middle")
  expect_equal(p_middle$theme$plot.title.position, "panel")
  expect_equal(p_middle$theme$plot.title$hjust, 0.5)
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

test_that("cpb_boxplot_extended's value_axis_linewidth defaults to 0.7 and is overridable", {
  d <- local_boxplot_extended_data()
  p <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                             p75 = p75, p95 = p95)
  expect_equal(p$theme$axis.line.x.top$linewidth, 0.7)

  p2 <- cpb_boxplot_extended(d, x = cat, p5 = p5, p25 = p25, p50 = p50,
                              p75 = p75, p95 = p95, value_axis_linewidth = 1.2)
  expect_equal(p2$theme$axis.line.x.top$linewidth, 1.2)
})
