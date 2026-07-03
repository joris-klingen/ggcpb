# test-wrappers.R ----

test_that("cpb_col returns a ggplot with a GeomCol layer and a fill scale", {
  df <- data.frame(
    year  = rep(2021:2022, each = 2),
    group = rep(c("a", "b"), 2),
    value = c(1, 2, 3, 4)
  )
  p <- cpb_col(df, x = year, y = value, fill = group)

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 1)
  expect_true(inherits(p$layers[[1]]$geom, "GeomCol"))
  expect_s3_class(p$theme, "theme")
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
})

test_that("cpb_col adds a value-label layer when requested", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value, value_labels = TRUE)
  expect_length(p$layers, 2)
  expect_true(inherits(p$layers[[2]]$geom, "GeomText"))
})

test_that("cpb_col without a fill column adds no colour/fill scale", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value)
  expect_length(p$scales$scales, 0)
})

test_that("cpb_area returns a stacked-area ggplot with a fill scale", {
  df <- data.frame(
    year    = rep(2020:2021, each = 2),
    bron    = rep(c("gas", "elektriciteit"), 2),
    aandeel = c(60, 40, 55, 45)
  )
  p <- cpb_area(df, x = year, y = aandeel, fill = bron)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomArea"))
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
})

test_that("cpb_line adds a colour scale only when colour is mapped", {
  df1 <- data.frame(jaar = 2018:2020, waarde = c(1, 2, 3))
  p1 <- cpb_line(df1, x = jaar, y = waarde)
  expect_true(inherits(p1$layers[[1]]$geom, "GeomLine"))
  expect_length(p1$scales$scales, 0)

  df2 <- data.frame(
    jaar   = rep(2018:2019, 2),
    g      = rep(c("a", "b"), each = 2),
    waarde = 1:4
  )
  p2 <- cpb_line(df2, x = jaar, y = waarde, colour = g)
  has_colour_scale <- any(vapply(p2$scales$scales, function(s) "colour" %in% s$aesthetics, logical(1)))
  expect_true(has_colour_scale)
})

test_that("cpb_box builds an errorbar-plus-boxplot combination", {
  df <- data.frame(
    groep = c("a", "b"),
    p5  = c(-8, -6),
    p25 = c(-4, -3),
    p50 = c(-2, -1),
    p75 = c(0, 1),
    p95 = c(3, 4)
  )
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
  expect_length(p$layers, 2)
  expect_true(inherits(p$layers[[1]]$geom, "GeomErrorbar"))
  expect_true(inherits(p$layers[[2]]$geom, "GeomBoxplot"))
})

test_that("all wrappers can be built into a gtable without error", {
  df <- data.frame(
    groep = c("a", "b"),
    p5  = c(-8, -6), p25 = c(-4, -3), p50 = c(-2, -1), p75 = c(0, 1), p95 = c(3, 4)
  )
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("zeroline adds a black hline at 0 to the wrappers", {
  df <- data.frame(x = c("a", "b"), y = c(-1, 2))
  p <- cpb_col(df, x = x, y = y, zeroline = TRUE)
  is_hline <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_true(any(is_hline))
  hl <- p$layers[[which(is_hline)]]
  expect_equal(hl$aes_params$colour, "black")

  # cpb_col draws it on top of the bars; cpb_line/cpb_box underneath
  expect_gt(which(is_hline), which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomCol"), logical(1))))

  p2 <- cpb_line(df, x = x, y = y, zeroline = TRUE)
  is_hline2 <- vapply(p2$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_equal(unname(which(is_hline2)), 1L)
})

test_that("wrappers forward the nplot-style knobs to theme_cpb", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2))
  p <- cpb_col(df, x = x, y = y,
               minor = FALSE, ticks = TRUE, axis_text_size = 7,
               legend_key_size = 0.45, grid_colour = "black",
               grid_linewidth = 0.1)
  th <- p$theme
  expect_s3_class(th$panel.grid.minor.y, "element_blank")
  expect_s3_class(th$axis.ticks.x, "element_line")
  expect_equal(th$axis.text$size, 7)
  expect_equal(th$panel.grid.major.y$colour, "black")
})

test_that("cpb_box errorbars dodge by group without a fill warning", {
  df <- data.frame(
    groep = rep(c("a", "b"), each = 2),
    jaar  = rep(c("2026", "2027"), 2),
    p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5
  )
  expect_no_warning(
    ggplot2::ggplotGrob(
      cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
              fill = jaar, position = ggplot2::position_dodge(width = 0.75),
              reverse_legend = TRUE)
    )
  )
})
