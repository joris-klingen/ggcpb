# test-wrappers.R ----

test_that("cpb_col returns a ggplot with a GeomCol layer and a fill scale", {
  df <- data.frame(
    year  = rep(2021:2022, each = 2),
    group = rep(c("a", "b"), 2),
    value = c(1, 2, 3, 4)
  )
  # style = "ggplot" pins the classic look: the default style adds a
  # zero line layer and a value-axis scale
  p <- cpb_col(df, x = year, y = value, fill = group, style = "ggplot")

  expect_s3_class(p, "ggplot")
  expect_length(p$layers, 1)
  expect_true(inherits(p$layers[[1]]$geom, "GeomCol"))
  expect_s3_class(p$theme, "theme")
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
})

test_that("cpb_col adds a value-label layer when requested", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value, value_labels = TRUE, style = "ggplot")
  expect_length(p$layers, 2)
  expect_true(inherits(p$layers[[2]]$geom, "GeomText"))
})

test_that("cpb_col without a fill column adds no colour/fill scale", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value, style = "ggplot")
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
  p1 <- cpb_line(df1, x = jaar, y = waarde, style = "ggplot")
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
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               style = "ggplot")
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

test_that("wrappers forward the theme knobs to theme_cpb", {
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

test_that("the default style sets the zero line automatically", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2))
  has_hline <- function(p) any(vapply(p$layers, function(l)
    inherits(l$geom, "GeomHline"), logical(1)))

  # bars are anchored at zero: always drawn by default, never under 'ggplot'
  expect_true(has_hline(cpb_col(df, x = x, y = y, style = "cpb_default")))
  expect_false(has_hline(cpb_col(df, x = x, y = y, style = "ggplot")))

  # lines: only when the y data spans zero
  expect_false(has_hline(cpb_line(df, x = x, y = y, style = "cpb_default")))
  df2 <- data.frame(x = c("a", "b"), y = c(-1, 2))
  expect_true(has_hline(cpb_line(df2, x = x, y = y, style = "cpb_default")))
})

test_that("cpb_line draws a single unmapped series in CPB blue", {
  df <- data.frame(x = 1:3, y = 4:6)
  p <- cpb_line(df, x = x, y = y)
  expect_equal(p$layers[[1]]$aes_params$colour, unname(cpb_cols(6)))
  p2 <- cpb_line(df, x = x, y = y, line_colour = "red")
  expect_equal(p2$layers[[1]]$aes_params$colour, "red")
})

test_that("cpb_line and cpb_area render ylab as the subtitle", {
  df <- data.frame(x = 1:3, y = 4:6, g = "a")
  p <- cpb_line(df, x = x, y = y, ylab = "%")
  expect_equal(p$labels$subtitle, "%")
  p2 <- cpb_area(df, x = x, y = y, fill = g, ylab = "aandeel")
  expect_equal(p2$labels$subtitle, "aandeel")
  # an explicit subtitle keeps ylab on the axis
  p3 <- cpb_line(df, x = x, y = y, ylab = "%", subtitle = "sub")
  expect_equal(p3$labels$subtitle, "sub")
  expect_equal(p3$labels$y, "%")
})

test_that("cpb_box fills unmapped boxes in CPB blue with thin strokes", {
  df <- data.frame(groep = c("a", "b"),
                   p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
  box <- p$layers[[2]]
  expect_equal(box$aes_params$fill, unname(cpb_cols(6)))
  expect_equal(box$aes_params$linewidth, 0.25)
})

test_that("the default style trims value-axis expansion on the zero side", {
  df_pos <- data.frame(x = c("a", "b"), y = c(1, 2))
  sc <- cpb_col(df_pos, x = x, y = y, style = "cpb_default")$scales$get_scales("y")
  expect_equal(sc$expand, ggplot2::expansion(mult = c(0, 0.05)))

  df_neg <- data.frame(x = c("a", "b"), y = c(-1, -2))
  sc2 <- cpb_col(df_neg, x = x, y = y, style = "cpb_default")$scales$get_scales("y")
  expect_equal(sc2$expand, ggplot2::expansion(mult = c(0.05, 0)))

  # mixed-sign data and the ggplot style keep the default expansion
  df_mix <- data.frame(x = c("a", "b"), y = c(-1, 2))
  expect_null(cpb_col(df_mix, x = x, y = y, style = "cpb_default")$scales$get_scales("y"))
  expect_null(cpb_col(df_pos, x = x, y = y, style = "ggplot")$scales$get_scales("y"))
})

test_that("cpb_col value_breaks land on the wrapper-built value scale", {
  df <- data.frame(x = c("a", "b"), y = c(10, 60))
  sc <- cpb_col(df, x = x, y = y, pct_axis = TRUE,
                value_breaks = seq(0, 70, 10))$scales$get_scales("y")
  expect_equal(sc$breaks, seq(0, 70, 10))
  expect_true(is.function(sc$labels))
})

test_that("default-style line charts draw the panel without expansion", {
  df <- data.frame(x = 1:3, y = 4:6)
  p <- cpb_line(df, x = x, y = y, style = "cpb_default")
  expect_false(p$coordinates$expand)
  expect_true(cpb_line(df, x = x, y = y, style = "ggplot")$coordinates$expand)
})

test_that("titled wrappers reserve the subtitle line when none is given", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2))
  expect_equal(cpb_col(df, x = x, y = y, title = "t")$labels$subtitle, " ")
  expect_equal(cpb_line(df, x = x, y = y, title = "t")$labels$subtitle, " ")
  # no title -> no reserved line; explicit ylab still wins
  expect_null(cpb_col(df, x = x, y = y)$labels$subtitle)
  expect_equal(cpb_col(df, x = x, y = y, title = "t", ylab = "u")$labels$subtitle, "u")
})
