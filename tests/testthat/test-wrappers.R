# test-wrappers.R ----

test_that("cpb_col returns a ggplot with a GeomCol layer and a fill scale", {
  df <- data.frame(
    year  = rep(2021:2022, each = 2),
    group = rep(c("a", "b"), 2),
    value = c(1, 2, 3, 4)
  )
  p <- cpb_col(df, x = year, y = value, fill = group)

  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomCol"), logical(1))))
  expect_s3_class(p$theme, "theme")
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
})

test_that("cpb_col adds a value-label layer when requested", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value, value_labels = TRUE)
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("cpb_col without a fill column adds no colour/fill scale", {
  df <- data.frame(year = 2021:2022, value = c(1, 2))
  p <- cpb_col(df, x = year, y = value)
  has_colour_scale <- any(vapply(p$scales$scales, function(s)
    any(c("fill", "colour") %in% s$aesthetics), logical(1)))
  expect_false(has_colour_scale)
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
  # data spans zero, so a zero line is drawn underneath the boxes first
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
  expect_length(p$layers, 3)
  expect_true(inherits(p$layers[[1]]$geom, "GeomHline"))
  expect_true(inherits(p$layers[[2]]$geom, "GeomErrorbar"))
  expect_true(inherits(p$layers[[3]]$geom, "GeomBoxplot"))
})

test_that("all wrappers can be built into a gtable without error", {
  df <- data.frame(
    groep = c("a", "b"),
    p5  = c(-8, -6), p25 = c(-4, -3), p50 = c(-2, -1), p75 = c(0, 1), p95 = c(3, 4)
  )
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("the zero line is drawn automatically per house convention", {
  has_hline <- function(p) any(vapply(p$layers, function(l)
    inherits(l$geom, "GeomHline"), logical(1)))

  # bars/areas are anchored at zero: always drawn, unless disabled
  df <- data.frame(x = c("a", "b"), y = c(1, 2), g = c("a", "b"))
  expect_true(has_hline(cpb_col(df, x = x, y = y)))
  expect_false(has_hline(cpb_col(df, x = x, y = y, zeroline = FALSE)))
  expect_true(has_hline(cpb_area(df, x = x, y = y, fill = g)))

  # lines and boxes: only when the data spans (or touches) zero
  expect_false(has_hline(cpb_line(df, x = x, y = y)))
  df2 <- data.frame(x = c("a", "b"), y = c(-1, 2))
  expect_true(has_hline(cpb_line(df2, x = x, y = y)))
})

test_that("the zero line layers correctly: over bars, under lines", {
  df <- data.frame(x = c("a", "b"), y = c(-1, 2))
  p <- cpb_col(df, x = x, y = y)
  is_hline <- vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_equal(p$layers[[which(is_hline)]]$aes_params$colour, "black")
  expect_gt(which(is_hline), which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomCol"), logical(1))))

  p2 <- cpb_line(df, x = x, y = y)
  is_hline2 <- vapply(p2$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))
  expect_equal(unname(which(is_hline2)), 1L)
})

test_that("wrappers forward the theme knobs to theme_cpb", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2))
  p <- cpb_col(df, x = x, y = y,
               minor = TRUE, ticks = FALSE, axis_text_size = 6,
               legend_key_size = 0.45, grid_colour = "grey50")
  th <- p$theme
  expect_s3_class(th$panel.grid.minor.y, "element_line")
  expect_null(th$axis.ticks.x)
  expect_equal(th$axis.text$size, 6)
  expect_equal(th$panel.grid.major.y$colour, "grey50")
  expect_equal(as.numeric(th$legend.key.height), 0.45)
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

test_that("cpb_line draws a single unmapped series in CPB blue", {
  df <- data.frame(x = 1:3, y = 4:6)
  p <- cpb_line(df, x = x, y = y)
  expect_equal(p$layers[[1]]$aes_params$colour, unname(cpb_cols(6)))
  expect_equal(p$layers[[1]]$aes_params$linewidth, 0.55)
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

test_that("the value axis is zero-flush for single-signed data", {
  df_pos <- data.frame(x = c("a", "b"), y = c(1, 2))
  sc <- cpb_col(df_pos, x = x, y = y)$scales$get_scales("y")
  expect_equal(sc$expand, ggplot2::expansion(mult = c(0, 0.05)))

  df_neg <- data.frame(x = c("a", "b"), y = c(-1, -2))
  sc2 <- cpb_col(df_neg, x = x, y = y)$scales$get_scales("y")
  expect_equal(sc2$expand, ggplot2::expansion(mult = c(0.05, 0)))

  # mixed-sign data keeps the default expansion (no wrapper scale)
  df_mix <- data.frame(x = c("a", "b"), y = c(-1, 2))
  expect_null(cpb_col(df_mix, x = x, y = y)$scales$get_scales("y"))
})

test_that("cpb_col value_breaks land on the wrapper-built value scale", {
  df <- data.frame(x = c("a", "b"), y = c(10, 60))
  sc <- cpb_col(df, x = x, y = y, pct_axis = TRUE,
                value_breaks = seq(0, 70, 10))$scales$get_scales("y")
  expect_equal(sc$breaks, seq(0, 70, 10))
  expect_true(is.function(sc$labels))
})

test_that("line charts draw the panel without expansion", {
  df <- data.frame(x = 1:3, y = 4:6)
  p <- cpb_line(df, x = x, y = y)
  expect_false(p$coordinates$expand)
})

test_that("titled wrappers reserve the subtitle line when none is given", {
  df <- data.frame(x = c("a", "b"), y = c(1, 2))
  expect_equal(cpb_col(df, x = x, y = y, title = "t")$labels$subtitle, " ")
  expect_equal(cpb_line(df, x = x, y = y, title = "t")$labels$subtitle, " ")
  # no title -> no reserved line; explicit ylab still wins
  expect_null(cpb_col(df, x = x, y = y)$labels$subtitle)
  expect_equal(cpb_col(df, x = x, y = y, title = "t", ylab = "u")$labels$subtitle, "u")
})

test_that("cpb_scatter picks the right colour treatment", {
  df <- data.frame(x = 1:4, y = 5:8, num = c(1, 2, 3, 4),
                   grp = c("a", "b", "a", "b"))

  # no colour mapping: house blue points
  p <- cpb_scatter(df, x = x, y = y)
  expect_true(inherits(p$layers[[1]]$geom, "GeomPoint"))
  expect_equal(p$layers[[1]]$aes_params$colour, unname(cpb_cols(6)))

  # numeric colour column: continuous gradient scale
  p2 <- cpb_scatter(df, x = x, y = y, colour = num)
  sc2 <- p2$scales$get_scales("colour")
  expect_s3_class(sc2, "ScaleContinuous")

  # discrete colour column: discrete CPB scale
  p3 <- cpb_scatter(df, x = x, y = y, colour = grp, index = c(6, 2))
  sc3 <- p3$scales$get_scales("colour")
  expect_s3_class(sc3, "ScaleDiscrete")
})

test_that("cpb_scatter draws the zero line only when y spans zero", {
  has_hline <- function(p) any(vapply(p$layers, function(l)
    inherits(l$geom, "GeomHline"), logical(1)))
  df <- data.frame(x = 1:3, y = c(1, 2, 3))
  expect_false(has_hline(cpb_scatter(df, x = x, y = y)))
  df2 <- data.frame(x = 1:3, y = c(-1, 0, 2))
  expect_true(has_hline(cpb_scatter(df2, x = x, y = y)))
})

test_that("cpb_hist bins with house-blue bars, white outlines and flush counts", {
  df <- data.frame(waarde = rnorm(200))
  p <- cpb_hist(df, x = waarde, bins = 10)
  bar <- p$layers[[1]]
  expect_s3_class(bar$stat, "StatBin")
  expect_equal(bar$aes_params$fill, unname(cpb_cols(6)))
  expect_equal(bar$aes_params$colour, "white")
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))))
  sc <- p$scales$get_scales("y")
  expect_equal(sc$expand, ggplot2::expansion(mult = c(0, 0.05)))
  expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("cpb_hist maps fill for grouped histograms", {
  df <- data.frame(waarde = rnorm(200), grp = rep(c("a", "b"), 100))
  p <- cpb_hist(df, x = waarde, fill = grp, bins = 10, index = c(6, 2))
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
})

test_that("forecast_x adds the raming window under the data with a label on top", {
  df <- data.frame(jaar = 2020:2027, waarde = 1:8)
  p <- cpb_line(df, x = jaar, y = waarde, forecast_x = 2024.5)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% geoms)
  expect_true("GeomText" %in% geoms)
  # rect underneath the line, label on top
  expect_lt(which(geoms == "GeomRect"), which(geoms == "GeomLine"))
  expect_gt(which(geoms == "GeomText"), which(geoms == "GeomLine"))
  # label centred in the window
  lab <- p$layers[[which(geoms == "GeomText")]]
  expect_equal(lab$data$x[1], (2024.5 + 2027) / 2)
  expect_equal(lab$aes_params$label, "raming")

  # forecast_label = NULL suppresses the label
  p2 <- cpb_line(df, x = jaar, y = waarde, forecast_x = 2024.5, forecast_label = NULL)
  geoms2 <- vapply(p2$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomText" %in% geoms2)

  # also available on columns and areas
  df$grp <- "a"
  for (p3 in list(cpb_col(df, x = jaar, y = waarde, forecast_x = 2024.5),
                  cpb_area(df, x = jaar, y = waarde, fill = grp, forecast_x = 2024.5))) {
    geoms3 <- vapply(p3$layers, function(l) class(l$geom)[1], character(1))
    expect_true(all(c("GeomRect", "GeomText") %in% geoms3))
  }
})

test_that("cpb_line draws an uncertainty band under the lines", {
  df <- data.frame(jaar = 2020:2025, waarde = 1:6)
  df$lo <- df$waarde - 1
  df$hi <- df$waarde + 1
  p <- cpb_line(df, x = jaar, y = waarde, ymin = lo, ymax = hi)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geoms)
  expect_lt(which(geoms == "GeomRibbon"), which(geoms == "GeomLine"))

  # grouped bands take the series colours but stay out of the legend
  df2 <- rbind(df, transform(df, waarde = waarde + 3, lo = lo + 3, hi = hi + 3))
  df2$reeks <- rep(c("a", "b"), each = 6)
  p2 <- cpb_line(df2, x = jaar, y = waarde, colour = reeks,
                 ymin = lo, ymax = hi, index = c(6, 2))
  expect_no_error(ggplot2::ggplotGrob(p2))
  expect_identical(p2$guides$guides$fill, "none")
})

test_that("cpb_box box_style = 'james' and 'modern' build the legacy box", {
  df <- data.frame(groep = c("a", "b"),
                   p5 = 0.2, p25 = 0.4, p50 = 0.6, p75 = 0.7, p95 = 0.9)
  for (style in c("james", "modern")) {
    p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                 box_style = style, orientation = "horizontal")
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    # two capless whiskers, borderless box, median tick
    expect_equal(sum(geoms == "GeomErrorbar"), 3)
    expect_true("GeomBoxplot" %in% geoms)
    box <- p$layers[[which(geoms == "GeomBoxplot")]]
    expect_true(is.na(box$aes_params$colour))
    # value labels on by default
    expect_true("GeomText" %in% geoms)
    expect_no_error(ggplot2::ggplotGrob(p))
  }

  # style-specific colours: james blue box/black median, modern light
  # blue box/dark blue median
  pj <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                box_style = "james")
  gj <- vapply(pj$layers, function(l) class(l$geom)[1], character(1))
  expect_equal(pj$layers[[which(gj == "GeomBoxplot")]]$aes_params$fill,
               unname(cpb_cols(6)))
  med_j <- pj$layers[[max(which(gj == "GeomErrorbar"))]]
  expect_equal(med_j$aes_params$colour, "black")

  pm <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                box_style = "modern")
  gm <- vapply(pm$layers, function(l) class(l$geom)[1], character(1))
  expect_equal(pm$layers[[which(gm == "GeomBoxplot")]]$aes_params$fill,
               unname(cpb_cols(5)))
  med_m <- pm$layers[[max(which(gm == "GeomErrorbar"))]]
  expect_equal(med_m$aes_params$colour, unname(cpb_cols(6)))

  # modern prints three label layers (median + both quartiles),
  # james one; box_labels = FALSE drops them
  expect_equal(sum(gm == "GeomText"), 3)
  expect_equal(sum(gj == "GeomText"), 1)
  p0 <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                box_style = "modern", box_labels = FALSE)
  expect_false("GeomText" %in% vapply(p0$layers, function(l) class(l$geom)[1], character(1)))
})

test_that("james/modern box styles reject a fill mapping", {
  df <- data.frame(groep = c("a", "b"), g = c("x", "y"),
                   p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)
  expect_error(
    cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
            fill = g, box_style = "modern"),
    "single-colour"
  )
})
