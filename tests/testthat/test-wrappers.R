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

# API consistency across wrappers ----

test_that("cpb_col supports an explicit subtitle with ylab falling back to the axis", {
  df <- data.frame(x = c("a", "b"), y = 1:2)
  p <- cpb_col(df, x = x, y = y, title = "t", subtitle = "sub", ylab = "unit")
  expect_equal(p$labels$subtitle, "sub")
  expect_equal(p$labels$y, "unit")
  # horizontal charts keep xlab on the value axis; ylab stays the subtitle
  p2 <- cpb_col(df, x = x, y = y, orientation = "horizontal",
                title = "t", ylab = "unit", xlab = "mld euro")
  expect_equal(p2$labels$subtitle, "unit")
  expect_equal(p2$labels$y, "mld euro")
})

test_that("value_breaks and value_limits work in area, line and box", {
  num <- data.frame(x = rep(2015:2017, 2), g = rep(c("s1", "s2"), each = 3),
                    y = c(1:3, 2:4))
  box_df <- data.frame(x = c("a", "b"), p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)

  sc <- cpb_area(num, x = x, y = y, fill = g,
                 value_breaks = c(0, 2, 4))$scales$get_scales("y")
  expect_equal(sc$breaks, c(0, 2, 4))
  sc <- cpb_line(num, x = x, y = y, colour = g,
                 value_breaks = c(1, 3))$scales$get_scales("y")
  expect_equal(sc$breaks, c(1, 3))
  sc <- cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                value_breaks = c(1, 3, 5))$scales$get_scales("y")
  expect_equal(sc$breaks, c(1, 3, 5))

  # limits go through the coordinate system (zoom), never dropping data
  p <- cpb_area(num, x = x, y = y, fill = g, value_limits = c(0, 10))
  expect_equal(p$coordinates$limits$y, c(0, 10))
  p <- cpb_line(num, x = x, y = y, colour = g, value_limits = c(0, 10))
  expect_equal(p$coordinates$limits$y, c(0, 10))
  expect_false(p$coordinates$expand)
  p <- cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               value_limits = c(0, 10))
  expect_equal(p$coordinates$limits$y, c(0, 10))
  # horizontal box: the limits ride along on coord_flip()
  p <- cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               orientation = "horizontal", value_limits = c(0, 10))
  expect_s3_class(p$coordinates, "CoordFlip")
  expect_equal(p$coordinates$limits$y, c(0, 10))
})

test_that("pct_axis works in cpb_box", {
  box_df <- data.frame(x = c("a", "b"), p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)
  sc <- cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                pct_axis = TRUE)$scales$get_scales("y")
  expect_equal(sc$labels(c(2.5, 50)), c("2%", "50%"))
})

test_that("reverse_legend reverses the colour guide in line and scatter", {
  num <- data.frame(x = rep(2015:2017, 2), g = rep(c("s1", "s2"), each = 3),
                    y = c(1:3, 2:4))
  p <- cpb_line(num, x = x, y = y, colour = g, reverse_legend = TRUE)
  expect_true(p$guides$guides$colour$params$reverse)
  p <- cpb_scatter(num, x = x, y = y, colour = g, reverse_legend = TRUE)
  expect_true(p$guides$guides$colour$params$reverse)
  # default stays FALSE: no stacking convention for lines/points
  expect_null(cpb_line(num, x = x, y = y, colour = g)$guides$guides$colour)
  # a numeric colour column keeps its continuous scale untouched
  numc <- transform(num, g = as.numeric(factor(g)))
  p <- cpb_scatter(numc, x = x, y = y, colour = g, reverse_legend = TRUE)
  expect_null(p$guides$guides$colour)
})

test_that("cpb_scatter draws the forecast window like cpb_line", {
  num <- data.frame(x = rep(2015:2019, 2), g = rep(c("s1", "s2"), each = 5),
                    y = c(1:5, 2:6))
  p <- cpb_scatter(num, x = x, y = y, colour = g, forecast_x = 2017.5)
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  rect_i  <- which(classes == "GeomRect")
  point_i <- which(classes == "GeomPoint")
  text_i  <- which(classes == "GeomText")
  expect_length(rect_i, 1)
  expect_length(text_i, 1)
  # window underneath the points, label on top
  expect_lt(rect_i, min(point_i))
  expect_gt(text_i, max(point_i))
  expect_equal(p$layers[[text_i]]$aes_params$label, "raming")
})

test_that("every wrapper forwards the shared theme knobs (anti-drift)", {
  df <- data.frame(x = c("a", "b"), y = 1:2)
  num <- data.frame(x = 2015:2016, y = 1:2)
  box_df <- data.frame(x = c("a", "b"), p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)
  plots <- list(
    cpb_col(df, x = x, y = y, axis_text_size = 9, legend = "none"),
    cpb_area(df, x = x, y = y, fill = x, axis_text_size = 9, legend = "none"),
    cpb_line(num, x = x, y = y, axis_text_size = 9, legend = "none"),
    cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
            axis_text_size = 9, legend = "none"),
    cpb_scatter(num, x = x, y = y, axis_text_size = 9, legend = "none"),
    cpb_hist(df, x = y, bins = 2, axis_text_size = 9, legend = "none")
  )
  for (p in plots) {
    expect_equal(p$theme$axis.text$size, 9)
    expect_equal(p$theme$legend.position, "none")
  }
})

test_that("facet adds bottom-strip house-style facets in every wrapper", {
  df <- data.frame(x = rep(c("a", "b"), 2), y = 1:4,
                   f = rep(c("p1", "p2"), each = 2))
  num <- data.frame(x = rep(2015:2016, 2), y = 1:4,
                    f = rep(c("p1", "p2"), each = 2))
  box_df <- data.frame(x = rep(c("a", "b"), 2), p5 = 1, p25 = 2, p50 = 3,
                       p75 = 4, p95 = 5, f = rep(c("p1", "p2"), each = 2))
  plots <- list(
    cpb_col(df, x = x, y = y, facet = f),
    cpb_area(df, x = x, y = y, fill = x, facet = f),
    cpb_line(num, x = x, y = y, facet = f),
    cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
            facet = f),
    cpb_scatter(num, x = x, y = y, facet = f),
    cpb_hist(df, x = y, bins = 2, facet = f)
  )
  for (p in plots) {
    expect_s3_class(p$facet, "FacetWrap")
    expect_equal(p$facet$params$strip.position, "bottom")
    # every panel is a complete mini-figure with its own axes
    expect_true(all(unlist(p$facet$params$axes)))
    expect_true(all(unlist(p$facet$params$axis.labels)))
  }
  # without facet the plot stays single-panel
  expect_s3_class(cpb_col(df, x = x, y = y)$facet, "FacetNull")
})

test_that("facet_ncol and facet_scales are forwarded", {
  df <- data.frame(x = rep(c("a", "b"), 2), y = 1:4,
                   f = rep(c("p1", "p2"), each = 2))
  p <- cpb_col(df, x = x, y = y, facet = f, facet_ncol = 1,
               facet_scales = "free_y")
  expect_equal(p$facet$params$ncol, 1)
  expect_true(p$facet$params$free$y)
  expect_false(p$facet$params$free$x)
})

test_that("theme_cpb places facet strips outside for bottom captions", {
  th <- theme_cpb()
  expect_equal(th$strip.placement, "outside")
  expect_equal(th$strip.text$face, "bold")
  expect_equal(th$strip.text$hjust, 0)
  expect_equal(th$strip.text$size, 7)
})

# grouped category axes ----

test_that("cpb_col group lays out gapped blocks with bold group labels", {
  df <- data.frame(
    cat = factor(rep(c("jongen", "meisje", "laag", "hoog"), each = 2),
                 levels = c("jongen", "meisje", "laag", "hoog")),
    grp = factor(rep(c("geslacht", "geslacht", "opleiding", "opleiding"), each = 2),
                 levels = c("geslacht", "opleiding")),
    serie = rep(c("a", "b"), 4),
    y = 1:8
  )
  p <- cpb_col(df, x = cat, y = y, fill = serie, group = grp,
               position = "dodge", title = "t")
  # categories at 1,2 then a gap, then 3.8,4.8
  sc <- p$scales$get_scales("x")
  expect_equal(sc$breaks, c(1, 2, 3.8, 4.8))
  expect_equal(sc$labels, c("jongen", "meisje", "laag", "hoog"))
  # the bold group labels are a text annotation at the group centres
  txt <- p$layers[[length(p$layers)]]
  expect_s3_class(txt$geom, "GeomText")
  expect_equal(txt$data$x, c(1.5, 4.3))
  expect_equal(txt$aes_params$label, c("geslacht", "opleiding"))
  expect_equal(txt$aes_params$fontface, "bold")
  # clip is off so the labels can render under the panel
  expect_equal(p$coordinates$clip, "off")
  # the axis-title line is reserved for the group labels
  expect_equal(p$labels$x, " ")
})

test_that("cpb_col group validates its input", {
  df <- data.frame(cat = c("a", "a"), grp = c("g1", "g2"), y = 1:2)
  expect_error(cpb_col(df, x = cat, y = y, group = grp),
               "exactly one `group`")
  df2 <- data.frame(cat = c("a", "b"), grp = c("g1", "g1"), y = 1:2)
  expect_error(cpb_col(df2, x = cat, y = y, group = grp,
                       orientation = "horizontal"),
               "vertical")
  expect_error(cpb_col(df2, x = cat, y = y, group = grp, forecast_x = 1.5),
               "forecast_x")
})

test_that("cpb_box group builds heading rows on the category axis", {
  df <- data.frame(
    cat = factor(c("Alle huishoudens", "1-20%", "21-40%", "Werkenden"),
                 levels = c("Alle huishoudens", "1-20%", "21-40%", "Werkenden")),
    grp = factor(c("Alle huishoudens", "Inkomensgroepen", "Inkomensgroepen",
                   "Inkomensbron"),
                 levels = c("Alle huishoudens", "Inkomensgroepen", "Inkomensbron")),
    p5 = -1, p25 = -0.2, p50 = 0.1, p75 = 0.3, p95 = 1
  )
  p <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               group = grp, orientation = "horizontal", title = "t")
  sc <- p$scales$get_scales("x")
  # only the 3 plain category rows are axis breaks (so the house ticks
  # land on them, not on the bold headings); positions descend so the
  # first group reads from the top under coord_flip()
  expect_equal(sc$labels, c("1-20%", "21-40%", "Werkenden"))
  expect_true(all(diff(sc$breaks) < 0))
  # the category ticks are kept on the category axis (y under flip)
  expect_s3_class(p$theme$axis.ticks.y, "element_line")
  # the 3 headings (2 group headings + the collapsed "Alle huishoudens"
  # total) are drawn as a bold text annotation, not as axis labels
  txt <- p$layers[[length(p$layers)]]
  expect_s3_class(txt$geom, "GeomText")
  expect_equal(sort(txt$aes_params$label),
               sort(c("Alle huishoudens", "Inkomensgroepen", "Inkomensbron")))
  expect_equal(txt$aes_params$fontface, "bold")
  # the collapsed total still carries its box, above the first break
  built <- ggplot2::ggplot_build(p)
  box_data <- built$data[[which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomBoxplot"), logical(1)))]]
  expect_true(max(box_data$x) > max(sc$breaks))
  # clip is off so the outdented headings can render outside the panel
  expect_equal(p$coordinates$clip, "off")
})

test_that("cpb_box group combines with a fill mapping (dodged boxes)", {
  df <- expand.grid(jaar = factor(c(2026, 2027)),
                    cat  = factor(c("a", "b", "c"), levels = c("a", "b", "c")))
  df$grp <- factor(ifelse(df$cat == "a", "G1", "G2"), levels = c("G1", "G2"))
  df$p5 <- 1; df$p25 <- 2; df$p50 <- 3; df$p75 <- 4; df$p95 <- 5
  p <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               fill = jaar, group = grp, orientation = "horizontal",
               position = ggplot2::position_dodge(width = 0.6),
               index = c(6, 2), title = "t")
  built <- ggplot2::ggplot_build(p)
  box_data <- built$data[[which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomBoxplot"), logical(1)))]]
  # 6 boxes (3 categories x 2 years), dodged: two distinct x offsets
  # around every category slot, so 6 unique positions in total
  expect_equal(nrow(box_data), 6)
  expect_length(unique(box_data$x), 6)
  expect_length(unique(box_data$fill), 2)
  # the heading rows are still bold text annotations
  txt <- p$layers[[length(p$layers)]]
  expect_s3_class(txt$geom, "GeomText")
  expect_equal(sort(txt$aes_params$label), c("G1", "G2"))
})

test_that("vector fill_colour tracks rows in james/modern boxes", {
  df <- data.frame(
    cat = factor(c("a", "b", "c"), levels = c("a", "b", "c")),
    grp = factor(c("G1", "G1", "G2"), levels = c("G1", "G2")),
    p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5
  )
  cols <- c("#87d2ff", "#87d2ff", "#e6006e")
  p <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               group = grp, box_style = "james", orientation = "horizontal",
               fill_colour = cols)
  built <- ggplot2::ggplot_build(p)
  box_i <- which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomBoxplot"), logical(1)))
  box_data <- built$data[[box_i]]
  # the box on the highest slot (category "a", top of the flipped
  # axis) is light blue; the lowest (category "c") is magenta
  expect_equal(as.character(box_data$fill[which.max(box_data$x)]), "#87d2ff")
  expect_equal(as.character(box_data$fill[which.min(box_data$x)]), "#e6006e")
})

# maps ----

test_that("cpb_nl_geo returns the three bundled levels", {
  for (lvl in c("gemeente", "corop", "provincie")) {
    g <- cpb_nl_geo(lvl)
    expect_true(all(c("code", "name", "part", "ring", "x", "y") %in% names(g)))
    expect_gt(nrow(g), 100)
  }
  expect_length(unique(cpb_nl_geo("provincie")$code), 12)
  expect_length(unique(cpb_nl_geo("corop")$code), 40)
})

test_that("cpb_map joins by code or name and styles the borders", {
  prov <- data.frame(code = unique(cpb_nl_geo("provincie")$code))
  prov$w <- seq_len(nrow(prov))
  p <- cpb_map(prov, region = code, value = w, level = "provincie",
               title = "t", subtitle = "s")
  poly <- p$layers[[1]]
  expect_s3_class(poly$geom, "GeomPolygon")
  # borders: thin, in the background colour (the deliberate deviation)
  expect_equal(poly$aes_params$colour, cpb_tokens()$background)
  expect_equal(poly$aes_params$linewidth, 0.1)
  expect_equal(p$coordinates$ratio, 1)  # fixed 1:1 aspect (RD metres)
  # numeric values get the continuous CPB scale
  expect_s3_class(p$scales$get_scales("fill"), "ScaleContinuous")
  # map theme: no axes
  expect_s3_class(p$theme$axis.text, "element_blank")

  # join by name works too, and a missing region fills as NA
  prov2 <- data.frame(naam = unique(cpb_nl_geo("provincie")$name)[1:11])
  prov2$w <- 1:11
  p2 <- cpb_map(prov2, region = naam, value = w, level = "provincie")
  expect_true(anyNA(p2$data$cpb__value))
  expect_false(anyNA(p2$data$cpb__value[p2$data$name == prov2$naam[1]]))
})

test_that("cpb_map warns on unmatched regions and errors on duplicates", {
  df <- data.frame(r = c("Groningen", "Atlantis"), w = 1:2)
  expect_warning(cpb_map(df, region = r, value = w, level = "provincie"),
                 "Atlantis")
  df2 <- data.frame(r = c("Groningen", "Groningen"), w = 1:2)
  expect_error(cpb_map(df2, region = r, value = w, level = "provincie"),
               "one row per region")
})

test_that("cpb_map uses discrete CPB palettes for discrete values", {
  prov <- data.frame(code = unique(cpb_nl_geo("provincie")$code))
  prov$klasse <- factor(rep(c("laag", "hoog"), 6))
  p <- cpb_map(prov, region = code, value = klasse, level = "provincie")
  expect_s3_class(p$scales$get_scales("fill"), "ScaleDiscrete")
})
