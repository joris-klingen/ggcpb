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
  # the value axis always gets its own flush y scale (see the
  # always-flush test below); only a *colour* scale is conditional
  expect_null(p1$scales$get_scales("colour"))

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

test_that("cpb_key_errorbar draws a capped bar, not geom_errorbar's default bare line", {
  key_data <- data.frame(colour = "black", linewidth = 0.4, linetype = 1, alpha = NA)

  glyph_h <- cpb_key_errorbar("horizontal")
  g_h <- glyph_h(key_data, list(), NULL)
  expect_s3_class(g_h, "gTree")
  expect_length(g_h$children, 3) # the bar itself plus a cap at each end

  glyph_v <- cpb_key_errorbar("vertical")
  g_v <- glyph_v(key_data, list(), NULL)
  expect_length(g_v$children, 3)
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

test_that("the value axis is always flush at both ends via pretty() breaks", {
  df_pos <- data.frame(x = c("a", "b"), y = c(1, 2))
  sc <- cpb_col(df_pos, x = x, y = y)$scales$get_scales("y")
  expect_equal(sc$expand, ggplot2::expansion(mult = c(0, 0)))
  expect_true(sc$limits[1] <= 0 && sc$limits[2] >= 2)

  df_neg <- data.frame(x = c("a", "b"), y = c(-1, -2))
  sc2 <- cpb_col(df_neg, x = x, y = y)$scales$get_scales("y")
  expect_equal(sc2$expand, ggplot2::expansion(mult = c(0, 0)))
  expect_true(sc2$limits[1] <= -2 && sc2$limits[2] >= 0)

  # mixed-sign data is now flush too (axis limits sit exactly on the data)
  df_mix <- data.frame(x = c("a", "b"), y = c(-1, 2))
  sc3 <- cpb_col(df_mix, x = x, y = y)$scales$get_scales("y")
  expect_false(is.null(sc3))
  expect_true(sc3$limits[1] <= -1 && sc3$limits[2] >= 2)
})

test_that("cpb_col value_breaks land on the wrapper-built value scale", {
  df <- data.frame(x = c("a", "b"), y = c(10, 60))
  sc <- cpb_col(df, x = x, y = y, pct_axis = TRUE,
                value_breaks = seq(0, 70, 10))$scales$get_scales("y")
  expect_equal(sc$breaks, seq(0, 70, 10))
  expect_true(is.function(sc$labels))
})

test_that("cpb_line draws the value axis flush via the scale, not a blanket coord expand", {
  df <- data.frame(x = c("a", "b", "c"), y = 4:6)
  p <- cpb_line(df, x = x, y = y)
  # coord is left at its default expand so a discrete x axis keeps its
  # normal padding (a blanket coord_cartesian(expand = FALSE) here
  # previously clipped markers/labels at the first or last category);
  # the value axis flush comes from the scale's own limits/expand. (A
  # *numeric* x instead gets its own coord-based flush by default now
  # -- see the x_lim_follow_data tests -- since only that survives a
  # caller's own follow-up scale_x_continuous(); a discrete x cannot
  # use the same trick, for the reason above, so it keeps the
  # scale-based flush and this test's original x = 1:3 case no longer
  # demonstrates it.)
  expect_true(p$coordinates$expand)
  sc <- p$scales$get_scales("y")
  expect_equal(sc$expand, ggplot2::expansion(mult = c(0, 0)))
  expect_true(sc$limits[1] <= 4 && sc$limits[2] >= 6)
})

test_that("cpb_line keeps discrete x-axis padding with points = TRUE, when asked to (regression)", {
  df <- data.frame(cat = factor(c("18-25", "26-35", "36-45"),
                                levels = c("18-25", "26-35", "36-45")),
                    y = c(5, 12, 18))
  # x_lim_follow_data defaults to TRUE now (see below), so this only
  # still protects against points = TRUE stripping the padding via
  # some *other*, unintended path when the caller has opted back into
  # ggplot2's normal padded margin
  p <- cpb_line(df, x = cat, y = y, points = TRUE, x_lim_follow_data = FALSE)
  built <- ggplot2::ggplot_build(p)
  xr <- built$layout$panel_params[[1]]$x.range
  # category positions are 1:3; a stripped discrete expansion would
  # make the range exactly c(1, 3) with no padding at either end
  expect_true(xr[1] < 1 && xr[2] > 3)
})

test_that("x_lim_follow_data defaults to TRUE: the x axis sits flush to the data by default", {
  df <- data.frame(cat = factor(c("18-25", "26-35", "36-45"),
                                levels = c("18-25", "26-35", "36-45")),
                    y = c(5, 12, 18))
  p <- cpb_line(df, x = cat, y = y)
  built <- ggplot2::ggplot_build(p)
  xr <- built$layout$panel_params[[1]]$x.range
  expect_equal(xr, c(1, 3))
})

test_that("a whole-number x axis never gets a fractional break, flush or not", {
  # confirmed empirically to trip ggplot2's own default break algorithm
  # for a plain integer `x` (2010-2022 -> ..., 2012.5, ..., 2022.5)
  df <- data.frame(jaar = 2010:2022, mld = seq_len(13))
  for (follow in c(TRUE, FALSE)) {
    p <- cpb_line(df, x = jaar, y = mld, x_lim_follow_data = follow)
    br <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks
    br <- br[!is.na(br)]
    expect_true(all(br == round(br)), info = paste("follow =", follow))
  }
  # a very short range trips even base pretty() itself
  # (2020-2021 -> 2020, 2020.2, 2020.4, ...)
  df2 <- data.frame(jaar = 2020:2021, mld = c(1, 2))
  p2 <- cpb_line(df2, x = jaar, y = mld, x_lim_follow_data = TRUE)
  br2 <- ggplot2::ggplot_build(p2)$layout$panel_params[[1]]$x$breaks
  br2 <- br2[!is.na(br2)]
  expect_true(all(br2 == round(br2)))
  expect_true(length(br2) >= 2)
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
  p3 <- cpb_scatter(df, x = x, y = y, colour = grp, colour_index = c(6, 2))
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
  p <- cpb_hist(df, x = waarde, fill = grp, bins = 10, fill_index = c(6, 2))
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
                 ymin = lo, ymax = hi, colour_index = c(6, 2))
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
  # cpb_line() applies value_limits on the scale too, not just the
  # coord -- but here `num`'s x (2015:2017) is numeric, so it also
  # gets its own coord-based flush by default (x_lim_follow_data),
  # which folds expand = FALSE into this same coord_cartesian() call;
  # that is safe for the value axis's own already-zero expansion (see
  # cpb_flush_scale_args()), so both agree on the same (0, 10) either
  # way
  p <- cpb_line(num, x = x, y = y, colour = g, value_limits = c(0, 10))
  expect_equal(p$scales$get_scales("y")$limits, c(0, 10))
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

test_that("sec_type controls how sec_y is drawn, sharing one legend key", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), heffing = c(1.2, 1.4, 1.1))

  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing)
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% classes)

  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "point")
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% classes)
  expect_false("GeomLine" %in% classes)

  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "col")
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_equal(sum(classes == "GeomCol"), 2) # the primary bars plus the secondary ones
  expect_false("GeomLine" %in% classes)

  # sec_points only takes effect for sec_type = "line"
  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "col", sec_points = TRUE)
  classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomPoint" %in% classes)

  # all three share one legend key (one colour scale), not one per geom
  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "col")
  n_colour_scales <- sum(vapply(p$scales$scales, function(s) "colour" %in% s$aesthetics, logical(1)))
  expect_equal(n_colour_scales, 1)
})

test_that("sec_y always gets a legend, even without a fill mapping", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), heffing = c(1.2, 1.4, 1.1))

  # no fill, no sec_y: a single-series bar chart needs no legend at all
  p <- cpb_col(df, x = jaar, y = mld)
  n_fill_scales <- sum(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_equal(n_fill_scales, 0)

  # no fill, but sec_y present: the primary bars get a one-level dummy
  # fill scale (named after the y column) so both series show up, and
  # -- since two axes now share the legend -- suffixed "(linkeras)" to
  # tell it apart from sec_y's own "(rechteras)" key. Scales must be
  # trained (via ggplot_build()) before get_breaks() reflects the
  # data -- p$scales itself is never mutated in place.
  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing)
  fill_scale <- ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")
  expect_false(is.null(fill_scale))
  expect_equal(fill_scale$get_labels(fill_scale$get_breaks()), "mld (linkeras)")
  colour_scale <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  expect_match(colour_scale$get_labels(colour_scale$get_breaks()), "\\(rechteras\\)$")

  # a real fill mapping still works, every level suffixed the same way
  df2 <- data.frame(jaar = rep(2018:2019, each = 2), soort = rep(c("a", "b"), 2),
                    mld = c(4, 6, 5, 7), heffing = rep(c(1.2, 1.4), each = 2))
  p2 <- cpb_col(df2, x = jaar, y = mld, fill = soort, sec_y = heffing)
  fill_scale2 <- ggplot2::ggplot_build(p2)$plot$scales$get_scales("fill")
  expect_setequal(fill_scale2$get_labels(fill_scale2$get_breaks()),
                  c("a (linkeras)", "b (linkeras)"))

  # without sec_y, labels are never suffixed
  p3 <- cpb_col(df2, x = jaar, y = mld, fill = soort)
  fill_scale3 <- ggplot2::ggplot_build(p3)$plot$scales$get_scales("fill")
  expect_setequal(fill_scale3$get_labels(fill_scale3$get_breaks()), c("a", "b"))
})

test_that("x_lim zooms without dropping data, across all wrappers", {
  yr_df   <- data.frame(x = 2015:2020, y = 1:6)
  box_df  <- data.frame(x = 2015:2020, p5 = 1:6, p25 = 2:7, p50 = 3:8, p75 = 4:9, p95 = 5:10)
  dot_df  <- data.frame(x = 2015:2020, y = 1:6, lower = 0:5, upper = 2:7)
  hist_df <- data.frame(x = 1:100)

  p <- cpb_col(yr_df, x = x, y = y, x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 6)
  expect_true(b$layout$panel_params[[1]]$x.range[1] > 2015)

  p <- cpb_line(yr_df, x = x, y = y, x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 6)

  p <- cpb_area(yr_df, x = x, y = y, fill = factor("a"), x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 6)

  p <- cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[2]]), 6)

  p <- cpb_dot(dot_df, x = x, y = y, lower = lower, upper = upper, x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[2]]), 6)

  p <- cpb_scatter(yr_df, x = x, y = y, x_lim = c(2017, 2019))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 6)
  expect_equal(b$layout$panel_params[[1]]$x.range, c(2017, 2019))

  p <- cpb_hist(hist_df, x = x, binwidth = 10, x_lim = c(30, 60))
  b <- ggplot2::ggplot_build(p)
  expect_equal(sum(b$data[[1]]$count), 100)
})

test_that("x_lim_follow_data flushes the x axis to the data range, no rounding required", {
  yr_df <- data.frame(x = c(2015, 2016, 2019), y = c(1, 2, 3))

  p <- cpb_line(yr_df, x = x, y = y, x_lim_follow_data = TRUE)
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x.range, c(2015, 2019))

  p <- cpb_scatter(yr_df, x = x, y = y, x_lim_follow_data = TRUE)
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$x.range, c(2015, 2019))

  # x_lim (manual) takes priority over x_lim_follow_data when both are set:
  # the panel reflects the c(2010, 2020) zoom, not a flush to 2015-2019
  p <- cpb_line(yr_df, x = x, y = y, x_lim = c(2010, 2020), x_lim_follow_data = TRUE)
  b <- ggplot2::ggplot_build(p)
  expect_true(b$layout$panel_params[[1]]$x.range[1] < 2015)
  expect_true(b$layout$panel_params[[1]]$x.range[2] > 2019)
})

test_that("legend_ncol lays the legend out in the requested number of columns", {
  num <- data.frame(x = rep(2015:2017, 2), g = rep(c("s1", "s2"), each = 3),
                    y = c(1:3, 2:4))
  cat_df <- data.frame(x = c("a", "b"), y = c(1, 2), g = c("s1", "s2"))

  # fill-based wrappers
  expect_equal(cpb_col(cat_df, x = x, y = y, fill = g, legend_ncol = 2)$guides$guides$fill$params$ncol, 2)
  expect_equal(cpb_area(num, x = x, y = y, fill = g, legend_ncol = 2)$guides$guides$fill$params$ncol, 2)
  box_df <- data.frame(x = c("a", "b"), p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5, g = c("s1", "s2"))
  expect_equal(cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                       fill = g, legend_ncol = 2)$guides$guides$fill$params$ncol, 2)
  expect_equal(cpb_hist(num, x = y, fill = g, legend_ncol = 2)$guides$guides$fill$params$ncol, 2)

  # colour-based wrappers
  expect_equal(cpb_line(num, x = x, y = y, colour = g, legend_ncol = 3)$guides$guides$colour$params$ncol, 3)
  expect_equal(cpb_scatter(num, x = x, y = y, colour = g, legend_ncol = 3)$guides$guides$colour$params$ncol, 3)
  dot_df <- data.frame(x = c("a", "b"), y = c(1, 2), lower = c(0, 1), upper = c(2, 3), g = c("s1", "s2"))
  expect_equal(cpb_dot(dot_df, x = x, y = y, lower = lower, upper = upper,
                       colour = g, legend_ncol = 3)$guides$guides$colour$params$ncol, 3)

  # a numeric colour column keeps its continuous colourbar untouched
  numc <- transform(num, g = as.numeric(factor(g)))
  expect_null(cpb_scatter(numc, x = x, y = y, colour = g, legend_ncol = 2)$guides$guides$colour)

  # NULL (default) is a no-op: no guides() call added at all
  expect_null(cpb_col(cat_df, x = x, y = y, fill = g, reverse_legend = FALSE)$guides$guides$fill)

  # reverse_legend and legend_ncol combine in the same guide_legend()
  p <- cpb_col(cat_df, x = x, y = y, fill = g, reverse_legend = TRUE, legend_ncol = 2)
  expect_true(p$guides$guides$fill$params$reverse)
  expect_equal(p$guides$guides$fill$params$ncol, 2)

  # cpb_col's secondary-axis layout (order = 1/2) also honours legend_ncol
  sec_df <- data.frame(x = c("a", "b", "c"), y = c(1, 2, 3), s = c(0.5, 1.5, 1.0))
  p_sec <- cpb_col(sec_df, x = x, y = y, sec_y = s, legend_ncol = 2)
  expect_equal(p_sec$guides$guides$fill$params$ncol, 2)
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
               fill_index = c(6, 2), title = "t")
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
  # borders: thin background-colour seams, the default
  expect_equal(poly$aes_params$colour, cpb_tokens()$bg)
  expect_equal(poly$aes_params$linewidth, 0.15)
  # legend sits inside the panel at top-left by default
  expect_equal(p$theme$legend.position, "inside")
  expect_equal(p$theme$legend.position.inside, c(0, 0.98))
  expect_equal(p$coordinates$ratio, 1)  # fixed 1:1 aspect (RD metres)
  # numeric values get the continuous CPB scale
  expect_s3_class(p$scales$get_scales("fill"), "ScaleContinuous")
  # map theme: no axes
  expect_s3_class(p$theme$axis.text, "element_blank")
  # tagged with the boundaries' true aspect ratio, for save_cpb() to
  # auto-fit the panel to (see test-save.R)
  geo <- cpb_nl_geo("provincie")
  expect_equal(attr(p, "cpb_map_aspect"),
               diff(range(geo$y)) / diff(range(geo$x)))

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

test_that("cpb_box value_axis = 'top' puts the value scale on top", {
  df <- data.frame(groep = factor(c("a", "b", "c"), levels = c("a", "b", "c")),
                   p5 = -1, p25 = -0.2, p50 = 0.1, p75 = 0.3, p95 = 1)
  p <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               orientation = "horizontal", value_axis = "top",
               value_breaks = c(-1, 0, 1))
  sc <- p$scales$get_scales("y")
  # under coord_flip(), the y scale's "right" position renders at the top
  expect_equal(sc$position, "right")
  # bottom (default) leaves the scale in its normal spot
  p2 <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                orientation = "horizontal", value_breaks = c(-1, 0, 1))
  expect_false(identical(p2$scales$get_scales("y")$position, "right"))
})

test_that("cpb_box's ylab/xlab follow cpb_col()'s convention in both orientations", {
  # ylab always describes whichever axis ends up vertical and is
  # promoted to the subtitle; xlab always describes whichever axis
  # ends up horizontal, as an ordinary (un-rotated) axis title --
  # verified against cpb_col()'s own, already-correct behaviour
  # (cpb_box()'s horizontal case used to promote xlab instead, the
  # opposite of cpb_col())
  df <- data.frame(groep = factor(c("a", "b", "c")),
                   p5 = -1, p25 = -0.2, p50 = 0.1, p75 = 0.3, p95 = 1)

  # default "vertical": ylab -> subtitle (value), xlab -> bottom title (category)
  p_v <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                orientation = "vertical", ylab = "eenheid", xlab = "categorie")
  expect_identical(p_v$labels$subtitle, "eenheid")
  expect_identical(p_v$labels$x, "categorie")
  expect_null(p_v$labels$y)

  # "horizontal": ylab -> subtitle (category), xlab -> bottom title (value)
  p_h <- cpb_box(df, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                orientation = "horizontal", ylab = "categorie", xlab = "eenheid")
  expect_identical(p_h$labels$subtitle, "categorie")
  expect_identical(p_h$labels$y, "eenheid")
  expect_null(p_h$labels$x)
})

test_that("cpb_map seams and legend fall back on request", {
  prov <- data.frame(code = unique(cpb_nl_geo("provincie")$code), w = 1:12)
  p <- cpb_map(prov, region = code, value = w, level = "provincie")
  poly <- p$layers[[1]]
  expect_equal(poly$aes_params$colour, cpb_tokens()$bg)
  expect_equal(poly$aes_params$linewidth, 0.15)
  # legend = "bottom" falls back to the flush bottom-left theme legend
  p2 <- cpb_map(prov, region = code, value = w, level = "provincie",
                legend = "bottom")
  expect_equal(p2$theme$legend.position, "bottom")
  expect_null(p2$theme$legend.position.inside)
  # a passed border_colour overrides the background default (e.g. white)
  p3 <- cpb_map(prov, region = code, value = w, level = "provincie",
                border_colour = "white")
  expect_equal(p3$layers[[1]]$aes_params$colour, "white")
})

test_that("cpb_line(points = TRUE) adds markers and keeps the lines joined", {
  df <- data.frame(
    cat = factor(rep(c("20-30", "30-40", "40-50"), 2),
                 levels = c("20-30", "30-40", "40-50")),
    y = c(1, 2, 3, 4, 5, 6),
    reeks = rep(c("a", "b"), each = 3)
  )
  p <- cpb_line(df, x = cat, y = y, colour = reeks, points = TRUE)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_true("GeomLine" %in% geoms)
  # a discrete x would otherwise leave every observation in its own
  # group and drop the lines entirely
  expect_equal(rlang::as_label(p$mapping$group), "reeks")
  expect_equal(nrow(unique(ggplot2::layer_data(p, 2)["group"])), 2)
  # markers are off by default
  p0 <- cpb_line(df, x = cat, y = y, colour = reeks)
  expect_false("GeomPoint" %in%
                 vapply(p0$layers, function(l) class(l$geom)[1], character(1)))
  # without a colour mapping the single series stays one group
  p1 <- cpb_line(df[df$reeks == "a", ], x = cat, y = y, points = TRUE)
  expect_equal(nrow(unique(ggplot2::layer_data(p1, 1)["group"])), 1)
})

test_that("cpb_box box_style = 'dot' draws markers with a named legend", {
  df <- data.frame(cat = c("a", "b"), p5 = c(0, 1), p25 = c(1, 2),
                   p50 = c(2, 3), p75 = c(3, 4), p95 = c(4, 5),
                   gem = c(2.2, 3.4))
  p <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75,
               p95 = p95, mean = gem, box_style = "dot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # no box: the style is markers plus ranges only
  expect_false("GeomBoxplot" %in% geoms)
  expect_equal(sum(geoms == "GeomPoint"), 4L)   # p5, p95, median, mean
  # the mean marker is dropped when no mean column is given
  p0 <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75,
                p95 = p95, box_style = "dot")
  expect_equal(
    sum(vapply(p0$layers, function(l) class(l$geom)[1], character(1)) ==
          "GeomPoint"), 3L
  )
  # every statistic is named in the legend, in the published order
  sc <- p$scales$get_scales("colour")
  expect_s3_class(sc, "ScaleDiscrete")
  expect_equal(sc$breaks, c("5e percentiel", "95e percentiel",
                            "25e-75e percentiel", "mediaan", "gemiddelde"))
  # each layer is tagged with the statistic it draws, so a legend key
  # only ever picks up the glyph of its own layer
  tags <- unlist(lapply(p$layers, function(l) {
    if (is.null(l$mapping$colour)) NULL else rlang::eval_tidy(l$mapping$colour)
  }))
  expect_equal(sort(unname(tags)), sort(sc$breaks))
  # with no mean column the mean is left out of the legend entirely
  expect_false("gemiddelde" %in% p0$scales$get_scales("colour")$breaks)
  # labels are overridable one at a time
  p2 <- cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75,
                p95 = p95, box_style = "dot", dot_labels = c(p50 = "median"))
  expect_true("median" %in% p2$scales$get_scales("colour")$breaks)
  # the untouched labels keep their defaults
  expect_true("5e percentiel" %in% p2$scales$get_scales("colour")$breaks)
  expect_error(
    cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
            box_style = "dot", dot_labels = c(nope = "x")),
    "named character vector"
  )
  # the mean marker belongs to this style alone
  expect_error(
    cpb_box(df, x = cat, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
            mean = gem, box_style = "james"),
    "only drawn by"
  )
})

test_that("cpb_dot draws estimates with intervals and a zero line", {
  df <- data.frame(term = c("a", "b", "c"), est = c(1, -2, 0.5),
                   lo = c(0.2, -3, -0.4), hi = c(1.8, -1, 1.4))
  p <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomHline", "GeomErrorbar", "GeomPoint") %in% geoms))
  expect_s3_class(p$coordinates, "CoordFlip")
  # the reference line can be turned off
  p0 <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi, zeroline = FALSE)
  expect_false("GeomHline" %in%
                 vapply(p0$layers, function(l) class(l$geom)[1], character(1)))
  # vertical drops coord_flip()
  pv <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi,
                orientation = "vertical")
  expect_false(inherits(pv$coordinates, "CoordFlip"))
})

test_that("cpb_dot's ylab/xlab follow cpb_col()'s convention in both orientations", {
  # ylab always describes whichever axis ends up vertical and is
  # promoted to the subtitle; xlab always describes whichever axis
  # ends up horizontal, as an ordinary (un-rotated) axis title --
  # verified against cpb_col()'s own, already-correct behaviour, not
  # just re-asserting whatever cpb_dot() used to do
  df <- data.frame(term = c("a", "b", "c"), est = c(1, -2, 0.5),
                   lo = c(0.2, -3, -0.4), hi = c(1.8, -1, 1.4))

  # default "horizontal": ylab -> subtitle (category), xlab -> bottom title (value)
  p_h <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi,
                orientation = "horizontal", ylab = "categorie", xlab = "eenheid")
  expect_identical(p_h$labels$subtitle, "categorie")
  expect_identical(p_h$labels$y, "eenheid")
  expect_null(p_h$labels$x)

  # "vertical": ylab -> subtitle (value), xlab -> bottom title (category)
  p_v <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi,
                orientation = "vertical", ylab = "eenheid", xlab = "categorie")
  expect_identical(p_v$labels$subtitle, "eenheid")
  expect_identical(p_v$labels$x, "categorie")
  expect_null(p_v$labels$y)
})

test_that("cpb_dot lays groups out under bold headings", {
  df <- data.frame(
    term = c("a", "b", "c", "d"), blok = rep(c("een", "twee"), each = 2),
    est = c(1, 2, 3, 4), lo = c(0, 1, 2, 3), hi = c(2, 3, 4, 5)
  )
  p <- cpb_dot(df, x = term, y = est, lower = lo, upper = hi, group = blok)
  # the category axis becomes numeric slots, with the headings drawn as
  # text rather than as axis breaks
  sc <- p$scales$get_scales("x")
  expect_s3_class(sc, "ScaleContinuous")
  expect_equal(length(sc$get_labels()), 4L)
  txt <- p$layers[[length(p$layers)]]
  expect_s3_class(txt$geom, "GeomText")
  expect_equal(sort(txt$aes_params$label), c("een", "twee"))
  expect_equal(txt$aes_params$fontface, "bold")
  expect_equal(p$coordinates$clip, "off")
})

test_that("cpb_col(sec_y) rescales a line onto a secondary axis", {
  df <- data.frame(jaar = rep(2020:2022, 2), v = c(6, 8, 10, 4, 2, 5),
                   soort = rep(c("a", "b"), each = 3),
                   klein = rep(c(0.5, 1, 1.5), 2))
  p <- cpb_col(df, x = jaar, y = v, fill = soort, sec_y = klein,
               sec_limits = c(0, 2), sec_label = "klein (rechteras)")
  sc <- p$scales$get_scales("y")
  expect_s3_class(sc$secondary.axis, "AxisSecondary")
  # the primary stack tops out at 15, so the secondary maximum of 2
  # maps onto it and the line's midpoint lands halfway
  line <- Filter(function(l) inherits(l$geom, "GeomLine"), p$layers)[[1]]
  expect_equal(nrow(line$data), 3L)          # one row per x, not per fill
  expect_equal(line$data$cpb__sec, c(3.75, 7.5, 11.25))
  expect_equal(rlang::eval_tidy(line$mapping$colour), "klein (rechteras)")
  # the combination is refused where it cannot be drawn
  expect_error(
    cpb_col(df, x = jaar, y = v, fill = soort, sec_y = klein,
            orientation = "horizontal"),
    "only supported for vertical"
  )
  expect_error(
    cpb_col(df, x = jaar, y = v, fill = soort, sec_y = klein,
            position = "fill"),
    "position = \"fill\""
  )
})

test_that("sec_y's default sec_limits use its own data range, not forced through zero", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), tekort = c(-30, -20, -25))

  p_default <- cpb_col(df, x = jaar, y = mld, sec_y = tekort)
  p_explicit <- cpb_col(df, x = jaar, y = mld, sec_y = tekort,
                        sec_limits = range(df$tekort))
  line_default <- Filter(function(l) inherits(l$geom, "GeomLine"), p_default$layers)[[1]]
  line_explicit <- Filter(function(l) inherits(l$geom, "GeomLine"), p_explicit$layers)[[1]]
  expect_equal(line_default$data$cpb__sec, line_explicit$data$cpb__sec)

  # forcing 0 into an all-negative series' range is a different,
  # visually much more compressed mapping -- confirms the default
  # actually changed, not just that it matches one particular
  # explicit sec_limits
  p_zero_forced <- cpb_col(df, x = jaar, y = mld, sec_y = tekort,
                           sec_limits = c(-30, 0))
  line_zero_forced <- Filter(function(l) inherits(l$geom, "GeomLine"), p_zero_forced$layers)[[1]]
  expect_false(isTRUE(all.equal(line_default$data$cpb__sec, line_zero_forced$data$cpb__sec)))
})

test_that("sec_limits' non-zero-range check tolerates floating point noise, not just exact equality", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), heffing = c(1.2, 1.4, 1.1))

  # representable as unequal doubles, but the same value in every way
  # that matters -- must still be caught, not treated as a valid range
  expect_error(
    cpb_col(df, x = jaar, y = mld, sec_y = heffing,
            sec_limits = c(0.06, 0.06 + 1e-10)),
    "non-zero range"
  )
  # a genuinely narrow range is a different thing and must still work
  expect_no_error(
    cpb_col(df, x = jaar, y = mld, sec_y = heffing,
            sec_limits = c(0.02, 0.06))
  )
})

test_that("cpb_sec_map()'s primary-range check also uses floating point tolerance", {
  # exercised directly: a primary axis with genuinely no data range to
  # map onto is very hard to reach through any wrapper's own public
  # arguments (their value-axis breaks already guard against it), so
  # this is the one sec_y check tested against the shared helper
  # itself rather than through a wrapper
  expect_error(
    cpb_sec_map(c(1, 2), NULL, 5, 5 + 1e-10),
    "no range for `sec_y`"
  )
  expect_no_error(cpb_sec_map(c(1, 2), NULL, 0.02, 0.06))
})

test_that("sec_accuracy rounds the secondary axis's own labels independently of the primary axis", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), heffing = c(1.234, 1.456, 1.789))
  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_accuracy = 0.01)
  sc <- p$scales$get_scales("y")
  expect_equal(sc$secondary.axis$labels(c(1.234, 1.5)), c("1,23", "1,50"))
})

test_that("sec_point_size and sec_col_width replace the old hardcoded sizes", {
  df <- data.frame(jaar = 2018:2020, mld = c(10, 12, 9), heffing = c(1.2, 1.4, 1.1))

  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "point",
              sec_point_size = 3)
  pt <- Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers)[[1]]
  expect_equal(pt$aes_params$size, 3)

  p <- cpb_col(df, x = jaar, y = mld, sec_y = heffing, sec_type = "col",
              sec_col_width = 0.7)
  cols <- Filter(function(l) inherits(l$geom, "GeomCol"), p$layers)
  expect_equal(cols[[2]]$aes_params$width, 0.7) # [[1]] is the primary bars

  # cpb_dot()'s own sec_point_size defaults to its primary `size`, so a
  # sec_type = "point" series reads as the same kind of mark by default
  df2 <- data.frame(
    term = c("a", "b", "c"), est = c(1, 2, 3), lo = c(0, 1, 2), hi = c(2, 3, 4),
    heffing = c(1.2, 1.4, 1.1)
  )
  p <- cpb_dot(df2, x = term, y = est, lower = lo, upper = hi,
              sec_y = heffing, sec_type = "point", size = 2.5,
              orientation = "vertical")
  point_sizes <- vapply(
    Filter(function(l) inherits(l$geom, "GeomPoint"), p$layers),
    function(l) l$aes_params$size, numeric(1)
  )
  expect_true(all(point_sizes == 2.5))
})

test_that("cpb_donut returns a ring built from GeomCol + coord_polar with a fill scale", {
  df <- data.frame(bron = c("gas", "elektriciteit", "warmte"), share = c(50, 30, 20))
  p <- cpb_donut(df, fill = bron, y = share)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomCol"))
  expect_s3_class(p$coordinates, "CoordPolar")
  has_fill_scale <- any(vapply(p$scales$scales, function(s) "fill" %in% s$aesthetics, logical(1)))
  expect_true(has_fill_scale)
  # every wedge stacks onto the same constant x position
  expect_equal(rlang::eval_tidy(p$mapping$x), 1)
})

test_that("cpb_donut draws no axis text, ticks or gridlines", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(60, 40))
  p <- cpb_donut(df, fill = bron, y = share)
  th <- p$theme

  expect_s3_class(th$axis.text, "element_blank")
  expect_s3_class(th$axis.ticks, "element_blank")
  expect_s3_class(th$axis.title, "element_blank")
  expect_s3_class(th$panel.grid, "element_blank")
})

test_that("cpb_donut's ring_width controls the hole via xlim, up to a full pie at 2", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(60, 40))

  p_ring <- cpb_donut(df, fill = bron, y = share, ring_width = 0.6)
  rng <- p_ring$scales$get_scales("x")$get_limits()
  expect_equal(rng[[1]], 0)
  expect_equal(rng[[2]], 1 + 0.6 / 2 + 0.05)

  # a ring_width of 2 closes the hole completely (inner edge at 0)
  expect_no_error(cpb_donut(df, fill = bron, y = share, ring_width = 2))
  expect_error(cpb_donut(df, fill = bron, y = share, ring_width = 0), "ring_width")
  expect_error(cpb_donut(df, fill = bron, y = share, ring_width = 2.5), "ring_width")
})

test_that("cpb_donut rejects negative values", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(60, -10))
  expect_error(cpb_donut(df, fill = bron, y = share), "non-negative")
})

test_that("cpb_donut prints a percentage wedge label by default, computed from y", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  p <- cpb_donut(df, fill = bron, y = share)

  txt <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(txt, 1L)
  expect_equal(p$data[["cpb__wedge_label"]], c("75%", "25%"))
})

test_that("cpb_donut's label column prefixes the auto-computed percentage", {
  df <- data.frame(bron = c("gas", "elektriciteit"),
                    share = c(75, 25), mld = c("7,5 mld", "2,5 mld"))
  p <- cpb_donut(df, fill = bron, y = share, label = mld)
  expect_equal(p$data[["cpb__wedge_label"]], c("7,5 mld (75%)", "2,5 mld (25%)"))
})

test_that("cpb_donut's wedge_labels = FALSE omits the text layer", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  p <- cpb_donut(df, fill = bron, y = share, wedge_labels = FALSE)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
})

test_that("cpb_donut rejects legend = \"none\" when wedge_labels = FALSE", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  expect_error(
    cpb_donut(df, fill = bron, y = share, wedge_labels = FALSE, legend = "none"),
    "wedge_labels"
  )
})

test_that("cpb_donut's legend_pct suffixes the fill legend with each share", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  p <- cpb_donut(df, fill = bron, y = share, wedge_labels = FALSE, legend_pct = TRUE)

  sc <- p$scales$get_scales("fill")
  expect_equal(sc$labels(c("gas", "elektriciteit")), c("gas (75%)", "elektriciteit (25%)"))
})

test_that("cpb_donut's label_style = \"leader\" draws a tick, a fan-out path and text outside the ring", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  p <- cpb_donut(df, fill = bron, y = share, label_style = "leader", leader_length = 0.2)

  segs <- Filter(function(l) inherits(l$geom, "GeomSegment"), p$layers)
  expect_length(segs, 1L) # the radial tick, perpendicular to the wedge
  paths <- Filter(function(l) inherits(l$geom, "GeomPath"), p$layers)
  expect_length(paths, 1L) # the straight fan-out run to the label
  txt <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)
  expect_length(txt, 1L)
  expect_equal(txt[[1]]$data[["wedge_label"]], c("75%", "25%"))
  # with nothing to separate, the tick starts exactly at the ring's
  # own outer edge and ends leader_length further out, for both wedges
  outer_edge <- 1 + 0.6 / 2
  expect_equal(unique(segs[[1]]$data$x_wedge), outer_edge)
  expect_equal(unique(segs[[1]]$data$x_tick), outer_edge + 0.2)
})

test_that("cpb_donut's label_style = \"wedge\" (default) draws no leader line", {
  df <- data.frame(bron = c("gas", "elektriciteit"), share = c(75, 25))
  p <- cpb_donut(df, fill = bron, y = share)
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1))))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPath"), logical(1))))
})

test_that("cpb_donut's label_style = \"leader\" separates labels whose wedges are close together", {
  # two wedges tiny and adjacent enough that their labels would land
  # at (near) the same height without the collision-avoidance nudge
  df <- data.frame(bron = c("groot", "klein1", "klein2"), share = c(98, 1, 1))
  p <- cpb_donut(df, fill = bron, y = share, label_style = "leader")

  txt <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
  # both tiny wedges' labels are on the same side (both near the top);
  # their final y_text values must differ by a real, visible amount --
  # not just fail to be bit-for-bit identical, which a wedge that got
  # clamped straight back to its own unadjusted position would still
  # technically satisfy
  small <- txt$data[txt$data$wedge_label == "1%", ]
  expect_equal(nrow(small), 2L)
  expect_gt(abs(diff(small$y_text)), 0.3)
})

test_that("cpb_label_wrap() wraps long text and leaves short text alone", {
  long <- "Een erg lange categorienaam die nooit helemaal past"
  short <- "kort"
  wrapped <- cpb_label_wrap()(c(long, short))
  expect_true(grepl("\n", wrapped[[1]], fixed = TRUE))
  expect_false(grepl("\n", wrapped[[2]], fixed = TRUE))
})

test_that("long category tick labels wrap instead of shrinking the panel", {
  long_cats <- c("Een erg lange categorienaam die nooit helemaal past",
                 "Nog een categorienaam die veel te lang is voor de as")
  get_x_labels <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  }
  # coord_flip() (orientation = "horizontal") swaps which built
  # panel_params slot the category axis ends up in: still the "x"
  # aesthetic's own scale, but drawn -- and read back here -- as "y"
  get_category_labels <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  }

  # plain discrete x (cpb_x_scale())
  df <- data.frame(cat = long_cats, y = c(1, 2))
  labs <- get_x_labels(cpb_col(df, x = cat, y = y))
  expect_true(all(grepl("\n", labs, fixed = TRUE)))

  # grouped x (cpb_col()'s own scale_x_continuous(breaks/labels))
  df_grp <- data.frame(cat = long_cats, grp = c("g1", "g1"), y = c(1, 2))
  labs_grp <- get_x_labels(cpb_col(df_grp, x = cat, y = y, group = grp))
  expect_true(all(grepl("\n", labs_grp, fixed = TRUE)))

  # cpb_box(), horizontal (coord_flip() draws the category axis as "y")
  box_df <- data.frame(cat = long_cats, p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)
  labs_box <- get_category_labels(cpb_box(box_df, x = cat, p5 = p5, p25 = p25, p50 = p50,
                                          p75 = p75, p95 = p95, orientation = "horizontal"))
  expect_true(all(grepl("\n", labs_box, fixed = TRUE)))

  # cpb_dot(), default horizontal orientation (category axis as "y" too)
  dot_df <- data.frame(cat = long_cats, y = c(1, 2), lower = c(0, 1), upper = c(2, 3))
  labs_dot <- get_category_labels(cpb_dot(dot_df, x = cat, y = y, lower = lower, upper = upper))
  expect_true(all(grepl("\n", labs_dot, fixed = TRUE)))
})

test_that("cpb_donut keeps legend labels single-line but wraps wedge labels (wedge and leader alike)", {
  long_cats <- c("Een erg lange categorienaam die nooit helemaal past",
                 "Nog een categorienaam die veel te lang is voor de as")
  df <- data.frame(bron = long_cats, share = c(60, 40))

  # unlike the axis ticks and wedge/leader labels, a legend entry stays
  # exactly one row -- however long the category name runs
  p_legend <- cpb_donut(df, fill = bron, y = share)
  built <- ggplot2::ggplot_build(p_legend)
  fill_scale <- Filter(function(s) "fill" %in% s$aesthetics, built$plot$scales$scales)[[1]]
  legend_labs <- fill_scale$get_labels(fill_scale$get_breaks())
  expect_false(any(grepl("\n", legend_labs, fixed = TRUE)))
  expect_setequal(legend_labs, long_cats)

  p_wedge <- cpb_donut(df, fill = bron, y = share, label = bron, label_style = "wedge")
  wedge_txt <- Filter(function(l) inherits(l$geom, "GeomText"), p_wedge$layers)[[1]]
  expect_true(all(grepl("\n", wedge_txt$data$cpb__wedge_label, fixed = TRUE)))

  # "leader" places labels with its own collision-avoidance math sized
  # for a roughly-bounded label height (see cpb_wrap_capped()'s use in
  # cpb_donut()'s cpb__wedge_label build) -- wraps too, capped at 3 lines
  p_leader <- cpb_donut(df, fill = bron, y = share, label = bron, label_style = "leader")
  leader_txt <- Filter(function(l) inherits(l$geom, "GeomText"), p_leader$layers)[[1]]
  expect_true(all(grepl("\n", leader_txt$data$wedge_label, fixed = TRUE)))
  n_lines <- lengths(strsplit(leader_txt$data$wedge_label, "\n", fixed = TRUE))
  expect_true(all(n_lines <= 3))
})

test_that("cpb_wrap_capped() caps line count and truncates with '...' only when needed", {
  short <- "kort"
  long <- "Een erg lange naam die net past"
  xlong <- paste(
    "Deze allereerste categorienaam is opzettelijk absurd lang gemaakt",
    "om te zien wat er gebeurt als een label vele regels zou beslaan"
  )

  expect_identical(cpb_wrap_capped(short, max_lines = 3), short)

  wrapped <- cpb_wrap_capped(long, max_lines = 3)
  expect_false(grepl("...", wrapped, fixed = TRUE))
  expect_lte(length(strsplit(wrapped, "\n", fixed = TRUE)[[1]]), 3)

  capped <- cpb_wrap_capped(xlong, max_lines = 3)
  expect_length(strsplit(capped, "\n", fixed = TRUE)[[1]], 3)
  expect_match(capped, "\\.\\.\\.$")
})

test_that("cpb_label_wrap()/cpb_wrap_capped() respect a manual '\\n' instead of collapsing it", {
  # a manual break is kept exactly as given, not reflowed back into
  # one line and rewrapped from scratch (which a bare
  # scales::label_wrap() would do)
  manual <- "Noord\nregio"
  expect_identical(cpb_label_wrap()(manual), manual)
  expect_identical(cpb_wrap_capped(manual, max_lines = 3), manual)

  # a manual break with one line still too wide on its own -> that
  # line wraps further, but "Noord" itself is kept intact as line one
  mixed <- "Noord\nEen erg lange categorienaam die nooit helemaal past"
  out <- cpb_label_wrap()(mixed)
  out_lines <- strsplit(out, "\n", fixed = TRUE)[[1]]
  expect_identical(out_lines[[1]], "Noord")
  expect_gt(length(out_lines), 2)

  # max_lines still caps a manually-broken label with too many lines
  four_lines <- "Regel een\nRegel twee\nRegel drie\nRegel vier"
  capped <- cpb_wrap_capped(four_lines, max_lines = 3)
  expect_length(strsplit(capped, "\n", fixed = TRUE)[[1]], 3)
  expect_match(capped, "\\.\\.\\.$")

  # no manual break at all: unaffected, still auto-wraps as before
  auto <- "Een erg lange categorienaam die nooit helemaal past"
  expect_identical(cpb_label_wrap()(auto), cpb_label_wrap()(auto))
  expect_true(grepl("\n", cpb_label_wrap()(auto), fixed = TRUE))
})

test_that("cpb_dot()'s x-axis tick labels respect a manual '\\n'", {
  manual_cats <- c("Noord\nregio", "Oost\nregio")
  df <- data.frame(cat = factor(manual_cats, levels = manual_cats),
                   y = c(1, 2), lower = c(0, 1), upper = c(2, 3))
  p <- cpb_dot(df, x = cat, y = y, lower = lower, upper = upper,
              orientation = "horizontal")
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_setequal(labs, manual_cats)
})
