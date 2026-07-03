# test-theme.R ----

test_that("theme_cpb sets the fixed 9/7/6 pt text sizes and faces", {
  th <- theme_cpb(style = "ggplot")
  expect_s3_class(th, "theme")

  expect_equal(th$plot.title$size, 9)
  expect_equal(th$plot.title$face, "bold")
  expect_equal(th$plot.title$hjust, 0)

  expect_equal(th$plot.subtitle$size, 7)
  expect_equal(th$plot.subtitle$face, "italic")

  expect_equal(th$axis.title$size, 7)
  expect_equal(th$axis.title$face, "italic")
  expect_equal(th$axis.title$hjust, 1)

  expect_equal(th$legend.text$size, 7)
  expect_equal(th$strip.text$size, 7)
  expect_equal(th$strip.text$face, "bold")

  expect_equal(th$axis.text$size, 6)
  expect_equal(th$axis.text$colour, "black")
})

test_that("theme_cpb fills the plot background with the CPB colour by default", {
  th <- theme_cpb(style = "ggplot")
  expect_s3_class(th$plot.background, "element_rect")
  expect_equal(th$plot.background$fill, cpb_tokens()$bg)
})

test_that("theme_cpb(background = FALSE) blanks the plot background", {
  th <- theme_cpb(background = FALSE)
  expect_s3_class(th$plot.background, "element_blank")
})

test_that("theme_cpb draws gridlines on the value axis implied by orientation", {
  th_v <- theme_cpb(orientation = "vertical", style = "ggplot")
  expect_s3_class(th_v$panel.grid.major.y, "element_line")
  expect_s3_class(th_v$panel.grid.minor.y, "element_line")
  expect_s3_class(th_v$panel.grid.major.x, "element_blank")
  expect_equal(th_v$panel.grid.major.y$colour, cpb_tokens()$grid)

  th_h <- theme_cpb(orientation = "horizontal", style = "ggplot")
  expect_s3_class(th_h$panel.grid.major.x, "element_line")
  expect_s3_class(th_h$panel.grid.major.y, "element_blank")
})

test_that("theme_cpb grid argument overrides the orientation default", {
  th_both <- theme_cpb(orientation = "vertical", grid = "both")
  expect_s3_class(th_both$panel.grid.major.x, "element_line")
  expect_s3_class(th_both$panel.grid.major.y, "element_line")

  th_none <- theme_cpb(grid = "none")
  expect_s3_class(th_none$panel.grid.major.x, "element_blank")
  expect_s3_class(th_none$panel.grid.major.y, "element_blank")

  th_x <- theme_cpb(orientation = "vertical", grid = "x")
  expect_s3_class(th_x$panel.grid.major.x, "element_line")
  expect_s3_class(th_x$panel.grid.major.y, "element_blank")
})

test_that("theme_cpb_min has no background and no gridlines", {
  th <- theme_cpb_min()
  expect_s3_class(th$plot.background, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")
  expect_s3_class(th$panel.grid.major.y, "element_blank")
})

test_that("theme_cpb(minor = FALSE) blanks the minor gridlines only", {
  th <- theme_cpb(orientation = "vertical", style = "ggplot", minor = FALSE)
  expect_s3_class(th$panel.grid.major.y, "element_line")
  expect_s3_class(th$panel.grid.minor.y, "element_blank")
})

test_that("theme_cpb grid_colour/grid_linewidth style the gridlines", {
  th <- theme_cpb(grid_colour = "black", grid_linewidth = 0.1)
  expect_equal(th$panel.grid.major.y$colour, "black")
  expect_equal(th$panel.grid.major.y$linewidth, 0.1)
})

test_that("theme_cpb(ticks = TRUE) draws ticks on the category axis", {
  th_v <- theme_cpb(orientation = "vertical", ticks = TRUE)
  expect_s3_class(th_v$axis.ticks.x, "element_line")
  expect_equal(th_v$axis.ticks.x$colour, "black")

  th_h <- theme_cpb(orientation = "horizontal", ticks = TRUE)
  expect_s3_class(th_h$axis.ticks.y, "element_line")
})

test_that("theme_cpb(flush_legend = TRUE) anchors the legend flush left", {
  th <- theme_cpb(legend = "bottom", flush_legend = TRUE)
  expect_equal(th$legend.justification, "left")
  expect_equal(th$legend.direction, "vertical")
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    expect_equal(th$legend.location, "plot")
  }
})

test_that("theme_cpb axis_text_size and legend_key_size are applied", {
  th <- theme_cpb(axis_text_size = 7, legend_key_size = 0.45)
  expect_equal(th$axis.text$size, 7)
  expect_equal(as.numeric(th$legend.key.height), 0.45)
  expect_equal(as.numeric(th$legend.key.width), 0.45)
})

test_that("the cpb_default preset (the default style) sets the house knobs", {
  th <- theme_cpb()
  expect_s3_class(th$panel.grid.minor.y, "element_blank")
  expect_equal(th$panel.grid.major.y$colour, "black")
  expect_equal(th$panel.grid.major.y$linewidth, 0.1)
  expect_s3_class(th$axis.ticks.x, "element_line")
  expect_equal(th$axis.text$size, 7)
  # key size is NOT changed by the preset: published nplot figures use
  # the same 0.25 x 0.30 cm keys as the classic style
  expect_equal(as.numeric(th$legend.key.height), 0.25)
  expect_s3_class(th$axis.line.x, "element_line")
  expect_equal(as.numeric(th$plot.margin)[3], 8)
  expect_equal(th$legend.position, "bottom")
  expect_equal(th$legend.justification, "left")

  # explicit knobs override the preset
  th2 <- theme_cpb(minor = TRUE, axis_text_size = 6)
  expect_s3_class(th2$panel.grid.minor.y, "element_line")
  expect_equal(th2$axis.text$size, 6)
})
