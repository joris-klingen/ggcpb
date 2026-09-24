# test-theme.R ----

test_that("theme_cpb sets the fixed house text sizes and faces", {
  th <- theme_cpb()
  expect_s3_class(th, "theme")

  expect_equal(th$plot.title$size, 9)
  expect_equal(th$plot.title$face, "bold")
  expect_equal(th$plot.title$hjust, 0)

  expect_equal(th$plot.subtitle$size, 7)
  expect_equal(th$plot.subtitle$face, "italic")

  expect_equal(th$axis.title$size, 7)
  expect_equal(th$axis.title$face, "italic")
  expect_equal(th$axis.title$hjust, 1)

  expect_equal(th$axis.text$size, 7)
  expect_equal(th$legend.title$size, 7)
  expect_equal(th$legend.text$size, 7)
  expect_equal(th$strip.text$size, 7)
})

test_that("theme_cpb fills the plot background with the CPB colour by default", {
  th <- theme_cpb()
  expect_s3_class(th$plot.background, "element_rect")
  expect_equal(th$plot.background$fill, cpb_tokens()$bg)
})

test_that("theme_cpb(background = FALSE) blanks the plot background", {
  th <- theme_cpb(background = FALSE)
  expect_s3_class(th$plot.background, "element_blank")
})

test_that("theme_cpb draws hairline black gridlines on the value axis only", {
  th_v <- theme_cpb(orientation = "vertical")
  expect_s3_class(th_v$panel.grid.major.y, "element_line")
  expect_equal(th_v$panel.grid.major.y$colour, "black")
  expect_equal(th_v$panel.grid.major.y$linewidth, 0.1)
  expect_s3_class(th_v$panel.grid.minor.y, "element_blank")
  expect_s3_class(th_v$panel.grid.major.x, "element_blank")

  th_h <- theme_cpb(orientation = "horizontal")
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

test_that("theme_cpb(minor = TRUE) draws minor gridlines", {
  th <- theme_cpb(orientation = "vertical", minor = TRUE)
  expect_s3_class(th$panel.grid.minor.y, "element_line")
})

test_that("theme_cpb grid_colour/grid_linewidth style the gridlines", {
  th <- theme_cpb(grid_colour = cpb_tokens()$grid, grid_linewidth = NULL)
  expect_equal(th$panel.grid.major.y$colour, cpb_tokens()$grid)
  expect_null(th$panel.grid.major.y$linewidth)
})

test_that("theme_cpb draws ticks and an axis line on the category axis", {
  th_v <- theme_cpb(orientation = "vertical")
  expect_s3_class(th_v$axis.ticks.x, "element_line")
  expect_equal(th_v$axis.ticks.x$colour, "black")
  expect_s3_class(th_v$axis.line.x, "element_line")

  th_h <- theme_cpb(orientation = "horizontal")
  expect_s3_class(th_h$axis.ticks.y, "element_line")
  expect_s3_class(th_h$axis.line.y, "element_line")

  th_off <- theme_cpb(ticks = FALSE)
  expect_null(th_off$axis.ticks.x)
  expect_null(th_off$axis.line.x)
})

test_that("theme_cpb anchors the legend flush left at the bottom", {
  th <- theme_cpb()
  expect_equal(th$legend.position, "bottom")
  expect_equal(th$legend.justification, "left")
  expect_equal(th$legend.direction, "vertical")
  expect_equal(th$legend.location, "plot")

  th_off <- theme_cpb(legend = "right", flush_legend = FALSE)
  expect_equal(th_off$legend.position, "right")
  expect_null(th_off$legend.location)
})

test_that("theme_cpb axis_text_size and legend_key_size are applied", {
  th <- theme_cpb(axis_text_size = 6, legend_key_size = 0.45)
  expect_equal(th$axis.text$size, 6)
  expect_equal(as.numeric(th$legend.key.height), 0.45)
  expect_equal(as.numeric(th$legend.key.width), 0.45)

  # default keeps the house 0.25 x 0.30 cm keys
  th_def <- theme_cpb()
  expect_equal(as.numeric(th_def$legend.key.height), 0.25)
  expect_equal(as.numeric(th_def$legend.key.width), 0.30)
})

test_that("theme_cpb uses the tight house margins", {
  th <- theme_cpb()
  expect_equal(as.numeric(th$plot.margin), c(10, 10, 8, 10))
})

test_that("theme_cpb pins the axis spacing that ggplot2 4.0 changed", {
  # theme_minimal() gives 2.2 pt in ggplot2 3.5 and 4.95 pt in 4.x; the
  # house theme spells it out so figures match on either version
  th <- ggplot2::theme_grey() + theme_cpb()
  mar <- function(el) as.numeric(ggplot2::calc_element(el, th)$margin)
  expect_equal(mar("axis.text.x.bottom"), c(4.95, 0, 0, 0))
  expect_equal(mar("axis.text.x.top"), c(0, 0, 4.95, 0))
  expect_equal(mar("axis.text.y.left"), c(0, 4.95, 0, 0))
  expect_equal(mar("axis.text.y.right"), c(0, 0, 0, 4.95))
  # 3.5 reserves the tick length even on an axis whose ticks are blank
  expect_equal(as.numeric(ggplot2::calc_element("axis.ticks.length.y.left", th)), 0)
  expect_equal(as.numeric(ggplot2::calc_element("axis.ticks.length.x.bottom", th)), 2.2)
})

test_that("the pinned axis text still disappears when a plot blanks it", {
  th <- ggplot2::theme_grey() + theme_cpb() + ggplot2::theme(axis.text = ggplot2::element_blank())
  expect_s3_class(ggplot2::calc_element("axis.text.y.left", th), "element_blank")
})

test_that("cpb_font_family falls back on devices without TTF lookup", {
  skip_if_not_installed("withr")
  # the bundled font is registered in this session
  expect_equal(cpb_font_family(), "RijksoverheidSansText")

  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  withr::defer(grDevices::dev.off())
  # pdf() cannot draw systemfonts-registered families: fall back
  expect_equal(cpb_font_family(), "")
  # a plot built with the default theme must render, not error
  df <- data.frame(x = c("a", "b"), y = 1:2)
  expect_no_error(print(cpb_col(df, x = x, y = y, title = "t", ylab = "u")))
  # escape hatch for showtext-style setups that draw text themselves
  withr::local_options(ggcpb.force_font_family = TRUE)
  expect_equal(cpb_font_family(), "RijksoverheidSansText")
})
