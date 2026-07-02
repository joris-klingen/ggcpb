# test-theme.R ----

test_that("theme_cpb sets the fixed 9/7/6 pt text sizes and faces", {
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

  expect_equal(th$legend.text$size, 7)
  expect_equal(th$strip.text$size, 7)
  expect_equal(th$strip.text$face, "bold")

  expect_equal(th$axis.text$size, 6)
  expect_equal(th$axis.text$colour, "black")
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

test_that("theme_cpb draws gridlines on the value axis implied by orientation", {
  th_v <- theme_cpb(orientation = "vertical")
  expect_s3_class(th_v$panel.grid.major.y, "element_line")
  expect_s3_class(th_v$panel.grid.minor.y, "element_line")
  expect_s3_class(th_v$panel.grid.major.x, "element_blank")
  expect_equal(th_v$panel.grid.major.y$colour, cpb_tokens()$grid)

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

test_that("theme_cpb_min has no background and no gridlines", {
  th <- theme_cpb_min()
  expect_s3_class(th$plot.background, "element_blank")
  expect_s3_class(th$panel.grid.major.x, "element_blank")
  expect_s3_class(th$panel.grid.major.y, "element_blank")
})
