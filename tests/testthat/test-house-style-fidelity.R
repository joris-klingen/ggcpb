test_that("the value axis uses Dutch number formatting by default", {
  df <- data.frame(x = c("a", "b"), y = c(-0.5, -2.75))
  sc <- cpb_col(df, x = x, y = y, value_breaks = seq(-3, 0, 0.5))$scales$get_scales("y")
  # published figures read "-0,5", never the ggplot2 default "-0.5"
  expect_equal(sc$get_labels(seq(-3, 0, 0.5)),
               c("-3,0", "-2,5", "-2,0", "-1,5", "-1,0", "-0,5", "0,0"))
})

test_that("pct_axis still wins over the plain number formatter", {
  df <- data.frame(x = c("a", "b"), y = c(10, 60))
  sc <- cpb_col(df, x = x, y = y, pct_axis = TRUE)$scales$get_scales("y")
  expect_match(sc$get_labels(c(10, 60))[[1]], "%")
})

test_that("forecast_x works on a discrete x axis", {
  d <- data.frame(jaar = factor(2023:2027), y = c(-0.3, -0.7, -1.6, -2.8, -2.1))
  # regression: this used to fail with "non-numeric argument to binary
  # operator" when forecast_x named a category rather than a number
  expect_no_error(cpb_col(d, x = jaar, y = y, forecast_x = "2026"))

  p <- cpb_col(d, x = jaar, y = y, forecast_x = "2026")
  b <- ggplot2::ggplot_build(p)
  rect <- b$data[[which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomRect"), logical(1)))[[1]]]]
  # the window opens at the left edge of the 2026 bar, not its centre
  expect_equal(rect$xmin[[1]], 3.5)
})

test_that("forecast_x rejects a category that is not on the axis", {
  d <- data.frame(jaar = factor(2023:2025), y = 1:3)
  expect_error(cpb_col(d, x = jaar, y = y, forecast_x = "2031"),
               "not one of the values on the x axis")
})

test_that("a numeric forecast_x is still passed through untouched", {
  d <- data.frame(jaar = 2023:2027, y = c(-0.3, -0.7, -1.6, -2.8, -2.1))
  p <- cpb_col(d, x = jaar, y = y, forecast_x = 2025.5)
  b <- ggplot2::ggplot_build(p)
  rect <- b$data[[which(vapply(p$layers, function(l)
    inherits(l$geom, "GeomRect"), logical(1)))[[1]]]]
  expect_equal(rect$xmin[[1]], 2025.5)
})

test_that("a default multi-series chart matches the published colours", {
  d <- data.frame(x = rep(1:3, 3), y = 1:9,
                  g = rep(c("a", "b", "c"), each = 3))
  cols <- unique(ggplot2::ggplot_build(
    cpb_line(d, x = x, y = y, colour = g))$data[[1]]$colour)
  expect_setequal(cols, unname(cpb_cols(6, 2, 8)))
})
