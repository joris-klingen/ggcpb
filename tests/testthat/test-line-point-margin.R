test_that("cpb_line(points = TRUE) leaves room for the markers", {
  d <- data.frame(x = 1:10, y = c(1.2, 2.4, 1.8, 3.1, 2.2, -0.6, 3.4, 2.9, 1.1, 2.6))

  rng <- function(p) {
    pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
    list(x = pp$x.range, y = pp$y.range)
  }

  # a plain line keeps the tight panel: the axis meets the data exactly
  bare <- rng(cpb_line(d, x = x, y = y))
  expect_equal(bare$x, c(1, 10))
  expect_equal(bare$y, range(d$y))

  # markers have a radius, so the panel has to grow past the data or the
  # first, last and extreme points are cut in half by the panel edge
  pts <- rng(cpb_line(d, x = x, y = y, points = TRUE))
  expect_lt(pts$x[[1]], 1)
  expect_gt(pts$x[[2]], 10)
  expect_lt(pts$y[[1]], min(d$y))
  expect_gt(pts$y[[2]], max(d$y))
})

test_that("the point margin survives a user-supplied x scale", {
  d <- data.frame(x = 1:10, y = 1:10)
  p <- cpb_line(d, x = x, y = y, points = TRUE) +
    ggplot2::scale_x_continuous(breaks = c(2, 6, 10))
  xr <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  expect_lt(xr[[1]], 1)
  expect_gt(xr[[2]], 10)
})

test_that("points do not stop value_limits from cropping", {
  d <- data.frame(x = 1:10, y = c(1.2, 2.4, 1.8, 3.1, 2.2, -0.6, 3.4, 2.9, 1.1, 2.6))
  p <- cpb_line(d, x = x, y = y, points = TRUE, value_limits = c(0, 3))
  b <- ggplot2::ggplot_build(p)
  # clipping stays on, so out-of-range data is cut at the panel edge
  # rather than spilling over the axis labels
  expect_equal(b$layout$coord$clip, "on")
  # the panel tracks the limits, not the data, so the -0.6 and 3.4
  # observations fall outside it and get cut
  yr <- b$layout$panel_params[[1]]$y.range
  expect_gt(yr[[1]], min(d$y))
  expect_lt(yr[[2]], max(d$y))
  # and it stays close to the requested limits: only the marker margin
  expect_equal(yr, c(0, 3), tolerance = 0.1)
})
