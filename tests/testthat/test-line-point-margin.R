test_that("cpb_line(points = TRUE) leaves room for the markers", {
  d <- data.frame(x = 1:10, y = c(1.2, 2.4, 1.8, 3.1, 2.2, -0.6, 3.4, 2.9, 1.1, 2.6))

  rng <- function(p) {
    pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
    list(x = pp$x.range, y = pp$y.range)
  }

  # a plain line keeps the tight panel on x (the axis meets the data
  # exactly); y is flush to pretty() breaks instead of the raw data,
  # so its range can run a little past the data on either side
  bare <- rng(cpb_line(d, x = x, y = y))
  expect_equal(bare$x, c(1, 10))
  expect_equal(bare$y, range(pretty(range(d$y))))

  # markers have a radius, so a marker exactly on the panel edge would
  # be cut in half if clipping stayed on -- the panel itself stays
  # pinned to the data range either way (a flush axis with points
  # still reads flush), but clip switches off so that edge marker
  # still draws in full, just past the strict panel boundary
  pts <- rng(cpb_line(d, x = x, y = y, points = TRUE))
  expect_equal(pts$x, c(1, 10))
  built_pts <- ggplot2::ggplot_build(cpb_line(d, x = x, y = y, points = TRUE))
  expect_equal(built_pts$layout$coord$clip, "off")
})

test_that("the point margin (via clip) survives a user-supplied x scale", {
  d <- data.frame(x = 1:10, y = 1:10)
  p <- cpb_line(d, x = x, y = y, points = TRUE) +
    ggplot2::scale_x_continuous(breaks = c(2, 6, 10))
  b <- ggplot2::ggplot_build(p)
  # a follow-up scale_x_continuous() replaces the scale, but clip lives
  # on the coord, a separate plot component, so it survives untouched
  expect_equal(b$layout$coord$clip, "off")
  xr <- b$layout$panel_params[[1]]$x.range
  expect_equal(xr, c(1, 10))
})

test_that("points do not stop value_limits from cropping", {
  d <- data.frame(x = 1:10, y = c(1.2, 2.4, 1.8, 3.1, 2.2, -0.6, 3.4, 2.9, 1.1, 2.6))
  p <- cpb_line(d, x = x, y = y, points = TRUE, value_limits = c(0, 3))
  b <- ggplot2::ggplot_build(p)
  # clip defaults to "off" now (see cpb_line's x_lim_follow_data docs),
  # but that doesn't stop value_limits from cropping: value_limits sets
  # the value scale's own `limits`, so the out-of-range observations
  # become NA (with a warning) and are silently skipped when drawn, the
  # same as setting `limits` on any ggplot2 scale -- clipping was never
  # what did the cropping here
  expect_equal(b$layout$coord$clip, "off")
  line_y <- b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), TRUE))]]$y
  expect_true(anyNA(line_y))
  expect_equal(sum(is.na(line_y)), sum(d$y < 0 | d$y > 3))
  yr <- b$layout$panel_params[[1]]$y.range
  expect_equal(yr, c(0, 3))
})
