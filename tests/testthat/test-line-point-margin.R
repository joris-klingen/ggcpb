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

test_that("value_limits narrower than the data widens the axis, it does not crop", {
  d <- data.frame(x = 1:10, y = c(1.2, 2.4, 1.8, 3.1, 2.2, -0.6, 3.4, 2.9, 1.1, 2.6))
  # value_limits asks for the span the axis must *at least* cover. The
  # data runs past it at both ends, so the breaks are extended outward
  # in their own step rather than the ends being hidden -- nothing is
  # cropped, so clip stays "off" and the markers sitting on the panel
  # edge still draw whole.
  p <- expect_no_warning(
    cpb_line(d, x = x, y = y, points = TRUE, value_limits = c(0, 3))
  )
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$coord$clip, "off")

  line_y <- b$data[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), TRUE))]]$y
  expect_false(anyNA(line_y))
  expect_equal(line_y, d$y)

  yr <- b$layout$panel_params[[1]]$y.range
  expect_lte(yr[1], min(d$y))
  expect_gte(yr[2], max(d$y))
})

test_that("a stacked total a hair over the limit is drawn, not censored", {
  # The axis is sized from tapply(..., sum) -- a forward sum -- while
  # position_stack() reaches its top by cumsum()ing the same numbers in
  # reverse. Floating point is not associative, so the two can differ in
  # the last bit: here the forward sum is exactly 100 while the stack
  # tops out at 100 + 1e-14. The scale's limits are flush (no expansion)
  # and oob_censor() compares strictly, so without a tolerance that top
  # point becomes NA -- geom_area() then fails to build a grob at all,
  # and geom_col() silently drops the segment.
  raw <- c(3.03, 6.10, 8.66)
  shares <- 100 * raw / sum(raw)
  expect_equal(sum(shares), 100, tolerance = 0) # what the axis is sized from
  expect_gt(cumsum(rev(shares))[3], 100) # what position_stack() reaches

  d <- data.frame(
    jaar  = rep(2020:2021, each = 3),
    grp   = factor(rep(c("a", "b", "c"), times = 2)),
    share = rep(shares, times = 2)
  )

  has_na <- function(p) {
    any(vapply(
      ggplot2::ggplot_build(p)$data,
      function(l) if ("y" %in% names(l)) anyNA(l$y) else FALSE,
      logical(1)
    ))
  }

  col <- cpb_col(d, x = jaar, y = share, fill = grp, pct_axis = TRUE)
  expect_false(has_na(col))

  area <- cpb_area(d, x = jaar, y = share, fill = grp, pct_axis = TRUE)
  expect_false(has_na(area))
  # the area has to survive all the way to a grob, not just to build
  expect_no_error(ggplot2::ggplotGrob(area))

  # the tolerance is far below a screen pixel, so the axis still reads
  # as flush: the limits are still 0-100 to any visible precision
  lims <- ggplot2::ggplot_build(col)$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(lims, c(0, 100))
})
