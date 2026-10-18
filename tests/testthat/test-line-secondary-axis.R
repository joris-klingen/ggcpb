test_that("cpb_line() draws a secondary value axis", {
  d <- data.frame(jaar = 2020:2027,
                  pct = c(3, 2.2, 3.2, 5.8, 6.7, 5.1, 4.2, 3.8),
                  idx = c(100.5, 100, 96.8, 98.5, 99.2, 100.4, 101.6, 102.5))

  p <- cpb_line(d, x = jaar, y = pct, sec_y = idx, sec_limits = c(95, 110),
                sec_legend_label = "index", ylab = "pct", value_limits = c(-5, 10))
  b <- ggplot2::ggplot_build(p)

  # the right-hand axis reads in the secondary series' own units, not
  # the primary ones it is drawn against
  expect_equal(b$layout$panel_params[[1]]$y.sec$get_labels(),
               c("95", "100", "105", "110"))

  # both series are named in one legend block, each key suffixed with
  # the axis it belongs to (auto, not passed in sec_legend_label)
  keys <- b$plot$guides$get_params("colour")$key$.label
  expect_length(keys, 2)
  expect_true("index (rechteras)" %in% keys)
  expect_true("pct (linkeras)" %in% keys)

  # the secondary line is rescaled onto the primary range: its lowest
  # point (96.8 of 95..110) maps to the matching share of -5..10
  sec_layer <- b$data[[length(b$data)]]
  expect_equal(min(sec_layer$y), -5 + (96.8 - 95) / 15 * 15, tolerance = 1e-6)

  # a colour mapping keys the secondary line into the same scale
  long <- data.frame(jaar = rep(2020:2027, 2),
                     waarde = c(d$pct, d$pct + 1),
                     reeks = rep(c("a", "b"), each = 8))
  long$idx <- rep(d$idx, 2)
  p2 <- cpb_line(long, x = jaar, y = waarde, colour = reeks, sec_y = idx,
                 sec_legend_label = "c")
  keys2 <- ggplot2::ggplot_build(p2)$plot$guides$get_params("colour")$key$.label
  expect_length(keys2, 3)
  expect_true("c (rechteras)" %in% keys2)
  expect_true(all(c("a (linkeras)", "b (linkeras)") %in% keys2))

  expect_error(cpb_line(d, x = jaar, y = pct, sec_y = idx, sec_limits = c(1, 1)),
               "non-zero range")
})

test_that("cpb_line() without sec_y is unchanged", {
  d <- data.frame(x = 1:8, y = runif(8))
  b <- ggplot2::ggplot_build(cpb_line(d, x = x, y = y, value_limits = c(-5, 10)))
  # with no sec_y the right-hand axis just mirrors the left one
  expect_equal(b$layout$panel_params[[1]]$y.sec$get_labels(),
               b$layout$panel_params[[1]]$y$get_labels())
  expect_length(b$plot$guides$guides, 0)
})

test_that("explicit sec_limits equal to the data range are kept, not rounded out", {
  s <- c(15, 17, 22, 19, 25)
  prim <- c(0, 2, 4, 6, 8)
  expect_equal(cpb_find_sec_breaks(prim, s, sec_limits = c(15, 25)),
               c(15, 17.5, 20, 22.5, 25))
  expect_equal(cpb_find_sec_breaks(prim, s), c(14, 18, 22, 26, 30))
})

test_that("a constant sec_y says so, and sec_limits or sec_at give it an axis", {
  d <- data.frame(x = 1:3, y = c(1, 3, 2), s = c(5, 5, 5))
  expect_error(cpb_line(d, x = x, y = y, sec_y = s),
               "same value everywhere", fixed = TRUE)
  expect_no_error(cpb_line(d, x = x, y = y, sec_y = s, sec_limits = c(0, 10)))
  expect_error(cpb_find_sec_breaks(0:4, 1:3, sec_limits = c(5, 5)),
               "`sec_limits` must be", fixed = TRUE)
  expect_error(cpb_find_sec_breaks(0:4, c(NA_real_, NA_real_)),
               "no values to draw", fixed = TRUE)
})

test_that("sec_axis_labels sets the labels at the ticks of the right axis", {
  d <- data.frame(x = 2020:2024, y = c(2, 6, 4, 8, 5), s = c(15, 17, 22, 19, 25))
  p <- cpb_line(d, x = x, y = y, sec_y = s, value_breaks = c(0, 2, 4, 6, 8),
                sec_at = c(10, 15, 20, 25, 30),
                sec_axis_labels = c("a", "b", "c", "d", "e"))
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$layout$panel_params[[1]]$y.sec$get_labels(), c("a", "b", "c", "d", "e"))
  expect_false("sec_labels" %in% names(formals(cpb_line)))
})

test_that("a secondary series past sec_limits widens the axis instead of being cut off", {
  d <- data.frame(x = 2019:2025, y = c(2, 6, 4, 8, 5, 3, 7), s = c(96, 99, 104, 101, 108, 97, 103))
  on_panel <- function(p) {
    pp <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
    k <- which(vapply(p$layers, function(l) "cpb__sec" %in% names(l$data), TRUE))[1]
    pos <- p$layers[[k]]$data$cpb__sec
    all(pos >= pp$y.range[1] - 1e-9 & pos <= pp$y.range[2] + 1e-9)
  }

  expect_message(p <- cpb_line(d, x = x, y = y, sec_y = s, sec_limits = c(98, 105)),
                 "runs past `sec_limits`, so the right axis is widened to 96 to 108")
  expect_true(on_panel(p))
  expect_message(p <- cpb_col(d, x = x, y = y, sec_y = s, sec_limits = c(100, 120)), "widened")
  expect_true(on_panel(p))
  # limits the data fits inside stay as given, without a message
  expect_no_message(p <- cpb_line(d, x = x, y = y, sec_y = s, sec_limits = c(95, 110)))
  labs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.sec$get_labels()
  expect_equal(c(labs[1], labs[length(labs)]), c("95,0", "110,0"))
})
