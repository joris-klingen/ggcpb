test_that("cpb_line() draws a secondary value axis", {
  d <- data.frame(jaar = 2020:2027,
                  pct = c(3, 2.2, 3.2, 5.8, 6.7, 5.1, 4.2, 3.8),
                  idx = c(100.5, 100, 96.8, 98.5, 99.2, 100.4, 101.6, 102.5))

  p <- cpb_line(d, x = jaar, y = pct, sec_y = idx, sec_limits = c(95, 110),
                sec_label = "index (rechteras)", value_limits = c(-5, 10))
  b <- ggplot2::ggplot_build(p)

  # the right-hand axis reads in the secondary series' own units, not
  # the primary ones it is drawn against
  expect_equal(b$layout$panel_params[[1]]$y.sec$get_labels(),
               c("95", "100", "105", "110"))

  # both series are named in one legend block, primary first
  keys <- b$plot$guides$get_params("colour")$key$.label
  expect_length(keys, 2)
  expect_true("index (rechteras)" %in% keys)

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
                 sec_label = "c (rechteras)")
  expect_length(ggplot2::ggplot_build(p2)$plot$guides$get_params("colour")$key$.label, 3)

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
