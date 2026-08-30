# test-sec-y-legend.R ----
#
# A bare show.legend = TRUE on a layer draws that layer's own key
# glyph into *every* active guide in the plot, not just the aesthetic
# it actually maps -- so cpb_col()/cpb_area()/cpb_box()'s primary fill
# layer, forced on to guarantee a key for a drop = FALSE empty level,
# used to also draw its filled rect glyph in behind sec_y's own
# colour guide, and cpb_dot()'s primary point/errorbar layer (which
# maps neither fill nor colour at all when sec_y replaces colour) used
# to draw its own point/errorbar glyph in there too. Fixed by naming
# show.legend per aesthetic (colour = FALSE) instead of a bare TRUE.

test_that("cpb_col()'s primary fill layer does not bleed into sec_y's colour guide", {
  d <- data.frame(x = 1:3, g = c("a", "b", "a"), y = 1:3, z = c(5, 6, 7))
  p <- cpb_col(d, x = x, y = y, fill = g, sec_y = z, sec_type = "line")
  expect_equal(p$layers[[1]]$show.legend, c(fill = TRUE, colour = FALSE))
})

test_that("cpb_area()'s primary fill layer does not bleed into sec_y's colour guide", {
  d <- data.frame(x = 1:3, g = c("a", "b", "a"), y = 1:3, z = c(5, 6, 7))
  p <- cpb_area(d, x = x, y = y, fill = g, sec_y = z, sec_type = "line")
  expect_equal(p$layers[[1]]$show.legend, c(fill = TRUE, colour = FALSE))
})

test_that("cpb_box()'s primary fill layer does not bleed into sec_y's colour guide", {
  d <- data.frame(
    x = c("A", "B"), p5 = 1:2, p25 = 2:3, p50 = 3:4, p75 = 4:5, p95 = 5:6,
    fill = c("a", "b"), z = c(5, 6)
  )
  p <- cpb_box(d, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               fill = fill, box_style = "ggcpb", sec_y = z, sec_type = "line")
  box_layer <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomBoxplot"), TRUE))]]
  expect_equal(box_layer$show.legend, c(fill = TRUE, colour = FALSE))
})

test_that("cpb_dot()'s primary point layer does not bleed into sec_y's colour guide", {
  d <- data.frame(x = c("a", "b"), y = c(1, 2), lo = c(0, 1), hi = c(2, 3), z = c(5, 6))
  p <- cpb_dot(d, x = x, y = y, lower = lo, upper = hi, orientation = "vertical",
               sec_y = z, sec_type = "line")
  point_layer <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), TRUE))]]
  expect_false(isTRUE(point_layer$show.legend))
})

test_that("cpb_dot()'s primary point layer still gets its own key when colour is mapped", {
  d <- data.frame(x = c("a", "b"), y = c(1, 2), lo = c(0, 1), hi = c(2, 3),
                   g = c("p", "q"))
  p <- cpb_dot(d, x = x, y = y, lower = lo, upper = hi, colour = g)
  point_layer <- p$layers[[which(vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), TRUE))]]
  expect_true(isTRUE(point_layer$show.legend))
})

# cpb_add_sec_ylab_grob() (see save.R) places the right-hand unit
# caption on the plot's own subtitle row, so save_cpb() needs that row
# reserved -- previously only guaranteed when a title or an explicit
# ylab/subtitle was also given; without either, the row collapsed to
# nothing and the caption ended up squeezed into the axis tick row
# below it, overlapping the axis's own top value.
test_that("a sec_ylab reserves the subtitle row even with no title and no ylab", {
  d <- data.frame(x = 1:3, y = 1:3, z = c(5, 6, 7))

  p <- cpb_line(d, x = x, y = y, sec_y = z, sec_ylab = "%")
  expect_equal(p$labels$subtitle, " ")

  p <- cpb_col(d, x = x, y = y, sec_y = z, sec_ylab = "%")
  expect_equal(p$labels$subtitle, " ")

  p <- cpb_area(d, x = x, y = y, fill = factor("a"), sec_y = z, sec_ylab = "%")
  expect_equal(p$labels$subtitle, " ")

  dd <- data.frame(x = c("A", "B"), p5 = 1:2, p25 = 2:3, p50 = 3:4,
                    p75 = 4:5, p95 = 5:6, z = c(5, 6))
  p <- cpb_box(dd, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
               sec_y = z, sec_ylab = "%")
  expect_equal(p$labels$subtitle, " ")

  dd2 <- data.frame(x = c("a", "b"), y = c(1, 2), lo = c(0, 1), hi = c(2, 3), z = c(5, 6))
  p <- cpb_dot(dd2, x = x, y = y, lower = lo, upper = hi, orientation = "vertical",
               sec_y = z, sec_ylab = "%")
  expect_equal(p$labels$subtitle, " ")
})

test_that("no sec_ylab still leaves the subtitle row alone with no title and no ylab", {
  d <- data.frame(x = 1:3, y = 1:3)
  p <- cpb_line(d, x = x, y = y)
  expect_null(p$labels$subtitle)
})
