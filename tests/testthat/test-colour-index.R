cols_of <- function(p) unique(ggplot2::ggplot_build(p)$data[[1]]$colour)
d3 <- data.frame(x = rep(1:3, 3), y = 1:9, g = rep(c("a", "b", "c"), each = 3))

test_that("colour_index takes palette positions", {
  expect_equal(cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                colour_index = c(2, 5, 6))),
               unname(cpb_cols(2, 5, 6)))
})

test_that("colour_index keywords choose a palette", {
  # "discrete" is the house qualitative palette: blue, magenta, taupe
  expect_equal(cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                colour_index = "discrete")),
               unname(cpb_cols(6, 2, 8)))
  # and matches the default, which defers to `palette`
  expect_equal(cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                colour_index = "discrete")),
               cols_of(cpb_line(d3, x = x, y = y, colour = g)))
  # "continuous" is the sequential ramp
  expect_equal(cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                colour_index = "continuous")),
               cpb_pal("sequential")(3))
})

test_that("color_index is accepted as an alias", {
  expect_equal(cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                color_index = c(2, 5, 6))),
               cols_of(cpb_line(d3, x = x, y = y, colour = g,
                                colour_index = c(2, 5, 6))))
})

test_that("fill_index works on the fill-mapped wrappers", {
  d <- data.frame(x = c("a", "b", "c"), y = 1:3, f = c("p", "q", "r"))
  fills <- unique(ggplot2::ggplot_build(
    cpb_col(d, x = x, y = y, fill = f, fill_index = c(2, 5, 6)))$data[[1]]$fill)
  expect_equal(fills, unname(cpb_cols(2, 5, 6)))
})

test_that("a keyword that contradicts `palette` is an error", {
  expect_error(
    cpb_line(d3, x = x, y = y, colour = g,
             colour_index = "continuous", palette = "qualitative"),
    "both set the palette")
  # agreeing is fine
  expect_no_error(cpb_line(d3, x = x, y = y, colour = g,
                           colour_index = "discrete", palette = "qualitative"))
})

test_that("bad colour_index values are rejected clearly", {
  expect_error(cpb_line(d3, x = x, y = y, colour = g, colour_index = "rainbow"),
               "not recognised")
  expect_error(cpb_line(d3, x = x, y = y, colour = g, colour_index = c(0, 2)),
               "positive whole numbers")
  expect_error(cpb_line(d3, x = x, y = y, colour = g, colour_index = c(1.5)),
               "positive whole numbers")
})

test_that("cpb_donut takes fill_index like the other fill charts", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 2, 1))
  fills <- function(...) ggplot2::ggplot_build(cpb_donut(d, fill = g, y = v, ...))$data[[1]]$fill
  expect_equal(fills(fill_index = c(2, 5, 6)), unname(cpb_cols(2, 5, 6)))
  expect_equal(fills(fill_index = "discrete"), fills())
  expect_error(cpb_donut(d, fill = g, y = v, fill_index = "rainbow"), "not recognised")
})

test_that("the wrappers no longer have an index argument", {
  fns <- list(cpb_col, cpb_area, cpb_line, cpb_box, cpb_boxplot_extended,
              cpb_scatter, cpb_hist, cpb_dot, cpb_donut, cpb_map)
  expect_false(any(vapply(fns, function(f) "index" %in% names(formals(f)), TRUE)))
})

test_that("cpb_map keeps its sequential default", {
  # a choropleth defaults to the ramp, not the discrete palette, so the
  # new argument must not force "discrete" on every wrapper
  expect_equal(eval(formals(cpb_map)$palette), "sequential")
  expect_null(eval(formals(cpb_map)$fill_index))
})
