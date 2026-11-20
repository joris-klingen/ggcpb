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

test_that("colour_index works on the fill-mapped wrappers", {
  d <- data.frame(x = c("a", "b", "c"), y = 1:3, f = c("p", "q", "r"))
  p <- cpb_col(d, x = x, y = y, fill = f, colour_index = c(2, 5, 6))
  fills <- unique(ggplot2::ggplot_build(p)$data[[1]]$fill)
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
  expect_error(cpb_line(d3, x = x, y = y, colour = g,
                        colour_index = c("siren", "rainbow")),
               "\"rainbow\" is not recognised")
  expect_error(cpb_line(d3, x = x, y = y, colour = g, colour_index = c(0, 2)),
               "positive whole numbers")
  expect_error(cpb_line(d3, x = x, y = y, colour = g, colour_index = c(1.5)),
               "positive whole numbers")
})

test_that("cpb_donut takes colour_index like the other fill charts", {
  d <- data.frame(g = c("a", "b", "c"), v = c(3, 2, 1))
  fills <- function(...) {
    ggplot2::ggplot_build(cpb_donut(d, fill = g, y = v,
                                    ...))$data[[1]]$fill
  }
  expect_equal(fills(colour_index = c(2, 5, 6)), unname(cpb_cols(2, 5, 6)))
  expect_equal(fills(colour_index = "discrete"), fills())
  expect_error(cpb_donut(d, fill = g, y = v, colour_index = "rainbow"),
               "not recognised")
})

test_that("the wrappers have no index or color_index argument", {
  fns <- list(cpb_col, cpb_area, cpb_line, cpb_box, cpb_boxplot_extended,
              cpb_scatter, cpb_hist, cpb_dot, cpb_donut, cpb_map)
  has <- function(name) {
    vapply(fns,
           function(f) name %in% names(formals(f)), TRUE)
  }
  expect_false(any(has("index")))
  expect_false(any(has("color_index")))
})

test_that("cpb_map keeps its sequential default", {
  # a choropleth defaults to the ramp, not the discrete palette, so the
  # new argument must not force "discrete" on every wrapper
  expect_equal(eval(formals(cpb_map)$palette), "sequential")
  expect_null(eval(formals(cpb_map)$colour_index))
})

test_that("colour_index takes colour names, as cpb_cols() does", {
  fills <- function(...) {
    unique(ggplot2::ggplot_build(
      cpb_line(d3, x = x, y = y, colour = g, ...)
    )$data[[1]]$colour)
  }
  expect_equal(fills(colour_index = c("siren", "brown", "endeavour")),
               unname(cpb_cols(3, 8, 6)))
  expect_equal(fills(colour_index = c("siren", "brown", "endeavour")),
               fills(colour_index = c(3, 8, 6)))
  # one name is a colour, not a keyword
  d1 <- d3[d3$g == d3$g[1], ]
  expect_equal(
    unique(ggplot2::ggplot_build(
      cpb_line(d1, x = x, y = y, colour_index = "endeavour")
    )$data[[1]]$colour),
    unname(cpb_cols(6))
  )
  # the other eleven colours need the extended palette
  expect_error(
    cpb_line(d3, x = x, y = y, colour = g,
             colour_index = c("siren", "apple", "brown")),
    "need palette = \"extended\""
  )
})

test_that("names with a palette that has none say so", {
  md <- data.frame(prov = c("Groningen", "Fryslân", "Drenthe"),
                   v = c("a", "b", "c"))
  # the default palette of cpb_map() is a ramp without colour names
  expect_error(
    cpb_map(md, region = prov, value = v, level = "provincie",
            colour_index = c("siren", "brown")),
    "palette 'sequential' has no colour names"
  )
  expect_error(
    cpb_line(d3, x = x, y = y, colour = g, palette = "blues",
             colour_index = c("siren", "brown", "dorado")),
    "palette = \"qualitative\" for colour names"
  )
  # with the qualitative palette the names work for a map too
  expect_no_error(ggplot2::ggplot_build(
    cpb_map(md, region = prov, value = v, level = "provincie",
            palette = "qualitative",
            colour_index = c("siren", "brown", "dorado"))
  ))
})
