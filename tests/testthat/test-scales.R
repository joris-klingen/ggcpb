# test-scales.R ----

test_that("scale_fill_cpb_d defaults na.value to the CPB NA colour", {
  sc <- scale_fill_cpb_d()
  expect_equal(sc$na.value, "lightgrey")
})

test_that("scale_fill_cpb_d maps discrete levels to the qualitative palette", {
  df <- data.frame(g = factor(c("a", "b", "c")))
  p <- ggplot2::ggplot(df, ggplot2::aes(g, fill = g)) +
    ggplot2::geom_bar() +
    scale_fill_cpb_d()
  built <- ggplot2::ggplot_build(p)
  fills <- unique(built$data[[1]]$fill)
  expect_true(all(fills %in% cpb_tokens()$colors))
})

test_that("scale_fill_cpb_c builds a gradient scale spanning the sequential ramp", {
  sc <- scale_fill_cpb_c()
  expect_equal(sc$na.value, "lightgrey")
  expect_s3_class(sc, "ScaleContinuous")
})

test_that("scale_fill_cpb_manual selects and orders specific palette positions", {
  df <- data.frame(g = factor(c("a", "b"), levels = c("a", "b")))
  p <- ggplot2::ggplot(df, ggplot2::aes(g, fill = g)) +
    ggplot2::geom_bar() +
    scale_fill_cpb_manual(index = c(6, 2))
  built <- ggplot2::ggplot_build(p)
  fills <- built$data[[1]]$fill
  expect_equal(fills, unname(cpb_cols(6, 2)))
})

test_that("colour/color aliases are the same function", {
  expect_identical(scale_color_cpb_d, scale_colour_cpb_d)
  expect_identical(scale_color_cpb_c, scale_colour_cpb_c)
  expect_identical(scale_color_cpb_manual, scale_colour_cpb_manual)
})
