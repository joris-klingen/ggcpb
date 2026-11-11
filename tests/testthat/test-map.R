# test-map.R ----

test_that("cpb_map: no na_fill leaves missing regions unfilled, no key", {
  d <- data.frame(prov = c("Groningen", "Fryslân"), v = c("a", "b"))
  sc <- cpb_map(d, region = prov, value = v, level = "provincie")$scales$
    get_scales("fill")
  expect_true(is.na(sc$na.value))
  expect_false(sc$na.translate)
})

test_that("cpb_map: na_fill fills them and names the legend key", {
  d <- data.frame(prov = c("Groningen", "Fryslân"), v = c("a", "b"))
  sc_of <- function(na, ...) {
    cpb_map(d, region = prov, value = v, level = "provincie", na_fill = na,
            ...)$scales$get_scales("fill")
  }
  # a palette position, the same as the string and a plain colour
  expect_equal(unname(sc_of(4)$na.value), unname(cpb_cols(4)))
  expect_equal(unname(sc_of("4")$na.value), unname(cpb_cols(4)))
  expect_equal(unname(sc_of("#bfbfbf")$na.value), "#bfbfbf")
  labels_of <- function(...) {
    p <- cpb_map(d, region = prov, value = v, level = "provincie",
                 na_fill = 4, ...)
    ggplot2::ggplot_build(p)$plot$scales$get_scales("fill")$get_labels()
  }
  expect_equal(labels_of(), c("a", "b", "Geen gegevens"))
  expect_equal(labels_of(style = "english"), c("a", "b", "No data"))
})
