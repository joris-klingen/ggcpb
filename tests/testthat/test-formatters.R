# test-formatters.R ----

test_that("label_euro_nl formats with a euro sign and Dutch marks", {
  out <- label_euro_nl(accuracy = 1)(c(1000, 25000))
  expect_equal(out, c("\u20ac1.000", "\u20ac25.000"))
})

test_that("label_pct_nl treats values as already being percentage points", {
  out <- label_pct_nl()(c(4, 12.4))
  expect_equal(out, c("4%", "12%"))
})

test_that("label_pct_nl handles proportions with scale = 100", {
  out <- label_pct_nl(scale = 100, accuracy = 0.1)(c(0.045, 0.12))
  expect_equal(out, c("4,5%", "12,0%"))
})

test_that("label_number_nl uses Dutch grouping and decimal marks", {
  out <- label_number_nl(accuracy = 1)(1234567)
  expect_equal(out, "1.234.567")

  out_dec <- label_number_nl(accuracy = 0.1)(1234.5)
  expect_equal(out_dec, "1.234,5")
})
