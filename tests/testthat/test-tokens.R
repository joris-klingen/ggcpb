# test-tokens.R ----

test_that("cpb_tokens excludes the NA colour from the data-colour vectors", {
  tok <- cpb_tokens()

  expect_false("lightgrey" %in% tok$colors)
  expect_false("lightgrey" %in% tok$colors_discr)
  expect_false("lightgrey" %in% tok$colors_scale)
  expect_equal(tok$na, "lightgrey")

  expect_length(tok$colors, 9)
  expect_length(tok$colors_discr, 7)
  expect_length(tok$colors_scale, 6)
})

test_that("cpb_tokens reports the correct structural colours", {
  tok <- cpb_tokens()
  expect_equal(tok$bg, "#eef8ff")
  expect_equal(tok$grid, "#c9d1da")
  expect_equal(tok$table_header, "#a81256")
  expect_equal(tok$table_total, "#f08bb8")
})
