# test-cut.R ----

test_that("cpb_cut builds open-ended Dutch class labels", {
  f <- cpb_cut(c(8, 24, 33, 47, 61, 72),
               breaks = c(0, 20, 30, 40, 50, 60, Inf),
               labeller = label_pct_nl())
  expect_s3_class(f, "ordered")
  expect_equal(levels(f),
    c("lager dan 20%", "20% - 30%", "30% - 40%",
      "40% - 50%", "50% - 60%", "60% en hoger"))
  # values land in the right classes
  expect_equal(as.character(f[1]), "lager dan 20%")
  expect_equal(as.character(f[6]), "60% en hoger")
})

test_that("cpb_cut formats with the number formatters and honours overrides", {
  # euro
  f <- cpb_cut(c(5, 50), breaks = c(0, 25, Inf), labeller = label_euro_nl())
  expect_match(levels(f)[1], "lager dan €25")
  expect_match(levels(f)[2], "€25 en hoger")
  # two-class (no middle), plus a first-label override
  f2 <- cpb_cut(c(0, 1, 5, 12), breaks = c(0, 1, 5, 10, Inf), first = "geen")
  expect_equal(levels(f2), c("geen", "1 - 5", "5 - 10", "10 en hoger"))
})

test_that("cpb_cut accepts a single integer for equal-width bins", {
  f <- cpb_cut(1:100, breaks = 4L)
  expect_length(levels(f), 4)
  expect_match(levels(f)[1], "^lager dan ")
  expect_match(levels(f)[4], " en hoger$")
})

test_that("cpb_cut validates its input", {
  expect_error(cpb_cut(letters, breaks = c(0, 1)), "numeric")
  expect_error(cpb_cut(1:10, breaks = 1, labeller = "x"), "function")
})

test_that("the blues palette ramps light to dark and integrates with scales", {
  six <- cpb_pal("blues")(6)
  expect_length(six, 6)
  # monotone decreasing lightness (sum of channels)
  lum <- vapply(grDevices::col2rgb(six), sum, numeric(1))[seq(1, 18, 3)]
  lum <- colSums(grDevices::col2rgb(six))
  expect_true(all(diff(lum) < 0))
  # exposed in tokens and usable as a discrete scale palette
  expect_true("blues" %in% names(cpb_tokens()) || !is.null(cpb_tokens()$colors_blues))
  sc <- scale_fill_cpb_d(palette = "blues")
  expect_s3_class(sc, "Scale")
  scc <- scale_fill_cpb_c()  # continuous still fine
  expect_s3_class(scc, "Scale")
})
