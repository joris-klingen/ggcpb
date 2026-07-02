# test-palettes.R ----

test_that("cpb_pal returns the exact qualitative palette values", {
  qual <- cpb_pal("qualitative")(9)
  expect_equal(qual, c(
    "#F596AF", "#e6006e", "#820050", "#d7c8c8", "#87d2ff",
    "#005faf", "#193c69", "#96827d", "#64504b"
  ))
  expect_false("lightgrey" %in% qual)
})

test_that("cpb_pal returns the exact discr palette values", {
  discr <- cpb_pal("discr")(7)
  expect_equal(discr, c(
    "#eb0073", "#005795", "#fad1e8", "#b7e4ff",
    "#820050", "#97cafb", "#00a5ff"
  ))
  expect_false("lightgrey" %in% discr)
})

test_that("cpb_pal reverses the palette when requested", {
  qual <- cpb_pal("qualitative")(3)
  qual_rev <- cpb_pal("qualitative", reverse = TRUE)(3)
  expect_equal(qual_rev, rev(cpb_pal("qualitative")(9))[1:3])
  expect_false(identical(qual, qual_rev))
})

test_that("cpb_pal interpolates the sequential ramp and anchors low/high", {
  ramp <- cpb_pal("sequential")(2)
  expect_length(ramp, 2)
  expect_equal(tolower(ramp[1]), "#fff1f8")
  expect_equal(tolower(ramp[2]), "#4f0a2a")
})

test_that("cpb_pal recycles qualitative colours with a warning beyond palette length", {
  expect_warning(cols <- cpb_pal("qualitative")(12), "recycling")
  expect_length(cols, 12)
})

test_that("cpb_cols pulls swatches by position from the qualitative palette", {
  out <- cpb_cols(6, 2)
  expect_equal(unname(out), c("#005faf", "#e6006e"))
  expect_equal(names(out), c("6", "2"))
})

test_that("cpb_cols supports other palettes", {
  out <- cpb_cols(1:3, palette = "discr")
  expect_equal(unname(out), c("#eb0073", "#005795", "#fad1e8"))
})

test_that("cpb_cols errors on an out-of-range index", {
  expect_error(cpb_cols(99), "out of range")
  expect_error(cpb_cols(0), "out of range")
})
