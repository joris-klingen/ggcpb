# test-palettes.R ----

test_that("cpb_pal cycles the qualitative palette in published order", {
  qual <- cpb_pal("qualitative")(9)
  expect_equal(qual, c(
    "#005faf", "#e6006e", "#96827d", "#193c69", "#820050",
    "#87d2ff", "#64504b", "#F596AF", "#d7c8c8"
  ))
  expect_false("lightgrey" %in% qual)

  # the published figures lead blue, magenta, taupe -- cpb_colors[c(6, 2, 8)]
  # (references/plots/: fig 1.1 left, 1.3 right, both p10 charts), so a
  # default chart must reach those without the caller passing `index =`
  expect_equal(cpb_pal("qualitative")(3), unname(cpb_cols(6, 2, 8)))
})

test_that("swatch access stays positional despite the cycling order", {
  # cpb_cols() and `index =` address the raw swatch vector by position,
  # the way CPB source scripts write cpb_colors[c(6, 2)]; reordering the
  # palette cycle must not renumber them
  expect_equal(unname(cpb_cols(1)), "#F596AF")
  expect_equal(unname(cpb_cols(6)), "#005faf")
  expect_equal(unname(cpb_cols(2)), "#e6006e")
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
