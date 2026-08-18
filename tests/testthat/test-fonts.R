test_that("cpb_font_face() maps weight and slant onto font_add() slots", {
  expect_equal(cpb_font_face(FALSE, FALSE), "plain")
  expect_equal(cpb_font_face(TRUE, FALSE), "bold")
  expect_equal(cpb_font_face(FALSE, TRUE), "italic")
  expect_equal(cpb_font_face(TRUE, TRUE), "bolditalic")
})

test_that("cpb_font_from_name() recovers family and face from a file name", {
  expect_equal(cpb_font_from_name("MyFont-BoldItalic_2_0.ttf"),
               list(family = "MyFont", face = "bolditalic"))
  expect_equal(cpb_font_from_name("Foo-Regular.otf"),
               list(family = "Foo", face = "plain"))
  expect_equal(cpb_font_from_name("Bar-Oblique.ttf")$face, "italic")
  # no face token at all: the whole stem is the family
  expect_equal(cpb_font_from_name("Plain.ttf"),
               list(family = "Plain", face = "plain"))
})

test_that("cpb_scan_fonts() reads family and face from font metadata", {
  dir <- system.file("fonts", package = "ggcpb")
  skip_if(!nzchar(dir) || !dir.exists(dir), "bundled fonts unavailable")

  found <- cpb_scan_fonts(dir)
  expect_true(all(found$family == "RijksoverheidSansText"))
  # the bundled family ships exactly these three faces
  expect_setequal(found$face, c("plain", "bold", "italic"))
  # regression: font_info() returns weight as an ordered factor, so a
  # naive identical(weight, "bold") files the bold face under "plain"
  expect_equal(basename(found$file[found$face == "bold"]),
               "RijksoverheidSansText-Bold_2_0.ttf")
})

test_that("cpb_scan_fonts() falls back to the file name for unreadable files", {
  tmp <- withr::local_tempdir()
  writeLines("not a font", file.path(tmp, "Fake-Bold.ttf"))
  found <- cpb_scan_fonts(tmp)
  expect_equal(found$family, "Fake")
  expect_equal(found$face, "bold")
})

test_that("cpb_scan_fonts() validates its path and searches recursively", {
  expect_error(cpb_scan_fonts(c("a", "b")), "single directory")
  expect_error(cpb_scan_fonts(file.path(tempdir(), "nope-not-here")),
               "not an existing directory")

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "nested"))
  file.copy(cpb_font_files()$plain, file.path(tmp, "nested", "Reg.ttf"))
  expect_equal(nrow(cpb_scan_fonts(tmp)), 0L)
  expect_equal(nrow(cpb_scan_fonts(tmp, recursive = TRUE)), 1L)
})

test_that("cpb_register_fonts() reports what it registered", {
  res <- cpb_register_fonts(enable_showtext = FALSE)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("family", "faces", "n_files", "systemfonts",
                      "sysfonts", "windows"))
  expect_equal(res$family, "RijksoverheidSansText")
  expect_true(res$systemfonts || res$sysfonts)
  expect_false(attr(res, "showtext"))
  # off Windows the base-device mapping is a documented no-op
  skip_on_os("windows")
  expect_false(res$windows)
})

test_that("cpb_register_fonts(path =) adds families without dropping the house one", {
  tmp <- withr::local_tempdir()
  writeLines("not a font", file.path(tmp, "Extra-Regular.ttf"))

  res <- cpb_register_fonts(path = tmp, enable_showtext = FALSE)
  expect_true("RijksoverheidSansText" %in% res$family)
  expect_true("Extra" %in% res$family)
  # each family reported once, even when `path` re-registers the bundled one
  expect_false(any(duplicated(res$family)))
  expect_true(cpb_font_family() == "RijksoverheidSansText" ||
                cpb_font_family() == "")
})

test_that("cpb_register_fonts() warns on a directory with no fonts", {
  tmp <- withr::local_tempdir()
  expect_warning(cpb_register_fonts(path = tmp, enable_showtext = FALSE),
                 "no font files found")
})

test_that("enable_showtext toggles showtext rendering", {
  skip_if_not_installed("showtext")
  withr::defer(showtext::showtext_auto(FALSE))

  res <- cpb_register_fonts(enable_showtext = TRUE)
  expect_true(attr(res, "showtext"))

  # and the acceptance case: a plot using the family renders on a device
  # that does its own font lookup, rather than erroring on it
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::theme(text = ggplot2::element_text(family = "RijksoverheidSansText"))
  f <- withr::local_tempfile(fileext = ".png")
  expect_silent({
    grDevices::png(f, width = 400, height = 300)
    print(p)
    grDevices::dev.off()
  })
  expect_gt(file.size(f), 0)
})
