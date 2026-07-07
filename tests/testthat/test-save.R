# test-save.R ----

test_that("save_cpb writes a file at the requested CPB page width", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar() +
    theme_cpb()

  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  out <- save_cpb(path, p, page = "half")

  expect_true(file.exists(path))
  expect_equal(out, path)
})

test_that("save_cpb rejects a width outside the CPB page presets", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()

  expect_error(
    save_cpb(tempfile(fileext = ".png"), p, width = 8),
    "CPB page widths"
  )
})

test_that("save_cpb accepts an explicit width matching a CPB page preset", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  expect_no_error(save_cpb(path, p, width = 5.96, height = 3))
  expect_true(file.exists(path))
})

test_that("preset controls the default height, and an explicit height wins", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  msg <- testthat::capture_output(
    save_cpb(path, p, page = "full", preset = "presentation")
  )
  expect_match(msg, "5.96 x 2.5 in", fixed = TRUE)

  msg2 <- testthat::capture_output(
    save_cpb(path, p, page = "half", preset = "presentation", height = 4)
  )
  expect_match(msg2, "2.98 x 4 in", fixed = TRUE)
})

test_that("save_cpb warns on a title too long for the page, not when wrapped", {
  skip_if_not_installed("ragg")
  df <- data.frame(x = c("a", "b"), y = 1:2)
  path <- withr::local_tempfile(fileext = ".png")
  long <- "Een uitzonderlijk lange titel die zeker niet op een halve pagina past"

  # long single-line title on a half page -> warning suggesting \n
  expect_warning(
    save_cpb(path, cpb_col(df, x = x, y = y, title = long), page = "half"),
    "\\\\n"
  )
  # the same length split over two lines fits -> no warning
  wrapped <- "Een uitzonderlijk lange titel\ndie over twee regels loopt"
  expect_no_warning(
    save_cpb(path, cpb_col(df, x = x, y = y, title = wrapped), page = "half")
  )
  # a long title fits on the full page
  expect_no_warning(
    save_cpb(path, cpb_col(df, x = x, y = y, title = long), page = "full")
  )
  # no title, no warning
  expect_no_warning(save_cpb(path, cpb_col(df, x = x, y = y), page = "half"))
})
