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

test_that("save_cpb auto-fits a cpb_map() panel to its geographic aspect ratio", {
  skip_if_not_installed("ragg")
  prov <- data.frame(naam = unique(cpb_nl_geo("provincie")$name))
  prov$w <- seq_len(nrow(prov))
  p <- cpb_map(prov, region = naam, value = w, level = "provincie")
  expect_false(is.null(attr(p, "cpb_map_aspect")))

  path <- withr::local_tempfile(fileext = ".png")
  msg <- testthat::capture_output(save_cpb(path, p, page = "half"))
  # not the 2.98 in "report" preset square -- the panel drove the height
  expect_false(grepl("2.98 x 2.98 in", msg, fixed = TRUE))
  expect_true(file.exists(path))

  # an explicit height opts back out, same as an explicit panel_size does
  msg2 <- testthat::capture_output(save_cpb(path, p, page = "half", height = 3))
  expect_match(msg2, "2.98 x 3 in", fixed = TRUE)
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

test_that("save_cpb keeps sec_y's own colour intact when a layer is inserted ahead of it", {
  # cpb_take_sec_ylab() (removing the wrapper's approximate sec_ylab
  # placeholder layer, so save_cpb()'s own exact one replaces it, see
  # cpb_add_sec_ylab_grob()) used to look that layer up by the
  # position it was added at. A caller inserting a layer ahead of it
  # afterward -- p$layers <- c(new, p$layers), the documented way to
  # draw something underneath everything else -- shifted every later
  # position by one without updating the stored one, so the wrong
  # layer got deleted: often the sec_y line itself, silently dropping
  # it (and its own colour scale then had no data left to train
  # against, warning "No shared levels found ...").
  d <- data.frame(x = 1:3, y = 1:3, z = c(5, 6, 7))
  p <- cpb_line(d, x = x, y = y, sec_y = z, sec_ylab = "n")
  p$layers <- c(list(ggplot2::geom_hline(yintercept = 0, colour = "white")), p$layers)

  path <- withr::local_tempfile(fileext = ".png")
  warnings <- character(0)
  withCallingHandlers(
    save_cpb(path, p, page = "half"),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("shared levels", warnings, fixed = TRUE)))

  b <- ggplot2::ggplot_build(p)
  is_line <- vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))
  sec_line_idx <- which(is_line)[[2]] # the first GeomLine is the inserted hline's sibling primary line
  expect_false(anyNA(b$data[[sec_line_idx]]$colour))
})
