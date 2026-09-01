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

test_that("save_cpb draws sec_ylab flush with the right edge of the secondary axis, and top-anchored like the subtitle", {
  # cpb_add_sec_ylab_grob() (see save.R) used to centre the caption's
  # last character over the tick text's own midpoint, which reads as
  # stopping short of the axis rather than aligned with it; it is now
  # anchored flush against the "axis-r" cell's own right edge instead,
  # matching the published look.
  #
  # It also used to centre the caption vertically in the shared
  # subtitle row (vjust = 0.5), while theme_cpb()'s own plot.subtitle
  # is top-anchored there instead (vjust = 1, a bottom-only margin),
  # so a centred caption drew visibly lower than ylab()'s own subtitle
  # on the left. Top-anchored here too now, to match.
  d <- data.frame(x = 1:5, y = 1:5, z = c(10, 12, 11, 13, 12))
  p <- cpb_line(d, x = x, y = y, sec_y = z, sec_ylab = "%")

  path <- withr::local_tempfile(fileext = ".png")
  save_cpb(path, p, page = "half")

  sec_ylab <- cpb_take_sec_ylab(p)
  g <- ggplot2::ggplotGrob(sec_ylab$plot)
  g <- cpb_add_sec_ylab_grob(g, sec_ylab$label, 2.98, 2.98)
  grob <- g$grobs[[which(g$layout$name == "sec-ylab")]]

  expect_equal(as.numeric(grob$x), 1)
  expect_equal(grid::unitType(grob$x), "npc")
  expect_equal(grob$hjust, 1)
  expect_equal(as.numeric(grob$y), 1)
  expect_equal(grid::unitType(grob$y), "npc")
  expect_equal(grob$vjust, 1)
})

test_that("cpb_align_value_axis_title nudges a flush-right value-axis title to match the outermost tick label", {
  # theme_cpb()'s axis.title is anchored flush with the *panel* edge
  # (hjust = 1), but every wrapper's value axis is flush too (the
  # highest break sits exactly on the panel edge, no expansion) and
  # axis text is centred on its own break -- so the outermost tick
  # label's own text overhangs the panel edge by half its width, and a
  # title anchored to the bare edge lands short of it by that much.
  d <- data.frame(
    groep = factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d")),
    val = c(10, 25, 47, 63)
  )
  p <- cpb_col(d, x = groep, y = val, orientation = "horizontal",
               xlab = "aandeel binnen inkomensgroep (%)", pct_axis = TRUE)
  g <- ggplot2::ggplotGrob(p)

  title_idx <- which(g$layout$name == "xlab-b")
  axis_idx <- which(g$layout$name == "axis-b")
  before <- cpb_find_grob(g$grobs[[title_idx]], "text")
  tick_text <- cpb_find_grob(g$grobs[[axis_idx]], "text")

  g2 <- cpb_align_value_axis_title(g)
  after <- cpb_find_grob(g2$grobs[[title_idx]], "text")

  # unchanged: y position, hjust, label text
  expect_equal(as.numeric(after$y), as.numeric(before$y))
  expect_equal(after$hjust, before$hjust)
  expect_equal(after$label, before$label)
  # x anchor moved out past the panel edge (1npc) by exactly half the
  # rightmost tick label's own rendered width
  expect_equal(grid::unitType(after$x), "sum")
  last_label <- tick_text$label[[which.max(as.numeric(tick_text$x))]]
  expected_shift <- grid::convertWidth(
    grid::grobWidth(grid::textGrob(last_label, gp = tick_text$gp)),
    "in", valueOnly = TRUE
  ) / 2
  actual_shift <- grid::convertWidth(after$x, "in", valueOnly = TRUE) -
    grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  expect_equal(actual_shift, expected_shift, tolerance = 1e-6)
})

test_that("cpb_align_value_axis_title is a no-op when there is no value-axis title to align", {
  d <- data.frame(
    groep = factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d")),
    val = c(10, 25, 47, 63)
  )
  # vertical orientation: xlab/ylab convention puts the value-axis
  # label in the subtitle, not a "xlab-t"/"xlab-b" cell
  p <- cpb_col(d, x = groep, y = val, orientation = "vertical", ylab = "%")
  g <- ggplot2::ggplotGrob(p)
  g2 <- cpb_align_value_axis_title(g)
  expect_identical(g2, g)
})

test_that("save_cpb aligns the value-axis title without a panel_size or sec_y set", {
  # exercises the branch in save_cpb() that has to build the grob to
  # check whether cpb_align_value_axis_title() applies, with neither
  # panel_size nor sec_y actually requesting the grob path
  d <- data.frame(
    groep = factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d")),
    val = c(10, 25, 47, 63)
  )
  p <- cpb_col(d, x = groep, y = val, orientation = "horizontal",
               xlab = "aandeel binnen inkomensgroep (%)", pct_axis = TRUE)
  path <- withr::local_tempfile(fileext = ".png")
  expect_no_error(save_cpb(path, p, page = "full"))
  expect_true(file.exists(path))
})

test_that("cpb_donut()/cpb_map() tag their plot with class \"cpb_plot\", a plain ggplot does not", {
  d <- data.frame(bron = factor(c("a", "b")), share = c(60, 40))
  expect_true(inherits(cpb_donut(d, fill = bron, y = share), "cpb_plot"))

  df <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
  expect_false(inherits(df, "cpb_plot"))
})

test_that("the \"cpb_plot\" class (and so the warning) survives an ordinary + theme() call", {
  d <- data.frame(bron = factor(c("a", "b")), share = c(60, 40))
  p <- cpb_donut(d, fill = bron, y = share) + ggplot2::theme(legend.position = "none")
  expect_true(inherits(p, "cpb_plot"))
})

test_that("print.cpb_plot() warns that a bare print shows an approximate placement", {
  # rlang::warn(.frequency = "once") tracks each distinct .frequency_id
  # globally for the R session, not per test -- a fresh, distinct
  # colour/fill combination keeps this test's warning independent of
  # any other test that already printed a cpb_donut()/cpb_map() plot
  d <- data.frame(
    bron = factor(paste0("bron_", sample.int(1e6, 1))), share = 100
  )
  p <- cpb_donut(d, fill = bron, y = share)
  expect_warning(print(p), "only render\\(s\\) exactly when written out through save_cpb\\(\\)")
})

test_that("save_cpb() never triggers print.cpb_plot()'s warning, on either its fast or grob path", {
  d <- data.frame(bron = factor(c("a", "b")), share = c(60, 40))
  p <- cpb_donut(d, fill = bron, y = share)
  path <- withr::local_tempfile(fileext = ".png")
  # the fast path (ggplot2::ggsave()) prints the plot internally to
  # render it -- without save_cpb() stripping the "cpb_plot" class
  # first, that internal print() would trigger the same warning on
  # every ordinary save_cpb() call
  expect_no_warning(save_cpb(path, p, page = "half"))

  # the map aspect fit forces the grob path (cpb_fix_panel_size())
  # whenever height is left auto; an explicit height instead takes the
  # plain ggsave() fast path even though cpb_map_aspect is still set,
  # since the caller explicitly opted out of the aspect fit
  prov <- data.frame(naam = unique(cpb_nl_geo("provincie")$name))
  prov$w <- seq_len(nrow(prov))
  m <- cpb_map(prov, region = naam, value = w, level = "provincie")
  path2 <- withr::local_tempfile(fileext = ".png")
  expect_no_warning(save_cpb(path2, m, page = "half", height = 3))
})
