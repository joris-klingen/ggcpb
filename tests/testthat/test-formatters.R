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

test_that("label_number_nl auto-detects decimal accuracy when accuracy is NULL", {
  breaks <- c(7.0, 20.8, 34.6, 48.4, 62.2, 76.0)
  out_dutch <- label_number_nl(style = "dutch")(breaks)
  expect_equal(out_dutch, c("7,0", "20,8", "34,6", "48,4", "62,2", "76,0"))

  out_english <- label_number_nl(style = "english")(breaks)
  expect_equal(out_english, c("7.0", "20.8", "34.6", "48.4", "62.2", "76.0"))
})

test_that("the ggcpb.style option supplies the default style", {
  # the option exists so a whole English report can be switched over in
  # one line: missing a single `style = "english"` in a twelve-figure
  # document otherwise prints one stray Dutch decimal comma, silently.
  withr::with_options(list(ggcpb.style = "english"), {
    expect_equal(label_number_nl(accuracy = 0.1)(1234.5), "1,234.5")
    expect_equal(label_pct_nl(accuracy = 0.1)(4.5), "4.5%")
    expect_match(label_euro_nl()(1234.5), "1,234.50", fixed = TRUE)
    # an explicit argument still wins over the option
    expect_equal(label_number_nl(accuracy = 0.1, style = "dutch")(1234.5), "1.234,5")
  })
  # unset, the Dutch house style is unchanged
  expect_equal(label_number_nl(accuracy = 0.1)(1234.5), "1.234,5")
})

test_that("an unusable ggcpb.style option is rejected, not ignored", {
  withr::with_options(list(ggcpb.style = "german"), {
    expect_error(label_number_nl(), "should be one of")
  })
})

test_that("wrappers take their style from the ggcpb.style option", {
  d <- data.frame(x = 1:5, y = c(0.15, 0.32, 0.48, 0.61, 0.79))
  ylabs <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  }
  withr::with_options(list(ggcpb.style = "english"), {
    expect_true(all(grepl("[0-9][.][0-9]", ylabs(cpb_line(d, x = x, y = y)))))
    expect_true(all(grepl(
      "[0-9][,][0-9]",
      ylabs(cpb_line(d, x = x, y = y, style = "dutch"))
    )))
  })
  expect_true(all(grepl("[0-9][,][0-9]", ylabs(cpb_line(d, x = x, y = y)))))
})

test_that("style reaches every number a wrapper prints, not just the axis", {
  # cpb_box's median/quartile labels and cpb_donut's percentages are
  # drawn as text layers rather than axis labels, so they need the
  # style passed through explicitly to avoid a Dutch comma slipping
  # into an otherwise English figure.
  labels_of <- function(p) {
    built <- ggplot2::ggplot_build(p)$data
    unlist(lapply(built, function(l) {
      if ("label" %in% names(l)) as.character(l$label)
    }))
  }

  b <- data.frame(
    grp = c("a", "b"), p5 = c(0, 1), p25 = c(1, 2),
    p50 = c(1.5, 2.5), p75 = c(2, 3), p95 = c(3, 4)
  )
  box_labs <- function(...) {
    labels_of(cpb_box(b,
      x = grp, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
      box_style = "modern", ...
    ))
  }
  expect_true(any(grepl("[0-9][.][0-9]", box_labs(style = "english"))))
  expect_true(any(grepl("[0-9][,][0-9]", box_labs(style = "dutch"))))

  dn <- data.frame(k = c("a", "b", "c"), v = c(45.5, 30.25, 24.25))
  donut_labs <- function(...) {
    labels_of(cpb_donut(dn, fill = k, y = v, label_accuracy = 0.1, ...))
  }
  expect_true(any(grepl("[0-9][.][0-9]%", donut_labs(style = "english"))))
  expect_true(any(grepl("[0-9][,][0-9]%", donut_labs())))
})
