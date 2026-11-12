# test-attached-packages.R ----

# The wrappers look their functions up in the package namespace, so what
# a user happened to attach before must not change what is drawn.

test_that("attached packages do not change what the wrappers draw", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("data.table")

  d <- data.frame(
    x = rep(2020:2024, 2), g = rep(c("a", "b"), each = 5),
    y = c(1, 3, 2, 5, 4, 2, 2, 3, 1, 4)
  )
  box <- data.frame(
    x = c("a", "b"), p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5
  )
  drawn <- function() {
    plots <- list(
      cpb_line(d, x = x, y = y, colour = g),
      cpb_col(d, x = x, y = y, fill = g),
      cpb_box(box, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95),
      cpb_scatter(d, x = x, y = y)
    )
    lapply(plots, function(p) ggplot2::ggplot_build(p)$data)
  }

  clean <- drawn()

  crowded <- withr::with_package("dplyr", withr::with_package(
    "tidyr", withr::with_package("data.table", drawn())
  ))

  expect_identical(crowded, clean)
})
