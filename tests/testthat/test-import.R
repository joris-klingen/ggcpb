# test-import.R ----

local_data_csv <- function() {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(c(
    "jaar,soort,mld,heffing",
    "2020,A,2.0,0.8",
    "2020,B,3.0,0.8",
    "2021,A,2.5,1.0",
    "2021,B,3.2,1.0"
  ), path)
  path
}

local_params_csv <- function(lines) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(lines, path)
  path
}

test_that("import_csv builds a single figure from a vertical params.csv", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,line",
    "title,Testfiguur",
    "x,jaar",
    "y,mld",
    "colour,soort",
    "ylab,mld euro"
  ))

  p <- import_csv(data_csv, params_csv)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
  expect_equal(p$labels$title, "Testfiguur")
  expect_true(!is.null(p$scales$get_scales("colour")))
})

test_that("import_csv builds several figures from a horizontal params.csv, skipping create = n", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,title,x,y,create",
    "col,Eerste,jaar,mld,y",
    "col,Overgeslagen,jaar,mld,n",
    "line,Tweede,jaar,mld,yes"
  ))

  ps <- import_csv(data_csv, params_csv)

  expect_type(ps, "list")
  expect_length(ps, 2)
  expect_named(ps, c("Eerste", "Tweede"))
  expect_true(inherits(ps$Eerste$layers[[1]]$geom, "GeomCol"))
  expect_true(inherits(ps$Tweede$layers[[1]]$geom, "GeomLine"))
})

test_that("import_csv resolves sec_y against the data and passes sec_type through", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,col",
    "x,jaar",
    "y,mld",
    "sec_y,heffing",
    "sec_type,point"
  ))

  p <- import_csv(data_csv, params_csv)

  is_point <- vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  expect_true(any(is_point))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
})

test_that("import_csv splits a semicolon-delimited value into a vector", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "colour,soort",
    "index,6;2"
  ))

  expect_no_error(import_csv(data_csv, params_csv))
})

test_that("import_csv drops unrecognised parameter names, warning instead of erroring", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "this_is_not_a_param,banaan"
  ))

  expect_no_error(suppressWarnings(import_csv(data_csv, params_csv)))
  expect_warning(import_csv(data_csv, params_csv), "this_is_not_a_param")
})

test_that("import_csv always (over)writes a run log next to params_csv", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,title,x,y,create",
    "col,Eerste,jaar,mld,y",
    "col,Overgeslagen,jaar,mld,n"
  ))
  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")

  import_csv(data_csv, params_csv)

  expect_true(file.exists(log_path))
  log_lines <- readLines(log_path)
  expect_match(paste(log_lines, collapse = "\n"), "Figure 1 \"Eerste\"")
  expect_match(paste(log_lines, collapse = "\n"), "skipped \\(create = n\\)")
  expect_match(paste(log_lines, collapse = "\n"), "2 figure\\(s\\) read, 1 skipped, 1 built")
})

test_that("import_csv errors clearly on an unknown plot_type", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c("plot_type,pie", "x,jaar", "y,mld"))

  expect_error(import_csv(data_csv, params_csv), "Unknown plot_type")
})

test_that("import_csv errors clearly when a column reference isn't in the data", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,doesnotexist"))

  expect_error(import_csv(data_csv, params_csv), "not a column in the data")
})

test_that("import_csv errors clearly on missing files", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,mld"))

  expect_error(import_csv("nope.csv", params_csv), "not found")
  expect_error(import_csv(data_csv, "nope.csv"), "not found")
})

test_that("import_csv applies ... overrides on top of params.csv", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c(
    "plot_type,line", "x,jaar", "y,mld", "colour,soort", "legend,bottom"
  ))

  p <- import_csv(data_csv, params_csv, legend = "right")

  expect_equal(p$theme$legend.position, "right")
})
