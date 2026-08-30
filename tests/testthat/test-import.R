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

test_that("import_csv skips (warning) rather than crashes on one bad figure, but errors if that leaves nothing built", {
  data_csv <- local_data_csv()

  # a single-figure params.csv where that one figure is unbuildable: a
  # console warning() for the specific reason, and -- since nothing at
  # all was built -- a top-level error, not a silently empty result
  params_csv <- local_params_csv(c("plot_type,pie", "x,jaar", "y,mld"))
  expect_warning(
    expect_error(import_csv(data_csv, params_csv), "Unknown plot_type"),
    "Unknown plot_type"
  )

  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,doesnotexist"))
  expect_warning(
    expect_error(import_csv(data_csv, params_csv), "not a column in the data"),
    "not a column in the data"
  )

  # the same error in just one of several figures instead leaves the
  # others built -- an unattended batch must not lose everything over
  # one broken row
  params_csv <- local_params_csv(c(
    "plot_type,title,x,y",
    "line,Goed,jaar,mld",
    "line,Kapot,jaar,doesnotexist"
  ))
  expect_warning(import_csv(data_csv, params_csv), "not a column in the data")
  ps <- suppressWarnings(import_csv(data_csv, params_csv))
  expect_s3_class(ps, "ggplot") # only one figure survived, so it is not a list
  expect_equal(ps$labels$title, "Goed")

  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")
  log_text <- paste(readLines(log_path), collapse = "\n")
  expect_match(log_text, "Figure 2 \"Kapot\": skipped \\(error: .*not a column")
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

test_that("cpb_import_kit copies every kit file into a new folder", {
  dest <- withr::local_tempdir()
  cpb_import_kit(dest)

  kit_dir <- system.file("import_kit", package = "ggcpb")
  expect_true(dir.exists(kit_dir))
  expect_setequal(list.files(dest), list.files(kit_dir))

  # the two data files ship ready to run as-is
  p <- import_csv(file.path(dest, "data.csv"), file.path(dest, "params.csv"))
  expect_type(p, "list")
})

test_that("cpb_import_kit keeps run_import.command executable", {
  dest <- withr::local_tempdir()
  cpb_import_kit(dest)

  command_file <- file.path(dest, "run_import.command")
  expect_true(file.exists(command_file))
  expect_equal(as.character(file.info(command_file)$mode), "755")
})

test_that("cpb_import_kit refuses to overwrite without being asked to", {
  dest <- withr::local_tempdir()
  cpb_import_kit(dest)

  expect_error(cpb_import_kit(dest), "already exist")
  expect_no_error(cpb_import_kit(dest, overwrite = TRUE))
})

test_that("cpb_import_kit creates the destination folder if needed", {
  dest <- file.path(withr::local_tempdir(), "nieuwe_map")
  expect_false(dir.exists(dest))

  cpb_import_kit(dest)

  expect_true(file.exists(file.path(dest, "run_import.R")))
})

test_that("import_csv errors clearly when data_csv has no header row", {
  data_csv <- local_params_csv(c("2020,A,2.0", "2021,B,3.0"))
  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,koopkracht"))

  expect_error(import_csv(data_csv, params_csv), "does not look like column names")
})

test_that("import_csv errors clearly on a stray line above data_csv's header", {
  data_csv <- local_params_csv(c(
    "Dit is een titel regel", "jaar,groep,koopkracht", "2020,A,2.0", "2021,B,3.0"
  ))
  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,koopkracht"))

  expect_error(import_csv(data_csv, params_csv), "no title, no blank line")
})

test_that("import_csv errors clearly when data_csv uses tabs but sep is a comma", {
  data_csv <- local_params_csv(c("jaar\tgroep\tkoopkracht", "2020\tA\t2.0", "2021\tB\t3.0"))
  params_csv <- local_params_csv(c("plot_type,line", "x,jaar", "y,koopkracht"))

  expect_error(import_csv(data_csv, params_csv), "might use a tab instead")
})

test_that("import_csv errors clearly when params_csv uses tabs but sep is a comma", {
  data_csv <- local_data_csv()
  params_csv <- local_params_csv(c("plot_type\tline", "x\tjaar", "y\tmld"))

  expect_error(import_csv(data_csv, params_csv), "might use a tab instead")
})

test_that("import_csv reads both files fine when sep matches a tab-separated pair", {
  data_csv <- local_params_csv(c("jaar\tgroep\tkoopkracht", "2020\tA\t2.0", "2021\tB\t3.0"))
  params_csv <- local_params_csv(c("plot_type\tline", "x\tjaar", "y\tkoopkracht"))

  expect_no_error(import_csv(data_csv, params_csv, sep = "\t"))
})
