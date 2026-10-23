# test-import.R ----

# A figure folder in a temp dir: params.csv plus data/data.csv. Returns the
# path of params.csv. `data_lines` replaces the default dataset.
default_data <- c(
  "jaar,soort,mld,heffing",
  "2020,A,2.0,0.8",
  "2020,B,3.0,0.8",
  "2021,A,2.5,1.0",
  "2021,B,3.2,1.0"
)

local_figs <- function(params_lines, data_lines = default_data) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data"))
  writeLines(data_lines, file.path(dir, "data", "data.csv"))
  writeLines(params_lines, file.path(dir, "params.csv"))
  file.path(dir, "params.csv")
}

test_that("cpb_import_csv builds a single figure from a one column params.csv", {
  params_csv <- local_figs(c(
    "plot_type,line",
    "title,Testfiguur",
    "x,jaar",
    "y,mld",
    "colour,soort",
    "ylab,mld euro"
  ))

  p <- cpb_import_csv(params_csv)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
  expect_equal(p$labels$title, "Testfiguur")
  expect_true(!is.null(p$scales$get_scales("colour")))
})

test_that("cpb_import_csv builds several figures, skipping create = n", {
  params_csv <- local_figs(c(
    "plot_type,col,col,line",
    "title,Eerste,Overgeslagen,Tweede",
    "x,jaar,jaar,jaar",
    "y,mld,mld,mld",
    "create,y,n,yes"
  ))

  ps <- cpb_import_csv(params_csv)

  expect_type(ps, "list")
  expect_length(ps, 2)
  # one dataset, two figures: the figure number is added to the name
  expect_named(ps, c("data_1", "data_3"))
  expect_true(inherits(ps$data_1$layers[[1]]$geom, "GeomCol"))
  expect_true(inherits(ps$data_3$layers[[1]]$geom, "GeomLine"))
})

test_that("cpb_import_csv resolves sec_y against the data and passes sec_type through", {
  params_csv <- local_figs(c(
    "plot_type,col",
    "x,jaar",
    "y,mld",
    "sec_y,heffing",
    "sec_type,point"
  ))

  p <- cpb_import_csv(params_csv)

  is_point <- vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))
  expect_true(any(is_point))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"), logical(1))))
})

test_that("cpb_import_csv splits a semicolon-delimited value into a vector", {
  params_csv <- local_figs(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "colour,soort",
    "colour_index,6;2"
  ))

  expect_no_error(cpb_import_csv(params_csv))
})

test_that("cpb_import_csv drops unrecognised parameter names, warning instead of erroring", {
  params_csv <- local_figs(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "this_is_not_a_param,banaan"
  ))

  expect_no_error(suppressWarnings(cpb_import_csv(params_csv)))
  expect_warning(cpb_import_csv(params_csv), "this_is_not_a_param")
})

test_that("cpb_import_csv always (over)writes a run log next to params_csv", {
  params_csv <- local_figs(c(
    "plot_type,col,col",
    "title,Eerste,Overgeslagen",
    "x,jaar,jaar",
    "y,mld,mld",
    "create,y,n"
  ))
  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")

  cpb_import_csv(params_csv)

  expect_true(file.exists(log_path))
  log_text <- paste(readLines(log_path), collapse = "\n")
  expect_match(log_text, "Figure 1 \"Eerste \\[data.csv\\]\"")
  expect_match(log_text, "skipped \\(create = n\\)")
  expect_match(log_text, "2 figure\\(s\\) read, 1 skipped, 1 built")
})

test_that("cpb_import_csv skips (warning) rather than crashes on one bad figure, but errors if that leaves nothing built", {
  # a single-figure params.csv where that one figure is unbuildable: a
  # console warning() for the specific reason, and -- since nothing at
  # all was built -- a top-level error, not a silently empty result
  params_csv <- local_figs(c("plot_type,pie", "x,jaar", "y,mld"))
  expect_warning(
    expect_error(cpb_import_csv(params_csv), "Unknown plot_type"),
    "Unknown plot_type"
  )

  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,doesnotexist"))
  expect_warning(
    expect_error(cpb_import_csv(params_csv), "not a column in the data"),
    "not a column in the data"
  )

  # the same error in just one of several figures instead leaves the
  # others built -- an unattended batch must not lose everything over
  # one broken row
  params_csv <- local_figs(c(
    "plot_type,line,line",
    "title,Goed,Kapot",
    "x,jaar,jaar",
    "y,mld,doesnotexist"
  ))
  expect_warning(cpb_import_csv(params_csv), "not a column in the data")
  ps <- suppressWarnings(cpb_import_csv(params_csv))
  expect_s3_class(ps, "ggplot") # only one figure survived, so it is not a list
  expect_equal(ps$labels$title, "Goed")

  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")
  log_text <- paste(readLines(log_path), collapse = "\n")
  expect_match(log_text, "Figure 2 \"Kapot \\[data.csv\\]\": skipped \\(error: .*not a column")
})

test_that("cpb_import_csv errors clearly when params.csv is missing", {
  expect_error(cpb_import_csv("nope.csv"), "not found")
})

test_that("cpb_import_csv applies ... overrides on top of params.csv", {
  params_csv <- local_figs(c(
    "plot_type,line", "x,jaar", "y,mld", "colour,soort", "legend,bottom"
  ))

  p <- cpb_import_csv(params_csv, legend = "right")

  expect_equal(p$theme$legend.position, "right")
})

test_that("autogenerate_plots copies every kit file into a new folder", {
  dest <- withr::local_tempdir()
  autogenerate_plots(dest)

  kit_dir <- system.file("autogenerate_plots", package = "ggcpb")
  expect_true(dir.exists(kit_dir))
  expect_setequal(list.files(dest), list.files(kit_dir))

  # the example datasets ship ready to run as-is
  expect_true(file.exists(file.path(dest, "data", "koopkracht.csv")))
  p <- cpb_import_csv(params_csv = file.path(dest, "params.csv"))
  expect_type(p, "list")
})

test_that("autogenerate_plots keeps run_import.command executable", {
  # run_import.command is a Mac-only double-click start script, and the
  # executable bit this checks is a POSIX permission concept Windows
  # doesn't have -- file.info()$mode reads back something else there
  # regardless of what Sys.chmod() was asked to do, which isn't a real
  # failure, just this check not applying on that platform
  skip_on_os("windows")

  dest <- withr::local_tempdir()
  autogenerate_plots(dest)

  command_file <- file.path(dest, "run_import.command")
  expect_true(file.exists(command_file))
  expect_equal(as.character(file.info(command_file)$mode), "755")
})

test_that("autogenerate_plots refuses to overwrite without being asked to", {
  dest <- withr::local_tempdir()
  autogenerate_plots(dest)

  expect_error(autogenerate_plots(dest), "already exist")
  expect_no_error(autogenerate_plots(dest, overwrite = TRUE))
})

test_that("autogenerate_plots creates the destination folder if needed", {
  dest <- file.path(withr::local_tempdir(), "nieuwe_map")
  expect_false(dir.exists(dest))

  autogenerate_plots(dest)

  expect_true(file.exists(file.path(dest, "run_import.R")))
})

test_that("cpb_import_csv errors clearly when the dataset has no header row", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("2020,A,2.0", "2021,B,3.0"))

  expect_error(suppressWarnings(cpb_import_csv(params_csv)), "does not look like column names")
})

test_that("cpb_import_csv errors clearly on a stray line above the dataset's header", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("Dit is een titel regel", "jaar,groep,koopkracht",
                                          "2020,A,2.0", "2021,B,3.0"))

  expect_error(suppressWarnings(cpb_import_csv(params_csv)), "no title, no blank line")
})

test_that("cpb_import_csv errors clearly when the dataset uses tabs but sep is a comma", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("jaar\tgroep\tkoopkracht", "2020\tA\t2.0", "2021\tB\t3.0"))

  expect_error(suppressWarnings(cpb_import_csv(params_csv)), "might use a tab instead")
})

test_that("cpb_import_csv errors clearly when params_csv uses tabs but sep is a comma", {
  params_csv <- local_figs(c("plot_type\tline", "x\tjaar", "y\tmld"))

  expect_error(cpb_import_csv(params_csv), "might use a tab instead")
})

test_that("cpb_import_csv reads both files fine when sep matches a tab-separated pair", {
  params_csv <- local_figs(c("plot_type\tline", "x\tjaar", "y\tkoopkracht"),
                           data_lines = c("jaar\tgroep\tkoopkracht", "2020\tA\t2.0", "2021\tB\t3.0"))

  expect_no_error(cpb_import_csv(params_csv, sep = "\t"))
})

# --- several datasets: params.csv plus a data/ folder -------------------

local_kit <- function(params, datasets = c("koopkracht.csv", "werkloosheid.csv", "data.csv")) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data"))
  for (f in datasets) {
    writeLines(c(
      "jaar,groep,waarde",
      "2020,A,2.0", "2020,B,3.0", "2021,A,2.5", "2021,B,3.2"
    ), file.path(dir, "data", f))
  }
  writeLines(params, file.path(dir, "params.csv"))
  dir
}

test_that("each figure reads the dataset named in `data`, and is named after it", {
  dir <- local_kit(c(
    "plot_type,line,line",
    "id,a,b",
    "x,jaar,jaar",
    "y,waarde,waarde",
    "data,koopkracht.csv,werkloosheid.csv"
  ))
  ps <- cpb_import_csv(params_csv = file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht", "werkloosheid"))
})

test_that("a blank or absent `data` reads data/data.csv", {
  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde", "data,"))
  p <- cpb_import_csv(params_csv = file.path(dir, "params.csv"))
  expect_s3_class(p, "ggplot")

  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde"))
  ps <- cpb_import_csv(params_csv = file.path(dir, "params.csv"), simplify = FALSE)
  expect_named(ps, "data")
})

test_that("a missing data/data.csv skips the figure with a warning, never another dataset", {
  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde"), datasets = "koopkracht.csv")
  expect_error(
    suppressWarnings(cpb_import_csv(params_csv = file.path(dir, "params.csv"))),
    "dataset not found"
  )
  expect_warning(
    try(cpb_import_csv(params_csv = file.path(dir, "params.csv")), silent = TRUE),
    "dataset not found"
  )
})

test_that("a list in `data` builds one figure per file, and one missing file does not cancel the rest", {
  dir <- local_kit(
    c(
      "plot_type,line,line",
      "id,a,later",
      "x,jaar,jaar",
      "y,waarde,waarde",
      "data,a.csv;b.csv;c.csv,d.csv"
    ),
    datasets = c("a.csv", "c.csv", "d.csv")
  )
  params_csv <- file.path(dir, "params.csv")
  expect_warning(
    ps <- cpb_import_csv(params_csv = params_csv),
    "b.csv"
  )
  expect_named(ps, c("a", "c", "d"))
  log <- paste(readLines(file.path(dir, "params_log.txt")), collapse = "\n")
  expect_match(log, "skipped \\(dataset not found")
})

test_that("a dataset used by several figures gets the figure number in its name", {
  dir <- local_kit(c(
    "plot_type,line,line,col",
    "id,a,b,c",
    "x,jaar,jaar,jaar",
    "y,waarde,waarde,waarde",
    "data,koopkracht.csv,werkloosheid.csv,koopkracht.csv"
  ))
  ps <- cpb_import_csv(params_csv = file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht_1", "werkloosheid", "koopkracht_3"))
})

test_that("other CSV files in data/ are ignored unless named", {
  dir <- local_kit(
    c("plot_type,line", "x,jaar", "y,waarde", "data,koopkracht.csv"),
    datasets = c("koopkracht.csv", "backup.csv")
  )
  p <- cpb_import_csv(params_csv = file.path(dir, "params.csv"))
  expect_s3_class(p, "ggplot")
})

test_that("running the import twice replaces the PNGs instead of duplicating them", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  autogenerate_plots(dir)
  png_files <- function() list.files(file.path(dir, "generated"), pattern = "[.]png$")

  utils::capture.output(cpb_import_run(dir))
  first <- png_files()
  expect_gt(length(first), 0)
  old <- file.info(file.path(dir, "generated", first))$mtime

  Sys.sleep(1.1)
  utils::capture.output(cpb_import_run(dir))
  expect_equal(png_files(), first)
  new <- file.info(file.path(dir, "generated", first))$mtime
  expect_true(all(new > old))
})

test_that("cpb_import_run lists skipped figures and still saves the others", {
  dir <- local_kit(
    c("plot_type,line", "id,a", "x,jaar", "y,waarde", "data,a.csv;b.csv"),
    datasets = "a.csv"
  )
  out <- utils::capture.output(cpb_import_run(dir))
  expect_true(file.exists(file.path(dir, "generated", "a.png")))
  expect_match(paste(out, collapse = "\n"), "b.csv")
})

test_that("text columns keep the order of the rows, and a numeric fill becomes a category", {
  params_csv <- local_figs(
    c("plot_type,col", "x,groep", "y,waarde", "fill,jaar"),
    data_lines = c("groep,jaar,waarde",
                   "zeta,2024,1", "alfa,2024,2", "zeta,2021,3", "alfa,2021,4")
  )
  p <- cpb_import_csv(params_csv)

  expect_equal(levels(p$data$groep), c("zeta", "alfa"))
  expect_s3_class(p$data$jaar, "factor")
  expect_equal(levels(p$data$jaar), c("2024", "2021"))
})

test_that("a numeric scatter colour stays a number", {
  params_csv <- local_figs(c("plot_type,scatter", "x,jaar", "y,mld", "colour,heffing"))
  p <- cpb_import_csv(params_csv)

  expect_true(is.numeric(p$data$heffing))
})

test_that("the .csv extension in `data` is optional", {
  dir <- local_kit(c(
    "plot_type,line,line,line",
    "id,a,b,c",
    "x,jaar,jaar,jaar",
    "y,waarde,waarde,waarde",
    "data,koopkracht,werkloosheid.csv,koopkracht.CSV;werkloosheid"
  ))
  ps <- cpb_import_csv(file.path(dir, "params.csv"))
  # `koopkracht` and `koopkracht.CSV` are the same dataset, so it is shared
  expect_named(ps, c("koopkracht_1", "werkloosheid_2", "koopkracht_3", "werkloosheid_3"))
})

test_that("figures keep the number of their column, and an empty column is not a figure", {
  dir <- local_kit(c(
    "plot_type,line,,col",
    "id,a,,c",
    "x,jaar,,jaar",
    "y,waarde,,waarde",
    "data,koopkracht,,koopkracht"
  ))
  ps <- cpb_import_csv(file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht_1", "koopkracht_3"))
})

test_that("rows may stop early, and a later row may be wider than the first ones", {
  dir <- local_kit(c(
    "plot_type,line",
    "x,jaar",
    "y,waarde",
    "data,koopkracht",
    "ylab,%,%",
    "title,Eerste,Tweede"
  ))
  # the 5th and 6th rows have a second value, but no figure there has a plot_type
  ps <- suppressWarnings(cpb_import_csv(file.path(dir, "params.csv")))
  expect_s3_class(ps, "ggplot")
  expect_equal(ps$labels$title, "Eerste")
})

test_that("a params.csv without a plot_type row is refused", {
  dir <- local_kit(c("x,jaar", "y,waarde"))
  expect_error(cpb_import_csv(file.path(dir, "params.csv")), "plot_type")
})
