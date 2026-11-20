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

test_that("figures_from_csv builds one figure from a one column params.csv",
          {
            params_csv <- local_figs(c(
              "plot_type,line",
              "title,Testfiguur",
              "x,jaar",
              "y,mld",
              "colour,soort",
              "ylab,mld euro"
            ))

            p <- figures_from_csv(params_csv)

            expect_s3_class(p, "ggplot")
            expect_true(inherits(p$layers[[1]]$geom, "GeomLine"))
            expect_equal(p$labels$title, "Testfiguur")
            expect_true(!is.null(p$scales$get_scales("colour")))
          })

test_that("figures_from_csv builds several figures, skipping create = n", {
  params_csv <- local_figs(c(
    "plot_type,col,col,line",
    "title,Eerste,Overgeslagen,Tweede",
    "x,jaar,jaar,jaar",
    "y,mld,mld,mld",
    "create,y,n,yes"
  ))

  ps <- figures_from_csv(params_csv)

  expect_type(ps, "list")
  expect_length(ps, 2)
  # one dataset, two figures: the figure number is added to the name
  expect_named(ps, c("data_1", "data_3"))
  expect_true(inherits(ps$data_1$layers[[1]]$geom, "GeomCol"))
  expect_true(inherits(ps$data_3$layers[[1]]$geom, "GeomLine"))
})

test_that("figures_from_csv resolves sec_y and passes sec_type through", {
  params_csv <- local_figs(c(
    "plot_type,col",
    "x,jaar",
    "y,mld",
    "sec_y,heffing",
    "sec_type,point"
  ))

  p <- figures_from_csv(params_csv)

  is_point <- vapply(p$layers,
                     function(l) inherits(l$geom, "GeomPoint"), logical(1))
  expect_true(any(is_point))
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomLine"),
                          logical(1))))
})

test_that("figures_from_csv splits a semicolon-delimited value into a vector", {
  params_csv <- local_figs(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "colour,soort",
    "colour_index,6;2"
  ))

  expect_no_error(figures_from_csv(params_csv))
})

test_that("figures_from_csv drops unknown parameter names with a warning", {
  params_csv <- local_figs(c(
    "plot_type,line",
    "x,jaar",
    "y,mld",
    "this_is_not_a_param,banaan"
  ))

  expect_no_error(suppressWarnings(figures_from_csv(params_csv)))
  expect_warning(figures_from_csv(params_csv), "this_is_not_a_param")
})

test_that("figures_from_csv always (over)writes a run log next to params_csv", {
  params_csv <- local_figs(c(
    "plot_type,col,col",
    "title,Eerste,Overgeslagen",
    "x,jaar,jaar",
    "y,mld,mld",
    "create,y,n"
  ))
  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")

  figures_from_csv(params_csv)

  expect_true(file.exists(log_path))
  log_text <- paste(readLines(log_path), collapse = "\n")
  expect_match(log_text, "Figure 1 \"Eerste \\[data.csv\\]\"")
  expect_match(log_text, "skipped \\(create = n\\)")
  expect_match(log_text, "2 figure\\(s\\) read, 1 skipped, 1 built")
})

test_that("figures_from_csv skips a bad figure, errors if none is built", {
  # a single-figure params.csv where that one figure is unbuildable: a
  # console warning() for the specific reason, and -- since nothing at
  # all was built -- a top-level error, not a silently empty result
  params_csv <- local_figs(c("plot_type,pie", "x,jaar", "y,mld"))
  expect_warning(
    expect_error(figures_from_csv(params_csv), "Unknown plot_type"),
    "Unknown plot_type"
  )

  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,doesnotexist"))
  expect_warning(
    expect_error(figures_from_csv(params_csv), "not a column in the data"),
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
  expect_warning(figures_from_csv(params_csv), "not a column in the data")
  ps <- suppressWarnings(figures_from_csv(params_csv))
  expect_s3_class(ps, "ggplot") # only one figure survived, so it is not a list
  expect_equal(ps$labels$title, "Goed")

  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")
  log_text <- paste(readLines(log_path), collapse = "\n")
  expect_match(log_text, paste0("Figure 2 \"Kapot \\[data.csv\\]\": ",
                                "skipped \\(error: .*not a column"))
})

test_that("figures_from_csv errors clearly when params.csv is missing", {
  expect_error(figures_from_csv("nope.csv"), "not found")
})

test_that("figures_from_csv applies ... overrides on top of params.csv", {
  params_csv <- local_figs(c(
    "plot_type,line", "x,jaar", "y,mld", "colour,soort", "legend,bottom"
  ))

  p <- figures_from_csv(params_csv, legend = "right")

  expect_equal(p$theme$legend.position, "right")
})

test_that("the example folder ships ready to run", {
  kit_dir <- system.file("examples", package = "ggcpb")
  expect_true(dir.exists(kit_dir))
  kit_files <- c("params.csv", "generate_plot.R", "generate_plot.bat")
  expect_true(all(file.exists(file.path(kit_dir, kit_files))))
  expect_true(file.exists(file.path(kit_dir, "data", "koopkracht.csv")))

  p <- figures_from_csv(params_csv = file.path(kit_dir, "params.csv"))
  expect_type(p, "list")
})

test_that("figures_from_csv errors when the dataset has no header row", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("2020,A,2.0", "2021,B,3.0"))

  expect_error(suppressWarnings(figures_from_csv(params_csv)),
               "does not look like column names")
})

test_that("figures_from_csv errors on a stray line above the header", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("Dit is een titel regel",
                                          "jaar,groep,koopkracht",
                                          "2020,A,2.0", "2021,B,3.0"))

  expect_error(suppressWarnings(figures_from_csv(params_csv)),
               "no title, no blank line")
})

test_that("figures_from_csv errors on a tab dataset with a comma sep", {
  params_csv <- local_figs(c("plot_type,line", "x,jaar", "y,koopkracht"),
                           data_lines = c("jaar\tgroep\tkoopkracht",
                                          "2020\tA\t2.0", "2021\tB\t3.0"))

  expect_error(suppressWarnings(figures_from_csv(params_csv)),
               "might use a tab instead")
})

test_that("figures_from_csv errors on a tab params file with a comma sep", {
  params_csv <- local_figs(c("plot_type\tline", "x\tjaar", "y\tmld"))

  expect_error(figures_from_csv(params_csv), "might use a tab instead")
})

test_that("figures_from_csv reads both files when sep matches a tab pair", {
  params_csv <- local_figs(c("plot_type\tline", "x\tjaar", "y\tkoopkracht"),
                           data_lines = c("jaar\tgroep\tkoopkracht",
                                          "2020\tA\t2.0", "2021\tB\t3.0"))

  expect_no_error(figures_from_csv(params_csv, sep = "\t"))
})

# --- several datasets: params.csv plus a data/ folder -------------------

local_kit <- function(params,
                      datasets = c("koopkracht.csv", "werkloosheid.csv",
                                   "data.csv")) {
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

test_that("each figure reads the dataset named in `data`", {
  dir <- local_kit(c(
    "plot_type,line,line",
    "id,a,b",
    "x,jaar,jaar",
    "y,waarde,waarde",
    "data,koopkracht.csv,werkloosheid.csv"
  ))
  ps <- figures_from_csv(params_csv = file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht", "werkloosheid"))
})

test_that("a blank or absent `data` reads data/data.csv", {
  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde", "data,"))
  p <- figures_from_csv(params_csv = file.path(dir, "params.csv"))
  expect_s3_class(p, "ggplot")

  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde"))
  ps <- figures_from_csv(params_csv = file.path(dir, "params.csv"),
                         simplify = FALSE)
  expect_named(ps, "data")
})

test_that("a missing data/data.csv skips the figure, no other dataset", {
  dir <- local_kit(c("plot_type,line", "x,jaar", "y,waarde"),
                   datasets = "koopkracht.csv")
  expect_error(
    suppressWarnings(
      figures_from_csv(params_csv = file.path(dir, "params.csv"))
    ),
    "dataset not found"
  )
  expect_warning(
    try(figures_from_csv(params_csv = file.path(dir, "params.csv")),
        silent = TRUE),
    "dataset not found"
  )
})

test_that("a `data` list builds a figure per file, skips a missing one", {
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
    ps <- figures_from_csv(params_csv = params_csv),
    "b.csv"
  )
  expect_named(ps, c("a", "c", "d"))
  log <- paste(readLines(file.path(dir, "params_log.txt")), collapse = "\n")
  expect_match(log, "skipped \\(dataset not found")
})

test_that("a dataset used by several figures gets the figure number", {
  dir <- local_kit(c(
    "plot_type,line,line,col",
    "id,a,b,c",
    "x,jaar,jaar,jaar",
    "y,waarde,waarde,waarde",
    "data,koopkracht.csv,werkloosheid.csv,koopkracht.csv"
  ))
  ps <- figures_from_csv(params_csv = file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht_1", "werkloosheid", "koopkracht_3"))
})

test_that("other CSV files in data/ are ignored unless named", {
  dir <- local_kit(
    c("plot_type,line", "x,jaar", "y,waarde", "data,koopkracht.csv"),
    datasets = c("koopkracht.csv", "backup.csv")
  )
  p <- figures_from_csv(params_csv = file.path(dir, "params.csv"))
  expect_s3_class(p, "ggplot")
})

test_that("running the import twice replaces the PNGs", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  file.copy(list.files(system.file("examples",
                                   package = "ggcpb"), full.names = TRUE),
            dir, recursive = TRUE)
  png_files <- function() {
    list.files(file.path(dir, "generated"),
               pattern = "[.]png$")
  }

  utils::capture.output(generate_figures_with_excel(dir))
  first <- png_files()
  expect_gt(length(first), 0)
  old <- file.info(file.path(dir, "generated", first))$mtime

  Sys.sleep(1.1)
  utils::capture.output(generate_figures_with_excel(dir))
  expect_equal(png_files(), first)
  new <- file.info(file.path(dir, "generated", first))$mtime
  expect_true(all(new > old))
})

test_that("generate_figures_with_excel lists skipped figures, saves others", {
  dir <- local_kit(
    c("plot_type,line", "id,a", "x,jaar", "y,waarde", "data,a.csv;b.csv"),
    datasets = "a.csv"
  )
  out <- utils::capture.output(generate_figures_with_excel(dir))
  expect_true(file.exists(file.path(dir, "generated", "a.png")))
  expect_match(paste(out, collapse = "\n"), "b.csv")
})

test_that("text columns keep the row order, a numeric fill is a category", {
  params_csv <- local_figs(
    c("plot_type,col", "x,groep", "y,waarde", "fill,jaar"),
    data_lines = c("groep,jaar,waarde",
                   "zeta,2024,1", "alfa,2024,2", "zeta,2021,3", "alfa,2021,4")
  )
  p <- figures_from_csv(params_csv)

  expect_equal(levels(p$data$groep), c("zeta", "alfa"))
  expect_s3_class(p$data$jaar, "factor")
  expect_equal(levels(p$data$jaar), c("2024", "2021"))
})

test_that("a numeric scatter colour stays a number", {
  params_csv <- local_figs(c("plot_type,scatter", "x,jaar", "y,mld",
                             "colour,heffing"))
  p <- figures_from_csv(params_csv)

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
  ps <- figures_from_csv(file.path(dir, "params.csv"))
  # `koopkracht` and `koopkracht.CSV` are the same dataset, so it is shared
  expect_named(ps, c("koopkracht_1", "werkloosheid_2", "koopkracht_3",
                     "werkloosheid_3"))
})

test_that("figures keep their column number, an empty column is no figure", {
  dir <- local_kit(c(
    "plot_type,line,,col",
    "id,a,,c",
    "x,jaar,,jaar",
    "y,waarde,,waarde",
    "data,koopkracht,,koopkracht"
  ))
  ps <- figures_from_csv(file.path(dir, "params.csv"))
  expect_named(ps, c("koopkracht_1", "koopkracht_3"))
})

test_that("rows may stop early, a later row may be wider", {
  dir <- local_kit(c(
    "plot_type,line",
    "x,jaar",
    "y,waarde",
    "data,koopkracht",
    "ylab,%,%",
    "title,Eerste,Tweede"
  ))
  # the 5th and 6th rows have a second value, but no figure there has a
  # plot_type
  ps <- suppressWarnings(figures_from_csv(file.path(dir, "params.csv")))
  expect_s3_class(ps, "ggplot")
  expect_equal(ps$labels$title, "Eerste")
})

test_that("a params.csv without a plot_type row is refused", {
  dir <- local_kit(c("x,jaar", "y,waarde"))
  expect_error(figures_from_csv(file.path(dir, "params.csv")), "plot_type")
})

test_that("`page` sets the width a figure is saved at, half by default", {
  skip_if_not_installed("png")
  dir <- local_kit(c(
    "plot_type,line,line",
    "x,jaar,jaar",
    "y,waarde,waarde",
    "data,koopkracht,werkloosheid",
    "page,,full"
  ))
  expect_no_warning(ps <- figures_from_csv(file.path(dir, "params.csv")))
  expect_null(attr(ps$koopkracht, "cpb_import_save")$page)
  expect_equal(attr(ps$werkloosheid, "cpb_import_save")$page, "full")

  utils::capture.output(generate_figures_with_excel(dir))
  px <- function(f) dim(png::readPNG(file.path(dir, "generated", f)))[2]
  expect_gt(px("werkloosheid.png"), 1.5 * px("koopkracht.png"))
})

test_that("an invalid `page` skips the figure with a warning", {
  params_csv <- local_figs(c(
    "plot_type,line,line",
    "x,jaar,jaar",
    "y,mld,mld",
    "page,huge,"
  ))
  expect_warning(ps <- figures_from_csv(params_csv), "half, full or small")
  expect_s3_class(ps, "ggplot")
})

# the column `general_settings` ----

general_lines <- c(
  "plot_type,,line,line,line",
  "id,general_settings,a,b,c",
  "x,jaar,jaar,jaar,jaar",
  "y,waarde,waarde,waarde,waarde",
  "data,koopkracht,,werkloosheid,",
  "legend,right,,,none",
  "page,full,,half,"
)
general_data <- c("koopkracht.csv", "werkloosheid.csv")

test_that("a figure takes the general settings, and its own value wins", {
  dir <- local_kit(general_lines, datasets = general_data)
  ps <- figures_from_csv(file.path(dir, "params.csv"))

  # the general column is not a figure, and the plot type is its own
  expect_length(ps, 3)
  # blank cells take the general value, own cells win
  expect_equal(ps[[1]]$theme$legend.position, "right")
  expect_equal(ps[[3]]$theme$legend.position, "none")
  expect_equal(attr(ps[[1]], "cpb_import_save")$page, "full")
  expect_equal(attr(ps[[2]], "cpb_import_save")$page, "half")
  # the data of figure b comes from its own cell
  expect_named(ps, c("koopkracht_1", "werkloosheid", "koopkracht_3"))
})

test_that("figure numbers do not count the general column", {
  dir <- local_kit(general_lines, datasets = general_data)
  ps <- figures_from_csv(file.path(dir, "params.csv"))
  # a and c share a dataset, so they carry their figure number 1 and 3
  expect_true(all(c("koopkracht_1", "koopkracht_3") %in% names(ps)))
})

test_that("id and create are not shared from the general column", {
  dir <- local_kit(c(
    "plot_type,line,line",
    "id,general_settings,a",
    "create,n,",
    "x,jaar,jaar",
    "y,waarde,waarde"
  ), datasets = "data.csv")
  ps <- figures_from_csv(file.path(dir, "params.csv"))
  expect_s3_class(ps, "ggplot")
})

test_that("an empty figure column stays empty with a general column", {
  dir <- local_kit(c(
    "plot_type,line,line,",
    "id,general_settings,a,",
    "x,jaar,jaar,",
    "y,waarde,waarde,",
    "legend,right,,"
  ), datasets = "data.csv")
  ps <- figures_from_csv(file.path(dir, "params.csv"))
  expect_s3_class(ps, "ggplot")
})

test_that("two general columns are an error", {
  dir <- local_kit(c(
    "plot_type,line,line,line",
    "id,general_settings,General_Settings,a",
    "x,jaar,jaar,jaar",
    "y,waarde,waarde,waarde"
  ), datasets = "data.csv")
  expect_error(figures_from_csv(file.path(dir, "params.csv")),
               "general_settings")
})

test_that("a params.csv with only a general column has no figures", {
  dir <- local_kit(c(
    "plot_type,line",
    "id,general_settings",
    "legend,right"
  ), datasets = "data.csv")
  expect_error(figures_from_csv(file.path(dir, "params.csv")), "no figures")
})

test_that("the run log names the general settings", {
  dir <- local_kit(general_lines, datasets = general_data)
  figures_from_csv(file.path(dir, "params.csv"))
  log <- readLines(file.path(dir, "params_log.txt"))
  expect_true(any(grepl("general settings: .*legend", log)))
})

test_that("generate_figures_with_excel saves with the general page", {
  skip_on_cran()
  dir <- local_kit(general_lines, datasets = general_data)
  utils::capture.output(paths <- generate_figures_with_excel(dir))
  width <- function(f) dim(png::readPNG(f))[2]
  # figure a is full (15.5 cm), figure b is half (7.5 cm)
  expect_gt(width(paths[1]), width(paths[2]))
})

# the clear messages

# runs the import, the error of "nothing built" is not the point here
run_quiet <- function(dir) {
  try(suppressWarnings(figures_from_csv(file.path(dir, "params.csv"))),
      silent = TRUE)
}

skipped_detail <- function(dir) {
  log <- readLines(file.path(dir, "params_log.txt"))
  paste(log, collapse = "\n")
}

test_that("a missing required parameter is named", {
  dir <- local_kit(c(
    "plot_type,line,col",
    "id,a,b",
    "x,jaar,jaar",
    "y,,waarde"
  ), datasets = "data.csv")
  run_quiet(dir)
  expect_match(skipped_detail(dir),
               "`y` is required for plot_type = \"line\"", fixed = TRUE)
})

test_that("an invalid general value says it comes from general_settings", {
  dir <- local_kit(c(
    "plot_type,,line,line",
    "id,general_settings,a,b",
    "x,jaar,jaar,jaar",
    "y,waarde,waarde,waarde",
    "page,huge,,half"
  ), datasets = "data.csv")
  run_quiet(dir)
  log <- skipped_detail(dir)
  expect_match(
    log,
    "`page` must be half, full or small, not \"huge\" (from general_settings)",
    fixed = TRUE
  )
  # figure b has its own page, so it is built
  expect_match(log, "1 built")
})

test_that("a wrong column from general_settings says so", {
  dir <- local_kit(c(
    "plot_type,,line,line",
    "id,general_settings,a,b",
    "x,jaar,jaar,jaar",
    "y,bestaatniet,,waarde"
  ), datasets = "data.csv")
  run_quiet(dir)
  expect_match(skipped_detail(dir),
               "not a column in the data (from general_settings)",
               fixed = TRUE)
})

test_that("a wrong value in the figure itself has no general note", {
  dir <- local_kit(c(
    "plot_type,,line",
    "id,general_settings,a",
    "x,jaar,jaar",
    "y,waarde,waarde",
    "page,,huge"
  ), datasets = "data.csv")
  run_quiet(dir)
  expect_false(grepl("from general_settings", skipped_detail(dir),
                     fixed = TRUE))
})
