# test-import-params.R ----

# A figure folder with the given datasets (name = lines) and params.csv.
# Returns the path of params.csv.
local_folder <- function(params, datasets) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "data"))
  for (nm in names(datasets)) {
    writeLines(datasets[[nm]], file.path(dir, "data", paste0(nm, ".csv")))
  }
  writeLines(params, file.path(dir, "params.csv"))
  file.path(dir, "params.csv")
}

series <- c("jaar,groep,waarde", "2020,A,2", "2020,B,3", "2021,A,4", "2021,B,1")
quantiles <- c("groep,p5,p25,p50,p75,p95", "A,1,2,3,4,5", "B,2,3,4,5,6")
estimates <- c("term,coef,lo,hi", "a,1,0,2", "b,-1,-2,0")
shares <- c("bron,aandeel", "gas,60", "warmte,40")

test_that("every plot type can be built from params.csv", {
  provinces <- unique(cpb_nl_geo("provincie")$name)
  map_data <- c("naam,waarde",
                paste0(provinces, ",", seq_along(provinces)))
  params_csv <- local_folder(c(
    paste0("plot_type,col,area,line,box,dot,scatter,hist,donut,map,",
           "boxplot_extended"),
    "data,series,series,series,box,dot,series,series,donut,map,box",
    "x,jaar,jaar,jaar,groep,term,jaar,waarde,,,groep",
    "y,waarde,waarde,waarde,,coef,waarde,,aandeel",
    "fill,groep,groep,,,,,,bron",
    "colour,,,groep",
    "p5,,,,p5,,,,,,p5", "p25,,,,p25,,,,,,p25", "p50,,,,p50,,,,,,p50",
    "p75,,,,p75,,,,,,p75", "p95,,,,p95,,,,,,p95",
    "lower,,,,,lo", "upper,,,,,hi",
    "region,,,,,,,,,naam",
    "value,,,,,,,,,waarde",
    "level,,,,,,,,,provincie"
  ), list(series = series, box = quantiles, dot = estimates, donut = shares,
          map = map_data))

  expect_no_warning(ps <- cpb_import_csv(params_csv))
  types <- c("col", "area", "line", "box", "dot", "scatter", "hist",
             "donut", "map", "boxplot_extended")
  expect_length(ps, length(types))
  for (p in ps) expect_s3_class(p, "ggplot")
  for (p in ps) expect_no_error(ggplot2::ggplot_build(p))
})

test_that("cut puts numbers into classes with Dutch labels", {
  params_csv <- local_folder(c(
    "plot_type,scatter",
    "x,jaar", "y,waarde", "colour,waarde",
    "cut,-Inf;2;3;Inf",
    "cut_labels,pct"
  ), list(data = series))
  p <- cpb_import_csv(params_csv)
  # the numbers stay on the y axis, the classes set the colours
  expect_true(is.numeric(p$data$waarde))
  classes <- p$data$.cpb_classes_waarde
  expect_equal(levels(classes),
               c("lager dan 2%", "2% - 3%", "3% en hoger"))
})

test_that("cut labels follow the style of the figure", {
  params_csv <- local_folder(c(
    "plot_type,scatter",
    "x,jaar", "y,waarde", "colour,waarde",
    "cut,-Inf;2.5;Inf",
    "style,english"
  ), list(data = series))
  p <- cpb_import_csv(params_csv)
  expect_equal(levels(p$data$.cpb_classes_waarde),
               c("below 2.5", "2.5 and above"))
})

test_that("cut needs the column that sets the colours, and known labels", {
  params_csv <- local_folder(c("plot_type,col", "x,jaar", "y,waarde",
                               "cut,0;2;Inf"),
                             list(data = series))
  expect_warning(
    expect_error(cpb_import_csv(params_csv), "nothing built|No figures"),
    "needs a `fill` column"
  )
  params_csv <- local_folder(c("plot_type,col", "x,jaar", "y,waarde",
                               "fill,waarde", "cut,0;2;Inf",
                               "cut_labels,dollar"),
                             list(data = series))
  expect_warning(try(cpb_import_csv(params_csv), silent = TRUE),
                 "number, euro or pct")
})

test_that("x_labels can name a formatter", {
  params_csv <- local_folder(c("plot_type,scatter", "x,inkomen", "y,y",
                               "x_labels,euro"),
                             list(data = c("inkomen,y", "1000,1", "3000,2")))
  p <- cpb_import_csv(params_csv)
  labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_true(all(grepl("€", stats::na.omit(labels))))
})

test_that("a column of dates becomes a Date column, ISO or day-month-year", {
  params_csv <- local_folder(c(
    "plot_type,line,line",
    "data,iso,dmy",
    "x,maand,maand", "y,waarde,waarde",
    "x_lim,2024-02-01;2024-03-01"
  ), list(
    iso = c("maand,waarde", "2024-01-01,1", "2024-02-01,2", "2024-03-01,3"),
    dmy = c("maand,waarde", "1-1-2024,1", "1-2-2024,2", "1-3-2024,3")
  ))
  ps <- cpb_import_csv(params_csv)
  expect_s3_class(ps$iso$data$maand, "Date")
  expect_s3_class(ps$dmy$data$maand, "Date")
  expect_equal(ps$dmy$data$maand[2], as.Date("2024-02-01"))
})

test_that("text that only looks like a date stays text", {
  params_csv <- local_folder(c("plot_type,col", "x,code", "y,waarde"),
                             list(data = c("code,waarde", "2024-13-45,1",
                                           "2024-01-01,2")))
  p <- cpb_import_csv(params_csv)
  expect_s3_class(p$data$code, "factor")
})

test_that("a written \\n is a line break, and Inf is a number", {
  params_csv <- local_folder(c("plot_type,line", "x,jaar", "y,waarde",
                               "title,Eerste regel\\ntweede regel"),
                             list(data = series))
  p <- cpb_import_csv(params_csv)
  expect_equal(p$labels$title, "Eerste regel\ntweede regel")
  expect_identical(cpb_import_coerce("-Inf;0;Inf"), c(-Inf, 0, Inf))
  expect_identical(cpb_import_coerce("1.5e2"), 150)
})

test_that("dot_labels takes name:value pairs", {
  params_csv <- local_folder(c(
    "plot_type,box", "x,groep",
    "p5,p5", "p25,p25", "p50,p50", "p75,p75", "p95,p95",
    "box_style,dot",
    "dot_labels,p5:onderste 5%;p95:bovenste 5%"
  ), list(data = quantiles))
  expect_no_error(p <- cpb_import_csv(params_csv))
  expect_s3_class(p, "ggplot")
  expect_identical(cpb_import_named("p5:a;p95:b"), c(p5 = "a", p95 = "b"))
  expect_error(cpb_import_named("p5;p95:b"), "name:value")
})

test_that("height, preset, dpi and lock say how a figure is saved", {
  params_csv <- local_folder(c(
    "plot_type,line,line",
    "data,a,b",
    "x,jaar,jaar", "y,waarde,waarde",
    "height,10",
    "dpi,,150",
    "lock,,FALSE"
  ), list(a = series, b = series))
  ps <- cpb_import_csv(params_csv)
  expect_equal(attr(ps$a, "cpb_import_save")$height, 10 / 2.54)
  expect_equal(attr(ps$b, "cpb_import_save")$dpi, 150)
  expect_false(attr(ps$b, "cpb_import_save")$lock)

  utils::capture.output(cpb_import_run(dirname(params_csv)))
  png_dim <- function(f) {
    dim(png::readPNG(file.path(dirname(params_csv), "generated", f)))
  }
  # 10 cm at 300 dpi, and the default width at 150 dpi
  expect_equal(png_dim("a.png")[1], round(10 / 2.54 * 300), tolerance = 2)
  expect_equal(png_dim("b.png")[2], round(7.5 / 2.54 * 150), tolerance = 2)
})

test_that("an invalid save parameter skips the figure with a warning", {
  params_csv <- local_folder(c("plot_type,line,line", "data,a,b",
                               "x,jaar,jaar", "y,waarde,waarde",
                               "preset,poster", "height,,-3"),
                             list(a = series, b = series))
  w <- character(0)
  withCallingHandlers(
    try(cpb_import_csv(params_csv), silent = TRUE),
    warning = function(cnd) {
      w <<- c(w, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("report, presentation or map", w)))
  expect_true(any(grepl("one positive number", w)))
})

test_that("cpb_import_run saves the other figures when one fails to draw", {
  # two boxes on one category draw an error only when the figure is drawn
  params_csv <- local_folder(c(
    "plot_type,boxplot_extended,line",
    "data,broken,ok",
    "x,groep,jaar", "y,,waarde",
    "p5,p5", "p25,p25", "p50,p50", "p75,p75", "p95,p95"
  ), list(broken = c(quantiles, "A,1,2,3,4,5"), ok = series))
  out <- utils::capture.output(cpb_import_run(dirname(params_csv)))
  generated <- file.path(dirname(params_csv), "generated")
  expect_true(file.exists(file.path(generated, "ok.png")))
  expect_false(file.exists(file.path(generated, "broken.png")))
  expect_match(paste(out, collapse = "\n"), "not saved")
  log <- readLines(file.path(dirname(params_csv), "params_log.txt"))
  expect_match(paste(log, collapse = "\n"), "broken.*not saved")
})

test_that("the parameter reference lists every argument of every plot type", {
  ref <- utils::read.csv(
    system.file("extdata", "parameter_settings.csv", package = "ggcpb"),
    stringsAsFactors = FALSE
  )
  for (pt in names(cpb_import_col_args)) {
    args <- setdiff(names(formals(get(paste0("cpb_", pt)))), "...")
    listed <- ref$param[ref$plot_type == pt]
    expect_setequal(setdiff(listed, c(cpb_import_save_args, "cut",
                                      "cut_labels")), args)
  }
})
