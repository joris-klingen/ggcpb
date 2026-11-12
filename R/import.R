# import.R
#
# Standalone CSV import: builds ready-to-use ggplot objects from a plain
# datasets in a data folder and a params.csv (column 1 holds the parameter
# names, every other column is one figure, naming which cpb_*() wrapper
# to call and its arguments).

# which args of each wrapper are column refs (not literal values),
# keyed by plot_type (the cpb_ name without its prefix)
cpb_import_col_args <- list(
  col     = c("x", "y", "fill", "group", "facet", "sec_y"),
  area    = c("x", "y", "fill", "facet", "sec_y"),
  line    = c("x", "y", "colour", "ymin", "ymax", "facet", "sec_y"),
  box     = c("x", "p5", "p25", "p50", "p75", "p95", "mean", "fill", "group",
              "facet", "sec_y"),
  dot     = c("x", "y", "lower", "upper", "colour", "group", "facet", "sec_y"),
  scatter = c("x", "y", "colour", "facet"),
  hist    = c("x", "fill", "facet"),
  donut   = c("fill", "y", "label"),
  map     = c("region", "value"),
  boxplot_extended = c("x", "p5", "p25", "p50", "p75", "p95", "mean",
                       "fill", "group", "facet", "sec_y")
)

# Parameters read by the import itself, not passed to the wrapper: the
# ones that say how a figure is saved, and the classes of `cut`.
cpb_import_save_args <- c("page", "height", "preset", "dpi", "lock")

# The column that `cut` puts into classes, per plot_type.
cpb_import_cut_target <- function(plot_type) {
  switch(plot_type,
    map = "value",
    scatter = , line = , dot = "colour",
    "fill"
  )
}

# Formatters a CSV cell can name, for parameters that take a labelling
# function (x_labels, sec_axis_labels) and for the labels of `cut`.
cpb_import_formatter <- function(name, style) {
  switch(tolower(trimws(name)),
    number = label_number_nl(style = style),
    euro = label_euro_nl(style = style),
    pct = , percent = label_pct_nl(style = style),
    NULL
  )
}
cpb_import_formatter_args <- c("x_labels", "sec_axis_labels")

# One string value from a CSV cell -> an R value. A value containing ";"
# becomes a vector, each piece coerced the same way (so "6;2" becomes the
# numeric vector c(6, 2), matching e.g. colour_index = c(6, 2)). Otherwise: a
# plain number if it looks like one (Inf and -Inf included), TRUE/FALSE if
# it reads as one, else the string, with a written "\\n" as a line break.
cpb_import_coerce_scalar <- function(x) {
  x <- trimws(x)
  number <- "^-?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][-+]?[0-9]+)?$"
  if (grepl(number, x) || x %in% c("Inf", "-Inf")) {
    return(as.numeric(x))
  }
  if (tolower(x) %in% c("true", "false")) {
    return(as.logical(x))
  }
  gsub("\\n", "\n", x, fixed = TRUE)
}

# "p5:onderste 5%;p95:bovenste 5%" -> c(p5 = "onderste 5%", ...), for the
# parameters that take a named vector
cpb_import_named <- function(value) {
  parts <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
  keys <- trimws(sub(":.*$", "", parts))
  vals <- trimws(sub("^[^:]*:", "", parts))
  if (!all(grepl(":", parts, fixed = TRUE))) {
    stop("write each value as name:value, separated by ;", call. = FALSE)
  }
  stats::setNames(vapply(vals, function(v) {
    as.character(cpb_import_coerce_scalar(v))
  }, character(1)), keys)
}
cpb_import_named_args <- "dot_labels"

cpb_import_coerce <- function(value) {
  if (grepl(";", value, fixed = TRUE)) {
    parts <- trimws(strsplit(value, ";", fixed = TRUE)[[1]])
    return(unlist(lapply(parts, cpb_import_coerce_scalar)))
  }
  cpb_import_coerce_scalar(value)
}

# A one-line hint appended to an error when a CSV reads as suspiciously
# few columns for `sep`: peeks the file's own first non-blank line and
# checks whether one of the *other* common separators would have split
# it into more pieces than `sep` did. Pasting tab-separated data (e.g.
# straight out of Excel) into a file still named/expected to be
# comma-separated is the recurring real case this catches -- read.csv()
# itself does not error on it at all, it just silently reads the whole
# line as one glued-together column. Returns "" (nothing to add) when
# no other candidate does better, so the caller's own error stands on
# its own.
cpb_import_sep_hint <- function(path, sep, actual_cols) {
  first_line <- tryCatch(readLines(path, n = 1, warn = FALSE),
                         error = function(e) "")
  if (!length(first_line) || !nzchar(first_line)) {
    return("")
  }
  candidates <- c("tab" = "\t", "comma" = ",", "semicolon" = ";")
  candidates <- candidates[candidates != sep]
  counts <- vapply(candidates, function(s) {
    lengths(regmatches(first_line, gregexpr(s, first_line, fixed = TRUE)))
  }, integer(1))
  best <- names(candidates)[which.max(counts)]
  if (length(best) && counts[[best]] >= actual_cols) {
    paste0(
      " Only ", actual_cols, " column(s) were found using ",
      "`sep = \"", sep, "\"`, but the first line looks like it might use a ",
      best, " instead. Pass the matching `sep` to `cpb_import_csv()`, or ",
      "resave the file using \"", sep, "\" throughout."
    )
  } else {
    ""
  }
}

# A minimal, easy to miss mistake -- no header row at all, or an extra
# line (a title, a blank line) sitting above the real one -- either
# silently eats the first data row as if it were the header (no header
# row: read.csv() does not warn, it just treats whichever row it reads
# first as the column names) or crashes with a plain base R error that
# does not say why ("more columns than column names", for an extra
# line above the header). Both are caught here and re-raised with a
# message that names the actual, likely cause instead.
cpb_import_read_data <- function(path, sep) {
  data_df <- tryCatch(
    utils::read.csv(path, check.names = FALSE,
                    stringsAsFactors = FALSE, sep = sep),
    error = function(e) {
      stop("Could not read the dataset (", conditionMessage(e),
        "). Check that ",
        "every row has the same number of fields, and that the very first ",
        "row is the header row naming each column, with nothing (no title, ",
        "no blank line) above it.",
        call. = FALSE
      )
    }
  )

  looks_numeric <- grepl("^-?[0-9]+(\\.[0-9]+)?$", trimws(names(data_df)))
  if (any(looks_numeric)) {
    stop("The first row of the dataset does not look like column names ",
      "(found ", paste0("\"", names(data_df)[looks_numeric], "\"",
                        collapse = ", "),
      "). `cpb_import_csv()` always reads the first row as the header; check ",
      "that a real header row is there, and that no other line comes ",
      "before it.", cpb_import_sep_hint(path, sep, ncol(data_df)),
      call. = FALSE
    )
  }

  if (ncol(data_df) == 1) {
    hint <- cpb_import_sep_hint(path, sep, ncol(data_df))
    if (nzchar(hint)) {
      stop("The dataset was read as a single column.", hint, call. = FALSE)
    }
  }

  # Text columns keep the order their values first appear in, so the row
  # order in the CSV is the order of the categories on the axis and in the
  # legend (read.csv would leave them to sort alphabetically). A column of
  # dates becomes a Date column first.
  data_df[] <- lapply(data_df, cpb_import_as_date)
  chr <- vapply(data_df, is.character, logical(1))
  data_df[chr] <- lapply(data_df[chr],
                         function(x) factor(x, levels = unique(x)))

  data_df
}

# A text column that holds only dates becomes a Date column: 2024-01-31 (ISO)
# or 31-1-2024 (day-month-year, as Dutch Excel writes it). Anything else is
# left as it is.
cpb_import_as_date <- function(x) {
  if (!is.character(x)) return(x)
  filled <- !is.na(x) & nzchar(trimws(x))
  v <- trimws(x[filled])
  if (!length(v)) return(x)
  fmt <- if (all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", v))) {
    "%Y-%m-%d"
  } else if (all(grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", v))) {
    "%d-%m-%Y"
  } else {
    return(x)
  }
  d <- as.Date(trimws(x), format = fmt)
  # a value that looks like a date but is none (2024-13-45) keeps the text
  if (anyNA(d[filled])) return(x)
  d
}

# Reads params.csv and returns one named list of (still all-character)
# parameters per figure, in file order. Column 1 holds the parameter names,
# one per row; every other column holds one figure's values. Blank cells
# are dropped entirely, so a missing value never shadows the wrapper's own
# default. A column without any value is returned as an empty list, so a
# figure keeps the number of its column.
#
# Rows may have fewer fields than the widest row (trailing commas can be
# left out), so the width is counted first rather than guessed from the
# first lines.
cpb_import_read_params <- function(params_csv, sep) {
  read_error <- function(e) {
    stop("Could not read `params_csv` (", conditionMessage(e), ").",
      call. = FALSE
    )
  }
  width <- tryCatch(
    max(utils::count.fields(params_csv, sep = sep, quote = "\"",
                            blank.lines.skip = TRUE), 0, na.rm = TRUE),
    error = read_error
  )
  if (width < 2) {
    stop("`params_csv` needs the parameter names in column 1 and one ",
      "column of values for every figure.",
      cpb_import_sep_hint(params_csv, sep, max(width, 1)),
      call. = FALSE
    )
  }
  raw <- tryCatch(
    utils::read.csv(params_csv,
      header = FALSE, check.names = FALSE, stringsAsFactors = FALSE,
      sep = sep, fill = TRUE, col.names = paste0("V", seq_len(width))
    ),
    error = read_error
  )

  param_names <- tolower(trimws(as.character(raw[[1]])))
  if (!"plot_type" %in% param_names) {
    stop("`params_csv` needs a `plot_type` row: put `plot_type` in column 1 ",
      "and the type of each figure (`line`, `col`, ...) next to it.",
      call. = FALSE
    )
  }
  lapply(seq_len(width - 1), function(i) {
    vals <- as.character(raw[[i + 1]])
    keep <- !is.na(vals) & nzchar(trimws(vals)) & nzchar(param_names)
    stats::setNames(as.list(vals[keep]), param_names[keep])
  })
}

# Turns one figure's (all-character) parameter list into a finished
# ggplot, by looking up its plot_type, splitting its params into column
# references (-> symbols, resolved against data_df) and literal values
# (-> coerced via cpb_import_coerce()), and calling the matching cpb_*()
# wrapper. Parameter names that aren't real arguments of that wrapper are
# dropped silently, so the wrapper's own default applies -- a typo or an
# irrelevant column in params.csv never breaks the import.
cpb_import_build_one <- function(params, data_df, extra_params) {
  plot_type <- tolower(trimws(params[["plot_type"]]))
  if (is.null(plot_type) || !nzchar(plot_type)) {
    stop("Every figure in `params_csv` needs a `plot_type`.", call. = FALSE)
  }
  if (!plot_type %in% names(cpb_import_col_args)) {
    stop("Unknown plot_type \"", plot_type, "\"; must be one of ",
      paste(names(cpb_import_col_args), collapse = ", "), ".",
      call. = FALSE
    )
  }
  fn <- get(paste0("cpb_", plot_type), mode = "function")
  col_args <- cpb_import_col_args[[plot_type]]
  valid_args <- names(formals(fn))

  params[["plot_type"]] <- NULL
  params[["create"]] <- NULL
  params[["id"]] <- NULL
  params[["data"]] <- NULL
  # how the figure is saved: not arguments of the wrapper
  save <- cpb_import_save_params(params[intersect(names(params),
                                                  cpb_import_save_args)])
  params[cpb_import_save_args] <- NULL
  cut <- params[["cut"]]
  cut_labels <- params[["cut_labels"]]
  params[["cut"]] <- NULL
  params[["cut_labels"]] <- NULL

  unknown <- setdiff(names(params), valid_args)
  if (length(unknown)) {
    warning("plot_type = \"", plot_type, "\" (cpb_", plot_type, "()) has no ",
      "argument named ", paste0("`", unknown, "`", collapse = ", "),
      " -- ignored, its own default is used instead.",
      call. = FALSE
    )
  }
  style <- params[["style"]]
  style <- if (is.null(style)) getOption("ggcpb.style", "dutch") else style

  # cut: numbers into classes, for the column that sets the colours. The
  # classes go into a new column, so the numbers can still be the x or y.
  if (!is.null(cut)) {
    target_arg <- cpb_import_cut_target(plot_type)
    target <- params[[target_arg]]
    if (is.null(target) || !target %in% names(data_df)) {
      stop("`cut` needs a `", cpb_import_cut_target(plot_type), "` column.",
           call. = FALSE)
    }
    labeller <- if (is.null(cut_labels)) {
      label_number_nl(style = style)
    } else {
      cpb_import_formatter(cut_labels, style)
    }
    if (is.null(labeller)) {
      stop("`cut_labels` must be number, euro or pct, not \"", cut_labels,
           "\".", call. = FALSE)
    }
    classes <- paste0(".cpb_classes_", target)
    # the open ends in the language of the figure
    ends <- if (identical(style, "english")) {
      list(start_label = "below ", end_label = " and above")
    } else {
      list()
    }
    data_df[[classes]] <- do.call(cpb_cut, c(
      list(data_df[[target]], breaks = cpb_import_coerce(cut),
           labeller = labeller),
      ends
    ))
    params[[target_arg]] <- classes
  }

  # a number used to split series (a year as `fill`) is a category, not a
  # gradient. Only a scatter `colour` and a map `value` keep their numbers.
  category_args <- c("fill", "group", "facet",
                     if (plot_type != "scatter") "colour")
  for (nm in intersect(names(params), category_args)) {
    col <- params[[nm]]
    if (col %in% names(data_df) && is.numeric(data_df[[col]])) {
      data_df[[col]] <- factor(data_df[[col]], levels = unique(data_df[[col]]))
    }
  }

  args <- list(data = data_df)
  for (nm in intersect(names(params), valid_args)) {
    val <- params[[nm]]
    if (nm %in% col_args) {
      if (!val %in% names(data_df)) {
        stop("plot_type = \"", plot_type, "\": `", nm, " = \"", val,
          "\"` is not a column in the data.",
          call. = FALSE
        )
      }
      args[[nm]] <- as.symbol(val)
    } else if (nm %in% cpb_import_formatter_args &&
                 !is.null(cpb_import_formatter(val, style))) {
      args[[nm]] <- cpb_import_formatter(val, style)
    } else if (nm %in% cpb_import_named_args) {
      args[[nm]] <- cpb_import_named(val)
    } else {
      args[[nm]] <- cpb_import_coerce(val)
    }
  }
  # x_lim on a date axis is given as dates
  x_col <- params[["x"]]
  if (!is.null(args$x_lim) && !is.null(x_col) &&
        inherits(data_df[[x_col]], "Date")) {
    args$x_lim <- cpb_import_as_date(as.character(args$x_lim))
  }
  args <- utils::modifyList(args, extra_params)
  list(plot = do.call(fn, args), plot_type = plot_type, unknown = unknown,
       save = save)
}

# The save parameters of one figure, checked: page (half, full, small),
# height in cm, preset (report, presentation, map), dpi and lock.
cpb_import_save_params <- function(p) {
  out <- list()
  if (!is.null(p$page)) {
    out$page <- tolower(trimws(p$page))
    if (!out$page %in% c("half", "full", "small")) {
      stop("`page` must be half, full or small, not \"", out$page, "\".",
           call. = FALSE)
    }
  }
  if (!is.null(p$preset)) {
    out$preset <- tolower(trimws(p$preset))
    if (!out$preset %in% c("report", "presentation", "map")) {
      stop("`preset` must be report, presentation or map, not \"",
           out$preset, "\".", call. = FALSE)
    }
  }
  for (nm in c("height", "dpi")) {
    if (!is.null(p[[nm]])) {
      v <- cpb_import_coerce(p[[nm]])
      if (!is.numeric(v) || length(v) != 1 || v <= 0) {
        stop("`", nm, "` must be one positive number, not \"", p[[nm]],
             "\".", call. = FALSE)
      }
      out[[nm]] <- v
    }
  }
  # height is written in cm, save_cpb() takes inches
  if (!is.null(out$height)) out$height <- out$height / 2.54
  if (!is.null(p$lock)) {
    out$lock <- cpb_import_coerce(p$lock)
    if (!is.logical(out$lock)) {
      stop("`lock` must be TRUE or FALSE.", call. = FALSE)
    }
  }
  out
}

# Always (over)writes a plain-text run log next to params_csv, listing
# every figure's outcome -- built (with any ignored/unknown parameter
# names), skipped (and why), plus a one-line summary. A console
# warning() is only ever seen in an interactive session; a script
# triggered from a scheduled task or a .bat file may never show its
# console at all, or may discard stderr entirely, so the log file is
# the only outcome record guaranteed to survive an unattended run.
cpb_import_write_log <- function(params_csv, data_dir, entries) {
  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")

  header <- c(
    paste0("ggcpb cpb_import_csv log -- ",
           format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("data:   ", data_dir),
    paste0("params: ", params_csv),
    ""
  )

  body <- unlist(lapply(entries, function(e) {
    i <- e$row
    lines <- if (e$status == "skipped") {
      paste0("Figure ", i, " \"", e$label, "\": skipped (", e$detail, ")")
    } else {
      head_line <- paste0(
        "Figure ", i, " \"", e$label, "\" (plot_type = ", e$plot_type, "):"
      )
      if (!length(e$unknown)) {
        paste0(head_line, " no issues")
      } else {
        c(head_line, paste0(
          "  - `", e$unknown, "` is not an argument of cpb_", e$plot_type,
          "(); ignored, its own default is used."
        ))
      }
    }
    c(lines, "")
  }))

  n_skipped <- sum(vapply(entries, function(e) e$status == "skipped",
                          logical(1)))
  n_built <- length(entries) - n_skipped
  n_issues <- sum(vapply(entries, function(e) {
    if (e$status == "skipped") 0L else length(e$unknown)
  }, integer(1)))
  summary_line <- paste0(
    length(entries), " figure(s) read, ", n_skipped, " skipped, ",
    n_built, " built, ", n_issues, if (n_issues == 1) " issue." else " issues."
  )

  writeLines(c(header, body, summary_line), log_path)
  tcat("ggcpb: wrote import log to ", log_path)
  invisible(log_path)
}

#' Import a plot (or set of plots) from a data CSV and a parameters CSV
#'
#' A CSV-only alternative to an Excel workbook: the datasets are plain CSV
#' files (long format -- one row per x/series combination) in a `data`
#' folder next to `params_csv`, and `params_csv` names which
#' `cpb_col()`/`cpb_area()`/... wrapper to call for each figure and its
#' arguments, in one of two layouts.
#'
#' Each figure names its dataset in the `data` parameter, with or without
#' the `.csv` extension. `data` = `koopkracht` reads `data/koopkracht.csv`,
#' relative to the folder holding `params_csv`. `data` = `a;b` builds the
#' figure once per file. A blank or absent `data` reads `data/data.csv`. A
#' dataset that
#' is missing or cannot be read is skipped with a warning, and every other
#' figure (and every other dataset in the same list) is still built.
#'
#' The optional `page` parameter (`half`, `full` or `small`, default `half`)
#' sets the page width a figure is saved at by [cpb_import_run()].
#'
#' Figures in the returned list are named after their dataset
#' (`koopkracht` for `koopkracht.csv`). When several figures use the same
#' dataset, the figure number in `params_csv` (the first figure column is
#' 1) is added:
#' `koopkracht_1`, `koopkracht_3`.
#'
#' `params_csv` needs a `plot_type` field naming the wrapper to use
#' (`"col"`, `"area"`, `"line"`, `"box"`, `"boxplot_extended"`, `"dot"`,
#' `"scatter"`, `"hist"`, `"donut"` or `"map"` -- i.e. the `cpb_*()` name
#' without the prefix), plus whichever
#' of that wrapper's own arguments the figure needs. Column-referencing
#' arguments (`x`, `y`, `fill`, `colour`, `p5`..`p95`, `sec_y`, ...) take
#' a column name from the dataset; everything else (`title`, `ylab`,
#' `palette`, `pct_axis`, `sec_type`, ...) is a literal value. A cell
#' containing `;` becomes a vector (e.g. `colour_index` = `"6;2"` becomes
#' `c(6, 2)`). Blank
#' cells, and any parameter name that isn't a real argument of the
#' chosen wrapper, are simply skipped -- the wrapper's own default
#' applies, so a typo or a stray column never breaks the import (a
#' `warning()` is still raised for each one). Set `create` to
#' `"n"`/`"no"`/`"false"`/`"f"` to skip a figure entirely.
#'
#' Some values are read specially, so that every argument can be set from a
#' CSV:
#' * `Inf` and `-Inf` are numbers, and a written `\n` is a line break.
#' * `x_labels` and `sec_axis_labels` take the name of a formatter:
#'   `number`, `euro` or `pct`.
#' * `dot_labels` takes name:value pairs, such as `p5:onderste 5%`.
#' * A dataset column of dates (`2024-01-31`, or `31-1-2024`) becomes a
#'   date axis, and `x_lim` on it is given as dates.
#' * `cut` (such as `0;20;40;Inf`) puts the numbers of the column that sets
#'   the colours (`value` of a map, `colour` of a line, dot or scatter plot,
#'   `fill` of the others) into classes. `cut_labels` (`number`, `euro` or
#'   `pct`) formats the class labels, as [cpb_cut()] does.
#' * `page`, `height` (in cm), `preset`, `dpi` and `lock` say how
#'   [cpb_import_run()] saves the figure, see [save_cpb()].
#'
#' In `params_csv` column 1 holds the parameter names, one per row. Every
#' other column is one figure. Fields at the end of a row can be left
#' out when they are blank.
#'
#' Every call also (over)writes a plain-text run log next to
#' `params_csv`, named `<params_csv>_log.txt`: one line per figure
#' (built, skipped and why, or built with any ignored parameter
#' names), plus a one-line summary. A console `warning()` only shows up
#' in an interactive session; the log file is there so a script run
#' unattended (e.g. from a scheduled task or a `.bat` file, where the
#' console may not be visible or stderr may be discarded) still leaves
#' a record of what happened.
#'
#' For a full reference of every `plot_type`'s own parameters (column
#' reference or literal, and its default), see
#' `system.file("extdata", "parameter_settings.csv", package = "ggcpb")`.
#'
#' @param params_csv Path to the parameters CSV (see Details). The datasets
#'   are read from the `data` folder next to it, with
#'   `read.csv(check.names = FALSE)`, so column names are kept exactly as
#'   written (spaces and all).
#' @param sep Field separator for both CSVs; defaults to `,`.
#' @param ... Optional overrides applied to every figure, taking
#'   priority over `params_csv` -- actual R values (not CSV strings),
#'   e.g. `legend = "right"`.
#' @param simplify If `TRUE` (default), a single figure is returned as a
#'   `ggplot` rather than a list of one. Set `FALSE` to always get the
#'   named list.
#' @return A single `ggplot` object if `params_csv` defines one figure,
#'   or a named list of them (named from each figure's `id`, else
#'   `title`, else `"figure_1"`, `"figure_2"`, ...) if it defines
#'   several. Also writes a run log as a side effect -- see Details.
#' @examples
#' \dontrun{
#' # datasets in ./data, named in the `data` column of params.csv
#' ps <- cpb_import_csv("params.csv")
#' ps$koopkracht
#' }
#' @export
cpb_import_csv <- function(params_csv, sep = ",", ..., simplify = TRUE) {
  if (!file.exists(params_csv)) {
    stop("Parameters CSV file not found: ", params_csv, call. = FALSE)
  }
  data_dir <- file.path(dirname(params_csv), "data")
  extra_params <- list(...)

  figs <- cpb_import_read_params(params_csv, sep)
  if (!any(lengths(figs) > 0)) {
    stop("`params_csv` defines no figures.", call. = FALSE)
  }

  entries <- list()
  items <- list()
  for (i in seq_along(figs)) {
    params <- figs[[i]]
    if (!length(params)) next # an empty column is not a figure
    label <- params[["id"]]
    if (is.null(label)) label <- params[["title"]]
    if (is.null(label)) label <- paste0("figure_", i)
    label <- trimws(label)

    create <- params[["create"]]
    create <- if (is.null(create)) "y" else tolower(trimws(create))
    if (create %in% c("n", "no", "false", "f")) {
      entries[[length(entries) + 1]] <- list(
        row = i, label = label, status = "skipped",
        detail = paste0("create = ", create)
      )
      next
    }

    files <- character(0)
    if (!is.null(params[["data"]])) {
      files <- trimws(strsplit(params[["data"]], ";", fixed = TRUE)[[1]])
      files <- files[nzchar(files)]
      # the extension is optional: `koopkracht` and `koopkracht.csv` are one
      # dataset
      # (also as `.CSV`, which is the same file on Windows and Mac, but not
      # on Linux)
      files <- unique(paste0(sub("[.]csv$", "", files, ignore.case = TRUE),
                             ".csv"))
    }
    if (!length(files)) files <- "data.csv"
    for (f in files) {
      items[[length(items) + 1]] <- list(
        row = i, params = params, label = label,
        path = file.path(data_dir, f), file = f
      )
    }
  }

  # a dataset used by more than one figure gets its figure number
  # added to its name, so no figure overwrites another
  stems <- vapply(items, function(it) tools::file_path_sans_ext(it$file),
                  character(1))
  shared <- unique(stems[duplicated(stems)])

  cache <- new.env(parent = emptyenv())
  read_cached <- function(path) {
    if (is.null(cache[[path]])) {
      cache[[path]] <- tryCatch(
        list(df = cpb_import_read_data(path, sep)),
        error = function(e) list(error = conditionMessage(e))
      )
    }
    cache[[path]]
  }

  plots <- list()
  for (it in items) {
    entry_label <- paste0(it$label, " [", it$file, "]")
    # warns, and returns the log entry of a figure that is left out
    skipped <- function(detail) {
      warning("Figure ", it$row, " \"", entry_label, "\": ", detail,
        " -- skipped.",
        call. = FALSE
      )
      list(row = it$row, label = entry_label, status = "skipped",
           detail = detail)
    }

    if (!file.exists(it$path)) {
      entries[[length(entries) + 1]] <-
        skipped(paste0("dataset not found: ", it$path))
      next
    }
    data <- read_cached(it$path)
    if (!is.null(data$error)) {
      entries[[length(entries) + 1]] <- skipped(paste0("error: ", data$error))
      next
    }

    # A bad figure (e.g. sec_y pointing at a non-numeric column, or any
    # other cpb_*() argument validation failure) must not take the
    # whole batch down with it, in an unattended run especially -- one
    # broken figure among twenty should still leave the other nineteen
    # built. The real error message goes in the log, not just the
    # console warning(), which an unattended run may never show.
    built <- tryCatch(
      cpb_import_build_one(it$params, data$df, extra_params),
      error = function(e) e
    )
    if (inherits(built, "error")) {
      entries[[length(entries) + 1]] <-
        skipped(paste0("error: ", conditionMessage(built)))
      next
    }
    entries[[length(entries) + 1]] <- list(
      row = it$row, label = entry_label, status = "built",
      plot_type = built$plot_type, unknown = built$unknown
    )

    stem <- tools::file_path_sans_ext(it$file)
    fig_id <- if (stem %in% shared) paste0(stem, "_", it$row) else stem
    plots[[fig_id]] <- built$plot
    attr(plots[[fig_id]], "cpb_import_save") <- built$save
  }

  entries <- entries[order(vapply(entries, function(e) e$row, numeric(1)))]
  log_path <- cpb_import_write_log(params_csv, data_dir, entries)

  # a partial batch (some figures built, some skipped or failed) is a
  # legitimate, useful result and must not error -- but coming back
  # with nothing at all almost never is, and should not do so quietly;
  # the individual reasons are already in the log, and repeated here
  # since an unattended run may never show this console error either
  if (!length(plots)) {
    detail <- vapply(entries,
                     function(e) paste0("\"", e$label, "\": ", e$detail),
                     character(1))
    stop("No figures were built from `params_csv` (", length(entries),
      " figure(s) defined, all skipped) -- ", paste(detail, collapse = "; "),
      ". See the run log: ", log_path,
      call. = FALSE
    )
  }

  if (simplify && length(plots) == 1) plots[[1]] else plots
}

#' Build and save every figure of a figure folder
#'
#' What `generate_plot.bat` starts. Reads `params.csv` in `dir`,
#' builds the figures with [cpb_import_csv()] (datasets
#' from `dir/data`), and saves each as a PNG in `dir/generated`, named after
#' its dataset. A PNG that already exists is replaced. Datasets or figures
#' that fail, also when a figure only fails while it is drawn, are skipped,
#' listed at the end, and written to `params_log.txt`. The parameters
#' `page`, `height` (in cm), `preset`, `dpi` and `lock` in `params.csv` are
#' passed to [save_cpb()].
#'
#' @param dir The figure folder, holding `params.csv` and `data/`.
#' @param page Page width passed to [save_cpb()] for a figure that has no
#'   `page` in `params_csv`.
#' @return Invisibly, the paths of the PNGs written.
#' @export
cpb_import_run <- function(dir = ".", page = "half") {
  params_csv <- file.path(dir, "params.csv")
  if (!file.exists(params_csv)) {
    stop("params.csv was not found in ", normalizePath(dir), ".", call. = FALSE)
  }
  warns <- character(0)
  figs <- withCallingHandlers(
    cpb_import_csv(params_csv = params_csv, simplify = FALSE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  out_dir <- file.path(dir, "generated")
  if (!dir.exists(out_dir)) dir.create(out_dir)
  paths <- file.path(out_dir,
                     paste0(gsub("[^A-Za-z0-9_-]+", "_", names(figs)), ".png"))
  # A figure that only fails when it is drawn must not stop the others.
  # It is skipped, listed at the end, and added to the log.
  saved <- rep(FALSE, length(figs))
  not_saved <- character(0)
  for (i in seq_along(figs)) {
    save <- utils::modifyList(list(page = page),
                              as.list(attr(figs[[i]], "cpb_import_save")))
    result <- withCallingHandlers(
      tryCatch({
        do.call(save_cpb, c(list(paths[[i]], figs[[i]]), save))
        TRUE
      }, error = function(e) conditionMessage(e)),
      warning = function(w) {
        warns <<- c(warns, paste0("\"", names(figs)[i], "\": ",
                                  conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
    if (isTRUE(result)) {
      saved[i] <- TRUE
    } else {
      msg <- paste0("Figure \"", names(figs)[i], "\": not saved (",
                    gsub("\\s+", " ", result), ") -- skipped.")
      warns <- c(warns, msg)
      not_saved <- c(not_saved, msg)
    }
  }
  if (length(not_saved)) {
    log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")
    cat("", not_saved, sep = "\n", file = log_path, append = TRUE)
  }

  cat("\nDone. ", sum(saved),
      " figure(s) saved in the generated folder.\n", sep = "")
  if (length(warns)) {
    n_skipped <- sum(grepl("-- skipped\\.$", warns))
    cat("\n", n_skipped, " figure(s) skipped. All warnings:\n", sep = "")
    cat(paste0("  ", warns), sep = "\n")
    cat("\nSee params_log.txt for details.\n")
  }
  invisible(paths[saved])
}
