# import.R
#
# Standalone CSV import: builds ready-to-use ggplot objects from a plain
# data.csv (the actual series data) and a params.csv (one row/column per
# figure, naming which cpb_*() wrapper to call and its arguments).

# which args of each wrapper are column refs (not literal values),
# keyed by plot_type (the cpb_ name without its prefix)
cpb_import_col_args <- list(
  col     = c("x", "y", "fill", "group", "facet", "sec_y"),
  area    = c("x", "y", "fill", "facet", "sec_y"),
  line    = c("x", "y", "colour", "ymin", "ymax", "facet", "sec_y"),
  box     = c("x", "p5", "p25", "p50", "p75", "p95", "mean", "fill", "group", "facet", "sec_y"),
  dot     = c("x", "y", "lower", "upper", "colour", "group", "facet", "sec_y"),
  scatter = c("x", "y", "colour", "facet"),
  hist    = c("x", "fill", "facet"),
  donut   = c("fill", "y", "label")
)

# The full set of recognised parameter names (plot_type, create, id, every
# wrapper's own arguments, plus the CSV-facing renames), used to tell
# params.csv's two layouts apart -- see cpb_import_read_params().
cpb_import_known_names <- function() {
  wrapper_names <- unlist(lapply(names(cpb_import_col_args), function(pt) {
    names(formals(get(paste0("cpb_", pt), mode = "function")))
  }))
  unique(c("plot_type", "create", "id", wrapper_names))
}

# One string value from a CSV cell -> an R value. A value containing ";"
# becomes a vector, each piece coerced the same way (so "6;2" becomes the
# numeric vector c(6, 2), matching e.g. index = c(6, 2)). Otherwise: a
# plain number if it looks like one, TRUE/FALSE if it reads as one, else
# the string as-is.
cpb_import_coerce_scalar <- function(x) {
  x <- trimws(x)
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", x)) {
    return(as.numeric(x))
  }
  if (tolower(x) %in% c("true", "false")) {
    return(as.logical(x))
  }
  x
}

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
  first_line <- tryCatch(readLines(path, n = 1, warn = FALSE), error = function(e) "")
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
      best, " instead. Pass the matching `sep` to `import_csv()`, or ",
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
cpb_import_read_data <- function(data_csv, sep) {
  data_df <- tryCatch(
    utils::read.csv(data_csv, check.names = FALSE, stringsAsFactors = FALSE, sep = sep),
    error = function(e) {
      stop("Could not read `data_csv` (", conditionMessage(e), "). Check that ",
        "every row has the same number of fields, and that the very first ",
        "row is the header row naming each column, with nothing (no title, ",
        "no blank line) above it.",
        call. = FALSE
      )
    }
  )

  looks_numeric <- grepl("^-?[0-9]+(\\.[0-9]+)?$", trimws(names(data_df)))
  if (any(looks_numeric)) {
    stop("The first row of `data_csv` does not look like column names ",
      "(found ", paste0("\"", names(data_df)[looks_numeric], "\"", collapse = ", "),
      "). `import_csv()` always reads the first row as the header; check ",
      "that a real header row is there, and that no other line comes ",
      "before it.", cpb_import_sep_hint(data_csv, sep, ncol(data_df)),
      call. = FALSE
    )
  }

  if (ncol(data_df) == 1) {
    hint <- cpb_import_sep_hint(data_csv, sep, ncol(data_df))
    if (nzchar(hint)) {
      stop("`data_csv` was read as a single column.", hint, call. = FALSE)
    }
  }

  data_df
}

# Reads params.csv in either of its two layouts and returns one named list
# of (still all-character) parameters per figure, in file order. Blank
# cells are dropped entirely, so a missing value never shadows the
# wrapper's own default.
#
# Vertical (meta-tab style): column 1 holds parameter names, one row
# each; columns 2..N hold one figure's values apiece.
# Horizontal (key-value table): row 1 holds parameter names as column
# headers; every other row is one figure.
#
# The two are told apart with a simple vote: read the file both ways, and
# see which candidate's "parameter name" column (the header row for
# horizontal, the first column for vertical) mostly matches known
# parameter names. The other candidate, read the wrong way round, ends
# up with a "parameter name" column full of plain data values (column
# names from data.csv, "line"/"box", ...) that don't match anything.
cpb_import_read_params <- function(params_csv, sep) {
  known <- cpb_import_known_names()

  read_error <- function(e) {
    stop("Could not read `params_csv` (", conditionMessage(e), "). Check ",
      "that every row has the same number of fields.",
      call. = FALSE
    )
  }
  raw_h <- tryCatch(
    utils::read.csv(params_csv,
      header = TRUE, check.names = FALSE,
      stringsAsFactors = FALSE, sep = sep
    ),
    error = read_error
  )
  raw_v <- tryCatch(
    utils::read.csv(params_csv,
      header = FALSE, check.names = FALSE,
      stringsAsFactors = FALSE, sep = sep
    ),
    error = read_error
  )

  score_h <- mean(tolower(trimws(names(raw_h))) %in% known)
  score_v <- mean(tolower(trimws(as.character(raw_v[[1]]))) %in% known)

  if (max(score_h, score_v) == 0 || score_h == score_v) {
    stop("Could not tell whether `params_csv` is in the vertical or ",
      "horizontal layout -- make sure it has a `plot_type` field, either ",
      "as a column header, or as a value in the first column.",
      cpb_import_sep_hint(params_csv, sep, ncol(raw_h)),
      call. = FALSE
    )
  }

  if (score_h > score_v) {
    names(raw_h) <- tolower(trimws(names(raw_h)))
    lapply(seq_len(nrow(raw_h)), function(i) {
      vals <- vapply(raw_h[i, , drop = FALSE], as.character, character(1))
      keep <- !is.na(vals) & nzchar(trimws(vals))
      as.list(vals[keep])
    })
  } else {
    param_names <- tolower(trimws(as.character(raw_v[[1]])))
    n_figs <- ncol(raw_v) - 1
    lapply(seq_len(n_figs), function(i) {
      vals <- as.character(raw_v[[i + 1]])
      keep <- !is.na(vals) & nzchar(trimws(vals))
      stats::setNames(as.list(vals[keep]), param_names[keep])
    })
  }
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

  unknown <- setdiff(names(params), valid_args)
  if (length(unknown)) {
    warning("plot_type = \"", plot_type, "\" (cpb_", plot_type, "()) has no ",
      "argument named ", paste0("`", unknown, "`", collapse = ", "),
      " -- ignored, its own default is used instead.",
      call. = FALSE
    )
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
    } else {
      args[[nm]] <- cpb_import_coerce(val)
    }
  }
  args <- utils::modifyList(args, extra_params)
  list(plot = do.call(fn, args), plot_type = plot_type, unknown = unknown)
}

# Always (over)writes a plain-text run log next to params_csv, listing
# every figure's outcome -- built (with any ignored/unknown parameter
# names), skipped (and why), plus a one-line summary. A console
# warning() is only ever seen in an interactive session; a script
# triggered from a scheduled task or a .bat file may never show its
# console at all, or may discard stderr entirely, so the log file is
# the only outcome record guaranteed to survive an unattended run.
cpb_import_write_log <- function(params_csv, data_csv, entries) {
  log_path <- paste0(tools::file_path_sans_ext(params_csv), "_log.txt")

  header <- c(
    paste0("ggcpb import_csv log -- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("data:   ", data_csv),
    paste0("params: ", params_csv),
    ""
  )

  body <- unlist(lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
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

  n_skipped <- sum(vapply(entries, function(e) e$status == "skipped", logical(1)))
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
#' A CSV-only alternative to an Excel workbook: `data_csv` holds the
#' plain series data (long format -- one row per x/series combination),
#' and `params_csv` names which `cpb_col()`/`cpb_area()`/... wrapper to
#' call for each figure and its arguments, in one of two layouts.
#'
#' `params_csv` needs a `plot_type` field naming the wrapper to use
#' (`"col"`, `"area"`, `"line"`, `"box"`, `"dot"`, `"scatter"`, or
#' `"hist"` -- i.e. the `cpb_*()` name without the prefix), plus whichever
#' of that wrapper's own arguments the figure needs. Column-referencing
#' arguments (`x`, `y`, `fill`, `colour`, `p5`..`p95`, `sec_y`, ...) take
#' a column name from `data_csv`; everything else (`title`, `ylab`,
#' `palette`, `pct_axis`, `sec_type`, ...) is a literal value. A cell
#' containing `;` becomes a vector (e.g. `index` = `"6;2"` becomes
#' `c(6, 2)`). Blank
#' cells, and any parameter name that isn't a real argument of the
#' chosen wrapper, are simply skipped -- the wrapper's own default
#' applies, so a typo or a stray column never breaks the import (a
#' `warning()` is still raised for each one). Set `create` to
#' `"n"`/`"no"`/`"false"`/`"f"` to skip a figure entirely.
#'
#' `params_csv` can be laid out either way, and the layout is detected
#' automatically:
#' * **Vertical** (meta-tab style): column 1 holds parameter names, one
#'   row each; columns 2..N hold one figure's values apiece.
#' * **Horizontal** (key-value table): row 1 holds parameter names as
#'   column headers; every other row is one figure.
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
#' @param data_csv Path to the data CSV. Read with
#'   `read.csv(check.names = FALSE)`, so column names are kept exactly
#'   as written (spaces and all).
#' @param params_csv Path to the parameters CSV (see Details).
#' @param sep Field separator for both CSVs; defaults to `,`.
#' @param ... Optional overrides applied to every figure, taking
#'   priority over `params_csv` -- actual R values (not CSV strings),
#'   e.g. `legend = "right"`.
#' @return A single `ggplot` object if `params_csv` defines one figure,
#'   or a named list of them (named from each figure's `id`, else
#'   `title`, else `"figure_1"`, `"figure_2"`, ...) if it defines
#'   several. Also writes a run log as a side effect -- see Details.
#' @examples
#' \dontrun{
#' p <- import_csv("data.csv", "params.csv")
#' p
#'
#' ps <- import_csv("data.csv", "params_multi.csv")
#' ps$koopkracht
#' }
#' @export
import_csv <- function(data_csv, params_csv, sep = ",", ...) {
  if (!file.exists(data_csv)) {
    stop("Data CSV file not found: ", data_csv, call. = FALSE)
  }
  if (!file.exists(params_csv)) {
    stop("Parameters CSV file not found: ", params_csv, call. = FALSE)
  }
  data_df <- cpb_import_read_data(data_csv, sep)
  extra_params <- list(...)

  figs <- cpb_import_read_params(params_csv, sep)
  if (!length(figs)) {
    stop("`params_csv` defines no figures.", call. = FALSE)
  }

  plots <- list()
  entries <- vector("list", length(figs))
  for (i in seq_along(figs)) {
    params <- figs[[i]]
    label <- params[["id"]]
    if (is.null(label)) label <- params[["title"]]
    if (is.null(label)) label <- paste0("figure_", i)
    label <- trimws(label)

    create <- params[["create"]]
    create <- if (is.null(create)) "y" else tolower(trimws(create))

    if (create %in% c("n", "no", "false", "f")) {
      entries[[i]] <- list(
        label = label, status = "skipped",
        detail = paste0("create = ", create)
      )
      next
    }

    # A bad figure (e.g. sec_y pointing at a non-numeric column, or any
    # other cpb_*() argument validation failure) must not take the
    # whole batch down with it, in an unattended run especially -- one
    # broken figure among twenty should still leave the other nineteen
    # built. Recorded the same way an explicit create = "n" skip is,
    # with the actual error message as the detail (not just a pointer
    # to the console warning() below, which an unattended run may
    # never show -- the log has to carry the real reason on its own).
    build_error <- NULL
    built <- tryCatch(
      cpb_import_build_one(params, data_df, extra_params),
      error = function(e) {
        build_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(build_error)) {
      warning("Figure ", i, " \"", label, "\": ", build_error,
        " -- skipped.",
        call. = FALSE
      )
      entries[[i]] <- list(
        label = label, status = "skipped",
        detail = paste0("error: ", build_error)
      )
      next
    }
    entries[[i]] <- list(
      label = label, status = "built",
      plot_type = built$plot_type, unknown = built$unknown
    )

    fig_id <- gsub("[[:punct:] ]+", "-", label)
    plots[[fig_id]] <- built$plot
  }

  log_path <- cpb_import_write_log(params_csv, data_csv, entries)

  # a partial batch (some figures built, some skipped or failed) is a
  # legitimate, useful result and must not error -- but coming back
  # with nothing at all almost never is, and should not do so quietly;
  # the individual reasons are already in the log, and repeated here
  # since an unattended run may never show this console error either
  if (!length(plots)) {
    detail <- vapply(entries, function(e) paste0("\"", e$label, "\": ", e$detail), character(1))
    stop("No figures were built from `params_csv` (", length(figs),
      " figure(s) defined, all skipped) -- ", paste(detail, collapse = "; "),
      ". See the run log: ", log_path,
      call. = FALSE
    )
  }

  if (length(plots) == 1) plots[[1]] else plots
}

#' Copy a ready-to-run import kit to a folder
#'
#' `import_csv()` itself still needs R to be started by hand. This copies
#' a small kit to `dest`: an example `data.csv` and `params.csv`, a
#' `run_import.R` script that reads them and saves each figure as a PNG
#' in a `generated` subfolder, and two launchers that run it with a
#' double click instead of opening R at all -- `run_import.bat` on
#' Windows, `run_import.command` on a Mac. R itself, and the ggcpb
#' package, still need to be installed on the machine the launcher runs
#' on; the launcher only skips having to open R and type a command by
#' hand each time.
#'
#' The example `data.csv` and `params.csv` are meant to be replaced.
#' Edit them (a plain text editor or a spreadsheet program both work),
#' keep the same file names, and the launcher builds whatever figures
#' the new `params.csv` describes on the next run.
#'
#' @param dest Destination folder. Created if it does not exist yet.
#' @param overwrite If `FALSE` (default), the function stops instead of
#'   replacing any file already present in `dest`. Set to `TRUE` to
#'   replace them (e.g. to update the launcher after a package update,
#'   without touching an already-edited `data.csv`/`params.csv` -- copy
#'   to an empty folder first in that case, then move just the two
#'   scripts over by hand).
#' @return Invisibly, `dest`.
#' @examples
#' \dontrun{
#' cpb_import_kit("~/Desktop/mijn_figuren")
#' }
#' @export
cpb_import_kit <- function(dest, overwrite = FALSE) {
  kit_dir <- system.file("import_kit", package = "ggcpb")
  if (!nzchar(kit_dir)) {
    stop("The import kit was not found in the installed ggcpb package.",
      call. = FALSE
    )
  }

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  files <- list.files(kit_dir)
  already_there <- files[file.exists(file.path(dest, files))]
  if (length(already_there) && !isTRUE(overwrite)) {
    stop("The following file(s) already exist in `dest` and were not ",
      "replaced: ", paste(already_there, collapse = ", "),
      ". Set `overwrite = TRUE` to replace them.",
      call. = FALSE
    )
  }

  file.copy(file.path(kit_dir, files), dest, overwrite = TRUE)

  # a Mac launcher has to keep its executable bit to double-click at
  # all; file.copy() does not promise to carry permissions over, so it
  # is set again explicitly on the copy
  command_file <- file.path(dest, "run_import.command")
  if (file.exists(command_file)) {
    Sys.chmod(command_file, mode = "0755")
  }

  tcat("ggcpb: copied the import kit to ", normalizePath(dest))
  invisible(dest)
}
