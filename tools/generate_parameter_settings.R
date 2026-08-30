# tools/generate_parameter_settings.R
#
# Regenerates inst/extdata/parameter_settings.csv from the cpb_*()
# wrappers' own formals(), so the reference file can never silently
# drift out of sync with the actual functions the way a hand-maintained
# copy eventually does. Run by hand after changing a wrapper's
# arguments:
#   Rscript tools/generate_parameter_settings.R

devtools::load_all(quiet = TRUE)

plot_types <- names(cpb_import_col_args)

# Shown for a "column" kind parameter with no example of its own
# below: always a plausible, generic column name, since the real one
# depends entirely on data_csv. Keyed by parameter name, not
# plot_type + parameter, since the same name (x, fill, sec_y, ...)
# means the same thing across every wrapper that has it.
column_examples <- c(
  x       = "jaar",
  y       = "koopkracht",
  fill    = "groep",
  colour  = "groep",
  group   = "sector",
  facet   = "regio",
  sec_y   = "werkloosheid",
  label   = "naam",
  lower   = "ondergrens",
  upper   = "bovengrens",
  ymin    = "ondergrens",
  ymax    = "bovengrens",
  mean    = "gemiddelde",
  p5      = "p5",
  p25     = "p25",
  p50     = "p50",
  p75     = "p75",
  p95     = "p95"
)

# Shown for a "literal" kind parameter whose own default is blank
# (NULL): the default itself is not a usable example there, so a
# concrete, representative value is given by hand instead. A literal
# parameter with a real default (a number, TRUE/FALSE, or a set of
# named choices) needs no entry here -- its own default already is a
# valid example, and is used as one automatically below.
literal_examples <- c(
  title            = "Titel van de figuur",
  subtitle         = "ondertitel (vervangt de standaard eenheid boven de figuur)",
  xlab             = "eenheid onderaan de x-as",
  ylab             = "% mutatie",
  colourlab        = "titel van de kleurenlegenda",
  filllab          = "titel van de vullingslegenda",
  fill_colour      = "#005faf",
  line_colour      = "#005faf",
  point_colour     = "#e6006e",
  sec_colour       = "#e6006e",
  sec_label        = "naam van de tweede reeks",
  sec_ylab         = "eenheid van de rechteras",
  sec_limits       = "0;100",
  sec_linewidth    = "0.55",
  sec_accuracy     = "0.1",
  value_accuracy   = "0.1",
  value_breaks     = "0;25;50;75;100",
  value_limits     = "0;100",
  x_lim            = "2015;2025",
  forecast_x       = "2025",
  legend_ncol      = "2",
  facet_ncol       = "2",
  legend_key_size  = "0.3",
  bins             = "30",
  binwidth         = "5",
  box_labels       = "TRUE",
  dot_labels       = "p5:onderste 5%;p95:bovenste 5%",
  zeroline         = "TRUE",
  index            = "gebruik colour_index of fill_index in plaats hiervan",
  colour_index     = "2;6",
  color_index      = "2;6",
  fill_index       = "2;6"
)

fmt_default <- function(val) {
  if (is.symbol(val) && identical(as.character(val), "")) {
    return(list(kind = "required", text = "(required)", is_blank = FALSE))
  }
  if (is.null(val)) {
    return(list(kind = "blank", text = "", is_blank = TRUE))
  }
  # a call, e.g. c("stack", "dodge", "fill") or size (another
  # argument's own default, e.g. sec_point_size = size): evaluate a
  # literal vector, otherwise fall back to its deparsed source text
  evaluated <- tryCatch(eval(val), error = function(e) NULL)
  if (is.character(evaluated) && length(evaluated) > 1) {
    return(list(
      kind = "enum",
      text = paste0(paste(evaluated, collapse = "/"),
        " (default: ", evaluated[[1]], ")"),
      is_blank = FALSE
    ))
  }
  text <- if (is.character(evaluated) && length(evaluated) == 1) {
    evaluated
  } else if (!is.null(evaluated) && length(evaluated) == 1) {
    format(evaluated)
  } else {
    paste(deparse(val), collapse = " ")
  }
  list(kind = "literal", text = text, is_blank = !nzchar(text))
}

rows <- list()
for (pt in plot_types) {
  fn <- get(paste0("cpb_", pt), mode = "function")
  f <- formals(fn)
  col_args <- cpb_import_col_args[[pt]]
  for (nm in names(f)) {
    if (nm %in% c("data", "...")) next
    is_column <- nm %in% col_args
    default <- fmt_default(f[[nm]])

    example <- if (is_column) {
      if (default$kind == "required") {
        unname(column_examples[nm])
      } else {
        unname(column_examples[nm])
      }
    } else if (default$kind %in% c("literal", "enum") && !default$is_blank) {
      default$text
    } else {
      unname(literal_examples[nm])
    }
    if (is.na(example)) example <- ""

    rows[[length(rows) + 1]] <- data.frame(
      plot_type = pt, param = nm,
      kind = if (is_column) "column (from data_csv)" else "literal",
      default = default$text, example = example,
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
write.csv(out, "inst/extdata/parameter_settings.csv", row.names = FALSE)
cat("Wrote inst/extdata/parameter_settings.csv (", nrow(out), " rows).\n", sep = "")

missing_example <- out[!nzchar(out$example), c("plot_type", "param", "kind", "default")]
if (nrow(missing_example)) {
  cat("\nNo example value for these (add one to column_examples or ",
    "literal_examples in this script):\n", sep = "")
  print(missing_example, row.names = FALSE)
}
