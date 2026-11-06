# Regenerates the parameter reference.
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
# depends entirely on the dataset. Keyed by parameter name, not
# plot_type + parameter, since the same name (x, fill, sec_y, ...)
# means the same thing across every wrapper that has it.
column_examples <- c(
  x       = "year",
  y       = "purchasing_power",
  fill    = "group",
  colour  = "group",
  group   = "sector",
  facet   = "region",
  sec_y   = "unemployment",
  label   = "name",
  lower   = "lower_bound",
  upper   = "upper_bound",
  ymin    = "lower_bound",
  ymax    = "upper_bound",
  mean    = "mean",
  p5      = "p5",
  p25     = "p25",
  p50     = "p50",
  p75     = "p75",
  p95     = "p95",
  region  = "code",
  value   = "share"
)

# Shown for a "literal" kind parameter whose own default is blank
# (NULL): the default itself is not a usable example there, so a
# concrete, representative value is given by hand instead. A literal
# parameter with a real default (a number, TRUE/FALSE, or a set of
# named choices) needs no entry here -- its own default already is a
# valid example, and is used as one automatically below.
literal_examples <- c(
  title            = "Title of the figure",
  subtitle         = "subtitle (replaces the default unit above the figure)",
  xlab             = "unit below the x axis",
  ylab             = "% change",
  legend_title     = "title of the legend",
  fill_colour      = "#005faf",
  line_colour      = "#005faf",
  symbol_colour    = "#e6006e",
  sec_colour       = "#e6006e",
  sec_legend_label = "name of the second series",
  sec_ylab         = "unit of the right axis",
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
  bins             = "30",
  binwidth         = "5",
  box_labels       = "TRUE",
  dot_labels       = "p5:bottom 5%;p95:top 5%",
  zeroline         = "TRUE",
  colour_index     = "2;6",
  x_labels         = "`euro`, `number`, `pct` or labels such as `a;b;c`",
  sec_axis_labels  = "`pct`, `number`, `euro` or labels such as `a;b;c`",
  border_colour    = "white",
  na_fill          = "lightgrey",
  label_colour     = "white",
  value_axis       = "bottom",
  sec_at           = "0;50;100",
  legend_nrow      = "2",
  x_lim_follow_data = "TRUE",
  panel_size       = "1.8",
  zero_indicator   = "TRUE",
  ylab_position    = "left"
)

# A short description per parameter, in plain language. Every parameter
# of every plot type needs one (the script stops when one is missing).
descriptions <- c(
  data = paste0(
    "The dataset (file in the folder data) to draw. Leave blank for ",
    "data.csv. Write a;b for one figure per dataset."
  ),
  page = paste0(
    "The width of the saved figure: half is 7.5 cm, full is 15.5 cm and ",
    "small is 6.8 cm."
  ),
  height = paste0(
    "The height of the saved figure in cm. Blank gives the standard height."
  ),
  preset = paste0(
    "The size preset: report, presentation or a map that is as tall as it ",
    "needs."
  ),
  dpi = "The sharpness of the saved image, in dots per inch.",
  lock = paste0(
    "TRUE stops with a message when the title or legend is wider than the ",
    "page. FALSE saves the figure anyway."
  ),
  cut = paste0(
    "Cuts the numbers of the colour column into classes. The values are the ",
    "class limits."
  ),
  cut_labels = "How the class limits are written in the legend.",
  x = "The column with the categories or numbers on the x axis.",
  y = "The column with the values.",
  fill = "The column that splits the bars or areas into colours.",
  fill_colour = paste0(
    "One colour for all bars or areas, when there is no fill column."
  ),
  group = "The column that groups the data in blocks next to each other.",
  group_gap = "The space between the groups, in bar widths.",
  position = paste0(
    "How bars of one category are placed: on top of each other, next to ",
    "each other or as shares of 100%."
  ),
  orientation = "Draws the figure upright (vertical) or turned (horizontal).",
  sec_y = "The column drawn on a second axis on the right.",
  sec_type = "How the second series is drawn.",
  sec_limits = "The lowest and highest value of the right axis.",
  sec_legend_label = "The name of the second series in the legend.",
  sec_ylab = "The unit above the right axis.",
  sec_colour = "The colour of the second series.",
  sec_linewidth = "The line width of the second series.",
  sec_points = "TRUE adds points to the second series when it is a line.",
  sec_symbol_size = "The size of the points of the second series.",
  sec_col_width = "The width of the bars of the second series.",
  sec_accuracy = "The rounding of the numbers on the right axis, such as 0.1.",
  sec_scale_auto = paste0(
    "TRUE chooses the right axis automatically so it fits the left axis ",
    "lines."
  ),
  sec_at = "The values at which the right axis is labelled.",
  sec_axis_labels = paste0(
    "How the numbers on the right axis are written, or your own labels."
  ),
  palette = "The colour set used for the categories.",
  colour_index = paste0(
    "Which house colours to use, by number. The first number is for the ",
    "first category."
  ),
  pct_axis = "TRUE writes the values on the axis as percentages.",
  value_accuracy = paste0(
    "The rounding of the numbers on the value axis, such as 0.1."
  ),
  value_breaks = "The values at which the value axis is labelled.",
  value_limits = "The lowest and highest value of the value axis.",
  value_crop = paste0(
    "TRUE cuts off the data outside the value limits, with a warning."
  ),
  value_labels = "TRUE prints the value on each bar.",
  x_lim = "The first and last value shown on the x axis.",
  x_lim_follow_data = paste0(
    "TRUE lets the x axis follow the data when it runs past x_lim."
  ),
  x_ticks_ends = "TRUE also labels the first and last month of a monthly axis.",
  forecast_x = paste0(
    "The x value where the forecast starts. Everything after it is shaded."
  ),
  forecast_label = "The word shown in the forecast area.",
  reverse_legend = "TRUE turns the order of the legend around.",
  legend_ncol = "The number of columns of the legend.",
  legend_nrow = "The number of rows of the legend.",
  facet = "The column that splits the figure into small panels.",
  facet_ncol = "The number of panels next to each other.",
  facet_scales = paste0(
    "Whether the panels share their axes (fixed) or each have their own ",
    "(free)."
  ),
  legend = "Where the legend is placed. none removes it.",
  zeroline = paste0(
    "TRUE draws a line at zero. Blank draws it only when zero is on the ",
    "axis."
  ),
  minor = "TRUE adds small ticks between the labels.",
  ticks = "TRUE draws ticks on the axis.",
  flush_legend = "TRUE places the legend at the left edge of the figure.",
  axis_text_size = "The size of the axis text, in points.",
  grid_colour = "The colour of the grid lines.",
  grid_linewidth = "The width of the grid lines.",
  title = "The title above the figure.",
  subtitle = "The text under the title. It replaces the unit.",
  xlab = "The text next to the x axis.",
  ylab = "The unit above the value axis.",
  legend_title = "The title of the legend.",
  style = paste0(
    "The language of the figure: dutch or english. It sets the number ",
    "format and the words."
  ),
  colour = "The column that gives lines or points their colour.",
  line_colour = "One colour for the line, when there is no colour column.",
  linewidth = "The width of the line.",
  points = "TRUE adds points to the line.",
  symbol_size = "The size of the points.",
  ymin = "The column with the lower end of an uncertainty band.",
  ymax = "The column with the upper end of an uncertainty band.",
  p5 = "The column with the 5th percentile, the lower whisker.",
  p25 = "The column with the 25th percentile, the bottom of the box.",
  p50 = "The column with the median, the line in the box.",
  p75 = "The column with the 75th percentile, the top of the box.",
  p95 = "The column with the 95th percentile, the upper whisker.",
  mean = "The column with the mean, drawn as a diamond.",
  box_style = "The look of the boxes.",
  dot_labels = "Names for the dots of the dot style, as name:text pairs.",
  box_labels = "TRUE prints the values of the box.",
  label_accuracy = "The rounding of the printed percentages.",
  width = "The width of the bars or boxes, as a share of the space.",
  value_axis = "Whether the value axis is at the bottom or at the top.",
  lower = "The column with the lower end of the interval.",
  upper = "The column with the upper end of the interval.",
  symbol_colour = "The colour of the dots.",
  cap_width = "The width of the end caps of the interval.",
  x_labels = "How the numbers on the x axis are written, or your own labels.",
  binwidth = "The width of one bar of the histogram, in the unit of the data.",
  bins = "The number of bars of the histogram.",
  outline = "The colour of the line around the bars.",
  label = "The column with the text of each slice.",
  wedge_labels = "TRUE prints the value on each slice.",
  label_style = paste0(
    "Prints the label inside the slice (wedge) or outside with a line ",
    "(leader)."
  ),
  label_colour = "The colour of the labels in the slices.",
  leader_length = "The length of the lines to the labels.",
  legend_pct = "TRUE adds the percentage to each name in the legend.",
  ring_width = "The thickness of the ring, as a share of the radius.",
  panel_size = paste0(
    "The size of the figure area in inches. It keeps the ring the same ",
    "size."
  ),
  region = paste0(
    "The column with the codes of the municipalities, COROP regions or ",
    "provinces."
  ),
  value = "The column with the values that give the regions their colour.",
  level = "The kind of regions on the map.",
  border_colour = "The colour of the region borders.",
  border_linewidth = "The width of the region borders.",
  reverse = paste0(
    "TRUE turns the colours around, so high values get the light colour."
  ),
  na_fill = "The colour of regions without a value.",
  panel_fill = "The colour behind the figure.",
  value_axis_linewidth = "The width of the line of the value axis.",
  zero_indicator = "TRUE draws a thick white line at zero behind the data.",
  zero_indicator_linewidth = "The width of the white zero line.",
  ylab_position = "Where the unit is placed: left, middle or right."
)

# The values to choose from, for a parameter that takes a few words. Shown
# as the example. A parameter that is a function argument with several
# choices (position, orientation, ...) gets its choices from the formals.
choices <- list(
  page = c("half", "full", "small"),
  preset = c("report", "presentation", "map"),
  cut_labels = c("number", "euro", "pct"),
  palette = c("qualitative", "discr", "sequential", "blues", "extended"),
  legend = c("bottom", "right", "left", "top", "none"),
  facet_scales = c("fixed", "free", "free_x", "free_y"),
  style = c("dutch", "english"),
  ylab_position = c("left", "middle", "right")
)

# `a`, `b` or `c`
choice_text <- function(x) {
  q <- paste0("`", x, "`")
  if (length(x) == 1) return(q)
  paste0(paste(q[-length(q)], collapse = ", "), " or ", q[length(q)])
}

# Parameters read by the import itself, the same for every plot type:
# how the figure is saved, and the classes of `cut`.
import_rows <- data.frame(
  param = c("page", "height", "preset", "dpi", "lock", "cut", "cut_labels"),
  default = c("half", "", "report", "300", "TRUE", "", "number"),
  example = c(choice_text(choices$page), "10", choice_text(choices$preset),
              "800", "FALSE", "0;20;40;Inf", choice_text(choices$cut_labels)),
  stringsAsFactors = FALSE
)

fmt_default <- function(val) {
  if (is.symbol(val) && identical(as.character(val), "")) {
    return(list(kind = "required", text = "(required)", is_blank = FALSE))
  }
  if (is.null(val)) {
    return(list(kind = "blank", text = "", is_blank = TRUE))
  }
  # a call, e.g. c("stack", "dodge", "fill") or symbol_size (another
  # argument's own default, e.g. sec_symbol_size = symbol_size): evaluate a
  # literal vector, otherwise fall back to its deparsed source text
  evaluated <- tryCatch(eval(val), error = function(e) NULL)
  if (is.character(evaluated) && length(evaluated) > 1) {
    return(list(
      kind = "enum",
      text = evaluated[[1]], choices = evaluated,
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
    if (nm == "...") next
    if (nm == "data") {
      # in a CSV `data` is not the data frame but the dataset file name(s)
      # inside the data folder, so it is a literal here
      rows[[length(rows) + 1]] <- data.frame(
        plot_type = pt, param = "data", kind = "literal",
        default = "data", example = "koopkracht;werkloosheid",
        description = unname(descriptions["data"]),
        stringsAsFactors = FALSE
      )
      rows[[length(rows) + 1]] <- data.frame(
        plot_type = pt, import_rows[, "param", drop = FALSE],
        kind = "literal", import_rows[, c("default", "example")],
        description = unname(descriptions[import_rows$param]),
        stringsAsFactors = FALSE
      )
      next
    }
    is_column <- nm %in% col_args
    default <- fmt_default(f[[nm]])

    own_choices <- if (!is.null(default$choices)) default$choices else
      choices[[nm]]
    example <- if (is_column) {
      unname(column_examples[nm])
    } else if (!is.null(own_choices)) {
      choice_text(own_choices)
    } else if (default$kind %in% c("literal", "enum") && !default$is_blank) {
      default$text
    } else {
      unname(literal_examples[nm])
    }
    if (is.na(example)) example <- ""

    rows[[length(rows) + 1]] <- data.frame(
      plot_type = pt, param = nm,
      kind = if (is_column) "column (from dataset)" else "literal",
      default = default$text, example = example,
      description = unname(descriptions[nm]),
      stringsAsFactors = FALSE
    )
  }
}

out <- do.call(rbind, rows)
no_description <- unique(out$param[is.na(out$description)])
if (length(no_description)) {
  stop("No description for: ", paste(no_description, collapse = ", "),
       " (add them to `descriptions` in this script).", call. = FALSE)
}
write.csv(out, "inst/extdata/parameter_settings.csv", row.names = FALSE)
cat("Wrote inst/extdata/parameter_settings.csv (", nrow(out),
    " rows).\n", sep = "")

missing_example <- out[!nzchar(out$example),
                       c("plot_type", "param", "kind", "default")]
if (nrow(missing_example)) {
  cat("\nNo example value for these (add one to column_examples or ",
      "literal_examples in this script):\n", sep = "")
  print(missing_example, row.names = FALSE)
}
