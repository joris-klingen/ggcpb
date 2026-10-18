# ggcpb 0.2.0

This release gives every figure a fixed layout and cleans up argument
names. Code written for 0.1.0 needs the changes under "Breaking
changes". A removed argument is not always an error: chart functions
pass unknown arguments on to ggplot2, which only warns, and
`cpb_import_csv()` skips an unknown name in a parameter file with a
warning. After updating, check the warnings.

## Breaking changes

Renamed or removed:

| Before | Now |
|---|---|
| `import_csv()` | `cpb_import_csv()` |
| `cpb_area()` | no longer exported (`cpb_import_csv()` still draws `plot_type = "area"`) |
| `filllab`, `colourlab` | `legend_title` |
| `index` | `fill_index` (columns, boxes, histograms, maps, donuts) or `colour_index` (lines, scatters, dots) |
| `color_index` | `colour_index` |
| `sec_label` | `sec_legend_label` |
| `sec_point_size` | `sec_symbol_size` |
| `size` (`cpb_scatter()`, `cpb_dot()`), `point_size` (`cpb_line()`) | `symbol_size` |
| `point_colour` | `symbol_colour` |
| `legend_key_size` (charts, `theme_cpb()`) | removed: legend symbols have one fixed size |

Changed behaviour:

* Figures have a fixed layout: 7.5 cm (`page = "half"`), 15.5 cm
  (`"full"`) or 6.8 cm (`"small"`) wide, fixed margins, and a fixed top
  and bottom. The legend sits on a fixed grid. `save_cpb()` stops on a
  title or legend wider than the page (`lock = TRUE`, the default).
* The value axis always covers the data. `value_limits` is a minimum:
  when the data fits inside it the axis shows exactly those limits,
  otherwise it widens to round values and says so. Neither
  `value_limits` nor `value_breaks` hides a value any more.
* Every continuous axis and colour bar labels its lowest and highest
  value, with equal steps. A flush axis never runs past the data.
* The zero line is drawn only when zero is on the axis.
* ggplot2 3.5.2 or newer is required.

## New features

* `palette = "extended"` gives 20 categorical colours, and
  `cpb_cols()` also finds them by name.
* `style = "english"` works in every chart, and
  `options(ggcpb.style = "english")` sets it for a whole report.
* `value_crop = TRUE` turns `value_limits` into a window: the axis is
  exactly those limits and values outside are cut off, with a warning
  that not all data is shown.
* `cpb_scatter()` and `cpb_hist()` have `value_limits`, `value_breaks`
  and `value_accuracy`; `cpb_scatter()` also has `pct_axis` and
  `x_labels`.
* The right axis (`sec_y`) places its ticks on the gridlines of the left
  axis. New arguments: `sec_at`, `sec_scale_auto`, `sec_axis_labels`.
  Its series is named in the legend.
* `legend_title` and `legend_nrow` work in every chart. `cpb_map()` has
  `reverse_legend`, `legend_ncol` and `legend_nrow`.
* A `cpb_map()` figure has the size of the page, and
  `save_cpb(preset = "map")` makes it as tall as the map needs.
* `cpb_donut()` uses the same layout as the other charts.
* `save_cpb()` warns when a plot does not suit a half page or when
  category labels are too long to sit side by side.

## Fixes

* A tick label names the value its tick sits at, on every axis and
  legend (within 0.1% of the axis length).
* The axis of a stacked chart covers the stacks ggplot2 draws, also
  with missing values and facets.
* The right axis widens when its series runs past `sec_limits`, instead
  of cutting the series off. A constant `sec_y` gives a clear error.
* A chart drawn on screen looks like the saved one.
