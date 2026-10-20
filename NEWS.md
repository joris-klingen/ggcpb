# ggcpb 0.2.0

## Changes

Renamed or removed:

| Before | Now |
|---|---|
| `import_csv()` | `cpb_import_csv()` |
| `cpb_area()` | no longer exported (`cpb_import_csv()` still draws `plot_type = "area"`) |
| `filllab`, `colourlab` | `legend_title` |
| `index`, `fill_index`, `color_index` | `colour_index` (every chart) |
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
  value, with equal steps.
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
  and `value_accuracy`. `cpb_scatter()` also has `pct_axis` and
  `x_labels`.
* The right axis (`sec_y`) places its ticks on the gridlines of the left
  axis. New arguments: `sec_at`, `sec_scale_auto`, `sec_axis_labels`.
  Its series is named in the legend.
* `legend_title` and `legend_nrow` work in every chart. `cpb_map()` has
  `reverse_legend`, `legend_ncol` and `legend_nrow`.
* `legend = "filled-empty"` draws no legend but keeps its space, so a figure
  without a legend has the same panel height as one with a legend.
* `cpb_boxplot_extended()` puts the axis title at same width as the value axis.
* A `cpb_map()` figure has the size of the page, and
  `save_cpb(preset = "map")` makes it as tall as the map needs.
* `cpb_donut()` uses the same layout as the other charts.
* `save_cpb()` warns when a plot does not suit a half page or when
  category labels are too long to sit side by side.

## Fixes

* The axis of a stacked chart covers the stacks ggplot2 draws, also
  with missing values and facets.
* * The right axis (`sec_y`) widens when its series runs past `sec_limits`
  instead of cutting it off, so it shows all possible observations. A `sec_y` containing only observations with the exact same values stops with a clear error (non-unique data set).
