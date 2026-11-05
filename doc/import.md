Building figures from CSV files
================

``` r
library(ggcpb)
```

# Introduction

Most figures in this package are built by calling one of its functions
directly in R code. This vignette describes an alternative based on CSV
files. The datasets are CSV files in a folder named `data`. One
parameter file, `params.csv`, specifies which figures to create and how
they should be configured. The `cpb_import_csv()` function reads them
and creates the figures.

This approach is useful when figure specifications need to be kept
separate from R code, for example when the data and figure settings are
maintained outside an R script.

# The figure folder

A figure folder holds `params.csv` and a `data` folder. The function
`autogenerate_plots()` creates a ready to use example. It is used
throughout this vignette.

``` r
figure_dir <- file.path(tempdir(), "figures")
autogenerate_plots(figure_dir)
params_csv <- file.path(figure_dir, "params.csv")
```

``` r
list.files(figure_dir, recursive = TRUE)
#> [1] "data/koopkracht.csv"   "data/werkloosheid.csv" "params.csv"           
#> [4] "run_import.bat"        "run_import.command"    "run_import.R"
```

In normal use, the folder lives somewhere you can write to, and the
start scripts (`run_import.bat` and `run_import.command`) run everything
for you. See “Run the import without opening R” below.

# The data files

Each dataset is a CSV file in the `data` folder. It can have any name
and holds the variables needed by the figures. The first row holds the
column names. The package example has two datasets.

``` r
read.csv(file.path(figure_dir, "data", "koopkracht.csv"))
#>    jaar           groep koopkracht werkloosheid
#> 1  2021       werkenden        0.3          3.4
#> 2  2021 gepensioneerden       -0.2          3.4
#> 3  2021       uitkering       -0.9          3.4
#> 4  2022       werkenden       -2.7          3.5
#> 5  2022 gepensioneerden       -2.3          3.5
#> 6  2022       uitkering       -3.2          3.5
#> 7  2023       werkenden        1.5          3.6
#> 8  2023 gepensioneerden        0.8          3.6
#> 9  2023       uitkering       -0.3          3.6
#> 10 2024       werkenden        1.2          3.7
#> 11 2024 gepensioneerden        0.6          3.7
#> 12 2024       uitkering       -0.5          3.7
#> 13 2025       werkenden        0.9          3.7
#> 14 2025 gepensioneerden        0.4          3.7
#> 15 2025       uitkering       -0.6          3.7
```

The important point for the import method is that the parameter file
refers to variables by their column names. For example, if a parameter
contains `jaar`, `cpb_import_csv()` looks for a column named `jaar` in
the dataset of that figure.

Text columns keep the order of the rows. The first group in the file is
the first on the axis and in the legend. A number used as `fill`,
`group` or `colour` (a year, for example) is treated as a category. A
numeric `colour` in a scatter plot keeps its colour gradient.

Save datasets from Excel as `CSV UTF-8 (comma delimited)`.

# The parameter file

The parameter file determines which figures are created. Each figure has
a `plot_type` and an `id`, together with the settings required for that
plot type. The parameter `data` names the dataset of the figure.

The first column holds the parameter names. Every other column describes
one figure. This is the example file:

    plot_type,col,line
    title,Koopkrachtontwikkeling naar groep,Werkloosheid
    x,jaar,jaar
    y,koopkracht,werkloosheid
    fill,groep
    ylab,% mutatie,%
    position,dodge
    sec_y,werkloosheid
    sec_type,line
    sec_legend_label,werkloosheid
    sec_ylab,%
    create,y,y
    data,koopkracht,werkloosheid

The same content, split on the comma and shown as a table:

| parameter        | figure.1                          | figure.2     |
|:-----------------|:----------------------------------|:-------------|
| plot_type        | col                               | line         |
| title            | Koopkrachtontwikkeling naar groep | Werkloosheid |
| x                | jaar                              | jaar         |
| y                | koopkracht                        | werkloosheid |
| fill             | groep                             |              |
| ylab             | % mutatie                         | %            |
| position         | dodge                             |              |
| sec_y            | werkloosheid                      |              |
| sec_type         | line                              |              |
| sec_legend_label | werkloosheid                      |              |
| sec_ylab         | %                                 |              |
| create           | y                                 | y            |
| data             | koopkracht                        | werkloosheid |

A cell can be left blank. Fields at the end of a row can be left out.

The parameter `page` sets the page width a figure is saved at: `half`
(7.5 cm, the default), `full` (15.5 cm) or `small`. Leave it out for
`half`.

Some parameters refer to columns in the dataset. For example:

- `x`, `y`, `fill` and `sec_y` are column references.
- `title`, `ylab`, `position` and `data` are literal values.

The distinction is important. The cell next to `y` says `koopkracht`, so
the `y` variable is taken from the `koopkracht` column. The cell next to
`ylab` says `% mutatie`, so that text is used directly.

# Creating the figures

Pass the parameter file to `cpb_import_csv()`. The datasets are read
from the `data` folder next to it:

``` r
figs <- cpb_import_csv(params_csv)
```

When the parameter file contains multiple figures, `cpb_import_csv()`
returns a named list. The names come from the datasets. Here the two
figures use different datasets, so they are called `koopkracht` and
`werkloosheid`.

The `koopkracht` figure has a second value axis (`sec_y`), and its right
side caption is only placed exactly by `save_cpb()`. Printed directly,
an approximate placement is shown instead, close but not exact, since at
that point the figure has not actually been saved yet to measure
against. So, rather than printing it directly, it is written out through
`save_cpb()` first and shown from that file:

``` r
path <- tempfile(fileext = ".png")
save_cpb(path, figs$koopkracht, page = "half")
```

<img src="import_files/figure-gfm/fig-koopkracht-show-1.png" alt="" width="350px" />

`werkloosheid` has no second axis, so it can be shown directly:

``` r
figs$werkloosheid
```

<img src="import_files/figure-gfm/fig-werkloosheid-1.png" alt="" width="350px" />

# Several datasets

The parameter `data` names the dataset of a figure, relative to the
`data` folder next to `params.csv`. The extension `.csv` is optional, so
`koopkracht` and `koopkracht.csv` are the same dataset.

| `data` parameter | What happens                                |
|------------------|---------------------------------------------|
| `koopkracht`     | The figure uses `data/koopkracht.csv`.      |
| `a;b`            | One figure is made from each named dataset. |
| blank            | The figure uses `data/data.csv`.            |
| column absent    | The same as blank.                          |

The figures are named after their dataset. If several figures use the
same dataset, the figure number is added to the name. The first figure
column in `params.csv` is number 1. For example, `koopkracht_1.png` and
`koopkracht_3.png`.

A named dataset that does not exist in the `data` folder is skipped with
a warning. Nothing falls back to another dataset. The other datasets in
the same list and the other rows are still created. With `data` set to
`a;b;c` and no `b.csv`, you get figures for `a` and `c`, and a warning
about `b.csv`.

# A parameter file with one figure

A parameter file can describe a single figure. It then has one column of
values next to the parameter names. For example, the following describes
a line chart. It can be saved in the same folder, so it finds the same
`data` folder:

``` r
params_one <- file.path(figure_dir, "params_one.csv")

writeLines(c(
  "plot_type,line",
  "title,Werkloosheid",
  "data,werkloosheid",
  "x,jaar",
  "y,werkloosheid",
  "ylab,%"
), params_one)

cat(readLines(params_one), sep = "\n")
#> plot_type,line
#> title,Werkloosheid
#> data,werkloosheid
#> x,jaar
#> y,werkloosheid
#> ylab,%
```

When the parameter file describes one figure, `cpb_import_csv()` returns
that figure directly rather than a named list. Use `simplify = FALSE` to
always get a named list.

``` r
fig_one <- cpb_import_csv(params_one)
```

<img src="import_files/figure-gfm/one-figure-show-1.png" alt="" width="350px" />

# Handling problems

A parameter can be left blank. In that case, the default value is used.

If a parameter is not valid for the selected `plot_type`, it is ignored
and the default is used. These cases produce a warning.

If a figure cannot be created, for example because a column referenced
by a parameter does not exist in the dataset, that figure is skipped.
Other figures in the same parameter file are still created.

Each call to `cpb_import_csv()` also writes a log file next to the
parameter file. The log records figures that were created or skipped and
any parameter problems.

# Run the import without opening R

The same CSV based method can also be used without opening R.

The `autogenerate_plots()` function creates a ready to use folder
containing:

- a `data` folder with the datasets
- a parameter file, `params.csv`
- a script for Windows (a `.bat` file)
- a script for a Mac (a `.command` file)

After editing the datasets and the parameter file, run the appropriate
script. It reads `params.csv`, builds every figure and saves the images
in the folder `generated`. Each image is named after its dataset.

Running the script again produces updated figures. Images with the same
name are replaced. A dataset or figure that fails is skipped and listed
at the end, and in `params_log.txt`. The other figures are still saved.

R and `ggcpb` must still be installed on the computer running the
script.

# Appendix A: Parameter values

The available parameters depend on the `plot_type`. The complete list is
stored in `parameter_settings.csv`, and gives the available parameters,
their kind, default values and examples.

``` r
ref_csv <- system.file(
  "extdata",
  "parameter_settings.csv",
  package = "ggcpb"
)
```

Many parameters are the same across every `plot_type`. To avoid
repeating those in every table, Table A1 lists the parameters shared,
with the same meaning and default, by all plot types. Table A2 onward
each cover one `plot_type`, listing only what is not already in Table
A1. A parameter is only listed for the plot types it actually applies
to, so a table for one `plot_type` does not necessarily match another.

**Table A1. Shared parameters across all plot types**

| setting | kind | default | example |
|:---|:---|:---|:---|
| colour_index | literal |  | 2;6 |
| data | literal | data | koopkracht;werkloosheid |
| flush_legend | literal | TRUE | TRUE |
| legend | literal | bottom | bottom |
| legend_ncol | literal |  | 2 |
| legend_nrow | literal |  |  |
| legend_title | literal |  | title of the legend |
| page | literal | half | full |
| palette | literal | qualitative | qualitative |
| style | literal | dutch | dutch |
| subtitle | literal |  | subtitle (replaces the default unit above the figure) |
| title | literal |  | Title of the figure |

**Table A2. Parameters specific to `col`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| facet | column (from dataset) |  | region |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from dataset) |  | group |
| fill_colour | literal |  | \#005faf |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from dataset) |  | sector |
| group_gap | literal | 0.8 | 0.8 |
| minor | literal | FALSE | FALSE |
| orientation | literal | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| pct_axis | literal | FALSE | FALSE |
| position | literal | stack/dodge/fill (default: stack) | stack/dodge/fill (default: stack) |
| reverse_legend | literal | TRUE | TRUE |
| sec_accuracy | literal |  | 0.1 |
| sec_at | literal |  |  |
| sec_axis_labels | literal |  |  |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_legend_label | literal |  | name of the second series |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_points | literal | FALSE | FALSE |
| sec_scale_auto | literal | TRUE | TRUE |
| sec_symbol_size | literal | 1.6 | 1.6 |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from dataset) |  | unemployment |
| sec_ylab | literal |  | unit of the right axis |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_crop | literal | FALSE | FALSE |
| value_labels | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| x | column (from dataset) | (required) | year |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | unit below the x axis |
| y | column (from dataset) | (required) | purchasing_power |
| ylab | literal |  | % change |
| zeroline | literal | TRUE | TRUE |

**Table A3. Parameters specific to `area`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| facet | column (from dataset) |  | region |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from dataset) | (required) | group |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| minor | literal | FALSE | FALSE |
| pct_axis | literal | FALSE | FALSE |
| reverse_legend | literal | TRUE | TRUE |
| sec_accuracy | literal |  | 0.1 |
| sec_at | literal |  |  |
| sec_axis_labels | literal |  |  |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_legend_label | literal |  | name of the second series |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_points | literal | FALSE | FALSE |
| sec_scale_auto | literal | TRUE | TRUE |
| sec_symbol_size | literal | 1.6 | 1.6 |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from dataset) |  | unemployment |
| sec_ylab | literal |  | unit of the right axis |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_crop | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| x | column (from dataset) | (required) | year |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | unit below the x axis |
| y | column (from dataset) | (required) | purchasing_power |
| ylab | literal |  | % change |
| zeroline | literal | TRUE | TRUE |

**Table A4. Parameters specific to `line`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| colour | column (from dataset) |  | group |
| facet | column (from dataset) |  | region |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| line_colour | literal |  | \#005faf |
| linewidth | literal | 0.55 | 0.55 |
| minor | literal | FALSE | FALSE |
| pct_axis | literal | FALSE | FALSE |
| points | literal | FALSE | FALSE |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_at | literal |  |  |
| sec_axis_labels | literal |  |  |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_legend_label | literal |  | name of the second series |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal |  | 0.55 |
| sec_points | literal | FALSE | FALSE |
| sec_scale_auto | literal | TRUE | TRUE |
| sec_symbol_size | literal | 1.6 | 1.6 |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from dataset) |  | unemployment |
| sec_ylab | literal |  | unit of the right axis |
| symbol_size | literal | 1.1 | 1.1 |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_crop | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| x | column (from dataset) | (required) | year |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | unit below the x axis |
| y | column (from dataset) | (required) | purchasing_power |
| ylab | literal |  | % change |
| ymax | column (from dataset) |  | upper_bound |
| ymin | column (from dataset) |  | lower_bound |
| zeroline | literal |  | TRUE |

**Table A5. Parameters specific to `box`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| box_labels | literal |  | TRUE |
| box_style | literal | ggcpb/james/modern/dot (default: ggcpb) | ggcpb/james/modern/dot (default: ggcpb) |
| dot_labels | literal |  | p5:bottom 5%;p95:top 5% |
| facet | column (from dataset) |  | region |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from dataset) |  | group |
| fill_colour | literal |  | \#005faf |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from dataset) |  | sector |
| group_gap | literal | 0.7 | 0.7 |
| label_accuracy | literal | 0.1 | 0.1 |
| linewidth | literal | 0.25 | 0.25 |
| mean | column (from dataset) |  | mean |
| minor | literal | FALSE | FALSE |
| orientation | literal | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| p25 | column (from dataset) | (required) | p25 |
| p5 | column (from dataset) | (required) | p5 |
| p50 | column (from dataset) | (required) | p50 |
| p75 | column (from dataset) | (required) | p75 |
| p95 | column (from dataset) | (required) | p95 |
| pct_axis | literal | FALSE | FALSE |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_at | literal |  |  |
| sec_axis_labels | literal |  |  |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_legend_label | literal |  | name of the second series |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_points | literal | FALSE | FALSE |
| sec_scale_auto | literal | TRUE | TRUE |
| sec_symbol_size | literal | 1.6 | 1.6 |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from dataset) |  | unemployment |
| sec_ylab | literal |  | unit of the right axis |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_axis | literal | bottom/top (default: bottom) | bottom/top (default: bottom) |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_crop | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| width | literal | 0.5 | 0.5 |
| x | column (from dataset) | (required) | year |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | unit below the x axis |
| ylab | literal |  | % change |
| zeroline | literal |  | TRUE |

**Table A6. Parameters specific to `dot`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| cap_width | literal | 0.25 | 0.25 |
| colour | column (from dataset) |  | group |
| facet | column (from dataset) |  | region |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from dataset) |  | sector |
| group_gap | literal | 0.7 | 0.7 |
| linewidth | literal | 0.4 | 0.4 |
| lower | column (from dataset) | (required) | lower_bound |
| minor | literal | FALSE | FALSE |
| orientation | literal | horizontal/vertical (default: horizontal) | horizontal/vertical (default: horizontal) |
| pct_axis | literal | FALSE | FALSE |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_at | literal |  |  |
| sec_axis_labels | literal |  |  |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_legend_label | literal |  | name of the second series |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_points | literal | FALSE | FALSE |
| sec_scale_auto | literal | TRUE | TRUE |
| sec_symbol_size | literal | symbol_size | symbol_size |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from dataset) |  | unemployment |
| sec_ylab | literal |  | unit of the right axis |
| symbol_colour | literal |  | \#e6006e |
| symbol_size | literal | 1.4 | 1.4 |
| ticks | literal | TRUE | TRUE |
| upper | column (from dataset) | (required) | upper_bound |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_crop | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| x | column (from dataset) | (required) | year |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | unit below the x axis |
| y | column (from dataset) | (required) | purchasing_power |
| ylab | literal |  | % change |
| zeroline | literal |  | TRUE |

**Table A7. Parameters specific to `scatter`**

| setting           | kind                  | default    | example               |
|:------------------|:----------------------|:-----------|:----------------------|
| axis_text_size    | literal               | 7          | 7                     |
| colour            | column (from dataset) |            | group                 |
| facet             | column (from dataset) |            | region                |
| facet_ncol        | literal               |            | 2                     |
| facet_scales      | literal               | fixed      | fixed                 |
| forecast_label    | literal               | raming     | raming                |
| forecast_x        | literal               |            | 2025                  |
| grid_colour       | literal               | black      | black                 |
| grid_linewidth    | literal               | 0.1        | 0.1                   |
| minor             | literal               | FALSE      | FALSE                 |
| pct_axis          | literal               | FALSE      | FALSE                 |
| reverse_legend    | literal               | FALSE      | FALSE                 |
| symbol_colour     | literal               |            | \#e6006e              |
| symbol_size       | literal               | 0.8        | 0.8                   |
| ticks             | literal               | TRUE       | TRUE                  |
| value_accuracy    | literal               |            | 0.1                   |
| value_breaks      | literal               |            | 0;25;50;75;100        |
| value_crop        | literal               | FALSE      | FALSE                 |
| value_limits      | literal               |            | 0;100                 |
| x                 | column (from dataset) | (required) | year                  |
| x_lim             | literal               |            | 2015;2025             |
| x_lim_follow_data | literal               | FALSE      | FALSE                 |
| xlab              | literal               |            | unit below the x axis |
| y                 | column (from dataset) | (required) | purchasing_power      |
| ylab              | literal               |            | % change              |
| zeroline          | literal               |            | TRUE                  |

**Table A8. Parameters specific to `hist`**

| setting           | kind                  | default    | example               |
|:------------------|:----------------------|:-----------|:----------------------|
| axis_text_size    | literal               | 7          | 7                     |
| bins              | literal               |            | 30                    |
| binwidth          | literal               |            | 5                     |
| facet             | column (from dataset) |            | region                |
| facet_ncol        | literal               |            | 2                     |
| facet_scales      | literal               | fixed      | fixed                 |
| fill              | column (from dataset) |            | group                 |
| fill_colour       | literal               |            | \#005faf              |
| grid_colour       | literal               | black      | black                 |
| grid_linewidth    | literal               | 0.1        | 0.1                   |
| minor             | literal               | FALSE      | FALSE                 |
| outline           | literal               | white      | white                 |
| position          | literal               | stack      | stack                 |
| reverse_legend    | literal               | TRUE       | TRUE                  |
| ticks             | literal               | TRUE       | TRUE                  |
| value_accuracy    | literal               |            | 0.1                   |
| value_breaks      | literal               |            | 0;25;50;75;100        |
| value_crop        | literal               | FALSE      | FALSE                 |
| value_limits      | literal               |            | 0;100                 |
| x                 | column (from dataset) | (required) | year                  |
| x_lim             | literal               |            | 2015;2025             |
| x_lim_follow_data | literal               | FALSE      | FALSE                 |
| xlab              | literal               |            | unit below the x axis |
| ylab              | literal               |            | % change              |
| zeroline          | literal               | TRUE       | TRUE                  |

**Table A9. Parameters specific to `donut`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| fill | column (from dataset) | (required) | group |
| label | column (from dataset) |  | name |
| label_accuracy | literal | 1 | 1 |
| label_colour | literal | black | black |
| label_style | literal | wedge/leader (default: wedge) | wedge/leader (default: wedge) |
| leader_length | literal | 0.15 | 0.15 |
| legend_pct | literal | FALSE | FALSE |
| panel_size | literal |  |  |
| reverse_legend | literal | FALSE | FALSE |
| ring_width | literal | 0.6 | 0.6 |
| wedge_labels | literal | TRUE | TRUE |
| y | column (from dataset) | (required) | purchasing_power |

A parameter of kind `column` takes a column name from the data file. A
parameter of kind `literal` takes a value directly from the parameter
file.

The `parameter_settings.csv` file can also be opened directly in a
spreadsheet program.
