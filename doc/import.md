Building figures from CSV files
================

- [Introduction](#introduction)
  - [How to create a figure without opening
    R?](#how-to-create-a-figure-without-opening-r)
- [The dataset](#the-dataset)
- [The parameter file](#the-parameter-file)
  - [Plot types](#plot-types)
  - [Special values](#special-values)
  - [Classes](#classes)
  - [Saving](#saving)
- [Creating the figures in R](#creating-the-figures-in-r)
- [Several datasets](#several-datasets)
- [A parameter file with one figure](#a-parameter-file-with-one-figure)
- [Handling problems](#handling-problems)
- [Appendix A: Parameters](#appendix-a-parameters)

``` r
library(ggcpb)
```

# Introduction

Figures can be created directly with or without using R. This vignette
described the method to create figures **using only Excel**.

To create a figure, you need:

- a `params.csv` file
  - this is the parameter file, which specifies which figures to create
    and how they should be configured
  - one `params.csv` can contain the parameters for several figures
- a `data` folder with the dataset(s)
  - each dataset is a CSV file
- the `run_import.bat` script

*Please note*: The datasets must be saved as CSV, not as spreadsheet
(`xlsx`). In Excel, choose `File` -\> `Save as` -\>
`CSV UTF-8 (comma delimited)`.

## How to create a figure without opening R?

The general workflow to create figures with ggcpb is as follows:

1.  Find the most recent ggcpb folder in `[path]`.
2.  Copy it to the place where you want to create figures.
3.  Put your datasets in the folder `data`. Open `params.csv` in Excel
    and add your parameters.
4.  To create a figure, double clik on `run_import.bat`. This reads
    `params.csv`, builds every figure and saves the images in the folder
    `generated`. Each image is named after its dataset. Running the
    script again produces updated figures. Images with the same name are
    replaced. A dataset or figure that fails is skipped and listed at
    the end, and in `params_log.txt`. The other figures are still saved.

R and `ggcpb` must still be installed on the computer running the
script.

# The dataset

Each dataset is a CSV file in the `data` folder. It can have any name
and holds the variables needed by the figures. The first row holds the
column names. The package example has two datasets. One of the example
datasets is shown here:

| jaar | groep           | koopkracht | werkloosheid |
|-----:|:----------------|-----------:|-------------:|
| 2021 | werkenden       |        0.3 |          3.4 |
| 2021 | gepensioneerden |       -0.2 |          3.4 |
| 2021 | uitkering       |       -0.9 |          3.4 |
| 2022 | werkenden       |       -2.7 |          3.5 |
| 2022 | gepensioneerden |       -2.3 |          3.5 |
| 2022 | uitkering       |       -3.2 |          3.5 |
| 2023 | werkenden       |        1.5 |          3.6 |
| 2023 | gepensioneerden |        0.8 |          3.6 |
| 2023 | uitkering       |       -0.3 |          3.6 |
| 2024 | werkenden       |        1.2 |          3.7 |
| 2024 | gepensioneerden |        0.6 |          3.7 |
| 2024 | uitkering       |       -0.5 |          3.7 |
| 2025 | werkenden       |        0.9 |          3.7 |
| 2025 | gepensioneerden |        0.4 |          3.7 |
| 2025 | uitkering       |       -0.6 |          3.7 |

The parameter file refers to variables by their column names.
Categorical columns (e.g. `regio`, `geslacht`) keep the order of the
rows in the created figure. So, the first group in the CSV-file
(e.g. here `groep`) is the first on the axis and in the legend.

# The parameter file

The parameter file determines which figures are created. Each figure has
a `plot_type`, together with the settings required for that plot type.
The parameter `data` names the dataset of the figure.

The first column holds the parameter names. Every other column describes
one figure. This is the example parameter file. The first row, with the
plot types, is the header.

| plot_type        | col                               | line         |
|:-----------------|:----------------------------------|:-------------|
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
| style            |                                   | english      |

This table shows that there are two figures. The first figure is a
column chart (`plot_type` = `col`) that uses the `koopkracht` dataset
(`data`) and that portrays `werkloosheid` on the second y-axis
(`sec_y`). The second figure is a line chart (`plot_type` = `line`) that
uses the `werkloosheid` dataset (`data`) and has no secondary y-axis. A
parameter value can be left blank, such as `sec_y` in the second plot.
Only the second figure sets `style` to `english`, so it gets English
labels and number formats.

The parameter `page` sets the page width a figure is saved at: `half`
(7.5 cm, the default), `full` (15.5 cm) or `small`. If left out, the
default `half` is used.

## Plot types

`plot_type` names the figure type: `line`, `col`, `area`, `box`,
`boxplot_extended`, `dot`, `scatter`, `hist`, `donut` or `map`. Each
type is the `cpb_` function of the same name, such as `cpb_line()`.
Every argument of that function can be a parameter. Appendix A lists
them all per type.

## Special values

Most values are plain text, a number, `TRUE` or `FALSE`. A few need a
special form:

| Value | Example | What happens |
|----|----|----|
| a list | `0;100` | `;` separates the values. |
| a line break | `First line\nsecond line` | `\n` starts a new line. |
| no upper limit | `0;20;Inf` | `Inf` and `-Inf` are numbers. |
| a formatter | `euro` | `x_labels` and `sec_axis_labels` take `number`, `euro` or `pct`. |
| pairs | `p5:onderste 5%` | `dot_labels` takes name:value pairs. |
| a date | `2024-01-31` | A dataset column of dates gives a date axis. `31-1-2024` works too. |

## Classes

The parameter `cut` puts numbers into classes, such as `0;20;30;40;Inf`.
That gives the classes “lager dan 20”, “20 - 30”, “30 - 40” and “40 en
hoger”, or “below 20” and “40 and above” with `style = english`.
`cut_labels` writes them as `number` (default), `euro` or `pct`. It
works on the column that sets the colours: `value` of a map, `colour` of
a line, dot or scatter plot, and `fill` of the other types. The numbers
themselves stay available, for example on the y axis.

## Saving

Next to `page`, four parameters say how a figure is saved:

| Parameter | Example | What happens |
|----|----|----|
| `height` | `10` | The height in cm. Left out, the page height is used. |
| `preset` | `map` | `map` makes a map as tall as it needs. `presentation` is for slides. |
| `dpi` | `800` | The resolution. The default is 300. |
| `lock` | `FALSE` | A title that is too wide only gives a warning, instead of an error. |

# Creating the figures in R

Pass the parameter file to `cpb_import_csv()`. The datasets are read
from the `data` folder next to it:

``` r
figure_dir <- file.path(tempdir(), "figures")
params_csv <- file.path(figure_dir, "params.csv")
figs <- cpb_import_csv(params_csv)
```

When the parameter file contains multiple figures, `cpb_import_csv()`
returns a named list. The names come from the datasets. Here the two
figures use different datasets, so they are called `koopkracht` and
`werkloosheid`.

`save_cpb()` precisely positions the secondary-axis caption in the
`koopkracht` figure, so the figure is saved using `save_cpb()` rather
than printed directly.

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

# Appendix A: Parameters

The available parameters depend on the `plot_type`. The complete list is
stored in `parameter_settings.csv`, and gives the available parameters,
their default values and examples.

Many parameters are the same across every `plot_type`. Table A1 lists
the parameters shared, with the same meaning and default, by all plot
types. Table A2 onward each cover one `plot_type`, listing only what is
not already in Table A1.

<span class="table-title">**Table A1. Shared parameters across all plot
types**</span>

| parameter | default | example |
|:---|:---|:---|
| colour_index |  | 2;6 |
| cut |  | 0;20;40;Inf |
| cut_labels | number/euro/pct (default: number) | pct |
| data | data | koopkracht;werkloosheid |
| dpi | 300 | 800 |
| flush_legend | TRUE | TRUE |
| height |  | 10 (cm) |
| legend_ncol |  | 2 |
| legend_nrow |  | 2 |
| legend_title |  | title of the legend |
| lock | TRUE | FALSE |
| page | half | full |
| preset | report/presentation/map (default: report) | map |
| style | dutch | dutch |
| subtitle |  | subtitle (replaces the default unit above the figure) |
| title |  | Title of the figure |

<span class="table-title">**Table A2. Parameters specific to
`col`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| fill |  | group |
| fill_colour |  | \#005faf |
| forecast_label | raming | raming |
| forecast_x |  | 2025 |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| group |  | sector |
| group_gap | 0.8 | 0.8 |
| legend | bottom | bottom |
| minor | FALSE | FALSE |
| orientation | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| position | stack/dodge/fill (default: stack) | stack/dodge/fill (default: stack) |
| reverse_legend | TRUE | TRUE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth | 0.55 | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | 1.6 | 1.6 |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| ticks | TRUE | TRUE |
| value_accuracy |  | 0.1 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_labels | FALSE | FALSE |
| value_limits |  | 0;100 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | FALSE | FALSE |
| xlab |  | unit below the x axis |
| y | (required) | purchasing_power |
| ylab |  | % change |
| zeroline | TRUE | TRUE |

<span class="table-title">**Table A3. Parameters specific to
`area`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| fill | (required) | group |
| forecast_label | raming | raming |
| forecast_x |  | 2025 |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| legend | bottom | bottom |
| minor | FALSE | FALSE |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| reverse_legend | TRUE | TRUE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth | 0.55 | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | 1.6 | 1.6 |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| ticks | TRUE | TRUE |
| value_accuracy |  | 0.1 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | FALSE | FALSE |
| xlab |  | unit below the x axis |
| y | (required) | purchasing_power |
| ylab |  | % change |
| zeroline | TRUE | TRUE |

<span class="table-title">**Table A4. Parameters specific to
`line`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| colour |  | group |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| forecast_label | raming | raming |
| forecast_x |  | 2025 |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| legend | bottom | bottom |
| line_colour |  | \#005faf |
| linewidth | 0.55 | 0.55 |
| minor | FALSE | FALSE |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| points | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth |  | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | 1.6 | 1.6 |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| symbol_size | 1.1 | 1.1 |
| ticks | TRUE | TRUE |
| value_accuracy |  | 0.1 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | TRUE | TRUE |
| xlab |  | unit below the x axis |
| y | (required) | purchasing_power |
| ylab |  | % change |
| ymax |  | upper_bound |
| ymin |  | lower_bound |
| zeroline |  | TRUE |

<span class="table-title">**Table A5. Parameters specific to
`box`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| box_labels |  | TRUE |
| box_style | ggcpb/james/modern/dot (default: ggcpb) | ggcpb/james/modern/dot (default: ggcpb) |
| dot_labels |  | p5:bottom 5%;p95:top 5% |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| fill |  | group |
| fill_colour |  | \#005faf |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| group |  | sector |
| group_gap | 0.7 | 0.7 |
| label_accuracy | 0.1 | 0.1 |
| legend | bottom | bottom |
| linewidth | 0.25 | 0.25 |
| mean |  | mean |
| minor | FALSE | FALSE |
| orientation | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| p25 | (required) | p25 |
| p5 | (required) | p5 |
| p50 | (required) | p50 |
| p75 | (required) | p75 |
| p95 | (required) | p95 |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth | 0.55 | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | 1.6 | 1.6 |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| ticks | TRUE | TRUE |
| value_accuracy |  | 0.1 |
| value_axis | bottom/top (default: bottom) | bottom/top (default: bottom) |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| width | 0.5 | 0.5 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | TRUE | TRUE |
| xlab |  | unit below the x axis |
| ylab |  | % change |
| zeroline |  | TRUE |

<span class="table-title">**Table A6. Parameters specific to
`dot`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| cap_width | 0.25 | 0.25 |
| colour |  | group |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| group |  | sector |
| group_gap | 0.7 | 0.7 |
| legend | bottom | bottom |
| linewidth | 0.4 | 0.4 |
| lower | (required) | lower_bound |
| minor | FALSE | FALSE |
| orientation | horizontal/vertical (default: horizontal) | horizontal/vertical (default: horizontal) |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth | 0.55 | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | symbol_size | symbol_size |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| symbol_colour |  | \#e6006e |
| symbol_size | 1.4 | 1.4 |
| ticks | TRUE | TRUE |
| upper | (required) | upper_bound |
| value_accuracy |  | 0.1 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | TRUE | TRUE |
| xlab |  | unit below the x axis |
| y | (required) | purchasing_power |
| ylab |  | % change |
| zeroline |  | TRUE |

<span class="table-title">**Table A7. Parameters specific to
`scatter`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| colour |  | group |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| forecast_label | raming | raming |
| forecast_x |  | 2025 |
| grid_colour | black | black |
| grid_linewidth | 0.1 | 0.1 |
| legend | bottom | bottom |
| minor | FALSE | FALSE |
| palette | qualitative | qualitative |
| pct_axis | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| symbol_colour |  | \#e6006e |
| symbol_size | 0.8 | 0.8 |
| ticks | TRUE | TRUE |
| value_accuracy |  | 0.1 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| x | (required) | year |
| x_labels |  | euro (or number, pct, or labels such as a;b;c) |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | FALSE | FALSE |
| xlab |  | unit below the x axis |
| y | (required) | purchasing_power |
| ylab |  | % change |
| zeroline |  | TRUE |

<span class="table-title">**Table A8. Parameters specific to
`hist`**</span>

| parameter         | default     | example               |
|:------------------|:------------|:----------------------|
| axis_text_size    | 7           | 7                     |
| bins              |             | 30                    |
| binwidth          |             | 5                     |
| facet             |             | region                |
| facet_ncol        |             | 2                     |
| facet_scales      | fixed       | fixed                 |
| fill              |             | group                 |
| fill_colour       |             | \#005faf              |
| grid_colour       | black       | black                 |
| grid_linewidth    | 0.1         | 0.1                   |
| legend            | bottom      | bottom                |
| minor             | FALSE       | FALSE                 |
| outline           | white       | white                 |
| palette           | qualitative | qualitative           |
| position          | stack       | stack                 |
| reverse_legend    | TRUE        | TRUE                  |
| ticks             | TRUE        | TRUE                  |
| value_accuracy    |             | 0.1                   |
| value_breaks      |             | 0;25;50;75;100        |
| value_crop        | FALSE       | FALSE                 |
| value_limits      |             | 0;100                 |
| x                 | (required)  | year                  |
| x_lim             |             | 2015;2025             |
| x_lim_follow_data | FALSE       | FALSE                 |
| xlab              |             | unit below the x axis |
| ylab              |             | % change              |
| zeroline          | TRUE        | TRUE                  |

<span class="table-title">**Table A9. Parameters specific to
`donut`**</span>

| parameter      | default                       | example                       |
|:---------------|:------------------------------|:------------------------------|
| fill           | (required)                    | group                         |
| label          |                               | name                          |
| label_accuracy | 1                             | 1                             |
| label_colour   | black                         | black                         |
| label_style    | wedge/leader (default: wedge) | wedge/leader (default: wedge) |
| leader_length  | 0.15                          | 0.15                          |
| legend         | bottom                        | bottom                        |
| legend_pct     | FALSE                         | FALSE                         |
| palette        | qualitative                   | qualitative                   |
| panel_size     |                               | 1.8                           |
| reverse_legend | FALSE                         | FALSE                         |
| ring_width     | 0.6                           | 0.6                           |
| wedge_labels   | TRUE                          | TRUE                          |
| y              | (required)                    | purchasing_power              |

<span class="table-title">**Table A10. Parameters specific to
`map`**</span>

| parameter | default | example |
|:---|:---|:---|
| border_colour |  | white |
| border_linewidth | 0.15 | 0.15 |
| legend | topleft | topleft |
| level | gemeente/corop/provincie (default: gemeente) | gemeente/corop/provincie (default: gemeente) |
| na_fill |  | lightgrey |
| palette | sequential | sequential |
| region | (required) | code |
| reverse | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| value | (required) | share |

<span class="table-title">**Table A11. Parameters specific to
`boxplot_extended`**</span>

| parameter | default | example |
|:---|:---|:---|
| axis_text_size | 7 | 7 |
| box_labels |  | TRUE |
| box_style | modern/ggcpb/james/dot (default: modern) | modern/ggcpb/james/dot (default: modern) |
| dot_labels |  | p5:bottom 5%;p95:top 5% |
| facet |  | region |
| facet_ncol |  | 2 |
| facet_scales | fixed | fixed |
| fill |  | group |
| fill_colour |  | \#005faf |
| grid_colour | white | white |
| grid_linewidth | 0.8 | 0.8 |
| group |  | sector |
| group_gap | 0.7 | 0.7 |
| label_accuracy | 0.1 | 0.1 |
| legend | bottom | bottom |
| linewidth | 0.25 | 0.25 |
| mean |  | mean |
| minor | FALSE | FALSE |
| orientation | horizontal/vertical (default: horizontal) | horizontal/vertical (default: horizontal) |
| p25 | (required) | p25 |
| p5 | (required) | p5 |
| p50 | (required) | p50 |
| p75 | (required) | p75 |
| p95 | (required) | p95 |
| palette | qualitative | qualitative |
| panel_fill | \#eef8ff | \#eef8ff |
| pct_axis | FALSE | FALSE |
| reverse_legend | FALSE | FALSE |
| sec_accuracy |  | 0.1 |
| sec_at |  | 0;50;100 |
| sec_axis_labels |  | pct (or number, euro, or labels such as a;b;c) |
| sec_col_width | 0.3 | 0.3 |
| sec_colour |  | \#e6006e |
| sec_legend_label |  | name of the second series |
| sec_limits |  | 0;100 |
| sec_linewidth | 0.55 | 0.55 |
| sec_points | FALSE | FALSE |
| sec_scale_auto | TRUE | TRUE |
| sec_symbol_size | 1.6 | 1.6 |
| sec_type | line/point/col (default: line) | line/point/col (default: line) |
| sec_y |  | unemployment |
| sec_ylab |  | unit of the right axis |
| ticks | FALSE | FALSE |
| value_accuracy |  | 0.1 |
| value_axis |  | bottom |
| value_axis_linewidth | 0.7 | 0.7 |
| value_breaks |  | 0;25;50;75;100 |
| value_crop | FALSE | FALSE |
| value_limits |  | 0;100 |
| width | 0.45 | 0.45 |
| x | (required) | year |
| x_lim |  | 2015;2025 |
| x_lim_follow_data | TRUE | TRUE |
| xlab |  | unit below the x axis |
| ylab |  | % change |
| ylab_position |  | left |
| zero_indicator |  | TRUE |
| zero_indicator_linewidth | 2 | 2 |

A parameter that refers to a variable (such as `x`, `y` or `fill`) takes
a column name from the dataset. Every other parameter takes a value
directly from the parameter file.

The `parameter_settings.csv` file can also be opened directly in a
spreadsheet program.
