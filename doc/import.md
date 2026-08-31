Building figures from CSV files
================

``` r
library(ggcpb)
```

# Introduction

Most figures in this package are built by calling one of its functions
directly in R code. This vignette describes an alternative based on two
CSV files, a data file and a parameter file.

The data file contains the data used in the figures. The parameter file
specifies which figures to create and how they should be configured. The
`import_csv()` function reads both files and creates the figures.

This approach is useful when figure specifications need to be kept
separate from R code, for example when the data and figure settings are
maintained outside an R script.

# The data file

The data file can be any CSV file containing the variables needed by the
figures. For this example, the package includes
`import_example_data.csv`.

``` r
data_csv <- system.file(
  "extdata",
  "import_example_data.csv",
  package = "ggcpb"
)
```

The file can be read in the usual way:

``` r
read.csv(data_csv)
#>    jaar                  groep koopkracht werkloosheid
#> 1  2021              werkenden        0.3          3.4
#> 2  2021        gepensioneerden       -0.2          3.4
#> 3  2021 uitkeringsgerechtigden       -0.9          3.4
#> 4  2022              werkenden       -2.7          3.5
#> 5  2022        gepensioneerden       -2.3          3.5
#> 6  2022 uitkeringsgerechtigden       -3.2          3.5
#> 7  2023              werkenden        1.5          3.6
#> 8  2023        gepensioneerden        0.8          3.6
#> 9  2023 uitkeringsgerechtigden       -0.3          3.6
#> 10 2024              werkenden        1.2          3.7
#> 11 2024        gepensioneerden        0.6          3.7
#> 12 2024 uitkeringsgerechtigden       -0.5          3.7
#> 13 2025              werkenden        0.9          3.7
#> 14 2025        gepensioneerden        0.4          3.7
#> 15 2025 uitkeringsgerechtigden       -0.6          3.7
```

The important point for the import method is that the parameter file
refers to variables by their column names. For example, if a parameter
contains `jaar`, `import_csv()` looks for a column named `jaar` in the
data file.

# The parameter file

The parameter file determines which figures are created. Each figure has
a `plot_type` and an `id`, together with the settings required for that
plot type.

The example parameter file is `import_example_params.csv`.

``` r
params_csv <- system.file(
  "extdata",
  "import_example_params.csv",
  package = "ggcpb"
)
```

This example uses the horizontal layout:

    plot_type,id,title,x,y,fill,ylab,position,sec_y,sec_type,sec_label,sec_ylab,create
    col,koopkracht,Koopkrachtontwikkeling naar groep,jaar,koopkracht,groep,% mutatie,dodge,werkloosheid,line,werkloosheid,%,y
    line,werkloosheid,Werkloosheid,jaar,werkloosheid,,%,,,,,,y

The same content, split on the comma and shown as a table:

| plot_type | id | title | x | y | fill | ylab | position | sec_y | sec_type | sec_label | sec_ylab | create |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| col | koopkracht | Koopkrachtontwikkeling naar groep | jaar | koopkracht | groep | % mutatie | dodge | werkloosheid | line | werkloosheid | % | y |
| line | werkloosheid | Werkloosheid | jaar | werkloosheid |  | % |  |  |  |  |  | y |

Each row describes one figure. The column names are parameter names.

Some parameters refer to columns in the data file. For example:

- `x`, `y`, `fill` and `sec_y` are column references.
- `title`, `ylab` and `position` are literal values.

The distinction is important. `y,koopkracht` means that the `y` variable
should be taken from the `koopkracht` column, while `ylab,% mutatie`
supplies the text `% mutatie` directly.

# Creating the figures

Once the two files are available, pass them to `import_csv()`:

``` r
tmp_params <- tempfile(fileext = ".csv")
file.copy(params_csv, tmp_params, overwrite = TRUE)

figs <- import_csv(data_csv, tmp_params)
```

When the parameter file contains multiple figures, `import_csv()`
returns a named list. The names come from the `id` parameter.

The `koopkracht` figure has a second value axis (`sec_y`), and its right
side caption is only placed exactly by `save_cpb()`. Printed directly,
an approximate placement is shown instead, close but not exact, since at
that point the figure has not actually been saved yet to measure
against. So, rather than printing it directly, it is written out through
`save_cpb()` first and shown from that file:

``` r
path <- tempfile(fileext = ".png")
save_cpb(path, figs$koopkracht, page = "full")
```

<img src="import_files/figure-gfm/fig-koopkracht-show-1.png" alt="" width="700px" />

`werkloosheid` has no second axis, so it can be shown directly:

``` r
figs$werkloosheid
```

<img src="import_files/figure-gfm/fig-werkloosheid-1.png" alt="" width="350px" />

In normal use, the data and parameter files can be stored in an ordinary
writable folder. The temporary copy above is only needed here because
the example parameter file is part of the installed package.

# One figure per parameter file

There is a second parameter file layout for cases where one parameter
file describes a single figure.

In the horizontal layout, each row is a figure and each column is a
parameter. In the vertical layout, each row is a parameter and the two
columns contain the parameter name and its value.

For example, the following describes a line chart:

``` r
params_vertical <- tempfile(fileext = ".csv")

writeLines(c(
  "plot_type,line",
  "title,Werkloosheid",
  "x,jaar",
  "y,werkloosheid",
  "ylab,%"
), params_vertical)

cat(readLines(params_vertical), sep = "\n")
#> plot_type,line
#> title,Werkloosheid
#> x,jaar
#> y,werkloosheid
#> ylab,%
```

The first column contains the parameter names:

```
plot_type
title
x
y
ylab
```

The second column contains their values:

```
line
Werkloosheid
jaar
werkloosheid
%
```

This layout can be convenient when a figure has many parameters, because
each setting has its own row.

When the parameter file describes one figure, `import_csv()` returns
that figure directly rather than a named list.

``` r
fig_vertical <- import_csv(data_csv, params_vertical)
```

<img src="import_files/figure-gfm/vertical-build-show-1.png" alt="" width="350px" />

# Handling problems

A parameter can be left blank. In that case, the default value is used.

If a parameter is not valid for the selected `plot_type`, it is ignored
and the default is used. These cases produce a warning.

If a figure cannot be created, for example because a column referenced
by a parameter does not exist in the data file, that figure is skipped.
Other figures in the same parameter file are still created.

Each call to `import_csv()` also writes a log file next to the parameter
file. The log records figures that were created or skipped and any
parameter problems.

# Run the import without opening R

The same CSV based method can also be used without opening R.

The `autogenerate_plots()` function creates a ready to use folder
containing:

- a data file
- a parameter file
- a script for Windows (a `.bat` file)
- a script for a Mac (a `.command` file)

After editing the data and parameter files, run the appropriate script.
It reads both files and saves the resulting figures as images.

Running the script again after changing either CSV file produces updated
figures.

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
| flush_legend | literal | TRUE | TRUE |
| index | literal |  | gebruik colour_index of fill_index in plaats hiervan |
| legend | literal | bottom | bottom |
| legend_key_size | literal |  | 0.3 |
| legend_ncol | literal |  | 2 |
| palette | literal | qualitative | qualitative |
| subtitle | literal |  | ondertitel (vervangt de standaard eenheid boven de figuur) |
| title | literal |  | Titel van de figuur |

**Table A2. Parameters specific to `col`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from data_csv) |  | groep |
| fill_colour | literal |  | \#005faf |
| fill_index | literal |  | 2;6 |
| filllab | literal |  | titel van de vullingslegenda |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from data_csv) |  | sector |
| group_gap | literal | 0.8 | 0.8 |
| minor | literal | FALSE | FALSE |
| orientation | literal | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| pct_axis | literal | FALSE | FALSE |
| position | literal | stack/dodge/fill (default: stack) | stack/dodge/fill (default: stack) |
| reverse_legend | literal | TRUE | TRUE |
| sec_accuracy | literal |  | 0.1 |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_label | literal |  | naam van de tweede reeks |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_point_size | literal | 1.6 | 1.6 |
| sec_points | literal | FALSE | FALSE |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from data_csv) |  | werkloosheid |
| sec_ylab | literal |  | eenheid van de rechteras |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_labels | literal | FALSE | FALSE |
| value_limits | literal |  | 0;100 |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | eenheid onderaan de x-as |
| y | column (from data_csv) | (required) | koopkracht |
| ylab | literal |  | % mutatie |
| zeroline | literal | TRUE | TRUE |

**Table A3. Parameters specific to `area`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from data_csv) | (required) | groep |
| fill_index | literal |  | 2;6 |
| filllab | literal |  | titel van de vullingslegenda |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| minor | literal | FALSE | FALSE |
| pct_axis | literal | FALSE | FALSE |
| reverse_legend | literal | TRUE | TRUE |
| sec_accuracy | literal |  | 0.1 |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_label | literal |  | naam van de tweede reeks |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_point_size | literal | 1.6 | 1.6 |
| sec_points | literal | FALSE | FALSE |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from data_csv) |  | werkloosheid |
| sec_ylab | literal |  | eenheid van de rechteras |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_limits | literal |  | 0;100 |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | eenheid onderaan de x-as |
| y | column (from data_csv) | (required) | koopkracht |
| ylab | literal |  | % mutatie |
| zeroline | literal | TRUE | TRUE |

**Table A4. Parameters specific to `line`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| color_index | literal |  | 2;6 |
| colour | column (from data_csv) |  | groep |
| colour_index | literal |  | 2;6 |
| colourlab | literal |  | titel van de kleurenlegenda |
| facet | column (from data_csv) |  | regio |
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
| point_size | literal | 1.1 | 1.1 |
| points | literal | FALSE | FALSE |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_label | literal |  | naam van de tweede reeks |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal |  | 0.55 |
| sec_point_size | literal | 1.6 | 1.6 |
| sec_points | literal | FALSE | FALSE |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from data_csv) |  | werkloosheid |
| sec_ylab | literal |  | eenheid van de rechteras |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_limits | literal |  | 0;100 |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | eenheid onderaan de x-as |
| y | column (from data_csv) | (required) | koopkracht |
| ylab | literal |  | % mutatie |
| ymax | column (from data_csv) |  | bovengrens |
| ymin | column (from data_csv) |  | ondergrens |
| zeroline | literal |  | TRUE |

**Table A5. Parameters specific to `box`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| box_labels | literal |  | TRUE |
| box_style | literal | ggcpb/james/modern/dot (default: ggcpb) | ggcpb/james/modern/dot (default: ggcpb) |
| dot_labels | literal |  | p5:onderste 5%;p95:bovenste 5% |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from data_csv) |  | groep |
| fill_colour | literal |  | \#005faf |
| fill_index | literal |  | 2;6 |
| filllab | literal |  | titel van de vullingslegenda |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from data_csv) |  | sector |
| group_gap | literal | 0.7 | 0.7 |
| label_accuracy | literal | 0.1 | 0.1 |
| linewidth | literal | 0.25 | 0.25 |
| mean | column (from data_csv) |  | gemiddelde |
| minor | literal | FALSE | FALSE |
| orientation | literal | vertical/horizontal (default: vertical) | vertical/horizontal (default: vertical) |
| p25 | column (from data_csv) | (required) | p25 |
| p5 | column (from data_csv) | (required) | p5 |
| p50 | column (from data_csv) | (required) | p50 |
| p75 | column (from data_csv) | (required) | p75 |
| p95 | column (from data_csv) | (required) | p95 |
| pct_axis | literal | FALSE | FALSE |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_label | literal |  | naam van de tweede reeks |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_point_size | literal | 1.6 | 1.6 |
| sec_points | literal | FALSE | FALSE |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from data_csv) |  | werkloosheid |
| sec_ylab | literal |  | eenheid van de rechteras |
| ticks | literal | TRUE | TRUE |
| value_accuracy | literal |  | 0.1 |
| value_axis | literal | bottom/top (default: bottom) | bottom/top (default: bottom) |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_limits | literal |  | 0;100 |
| width | literal | 0.5 | 0.5 |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | eenheid onderaan de x-as |
| ylab | literal |  | % mutatie |
| zeroline | literal |  | TRUE |

**Table A6. Parameters specific to `dot`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| cap_width | literal | 0.25 | 0.25 |
| color_index | literal |  | 2;6 |
| colour | column (from data_csv) |  | groep |
| colour_index | literal |  | 2;6 |
| colourlab | literal |  | titel van de kleurenlegenda |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| group | column (from data_csv) |  | sector |
| group_gap | literal | 0.7 | 0.7 |
| linewidth | literal | 0.4 | 0.4 |
| lower | column (from data_csv) | (required) | ondergrens |
| minor | literal | FALSE | FALSE |
| orientation | literal | horizontal/vertical (default: horizontal) | horizontal/vertical (default: horizontal) |
| pct_axis | literal | FALSE | FALSE |
| point_colour | literal |  | \#e6006e |
| reverse_legend | literal | FALSE | FALSE |
| sec_accuracy | literal |  | 0.1 |
| sec_col_width | literal | 0.3 | 0.3 |
| sec_colour | literal |  | \#e6006e |
| sec_label | literal |  | naam van de tweede reeks |
| sec_limits | literal |  | 0;100 |
| sec_linewidth | literal | 0.55 | 0.55 |
| sec_point_size | literal | size | size |
| sec_points | literal | FALSE | FALSE |
| sec_type | literal | line/point/col (default: line) | line/point/col (default: line) |
| sec_y | column (from data_csv) |  | werkloosheid |
| sec_ylab | literal |  | eenheid van de rechteras |
| size | literal | 1.4 | 1.4 |
| ticks | literal | TRUE | TRUE |
| upper | column (from data_csv) | (required) | bovengrens |
| value_accuracy | literal |  | 0.1 |
| value_breaks | literal |  | 0;25;50;75;100 |
| value_limits | literal |  | 0;100 |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | TRUE | TRUE |
| xlab | literal |  | eenheid onderaan de x-as |
| y | column (from data_csv) | (required) | koopkracht |
| ylab | literal |  | % mutatie |
| zeroline | literal | TRUE | TRUE |

**Table A7. Parameters specific to `scatter`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| color_index | literal |  | 2;6 |
| colour | column (from data_csv) |  | groep |
| colour_index | literal |  | 2;6 |
| colourlab | literal |  | titel van de kleurenlegenda |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| forecast_label | literal | raming | raming |
| forecast_x | literal |  | 2025 |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| minor | literal | FALSE | FALSE |
| point_colour | literal |  | \#e6006e |
| reverse_legend | literal | FALSE | FALSE |
| size | literal | 0.8 | 0.8 |
| ticks | literal | TRUE | TRUE |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | eenheid onderaan de x-as |
| y | column (from data_csv) | (required) | koopkracht |
| ylab | literal |  | % mutatie |
| zeroline | literal |  | TRUE |

**Table A8. Parameters specific to `hist`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| axis_text_size | literal | 7 | 7 |
| bins | literal |  | 30 |
| binwidth | literal |  | 5 |
| facet | column (from data_csv) |  | regio |
| facet_ncol | literal |  | 2 |
| facet_scales | literal | fixed | fixed |
| fill | column (from data_csv) |  | groep |
| fill_colour | literal |  | \#005faf |
| fill_index | literal |  | 2;6 |
| filllab | literal |  | titel van de vullingslegenda |
| grid_colour | literal | black | black |
| grid_linewidth | literal | 0.1 | 0.1 |
| minor | literal | FALSE | FALSE |
| outline | literal | white | white |
| position | literal | stack | stack |
| reverse_legend | literal | TRUE | TRUE |
| ticks | literal | TRUE | TRUE |
| x | column (from data_csv) | (required) | jaar |
| x_lim | literal |  | 2015;2025 |
| x_lim_follow_data | literal | FALSE | FALSE |
| xlab | literal |  | eenheid onderaan de x-as |
| ylab | literal |  | % mutatie |
| zeroline | literal | TRUE | TRUE |

**Table A9. Parameters specific to `donut`**

| setting | kind | default | example |
|:---|:---|:---|:---|
| fill | column (from data_csv) | (required) | groep |
| filllab | literal |  | titel van de vullingslegenda |
| label | column (from data_csv) |  | naam |
| label_accuracy | literal | 1 | 1 |
| label_colour | literal | black | black |
| label_style | literal | wedge/leader (default: wedge) | wedge/leader (default: wedge) |
| leader_length | literal | 0.15 | 0.15 |
| legend_pct | literal | FALSE | FALSE |
| panel_size | literal | 1.8 | 1.8 |
| reverse_legend | literal | FALSE | FALSE |
| ring_width | literal | 0.6 | 0.6 |
| wedge_labels | literal | TRUE | TRUE |
| y | column (from data_csv) | (required) | koopkracht |

A parameter of kind `column` takes a column name from the data file. A
parameter of kind `literal` takes a value directly from the parameter
file.

The `parameter_settings.csv` file can also be opened directly in a
spreadsheet program.
