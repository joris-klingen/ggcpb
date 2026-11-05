Introduction to ggcpb
================

- [What is ggcpb?](#what-is-ggcpb)
  - [Why ggcpb instead of James?](#why-ggcpb-instead-of-james)
  - [What you need to know to use ggcpb in
    Excel](#what-you-need-to-know-to-use-ggcpb-in-excel)
  - [Use ggcpb in R](#use-ggcpb-in-r)
- [Figure types](#figure-types)
  - [Line with forecast](#line-with-forecast)
  - [Columns with two axes](#columns-with-two-axes)
  - [Horizontal bars with chosen
    colours](#horizontal-bars-with-chosen-colours)
  - [Line with markers in English](#line-with-markers-in-english)
  - [Boxplot](#boxplot)
  - [Donut chart](#donut-chart)
  - [Dot plot](#dot-plot)
  - [Scatter plot](#scatter-plot)
  - [Histogram](#histogram)
- [Ggplot2 base allows for endless
  possibilities](#ggplot2-base-allows-for-endless-possibilities)
  - [Classed map](#classed-map)
- [Example code to create figures in
  R](#example-code-to-create-figures-in-r)
  - [Line with forecast](#line-with-forecast-1)
  - [Columns with two axes](#columns-with-two-axes-1)
  - [Horizontal bars with chosen
    colours](#horizontal-bars-with-chosen-colours-1)
  - [Line with markers in English](#line-with-markers-in-english-1)
  - [Boxplot](#boxplot-1)
  - [Donut](#donut)
  - [Dot plot](#dot-plot-1)
  - [Scatter plot](#scatter-plot-1)
  - [Histogram](#histogram-1)
  - [Classed map](#classed-map-1)

<style>
.figrow { display: flex; gap: 24px; align-items: flex-start; margin: 1em 0; }
.figrow > * { flex: 1 1 0; min-width: 0; }
.figrow table { font-size: 0.85em; }
table.ptab { width: 100%; }
.figrow h4 { margin-top: 0; color: #c02a6a; }
@media (max-width: 600px) { .figrow { display: block; } }
</style>

This vignette gives a short introduction to creating customizable CPB
style figures with ggcpb.

# What is ggcpb?

ggcpb is the new version of James. It draws figures in the CPB house
style from Excel or from R.

## Why ggcpb instead of James?

ggcpb is more future proof. It is built on ggplot2, the most widely used
plotting package in R. This brings the following benefits:

- **More possibilities.** Anything ggplot2 can do, ggcpb can do. If you
  need an extra layer or a special annotation, you can add it. It allows
  for new figure types that were not possible with James, such as a
  donut chart.
- **Easier to maintain.** ggcpb adds a thin layer to a well tested
  package.

## What you need to know to use ggcpb in Excel

This is the general workflow to create figures with ggcpb without using
R:

1.  Find the most recent ggcpb folder in `[path]`.
2.  Copy it to the place where you want to create figures.
3.  Put your datasets in the folder `data`. Open `params.csv` in Excel
    and describe your figures.
4.  Start `run_import.bat` by double clicking it. Your figures appear in
    the folder `generated`.

You do not need to know R to follow this workflow. One parameter file
and one or more datasets drive everything.

- `params.csv` describes your figures. Column 1 holds the parameter
  names, such as `title`, `x` and `data`. Every other column describes
  one figure. The row `data` names the dataset of the figure.
- The folder `data` holds your datasets. Each dataset is its own CSV
  file with any name, such as `koopkracht.csv`. The first row holds the
  column names. The following rows are the observations per column.

Add the row `page` to save a figure at `full` width (15.5 cm). The
default is `half` (7.5 cm). Write the dataset name without `.csv`, such
as `koopkracht`. Leave `data` blank to use `data/data.csv`. Write `a;b`
to draw the same figure for both datasets. A figure is saved under the
name of its dataset, so `koopkracht` gives `koopkracht.png`. If two
figures use the same dataset, the figure number is added, such as
`koopkracht_1.png`.

*Please note*: The files must be saved as CSV, not as spreadsheet
(`xlsx`). In Excel, choose `File` -\> `Save as` -\>
`CSV UTF-8 (comma delimited)`.

Run the script again after every change. This will update the figures.
The import vignette gives a more detailed explanation and lists all
settings per figure type. See [`vignette("import")`](import.md).

## Use ggcpb in R

Next to plotting Excel data with ggcpb, you can also plot data that you
already have in R. The caption below each figure in this introduction
shows the function to use. Load the packages first.

``` r
library(conflicted) # optional, but recommended
library(ggcpb)
```

Each figure type has its own function.

``` r
# Line chart
cpb_line(data = df, x = year, y = value)
# Bar chart
cpb_col(data = df, x = year, y = value, fill = group)
# Area chart
cpb_area(data = df, x = year, y = value, fill = group)
# Box plot
cpb_box(data = df, x = group, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
# Dot plot
cpb_dot(data = df, x = term, y = estimate, lower = lower, upper = upper)
# Donut chart
cpb_donut(data = df, fill = group, y = share)
# Scatter plot
cpb_scatter(data = df, x = income, y = spending)
# Histogram
cpb_hist(data = df, x = duration)
# Map
cpb_map(data = df, region = code, value = share)
```

To export a figure, use `save_cpb()`. It writes the figure at the CPB
page widths. Use `page = "half"` for 7.5 cm and `page = "full"` for 15.5
cm.

``` r
save_cpb("zonnepanelen.png", p, page = "half")
```

The rest of this introduction assumes that you work in Excel.

# Figure types

Each figure below is combined with a feature that you can also use with
several other plot types, if not all. The parameters and their
corresponding values are portrayed next to the figure. Other settings
keep their default.

It is possible to set the style for a whole publication. Dutch is the
default. An English report can use `style = english` in `params.csv`, or
`options(ggcpb.style = "english")` in R.

## Line with forecast

A line chart with a forecast window and an uncertainty band.

<div class="figrow">

<img src="introduction_files/figure-gfm/line-forecast-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

line
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Economische groei met onzekerheid
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

jaar
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

groei
</td>

</tr>

<tr>

<td style="text-align:left;">

ymin
</td>

<td style="text-align:left;">

lo
</td>

</tr>

<tr>

<td style="text-align:left;">

ymax
</td>

<td style="text-align:left;">

hi
</td>

</tr>

<tr>

<td style="text-align:left;">

forecast_x
</td>

<td style="text-align:left;">

2023.5
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

%
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `line-forecast.csv` and `run_import.bat` to a
personal folder. Fill in the parameters above in `params.csv` and start
the script. In R you can call `cpb_line()` instead. See section 1.3 and
the example code at the end.

## Columns with two axes

Two series on one figure, with the second series on the right axis. The
legend tells you which axis belongs to which series.

<div class="figrow">

<img src="introduction_files/figure-gfm/col-secaxis-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

col
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Erfenissen en erfbelasting
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

jaar
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

mld
</td>

</tr>

<tr>

<td style="text-align:left;">

fill
</td>

<td style="text-align:left;">

soort
</td>

</tr>

<tr>

<td style="text-align:left;">

sec_y
</td>

<td style="text-align:left;">

heffing
</td>

</tr>

<tr>

<td style="text-align:left;">

sec_limits
</td>

<td style="text-align:left;">

0;3
</td>

</tr>

<tr>

<td style="text-align:left;">

sec_legend_label
</td>

<td style="text-align:left;">

erfbelasting
</td>

</tr>

<tr>

<td style="text-align:left;">

sec_ylab
</td>

<td style="text-align:left;">

mld euro
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

mld euro
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `col-secaxis.csv` and `run_import.bat` to a personal
folder. Fill in the parameters above in `params.csv` and start the
script. In R you can call `cpb_col()` instead. See section 1.3 and the
example code at the end.

## Horizontal bars with chosen colours

The colours decide the legend. Pick them with `colour_index`.

<div class="figrow">

<img src="introduction_files/figure-gfm/col-horizontal-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

col
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Zonnepanelen naar inkomen
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

groep
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

aandeel
</td>

</tr>

<tr>

<td style="text-align:left;">

fill
</td>

<td style="text-align:left;">

jaar
</td>

</tr>

<tr>

<td style="text-align:left;">

position
</td>

<td style="text-align:left;">

dodge
</td>

</tr>

<tr>

<td style="text-align:left;">

orientation
</td>

<td style="text-align:left;">

horizontal
</td>

</tr>

<tr>

<td style="text-align:left;">

colour_index
</td>

<td style="text-align:left;">

2;1
</td>

</tr>

<tr>

<td style="text-align:left;">

value_limits
</td>

<td style="text-align:left;">

0;70
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

inkomensgroep
</td>

</tr>

<tr>

<td style="text-align:left;">

xlab
</td>

<td style="text-align:left;">

aandeel binnen inkomensgroep (%)
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `col-horizontal.csv` and `run_import.bat` to a
personal folder. Fill in the parameters above in `params.csv` and start
the script. In R you can call `cpb_col()` instead. See section 1.3 and
the example code at the end.

## Line with markers in English

A line with markers suits a few ordered categories. This figure uses the
English style.

<div class="figrow">

<img src="introduction_files/figure-gfm/line-points-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

line
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Preferred tax rate by age
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

leeftijd
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

tarief
</td>

</tr>

<tr>

<td style="text-align:left;">

colour
</td>

<td style="text-align:left;">

erfenis
</td>

</tr>

<tr>

<td style="text-align:left;">

points
</td>

<td style="text-align:left;">

TRUE
</td>

</tr>

<tr>

<td style="text-align:left;">

value_limits
</td>

<td style="text-align:left;">

0;30
</td>

</tr>

<tr>

<td style="text-align:left;">

style
</td>

<td style="text-align:left;">

english
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

% average rate
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `line-points.csv` and `run_import.bat` to a personal
folder. Fill in the parameters above in `params.csv` and start the
script. In R you can call `cpb_line()` instead. See section 1.3 and the
example code at the end.

## Boxplot

A box shows the 25th to 75th percentile. The line in the box is the
median. The whiskers run from the 5th to the 95th percentile.

<div class="figrow">

<img src="introduction_files/figure-gfm/box-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

box
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Koopkracht per inkomensgroep
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

groep
</td>

</tr>

<tr>

<td style="text-align:left;">

p5
</td>

<td style="text-align:left;">

p5
</td>

</tr>

<tr>

<td style="text-align:left;">

p25
</td>

<td style="text-align:left;">

p25
</td>

</tr>

<tr>

<td style="text-align:left;">

p50
</td>

<td style="text-align:left;">

p50
</td>

</tr>

<tr>

<td style="text-align:left;">

p75
</td>

<td style="text-align:left;">

p75
</td>

</tr>

<tr>

<td style="text-align:left;">

p95
</td>

<td style="text-align:left;">

p95
</td>

</tr>

<tr>

<td style="text-align:left;">

orientation
</td>

<td style="text-align:left;">

horizontal
</td>

</tr>

<tr>

<td style="text-align:left;">

subtitle
</td>

<td style="text-align:left;">

inkomensgroep
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

% koopkrachtmutatie
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `box.csv` and `run_import.bat` to a personal folder.
Fill in the parameters above in `params.csv` and start the script. In R
you can call `cpb_box()` instead. See section 1.3 and the example code
at the end.

More box plot styles are in [`vignette("boxplots")`](boxplots.md).

## Donut chart

Individual categories add up to 100% in the donut chart. Categories
don’t need to be expressed as percentages. Any numerical values will
work, as the donut chart automatically calculates the proportions.

<div class="figrow">

<img src="introduction_files/figure-gfm/donut-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

donut
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Energiemix van huishoudens
</td>

</tr>

<tr>

<td style="text-align:left;">

fill
</td>

<td style="text-align:left;">

bron
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

aandeel
</td>

</tr>

<tr>

<td style="text-align:left;">

colour_index
</td>

<td style="text-align:left;">

6;5;2;4
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `donut.csv` and `run_import.bat` to a personal
folder. Fill in the parameters above in `params.csv` and start the
script. In R you can call `cpb_donut()` instead. See section 1.3 and the
example code at the end.

## Dot plot

A dot plot shows an estimate with its interval.

<div class="figrow">

<img src="introduction_files/figure-gfm/dot-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

dot
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Samenhang met voorkeurstarieven
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

term
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

coef
</td>

</tr>

<tr>

<td style="text-align:left;">

lower
</td>

<td style="text-align:left;">

lo
</td>

</tr>

<tr>

<td style="text-align:left;">

upper
</td>

<td style="text-align:left;">

hi
</td>

</tr>

<tr>

<td style="text-align:left;">

group
</td>

<td style="text-align:left;">

blok
</td>

</tr>

<tr>

<td style="text-align:left;">

xlab
</td>

<td style="text-align:left;">

%punt verandering
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `dot.csv` and `run_import.bat` to a personal folder.
Fill in the parameters above in `params.csv` and start the script. In R
you can call `cpb_dot()` instead. See section 1.3 and the example code
at the end.

## Scatter plot

<div class="figrow">

<img src="introduction_files/figure-gfm/scatter-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

scatter
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Energierekening naar inkomen
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

inkomen
</td>

</tr>

<tr>

<td style="text-align:left;">

y
</td>

<td style="text-align:left;">

energierekening
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

euro per maand
</td>

</tr>

<tr>

<td style="text-align:left;">

xlab
</td>

<td style="text-align:left;">

besteedbaar inkomen (euro per maand)
</td>

</tr>

<tr>

<td style="text-align:left;">

x_labels
</td>

<td style="text-align:left;">

euro
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `scatter.csv` and `run_import.bat` to a personal
folder. Fill in the parameters above in `params.csv` and start the
script. In R you can call `cpb_scatter()` instead. See section 1.3 and
the example code at the end.

## Histogram

<div class="figrow">

<img src="introduction_files/figure-gfm/hist-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

hist
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Verdeling van de duur
</td>

</tr>

<tr>

<td style="text-align:left;">

x
</td>

<td style="text-align:left;">

maanden
</td>

</tr>

<tr>

<td style="text-align:left;">

binwidth
</td>

<td style="text-align:left;">

2
</td>

</tr>

<tr>

<td style="text-align:left;">

ylab
</td>

<td style="text-align:left;">

aantal
</td>

</tr>

<tr>

<td style="text-align:left;">

xlab
</td>

<td style="text-align:left;">

duur (maanden)
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `hist.csv` and `run_import.bat` to a personal
folder. Fill in the parameters above in `params.csv` and start the
script. In R you can call `cpb_hist()` instead. See section 1.3 and the
example code at the end.

# Ggplot2 base allows for endless possibilities

Every ggcpb figure is a real ggplot2 object. This means you can stack
extra functionalities to the plot in R with `+`.

The next figure has a sharp rise between 2020 and 2021. We add a
reference line, and a text along the diagonal in dark pink italic
letters. The position comes from the coordinates of the data. The
`angle` follows the slope of the line.

``` r
stijging <- tibble(jaar = 2017:2024,
                   waarde = c(2.1, 2.3, 2.0, 1.9, 6.4, 6.8, 6.5, 6.6))

cpb_line(stijging, x = jaar, y = waarde,
         title = "Een scherpe stijging", ylab = "%") +
  geom_hline(yintercept = 6, linetype = "dashed", linewidth = 0.3) +
  annotate("text", x = 2020.2, y = 4.2, label = "scherpe stijging",
           angle = 72, colour = "#a0004e", fontface = "italic",
           size = 2.4, family = cpb_font_family())
```

<img src="introduction_files/figure-gfm/diagonal-1.png" alt="" width="50%" />

You can stack as many pieces as you need. More patterns are in
[`vignette("annotation")`](annotation.md).

## Classed map

The maps can display municipalities or provinces on bundled generalised
CBS/Kadaster boundaries. The following map colours each Dutch
municipality by class. The parameter `cut` makes the classes, and
`cut_labels = pct` writes them as percentages. In R, `cpb_cut()` and
`label_pct_nl()` do the same. The boundaries come with the package.

``` r
gemeenten <- tibble(code = unique(cpb_nl_geo("gemeente")$code)) |>
  mutate(aandeel = runif(n(), 5, 75),
         klasse  = cpb_cut(aandeel, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                           labeller = label_pct_nl()))
```

<div class="figrow">

<img src="introduction_files/figure-gfm/map-1.png" alt="" width="100%" />

<div>

#### Relevant parameters

<table class="ptab">

<tbody>

<tr>

<td style="text-align:left;">

plot_type
</td>

<td style="text-align:left;">

map
</td>

</tr>

<tr>

<td style="text-align:left;">

title
</td>

<td style="text-align:left;">

Aandeel huishoudens met zonnepanelen
</td>

</tr>

<tr>

<td style="text-align:left;">

region
</td>

<td style="text-align:left;">

code
</td>

</tr>

<tr>

<td style="text-align:left;">

value
</td>

<td style="text-align:left;">

aandeel
</td>

</tr>

<tr>

<td style="text-align:left;">

cut
</td>

<td style="text-align:left;">

0;20;30;40;50;60;Inf
</td>

</tr>

<tr>

<td style="text-align:left;">

cut_labels
</td>

<td style="text-align:left;">

pct
</td>

</tr>

<tr>

<td style="text-align:left;">

palette
</td>

<td style="text-align:left;">

blues
</td>

</tr>

<tr>

<td style="text-align:left;">

legend_title
</td>

<td style="text-align:left;">

aandeel
</td>

</tr>

</tbody>

</table>

</div>

</div>

**How to reproduce this figure:** copy `params.csv`, the folder `data`
containing at least `map.csv` and `run_import.bat` to a personal folder.
Fill in the parameters above in `params.csv` and start the script. In R
you can call `cpb_map()` instead. See section 1.3 and the example code
at the end.

More options are in [`vignette("maps")`](maps.md).

# Example code to create figures in R

The code below creates the figures above in R. It first builds the data,
then draws the figure. It is the same code that made the figures in this
document. Start the script with these lines.

``` r
library(conflicted)
library(ggcpb)
library(ggplot2)
library(dplyr)
library(tidyr)
```

## Line with forecast

``` r
groei <- tibble(jaar = 2015:2027,
                groei = round(rnorm(13, 1.5, 0.8), 1)) |>
  mutate(marge = c(rep(0, 9), 0.4, 0.9, 1.4, 1.8),
         lo = groei - marge, hi = groei + marge)
cpb_line(groei, x = jaar, y = groei, ymin = lo, ymax = hi,
         forecast_x = 2023.5,
         title = "Economische groei met onzekerheid", ylab = "%") +
  scale_x_continuous(breaks = seq(2015, 2027, 3), minor_breaks = 2015:2027,
                     guide = guide_axis(minor.ticks = TRUE))
```

## Columns with two axes

``` r
erfenissen <- expand_grid(
  soort = factor(c("geen aangifte", "wel aangifte"),
                 levels = c("geen aangifte", "wel aangifte")),
  jaar  = 2015:2022
) |>
  mutate(mld = c(15.0, 15.3, 15.5, 16.3, 19.4, 20.5, 22.2, 23.0,
                 5.8, 5.6, 5.6, 6.6, 6.4, 6.5, 6.8, 7.0)) |>
  left_join(tibble(jaar = 2015:2022,
                   heffing = c(1.7, 1.8, 1.8, 1.8, 2.3, 2.4, 2.5, 2.6)),
            by = "jaar")
cpb_col(erfenissen, x = jaar, y = mld, fill = soort,
        sec_y = heffing, sec_limits = c(0, 3),
        sec_legend_label = "erfbelasting", sec_ylab = "mld euro",
        title = "Erfenissen en erfbelasting", ylab = "mld euro")
```

## Horizontal bars with chosen colours

``` r
groepen <- c("tot 120% wml", "120% wml tot modaal", "1 tot 1,5x modaal",
             "1,5 tot 2x modaal", "2 tot 3x modaal", "boven 3x modaal")
zonnepanelen <- expand_grid(
  jaar  = factor(c(2021, 2024), levels = c(2024, 2021)),
  groep = factor(groepen, levels = rev(groepen))
) |>
  mutate(aandeel = c(10, 14, 19, 22, 26, 33,
                     19, 25, 31, 38, 47, 59))
cpb_col(zonnepanelen, x = groep, y = aandeel, fill = jaar,
        position = "dodge", orientation = "horizontal",
        colour_index = c(2, 1), value_limits = c(0, 70), width = 0.85,
        title = "Zonnepanelen naar inkomen",
        ylab  = "inkomensgroep", xlab = "aandeel binnen inkomensgroep (%)")
```

## Line with markers in English

``` r
leeftijden <- c("20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70")
tarieven <- expand_grid(
  erfenis  = factor(c("10,000 euro", "100,000 euro"),
                    levels = c("10,000 euro", "100,000 euro")),
  leeftijd = factor(leeftijden, levels = leeftijden)
) |>
  mutate(tarief = c(14.5, 12.5, 13.5, 10.5, 8.5,
                    21.5, 18.5, 18.5, 17.5, 17.5))
cpb_line(tarieven, x = leeftijd, y = tarief, colour = erfenis,
         points = TRUE, value_limits = c(0, 30), style = "english",
         title = "Preferred tax rate by age", ylab = "% average rate")
```

## Boxplot

``` r
raw <- tibble(
  groep      = factor(rep(groepen, each = 400), levels = rev(groepen)),
  koopkracht = rnorm(2400, mean = rep(c(-3, -1.5, 0, 1, 2, 3.5), each = 400),
                     sd = 2)
)
kk <- raw |>
  summarise(p5 = quantile(koopkracht, 0.05), p25 = quantile(koopkracht, 0.25),
            p50 = quantile(koopkracht, 0.50), p75 = quantile(koopkracht, 0.75),
            p95 = quantile(koopkracht, 0.95), .by = groep)
cpb_box(kk, x = groep,
        p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
        orientation = "horizontal",
        title = "Koopkracht per inkomensgroep",
        subtitle = "inkomensgroep", ylab = "% koopkrachtmutatie")
```

## Donut

``` r
bronnen <- c("gas", "elektriciteit", "warmte", "overig")
energie <- tibble(bron = factor(bronnen, levels = bronnen),
                  aandeel = c(45, 30, 15, 10))
cpb_donut(energie, fill = bron, y = aandeel,
          colour_index = c(6, 5, 2, 4),
          title = "Energiemix van huishoudens")
```

## Dot plot

``` r
termen <- c("Werken ontmoedigd", "Ongelijkheid", "Hard werken loont",
            "Leeftijd", "Geslacht", "Kinderen")
schatting <- tibble(
  term = factor(termen, levels = termen),
  blok = factor(rep(c("Percepties", "Achtergrond"), each = 3),
                levels = c("Percepties", "Achtergrond")),
  coef = c(3.2, 2.9, -2.0, 3.5, 0.4, -1.4),
  lo   = c(-0.3, 2.1, -3.0, 2.6, -1.3, -3.3),
  hi   = c(6.6, 3.6, -1.1, 4.3, 2.2, 0.6)
)
cpb_dot(schatting, x = term, y = coef, lower = lo, upper = hi,
        group = blok,
        title = "Samenhang met voorkeurstarieven",
        xlab  = "%punt verandering")
```

## Scatter plot

``` r
hh <- tibble(inkomen = round(rlnorm(400, log(2500), 0.35))) |>
  mutate(energierekening = round(90 + 0.04 * inkomen + rnorm(n(), 0, 35)))
cpb_scatter(hh, x = inkomen, y = energierekening,
            title = "Energierekening naar inkomen",
            ylab  = "euro per maand",
            xlab  = "besteedbaar inkomen (euro per maand)",
            x_labels = label_euro_nl())
```

## Histogram

``` r
duur <- tibble(maanden = round(rgamma(1200, 8, 0.6)))
cpb_hist(duur, x = maanden, binwidth = 2,
         title = "Verdeling van de duur", ylab = "aantal",
         xlab  = "duur (maanden)")
```

## Classed map

``` r
gemeenten <- tibble(code = unique(cpb_nl_geo("gemeente")$code)) |>
  mutate(aandeel = runif(n(), 5, 75),
         klasse  = cpb_cut(aandeel, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                           labeller = label_pct_nl()))
cpb_map(gemeenten, region = code, value = klasse,
        palette = "blues",
        title = "Aandeel huishoudens met zonnepanelen",
        legend_title = "aandeel")
```
