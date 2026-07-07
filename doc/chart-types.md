Chart types
================

``` r
library(ggcpb)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(42)
```

This vignette is a gallery: one section per CPB chart type, each with
the simulated data it needs and the wrapper call that draws it. All
wrappers return a plain `ggplot` object, so anything here can be
extended further with `+`.

The other vignettes build on these basics:

- `vignette("layouts")` – grouped category axes, facets and maps:
  arranging more than one chart’s worth of information in a figure.
- `vignette("recipes")` – forecast windows, composing charts by hand,
  and exporting at CPB page sizes.
- `vignette("ggcpb")` – the composable core the wrappers are built on,
  as an end-to-end walkthrough.

Two house conventions to know up front:

- **Titles and units.** `title` is the bold heading. The value-axis unit
  goes in `ylab` and is rendered as the italic caption above the panel
  (the CPB “subtitle” position), not as a rotated axis title. A titled
  figure always reserves that caption line, so figures with and without
  a unit align.
- **Zero line.** A solid black zero line is drawn automatically on the
  value axis: always for columns and areas (which are anchored at zero),
  and for lines and boxplots whenever the data spans zero.

# Line charts

A single series needs only `x` and `y`; it is drawn in the CPB primary
blue:

``` r
bbp <- tibble(
  jaar  = 2015:2027,
  index = 100 + cumsum(rnorm(13, mean = 1.5, sd = 1.2))
)

cpb_line(bbp, x = jaar, y = index,
  title = "Bruto binnenlands product",
  ylab  = "index (2015 = 100)") +
  scale_x_continuous(breaks = seq(2015, 2027, 3), minor_breaks = 2015:2027,
                     guide = guide_axis(minor.ticks = TRUE))
```

<img src="chart-types_files/figure-gfm/line-single-1.png" width="350px" />

Map a column to `colour` for multiple series, and pick house colours by
palette position with `index` – `c(6, 2)` is the recurring blue/magenta
pair. Growth rates span zero, so the black zero line appears by itself:

``` r
groei <- expand_grid(reeks = c("arbeidsproductiviteit", "tfp"),
                     jaar  = 2000:2024) |>
  mutate(waarde = round(rnorm(n(), mean = 1, sd = 1.6), 1))

cpb_line(groei, x = jaar, y = waarde, colour = reeks,
  index = c(6, 2),
  title = "Productiviteitsgroei",
  ylab  = "%") +
  scale_x_continuous(
    breaks       = seq(2000, 2024, 4),
    minor_breaks = 2000:2024,
    guide        = guide_axis(minor.ticks = TRUE)
  )
```

<img src="chart-types_files/figure-gfm/line-multi-1.png" width="350px" />

The added `scale_x_continuous()` shows the usual refinements for a year
axis: labelled breaks every few years plus small minor ticks for the
years in between.

# Column charts

`cpb_col()` covers stacked, dodged and horizontal bars. Stacked is the
default `position`:

``` r
sectoren <- c("industrie", "diensten", "landbouw", "overheid")
tw <- expand_grid(jaar   = 2023:2027,
                  sector = factor(sectoren, levels = sectoren)) |>
  mutate(waarde = round(runif(n(), 5, 25), 1))

cpb_col(tw, x = jaar, y = waarde, fill = sector,
  index = c(6, 5, 2, 4),
  title = "Toegevoegde waarde per sector",
  ylab  = "mld euro")
```

<img src="chart-types_files/figure-gfm/col-stacked-1.png" width="350px" />

By default the fill legend is reversed (`reverse_legend = TRUE`), so it
reads in the same top-to-bottom order as the stack. For dodged bars,
switch that off so the legend follows the series order:

``` r
scenario <- expand_grid(regio    = c("Noord", "Oost", "Zuid", "West"),
                        scenario = c("basispad", "beleidsvariant")) |>
  mutate(effect = round(runif(n(), -2, 6), 1))

cpb_col(scenario, x = regio, y = effect, fill = scenario,
  position = "dodge",
  index = c(6, 2),
  reverse_legend = FALSE,
  title = "Effect per regio en scenario",
  ylab  = "% mutatie")
```

<img src="chart-types_files/figure-gfm/col-dodged-1.png" width="350px" />

## Horizontal bars

`orientation = "horizontal"` flips the chart and moves the gridlines to
the value axis. Here `ylab` labels the *category* axis (still the
caption above the panel) and `xlab` labels the value axis at the bottom.
`pct_axis` formats the value axis with Dutch percentage labels,
`value_limits` fixes its range, and the panel starts exactly at the zero
axis:

``` r
groepen <- c("tot 120% wml", "120% wml - mod.", "1 - 1,5x mod.",
             "1,5 - 2x mod.", "2 - 3x mod.", "boven 3x mod.")
auto <- tibble(
  # reversed levels put the first-named group at the top after the flip
  inkomensgroep = factor(groepen, levels = rev(groepen)),
  share         = c(62, 35, 26, 19, 14, 13)
)

cpb_col(auto, x = inkomensgroep, y = share,
  orientation  = "horizontal",
  pct_axis     = TRUE,
  value_limits = c(0, 70),
  width        = 0.6,
  title = "Aandeel huishoudens zonder auto naar inkomen",
  ylab  = "inkomensgroep",
  xlab  = "huishoudens zonder auto")
```

<img src="chart-types_files/figure-gfm/col-horizontal-1.png" width="350px" />

A horizontal *dodged* bar needs one extra trick. Under `coord_flip()`
the dodge draws the *last* factor level on top within each group, so to
show (say) 2021 above 2024 you reverse the `jaar` levels, swap the
`index` to keep each year its colour, and let `reverse_legend` restore
the reading order:

``` r
pv <- expand_grid(
  jaar          = factor(c(2021, 2024), levels = c(2024, 2021)),
  inkomensgroep = factor(groepen, levels = rev(groepen))
) |>
  mutate(share = c(10, 14, 19, 22, 26, 33,   # 2021
                   19, 25, 31, 38, 47, 59))  # 2024

cpb_col(pv, x = inkomensgroep, y = share, fill = jaar,
  position     = "dodge",
  orientation  = "horizontal",
  index        = c(2, 6),        # level order is (2024, 2021)
  value_breaks = seq(0, 70, 10),
  value_limits = c(0, 70),
  width        = 0.85,
  title = "Zonnepanelen naar inkomen",
  ylab  = "inkomensgroepen",
  xlab  = "aandeel binnen inkomensgroep (%)")
```

<img src="chart-types_files/figure-gfm/col-horizontal-dodged-1.png" width="350px" />

Note `value_breaks`: custom breaks for the value axis go through the
wrapper, not through a second `scale_y_continuous()`, which would
silently replace the wrapper’s percentage labels and zero-flush
expansion.

For a two-level category axis – categories grouped into blocks under
bold group names on one shared axis – see the `group` argument in
`vignette("layouts")`.

# Area charts

`cpb_area()` draws the recurring share-of-total-over-time figure. With
`pct_axis = TRUE` the y axis gets Dutch percentage labels:

``` r
bronnen <- c("gas", "elektriciteit", "warmte", "overig")
mix <- expand_grid(jaar = 2018:2027,
                   bron = factor(bronnen, levels = bronnen)) |>
  mutate(ruw = runif(n(), 1, 10)) |>
  mutate(aandeel = 100 * ruw / sum(ruw), .by = jaar)

cpb_area(mix, x = jaar, y = aandeel, fill = bron,
  pct_axis = TRUE,
  index    = c(6, 5, 2, 4),
  title    = "Energiemix van huishoudens",
  ylab     = "aandeel") +
  scale_x_continuous(breaks = seq(2018, 2027, 3), minor_breaks = 2018:2027,
                     guide = guide_axis(minor.ticks = TRUE))
```

<img src="chart-types_files/figure-gfm/area-1.png" width="350px" />

# Quantile boxplots

`cpb_box()` draws the CPB distributional figure: a box over the p25–p75
interquartile range with the median at p50, and thin errorbar whiskers
out to p5 and p95. It expects **precomputed quantile columns** (both
layers use `stat = "identity"`), so aggregate your microdata first:

``` r
groepen5 <- c("laagste 20%", "2e 20%", "midden 20%", "4e 20%", "hoogste 20%")
raw <- tibble(
  groep      = factor(rep(groepen5, each = 400), levels = groepen5),
  koopkracht = rnorm(2000, mean = rep(c(-3, -1.5, 0, 1.5, 3.5), each = 400), sd = 2)
)
kk <- raw |>
  summarise(
    p5  = quantile(koopkracht, 0.05),
    p25 = quantile(koopkracht, 0.25),
    p50 = quantile(koopkracht, 0.50),
    p75 = quantile(koopkracht, 0.75),
    p95 = quantile(koopkracht, 0.95),
    .by = groep
  )

cpb_box(kk, x = groep,
  p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
  orientation = "horizontal",
  title    = "Koopkracht per inkomensgroep",
  subtitle = "inkomensgroep",
  ylab     = "% koopkrachtmutatie")
```

<img src="chart-types_files/figure-gfm/box-single-1.png" width="350px" />

Boxes without a `fill` mapping are drawn in the CPB primary blue. Map
`fill` and pass a `position_dodge()` for grouped boxes – for example one
pair of years per income group:

``` r
kk2 <- expand_grid(kk, jaar = factor(c(2026, 2027))) |>
  mutate(across(p5:p95, \(q) q + (jaar == "2027") * 0.8))

cpb_box(kk2, x = groep,
  p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
  fill     = jaar,
  position = position_dodge(width = 0.6),
  index    = c(6, 2),
  title    = "Koopkracht per jaar, 2026 en 2027",
  ylab     = "% koopkrachtmutatie") +
  scale_y_continuous(labels = label_number_nl())
```

<img src="chart-types_files/figure-gfm/box-dodged-1.png" width="700px" />

## Box styles

`box_style` selects how the boxes are drawn. Besides the default
`"ggcpb"` construction above there is `"james"`, the legacy plotter’s
box – borderless, plain capless whiskers, a black median line extending
past the box, and the median value printed above it – and `"modern"`,
the designer variant with light-blue boxes, a thick dark-blue median and
the quartile values printed below the box ends:

``` r
cpb_box(kk, x = groep,
  p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
  box_style   = "modern",
  orientation = "horizontal",
  width       = 0.35,
  title    = "Koopkracht per inkomensgroep",
  subtitle = "inkomensgroep",
  ylab     = "% koopkrachtmutatie") +
  scale_y_continuous(labels = label_number_nl(accuracy = 0.1))
```

<img src="chart-types_files/figure-gfm/box-modern-1.png" width="350px" />

Both styles print value labels by default (`box_labels = FALSE` turns
them off, `label_accuracy` controls their rounding) and draw
single-colour boxes: a `fill` mapping is only supported by `"ggcpb"`.
For the vertically grouped distributional layout (boxes organised under
bold group headings) see `vignette("layouts")`.

# Scatter plots

`cpb_scatter()` draws points in the house style. Without a `colour`
column the points are CPB blue; a *numeric* `colour` column gets the
continuous CPB gradient, a discrete one the discrete palette:

``` r
hh <- tibble(inkomen = round(rlnorm(400, log(2500), 0.35))) |>
  mutate(energierekening = round(90 + 0.04 * inkomen + rnorm(n(), 0, 35)),
         koopkracht      = round(rnorm(n(), (inkomen - 2500) / 1500, 2), 1))

cpb_scatter(hh, x = inkomen, y = energierekening, colour = koopkracht,
  title = "Energierekening naar inkomen",
  ylab  = "energierekening (euro per maand)",
  xlab  = "besteedbaar inkomen (euro per maand)",
  colourlab = "koopkracht (%)") +
  scale_x_continuous(labels = label_euro_nl())
```

<img src="chart-types_files/figure-gfm/scatter-1.png" width="700px" />

# Histograms

`cpb_hist()` bins a column of observations into house-blue bars with
white outlines; set `binwidth` or `bins`. Map `fill` for grouped
histograms:

``` r
duur <- tibble(maanden = round(rgamma(1200, 8, 0.6)))

cpb_hist(duur, x = maanden, binwidth = 2,
  title = "Verdeling van de duur",
  ylab  = "aantal",
  xlab  = "duur (maanden)")
```

<img src="chart-types_files/figure-gfm/hist-1.png" width="350px" />

# Where next

- Arrange more than one chart’s worth of information – grouped category
  axes, facets and choropleth maps – in `vignette("layouts")`.
- Forecast windows, composing charts by hand and export at CPB page
  sizes are in `vignette("recipes")`.

For a rendered gallery of all chart types against the published
reference figures, run `inst/examples/smoke_test_plots.R`.
