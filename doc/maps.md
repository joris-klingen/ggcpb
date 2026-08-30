Maps
================

``` r
library(ggcpb)
library(ggplot2)
library(dplyr)
set.seed(42)
```

`cpb_map()` draws a value per Dutch municipality, COROP region or
province on bundled generalised CBS/Kadaster boundaries (2025, via
cartomap), so no geo packages or downloads are needed. Regions are
separated by thin background-colour seams, and the legend sits inside
the panel at top-left – in the empty North Sea corner of the country.

Every map here is written out through `save_cpb()` first and displayed
from that file, exactly as it would look saved for a report – not
printed directly. `save_cpb()` auto-fits a `cpb_map()` panel to the
boundaries’ true geographic aspect ratio (see “Styling and raw
boundaries” below), which only applies on that path; printed directly
through knitr’s own plotting device, a map would sit letterboxed inside
a guessed `fig.height` instead, the way `cpb_donut()`’s `panel_size`
does (see the donut section of `vignette("chart-types")`):

``` r
show_map <- function(...) {
  f <- tempfile(fileext = ".png")
  invisible(utils::capture.output(save_cpb(f, cpb_map(...), page = "half")))
  knitr::include_graphics(f)
}
```

# Classed maps

CPB choropleths are usually *classed*: the values are binned into a
handful of ordered classes and filled with a light-to-dark ramp.
`cpb_cut()` does the binning with tidy Dutch class labels (“lager dan
20%”, “20% - 30%”, …, “60% en hoger”), and `palette = "blues"` gives the
house blue ramp. Regions are joined by CBS code (`"GM0014"`, `"CR01"`,
`"PV20"`) or by name, whichever matches the `region` column best;
regions without a value get the CPB missing-value grey, and unmatched
regions raise a warning:

``` r
gemeenten <- tibble(code = unique(cpb_nl_geo("gemeente")$code)) |>
  mutate(aandeel = runif(n(), 5, 75),
         klasse  = cpb_cut(aandeel, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                           labeller = label_pct_nl()))

show_map(gemeenten, region = code, value = klasse,
  palette = "blues",
  title   = "Aandeel huishoudens met zonnepanelen",
  filllab = "aandeel")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d16ac40445.png" alt="" width="350px" />

The map fills the half-page width (`page = "half"`); because the
Netherlands is taller than it is wide, a map figure needs a taller
canvas than a chart – `save_cpb()` sizes that automatically (see
“Styling and raw boundaries” below), which is exactly why every map here
goes through `show_map()`/`save_cpb()` rather than being printed
directly. A title that runs wider than the panel triggers a warning from
`save_cpb()`, which suggests breaking it over two lines with `"\n"` (see
the province example below).

`cpb_cut()` is a house-styled wrapper around `cut()`: give it the
`breaks` (including the outer bounds, `Inf` for an open top class) and a
formatter (`label_pct_nl()`, `label_euro_nl()`, `label_number_nl()`),
and it returns an ordered factor whose levels read the way published
figures label them. A single integer (`breaks = 5`) asks for that many
equal-width bins. The `"blues"` palette works in every CPB scale, not
just maps – use it for classed bars too.

# Continuous maps

For a raw numeric value, `cpb_map()` fills with the CPB sequential
gradient and a compact colourbar (vertical, top-left, alongside the
map). As elsewhere the unit caption goes in `subtitle`; there is no
value axis, so `ylab` does not apply here:

``` r
gemeenten_ct <- tibble(code = unique(cpb_nl_geo("gemeente")$code)) |>
  mutate(index = rnorm(n(), 100, 15))

show_map(gemeenten_ct, region = code, value = index,
  title    = "Voorbeeldindex per gemeente",
  subtitle = "index (Nederland = 100)")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d111c4b084.png" alt="" width="350px" />

# Provinces and COROP regions

`level` selects the boundaries – `"provincie"` and `"corop"` for the
coarser levels – and joining by *name* works too. A discrete `value`
column gets the discrete CPB palettes (pick colours with `index`):

``` r
provincies <- tibble(naam = unique(cpb_nl_geo("provincie")$name)) |>
  mutate(klasse = factor(
    sample(c("onder gemiddeld", "boven gemiddeld"), n(), replace = TRUE),
    levels = c("onder gemiddeld", "boven gemiddeld")
  ))

show_map(provincies, region = naam, value = klasse, level = "provincie",
  fill_index = c(2, 6),
  title = "Groei ten opzichte van het\nlandelijk gemiddelde")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d176f64953.png" alt="" width="350px" />

A raw numeric `value` works the same way at this level too, with the
same continuous gradient and colourbar the gemeente example above used:

``` r
provincies_ct <- tibble(naam = unique(cpb_nl_geo("provincie")$name)) |>
  mutate(index = rnorm(n(), 100, 12))

show_map(provincies_ct, region = naam, value = index, level = "provincie",
  title    = "Voorbeeldindex per provincie",
  subtitle = "index (Nederland = 100)")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d110d835c3.png" alt="" width="350px" />

COROP regions – the 40-region tier between municipality and province
that Dutch regional-economic statistics are usually published at – work
identically, classed:

``` r
corops <- tibble(code = unique(cpb_nl_geo("corop")$code)) |>
  mutate(aandeel = runif(n(), 5, 75),
         klasse  = cpb_cut(aandeel, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                           labeller = label_pct_nl()))

show_map(corops, region = code, value = klasse, level = "corop",
  palette = "blues",
  title   = "Aandeel huishoudens met zonnepanelen",
  filllab = "aandeel")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d160f44107.png" alt="" width="350px" />

and continuous:

``` r
corops_ct <- tibble(code = unique(cpb_nl_geo("corop")$code)) |>
  mutate(index = rnorm(n(), 100, 15))

show_map(corops_ct, region = code, value = index, level = "corop",
  title    = "Voorbeeldindex per COROP-gebied",
  subtitle = "index (Nederland = 100)")
```

<img src="../../../../../../private/var/folders/93/zq1v1syn35b6x4hkyfpjvt8d4gyw7c/T/RtmpPuZZvr/file98d11d9762dc.png" alt="" width="350px" />

# Styling and raw boundaries

`save_cpb(plot, page = ...)` auto-fits a `cpb_map()` panel to the
boundaries’ true geographic aspect ratio when `height` is left at its
default, so the map fills the figure exactly instead of sitting
letterboxed inside a fixed-height page – no more guessing a `height` by
eye. Pass `height` explicitly to opt back into a fixed height, e.g. to
match a neighbouring figure.

The border seams are controlled with `border_colour` (default the CPB
background colour; pass `"white"` for more contrast, e.g. on a two-class
map) and `border_linewidth` (default `0.15`). The legend defaults to
`legend = "topleft"` inside the panel; pass `legend = "bottom"` for the
flush bottom-left legend of the other wrappers, or `legend = "none"`.
`reverse` flips the sequential gradient and `na_fill` overrides the
missing-value colour.

For anything the wrapper does not cover, the raw boundary tables are
available through `cpb_nl_geo(level)`: one row per polygon vertex with
the region `code` and `name`, ready for `ggplot2::geom_polygon()` (use
`part` as `group` and `ring` as `subgroup`).

``` r
head(cpb_nl_geo("provincie"), 3)
#>   code      name   part     ring      x      y
#> 1 PV20 Groningen PV20.1 PV20.1.1 269919 540356
#> 2 PV20 Groningen PV20.1 PV20.1.1 269519 541648
#> 3 PV20 Groningen PV20.1 PV20.1.1 270634 543238
```
