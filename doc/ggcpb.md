ggcpb
================

``` r
library(ggcpb)
library(ggplot2)
library(dplyr)
```

This vignette walks through the composable core of `ggcpb` – the theme,
the discrete and continuous colour scales, manual palette selection, the
Dutch-locale formatters, and `save_cpb()` – the building blocks for
styling figures you construct from raw `ggplot2`. For the high-level
chart wrappers (`cpb_line()`, `cpb_col()`, `cpb_area()`, `cpb_box()`),
which apply all of this in one call, see `vignette("chart-types")`.

# The theme —-

`theme_cpb()` applies the CPB house style on top of
`ggplot2::theme_minimal()`. The default `style = "cpb_default"` is the
look of published CPB figures: hairline black gridlines at labelled
breaks only, black tick marks (with an axis line) on the category axis,
absolute text sizes (9 pt bold title, 7 pt axis text), a flush-left
bottom legend, and the CPB-blue plot background.

``` r
pv_counts <- tibble(
  jaar = 2019:2023,
  aantal = c(120, 135, 128, 150, 162)
)

ggplot(pv_counts, aes(jaar, aantal)) +
  geom_col(fill = cpb_cols(6)) +
  labs(title = "Aantal PV-meldingen per jaar", x = NULL, y = "aantal") +
  theme_cpb()
```

<img src="ggcpb_files/figure-gfm/theme-basic-1.png" width="447px" />

For a chart built with `coord_flip()`, pass `orientation = "horizontal"`
so the gridlines (and ticks) move to the right axes:

``` r
ggplot(pv_counts, aes(factor(jaar), aantal)) +
  geom_col(fill = cpb_cols(6)) +
  coord_flip() +
  labs(title = "Aantal PV-meldingen per jaar", x = NULL, y = "aantal") +
  theme_cpb(orientation = "horizontal")
```

<img src="ggcpb_files/figure-gfm/theme-horizontal-1.png" width="447px" />

`style = "ggplot"` switches to the lighter look of the hand-rolled CPB
ggplot2 scripts: CPB-grey gridlines including minors, no ticks, 6 pt
axis text, legend on the right:

``` r
ggplot(pv_counts, aes(jaar, aantal)) +
  geom_col(fill = cpb_cols(6)) +
  labs(title = "Aantal PV-meldingen per jaar", x = NULL, y = "aantal") +
  theme_cpb(style = "ggplot")
```

<img src="ggcpb_files/figure-gfm/theme-ggplot-style-1.png" width="447px" />

Both presets are only defaults. Each element is an individual argument
that overrides the preset: `minor` (minor gridlines), `ticks`
(category-axis tick marks), `grid_colour`/`grid_linewidth`,
`axis_text_size`, `legend`/`flush_legend` (position and the flush-left
bottom block), and `legend_key_size`. `grid` selects which axes get
gridlines (`"value"`, `"both"`, `"none"`, `"x"`, `"y"`), and
`background = FALSE` (or the `theme_cpb_min()` shorthand) drops the CPB
background fill – useful for small multiples.

# Discrete scales —-

`scale_fill_cpb_d()` / `scale_colour_cpb_d()` draw from one of the three
CPB palettes (`"qualitative"`, `"discr"`, `"sequential"`) and route `NA`
values to the CPB NA colour automatically.

``` r
pv_by_group <- tibble(
  jaar = rep(2021:2023, each = 2),
  groep = rep(c("huishoudens", "bedrijven"), 3),
  aantal = c(60, 40, 68, 52, 74, 58)
)

ggplot(pv_by_group, aes(jaar, aantal, fill = groep)) +
  geom_col(position = "stack") +
  labs(title = "PV-meldingen naar groep", x = NULL, y = "aantal", fill = NULL) +
  scale_fill_cpb_d() +
  theme_cpb()
```

<img src="ggcpb_files/figure-gfm/scales-discrete-1.png" width="447px" />

# Continuous scales —-

`scale_fill_cpb_c()` / `scale_colour_cpb_c()` build a full gradient
across the CPB sequential palette, from its lightest to its darkest
entry.

``` r
ggplot(mtcars, aes(wt, mpg, colour = hp)) +
  geom_point(size = 2) +
  labs(title = "Gewicht versus verbruik", x = "gewicht", y = "mpg", colour = "pk") +
  scale_colour_cpb_c() +
  theme_cpb(legend = "right")
```

<img src="ggcpb_files/figure-gfm/scales-continuous-1.png" width="447px" />

# Manual palette selection —-

`scale_fill_cpb_manual()` / `scale_colour_cpb_manual()` (and the
`cpb_cols()` accessor) select and order specific palette positions, for
the common case of a small, deliberately ordered subset – e.g.
`index = c(6, 2)` for the recurring CPB blue/magenta pair.

``` r
raming_vergelijking <- tibble(
  scenario = factor(c("basispad", "hoog scenario"), levels = c("basispad", "hoog scenario")),
  effect = c(-1.2, 2.4)
)

ggplot(raming_vergelijking, aes(scenario, effect, fill = scenario)) +
  geom_col() +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.25) +
  labs(title = "Effect op koopkracht", x = NULL, y = "%-punt", fill = NULL) +
  scale_fill_cpb_manual(index = c(6, 2)) +
  theme_cpb(legend = "none")
```

<img src="ggcpb_files/figure-gfm/scales-manual-1.png" width="447px" />

(The wrappers add that black zero line for you; when composing from raw
`ggplot2` it is a one-line `geom_hline()`.)

# Dutch-locale formatters —-

`label_euro_nl()`, `label_pct_nl()` and `label_number_nl()` wrap
`scales::label_*()` with the Dutch thousands separator (`.`) and decimal
mark (`,`).

``` r
kosten_vergelijking <- tibble(
  maatregel = factor(c("optie A", "optie B"), levels = c("optie A", "optie B")),
  kosten = c(1250000, 2100000)
)

ggplot(kosten_vergelijking, aes(maatregel, kosten, fill = maatregel)) +
  geom_col() +
  labs(title = "Geraamde kosten per maatregel", x = NULL, y = NULL, fill = NULL) +
  scale_y_continuous(labels = label_euro_nl()) +
  scale_fill_cpb_manual(index = c(6, 2)) +
  theme_cpb(legend = "none")
```

<img src="ggcpb_files/figure-gfm/formatters-1.png" width="447px" />

# Raw values and tokens —-

`cpb_tokens()` exposes the raw design tokens (palettes, background, grid
and NA colours) and `cpb_pal()`/`cpb_cols()` generate palette subsets,
for anything the scales above do not cover:

``` r
cpb_cols(c(6, 2))
#>         6         2 
#> "#005faf" "#e6006e"
cpb_tokens()$bg
#> [1] "#eef8ff"
```

# Fonts —-

The bundled Rijksoverheid Sans Text family registers automatically when
the package loads; `cpb_font_family()` reports the family name
`theme_cpb()` resolves to (`"RijksoverheidSansText"`, or `""` when
falling back to the ggplot2 default), and `cpb_register_fonts()` re-runs
registration in a fresh worker process.

# Saving figures —-

`save_cpb()` enforces the CPB page widths (half page: 2.98 in, full
page: 5.96 in) and renders with `ragg::agg_png()` by default, so the
bundled CPB font renders correctly.

``` r
p <- ggplot(pv_counts, aes(jaar, aantal)) +
  geom_col(fill = cpb_cols(6)) +
  labs(title = "Aantal PV-meldingen per jaar", x = NULL, y = "aantal") +
  theme_cpb()

save_cpb("pv_counts.png", p, page = "half")
save_cpb("pv_counts_presentatie.png", p, page = "half", preset = "presentation")
```
