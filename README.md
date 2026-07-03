# ggcpb

`ggcpb` encodes the CPB (Centraal Planbureau) house plotting style on a
`ggplot2` backend. It gives you a composable core -- a theme, colour
palettes and scales, Dutch-locale number formatters, and a figure export
helper -- plus a thin layer of high-level wrapper functions for the most
common CPB chart types. The wrapper layer returns real `ggplot` objects
that you can keep extending with `+`.

`ggcpb` is the `ggplot2`-native counterpart to the internal `nicerplot`
package (`devtools::load_all('../Tech/nicerplot')`, `nplot()`). It does
not depend on or duplicate `nicerplot`.

## Install / load

Like `nicerplot`, this package is developed with `devtools::load_all()`
rather than being installed from a repository:

```r
devtools::load_all("../Tech/ggcpb")
```

To install it properly instead (e.g. once it lives in its own checkout):

```r
devtools::install("../Tech/ggcpb")
# or, from a git checkout:
# devtools::install_git("<internal-git-url>/ggcpb")
```

## Before / after

A plain `ggplot2` bar chart:

```r
library(ggplot2)

p <- ggplot(mpg, aes(class, fill = class)) +
  geom_bar() +
  labs(title = "Aantal auto's per klasse", x = NULL, y = "aantal")

p
```

The same plot in CPB house style:

```r
library(ggcpb)

p +
  theme_cpb() +
  scale_fill_cpb_d()
```

`theme_cpb()` applies the fixed 9/8/7 pt CPB type scale, the CPB-blue
plot background, and value-axis-only gridlines in the CPB grid colour.
`scale_fill_cpb_d()` recolours the bars with the CPB qualitative
palette and routes any `NA` category to the CPB NA colour
automatically.

For the common chart types, the same figure can be built directly with
the wrapper layer:

```r
cpb_col(mpg, x = class, fill = class) +
  labs(title = "Aantal auto's per klasse", y = "aantal")
```

## Composable core

- `theme_cpb()` / `theme_cpb_min()` -- the house theme, with
  `orientation`/`grid` controlling which gridlines are drawn.
  `style = "nplot"` switches to the legacy `nplot()` look: hairline
  black gridlines at labelled breaks only, category-axis tick marks,
  7 pt axis text, 0.45 cm legend keys, and a flush-left bottom
  legend. Each element is also an individual knob (`minor`, `ticks`,
  `flush_legend`, `axis_text_size`, `legend_key_size`, `grid_colour`,
  `grid_linewidth`) that overrides the preset. The wrappers forward
  all of these, and additionally take `zeroline` for the bold black
  line at zero on the value axis (drawn automatically under
  `style = "nplot"` when zero is in range).
- `cpb_pal()`, `cpb_cols()`, `cpb_tokens()` -- palette generators and
  raw hex-value accessors.
- `scale_fill_cpb_d()` / `scale_colour_cpb_d()` / `scale_color_cpb_d()`
  -- discrete scales.
- `scale_fill_cpb_c()` / `scale_colour_cpb_c()` / `scale_color_cpb_c()`
  -- continuous scales (full gradient across the sequential palette).
- `scale_fill_cpb_manual()` / `scale_colour_cpb_manual()` /
  `scale_color_cpb_manual()` -- pick and order specific palette
  positions, e.g. `scale_fill_cpb_manual(index = c(6, 2))`.
- `label_euro_nl()`, `label_pct_nl()`, `label_number_nl()` -- Dutch-locale
  number formatters.
- `save_cpb()` -- figure export at the CPB half/full page width presets.

See `vignette("ggcpb")` for a full walkthrough of the composable
core, and `vignette("chart-types")` for a cookbook with one worked
example per CPB chart type.

## Wrapper layer

`cpb_col()`, `cpb_area()`, `cpb_line()` and `cpb_box()` cover the
recurring chart types in CPB projects: stacked/dodged columns, stacked
share-of-total areas, line charts, and the p5/p25/p50/p75/p95
box-and-errorbar combination used in distributional figures. Each
takes a data.frame (or data.table -- it inherits data.frame, so it
works transparently) plus tidy-eval column arguments, and returns a
real `ggplot` object with `theme_cpb()` and a CPB scale already
applied, ready to extend further with `+`.

## Fonts

`ggcpb` bundles the four Rijksoverheid Sans Text TTFs in
`inst/fonts/` and registers the `"RijksoverheidSansText"` family
automatically when the package loads, via both `systemfonts` (for
`ragg`/`ggplot2` output) and `sysfonts` (for a
`showtext::showtext_auto()` rendering path). Loading `ggcpb` never
fails because of fonts: if the bundled files or a backend are
unavailable, a single warning is issued and `theme_cpb()` falls back
to the ggplot2 default family.

**Font redistribution note:** confirm with CPB that the bundled
Rijksoverheid Sans Text TTFs may be committed to this package's
repository before adding them to `inst/fonts/`. If licensing is
uncertain, do not commit the binaries -- instead register the fonts
by path against the existing internal copy:

```r
systemfonts::register_font(
  name       = "RijksoverheidSansText",
  plain      = "../Tech/rijks_font/RijksoverheidSansText-Regular_2_0.ttf",
  bold       = "../Tech/rijks_font/RijksoverheidSansText-Bold_2_0.ttf",
  italic     = "../Tech/rijks_font/RijksoverheidSansText-Italic_2_0.ttf",
  bolditalic = "../Tech/rijks_font/RijksoverheidSansText-BoldItalic_2_0.ttf"
)
```

Call `cpb_register_fonts()` any time to re-run registration (e.g.
after toggling `showtext::showtext_auto()`, or in a fresh worker
process), and `cpb_font_family()` to check which family name
`theme_cpb()` is currently resolving to (`"RijksoverheidSansText"` or
`""`).

## Tests

```r
devtools::test()
```
