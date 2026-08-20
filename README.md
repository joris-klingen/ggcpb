# ggcpb

`ggcpb` encodes the CPB (Centraal Planbureau) house plotting style on
a `ggplot2` backend: a theme, colour palettes and scales, Dutch-locale
number formatters, a figure export helper, and high-level wrappers for
the common CPB chart types. Everything returns real `ggplot` objects
that you can keep extending with `+`.

> **Status: work in progress.** The style is calibrated against a set
> of published reference figures, but robustness still needs to be
> checked and improved (edge cases, unusual data shapes, device/font
> environments), and wrappers for more chart types will be added
> later. 

## Documentation

- **[Setup](doc/ggcpb.md)** -- `vignette("ggcpb")`: the basic idea in
  a couple of plots -- one wrapper call, how it relates to ggplot2,
  using only the theme without the wrappers, and export.
- **[Chart types](doc/chart-types.md)** -- `vignette("chart-types")`:
  every CPB chart type in its default style -- lines, columns,
  horizontal bars, areas, quantile boxplot, scatter, histogram.
- **[Layout](doc/layout.md)** -- `vignette("layout")`: facets, grouped
  categories on one shared axis, and grouped boxplots.
- **[Annotation](doc/annotation.md)** -- `vignette("annotation")`:
  forecast windows and uncertainty bands, reference lines, text notes,
  and composing an extra series onto a chart.
- **[Box plots](doc/boxplots.md)** -- `vignette("boxplots")`: the
  three box styles, a fill per year, and grouped boxes with a fill
  per year.
- **[Colours](doc/colours.md)** -- `vignette("colours")`: how series
  get their house colours, choosing swatches by position, and the
  sequential ramp for ordered classes.
- **[Maps](doc/maps.md)** -- `vignette("maps")`: choropleths of the
  Netherlands at municipality, COROP or province level.

The links point to rendered versions (with figures) in `doc/`,
regenerated from `vignettes/` with `Rscript tools/render_vignettes.R`.

## Install / load

```r
devtools::load_all("path/to/ggcpb")   # develop-mode
# or install from a checkout:
devtools::install("path/to/ggcpb")
```

## Quick start

```r
library(ggcpb)

df <- data.frame(jaar = 2021:2024, groep = rep(c("a", "b"), 4),
                 waarde = c(10, 15, 12, 18, 14, 20, 16, 22))

cpb_col(df, x = jaar, y = waarde, fill = groep,
  title = "Waarde per groep",
  ylab  = "mld euro")
```

The wrappers -- `cpb_line()`, `cpb_col()`, `cpb_area()`, `cpb_box()`,
`cpb_scatter()`, `cpb_hist()` -- accept a data.frame or data.table
plus tidy-eval column arguments,
and apply `theme_cpb()` and a CPB scale: the published-figure look
(hairline black gridlines at labelled breaks, category-axis ticks,
black zero line, flush-left bottom legend). Time-series wrappers can
mark a forecast window (`forecast_x`) and `cpb_line()` can draw an
uncertainty band (`ymin`/`ymax`). Every style element is also an
individual argument for per-figure deviations.

On top of the wrappers sits the composable core: `theme_cpb()`,
`cpb_pal()`/`cpb_cols()`/`cpb_tokens()`, discrete/continuous/manual
`scale_*_cpb_*()` scales, the `label_euro_nl()`/`label_pct_nl()`/
`label_number_nl()` formatters, and `save_cpb()` for export at the
strict CPB half/full page widths.

## Fonts

The four Rijksoverheid Sans Text TTFs ship in `inst/fonts/` and
register automatically on load; if the files or a font backend are
missing, the theme falls back to the ggplot2 default with a single
warning. `cpb_register_fonts()` re-runs registration,
`cpb_font_family()` reports the family in use.

Point it at a directory to register your own fonts alongside the house
one -- family and regular/bold/italic face are read from each file's
metadata, falling back to the file name:

```r
cpb_register_fonts(path = "~/fonts", recursive = TRUE)
#>                  family               faces n_files systemfonts sysfonts windows
#> 1 RijksoverheidSansText plain, bold, italic       3        TRUE     TRUE   FALSE
```

Registering a font does not by itself teach a graphics device about
it: on the base Windows device that shows up as `"font family not
found in Windows font database"` even though the font is registered.
`cpb_register_fonts()` therefore calls `showtext::showtext_auto()` by
default, which draws text on *any* device. (Attaching the package does
not -- `.onLoad` registers with `enable_showtext = FALSE`, so
`library(ggcpb)` never changes how the rest of your session renders
text.) The `ragg`/`svglite` devices consult `systemfonts` and need
none of this, which is why `save_cpb()` works either way.

With showtext on, keep its DPI equal to the device's or text comes out
the wrong size while the rest of the plot is fine:

```r
showtext::showtext_opts(dpi = 300)
ggsave("fig.png", p, dpi = 300)
showtext::showtext_opts(dpi = 96)   # back to screen
```

**Note:** confirm with
CPB that the bundled TTFs may be redistributed in this repository; if
not, register them by path against an internal copy instead:

```r
systemfonts::register_font(
  name   = "RijksoverheidSansText",
  plain  = "path/to/RijksoverheidSansText-Regular_2_0.ttf",
  bold   = "path/to/RijksoverheidSansText-Bold_2_0.ttf",
  italic = "path/to/RijksoverheidSansText-Italic_2_0.ttf"
)
```

## Tests

```r
devtools::test()
```

A visual end-to-end check lives in `inst/examples/smoke_test_plots.R`,
which renders every chart type against the published reference figures
in `references/plots/`. Next to it, `inst/examples/legend_stability_test.R`
sweeps legend-label lengths, category-label lengths, legend item counts,
chart types, canvas sizes and title combinations, and verifies in the
rendered pixels that the flush legend stays anchored to the same
bottom-left spot in every variant.
