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

- **[Chart types](doc/chart-types.md)** --
  `vignette("chart-types")`: a cookbook with one worked example per
  CPB chart type (lines, stacked/dodged columns, horizontal bars,
  areas, quantile boxplots, overlays) and the house conventions they
  follow.
- **[Walkthrough](doc/ggcpb.md)** -- `vignette("ggcpb")`: the
  composable core -- theme, scales, palettes, formatters, fonts and
  export -- for building figures from raw `ggplot2`.

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
`cpb_font_family()` reports the family in use. **Note:** confirm with
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
in `references/plots/`.
