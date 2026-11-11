# ggcpb

`ggcpb` encodes the CPB (Centraal Planbureau) house plotting style on
a `ggplot2` backend: a theme, colour palettes and scales, Dutch-locale
number formatters, a figure export helper, and high-level wrappers for
the common CPB chart types. Everything returns real `ggplot` objects
that you can keep extending with `+`.

**New to ggcpb? Start with the introduction:
[introduction](doc/introduction.html).**
It shows how to create figures with Excel and/or R.

> **Status: work in progress.** The style is calibrated against a set
> of published reference figures, but robustness still needs to be
> checked and improved (edge cases, unusual data shapes, device/font
> environments), and wrappers for more chart types will be added
> later. 

## Documentation

The documentation is three HTML files in the [doc/](doc/) folder.
Download the file and open it in your browser. On GitHub, open the file
and choose "Download raw file".

- **[Introduction](doc/introduction.html)** --
  `vignette("introduction")`: creating figures with Excel and/or R.
- **[Import](doc/import.html)** -- `vignette("import")`: building
  figures from CSV files, with Excel and without R.
- **[Extended information](doc/extended.html)** --
  `vignette("extended")`: more colours, columns, boxplot styles and
  maps, and what is possible in R only.

The HTML files are made from `vignettes/` with
`Rscript tools/render_vignettes.R`.

## Install

Open RStudio and type in the Terminal tab:

```
R -e "install.packages('remotes'); remotes::install_github('joris-klingen/ggcpb')"
```

Or type in the R Console:

```r
install.packages("remotes")
remotes::install_github("joris-klingen/ggcpb")
```

Then load it with `library(ggcpb)`. The code is on
<https://github.com/joris-klingen/ggcpb>.

To develop the package, use
`devtools::load_all("path/to/ggcpb")` on a checkout.

## Quick start

```r
library(ggcpb)

df <- data.frame(jaar = 2021:2024, groep = rep(c("a", "b"), 4),
                 waarde = c(10, 15, 12, 18, 14, 20, 16, 22))

cpb_col(df, x = jaar, y = waarde, fill = groep,
  title = "Waarde per groep",
  ylab  = "mld euro")
```

The wrappers -- `cpb_line()`, `cpb_col()`, `cpb_box()`,
`cpb_boxplot_extended()`, `cpb_dot()`, `cpb_scatter()`, `cpb_hist()`,
`cpb_donut()` and `cpb_map()` -- accept a data.frame or data.table
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

The house font, Rijksoverheid Sans Text, may not be published, so it
is not in this repository. 

Without it, the bundled Source Sans 3 (SIL OFL 1.1, see `inst/fonts/`)
stands in. Its text runs about 10-15% wider. `save_cpb()` and printing
a chart then warn that the figure does not meet the Rijkshuisstijl.
`options(ggcpb.font_warning = FALSE)` turns that off. If no font
registers at all, the theme falls back to the ggplot2 default with a
single warning. `cpb_register_fonts()` re-runs registration,
`cpb_font_family()` reports the family in use.

Point it at a directory to register your own fonts alongside the house
one -- family and regular/bold/italic face are read from each file's
metadata, falling back to the file name:

```r
cpb_register_fonts(path = "~/fonts", recursive = TRUE)
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

## Tests

```r
devtools::test()
```
