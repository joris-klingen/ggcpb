# TODO

The package encodes a single CPB house style, calibrated against the
published reference figures in `references/plots/` (see the smoke test
`inst/examples/smoke_test_plots.R` for side-by-side recreations).

## Open items

- **Robustness**: check and improve behaviour on edge cases -- empty
  or single-row data, NA-heavy series, very long labels, many series,
  date axes. (Missing font backends and non-TTF devices are covered:
  `cpb_font_family()` falls back to the default family on
  `pdf()`/`postscript()` and when registration fails.)
- **Input validation**: the wrappers barely validate their input;
  misspelled columns or wrong types surface as downstream ggplot2
  errors. Add classed `rlang::abort()` errors with hints.
- **Visual snapshot tests**: the legend position is pinned by
  rendered-pixel tests, but the rest of the styling (gridlines,
  spacing, titles) has no regression net; add `vdiffr` snapshots for
  the smoke-test figures.
- **Adoption infrastructure**: pkgdown site, `NEWS.md` plus a
  versioning/lifecycle policy, colour-blindness documentation for the
  palettes, an optional source-line/footer argument in `save_cpb()`.
- **More chart types**: `cpb_scatter()` and `cpb_hist()` are done, as
  are the forecast window (`forecast_x`, the nplot "raming" overlay)
  and the `cpb_line()` uncertainty band (`ymin`/`ymax`). cpb_box() supports three box styles (ggcpb, the legacy james
  construction, and the designer modern variant). Still open:
  heatmap (built from the core in the walkthrough for now), fan
  charts (the dedicated fan palette of the legacy plotter has not
  been recovered), the standardized purchasing-power plot, faceted
  variants, and geographic maps.
- **Sequential blue palette**: reference p17_img19 (deciles) uses a
  10-step blue ramp that the package palettes do not cover yet.
- **Minor tick marks on continuous (year) axes** are currently added
  per plot via `guide_axis(minor.ticks = TRUE)` (see the vignettes and
  smoke-test figures 1/4/9/10); could be folded into the wrappers.
- **Em dash**: the bundled Rijksoverheid font has no em-dash glyph (it
  renders as `...`); consider substituting en/em dashes in title
  strings automatically.
