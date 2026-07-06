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
- **Input validation** (priority): the wrappers barely validate their
  input; misspelled columns or wrong types surface as downstream
  ggplot2 errors. Add classed `rlang::abort()` errors with hints.
- **Stability under varying group counts** (priority): extend the
  robustness/stability testing to figures whose number of groups or
  categories changes -- 1 up to many series and categories per chart
  type. The legend sweep covers the legend key position for 1-8
  items; still open is what varying group counts do to the *rest* of
  the layout (panel height, bar widths, palette assignment, dodge
  spacing) and to horizontal charts whose category count grows.
- **Smart default plot height** (priority): the export height is a
  fixed preset (2.98 in report / 2.5 in presentation), but the right
  height often follows from the content -- more than one sensible
  default exists. E.g. a horizontal bar or box chart needs height
  proportional to its number of categories; a legend with more rows
  needs extra height to keep the panel proportions; faceted figures
  need height per facet row. Let `save_cpb()` (or the wrappers)
  derive a sensible default from the plot object, with the presets as
  fallback.
- **Facets in nicerplot style**: done -- every wrapper accepts
  `facet` (plus `facet_ncol`/`facet_scales`); the facet title is a
  bold strip *below* each panel and every panel gets its own axes
  (see the cookbook vignette and smoke-test figure 19). Still open:
  smarter default export height for faceted figures (see the plot
  height item above).
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
  been recovered), the standardized purchasing-power plot, and
  geographic maps. (Faceting has its own item above.)
- **Sequential blue palette**: reference p17_img19 (deciles) uses a
  10-step blue ramp that the package palettes do not cover yet.
- **Minor tick marks on continuous (year) axes** are currently added
  per plot via `guide_axis(minor.ticks = TRUE)` (see the vignettes and
  smoke-test figures 1/4/9/10); could be folded into the wrappers.
- **Em dash**: the bundled Rijksoverheid font has no em-dash glyph (it
  renders as `...`); consider substituting en/em dashes in title
  strings automatically.
