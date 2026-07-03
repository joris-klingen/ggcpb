# TODO

The package encodes a single CPB house style, calibrated against the
published reference figures in `references/plots/` (see the smoke test
`inst/examples/smoke_test_plots.R` for side-by-side recreations).

## Open items

- **Robustness**: check and improve behaviour on edge cases -- empty
  or single-row data, NA-heavy series, very long labels, many series,
  date axes, missing font backends, unusual devices.
- **More chart types**: wrappers for additional recurring CPB figures
  (e.g. scatter, histogram, faceted variants) -- for now these are
  built from the composable core, see the walkthrough vignette.
- **Sequential blue palette**: reference p17_img19 (deciles) uses a
  10-step blue ramp that the package palettes do not cover yet.
- **Minor tick marks on continuous (year) axes** are currently added
  per plot via `guide_axis(minor.ticks = TRUE)` (see the vignettes and
  smoke-test figures 1/4/9/10); could be folded into the wrappers.
- **Em dash**: the bundled Rijksoverheid font has no em-dash glyph (it
  renders as `...`); consider substituting en/em dashes in title
  strings automatically.
