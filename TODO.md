# TODO

## Promote nplot house-style conventions into `theme_cpb()` / `cpb_col()`

Most CPB reference figures were made with the internal `nplot()` (base R), whose styling
differs systematically from the hand-rolled *ggplot* scripts that `theme_cpb()` was built
from. The nplot look currently requires per-figure local `+ theme(...)` overrides on the
smoke-test call (see figure 8, "Zonnepanelen naar inkomen", in
`inst/examples/smoke_test_plots.R`). Promote these to first-class options so nplot
recreations become one-liners:

- **Minor gridlines** — add a toggle (e.g. `minor = TRUE/FALSE`); nplot draws lines only at
  labelled ticks (no minor gridlines).
- **Legend glyph size** — larger by default (nplot squares ≈ 0.45 cm vs current 0.25/0.30 cm).
- **Axis-text size** — expose it; nplot category/turned-axis labels are 7 pt vs current 6 pt.
- **Legend flush-left** — option for `legend.location = "plot"` + `legend.justification = "left"`.
- **Thicker bars / bigger panel** — bar `width` ~0.85 and a tighter `plot.margin`.

Tradeoff: keep plot 7 (hand-rolled ggplot, ref p11) matching — prefer **opt-in knobs** over
flipping hard defaults, or give plot 7 a local override.

## (a) Bold zero line, more solid black

nplot draws a heavier solid **black** line at the zero of the value axis (its `hline_bold = 0`
/ `x_axis_bold_if_zero`; see `references/nicerplot/R/gridlines.R`). Add an option to
`cpb_col()` (and where relevant the theme) to draw a bold black line at 0 on the value axis
(e.g. `geom_hline`/`geom_vline` at 0 with a larger `linewidth`, colour black). Visible in
reference p20_img24 and others.
