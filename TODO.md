# TODO

## Done: nplot house-style conventions are now first-class options

`theme_cpb()` (and every wrapper, which forwards them) now exposes the
nplot-look knobs directly, so nplot recreations are one-liners — see
figures 8–12 in `inst/examples/smoke_test_plots.R`:

- **Minor gridlines** — `minor = FALSE` draws gridlines only at labelled
  breaks.
- **Gridline colour/weight** — `grid_colour = "black"`,
  `grid_linewidth = 0.1` give the nplot hairline-black gridlines
  (default stays the CPB grey `cpb_tokens()$grid`).
- **Legend glyph size** — `legend_key_size = 0.45` (cm); default keeps
  the classic 0.25/0.30 cm keys.
- **Axis-text size** — `axis_text_size = 7` (default 6).
- **Legend flush-left** — `flush_legend = TRUE`
  (`legend.location = "plot"` + left justification + vertical keys).
- **Category-axis ticks** — `ticks = TRUE` draws black tick marks on
  the category axis.
- **Bold zero line** — `zeroline = TRUE` on `cpb_col()`, `cpb_line()`
  and `cpb_box()` draws a solid black line at 0 on the value axis
  (on top of bars, underneath lines/boxes), the nplot
  `hline_bold = 0` / `x_axis_bold_if_zero` behaviour.

Defaults are unchanged, so the hand-rolled-ggplot recreations (e.g.
figure 7, ref p11) still match.

## Remaining ideas

- A `style = "nplot"` preset that flips all of the above in one
  argument, if the per-call knob list proves too verbose in practice.
- Sequential blue palette: reference p17_img19 (deciles) uses a 10-step
  blue ramp that the package palettes do not cover yet.
- Minor tick marks on continuous (year) axes are currently added
  per-plot via `guide_axis(minor.ticks = TRUE)` (smoke-test figures
  9/10); could be folded into `cpb_line()`/`cpb_col()`.
