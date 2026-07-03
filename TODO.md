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

All of these are bundled in the `style = "nplot"` preset on
`theme_cpb()` and the wrappers; any knob set explicitly overrides the
preset. Defaults (`style = "ggplot"`) are unchanged, so existing plots
keep the hand-rolled CPB ggplot look.

`zeroline` resolves automatically under `style = "nplot"`: always for
bars/areas (anchored at zero), and only when the data spans zero for
lines and boxes (nplot's bold-axis-if-zero behaviour). `cpb_line()`
draws a single unmapped series in CPB blue at nplot line weight
(`0.55`), and `cpb_box()` fills unmapped boxes in CPB blue with thin
(`0.25`) strokes. `cpb_line()`/`cpb_area()` (and vertical `cpb_box()`)
render `ylab` as the subtitle -- the unit caption above the panel --
matching `cpb_col()`.

Under the preset the value axis is also *zero-flush*: when the data
does not cross zero, the zero side of the scale gets no expansion, so
bars/areas sit directly on the axis line and the category ticks touch
it -- the panel edge is the axis, as in nplot output. `cpb_col()`
takes `value_breaks` so custom breaks go on the wrapper-built scale
instead of a second (conflicting) `scale_y_continuous()`. The nplot
legend also uses a tighter key-to-label gap (3.5 pt), zero legend
margins and a 6 pt gap to the panel, and the preset's bottom plot
margin is 8 pt instead of the classic 25 pt (which existed for
overlay legends drawn below the panel). nplot ticks come with a
hairline axis line on the category axis so the tick strip never
floats when the lowest break is not at zero, and nplot line charts
draw the panel without expansion (`coord_cartesian(expand = FALSE)`),
so ticks meet the outermost gridlines. Titled wrappers always reserve
the subtitle line (a blank one if none is given), keeping the gap
between title and panel stable.

## Remaining ideas

- Sequential blue palette: reference p17_img19 (deciles) uses a 10-step
  blue ramp that the package palettes do not cover yet.
- Minor tick marks on continuous (year) axes are currently added
  per-plot via `guide_axis(minor.ticks = TRUE)` (smoke-test figures
  1/4/9/10); could be folded into `cpb_line()`/`cpb_col()`.
- The bundled Rijksoverheid font has no em-dash glyph (it renders as
  `...`); consider substituting en/em dashes in title strings
  automatically.
