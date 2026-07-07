# Plan: aligning ggcpb with real CPB publication practice

*Written after an audit of the package (2026-07) against four current CPB
publications, inspected page by page: the **CEP 2026 raming**, the **MEV
2026 raming**, the themed study **"Verdeling van zonnepanelen over
huishoudens"**, and the survey study **"Erfbelasting in beeld"** -- plus
the reference figures already in `references/plots/`. Peer packages
consulted: bbplot (BBC), sgplot (Scottish Government), afcharts (UK
Analysis Function).*

## Status after review (2026-07)

The user reviewed this plan and scoped it to what CPB actually needs
in the package; the rest is handled in Word at publication time.

- **DONE (branch `feature/cut-blues-topbox`)**: classed choropleths via
  a new `cpb_cut()` helper (Dutch open-ended class labels around
  `cut()`, inspired by OS Amsterdam's `os_cut`) plus a `"blues"`
  sequential palette pixel-sampled from the erfbelasting figures; the
  `value_axis = "top"` box variant (the CEP fig 1.4 koopkracht layout).
  Covers `M1.2` (partly -- example not full inside-legend map),
  `M1.3`, and `M2.1`.
- **DROPPED** (done in Word, not the package): `cpb_duo()` two-panel
  composition (`M1.1`; figures are half-page, composed in Word);
  `bron`/`noot` source lines (`M1.4`); icon infographics and the
  kerngegevens table theme; `cpb_dot()` (`M2.2`) for now.
- **DEFERRED to TODO.md**: dual-axis line charts with
  "(linkeras)/(rechteras)" legend suffixes (`M2.3`) and the
  horizontal/rotated raming label (`M2.4`).
- **CLOSED**: fan charts -- confirmed absent from CEP 2026 and MEV 2026.

The maturity block (`M3`: pkgdown, NEWS/lifecycle, validation, vdiffr,
CVD docs, smart heights) remains valid and open, to be picked up
separately. The evidence and workstream detail below are kept for
reference.

## Where the package stands

The style core is strong and verified: pixel-calibrated theme, 368
tests, a 23-figure smoke test against published references, a 33-variant
legend-stability sweep, CI on Ubuntu + Windows, clean R CMD check, six
focused vignettes, and eight wrappers (`cpb_line/col/area/box/scatter/
hist/map` + `cpb_nl_geo`). Compared to peer government packages
(sgplot, afcharts are theme+palettes only; bbplot is theme+finalise),
ggcpb is already *broader*. Its remaining gaps are of two kinds:

1. **Figure-construction gaps**: things nearly every real publication
   figure does that the package cannot yet produce (see the evidence
   table below).
2. **Maturity gaps**: infrastructure that peers ship and an org-wide
   staple needs (pkgdown, versioning/lifecycle, validation, snapshots).

## Evidence: what real publication figures actually look like

| Pattern observed | Where | ggcpb today |
|---|---|---|
| **Two-panel side-by-side figure** ("links/rechts"), each panel its own bold title + italic unit caption; one magenta "Figuur N" title in the document text (NOT in-image) | ~90% of figures in CEP 2026 and zonnepanelen (figs 1.1, 1.2, 2.1-2.3; figs 3-9); triple-panel for maps (zonnepanelen fig 2) | missing -- single-panel only |
| Classed (binned) choropleths: 5-6 value classes, small swatch legend top-left *inside* the map, white background | zonnepanelen fig 2 (3 maps side by side) | continuous colourbar on blue background |
| **Sequential blue ramp** (6 steps light→dark) for classed fills; pink ramp as its sibling | erfbelasting figs 2.1/2.2; reference p17_img19 | only the pink sequential exists |
| Koopkracht boxplot: modern style + vertical grouping + **value axis drawn at the TOP** of the panel; two side-by-side year panels | CEP fig 1.4, fig 2.4 (the flagship figure) | modern+group exist; top axis and duo layout missing |
| **Dot-interval plot**: p5/p95 as light dots, p25-p75 capped interval, median dot, mean diamond, dashed connector | erfbelasting fig 3.1 (heir of nicerplot dot.R/whisker.R) | missing |
| **Dual-axis** charts, legend entries suffixed "(linkeras)"/"(rechteras)", or "zie linker/rechter y-as" annotations with dashed separator | CEP fig 1.2 (right), kader "Handel in onzekere tijden" | by-hand only |
| Raming window over horizontal charts, "raming" label rotated 90° | CEP fig 2.3 (right) | forecast window is vertical-only |
| Lines with point markers; grey dashed line-with-markers as overlay | erfbelasting fig 3.3; CEP figs 1.1/2.2 (bbp-groei) | by-hand only |
| "Noot: ..." and "Bron: CBS en CPB" lines under every figure | every publication | missing (planned `bron`) |
| Small italic footnote inside the panel, bottom-right | erfbelasting fig 3.2 | by-hand |
| Multi-line category labels with parenthetical detail ("5e kwintiel\n(82 dzd euro of hoger)") | zonnepanelen figs 3-7, CEP fig 1.4 | by-hand `\n` (works) |
| Value labels at bar ends (not centred) with Dutch formatting | CEP fig 1.3, common in bars | `value_labels` centres, no format control |
| Angled (~45°) x category labels | CEP kader | by-hand |
| **Fan charts (waaier)** | **absent from CEP 2026 and MEV 2026** -- current practice is the shaded raming window + variant lines | correctly absent; **deprioritise** (close the TODO item) |
| Pink-striped kerngegevens tables | every raming | out of scope for a plotting package (optional future: a `gt` theme) |
| Icon infographics (armoede fig 1.3) | CEP | out of scope |

Two validations worth noting: the in-image **panel** conventions (bold
title top-left, italic unit below it, bottom-left legend, axis title
bottom-right) match `theme_cpb()`/the wrappers exactly -- the document-
level "Figuur N" title lives in the surrounding text, not the image.
And the erfbelasting legend "title" line ("op een erfenis van...")
matches our italic `colourlab`/`filllab`. The style core is right; the
gaps are composition and a handful of chart forms.

---

## Workstreams

Ordered by real-world frequency x effort. Each has an API sketch,
implementation notes, and acceptance criteria. M1-M2 are the
publication-alignment core; M3 is maturity; M4 is stretch.

### M1.1 Two-panel figure composition -- `cpb_duo()` (P0, the biggest gap)

Nearly every CEP/study figure is two complete house-style panels placed
side by side on the full page width (occasionally three for maps).
Each panel is exactly what a wrapper already returns. What is missing
is the composer and the export path.

**API sketch**

```r
cpb_duo(left, right, widths = c(1, 1))   # returns a patchwork/composite
save_cpb("fig.png", cpb_duo(p1, p2), page = "full")  # duo == full width
```

- Implement on **patchwork** (add to Imports; it is the standard, keeps
  panels real ggplots). Alternatively hand-rolled gtable cbind if the
  dependency is unwanted -- but patchwork handles alignment of panel
  areas across different label widths, which is the hard part and
  exactly what the published figures get right.
- Each panel keeps its own title/subtitle/legend (that is the observed
  convention -- legends are per panel, bottom-left of each half).
- `save_cpb()` must accept the composite; a duo is by definition
  `page = "full"` (5.96 in) -- warn on `page = "half"`.
- Backgrounds: the published duo has one continuous background --
  set the composite background via
  `patchwork::plot_annotation(theme = ...)` or draw each panel with the
  house background and zero outer margin between them (inspect CEP fig
  1.1: the two panels are two separate blue tiles with a white gutter;
  reproduce that, it is simpler).
- Also accept 3 panels (`cpb_duo(p1, p2, p3)` or `cpb_row(...)`) for
  the map trio (zonnepanelen fig 2).

**Acceptance**: smoke figure replicating CEP 2026 figuur 1.1 (line duo
+ stacked-bars-with-line duo) and zonnepanelen figuur 3 (area | dodged
horizontal bars); legend pixel test extended with a duo variant (both
legends anchored); vignette section in `layout.Rmd`.

### M1.2 Classed choropleths + white map background (P0)

Real CPB maps are classed, not continuous: 5-6 bins, labels like
"lager dan 20%", "20% - 30%", ..., "60% en hoger", small swatch legend
at the **top-left inside** the map, on a **white** background.

**API sketch**

```r
cpb_map(gem, region = code, value = aandeel,
        breaks = c(0, 20, 30, 40, 50, 60, Inf),   # or breaks = 6 for pretty bins
        break_labels = NULL,       # auto: "lager dan 20%", "20% - 30%", ... "60% en hoger"
        palette = "blues",         # see M1.3; "sequential" (pink) also valid
        background = FALSE,        # maps default to white per publications
        legend = "topleft")        # inside-panel swatch legend
```

- `breaks` numeric vector or single integer; cut into ordered classes;
  Dutch label construction helper (`label_number_nl` for the bounds,
  "lager dan X" / "X - Y" / "Z en hoger" pattern -- make it a small
  exported helper `cpb_bin_labels()` so bar charts can reuse it).
- Classed fills use `scale_fill_cpb_manual`/ordered ramp slice; keep
  the continuous colourbar path for `breaks = NULL` (still useful).
- `legend = "topleft"` places the legend inside the panel
  (`legend.position = "inside"`, `legend.position.inside = c(0, 1)`,
  left-justified) with the 0.25/0.30 cm keys; keep `"bottom"` supported.
- `background = FALSE` already exists on `theme_cpb()`; default maps to
  white and drop the blue tile (observed practice differs from the
  chart tiles). Border seams then need a visible colour on white:
  default `border_colour` to white-on-blue as now but `"white"` when
  background is off -- inspect zonnepanelen fig 2: borders are white.

**Acceptance**: smoke figure replicating zonnepanelen figuur 2 (trio of
classed maps via `cpb_duo`/`cpb_row`); tests for binning, auto labels,
inside legend; `maps.Rmd` updated to lead with the classed variant.

### M1.3 Sequential blue palette + palette API polish (P0)

Erfbelasting uses a 6-step light→dark **blue** ramp for classed fills
(and the pink ramp for the sibling figure); reference p17_img19 uses a
~10-step blue ramp for deciles.

- Add `cpb_colors_blues` to `R/tokens.R`. Derivation: sample the actual
  hexes from erfbelasting fig 2.1 and reference p17_img19 (pixel-pick as
  done for the original calibration; the anchors are the house light
  blue `#87d2ff`, `#00a5ff`, primary `#005faf`, dark `#193c69`).
  Provide it as a ramp function so any n works
  (`grDevices::colorRampPalette` over the anchor stops).
- Register as palette `"blues"` in `cpb_pal()`/scales; make
  `scale_*_cpb_c(palette = c("sequential", "blues"))` and the discrete
  scales accept it.
- Update `cpb_tokens()`, palette docs, and the Setup vignette palette
  mention.

**Acceptance**: palette test (n-step monotone lightness ordering),
map + classed-bar examples using it; erfbelasting-style dodged bars
with 6 blue classes render in the smoke test.

### M1.4 Bron/Noot source line (P0, small)

Every published figure carries "Bron: CBS en CPB" (and often "Noot:
(a) ...") directly under it. bbplot's key adoption lesson: attribution
lives in the export step.

```r
save_cpb("fig.png", p, bron = "CBS en CPB", noot = "(a) ...")
# and/or wrapper-level: cpb_col(..., bron = "CBS")
```

- Implement as a styled `plot.caption` (6.5pt, grey `#666666`, plain,
  `plot.caption.position = "plot"`, left-aligned, small top margin) --
  stays a real ggplot, composes with `cpb_duo()` via
  `patchwork::plot_annotation(caption = ...)` for one shared line.
- Prefix handling: `bron = "CBS"` renders "Bron: CBS"; `noot` renders
  above the bron line.

**Acceptance**: theme test for caption styling; smoke figures updated
to carry bron lines; documented in `ggcpb.Rmd` (Setup, export section).

### M2.1 Top-position value axis (P1)

The flagship koopkracht figures (CEP 1.4/2.4) draw the value axis at
the **top** of the panel (labels + axis title above, categories down the
side). Add to `cpb_box()` (and `theme_cpb()`):

```r
cpb_box(..., orientation = "horizontal", value_axis = c("bottom", "top"))
```

- Mechanically: `scale_y_continuous(position = "right")` under
  `coord_flip()` puts the value scale on top; move the axis title
  styling accordingly (`axis.title.x.top`, hjust 1 italic); ticks on
  the top axis line.
- The grouped+modern+top-axis combination must work together (that IS
  figure 1.4).

**Acceptance**: smoke figure replicating CEP figuur 2.4 (single-panel
version: modern style + group + top axis + wml income-group labels
with parenthetical bounds); pixel legend test unaffected.

### M2.2 `cpb_dot()` -- dot-interval distribution plot (P1)

Erfbelasting fig 3.1; the heir of nicerplot's `dot.R`/`whisker.R`
(sources already fetched in the scratchpad during the earlier port).

```r
cpb_dot(data, x, p5, p25, p50, p75, p95, mean = NULL,
        orientation = "horizontal", ...)
```

- Construction per category: dashed grey connector across p5-p95;
  light-pink open dots at p5/p95; solid capped errorbar p25-p75;
  magenta filled dot at median; blue diamond (shape 18) at mean when
  supplied. Sizes ~1.6/2.2; house colours via index.
- Reuse the `group` heading machinery from `cpb_box` (shared helper
  already exists: `cpb_group_heading_positions`).
- Legend: constructed via dummy aesthetics or manual override so the
  five marker meanings appear as in the publication (p5/p95, 25-75,
  mediaan, gemiddelde).

**Acceptance**: smoke figure replicating erfbelasting figuur 3.1;
layer-composition tests; `boxplots.Rmd` gains a dot-plot section (or a
short own section in chart-types).

### M2.3 Dual-axis support (P1)

CEP uses two patterns: (a) two series on left/right axes with legend
suffixes "(linkeras)"/"(rechteras)" (fig 1.2 right); (b) split panel
with dashed separator and "zie linker y-as" annotations (kader).
Support (a); (b) stays a recipe.

```r
cpb_line(..., y2 = <column>, y2_scale = NULL, y2lab = NULL)
# draws y2 series against sec_axis; auto-suffixes legend labels
```

- Compute the linear map between ranges (`y2_scale` overrides);
  `scale_y_continuous(sec.axis = sec_axis(...))`; right-axis text
  styled like the left. Auto-append " (linkeras)"/" (rechteras)" to
  legend labels.
- Keep scope to `cpb_line` (that is where it is used).

**Acceptance**: recreation of CEP fig 1.2-right in the smoke test
(cao-loon + inflatie left, reëel loon right, index axis); tests for
the transform maths and legend suffixing; `annotation.Rmd` section.

### M2.4 Forecast window for horizontal charts + marker/label polish (P1, small)

- `cpb_forecast_rect`/`label` currently assume vertical time axes. For
  `orientation = "horizontal"` draw the window across the category rows
  ≥ `forecast_x` and rotate the label 90° at the right edge (CEP fig
  2.3-right). Wire through `cpb_col`.
- `value_labels` upgrades in `cpb_col`: `value_labels = "end"` places
  labels just beyond the bar end (the dominant published placement),
  `value_label_fmt = label_number_nl()` controls formatting; keep
  `TRUE` (centred) working.
- `cpb_line(points = TRUE)`: draw point markers on the lines
  (erfbelasting 3.3 / CEP overlays), size ~1.4, matching colours.

**Acceptance**: tests per feature; one smoke figure combining
horizontal bars + raming window; chart-types vignette notes.

### M3 Maturity block (P2 -- run as one hardening pass)

1. **pkgdown site**: `_pkgdown.yml` with the six vignettes as articles
   (Setup as Get started), reference grouped by layer (wrappers /
   theme+scales / formatters / export / data). Deploy via GitHub Actions
   to Pages. Add `URL:`/`BugReports:` to DESCRIPTION (peers all do).
2. **NEWS.md + versioning**: backfill 0.1.0 (style core) → 0.2.0
   (chart types) → 0.3.0 (layouts/maps/docs); bump per milestone here;
   state the policy in README (semver-ish, `lifecycle` badges for
   experimental args -- `group`, `cpb_map` breaks arg start as
   experimental).
3. **Input validation**: small `cpb_check_*()` helpers (data.frame-ness,
   required columns exist with tidy-eval-aware messages, length checks
   on `index` vs levels, `fill_colour` recycling warning). Use
   `cli`-formatted classed errors (`rlang::abort(class = "ggcpb_error")`);
   add `cli` to Imports (peers do). Robustness tests: empty data,
   one row, all-NA, unknown palette index.
4. **vdiffr snapshots**: one snapshot per smoke-test figure family
   (svglite; skip on CRAN/CI-without-fonts via
   `announce_snapshot_file`). This closes the "rest of the styling has
   no regression net" gap -- today only the legend is pinned.
5. **Accessibility**: document CVD simulation of the qualitative
   palette (colorspace::simulate_cvd or precomputed images in a
   `palettes` article); note the blues ramp as the CVD-safer choice for
   ordered data. sgplot/afcharts set the bar here.
6. **Smart default heights**: `save_cpb()` derives height when not
   given -- horizontal bars/boxes: base + per-category increment;
   grouped boxes: + per-heading; facets: per-row. Cap and message the
   chosen height. (Existing TODO item; the vignettes now hand-pick
   heights in five places -- good calibration data.)

### M4 Stretch / explicitly deprioritised

- **Fan charts: close the TODO item** -- absent from CEP 2026 and MEV
  2026; the raming window + variant lines (already supported) is the
  current practice. Keep a note in TODO for historical republication
  only.
- Kerngegevens **table theme** (gt or flextable, pink striped rows per
  Tabel 1.1): valuable but a different medium; propose as a separate
  small package or an opt-in module later.
- Icon infographics (CEP fig 1.3): designer territory, out of scope.
- Split-panel dual-axis (kader variant), angled-label helper, in-panel
  footnote helper (`cpb_footnote()`): document as recipes in
  `annotation.Rmd` first; promote to API only if usage demands.

## Suggested sequencing

| Milestone | Content | Exit criterion |
|---|---|---|
| **M1** (publication core) | duo composition, classed maps, blues palette, bron/noot | Smoke test contains faithful recreations of CEP fig 1.1 and zonnepanelen figs 2+3; all green |
| **M2** (flagship + forms) | top axis, cpb_dot, dual axis, horizontal raming, label/marker polish | Recreations of CEP fig 2.4 and erfbelasting fig 3.1 in the smoke test |
| **M3** (hardening) | pkgdown, NEWS/lifecycle, validation, vdiffr, CVD docs, smart heights | pkgdown live; R CMD check + snapshots green on CI; version 0.4.0 |
| **M4** | close/park stretch items | TODO reflects reality |

## Implementation notes for the executor

- Patterns to follow: every new wrapper argument goes through the
  anti-drift helpers (`cpb_wrapper_theme()` reads knobs by name;
  see `R/wrappers.R` head). New chart forms get: roxygen with house
  conventions, tests in `test-wrappers.R`, a smoke figure, a legend
  sweep variant when a legend exists, and a vignette section.
- Pixel-calibrate against the actual PDFs: the four publications are
  fetchable (see `tools/fetch_nl_geo.R` header for the proxy-safe
  pattern; publication PDFs at
  `cpb.nl/system/files/cpbmedia/CPB_Raming-centraal-economisch-plan-2026.pdf`
  etc.). Render pages with poppler and colour-pick as was done for the
  original theme calibration.
- The two-panel gutter, per-panel tile size and the top-axis metrics
  should be measured from CEP figs 1.1/1.4 before implementing --
  do not guess.
- data.table stays example-side only; jsonlite/poppler are tool-side
  only; the only proposed new runtime Imports are `patchwork` (M1.1)
  and `cli` (M3.3) -- both justified, both used by peers.
