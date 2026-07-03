# `references/` — house-style targets for ggcpb

Reference material for building **ggcpb**. None of this is part of the
package build; it exists to pin down the CPB house look that ggcpb must
reproduce. Most of the folder is gitignored (see the repo `.gitignore`);
only the files noted below are tracked.

## The goal

**ggcpb should mimic the output of the legacy `nplot()` function** (from the
internal `nicerplot` package). `nplot()` is the current CPB house-style
plotter, and its rendered figures are the target that ggcpb's theme,
palettes, scales and wrapper functions are trying to match.

When judging whether a ggcpb figure is "right", compare it against the
`nplot()` look — not against generic ggplot2 defaults.

## What's here

- **`plots/`** *(tracked)* — rendered figures from CPB publications, i.e.
  examples of the target output. Treat these as the visual ground truth.
- **`code/reference_plot_snippets.R`** *(tracked)* — anonymised raw-ggplot2
  snippets distilled from internal CPB analysis scripts. **These are not the
  target.** They only show how close plain ggplot2 (hand-rolled `theme()`
  calls plus the CPB palette) already got to the house style *before* ggcpb
  existed — a starting point and a sanity check, nothing more. All data is
  simulated and all labels are placeholders.
- **`nicerplot/`** *(gitignored, local only)* — a clone of the internal
  `nicerplot` package, the source of `nplot()`. Read it here to see exactly
  what output ggcpb is chasing.

## Notes for whoever iterates on this

- The snippets include chart types that **`nplot()` could not produce** —
  e.g. dodged (grouped) boxplots. Those are legitimate extensions, not
  deviations to "fix": ggcpb is allowed to go beyond nplot's repertoire.
- The **boxplot layout differs slightly** from nplot's, and that is fine —
  an exact boxplot match is not required.
