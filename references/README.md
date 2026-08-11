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
  Each file is named `<publication>_p<page>_img<n>.png`, so the prefix
  says which publication it came from:

  | Filename prefix | Publication |
  | --- | --- |
  | `CPB_publicatie_erfbelasting_in_beeld_20260803` | [Erfbelasting in beeld: feiten, percepties en voorkeuren van Nederlanders](https://www.cpb.nl/publicatie/erfbelasting-beeld-feiten-percepties-en-voorkeuren-van-nederlanders) |
  | `CPB_publicatie_inkomenseffecten_hogere_energie_en_brandstofprijzen` | [Inkomenseffecten van hogere energie- en brandstofprijzen](https://www.cpb.nl/publicatie/inkomenseffecten-van-hogere-energie-en-brandstofprijzen) |

  Add a row when you drop a new publication's figures in here, so the
  ground truth can always be traced back to what was actually
  published. Look the URL up rather than deriving it from the
  filename: CPB slugs do not reliably match their titles, and the site
  uses more than one URL shape, so a slug guessed from a filename can
  easily point at the wrong publication.
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
