# Bundled fonts

The house font, Rijksoverheid Sans Text, may not be published, so it is
not in this repository. `cpb_register_fonts()` (see `R/fonts.R`) takes
it from the first of these folders that has it, as nicerplot does:

| Folder | Platform |
|---|---|
| `M:/p_james/fonts` | Windows |
| `/nas/mdk/M/p_james/fonts`, `/cifs/p_james/fonts` | Linux |
| `~/Library/Fonts` | macOS |

`options(ggcpb.font_path = )` replaces this list.

Where none of them has it, the Source Sans 3 files here stand in:

```
SourceSans3-Regular.ttf
SourceSans3-Bold.ttf
SourceSans3-It.ttf
```

These are Adobe's original files (version 3.052, from
[adobe-fonts/source-sans](https://github.com/adobe-fonts/source-sans)),
licensed under the SIL Open Font License 1.1 (`OFL.txt`). Source Sans 3
sets text about 10-15% wider than the Rijksoverheid font on the M share
(italic the most), so a title or legend that only just fits there may
not fit here. Figures in it do not meet the Rijkshuisstijl, so
`save_cpb()` and printing a ggcpb chart warn about it.

No bold-italic face is shipped: `theme_cpb()` never combines bold and
italic, and `cpb_register_fonts()` reuses the bold face for that slot.
