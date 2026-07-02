# Bundled fonts

This directory holds the Rijksoverheid Sans Text TTF files that `ggcpb`
registers automatically on load (see `R/fonts.R` / `cpb_register_fonts()`):

```
RijksoverheidSansText-Regular_2_0.ttf
RijksoverheidSansText-Bold_2_0.ttf
RijksoverheidSansText-Italic_2_0.ttf
```

These are version 2.000 of the family (`family = "RijksoverheidSansText"`),
sourced from the Rijkshuisstijl web assets at
[Pleio/rijkshuisstijl](https://github.com/Pleio/rijkshuisstijl/tree/master/src/font)
(the `rijksoverheidsanstext-*-webfont.ttf` files, renamed to the versioned
names above).

The family has **no bold-italic face**, so no
`RijksoverheidSansText-BoldItalic_2_0.ttf` is shipped. `theme_cpb()` never
combines bold + italic (it uses bold for titles/strips and italic for the
subtitle, axis titles and legend), so this makes no visual difference;
`cpb_register_fonts()` reuses the bold face for the bold-italic slot to keep
any stray bold + italic request from erroring.

If the three required files are absent, `cpb_register_fonts()` fails
gracefully with a single warning and `theme_cpb()` falls back to the ggplot2
default font family (`cpb_font_family()` returns `""`) -- see the top-level
README for the path-based registration fallback against
`../Tech/rijks_font/`.

**Licensing.** The Rijksoverheid house-style fonts are the property of the
Dutch central government; use them only where you are permitted to. Confirm
redistribution is acceptable before publishing this package externally.
