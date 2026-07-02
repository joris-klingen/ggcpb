# Bundled fonts

This directory should contain the four Rijksoverheid Sans Text TTF
files that `ggcpb` registers automatically on load (see
`R/fonts.R` / `cpb_register_fonts()`):

```
RijksoverheidSansText-Regular_2_0.ttf
RijksoverheidSansText-Bold_2_0.ttf
RijksoverheidSansText-Italic_2_0.ttf
RijksoverheidSansText-BoldItalic_2_0.ttf
```

They are not committed in this checkout. Before relying on
`theme_cpb()`'s bundled font, confirm with CPB that these TTFs may be
redistributed inside this package's repository, then copy them in
from:

```
../Tech/rijks_font/
```

Until they are present, `cpb_register_fonts()` fails gracefully with a
single warning and `theme_cpb()` falls back to the ggplot2 default
font family (`cpb_font_family()` returns `""`) -- see the top-level
README for the path-based registration fallback.
