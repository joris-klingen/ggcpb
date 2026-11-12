# ggcpb

`ggcpb` is the new version of James. It draws figures in the 
CPB house style from Excel or from R. 

**New to ggcpb? Download the introduction file and open it in your browser:
[Introduction](doc/introduction.html).**
It shows how to create figures with Excel and/or R.

> **Status: in development.** 

## Documentation

The documentation is three HTML files in the [doc/](doc/) folder.
Download the file and open it in your browser. On GitHub, open the file
and choose "Download raw file".

- **[Introduction](doc/introduction.html)** --
  `vignette("introduction")`: creating figures with Excel and/or R.
- **[Import](doc/import.html)** -- `vignette("import")`: building
  figures from CSV files, with Excel and without R.
- **[Extended information](doc/extended.html)** --
  `vignette("extended")`: extended information about colours, columns, 
  boxplot styles and maps, and extra functionalities in R.

The HTML files are made from `vignettes/` with
`Rscript tools/render_vignettes.R`.

## Install

Open RStudio and type in the Console:

```r
install.packages("remotes")
remotes::install_github("joris-klingen/ggcpb")
```

Then load it with `library(ggcpb)`. The code is on
<https://github.com/joris-klingen/ggcpb>.

## How to use ggcpb in Excel?

This is the general workflow to create figures with ggcpb without using R:

1. Find the most recent ggcpb version (and install it following the steps above).
2. Copy it to the place where you want to create figures.
3. Put your datasets in the folder `data`. 
   Open `params.csv` in Excel and describe your figures.
4. Start `generate_plot.bat` by double clicking it. This runs the `generate_plot.R` script.
   Your figures appear in the folder `generated`. Each image is named after
   its dataset. Running the script again produces updated figures. 
   Images with the same name are replaced. A dataset or figure that 
   fails is skipped and listed at the end, and in `params_log.txt`. 
   The other figures are still saved.

For more information, take a look at the **[introduction](doc/introduction.html)** and **[import](doc/import.html)** vignettes. 

## Quick start in R

```r
library(ggcpb)

df <- data.frame(jaar = 2021:2024, groep = rep(c("a", "b"), 4),
                 waarde = c(10, 15, 12, 18, 14, 20, 16, 22))

cpb_col(df, x = jaar, y = waarde, fill = groep,
  title = "Waarde per groep",
  ylab  = "mld euro")
```
The CPB plotting functions accept a data.frame or data.table
plus tidy-eval column arguments:

```r
# Line chart
cpb_line(data = df, x = year, y = value)
# Bar chart
cpb_col(data = df, x = year, y = value, fill = group)
# Box plot
cpb_box(data = df, x = group, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95)
# Dot plot
cpb_dot(data = df, x = term, y = estimate, lower = lower, upper = upper)
# Donut chart
cpb_donut(data = df, fill = group, y = share)
# Scatter plot
cpb_scatter(data = df, x = income, y = spending)
# Histogram
cpb_hist(data = df, x = duration)
# Map
cpb_map(data = df, region = code, value = share)
```

## Fonts

The house font, Rijksoverheid Sans Text is not in this repository,
because it may not be published. If the Rijksoverheid font can
not be found locally, the bundled Source Sans 3 (SIL OFL 1.1, see `inst/fonts/`) stands in.
`save_cpb()` and printing a chart then warn that the figure does not meet the Rijkshuisstijl.
`options(ggcpb.font_warning = FALSE)` turns that off. If no font
registers at all, the theme falls back to the ggplot2 default with a
single warning. `cpb_register_fonts()` re-runs registration,
`cpb_font_family()` reports the family in use.

---

All data in this document is made up. It is generated to show the figures and is not real data.
