# smoke_test_plots.R ----
#
# Visual smoke test for the ggcpb wrapper layer. Builds one figure of
# every supported chart type on simulated data and writes them to
# inst/examples/output/. Run it from anywhere with:
#
#   Rscript inst/examples/smoke_test_plots.R
#
# It loads the package with devtools::load_all() (no install needed) and
# reports OK / FAIL per figure, so it doubles as a quick end-to-end
# render check across theme_cpb(), the CPB colour scales, save_cpb() and
# the font-fallback path.

# Setup ----

suppressPackageStartupMessages({
  library(data.table)
})

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(normalizePath(sub("^--file=", "", file_arg[[1]])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(sys.frames()[[1]]$ofile))
  # interactive fallback: assume the working directory is the package root
  normalizePath(file.path(getwd(), "inst", "examples", "smoke_test_plots.R"),
                mustWork = FALSE)
}

script_dir <- dirname(get_script_path())
pkg_root   <- normalizePath(file.path(script_dir, "..", ".."))
out_dir    <- file.path(script_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

devtools::load_all(pkg_root, quiet = TRUE)

set.seed(42)

results <- list()
render <- function(id, title, plot, file, page = "half", height = NULL) {
  path <- file.path(out_dir, file)
  ok <- tryCatch({
    save_cpb(path, plot, page = page, height = height)
    TRUE
  }, error = function(e) {
    message("  FAIL [", id, "] ", title, ": ", conditionMessage(e))
    FALSE
  })
  results[[as.character(id)]] <<- data.table(id = id, title = title, file = file, ok = ok)
  invisible(ok)
}

# Simulated data ----

# single time series for the line chart
line_dt <- data.table(
  jaar  = 2015:2027,
  index = 100 + cumsum(rnorm(13, mean = 1.5, sd = 1.2))
)

# year x sector panel for the stacked columns
sectoren <- c("industrie", "diensten", "landbouw", "overheid")
stack_dt <- CJ(jaar = 2023:2027, sector = factor(sectoren, levels = sectoren))
stack_dt[, waarde := round(runif(.N, 5, 25), 1)]

# region x scenario panel for the dodged columns
scenarios <- c("basispad", "beleidsvariant")
dodge_dt <- CJ(regio    = factor(c("Noord", "Oost", "Zuid", "West")),
               scenario = factor(scenarios, levels = scenarios))
dodge_dt[, effect := round(runif(.N, -2, 6), 1)]

# energy-mix shares over time for the stacked area (shares sum to 100 per year)
bronnen <- c("gas", "elektriciteit", "warmte", "overig")
area_dt <- CJ(jaar = 2018:2027, bron = factor(bronnen, levels = bronnen))
area_dt[, ruw := runif(.N, 1, 10)]
area_dt[, aandeel := 100 * ruw / sum(ruw), by = jaar]
area_dt[, ruw := NULL]

# five income groups: raw draws -> precomputed quantiles for the boxplot
groepen5 <- c("laagste 20%", "2e 20%", "midden 20%", "4e 20%", "hoogste 20%")
raw5 <- data.table(
  groep      = factor(rep(groepen5, each = 400L), levels = groepen5),
  koopkracht = rnorm(5L * 400L, mean = rep(c(-3, -1.5, 0, 1.5, 3.5), each = 400L), sd = 2)
)
box5_dt <- raw5[, .(
  p5  = quantile(koopkracht, 0.05),
  p25 = quantile(koopkracht, 0.25),
  p50 = quantile(koopkracht, 0.50),
  p75 = quantile(koopkracht, 0.75),
  p95 = quantile(koopkracht, 0.95)
), by = groep]

# three groups x two years: raw draws -> quantiles, coloured by year
groepen6 <- c("laag inkomen", "midden inkomen", "hoog inkomen")
raw6 <- rbindlist(lapply(c(2026L, 2027L), function(y) {
  data.table(
    groep  = factor(rep(groepen6, each = 300L), levels = groepen6),
    jaar   = factor(y, levels = c(2026L, 2027L)),
    waarde = rnorm(3L * 300L,
                   mean = rep(c(-2, 0, 2), each = 300L) + (y - 2026L) * 0.8,
                   sd   = 1.5)
  )
}))
box6_dt <- raw6[, .(
  p5  = quantile(waarde, 0.05),
  p25 = quantile(waarde, 0.25),
  p50 = quantile(waarde, 0.50),
  p75 = quantile(waarde, 0.75),
  p95 = quantile(waarde, 0.95)
), by = .(groep, jaar)]

# shared income-group ordering (top -> bottom as printed in the reference)
inkomensgroepen <- c("tot 120% wml", "120% wml - mod.", "1 - 1,5x mod.",
                     "1,5 - 2x mod.", "2 - 3x mod.", "boven 3x mod.")

# household-level car ownership by income group -> share without a car.
# (style example based on references/code scripts/304_plot_autobezit.R.)
# simulate a population (one row per household), then aggregate.
nocar_n <- c(1.4e6, 1.0e6, 1.4e6, 1.1e6, 1.5e6, 1.5e6)
nocar_p <- c(0.62, 0.35, 0.26, 0.19, 0.14, 0.13)

nocar_pop <- rbindlist(Map(function(g, n, p) {
  data.table(inkomensgroep = g, cars_total = rbinom(n, size = 1L, prob = 1 - p))
}, inkomensgroepen, nocar_n, nocar_p))
nocar_pop[, geen_auto := cars_total == 0L]

nocar_dt <- nocar_pop[, .(n_pop = .N, share = 100 * mean(geen_auto)),
                      by = inkomensgroep]
rm(nocar_pop)

# order so "tot 120% wml" lands on top after coord_flip (reverse of canonical)
nocar_dt[, inkomensgroep := factor(inkomensgroep,
                                   levels = rev(inkomensgroepen), ordered = TRUE)]
setorder(nocar_dt, inkomensgroep)

nocar_dt[, n_pop_mln := round(n_pop / 1e6, 1)]
nocar_dt[, inkomensgroep_label := paste0(
  as.character(inkomensgroep), "\n(",
  format(n_pop_mln, decimal.mark = ",", nsmall = 1), " mln hh)")]
nocar_dt[, inkomensgroep_label := factor(inkomensgroep_label,
                                         levels = inkomensgroep_label, ordered = TRUE)]

# share of households with solar panels by income group x year (2021 vs 2024):
# a horizontal *dodged* bar with a two-colour fill (blue 2021, magenta 2024).
pv_targets <- data.table(
  inkomensgroep = rep(inkomensgroepen, each = 2L),
  jaar          = rep(c(2021L, 2024L), times = 6L),
  p             = c(10, 19, 14, 25, 19, 31, 22, 38, 26, 47, 33, 59) / 100
)
pv_pop <- rbindlist(Map(function(g, y, p) {
  data.table(inkomensgroep = g, jaar = y, heeft_pv = rbinom(40000L, 1L, p))
}, pv_targets[, inkomensgroep], pv_targets[, jaar], pv_targets[, p]))

pv_dt <- pv_pop[, .(share = 100 * mean(heeft_pv)), by = .(inkomensgroep, jaar)]
rm(pv_pop)
pv_dt[, inkomensgroep := factor(inkomensgroep, levels = rev(inkomensgroepen), ordered = TRUE)]
# jaar levels are reversed (2024 before 2021) so that under coord_flip() the
# dodge draws 2021 on top of 2024 within each group; the swapped index below
# keeps 2021 blue / 2024 magenta, and reverse_legend restores 2021-above-2024.
pv_dt[, jaar := factor(jaar, levels = c(2024L, 2021L))]

# Figures ----

render(1, "line: single series",
  cpb_line(line_dt, x = jaar, y = index,
    title = "1) Simpele lijn — bbp-index",
    ylab  = "index (2015 = 100)"),
  "01_line.png")

render(2, "col: stacked",
  cpb_col(stack_dt, x = jaar, y = waarde, fill = sector, position = "stack",
    title   = "2) Gestapelde kolommen — bijdrage per sector",
    ylab    = "mld euro",
    filllab = "sector"),
  "02_col_stacked.png")

render(3, "col: dodged",
  cpb_col(dodge_dt, x = regio, y = effect, fill = scenario, position = "dodge",
    title   = "3) Gegroepeerde kolommen — effect per scenario",
    ylab    = "% mutatie",
    filllab = "scenario"),
  "03_col_dodged.png")

render(4, "area: stacked shares",
  cpb_area(area_dt, x = jaar, y = aandeel, fill = bron, pct_axis = TRUE,
    title   = "4) Vlakdiagram — energiemix (aandeel)",
    filllab = "bron"),
  "04_area.png")

render(5, "box: horizontal, 5 categories",
  cpb_box(box5_dt, x = groep,
    p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
    orientation = "horizontal",
    title = "5) Boxplot (horizontaal) — koopkracht per groep",
    ylab  = "% koopkrachtmutatie"),
  "05_box_horizontal.png", page = "full", height = 3.2)

render(6, "box: split by year (2026/2027)",
  cpb_box(box6_dt, x = groep,
    p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
    fill     = jaar,
    position = ggplot2::position_dodge(width = 0.6),
    title    = "6) Boxplot per jaar — 2026 vs 2027",
    ylab     = "% koopkrachtmutatie",
    filllab  = "jaar"),
  "06_box_by_year.png", page = "full")

# recreation of reference figure p11_img11: horizontal single-colour bar
# (CPB blue, the default fill) with a percentage value axis. ylab is the
# vertical (category) axis -> rendered as the subtitle; xlab is the
# horizontal (value) axis.
render(7, "col: horizontal, single colour",
  cpb_col(nocar_dt, x = inkomensgroep_label, y = share,
    orientation  = "horizontal",
    pct_axis     = TRUE,
    value_limits = c(0, 70),
    width        = 0.6,
    title = "Aandeel huishoudens zonder auto naar inkomen \n",
    ylab  = "inkomensgroep",
    xlab  = "huishoudens zonder auto") +
    ggplot2::theme(plot.margin = ggplot2::margin(10, 10, 30, 10)),
  "07_col_horizontal.png", page = "half")

# recreation of reference figure p20_img24: horizontal dodged bar, two
# years side by side per group (2021 blue = palette 6, 2024 magenta =
# palette 2), legend at the bottom.
render(8, "col: horizontal, dodged (fill)",
  cpb_col(pv_dt, x = inkomensgroep, y = share, fill = jaar,
    position     = "dodge",
    orientation  = "horizontal",
    index        = c(2, 6),
    value_limits = c(0, 70),
    width        = 0.85,
    legend       = "bottom",
    title = "Zonnepanelen naar inkomen",
    ylab  = "inkomensgroepen",
    xlab  = "aandeel binnen inkomensgroep (%)") +
    ggplot2::scale_y_continuous(breaks = seq(0, 70, 10)) +
    ggplot2::theme(
      panel.grid.minor.x   = ggplot2::element_blank(),
      axis.text.y          = ggplot2::element_text(size = 7),
      legend.direction     = "vertical",
      legend.location      = "plot",
      legend.justification = "left",
      legend.key.size      = grid::unit(0.45, "cm"),
      plot.margin          = ggplot2::margin(8, 10, 6, 10)
    ),
  "08_col_horizontal_dodged.png", page = "half")

# Summary ----

summary_dt <- rbindlist(results)
cat("\n")
print(summary_dt)
n_ok <- summary_dt[, sum(ok)]
tcat("ggcpb smoke test: ", n_ok, "/", nrow(summary_dt),
     " figures rendered OK -> ", out_dir)
if (n_ok < nrow(summary_dt)) quit(status = 1L)
