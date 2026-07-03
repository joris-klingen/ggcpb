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

# productivity growth 2000-2024, two series (style of productivity-report
# p06_img01): mildly positive growth with crisis dips in 2009/2020 and a
# rebound in 2021.
prod_jaren <- 2000:2024
prod_reeksen <- c("arbeidsproductiviteit", "tfp")
sim_groei <- function() {
  g <- rnorm(length(prod_jaren), mean = 1.2, sd = 1.0)
  g[prod_jaren == 2009] <- g[prod_jaren == 2009] - 4
  g[prod_jaren == 2020] <- g[prod_jaren == 2020] - 3
  g[prod_jaren == 2021] <- g[prod_jaren == 2021] + 2.5
  g[prod_jaren == 2024] <- g[prod_jaren == 2024] - 2
  round(pmax(pmin(g, 5.8), -3.8), 1)
}
prod_dt <- rbindlist(lapply(prod_reeksen, function(r) {
  data.table(jaar = prod_jaren,
             reeks = factor(r, levels = prod_reeksen),
             groei = sim_groei())
}))

# growth decomposition (style of productivity-report p06_img02): three
# stacked contribution components per year, and their sum as the total
# labour-productivity line drawn on top.
# levels are reversed so that "kapitaal/uren" stacks nearest the zero
# line and "tfp" outermost, as in the reference; reverse_legend restores
# the canonical legend order and the index is reversed to match.
componenten <- c("kapitaal/uren", "arbeidssamenstelling", "tfp")
decomp_dt <- CJ(jaar = prod_jaren,
                component = factor(componenten, levels = rev(componenten)))
decomp_dt[, bijdrage := fcase(
  component == "kapitaal/uren",        round(rnorm(.N, 0.6, 0.35), 1),
  component == "arbeidssamenstelling", round(rnorm(.N, 0.1, 0.45), 1),
  component == "tfp",                  round(rnorm(.N, 0.6, 1.4), 1)
)]
decomp_dt[jaar == 2009 & component == "tfp", bijdrage := -3.7]
decomp_dt[jaar == 2020 & component == "tfp", bijdrage := -2.7]
decomp_dt[jaar == 2021 & component == "tfp", bijdrage := 3.0]
totaal_dt <- decomp_dt[, .(totaal = sum(bijdrage)), by = jaar]

# share with a commuting allowance by income group x household group
# (style of reference p13_img15): three dodged series per income group.
# levels are reversed twice, as in the pv_dt figure: the reversed factor
# levels make the first-named series land on top within each dodge group
# after coord_flip(), and the matching reversed palette `index` plus
# reverse_legend keep colours and legend in the canonical order.
reisk_groepen <- c("alle huishoudens", "werkenden", "werkenden met auto")
reisk_dt <- CJ(inkomensgroep = factor(inkomensgroepen, levels = rev(inkomensgroepen)),
               groep         = factor(reisk_groepen, levels = rev(reisk_groepen)))
reisk_basis <- c(3, 11, 23, 39, 55, 59)   # per income group, low -> high
reisk_dt[, share := reisk_basis[match(inkomensgroep, inkomensgroepen)] +
             fcase(groep == "alle huishoudens",   0,
                   groep == "werkenden",          c(14, 17, 17, 12, 7, 4)[match(inkomensgroep, inkomensgroepen)],
                   groep == "werkenden met auto", c(49, 48, 39, 25, 14, 8)[match(inkomensgroep, inkomensgroepen)])]

# energy share of the income effect by income group x year (style of
# reference p10_img07): precomputed quantiles on a 0-100% scale, dodged
# 2026/2027 boxes per income group. jaar levels reversed for the same
# coord_flip() dodge reason as above.
opbouw_dt <- CJ(inkomensgroep = factor(inkomensgroepen, levels = rev(inkomensgroepen)),
                jaar          = factor(c(2026L, 2027L), levels = c(2027L, 2026L)))
opbouw_mid <- c(47, 47, 30, 22, 18, 18)   # 2026 medians, low -> high income
opbouw_dt[, mid := opbouw_mid[match(inkomensgroep, inkomensgroepen)] +
              (jaar == "2027") * 29]
opbouw_dt[, `:=`(
  p50 = mid,
  p25 = pmax(mid - runif(.N, 10, 15), 0),
  p75 = pmin(mid + runif(.N, 15, 25), 100),
  p5  = pmax(mid - runif(.N, 25, 35), 0),
  p95 = 100
)]
opbouw_dt[, mid := NULL]

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
# palette 2), nplot() look via the first-class knobs: hairline black
# gridlines at labelled breaks only, black zero line, category-axis
# ticks, 7 pt axis text, and a flush-left bottom legend with 0.45 cm keys.
render(8, "col: horizontal, dodged (fill)",
  cpb_col(pv_dt, x = inkomensgroep, y = share, fill = jaar,
    position        = "dodge",
    orientation     = "horizontal",
    index           = c(2, 6),
    value_limits    = c(0, 70),
    width           = 0.85,
    legend          = "bottom",
    flush_legend    = TRUE,
    zeroline        = TRUE,
    minor           = FALSE,
    ticks           = TRUE,
    axis_text_size  = 7,
    legend_key_size = 0.45,
    grid_colour     = "black",
    grid_linewidth  = 0.1,
    title = "Zonnepanelen naar inkomen",
    ylab  = "inkomensgroepen",
    xlab  = "aandeel binnen inkomensgroep (%)") +
    ggplot2::scale_y_continuous(breaks = seq(0, 70, 10)) +
    ggplot2::theme(plot.margin = ggplot2::margin(8, 10, 6, 10)),
  "08_col_horizontal_dodged.png", page = "half")

# recreation of reference figure productivity-report p06_img01: two growth
# series (blue = palette 6, magenta = palette 2) in the nplot() look --
# no title, the unit ("%") as subtitle above the axis, hairline black
# gridlines at labelled breaks only, a black zero line under the data
# lines, year ticks on the x axis and a flush-left bottom legend.
render(9, "line: two series, nplot look",
  cpb_line(prod_dt, x = jaar, y = groei, colour = reeks,
    linewidth       = 0.55,
    index           = c(6, 2),
    legend          = "bottom",
    flush_legend    = TRUE,
    zeroline        = TRUE,
    minor           = FALSE,
    ticks           = TRUE,
    axis_text_size  = 7,
    grid_colour     = "black",
    grid_linewidth  = 0.1,
    subtitle = "%") +
    ggplot2::scale_y_continuous(breaks = seq(-4, 6, 2), limits = c(-4, 6)) +
    ggplot2::scale_x_continuous(
      breaks       = c(seq(2000, 2020, 5), 2024),
      minor_breaks = 2000:2024,
      guide        = ggplot2::guide_axis(minor.ticks = TRUE)
    ),
  "09_line_nplot.png", page = "half")

# recreation of reference figure productivity-report p06_img02: stacked
# growth-contribution columns spanning zero (blue / light blue / magenta)
# with the total drawn as a light-pink line (palette 1) on top.
render(10, "col: stacked +/- with line overlay",
  cpb_col(decomp_dt, x = jaar, y = bijdrage, fill = component,
    position        = "stack",
    index           = c(2, 5, 6),
    width           = 0.75,
    legend          = "bottom",
    flush_legend    = TRUE,
    zeroline        = TRUE,
    minor           = FALSE,
    ticks           = TRUE,
    axis_text_size  = 7,
    legend_key_size = 0.45,
    grid_colour     = "black",
    grid_linewidth  = 0.1,
    ylab = "%") +
    ggplot2::geom_line(
      data    = totaal_dt,
      mapping = ggplot2::aes(x = jaar, y = totaal, colour = "arbeidsproductiviteit"),
      linewidth = 0.55, inherit.aes = FALSE
    ) +
    scale_colour_cpb_manual(index = 1) +
    ggplot2::labs(colour = NULL) +
    ggplot2::guides(
      fill   = ggplot2::guide_legend(reverse = TRUE, order = 1),
      colour = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_y_continuous(breaks = seq(-4, 6, 2), limits = c(-4, 6)) +
    ggplot2::scale_x_continuous(
      breaks       = c(seq(2000, 2020, 5), 2024),
      minor_breaks = 2000:2024,
      guide        = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::theme(legend.box = "horizontal", legend.box.just = "top"),
  "10_col_stacked_line_overlay.png", page = "half")

# recreation of reference figure p13_img15: three series dodged
# horizontally (blue = palette 6, magenta = palette 2, grey-brown =
# palette 8), nplot() look, flush-left bottom legend.
render(11, "col: horizontal, dodged, 3 series",
  cpb_col(reisk_dt, x = inkomensgroep, y = share, fill = groep,
    position        = "dodge",
    orientation     = "horizontal",
    index           = c(8, 2, 6),
    value_limits    = c(0, 70),
    width           = 0.85,
    legend          = "bottom",
    flush_legend    = TRUE,
    zeroline        = TRUE,
    minor           = FALSE,
    ticks           = TRUE,
    axis_text_size  = 7,
    legend_key_size = 0.45,
    grid_colour     = "black",
    grid_linewidth  = 0.1,
    title = "Percentage met reiskostenvergoeding voor auto",
    ylab  = "inkomensgroepen",
    xlab  = "% met reiskostenvergoeding voor auto") +
    ggplot2::scale_y_continuous(breaks = seq(0, 70, 10)),
  "11_col_horizontal_dodged3.png", page = "half")

# recreation of reference figure p10_img07: horizontal dodged boxplot,
# 2026 blue above 2027 magenta per income group, percentage value axis.
# This reference keeps the hand-rolled ggplot grid (CPB grey, with
# minors), so only the box construction and colours matter here.
render(12, "box: horizontal, dodged by year",
  cpb_box(opbouw_dt, x = inkomensgroep,
    p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
    fill        = jaar,
    position    = ggplot2::position_dodge(width = 0.75),
    width       = 0.6,
    linewidth   = 0.25,
    orientation = "horizontal",
    index       = c(2, 6),
    legend      = "bottom",
    reverse_legend  = TRUE,
    flush_legend    = TRUE,
    axis_text_size  = 7,
    legend_key_size = 0.45,
    title = "Opbouw inkomenseffect: marktverwachtingen",
    subtitle = "inkomensgroepen",
    ylab  = "aandeel energie in inkomenseffect") +
    ggplot2::scale_y_continuous(labels = label_pct_nl(scale = 1),
                                breaks = seq(0, 100, 25)),
  "12_box_horizontal_dodged.png", page = "half")

# Summary ----

summary_dt <- rbindlist(results)
cat("\n")
print(summary_dt)
n_ok <- summary_dt[, sum(ok)]
tcat("ggcpb smoke test: ", n_ok, "/", nrow(summary_dt),
     " figures rendered OK -> ", out_dir)
if (n_ok < nrow(summary_dt)) quit(status = 1L)
