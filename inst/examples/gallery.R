# gallery.R ----
#
# A beginner-friendly tour of ggcpb: eight different chart types, each a
# single wrapper call on a small tidyverse dataset. Open it in RStudio
# and run it top to bottom -- every figure appears in the Plots pane
# (page through them with the arrows). Nothing is written to disk.

library(tidyverse)   # ggplot2, dplyr, tibble, tidyr, ...

# Load the CPB house style. If ggcpb is installed, library() is enough;
# inside the package project (not yet installed) load_all() picks it up.
if (requireNamespace("ggcpb", quietly = TRUE)) library(ggcpb) else devtools::load_all()

set.seed(7)

# Line chart ----
# A single time series. Growth crosses zero, so ggcpb draws the zero line.
groei <- tibble(
  jaar  = 2015:2024,
  bbp   = c(2.0, 2.2, 2.9, 2.4, 2.0, -3.9, 4.9, 4.3, 0.1, 0.6)
)

print(
  cpb_line(groei, x = jaar, y = bbp,
    title = "Economische groei",
    ylab  = "% volumemutatie bbp")
)

# Bar chart ----
# One bar per category. No fill mapping -> the house blue.
sectoren <- tibble(
  sector = c("industrie", "diensten", "landbouw", "bouw", "overheid"),
  waarde = c(18, 42, 6, 9, 25)
)

print(
  cpb_col(sectoren, x = sector, y = waarde,
    title = "Toegevoegde waarde per sector",
    ylab  = "mld euro")
)

# Stacked bars ----
# A fill column stacks the bars; `index` picks the palette colours.
bijdrage <- expand_grid(jaar = 2021:2024,
                        groep = c("bedrijven", "huishoudens", "overheid")) |>
  mutate(waarde = round(runif(n(), 5, 20), 1))

print(
  cpb_col(bijdrage, x = jaar, y = waarde, fill = groep, position = "stack",
    index = c(6, 5, 2),
    title = "Investeringen per groep",
    ylab  = "mld euro")
)

# Stacked area ----
# Shares that sum to 100 per year -> a percentage axis.
energie <- expand_grid(jaar = 2018:2024,
                       bron = c("gas", "elektriciteit", "warmte", "overig")) |>
  mutate(ruw = runif(n(), 1, 10)) |>
  group_by(jaar) |>
  mutate(aandeel = 100 * ruw / sum(ruw)) |>
  ungroup()

print(
  cpb_area(energie, x = jaar, y = aandeel, fill = bron, pct_axis = TRUE,
    index = c(6, 5, 2, 4),
    title = "Energiemix van huishoudens",
    ylab  = "aandeel")
)

# Scatter plot ----
# A numeric colour column gets the continuous CPB gradient.
huishoudens <- tibble(inkomen = round(rlnorm(120, log(2500), 0.3))) |>
  mutate(energierekening = round(90 + 0.03 * inkomen + rnorm(n(), 0, 25)),
         koopkracht      = round(rnorm(n(), 0, 2), 1))

print(
  cpb_scatter(huishoudens, x = inkomen, y = energierekening, colour = koopkracht,
    title = "Energierekening naar inkomen",
    ylab  = "energierekening (euro per maand)",
    xlab  = "besteedbaar inkomen (euro per maand)",
    colourlab = "koopkracht (%)")
)

# Histogram ----
wachttijd <- tibble(dagen = round(rgamma(500, 6, 0.4)))

print(
  cpb_hist(wachttijd, x = dagen, binwidth = 2,
    title = "Verdeling van de wachttijd",
    ylab  = "aantal",
    xlab  = "wachttijd (dagen)")
)

# Box plot ----
# The CPB quantile box: pass precomputed p5/p25/p50/p75/p95 columns.
spreiding <- tibble(
  groep = factor(c("laag", "midden", "hoog"), levels = c("hoog", "midden", "laag")),
  p5  = c(-6, -4, -2), p25 = c(-3, -2, -1), p50 = c(-1, 0, 1),
  p75 = c(1, 2, 3),    p95 = c(4, 5, 6)
)

print(
  cpb_box(spreiding, x = groep, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
    orientation = "horizontal",
    title    = "Spreiding van de koopkracht",
    subtitle = "inkomensgroep",
    ylab     = "% koopkrachtmutatie")
)

# Choropleth map ----
# One value per province; ggcpb joins on the province name.
provincies <- tibble(
  provincie = c("Groningen", "Fryslân", "Drenthe", "Overijssel", "Flevoland",
                "Gelderland", "Utrecht", "Noord-Holland", "Zuid-Holland",
                "Zeeland", "Noord-Brabant", "Limburg"),
  index = round(runif(12, 90, 110))
)

print(
  cpb_map(provincies, region = provincie, value = index, level = "provincie",
    title    = "Voorbeeldindex per provincie",
    subtitle = "index (Nederland = 100)")
)
