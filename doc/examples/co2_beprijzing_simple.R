library(tidyverse)
library(ggcpb)

# simulate data ----
# one block per sector/instrument: emissions (Mton) and effective CO2 price
blocks <- tribble(
  ~sector,         ~instrument,        ~emission, ~price,
  "Elektriciteit", "Energiebelasting",        15,    250,
  "Elektriciteit", "ETS",                      7,     70,
  "Industrie",     "Energiebelasting",        10,    300,
  "Industrie",     "ETS",                     33,     70,
  "Mobiliteit",    "Accijns",                 35,    300,
  "Gebouwen",      "Energiebelasting",        18,    330,
  "Landbouw",      "Energiebelasting",         6,    400,
  "Landbouw",      "Geen",                    16,      0,
  "Scheepvaart",   "ETS",                     10,     70,
  "Scheepvaart",   "Geen",                    25,      0,
  "Luchtvaart",    "Vliegbelasting",          10,    200,
  "Bio-energie",   "Accijns",                 13,    250
) |>
  mutate(xmax = cumsum(emission),
         xmin = xmax - emission)

sector_pos <- blocks |>
  group_by(sector) |>
  summarise(x = (min(xmin) + max(xmax)) / 2, xmin = min(xmin))

# plot ----
p <- ggplot(filter(blocks, price > 0)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = price,
                fill = instrument)) +
  geom_vline(data = slice(sector_pos, -1), aes(xintercept = xmin),
             colour = "grey60", linewidth = 0.2) +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_text(data = sector_pos, aes(x = x, y = 480, label = sector),
            size = 1.8, angle = 90, hjust = 1) +
  scale_fill_cpb_d() +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Cumulatieve emissie (Mton CO2-eq.)",
       y = "Effectieve CO2-prijs (euro per ton)",
       fill = NULL) +
  theme_cpb()

save_cpb("co2_beprijzing_simple.png", p, page = "full")
