library(tidyverse)
library(ggcpb)

# simulate data ----
# per sector a few blocks: emissions (Mton) and effective CO2 price
# within a sector the blocks are sorted from high to low price (staircase)
blocks <- tribble(
  ~sector,         ~instrument,        ~emission, ~price,
  "Elektriciteit", "Energiebelasting",         2,    450,
  "Elektriciteit", "Energiebelasting",         4,    300,
  "Elektriciteit", "Energiebelasting",         6,    180,
  "Elektriciteit", "ETS",                     10,     70,
  "Industrie",     "Energiebelasting",         3,    380,
  "Industrie",     "Energiebelasting",         5,    220,
  "Industrie",     "ETS",                     15,     90,
  "Industrie",     "ETS",                     20,     70,
  "Industrie",     "Geen",                     5,      0,
  "Mobiliteit",    "Accijns",                 20,    340,
  "Mobiliteit",    "Accijns",                 15,    210,
  "Gebouwen",      "Energiebelasting",         4,    420,
  "Gebouwen",      "Energiebelasting",         8,    330,
  "Gebouwen",      "Energiebelasting",         6,    150,
  "Landbouw",      "Energiebelasting",         3,    250,
  "Landbouw",      "Energiebelasting",         4,    100,
  "Landbouw",      "Geen",                    15,      0
) |>
  mutate(sector = fct_inorder(sector)) |>
  arrange(sector, desc(price)) |>
  mutate(xmax = cumsum(emission),
         xmin = xmax - emission)

sector_pos <- blocks |>
  group_by(sector) |>
  summarise(x = (min(xmin) + max(xmax)) / 2, xmin = min(xmin))

# plot ----
p <- ggplot(filter(blocks, price > 0)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = price,
                fill = instrument)) +
  geom_vline(data = filter(sector_pos, xmin > 0), aes(xintercept = xmin),
             colour = "grey60", linewidth = 0.2) +
  geom_hline(yintercept = 180, linetype = "dashed") +
  geom_text(data = sector_pos, aes(x = x, y = 490, label = sector),
            size = 2, vjust = 1) +
  scale_fill_cpb_d() +
  scale_y_continuous(limits = c(0, 500), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, max(blocks$xmax)), expand = c(0, 0)) +
  labs(title = "CO2-beprijzing van broeikasgasemissie, 2024",
       subtitle = "Effectieve CO2-prijs (euro per ton CO2-eq.)",
       x = "Cumulatieve emissie (Mton CO2-eq.)",
       y = NULL,
       fill = NULL) +
  theme_cpb()

save_cpb("co2_beprijzing_simple.png", p, page = "full")
