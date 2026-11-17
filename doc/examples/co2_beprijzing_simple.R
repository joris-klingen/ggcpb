library(tidyverse)
library(ggcpb)

# simulate data ----
# one row per block of emissions (Mton) with an ETS price (bottom layer)
# and a tax price (top layer); letter marks the tax in the key below the plot
blocks <- tribble(
  ~sector,              ~emission, ~ets,           ~ets_price, ~tax,                                  ~tax_price, ~letter,
  "Elektriciteit",              2, "ETS1 geveild",         70, "Energiebelasting (A en B)",                 520, "A",
  "Elektriciteit",              4, "ETS1 geveild",         70, "Energiebelasting (A en B)",                 250, "A",
  "Elektriciteit",              6, "ETS1 geveild",         70, "Energiebelasting (A en B)",                 100, "A",
  "Elektriciteit",             10, "ETS1 geveild",         70, NA,                                            0, NA,
  "Industrie",                  3, "ETS1 geveild",         70, "Energiebelasting (A en B)",                 450, "A",
  "Industrie",                  3, "ETS1 geveild",         70, "Energiebelasting (A en B)",                 150, "A",
  "Industrie",                 20, "ETS1 gratis",          70, "Overige belastingen op energie (E)",         30, "E",
  "Industrie",                 10, "ETS1 gratis",          70, NA,                                            0, NA,
  "Industrie",                  4, NA,                      0, "Energiebelasting (A en B)",                 300, "B",
  "Mobiliteit",                20, NA,                      0, "Accijns op minerale oliën (C en D)",        350, "C",
  "Mobiliteit",                15, NA,                      0, "Accijns op minerale oliën (C en D)",        210, "D",
  "Gebouwde\nomgeving",         8, NA,                      0, "Energiebelasting (A en B)",                 330, "B",
  "Gebouwde\nomgeving",         6, NA,                      0, "Energiebelasting (A en B)",                 150, "B",
  "Gebouwde\nomgeving",         2, NA,                      0, "Energiebelasting (A en B)",                 480, "A",
  "Luchtvaart",                 6, "ETS1 geveild",         50, "Vliegbelasting",                            200, NA,
  "Luchtvaart",                 4, NA,                      0, NA,                                            0, NA,
  "Bio-energie",                4, NA,                      0, "Belastingen op biomassa",                   550, "C",
  "Bio-energie",                5, NA,                      0, "Belastingen op biomassa",                   200, "D",
  "Bio-energie",                4, NA,                      0, "Belastingen op biomassa",                    20, "E"
) |>
  mutate(sector = fct_inorder(sector),
         total = ets_price + tax_price) |>
  arrange(sector, desc(total)) |>
  mutate(xmax = cumsum(emission),
         xmin = xmax - emission)

instruments <- c("Energiebelasting (A en B)", "Accijns op minerale oliën (C en D)",
                 "ETS1 gratis", "Overige belastingen op energie (E)", "ETS1 geveild",
                 "Vliegbelasting", "Belastingen op biomassa")

# rectangles: ETS layer at the bottom, tax layer on top
rects <- bind_rows(
  blocks |> transmute(xmin, xmax, ymin = 0, ymax = ets_price, instrument = ets),
  blocks |> transmute(xmin, xmax, ymin = ets_price, ymax = total, instrument = tax)
) |>
  filter(!is.na(instrument)) |>
  mutate(instrument = factor(instrument, levels = instruments))

# letter above the highest block of each sector/letter combination
letters_pos <- blocks |>
  filter(!is.na(letter)) |>
  group_by(sector, letter) |>
  slice_max(total, n = 1, with_ties = FALSE) |>
  ungroup()

sector_pos <- blocks |>
  group_by(sector) |>
  summarise(xmin = min(xmin), xmax = max(xmax)) |>
  mutate(x = (xmin + xmax) / 2)

# key for the letters, shown as a legend with the letter as point shape
letter_key <- c(A = "Energiebelasting op elektriciteit",
                B = "Energiebelasting op aardgas",
                C = "Accijns op benzine",
                D = "Accijns op diesel",
                E = "Afvalstoffenbelasting")
letter_df <- tibble(key = factor(letter_key, levels = letter_key), x = 0, y = 0)

# national total: everything except aviation bunkers and bio-energy
national_end <- sector_pos |>
  filter(sector == "Gebouwde\nomgeving") |>
  pull(xmax)

# plot ----
p <- ggplot(rects) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = instrument)) +
  geom_segment(data = filter(sector_pos, xmin > 0),
               aes(x = xmin, xend = xmin, y = 0, yend = 960),
               colour = "grey60", linewidth = 0.2) +
  geom_hline(aes(yintercept = 180, linetype = "Externe kosten klimaat")) +
  geom_text(data = letters_pos, aes(x = xmin, y = total + 15, label = letter),
            size = 2, hjust = 0, vjust = 0) +
  geom_point(data = letter_df, aes(x = x, y = y, shape = key), alpha = 0) +
  geom_text(data = sector_pos, aes(x = x, y = 950, label = sector),
            size = 2, vjust = 1, lineheight = 0.9) +
  annotate("segment", x = 0.5, xend = national_end - 0.5, y = 1000, yend = 1000,
           linewidth = 0.3) +
  annotate("label", x = national_end / 2, y = 1000, size = 2,
           label = "Nationaal emissietotaal", fill = cpb_tokens()$bg,
           linewidth = 0, label.padding = unit(0.1, "lines")) +
  scale_fill_cpb_d(drop = FALSE) +
  scale_linetype_manual(values = "dashed") +
  scale_shape_manual(values = names(letter_key)) +
  scale_x_continuous(limits = c(0, max(blocks$xmax)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1040), breaks = seq(0, 800, 200),
                     expand = c(0, 0)) +
  labs(title = "CO2-beprijzing van broeikasgasemissie, 2024",
       subtitle = "Effectieve CO2-prijs (euro per ton CO2-eq.)",
       x = "Cumulatieve emissie (Mton CO2-eq.)",
       y = NULL, fill = NULL, linetype = NULL, shape = NULL,
       caption = "Bron: gesimuleerde data") +
  guides(fill = guide_legend(ncol = 2, order = 1),
         linetype = guide_legend(order = 2),
         shape = guide_legend(order = 3, override.aes = list(alpha = 1, size = 2.5))) +
  theme_cpb()

save_cpb("co2_beprijzing_simple.png", p, page = "full")
