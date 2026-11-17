library(tidyverse)
library(ggcpb)

set.seed(2024)

# sectors ----
sectors <- tibble(
  sector = c("Elektriciteit", "Industrie", "Mobiliteit", "Gebouwde\nomgeving",
             "Land-\nbouw", "Bunkers\nscheepvaart", "Bunkers\nluchtvaart",
             "Niet-\nenergetisch", "Bio-\nenergie"),
  xmin = c(0, 22, 65, 100, 118, 140, 175, 185, 215),
  xmax = c(22, 65, 100, 118, 140, 175, 185, 215, 228)
) |>
  mutate(sector = fct_inorder(sector),
         x = (xmin + xmax) / 2)

# simulate slices ----
# each group: total emission w (Mton) split into n slices, an ETS layer
# (bottom) and a tax layer (top) whose price falls from tax_max (staircase)
groups <- tribble(
  ~sector,                 ~w,  ~n, ~ets,           ~ets_price, ~tax,                                 ~tax_max, ~letter,
  "Elektriciteit",         15,  10, "ETS1 geveild",         70, "Energiebelasting (A en B)",               650, "A",
  "Elektriciteit",          7,   1, "ETS1 geveild",         70, NA,                                          0, NA,
  "Industrie",              8,   8, "ETS1 geveild",         70, "Energiebelasting (A en B)",               600, "A",
  "Industrie",             30,   4, "ETS1 gratis",          70, "Overige belastingen op energie (E)",       40, "E",
  "Industrie",              5,   3, NA,                      0, "Energiebelasting (A en B)",               330, "B",
  "Mobiliteit",            20,   1, NA,                      0, "Accijns op minerale oliën (C en D)",      350, "C",
  "Mobiliteit",            15,   1, NA,                      0, "Accijns op minerale oliën (C en D)",      210, "D",
  "Gebouwde\nomgeving",    18,   6, NA,                      0, "Energiebelasting (A en B)",               340, "B",
  "Land-\nbouw",            4,   6, NA,                      0, "Energiebelasting (A en B)",               520, "A",
  "Land-\nbouw",           18,   1, NA,                      0, NA,                                          0, NA,
  "Bunkers\nscheepvaart",  10,   1, "ETS1 geveild",         70, NA,                                          0, NA,
  "Bunkers\nscheepvaart",  25,   1, NA,                      0, NA,                                          0, NA,
  "Bunkers\nluchtvaart",    6,   3, "ETS1 geveild",         50, "Vliegbelasting",                          200, NA,
  "Bunkers\nluchtvaart",    4,   1, NA,                      0, NA,                                          0, NA,
  "Niet-\nenergetisch",    30,   1, NA,                      0, NA,                                          0, NA,
  "Bio-\nenergie",          5,   4, NA,                      0, "Belastingen op biomassa",                 560, "C",
  "Bio-\nenergie",          5,   2, NA,                      0, "Belastingen op biomassa",                 200, "D",
  "Bio-\nenergie",          3,   2, NA,                      0, "Belastingen op biomassa",                  30, "E"
)

sim_slices <- function(w, n, tax_max) {
  tibble(
    width = w * prop.table(rexp(n)),
    tax_price = if (tax_max > 0) {
      sort(c(tax_max, tax_max * rbeta(n - 1, 1, 3)), decreasing = TRUE)
    } else {
      rep(0, n)
    }
  )
}

slices <- groups |>
  mutate(slice = pmap(list(w, n, tax_max), sim_slices)) |>
  unnest(slice) |>
  mutate(sector = factor(sector, levels = levels(sectors$sector)),
         total = ets_price + tax_price) |>
  left_join(select(sectors, sector, sector_start = xmin), by = "sector") |>
  arrange(sector, desc(total)) |>
  group_by(sector) |>
  mutate(xmax = sector_start + cumsum(width),
         xmin = xmax - width) |>
  ungroup()

instruments <- c("Energiebelasting (A en B)", "Accijns op minerale oliën (C en D)",
                 "ETS1 gratis", "Overige belastingen op energie (E)", "ETS1 geveild",
                 "Vliegbelasting", "Belastingen op biomassa")

# rectangles: ETS layer at the bottom, tax layer on top ----
rects <- bind_rows(
  slices |> transmute(xmin, xmax, ymin = 0, ymax = ets_price, instrument = ets),
  slices |> transmute(xmin, xmax, ymin = ets_price, ymax = total, instrument = tax)
) |>
  filter(!is.na(instrument), ymax > ymin) |>
  mutate(instrument = factor(instrument, levels = instruments))

# letters above the highest slice of each sector/letter combination
letters_pos <- slices |>
  filter(!is.na(letter)) |>
  group_by(sector, letter) |>
  slice_max(total, n = 1, with_ties = FALSE) |>
  ungroup()

# key for the letters, shown as a legend with the letter as point shape
letter_key <- c(A = "Energiebelasting op elektriciteit",
                B = "Energiebelasting op aardgas",
                C = "Accijns op benzine",
                D = "Accijns op diesel",
                E = "Afvalstoffenbelasting")
letter_df <- tibble(key = factor(letter_key, levels = letter_key), x = 0, y = 0)

# national total: all sectors up to and including agriculture
national_end <- sectors |>
  filter(sector == "Land-\nbouw") |>
  pull(xmax)

# plot ----
p <- ggplot(rects) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = instrument)) +
  geom_segment(data = filter(sectors, xmin > 0),
               aes(x = xmin, xend = xmin, y = 0, yend = 960),
               colour = "grey60", linewidth = 0.2) +
  geom_hline(aes(yintercept = 180, linetype = "Externe kosten klimaat")) +
  geom_point(data = letter_df, aes(x = x, y = y, shape = key), alpha = 0) +
  geom_text(data = letters_pos, aes(x = xmin, y = total + 15, label = letter),
            size = 2, hjust = 0, vjust = 0) +
  geom_text(data = sectors, aes(x = x, y = 950, label = sector),
            size = 1.8, vjust = 1, lineheight = 0.9) +
  annotate("segment", x = 0.5, xend = national_end - 0.5, y = 1000, yend = 1000,
           linewidth = 0.3) +
  annotate("label", x = national_end / 2, y = 1000, size = 2,
           label = "Nationaal emissietotaal", fill = cpb_tokens()$bg,
           linewidth = 0, label.padding = unit(0.1, "lines")) +
  scale_fill_cpb_d(drop = FALSE) +
  scale_linetype_manual(values = "dashed") +
  scale_shape_manual(values = names(letter_key)) +
  scale_x_continuous(limits = c(0, max(sectors$xmax)), breaks = seq(0, 225, 25),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1040), breaks = seq(0, 800, 200),
                     expand = c(0, 0)) +
  labs(title = "CO2-beprijzing van broeikasgasemissie, 2024",
       subtitle = "Effectieve CO2-prijs (euro per ton CO2-eq.)",
       x = "Cumulatieve broeikasgasemissie (Mton CO2-eq.)",
       y = NULL, fill = NULL, linetype = NULL, shape = NULL,
       caption = "Bron: gesimuleerde data") +
  guides(fill = guide_legend(ncol = 2, order = 1),
         linetype = guide_legend(order = 2),
         shape = guide_legend(order = 3,
                              override.aes = list(alpha = 1, size = 2.5))) +
  theme_cpb()

save_cpb("co2_beprijzing_tidy.png", p, page = "full")
