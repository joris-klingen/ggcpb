library(data.table)
library(ggplot2)
library(ggcpb)

set.seed(2024)

# sectors ----
sectors <- data.table(
  sector = c("Elektriciteit", "Industrie", "Mobiliteit", "Gebouwde\nomgeving",
             "Land-\nbouw", "Bunkers\nscheepvaart", "Bunkers\nluchtvaart",
             "Niet-\nenergetisch", "Bio-\nenergie"),
  xmin = c(0, 22, 65, 100, 118, 140, 175, 185, 215),
  xmax = c(22, 65, 100, 118, 140, 175, 185, 215, 228)
)

# simulate slices ----
# each slice: width in Mton, an ETS layer (bottom) and a tax layer (top)
sim_sector <- function(s, w, n, ets, ets_price, tax, tax_max, tax_label = NA) {
  width <- w * prop.table(rexp(n))
  tax_price <- if (tax_max > 0) sort(c(tax_max, tax_max * rbeta(n - 1, 1, 3)), decreasing = TRUE)[seq_len(n)] else 0
  data.table(sector = s, width = width,
             ets = ets, ets_price = ets_price,
             tax = tax, tax_price = tax_price, tax_label = tax_label)
}

slices <- rbind(
  sim_sector("Elektriciteit",        15, 10, "ETS1 geveild",  70, "Energiebelasting (A en B)", 650, "A"),
  sim_sector("Elektriciteit",         7,  1, "ETS1 geveild",  70, "Energiebelasting (A en B)",   0),
  sim_sector("Industrie",             8,  8, "ETS1 geveild",  70, "Energiebelasting (A en B)", 600, "A"),
  sim_sector("Industrie",            30,  4, "ETS1 gratis",   70, "Overige belastingen op energie (E)", 40, "E"),
  sim_sector("Industrie",             5,  3, "Geen",           0, "Energiebelasting (A en B)", 330, "B"),
  sim_sector("Mobiliteit",           20,  1, "Geen",           0, "Accijns op minerale oliën (C en D)", 350, "C"),
  sim_sector("Mobiliteit",           15,  1, "Geen",           0, "Accijns op minerale oliën (C en D)", 210, "D"),
  sim_sector("Gebouwde\nomgeving",   18,  6, "Geen",           0, "Energiebelasting (A en B)", 340, "B"),
  sim_sector("Land-\nbouw",           4,  6, "Geen",           0, "Energiebelasting (A en B)", 520, "A"),
  sim_sector("Land-\nbouw",          18,  1, "Geen",           0, "Geen",                       0),
  sim_sector("Bunkers\nscheepvaart", 10,  1, "ETS1 geveild",  70, "Geen",                       0),
  sim_sector("Bunkers\nscheepvaart", 25,  1, "Geen",           0, "Geen",                       0),
  sim_sector("Bunkers\nluchtvaart",   6,  3, "ETS1 geveild",  50, "Vliegbelasting",           200),
  sim_sector("Bunkers\nluchtvaart",   4,  1, "Geen",           0, "Geen",                       0),
  sim_sector("Niet-\nenergetisch",   30,  1, "Geen",           0, "Geen",                       0),
  sim_sector("Bio-\nenergie",        13,  8, "Geen",           0, "Belastingen op biomassa",  560, "C")
)

# cumulative x positions per sector, ordered by total price within sector
slices <- merge(slices, sectors, by = "sector", sort = FALSE)
slices[, total := ets_price + tax_price]
setorder(slices, xmin, -total)
slices[, x1 := xmin + cumsum(width) - width, by = sector]
slices[, x2 := x1 + width]

# rectangles in long format ----
rects <- rbind(
  slices[ets != "Geen", .(x1, x2, y1 = 0, y2 = ets_price, instrument = ets)],
  slices[tax != "Geen" & tax_price > 0,
         .(x1, x2, y1 = ets_price, y2 = ets_price + tax_price, instrument = tax)]
)

instr_levels <- c("Energiebelasting (A en B)", "Accijns op minerale oliën (C en D)",
                  "Overige belastingen op energie (E)", "ETS1 geveild", "ETS1 gratis",
                  "Vliegbelasting", "Belastingen op biomassa")
rects[, instrument := factor(instrument, levels = instr_levels)]

# labels A-E at the top of the first slice of each group
labs <- slices[!is.na(tax_label), .SD[1], by = .(sector, tax_label)][
  , .(x = x1, y = total + 25, label = tax_label)]

ext_cost <- 180

# plot ----
p <- ggplot(rects) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = instrument)) +
  geom_vline(data = sectors[-1], aes(xintercept = xmin),
             colour = "grey60", linewidth = 0.2) +
  geom_hline(aes(yintercept = ext_cost, linetype = "Externe kosten klimaat"),
             colour = cpb_cols("biscay", palette = "extended"), linewidth = 0.5) +
  geom_text(data = labs, aes(x, y, label = label), size = 2, hjust = 0) +
  annotate("text", x = (sectors$xmin + sectors$xmax) / 2, y = 790,
           label = sectors$sector, size = 1.8, vjust = 1, lineheight = 0.9) +
  scale_fill_manual(values = unname(cpb_cols("endeavour", "anakiwa", "cerulean", "siren",
                                       "mauvelous", "purple1", "green2",
                                       palette = "extended")),
                    breaks = instr_levels) +
  scale_linetype_manual(values = "dashed") +
  scale_x_continuous(breaks = seq(0, 225, 25), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, 200),
                     expand = c(0, 0)) +
  labs(x = "Cumulatieve broeikasgasemissie (Mton CO2-eq.)",
       y = "Effectieve CO2-prijs (euro per ton CO2-eq.)",
       fill = NULL, linetype = NULL) +
  guides(fill = guide_legend(ncol = 2, order = 1),
         linetype = guide_legend(order = 2)) +
  theme_cpb()

save_cpb("co2_beprijzing.png", p, page = "full")
