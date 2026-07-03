# reference_plot_snippets.R ----
#
# Anonymised style reference for the ggcpb package.
#
# These are the raw ggplot2 plotting snippets distilled from the internal
# CPB analysis scripts that ggcpb is built to reproduce. Everything that is
# specific to the source project -- data loading, file paths, statistical
# preparation and the original (Dutch) titles and labels -- has been
# stripped out. What remains is only the *styling*: the geoms, scales,
# colour choices, coordinate systems and theme() settings that define the
# CPB house look. All data below is simulated and all text is a generic
# placeholder.
#
# Run the whole file with:
#
#   Rscript references/code/reference_plot_snippets.R
#
# Each figure is rendered to a ragg PNG under tempdir()/ggcpb_reference/.
# The CPB palette and background/grid tokens are defined inline so the file
# has no dependency on the package itself; ggcpb exposes the same values via
# cpb_tokens(), cpb_cols() and cpb_pal(). Scripts that only did data
# aggregation, and the ones that used the legacy nicerplot::nplot() wrapper
# instead of raw ggplot2, are not represented here.

# Setup ----

suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
  library(grid)      # unit()
})

set.seed(1)

# CPB design tokens (see ggcpb::cpb_tokens()) ----

cpb_colors <- c(
  "#F596AF", "#e6006e", "#820050", "#d7c8c8", "#87d2ff",
  "#005faf", "#193c69", "#96827d", "#64504b", "lightgrey"
)
cpb_colors_discr <- c(
  "#eb0073", "#005795", "#fad1e8", "#b7e4ff",
  "#820050", "#97cafb", "#00a5ff", "lightgrey"
)
cpb_bg   <- "#eef8ff" # plot background
cpb_grid <- "#c9d1da" # gridline colour

# Font + rendering ----
# The CPB house font (RijksoverheidSansText) ships with ggcpb. Register the
# bundled files best-effort so the figures render in the real typeface; if
# the files or the systemfonts backend are missing, fall back to the default
# family so the script still runs. Figures are drawn through the ragg device,
# which is font-safe on any platform -- base graphics devices such as pdf()
# choke on registered TTF families, so we avoid them here.

have_ragg <- requireNamespace("ragg", quietly = TRUE)

cpb_family <- tryCatch({
  fdir <- file.path("inst", "fonts")
  reg  <- file.path(fdir, "RijksoverheidSansText-Regular_2_0.ttf")
  if (have_ragg && requireNamespace("systemfonts", quietly = TRUE) && file.exists(reg)) {
    systemfonts::register_font(
      name   = "RijksoverheidSansText",
      plain  = reg,
      bold   = file.path(fdir, "RijksoverheidSansText-Bold_2_0.ttf"),
      italic = file.path(fdir, "RijksoverheidSansText-Italic_2_0.ttf")
    )
    "RijksoverheidSansText"
  } else {
    ""   # default ggplot2 family: base devices cannot use the TTF
  }
}, error = function(e) "")

out_dir <- Sys.getenv("GGCPB_REF_OUTDIR",
                      unset = file.path(tempdir(), "ggcpb_reference"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# render a figure to a ragg PNG at the CPB export size; falls back to the
# active device (with an explicit print()) when ragg is unavailable.
show_fig <- function(p, file, width = 2.98, height = 2.98, dpi = 150) {
  if (have_ragg) {
    ragg::agg_png(file.path(out_dir, file), width = width, height = height,
                  units = "in", res = dpi)
    on.exit(grDevices::dev.off())
  }
  print(p)
  invisible(NULL)
}

# Placeholder labels ----

# generic category / series names reused across the snippets, so no real
# category (income group, technology, contract type, ...) is exposed.
groups6 <- paste("Group", 1:6)
series2 <- paste("Series", 1:2)
series3 <- paste("Series", 1:3)
series4 <- paste("Series", 1:4)

# Faceted boxplot ----
# Errorbars (p5-p25 and p75-p95) drawn around a p25/p50/p75 box, dodged by a
# two-level series, flipped horizontal, with the category blocks stacked as
# free-scale vertical facets. Percentage value axis.

facets <- paste("Block", 1:4)
box_fac <- CJ(block    = factor(facets, levels = facets),
              category = factor(groups6, levels = groups6),
              series   = factor(series2, levels = series2))
box_fac[, mu := rnorm(.N, 0, 2)]
box_fac[, `:=`(p50 = mu,
               p25 = mu - runif(.N, .5, .9),
               p75 = mu + runif(.N, .5, .9),
               p5  = mu - runif(.N, 1.4, 1.9),
               p95 = mu + runif(.N, 1.4, 1.9))]

p_box_facet <- ggplot(box_fac, aes(x = category,
                                   fill = factor(series, levels = rev(series2)))) +
  geom_errorbar(aes(ymin = p5, ymax = p25),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_errorbar(aes(ymin = p75, ymax = p95),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_boxplot(aes(ymin = p25, lower = p25, middle = p50, upper = p75, ymax = p75),
               stat = "identity", width = 0.6, alpha = 0.85, linewidth = 0.15,
               position = position_dodge(width = 0.7), key_glyph = "rect") +
  geom_hline(aes(yintercept = 0), color = "black", linewidth = .3) +
  coord_flip(ylim = c(-6, 6)) +
  facet_wrap(~ block, ncol = 1, scales = "free_y",
             strip.position = "top", axes = "all", axis.labels = "all") +
  labs(title = "Figure title", x = NULL, y = "value axis label", fill = "") +
  scale_fill_manual(values = setNames(cpb_colors[c(6, 2)], series2)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position  = "plot",
    plot.title           = element_text(size = 9, face = "bold",   hjust = 0),
    axis.text            = element_text(size = 6, color = "black"),
    axis.title           = element_text(size = 7, face = "italic", hjust = 1),
    legend.text          = element_text(face = "italic", size = 7),
    strip.text           = element_text(size = 7, face = "bold", hjust = 0),
    strip.background     = element_blank(),
    panel.spacing.y      = unit(0.8, "lines"),
    panel.grid.minor.y   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor.x   = element_line(color = cpb_grid),
    panel.grid.major.x   = element_line(color = cpb_grid),
    legend.key.height    = unit(.25, "cm"),
    legend.key.width     = unit(.30, "cm"),
    legend.key.spacing.y = unit(.05, "cm"),
    legend.position      = c(-0.18, -.03),
    legend.direction     = "vertical",
    plot.margin          = margin(10, 10, 25, 10),
    plot.background      = element_rect(fill = cpb_bg, color = NA)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

show_fig(p_box_facet, "01_box_facet.png", width = 2.95, height = 9)   # export dpi: 800

# Boxplot with two series and a euro value axis ----
# Single panel version of the above with a subtitle, a currency (euro) axis
# and the small trick that nudges the (overlaid) legend left based on the
# longest category label.

box_eur <- CJ(category = factor(groups6, levels = groups6),
              series   = factor(series2, levels = series2))
box_eur[, mu := runif(.N, 300, 2500)]
box_eur[, `:=`(p50 = mu, p25 = mu * 0.80, p75 = mu * 1.20,
               p5  = mu * 0.55, p95 = mu * 1.50)]

max_label_len <- max(nchar(as.character(box_eur[, category])))
legend_x      <- pmin(-0.01, 0.2 - max_label_len * 0.027)

p_box_eur <- ggplot(box_eur, aes(x = category,
                                 fill = factor(series, levels = rev(series2)))) +
  geom_errorbar(aes(ymin = p5, ymax = p25),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_errorbar(aes(ymin = p75, ymax = p95),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_boxplot(aes(ymin = p25, lower = p25, middle = p50, upper = p75, ymax = p75),
               stat = "identity", width = 0.6, alpha = 0.85, linewidth = 0.15,
               position = position_dodge(width = 0.7), key_glyph = "rect") +
  coord_flip(ylim = c(0, box_eur[, max(p95)])) +
  labs(title = "Figure title", subtitle = "category label",
       x = NULL, y = "value axis label", fill = "") +
  scale_fill_manual(values = setNames(cpb_colors[c(6, 2)], series2)) +
  scale_y_continuous(labels = scales::label_currency(big.mark = ".", decimal.mark = ",",
                                                     prefix = "€")) +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position    = "plot",
    plot.title             = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle          = element_text(size = 7, face = "italic", hjust = 0),
    axis.text              = element_text(size = 6, color = "black"),
    axis.title             = element_text(size = 7, face = "italic", hjust = 1),
    legend.text            = element_text(face = "italic", size = 7),
    panel.grid.minor.y     = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor.x     = element_line(color = cpb_grid),
    panel.grid.major.x     = element_line(color = cpb_grid),
    legend.key.height      = unit(.25, "cm"),
    legend.key.width       = unit(.30, "cm"),
    legend.key.spacing.y   = unit(.05, "cm"),
    legend.location        = "plot",
    legend.position        = c(legend_x, -0.15),
    plot.margin            = margin(10, 10, 25, 10),
    plot.background        = element_rect(fill = cpb_bg, color = NA)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

show_fig(p_box_eur, "02_box_euro.png", width = 2.98, height = 2.98)

# Boxplot with a four-level fill ----
# Same box construction dodged by a four-level series, coloured from an
# explicit hand-picked subset of the palette (light blue / blue / light
# pink / magenta), legend stacked vertically below the panel.

box4 <- CJ(category = factor(groups6, levels = groups6),
           series   = factor(series4, levels = series4))
box4[, mu := rnorm(.N, 0, 1.5)]
box4[, `:=`(p50 = mu, p25 = mu - .6, p75 = mu + .6,
            p5  = mu - 1.4, p95 = mu + 1.4)]

series4_colors <- setNames(c("#87d2ff", "#005faf", "#F596AF", "#e6006e"), series4)

p_box_four <- ggplot(box4, aes(x = category,
                               fill = factor(series, levels = rev(series4)))) +
  geom_errorbar(aes(ymin = p5, ymax = p25),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_errorbar(aes(ymin = p75, ymax = p95),
                position = position_dodge(width = 0.7), width = 0.25, linewidth = .15) +
  geom_boxplot(aes(ymin = p25, lower = p25, middle = p50, upper = p75, ymax = p75),
               stat = "identity", width = 0.6, alpha = 0.85, linewidth = 0.15,
               position = position_dodge(width = 0.7), key_glyph = "rect") +
  coord_flip(ylim = c(-4, 4)) +
  geom_hline(aes(yintercept = 0), color = "black", linewidth = .3) +
  scale_fill_manual(values = series4_colors) +
  labs(title = "Figure title", subtitle = "category label",
       x = NULL, y = "value axis label", fill = "") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position  = "plot",
    plot.title           = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle        = element_text(size = 7, face = "italic", hjust = 0),
    axis.text            = element_text(size = 6, color = "black"),
    axis.title           = element_text(size = 7, face = "italic", hjust = 1),
    legend.text          = element_text(face = "italic", size = 7),
    panel.grid.minor.y   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor.x   = element_line(color = cpb_grid),
    panel.grid.major.x   = element_line(color = cpb_grid),
    legend.key.height    = unit(.25, "cm"),
    legend.key.width     = unit(.30, "cm"),
    legend.key.spacing.y = unit(.05, "cm"),
    legend.position      = c(-0.025, -.18),
    legend.direction     = "vertical",
    plot.margin          = margin(10, 10, 50, 10),
    plot.background      = element_rect(fill = cpb_bg, color = NA)
  ) +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE))

show_fig(p_box_four, "03_box_four.png", width = 2.98, height = 4)   # export dpi: 500

# Simple single-series boxplot ----
# One box per category, no fill grouping, single palette colour, a plain
# number axis (Dutch thousands/decimal marks) and no legend.

box_one <- data.table(category = factor(groups6, levels = groups6))
box_one[, mu := runif(.N, 500, 3000)]
box_one[, `:=`(p50 = mu, p25 = mu * 0.80, p75 = mu * 1.20,
               p5  = mu * 0.60, p95 = mu * 1.45)]

p_box_one <- ggplot(box_one, aes(x = category)) +
  geom_errorbar(aes(ymin = p5,  ymax = p25), width = 0.25, linewidth = .15) +
  geom_errorbar(aes(ymin = p75, ymax = p95), width = 0.25, linewidth = .15) +
  geom_boxplot(aes(ymin = p25, lower = p25, middle = p50, upper = p75, ymax = p75),
               stat = "identity", width = 0.6, alpha = 0.85, linewidth = 0.15,
               fill = cpb_colors[6], key_glyph = "rect") +
  coord_flip(ylim = c(0, box_one[, max(p95)])) +
  labs(title = "Figure title \n", subtitle = "category label",
       x = NULL, y = "value axis label") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position    = "plot",
    plot.title             = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle          = element_text(size = 7, face = "italic", hjust = 0),
    axis.text              = element_text(size = 6, color = "black"),
    axis.title             = element_text(size = 7, face = "italic", hjust = 1),
    panel.grid.minor.y     = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor.x     = element_line(color = cpb_grid),
    panel.grid.major.x     = element_line(color = cpb_grid),
    plot.margin            = margin(10, 10, 30, 10),
    plot.background        = element_rect(fill = cpb_bg, color = NA)
  )

show_fig(p_box_one, "04_box_single.png", width = 2.98, height = 2.98)

# Dodged horizontal bar with annotation ----
# Grouped (dodged) horizontal bars coloured from the palette, a dashed
# reference line and an italic annotation label anchored to it.

bar_dodge <- CJ(category = factor(groups6, levels = groups6),
                level    = factor(series4, levels = series4))
bar_dodge[, share := runif(.N, 0.05, 0.60)]
n_lvl <- nlevels(bar_dodge[, level])

p_bar_dodge <- ggplot(bar_dodge, aes(x = category, y = share, fill = level)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           alpha = 0.9, key_glyph = "rect") +
  geom_hline(yintercept = 0.20, linetype = "dashed",
             color = "#666666", linewidth = 0.4) +
  annotate("text", x = 6.43, y = 0.2, label = "reference line",
           hjust = 1.05, vjust = 0, size = 2.0, color = "#666666",
           family = cpb_family, fontface = "italic") +
  coord_flip() +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_fill_manual(values = unname(cpb_colors)[seq_len(n_lvl)]) +
  labs(title = "Figure title", subtitle = "category label",
       x = NULL, y = "value axis label", fill = "") +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position  = "plot",
    plot.title           = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle        = element_text(size = 7, face = "italic", hjust = 0),
    axis.text            = element_text(size = 7, color = "black"),
    axis.title           = element_text(size = 7, face = "italic", hjust = 1),
    legend.text          = element_text(face = "italic", size = 7),
    panel.grid.minor.y   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor.x   = element_line(color = cpb_grid),
    panel.grid.major.x   = element_line(color = cpb_grid),
    legend.key.height    = unit(.25, "cm"),
    legend.key.width     = unit(.30, "cm"),
    legend.key.spacing.y = unit(.05, "cm"),
    legend.position      = c(-.1, -0.25),
    legend.direction     = "vertical",
    plot.margin          = margin(10, 10, 50, 10),
    plot.background      = element_rect(fill = cpb_bg, color = NA)
  )

show_fig(p_bar_dodge, "05_bar_dodged.png", width = 2.98, height = 3.4)   # export dpi: 500

# Simple horizontal bar ----
# One bar per category, single palette colour, percentage axis with a fixed
# limit.

bar_one <- data.table(category = factor(groups6, levels = groups6),
                      share    = runif(6, 10, 65))

p_bar_one <- ggplot(bar_one, aes(x = category, y = share)) +
  geom_col(width = 0.6, alpha = 1, fill = cpb_colors[6], key_glyph = "rect") +
  coord_flip(ylim = c(0, 70)) +
  labs(title = "Figure title \n", subtitle = "category label",
       x = NULL, y = "value axis label") +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title.position    = "plot",
    plot.title             = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle          = element_text(size = 7, face = "italic", hjust = 0),
    axis.text              = element_text(size = 6, color = "black"),
    axis.title             = element_text(size = 7, face = "italic", hjust = 1),
    panel.grid.minor.y     = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor.x     = element_line(color = cpb_grid),
    panel.grid.major.x     = element_line(color = cpb_grid),
    plot.margin            = margin(10, 10, 30, 10),
    plot.background        = element_rect(fill = cpb_bg, color = NA)
  )

show_fig(p_bar_one, "06_bar_single.png", width = 2.98, height = 2.98)

# Stacked share bar over time ----
# Columns filled to 100% per month, three-level fill from a hand-picked
# palette subset, a date x-axis and a percentage y-axis.

report_months <- seq(as.Date("2026-01-01"), as.Date("2027-12-01"), by = "month")
share_time <- CJ(peildatum = report_months,
                 status    = factor(series3, levels = series3))
share_time[, peildatum := as.Date(peildatum)]
share_time[, n_hh := round(runif(.N, 1000, 5000))]

p_share_time <- ggplot(share_time, aes(peildatum, n_hh, fill = status)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = setNames(cpb_colors[c(6, 5, 2)], series3)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b `%y") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(title = "Figure title", subtitle = "figure subtitle",
       x = NULL, y = "", fill = NULL) +
  theme_minimal(base_family = cpb_family) +
  theme(
    legend.position        = c(0.1, -.25),
    plot.title.position    = "plot",
    plot.title             = element_text(size = 9, face = "bold",   hjust = 0),
    plot.subtitle          = element_text(size = 7, face = "italic", hjust = 0),
    axis.text              = element_text(size = 7, color = "black"),
    axis.title             = element_text(size = 7, face = "italic", hjust = 1),
    legend.text            = element_text(face = "italic", size = 7),
    strip.text             = element_text(size = 7),
    panel.grid.minor.x     = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.minor.y     = element_line(color = cpb_grid),
    panel.grid.major.y     = element_line(color = cpb_grid),
    legend.key.height      = unit(.25, "cm"),
    legend.key.width       = unit(.30, "cm"),
    legend.key.spacing.y   = unit(.05, "cm"),
    legend.location        = "plot",
    plot.margin            = margin(10, 10, 40, 10),
    plot.background        = element_rect(fill = cpb_bg, color = NA)
  )

show_fig(p_share_time, "07_stacked_share_time.png", width = 5.9, height = 2.9)   # export dpi: 500

# Faceted stacked columns ----
# Stacked columns over a small number of periods, one facet per panel, a
# three-colour fill (one series drawn below the zero line), a solid zero line
# and a dashed vertical marker.

panels <- paste("Panel", 1:2)
stack_fac <- CJ(panel = factor(panels, levels = panels),
                jaar  = 2022:2024,
                etype = factor(series3, levels = series3))
stack_fac[, value := runif(.N, 100, 1200)]
stack_fac[etype == series3[3], value := -value * 0.5]   # a below-axis series

p_stack_fac <- ggplot(stack_fac, aes(x = as.character(jaar), y = value, fill = etype)) +
  geom_col(position = "stack", width = .75) +
  scale_fill_manual(values = unname(cpb_colors)[c(6, 2, 4)]) +
  facet_wrap(~ panel, nrow = 1, scales = "free_x") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2, linetype = "dashed") +   # marks the middle period
  labs(x = "", y = "", title = "Figure title", subtitle = "figure subtitle", fill = "") +
  theme_minimal(base_family = cpb_family) +
  theme(
    plot.title           = element_text(size = 9, face = "bold", hjust = 0),
    plot.title.position  = "plot",
    plot.subtitle        = element_text(size = 7, face = "italic", hjust = 0),
    axis.text            = element_text(size = 7, color = "black"),
    axis.title           = element_text(size = 7),
    strip.text           = element_text(size = 7),
    legend.text          = element_text(face = "italic", size = 7),
    panel.spacing.x      = unit(4, "lines"),
    panel.grid.minor.x   = element_blank(),
    panel.grid.major.x   = element_blank(),
    panel.grid.minor.y   = element_line(color = cpb_grid),
    panel.grid.major.y   = element_line(color = cpb_grid),
    legend.key.size      = unit(.3, "cm"),
    legend.position      = c(0, -.3),
    plot.margin          = margin(10, 10, 40, 10),
    plot.background      = element_rect(fill = cpb_bg, color = NA)
  )

show_fig(p_stack_fac, "08_stacked_columns_facet.png", width = 2.95, height = 2.95)   # export dpi: 500

# Faceted histogram ----
# One histogram per category level, free y-scales, white bar outlines and a
# three-colour fill. This one keeps the plain theme_minimal() the source used
# (no CPB background), included for contrast.

hist_dt <- data.table(
  value = c(round(rgamma(3000,  8, 0.6)),
            round(rgamma(1500, 20, 0.9)),
            round(rgamma( 800, 30, 1.2))),
  type  = factor(rep(series3, c(3000, 1500, 800)), levels = series3)
)

p_hist <- ggplot(hist_dt, aes(value, fill = type)) +
  geom_histogram(binwidth = 1, colour = "white") +
  facet_wrap(~ type, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = c(1, 6, 12, 18, 24, 36, 48, 60)) +
  scale_fill_manual(values = setNames(cpb_colors_discr[1:3], series3)) +
  labs(title = "Figure title", subtitle = "figure subtitle",
       x = "value axis label", y = "count axis label") +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold"),
        legend.position = "none",
        strip.text      = element_text(face = "bold"))

show_fig(p_hist, "09_histogram_facet.png", width = 8, height = 7)

# Stacked area over time ----
# Counts over a monthly date axis, stacked area by a three-level source,
# relabelled legend keys, legend on top. Also uses the plain theme_minimal()
# of the source.

months_seq <- seq(as.Date("2021-01-01"), as.Date("2027-01-01"), by = "month")
area_dt <- CJ(peildatum = months_seq, source = factor(series3, levels = series3))
area_dt[, peildatum := as.Date(peildatum)]
area_dt[, N := round(runif(.N, 100, 1000))]

p_area <- ggplot(area_dt, aes(peildatum, N, fill = source)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = setNames(cpb_colors_discr[1:3], series3),
                    labels = setNames(c("label a", "label b", "label c"), series3)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(title = "Figure title", subtitle = "figure subtitle",
       x = NULL, y = "count axis label", fill = NULL) +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold"),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "top")

show_fig(p_area, "10_area_time.png", width = 9, height = 5)

# Done ----

message("reference figures written to: ", out_dir)
