# format.R ----
#
# The fixed CPB figure format: page sizes, margins, the figure top and
# bottom, the legend grid, the footnote and the font sizes. Every
# figure is laid out with these same distances, whatever it shows.

# Page sizes, in cm, and the margins a figure is laid out with
cpb_page_width_cm    <- c(half = 7.5, full = 15.5, small = 6.8)
cpb_page_height_cm   <- c(half = 7.5, full = 7.5, small = 6.8)
# figure edge to the titles, the legend and the axis titles, left and right
cpb_labels_margin_cm <- 0.45
cpb_margin_east_cm   <- 0.635 # figure edge to the plot area, on the right
# tick labels to the plot area: 1.5% of the figure width
cpb_y_lab_gap_cm <- function(width_cm) 0.015 * width_cm

# The figure bottom, in cm below the plot area: the x tick labels, the
# x-title's centre, and the legend grid, whose first row is centred
# 1.3 cm above the figure's bottom edge
cpb_margin_south_cm           <- 2.3   # plot area to the bottom edge
cpb_margin_south_no_legend_cm <- 1.25  # the same, without a legend
cpb_x_lab_gap_cm              <- 0.15  # plot area to the x tick labels
cpb_x_title_cm                <- 0.6   # plot area to the x-title's centre
cpb_legend_line_cm            <- 0.315 # height of one legend row
cpb_legend_top_cm <- cpb_margin_south_cm - 1.3 - cpb_legend_line_cm / 2
cpb_legend_symbol_cm <- c(width = 0.3, height = 0.8 * cpb_legend_line_cm)
cpb_legend_symbol_txt_cm <- 0.15  # legend symbol to its label
cpb_legend_column_cm     <- 0.375 # between two legend columns
cpb_legend_per_column    <- 3     # legend items per column
# The footnote a ggplot2 caption is drawn as: right-aligned, centred
# just above the figure's bottom edge, italic
cpb_footnote_cm   <- c(x = 0.25, y = 0.15) # from the right and bottom edges
cpb_footnote_size <- 0.85                  # as a share of the base font size

# The fixed font sizes, in points: never shrunk to fit
cpb_font_pt  <- 7 # all text except the title
cpb_title_pt <- 9 # the title

cpb_cm_to_in <- function(cm) cm / 2.54
cpb_in_to_cm <- function(inches) inches * 2.54
