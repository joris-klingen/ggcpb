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


# The fixed font sizes, in points: never shrunk to fit
cpb_font_pt  <- 7 # all text except the title
cpb_title_pt <- 9 # the title

cpb_cm_to_in <- function(cm) cm / 2.54
cpb_in_to_cm <- function(inches) inches * 2.54
