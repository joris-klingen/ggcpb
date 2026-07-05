# legend_stability_test.R ----
#
# Extensive legend-position stability sweep, the companion to
# smoke_test_plots.R. The CPB flush legend must sit in one fixed
# bottom-left spot regardless of what the rest of the figure does. This
# script renders every variant in a large grid -- legend-label lengths,
# category-label lengths, legend item counts, all chart types, canvas
# sizes and title/axis-title combinations -- measures where the legend
# key actually lands in the rendered pixels, and reports PASS/FAIL per
# variant. Run it from anywhere with:
#
#   Rscript inst/examples/legend_stability_test.R
#
# Failing variants get their PNG written to
# inst/examples/output/legend_stability/ for visual inspection.
#
# How the measurement works: every variant maps a series to CPB primary
# blue (cpb_cols(6), #005faf) that is the *bottom* legend entry. The
# legend key sits at the plot margin while in-panel marks are indented
# past the axis labels, so the leftmost blue pixel IS the key's left
# edge and the lowest blue pixel marks the bottom key. Glyphs differ:
# rect keys (col/area/box/hist fills) paint their key box corner, line
# keys draw a stroke centred vertically in the box, point keys centre a
# dot in both directions -- so positions are judged per glyph family.

# Setup ----

get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(normalizePath(sub("^--file=", "", file_arg[[1]])))
  if (!is.null(sys.frames()[[1]]$ofile)) return(normalizePath(sys.frames()[[1]]$ofile))
  normalizePath(file.path(getwd(), "inst", "examples", "legend_stability_test.R"),
                mustWork = FALSE)
}

script_dir <- dirname(get_script_path())
pkg_root   <- normalizePath(file.path(script_dir, "..", ".."))
out_dir    <- file.path(script_dir, "output", "legend_stability")

devtools::load_all(pkg_root, quiet = TRUE)

for (pkg in c("ragg", "png")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("package '", pkg, "' is required for the pixel measurements")
  }
}

set.seed(42)

DPI <- 96
margin_px <- 10 / 72 * DPI        # left plot margin: 10 pt
key_h_px  <- 0.25 / 2.54 * DPI    # legend key height: 0.25 cm
key_w_px  <- 0.30 / 2.54 * DPI    # legend key width: 0.30 cm

# render a plot and locate the blue legend key in the pixels; returns
# the leftmost blue column and the lowest blue row's distance from the
# image's bottom edge (so values compare across canvas sizes)
measure_legend <- function(plot, width = 4, height = 4, file = NULL) {
  f <- if (is.null(file)) tempfile(fileext = ".png") else file
  ragg::agg_png(f, width = width, height = height, units = "in", res = DPI)
  ok <- tryCatch({ print(plot); TRUE },
                 error = function(e) { message("  render error: ", conditionMessage(e)); FALSE })
  grDevices::dev.off()
  if (!ok) return(list(left = NA_real_, bottom_off = NA_real_, file = f))
  img <- png::readPNG(f)
  blue <- img[, , 1] < 0.25 & img[, , 3] > 0.5 & img[, , 3] > img[, , 1] + 0.3
  if (!any(blue)) return(list(left = NA_real_, bottom_off = NA_real_, file = f))
  list(
    left       = min(which(apply(blue, 2, any))),
    bottom_off = nrow(img) - max(which(apply(blue, 1, any))),
    file       = f
  )
}

# Variant grid ----

# small helpers to build the data behind each variant
dodge_df <- function(labs) {
  data.frame(
    x = rep(c("a", "b"), each = length(labs)),
    g = factor(rep(labs, 2), levels = labs),
    y = rep(seq_along(labs), 2)
  )
}
num_df <- data.frame(
  x = rep(2015:2020, each = 2),
  g = factor(rep(c("s1", "s2"), 6), levels = c("s1", "s2")),
  y = c(1, 2, 2, 3, 3, 3, 4, 5, 4, 6, 5, 7)
)
hist_df <- data.frame(
  v = rep(c(1, 2, 2, 3, 3, 4, 4, 4, 5, 6), 2),
  g = factor(rep(c("s1", "s2"), each = 10), levels = c("s1", "s2"))
)
box_df <- data.frame(x = c("a", "b"), g = c("s1", "s2"),
                     p5 = 1, p25 = 2, p50 = 3, p75 = 4, p95 = 5)

col2 <- function(df, ...) {
  cpb_col(df, x = x, y = y, fill = g, position = "dodge",
          index = c(6, 2), title = "t", ...)
}
rev_colour <- ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE))

# each variant: name, glyph family, plot and canvas size (inches)
variant <- function(name, family, plot, width = 4, height = 4) {
  list(name = name, family = family, plot = plot, width = width, height = height)
}

variants <- list()

# 1) legend-label lengths, 1 character up to wider-than-the-panel and a
#    multi-line label (blue bottom entry keeps a one-line label)
lab_sets <- list(
  lab_tiny      = c("a", "b"),
  lab_short     = c("s1", "s2"),
  lab_medium    = c("koopkracht", "inflatie"),
  lab_long      = c("mediane koopkrachtontwikkeling van huishoudens",
                    "gemiddelde contractloonstijging in de marktsector"),
  lab_xlong     = c(paste(rep("een uitzonderlijk lang legenda-label", 2), collapse = " "),
                    "kort"),
  lab_multiline = c("s1", "een legenda-label\nover twee regels")
)
for (nm in names(lab_sets)) {
  variants[[length(variants) + 1]] <-
    variant(nm, "rect", col2(dodge_df(lab_sets[[nm]])))
}

# 2) category-label lengths (they move the panel edge, which is exactly
#    what the old panel-relative legend positioning tripped over)
long_cats <- data.frame(
  x = rep(c("een hele lange categorienaam (1,4 mln hh)",
            "nog een veel langere categorienaam"), each = 2),
  g = factor(rep(c("s1", "s2"), 2)),
  y = c(10, 20, 30, 40)
)
variants[[length(variants) + 1]] <-
  variant("cat_long_vertical", "rect", col2(long_cats))
variants[[length(variants) + 1]] <-
  variant("cat_long_horizontal", "rect", col2(long_cats, orientation = "horizontal"))

# 3) legend item counts
for (n in c(1, 2, 3, 5, 8)) {
  df <- dodge_df(paste("serie", seq_len(n)))
  variants[[length(variants) + 1]] <- variant(
    paste0("items_", n), "rect",
    cpb_col(df, x = x, y = y, fill = g, position = "dodge",
            index = c(6, 2, 3, 4, 5, 1, 7, 8)[seq_len(n)], title = "t")
  )
}

# 4) chart types
variants <- c(variants, list(
  variant("type_col_stack", "rect",
          cpb_col(dodge_df(c("s1", "s2")), x = x, y = y, fill = g,
                  index = c(6, 2), title = "t")),
  variant("type_area", "rect",
          cpb_area(num_df, x = x, y = y, fill = g, index = c(6, 2), title = "t")),
  variant("type_hist", "rect",
          cpb_hist(hist_df, x = v, fill = g, binwidth = 1,
                   index = c(6, 2), title = "t")),
  variant("type_box", "rect",
          cpb_box(box_df, x = x, p5 = p5, p25 = p25, p50 = p50, p75 = p75, p95 = p95,
                  fill = g, reverse_legend = TRUE, index = c(6, 2), title = "t")),
  variant("type_line_short", "line",
          cpb_line(num_df, x = x, y = y, colour = g,
                   index = c(6, 2), title = "t") + rev_colour),
  variant("type_line_long", "line",
          cpb_line(transform(num_df,
                             g = factor(g, labels = c("mediane koopkrachtontwikkeling",
                                                      "gemiddelde contractloonstijging"))),
                   x = x, y = y, colour = g, index = c(6, 2), title = "t") + rev_colour),
  variant("type_scatter_short", "point",
          cpb_scatter(num_df, x = x, y = y, colour = g,
                      index = c(6, 2), title = "t") + rev_colour),
  variant("type_scatter_long", "point",
          cpb_scatter(transform(num_df,
                                g = factor(g, labels = c("huishoudens met kinderen",
                                                         "huishoudens zonder kinderen"))),
                      x = x, y = y, colour = g, index = c(6, 2), title = "t") + rev_colour)
))

# 5) canvas sizes (CPB half/full page and off-grid sizes)
sizes <- list(half = c(2.98, 2.98), full = c(5.96, 2.98),
              tall = c(2.98, 4.5), square = c(4, 4), wide = c(6.5, 3))
for (nm in names(sizes)) {
  variants[[length(variants) + 1]] <- variant(
    paste0("size_", nm), "rect", col2(dodge_df(c("s1", "s2"))),
    width = sizes[[nm]][1], height = sizes[[nm]][2]
  )
}

# 6) titles and axis titles around the panel
base_df <- dodge_df(c("s1", "s2"))
mk <- function(...) cpb_col(base_df, x = x, y = y, fill = g, position = "dodge",
                            index = c(6, 2), ...)
variants <- c(variants, list(
  variant("ann_plain", "rect", mk()),
  variant("ann_title", "rect", mk(title = "titel")),
  variant("ann_title_ylab", "rect", mk(title = "titel", ylab = "mln euro")),
  variant("ann_title_xlab", "rect", mk(title = "titel", xlab = "inkomensgroep")),
  variant("ann_everything", "rect",
          mk(title = "een titel die over de volle breedte doorloopt",
             ylab = "% van het beschikbaar inkomen",
             xlab = "huishoudtype"))
))

# Measure ----

message("Measuring ", length(variants), " variants ...")
res <- do.call(rbind, lapply(variants, function(v) {
  m <- measure_legend(v$plot, width = v$width, height = v$height)
  data.frame(name = v$name, family = v$family,
             width = v$width, height = v$height,
             left = m$left, bottom_off = m$bottom_off,
             file = m$file, stringsAsFactors = FALSE)
}))

# Judge ----

# left edge: judged against the rect-family consensus column (which
# itself must sit at the plot margin, within antialiasing). Line keys
# may antialias their stroke end one further pixel in; the point key's
# dot must fall inside that same key box.
left_anchor <- stats::median(res$left[res$family == "rect"], na.rm = TRUE)
stopifnot(abs(left_anchor - margin_px) <= 2)
left_tol <- c(rect = 1, line = 2, point = key_w_px + 2)
res$left_ok <- !is.na(res$left) &
  res$left >= left_anchor - 1 &
  res$left <= left_anchor + left_tol[res$family]

# bottom: judged against the rect-family consensus row; line strokes sit
# up to half a key higher (stroke centring), points anywhere in the box
rect_anchor <- stats::median(res$bottom_off[res$family == "rect"], na.rm = TRUE)
tol <- c(rect = 2, line = key_h_px / 2 + 2, point = key_h_px + 2)
res$bottom_ok <- !is.na(res$bottom_off) &
  abs(res$bottom_off - rect_anchor) <= tol[res$family]
res$ok <- res$left_ok & res$bottom_ok

# Report ----

fmt <- res[, c("name", "family", "width", "height", "left", "bottom_off")]
fmt$left_ok   <- ifelse(res$left_ok, "ok", "FAIL")
fmt$bottom_ok <- ifelse(res$bottom_ok, "ok", "FAIL")
print(fmt, row.names = FALSE)

message(sprintf(
  "\nanchors at %d dpi: left = %.0f px (plot margin %.1f px), bottom offset = %.0f px",
  DPI, left_anchor, margin_px, rect_anchor
))
message(sprintf("%d/%d variants stable", sum(res$ok), nrow(res)))

if (any(!res$ok)) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  for (i in which(!res$ok)) {
    kept <- file.path(out_dir, paste0(res$name[i], ".png"))
    file.copy(res$file[i], kept, overwrite = TRUE)
    message("  wrote ", kept)
  }
  stop("legend position is NOT stable for: ",
       paste(res$name[!res$ok], collapse = ", "))
}
unlink(res$file)
message("legend position is stable across all variants")
