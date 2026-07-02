# setup.R ----
#
# The bundled RijksoverheidSansText font is registered with systemfonts, so it
# renders correctly on the ragg device used by save_cpb() -- the real CPB
# export path. Both ggplot_build() and ggsave(device = ragg) are warning-free.
# Base graphics devices (the PostScript/PDF device R falls back to when a plot
# is *drawn* headlessly, e.g. inside ggplotGrob()) do not know the font and
# emit a benign "font family 'RijksoverheidSansText' not found in ... font
# database" warning that says nothing about the code under test. Point the
# default device at ragg's in-memory (systemfonts-aware) device for the test
# run so drawn plots resolve the font -- no files written, and every genuine
# warning still surfaces.
if (requireNamespace("ragg", quietly = TRUE)) {
  options(device = function(...) ragg::agg_capture(...))
}
