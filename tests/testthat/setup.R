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

# A layer's geom class name, for tests that check which geom a layer uses
# (e.g. `geom_class1(l) == "GeomBoxplot"`). Not simply `class(l$geom)[1]`:
# under ggplot2 3.5.x (not 4.0, where this package is developed), a geom
# constructed with a string `key_glyph` (e.g. `key_glyph = "rect"`, used
# throughout wrappers.R) gets an anonymous ggproto subclass with class
# `""` prepended ahead of its real class -- an internal ggplot2 quirk,
# unrelated to and unaffected by anything ggcpb does, that does not change
# how the geom draws. Dropping empty-string entries keeps these tests
# meaningful on both ggplot2 versions instead of failing on 3.5.x over an
# artifact of how the class vector happens to be ordered.
geom_class1 <- function(l) {
  cl <- class(l$geom)
  cl[cl != ""][1]
}
