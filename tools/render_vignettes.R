# render_vignettes.R ----
#
# Renders the vignettes to HTML in doc/, so there is a readable copy with
# the CPB layout (figures next to their parameters) without installing the
# package.
#
# Re-run after editing a vignette:
#
#   Rscript tools/render_vignettes.R
#
# and commit the updated doc/*.html. doc/ is .Rbuildignore'd, so the built
# package ships only the real vignettes.

pkg_root <- normalizePath(file.path(dirname(sub(
  "^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
)), ".."))

devtools::load_all(pkg_root, quiet = TRUE)

# Dutch figures carry accented characters. Under a non-UTF-8 locale R
# reads each of those as two undefined bytes, and the graphics device
# silently draws them as missing glyphs, so the rendered figures look fine
# to the build and wrong to the reader. Fail loudly instead of committing
# that.
if (!ggcpb:::cpb_utf8_locale()) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "nl_NL.UTF-8",
                "English_United States.utf8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
  }
}
if (!ggcpb:::cpb_utf8_locale()) {
  stop("LC_CTYPE is '", Sys.getlocale("LC_CTYPE"), "', which cannot ",
       "represent the accented characters in the figures, and no UTF-8 ",
       "locale is available to switch to. Install one, or re-run with e.g. ",
       "LC_ALL=C.UTF-8 Rscript tools/render_vignettes.R", call. = FALSE)
}

doc_dir <- file.path(pkg_root, "doc")
dir.create(doc_dir, showWarnings = FALSE)

for (rmd in list.files(file.path(pkg_root, "vignettes"),
                       pattern = "\\.Rmd$", full.names = TRUE)) {
  message("rendering ", basename(rmd))
  rmarkdown::render(rmd, output_dir = doc_dir, quiet = TRUE)
}

# the installed package carries the same pages in inst/doc, so they show up
# under "User guides" in the help after install_github(), which does not
# build vignettes
inst_doc <- file.path(pkg_root, "inst", "doc")
dir.create(inst_doc, showWarnings = FALSE, recursive = TRUE)
for (rmd in list.files(file.path(pkg_root, "vignettes"),
                       pattern = "\\.Rmd$", full.names = TRUE)) {
  html <- file.path(doc_dir, sub("\\.Rmd$", ".html", basename(rmd)))
  file.copy(c(rmd, html), inst_doc, overwrite = TRUE)
}

message("rendered vignettes written to ", doc_dir, " and ", inst_doc)
