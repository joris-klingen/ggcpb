# render_vignettes.R ----
#
# Renders the vignettes to GitHub-flavoured markdown in doc/, so the
# README can link to *rendered* pages (with figures) that display
# directly in the repository UI -- GitHub shows raw HTML files as
# source, but renders .md files. Re-run after editing a vignette:
#
#   Rscript tools/render_vignettes.R
#
# and commit the updated doc/ output. doc/ is .Rbuildignore'd, so the
# built package still ships only the real (html_vignette) vignettes.

pkg_root <- normalizePath(file.path(dirname(sub(
  "^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
)), ".."))

devtools::load_all(pkg_root, quiet = TRUE)

# Dutch figures carry accented characters ("reele", "geindexeerd" with
# their diaereses). Under a non-UTF-8 locale R reads each of those as
# two undefined bytes, and the graphics device silently draws them as
# missing glyphs -- so the rendered figures look fine to the build and
# wrong to the reader. Fail loudly instead of committing that.
if (!ggcpb:::cpb_utf8_locale()) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "nl_NL.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
  }
}
if (!ggcpb:::cpb_utf8_locale()) {
  stop("LC_CTYPE is '", Sys.getlocale("LC_CTYPE"), "', which cannot ",
       "represent the accented characters in the figures, and no UTF-8 ",
       "locale is available to switch to. Install one, or re-run with e.g. ",
       "LC_ALL=C.UTF-8 Rscript tools/render_vignettes.R", call. = FALSE)
}

out_dir <- file.path(pkg_root, "doc")
dir.create(out_dir, showWarnings = FALSE)

for (rmd in list.files(file.path(pkg_root, "vignettes"),
                       pattern = "\\.Rmd$", full.names = TRUE)) {
  message("rendering ", basename(rmd))
  # render from a copy inside doc/ so the figure links in the .md come
  # out relative (chart-types_files/...), not absolute
  local_rmd <- file.path(out_dir, basename(rmd))
  file.copy(rmd, local_rmd, overwrite = TRUE)
  rmarkdown::render(
    local_rmd,
    output_format = rmarkdown::github_document(html_preview = FALSE),
    quiet         = TRUE
  )
  unlink(local_rmd)

  # Cross-vignette links are written as sibling .html in the .Rmd, which
  # is what the built (html_vignette) package needs. In doc/ the siblings
  # are .md, so repoint them there -- otherwise the link 404s on GitHub.
  md <- sub("\\.Rmd$", ".md", local_rmd)
  if (file.exists(md)) {
    txt <- readLines(md, warn = FALSE)
    writeLines(gsub("(\\]\\([^):]*)\\.html\\)", "\\1.md)", txt), md)
  }
}

message("rendered vignettes written to ", out_dir)
