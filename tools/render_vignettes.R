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
}

message("rendered vignettes written to ", out_dir)
