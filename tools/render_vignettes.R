# render_vignettes.R ----
#
# Renders the vignettes for GitHub, so the README can link to *rendered*
# pages (with figures) in the repository UI. GitHub shows raw HTML files
# as source, so:
#
# * the introduction, import and extended vignettes (the ones with the CPB
#   layout: figures next to their parameters) are printed from their HTML
#   to a PDF in doc/. The PDFs are not tracked in git (they are in
#   .gitignore), so everybody makes their own. This needs Chrome (set the
#   CHROME environment variable to its path if it is not found).
# * the other vignettes are rendered to GitHub-flavoured markdown in doc/.
#
# Re-run after editing a vignette:
#
#   Rscript tools/render_vignettes.R
#
# and commit the updated doc/ output. doc/ is .Rbuildignore'd, so the built
# package still ships only the real (html_vignette) vignettes.
#
# tools/check_docs_fresh.R checks whether the markdown needs re-running at
# all -- run that first if unsure.

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

pdf_vignettes <- c("introduction", "import", "extended")

# Chrome, to print a vignette's HTML to a PDF
find_chrome <- function() {
  candidates <- c(
    Sys.getenv("CHROME"),
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    Sys.which(c("google-chrome", "chromium", "chromium-browser")),
    "C:/Program Files/Google/Chrome/Application/chrome.exe"
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates)) candidates[[1]] else ""
}
chrome <- find_chrome()

# Cross-vignette links are written as sibling .html in the .Rmd, which is
# what the built (html_vignette) package needs. In doc/ the siblings are
# .md, except the PDF vignettes, which are .pdf.
repoint_links <- function(md) {
  txt <- readLines(md, warn = FALSE)
  for (nm in pdf_vignettes) {
    txt <- gsub(sprintf("\\]\\(%s\\.html\\)", nm),
                sprintf("](%s.pdf)", nm), txt)
  }
  writeLines(gsub("(\\]\\([^):]*)\\.html\\)", "\\1.md)", txt), md)
}

doc_dir <- file.path(pkg_root, "doc")
dir.create(doc_dir, showWarnings = FALSE)
vig_dir <- file.path(pkg_root, "vignettes")

for (rmd in list.files(vig_dir, pattern = "\\.Rmd$", full.names = TRUE)) {
  name <- sub("\\.Rmd$", "", basename(rmd))
  message("rendering ", basename(rmd))

  if (name %in% pdf_vignettes) {
    html <- rmarkdown::render(rmd, quiet = TRUE)
    if (!nzchar(chrome)) {
      warning("Chrome was not found, so doc/", name, ".pdf was not made. ",
              "Set the CHROME environment variable to its path.",
              call. = FALSE)
      next
    }
    pdf <- file.path(doc_dir, paste0(name, ".pdf"))
    system2(chrome, c("--headless=new", "--disable-gpu",
                      "--no-pdf-header-footer", "--virtual-time-budget=5000",
                      paste0("--print-to-pdf=", shQuote(pdf)),
                      shQuote(paste0("file://", html))),
            stdout = FALSE, stderr = FALSE)
    if (!file.exists(pdf)) warning("doc/", name, ".pdf was not made.",
                                   call. = FALSE)
    next
  }

  # render from a copy inside doc/ so the figure links in the .md come out
  # relative (chart-types_files/...), not absolute
  local_rmd <- file.path(doc_dir, basename(rmd))
  file.copy(rmd, local_rmd, overwrite = TRUE)
  # a table of contents where the vignette itself has one
  front <- rmarkdown::yaml_front_matter(rmd)$output
  has_toc <- is.list(front) &&
    isTRUE(front[["rmarkdown::html_vignette"]]$toc)
  rmarkdown::render(
    local_rmd,
    output_format = rmarkdown::github_document(
      toc = has_toc, html_preview = FALSE
    ),
    quiet         = TRUE
  )
  unlink(local_rmd)
  md <- sub("\\.Rmd$", ".md", local_rmd)
  if (file.exists(md)) repoint_links(md)
}

message("rendered vignettes written to ", doc_dir)
