# check_docs_fresh.R ----
#
# Nothing else catches doc/*.md drifting from vignettes/*.Rmd -- it
# takes an explicit tools/render_vignettes.R run to regenerate them,
# and this session found real cases of forgetting to (doc/*.md stale
# by 10 days and several commits; a vignette section whose figures
# were on disk but never committed at all). Run before committing a
# vignette change:
#
#   Rscript tools/check_docs_fresh.R
#
# Re-renders every vignette to a throwaway directory (the same way
# tools/render_vignettes.R renders to doc/) and compares the resulting
# .md text against what's committed in doc/, plus checks that every
# figure the .md references actually exists in doc/. Exits non-zero,
# printing which vignette(s) are stale, if anything differs -- the fix
# is always the same: `Rscript tools/render_vignettes.R`, review the
# diff, commit doc/ alongside the vignette change.
#
# Does not byte-compare the figure PNGs themselves: two renders of the
# same chunk are not guaranteed pixel-identical (font hinting/
# antialiasing can vary by run), so that would be a source of false
# positives, not a real drift signal. The .md text (source code and
# prose, which is deterministic) plus "does the figure exist at all"
# is the actual freshness signal that matters here.

pkg_root <- normalizePath(file.path(dirname(sub(
  "^--file=", "",
  grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
)), ".."))

devtools::load_all(pkg_root, quiet = TRUE)

if (!ggcpb:::cpb_utf8_locale()) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "nl_NL.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_CTYPE", loc)) != "") break
  }
}
if (!ggcpb:::cpb_utf8_locale()) {
  stop("LC_CTYPE is '", Sys.getlocale("LC_CTYPE"), "', which cannot ",
       "represent the accented characters in the figures, and no UTF-8 ",
       "locale is available to switch to.", call. = FALSE)
}

doc_dir <- file.path(pkg_root, "doc")
tmp_dir <- tempfile("ggcpb_doc_check_")
dir.create(tmp_dir)
on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

stale <- character(0)

for (rmd in list.files(file.path(pkg_root, "vignettes"),
                       pattern = "\\.Rmd$", full.names = TRUE)) {
  name <- sub("\\.Rmd$", "", basename(rmd))
  message("checking ", name)

  local_rmd <- file.path(tmp_dir, basename(rmd))
  file.copy(rmd, local_rmd, overwrite = TRUE)
  rmarkdown::render(
    local_rmd,
    output_format = rmarkdown::github_document(html_preview = FALSE),
    quiet         = TRUE
  )
  unlink(local_rmd)

  new_md <- file.path(tmp_dir, paste0(name, ".md"))
  old_md <- file.path(doc_dir, paste0(name, ".md"))
  if (!file.exists(new_md)) next # nothing to compare (shouldn't happen)

  txt <- readLines(new_md, warn = FALSE)
  writeLines(gsub("(\\]\\([^):]*)\\.html\\)", "\\1.md)", txt), new_md)

  if (!file.exists(old_md)) {
    stale <- c(stale, paste0(name, ": doc/", name, ".md does not exist yet"))
    next
  }
  if (!identical(readLines(new_md, warn = FALSE), readLines(old_md, warn = FALSE))) {
    stale <- c(stale, paste0(name, ": doc/", name, ".md text differs from a fresh render"))
    next
  }

  # every figure the (now known-matching) .md references must actually
  # exist in doc/ -- catches a figure generated on disk but never
  # committed, which the text diff above can't see since the src line
  # that references it is identical either way
  refs <- regmatches(txt, regexpr('(?<=src=")[^"]+\\.png', txt, perl = TRUE))
  refs <- refs[nzchar(refs)]
  missing <- refs[!file.exists(file.path(doc_dir, refs))]
  if (length(missing)) {
    stale <- c(stale, paste0(
      name, ": referenced but not committed: ", paste(missing, collapse = ", ")
    ))
  }
}

if (length(stale)) {
  message("\ndoc/ is stale:\n  - ", paste(stale, collapse = "\n  - "))
  message("\nFix: Rscript tools/render_vignettes.R, review the diff, commit doc/.")
  quit(status = 1)
}

message("doc/ matches a fresh render of every vignette.")
