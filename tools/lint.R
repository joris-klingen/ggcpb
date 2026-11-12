# tools/lint.R ----
#
# Run lintr over the package with the settings in .lintr. The exit status
# is 1 when anything is reported, so CI fails on it.
#
# From the project folder:
#   Rscript tools/lint.R

stopifnot(file.exists("DESCRIPTION"))

lints <- c(
  lintr::lint_package(),
  lintr::lint_dir("tools")
)

if (length(lints)) {
  print(lints)
  cat(length(lints), "lint(s) found.\n")
  if (!interactive()) quit(status = 1)
} else {
  cat("No lints found.\n")
}
