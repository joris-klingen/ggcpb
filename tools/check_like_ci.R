# tools/check_like_ci.R ----
#
# Runs what CI runs (.github/workflows/R-CMD-check.yaml) on this machine:
#   current  R CMD check with your installed ggplot2; warnings fail
#   ggplot2  the same against ggplot2 3.5.2, in a temporary library
#
# From the project folder:
#   Rscript tools/check_like_ci.R             # both
#   Rscript tools/check_like_ci.R ggplot2     # only the ggplot2 3.5.2 run
# or in R: source("tools/check_like_ci.R")
#
# Needs rcmdcheck and remotes (installed when missing). The exit status
# is 1 when anything fails.

steps <- commandArgs(trailingOnly = TRUE)
if (!length(steps)) steps <- c("current", "ggplot2")
stopifnot(file.exists("DESCRIPTION"), all(steps %in% c("current", "ggplot2")))

# Rscript has no CRAN mirror unless one is set
if (!length(getOption("repos")) ||
      identical(unname(getOption("repos")["CRAN"]), "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

for (pkg in c("rcmdcheck", "remotes")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

cat("R", as.character(getRversion()), "on", R.version$platform,
    "| ggplot2", as.character(packageVersion("ggplot2")), "\n")

# without pandoc the vignettes cannot be built; CI has it
pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
  rmarkdown::pandoc_available()
if (!pandoc) {
  message("pandoc not found: vignettes are skipped in the checks below")
}

outcome <- list()

check <- function(label, libpath = .libPaths()) {
  cat("\n=== ", label, " ===\n", sep = "")
  res <- rcmdcheck::rcmdcheck(
    ".",
    args = c("--no-manual", "--as-cran", if (!pandoc) "--ignore-vignettes"),
    build_args = c("--no-manual", if (!pandoc) "--no-build-vignettes"),
    error_on = "never", libpath = libpath
  )
  list(
    ok = !length(res$errors) && !length(res$warnings),
    detail = sprintf("%d errors, %d warnings, %d notes",
                     length(res$errors), length(res$warnings),
                     length(res$notes))
  )
}

if ("current" %in% steps) {
  outcome[["check, installed ggplot2"]] <- check("check, installed ggplot2")
}

if ("ggplot2" %in% steps) {
  lib <- file.path(tempdir(), "ggplot2-3.5.2")
  dir.create(lib, showWarnings = FALSE)
  remotes::install_version("ggplot2", "3.5.2", lib = lib, upgrade = "never",
                           dependencies = FALSE, quiet = TRUE)
  cat("using ggplot2", as.character(packageVersion("ggplot2",
                                                   lib.loc = lib)), "\n")
  outcome[["check, ggplot2 3.5.2"]] <- check("check, ggplot2 3.5.2",
                                             libpath = c(lib, .libPaths()))
}

cat("\n=== summary ===\n")
for (nm in names(outcome)) {
  cat(sprintf("%-28s %-4s %s\n", nm, if (outcome[[nm]]$ok) "OK" else "FAIL",
              outcome[[nm]]$detail))
}
failed <- !all(vapply(outcome, function(x) x$ok, TRUE))
if (failed && !interactive()) quit(status = 1)
