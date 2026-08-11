#!/bin/bash
# Installs R plus the system and R package dependencies ggcpb needs, so
# that tests, R CMD check and the vignette/pixel scripts can run in a
# Claude Code on the web session.
#
# Packages come from the Ubuntu archive (r-cran-*) rather than CRAN:
# the remote environment's network policy blocks CRAN/P3M, and apt is
# reachable. Two Suggests-level packages have no Ubuntu build
# (sysfonts, showtext); they are installed from CRAN only when a mirror
# happens to be reachable, and skipped otherwise. ggcpb degrades
# gracefully without them -- R/fonts.R falls back to the systemfonts
# path.
set -euo pipefail

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

export DEBIAN_FRONTEND=noninteractive

APT_PKGS=(
  r-base-core r-base-dev r-recommended
  pandoc
  libcurl4-openssl-dev libssl-dev libxml2-dev
  libpng-dev libtiff5-dev libjpeg-dev
  libfreetype6-dev libharfbuzz-dev libfribidi-dev libfontconfig1-dev
  r-cran-ggplot2 r-cran-scales r-cran-systemfonts r-cran-rlang r-cran-ragg
  r-cran-testthat r-cran-png r-cran-withr r-cran-knitr r-cran-rmarkdown
  r-cran-dplyr r-cran-tidyr r-cran-data.table
  r-cran-devtools r-cran-roxygen2 r-cran-rcmdcheck
)

# The image carries third-party PPAs that can fail to refresh; their
# errors are not fatal for the Ubuntu archive packages we need.
sudo apt-get update -qq || true
sudo apt-get install -y --no-install-recommends "${APT_PKGS[@]}"

# User library for anything installed from source, kept out of the
# system tree so it does not need root.
R_LIBS_USER="${R_LIBS_USER:-$HOME/R/library}"
mkdir -p "$R_LIBS_USER"
export R_LIBS_USER
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export R_LIBS_USER=\"$R_LIBS_USER\"" >> "$CLAUDE_ENV_FILE"
fi

Rscript -e '
  optional <- c("sysfonts", "showtext")
  missing <- optional[!vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    reachable <- tryCatch({
      con <- url("https://cloud.r-project.org/src/contrib/PACKAGES.gz", open = "rb")
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (reachable) {
      install.packages(missing, repos = "https://cloud.r-project.org")
    } else {
      message(
        "note: no CRAN mirror reachable; skipping optional packages: ",
        paste(missing, collapse = ", ")
      )
    }
  }
'

# Fail loudly if a hard dependency did not make it in.
Rscript -e '
  required <- c("ggplot2", "scales", "systemfonts", "rlang", "ragg", "testthat")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("missing required packages: ", paste(missing, collapse = ", "))
  cat("R deps ready:", R.version.string, "\n")
'
