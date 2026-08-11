#!/bin/bash
# Installs R plus the system and R package dependencies ggcpb needs, so
# that tests, R CMD check and the vignette/pixel scripts can run in a
# Claude Code on the web session.
#
# R itself and the compiled toolchain come from the Ubuntu archive,
# which is fast and always reachable. The R packages come from CRAN,
# because Ubuntu 24.04 ships ggplot2 3.4.4 while ggcpb needs >= 3.5.0
# (R/scales.R uses the post-3.5 discrete_scale() signature, and
# theme.R/map.R use theme elements that 3.4 does not have).
#
# CRAN, not Posit Package Manager: P3M serves its index from
# packagemanager.posit.co but redirects the actual downloads to
# rspm-sync.rstudio.com, which the environment's network policy does
# not allow, so every install fails at the download step. Source
# installs from cloud.r-project.org are slower but complete.
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
  # the pure-R dependency tree, which apt resolves far faster than a
  # source install. ggplot2 is deliberately NOT taken from apt: its
  # 3.4.4 build is too old, and CRAN's newer one below shadows it from
  # the user library.
  r-cran-scales r-cran-systemfonts r-cran-rlang r-cran-ragg
  r-cran-testthat r-cran-png r-cran-withr r-cran-knitr r-cran-rmarkdown
  r-cran-dplyr r-cran-tidyr r-cran-data.table
  r-cran-devtools r-cran-rcmdcheck
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

# The user library comes first on .libPaths(), so what is installed
# here shadows any older apt build of the same package.
Rscript -e '
  options(repos = c(CRAN = "https://cloud.r-project.org"),
          Ncpus = max(1L, parallel::detectCores()))
  needed <- function(pkg, min_version = NULL) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(TRUE)
    !is.null(min_version) && utils::packageVersion(pkg) < min_version
  }
  wanted <- list(
    ggplot2  = "3.5.0",   # the floor ggcpb declares in DESCRIPTION
    sysfonts = NULL,      # no Ubuntu build exists for these two
    showtext = NULL,
    roxygen2 = "7.3.3"    # matches RoxygenNote, so docs regenerate clean
  )
  missing <- names(wanted)[vapply(names(wanted),
                                  function(p) needed(p, wanted[[p]]),
                                  logical(1))]
  if (length(missing)) install.packages(missing)
'

# Fail loudly if a hard dependency did not make it in.
Rscript -e '
  required <- c("ggplot2", "scales", "systemfonts", "rlang", "ragg", "testthat")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("missing required packages: ", paste(missing, collapse = ", "))
  gg <- utils::packageVersion("ggplot2")
  if (gg < "3.5.0") {
    stop("ggplot2 ", gg, " is installed but ggcpb needs >= 3.5.0; ",
         "check that cloud.r-project.org is reachable from this environment.")
  }
  cat("R deps ready:", R.version.string, "with ggplot2", format(gg), "\n")
'
