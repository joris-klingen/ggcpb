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
# The session also gets a UTF-8 locale pinned below: CPB figure labels
# are Dutch and carry accented characters, which a C locale silently
# mangles into missing glyphs.
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
  pandoc qpdf
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
#
# stdout is dropped: -qq silences apt's own progress but not dpkg's
# per-package unpack/setup lines, which on a cold container bury the
# summary this script ends with under ~100 kB of noise. Failures still
# surface -- apt writes them to stderr, set -e aborts on a non-zero
# exit, and the verification block below re-checks every hard dependency.
sudo apt-get update -qq || true
sudo apt-get install -y -qq --no-install-recommends "${APT_PKGS[@]}" >/dev/null

# Pin a UTF-8 locale. The image comes up in the C locale, where R reads
# each accented character ("reele", "geindexeerd" with their diaereses)
# as two undefined bytes and every graphics device draws them as
# missing glyphs -- with no warning, so the figure looks finished and
# reads as mojibake. tools/render_vignettes.R refuses to run without
# this; plots drawn by hand would silently come out wrong.
for loc in C.UTF-8 en_US.UTF-8; do
  if locale -a 2>/dev/null | tr -d '-' | grep -qix "$(echo "$loc" | tr -d '-')"; then
    export LANG="$loc" LC_ALL="$loc"
    break
  fi
done
if [ -z "${LC_ALL:-}" ]; then
  sudo locale-gen C.UTF-8 >/dev/null 2>&1 || true
  export LANG=C.UTF-8 LC_ALL=C.UTF-8
fi
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  echo "export LANG=\"$LANG\"" >> "$CLAUDE_ENV_FILE"
  echo "export LC_ALL=\"$LC_ALL\"" >> "$CLAUDE_ENV_FILE"
fi

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
  # roxygen2 must match what man/ was generated with, or devtools
  # regenerates every .Rd and tools/check_docs_fresh.R reports drift.
  # Read it from DESCRIPTION rather than pinning a literal here, so
  # this cannot silently fall behind the package again.
  desc_path <- file.path(Sys.getenv("CLAUDE_PROJECT_DIR", "."), "DESCRIPTION")
  rox <- tryCatch({
    d <- read.dcf(desc_path)
    fld <- intersect(c("Config/roxygen2/version", "RoxygenNote"), colnames(d))
    if (length(fld)) unname(d[1, fld[[1]]]) else NULL
  }, error = function(e) NULL)

  wanted <- list(
    ggplot2  = "3.5.0",   # the floor ggcpb declares in DESCRIPTION
    sysfonts = NULL,      # no Ubuntu build exists for these two
    showtext = NULL
  )
  missing <- names(wanted)[vapply(names(wanted),
                                  function(p) needed(p, wanted[[p]]),
                                  logical(1))]
  if (length(missing)) install.packages(missing)

  # roxygen2 is pinned to the *exact* recorded version, not a floor: it
  # stamps its own version into DESCRIPTION and rewrites every .Rd, so a
  # newer one turns `devtools::document()` into a whole-tree diff.
  if (!is.null(rox) && !identical(as.character(utils::packageVersion("roxygen2")), rox)) {
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_version("roxygen2", version = rox, upgrade = "never")
  }
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
  ctype <- Sys.getlocale("LC_CTYPE")
  if (!grepl("UTF-?8", ctype, ignore.case = TRUE)) {
    stop("LC_CTYPE is \"", ctype, "\", not UTF-8: accented characters in ",
         "figures would be drawn as missing glyphs.")
  }
  cat("R deps ready:", R.version.string, "with ggplot2", format(gg),
      "| roxygen2", format(utils::packageVersion("roxygen2")),
      "| LC_CTYPE", ctype, "\n")
'
