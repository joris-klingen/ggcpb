# run_import.R
#
# This script is started by run_import.bat on Windows or
# run_import.command on Mac. It is not normally opened by hand.
#
# It reads data.csv and params.csv from its own folder, builds every
# figure described in params.csv, and saves each one as a PNG file in
# a folder named generated, created next to this script if it does
# not exist yet.

# Rscript does not set the working directory to the folder the script
# lives in, unlike opening a file in RStudio. The folder is instead
# found from R's own startup arguments, which record the full path
# this script was started with.
find_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  getwd()
}

script_dir <- find_script_dir()
data_csv <- file.path(script_dir, "data.csv")
params_csv <- file.path(script_dir, "params.csv")
out_dir <- file.path(script_dir, "generated")

cat("Looking for data.csv and params.csv in:\n")
cat(script_dir, "\n\n")

result <- tryCatch({
  if (!requireNamespace("ggcpb", quietly = TRUE)) {
    stop("The ggcpb package is not installed. Install it in R first, ",
      "then run this script again.")
  }
  if (!file.exists(data_csv)) {
    stop("data.csv was not found in this folder.")
  }
  if (!file.exists(params_csv)) {
    stop("params.csv was not found in this folder.")
  }

  figs <- ggcpb::import_csv(data_csv, params_csv)
  if (ggplot2::is.ggplot(figs)) {
    figs <- list(figuur_1 = figs)
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  for (nm in names(figs)) {
    file_name <- paste0(gsub("[^A-Za-z0-9_-]+", "_", nm), ".png")
    ggcpb::save_cpb(file.path(out_dir, file_name), figs[[nm]], page = "half")
  }

  cat("\nDone. ", length(figs), " figure(s) saved in the generated folder.\n", sep = "")
  "ok"
}, error = function(e) {
  cat("Something went wrong and no figures were saved.\n\n")
  cat(conditionMessage(e), "\n")
  "error"
})

invisible(result)
