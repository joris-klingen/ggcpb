# run_import.R
#
# This script is started by run_import.bat on Windows or
# run_import.command on Mac. It is not normally opened by hand.
#
# It reads params.csv from its own folder, builds every figure described
# there from the datasets in the data folder, and saves each one as a PNG
# file in a folder named generated. A PNG that already exists is replaced.

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

cat("Looking for params.csv and the data folder in:\n")
cat(script_dir, "\n\n")

result <- tryCatch({
  if (!requireNamespace("ggcpb", quietly = TRUE)) {
    stop("The ggcpb package is not installed. Install it in R first, ",
      "then run this script again.")
  }
  ggcpb::cpb_import_run(script_dir)
  "ok"
}, error = function(e) {
  cat("Something went wrong and no figures were saved.\n\n")
  cat(conditionMessage(e), "\n")
  "error"
})

invisible(result)
