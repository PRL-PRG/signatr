#!/usr/bin/env Rscript

library(signatr)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  message("Usage: merge-gbov.R <dir> <file1> [... <fileN>]")
  q(status=1)
}
browser()
run_dir <- args[1]
if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

files <- list.files(path = run_dir, pattern = "values.RDS$", recursive = TRUE)

num_files <- length(files)

cat(sprintf("Merging %s values.RDS files:\n\n", num_files))

gbov <- list()

for (file in files) {
  values <- load_gbov(paste0(run_dir, "/", file))
  gbov <- c(gbov, values)
}

saveRDS(unique(gbov), file = paste0(run_dir, "/", "merged-gbov.RDS"))
