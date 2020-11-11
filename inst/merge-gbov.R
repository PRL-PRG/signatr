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

howmany <- length(files)

cat(sprintf("Merging %s values.RDS files:\n\n", howmany))

gbov <- list()

# ticktocking to measure time spent on appending and merging
tictoc::tic("appending:")
print("appending started ...")

for (file in files) {
  values <- load_gbov(paste0(run_dir, "/", file))
  gbov <- c(gbov, values)
}

print("appending done.")
tictoc::toc()

tictoc::tic("removing:")
print("removing dupicates ...")

saveRDS(unique(gbov), file = paste0(run_dir, "/", "gbov.RDS"))

print("removing done.")
tictoc::toc()
