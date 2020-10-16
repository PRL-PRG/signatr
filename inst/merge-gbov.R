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

gbov <- character()

for (file in files) {
  values <- load(file)
  gbov <- append(gbov, values)
}

## for (file in files) {
##   values <- readRDS(file)  
##   class(values) <- c("gbov", class(values))
##   gbov <- append(values, gbov)
## }

saveRDS(gbov, file = paste0(run_dir, "/", "gbov.RDS"))
