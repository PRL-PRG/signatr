#!/usr/bin/env Rscript

options(error = function() { traceback(3); q(status=1) })

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  stop("Missing a path to running directory")
}

OUTPUT_FILE <- "combined-meta.csv"

run_dir <- args[1]

files <- list.files(path = run_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

meta <- lapply(files, function(f) read.csv(f))
result <- do.call(rbind, meta)

write.csv(result, OUTPUT_FILE, row.names=FALSE)


