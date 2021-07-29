#!/usr/bin/env Rscript

options(error = function() { traceback(3); q(status=1) })

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  stop("Missing a path to the running directory")
}

OUTPUT_FILE1 <- "combined-meta.csv"
OUTPUT_FILE2 <- "combined-data.RDS"

run_dir <- args[1]

if(!dir.exists(run_dir)) {
  stop(paste0(run_dir, " does not exist"))
}

meta_files <- list.files(path = run_dir, pattern = "*_metadata\\.csv$", recursive = TRUE, full.names = TRUE)
data_files <- list.files(path = run_dir, pattern = "*_data\\.RDS$", recursive = TRUE, full.names = TRUE)

meta <- lapply(meta_files, function(f) read.csv(f))
result1 <- do.call(rbind, meta)

data <- lapply(data_files, function(f) readRDS(f))
result2 <- do.call(rbind, data)

write.csv(result1, OUTPUT_FILE1, row.names=FALSE)
saveRDS(result2, file = OUTPUT_FILE2)


