#!/usr/bin/env Rscript

library(signatr)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  message("Usage: merge-rds.R <dir> <file1> [... <fileN>]")
  q(status=1)
}

run_dir <- args[1]
if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

pattern <- args[2]

files <- list.files(path = run_dir, pattern = paste0(pattern, ".RDS"), recursive = TRUE)

howmany <- length(files)

cat(sprintf("Merging %s %s.RDS files:\n\n", howmany, pattern))

merged_RDS <- character()

for (file in files) {
  perfile <- readRDS(paste0(run_dir, "/", file))
  merged_RDS <- c(perfile, merged_RDS)
}

saveRDS(merged_RDS, file = paste0(run_dir, "/", pattern, ".RDS"))


## merged_RDS <- rep(0, howmany)

## for (i in seq_along(files)) {
##   file <- load_gbov(paste0(run_dir,"/", files[[i]]))
##   merged_RDS[[i]] <- file
## }


## for (i in seq_along(merged_RDS)) {
##   perfile <- merged_RDS[[i]][[1]]
##   i <- length(result)
##   for(j in seq_along(perfile)) {
##     result[[j+i]] <- perfile[[j]]
##   }
## }

## saveRDS(gbov, file = paste0(run_dir, "/", "gbov.RDS"))
