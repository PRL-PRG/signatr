#!/usr/bin/env Rscript

start <- Sys.time()
library(record)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 1) {
  message("Usage: merge-dbs.R <dir>")
  q(status=1)
}

run_dir <- args[1]

if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

dirs <- list.dirs(path = run_dir, recursive = TRUE)
dbs <- lapply(which(lapply(dirs, basename) == "db"), function(i) dirs[[i]])
num_dbs <- length(dbs)

cat(sprintf("Merging %s dbs\n\n", num_dbs))

gbov <- paste0(run_dir, "/gbov")
open_db(gbov, create = TRUE)

lapply(dbs, function(db) {
  cat("- merging", db, "\n") 
  merge_db(db)
})

cat(sprintf("%s values are stored in gbov\n\n", size_db()))

close_db()

end <- Sys.time()
round(end - start, digits = 2)
