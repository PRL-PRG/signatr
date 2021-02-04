#!/usr/bin/env Rscript

library(signatr)
library(dplyr)

## args <- "../test/experiments/simple/reshape2"

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  message("Usage: merge-gbov.R <dir> [... <fileN>]")
  q(status=1)
}

run_dir <- args[1]
if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

values_files <- list.files(path = run_dir, pattern = "values.RDS", recursive = TRUE)
sources_files <- list.files(path = run_dir, pattern = "sources.RDS", recursive = TRUE)
values_sources_files <- list.files(path = run_dir, pattern = "counts.RDS", recursive = TRUE)


tictoc::tic("merging started")
cat(sprintf("merging %s files started ...\n\n", length(values_files)))


gbov <- new.env(parent=emptyenv())
meta <- data.frame()

for (i in seq_along(values_files)) {
  values_list <- readRDS(paste0(run_dir, "/", values_files[[i]]))
  if(length(values_list) == 0) {
    next
  }

  values_sources_df <- readRDS(paste0(run_dir, "/", values_sources_files[[i]]))
  sources_df <- readRDS(paste0(run_dir, "/", sources_files[[i]]))

  for(value in values_list) {
    hash <- value[[1]]
    val <- value[[3]]
    type <- value[[2]]

    values_sources_df[values_sources_df$value_hash == hash, "type"] <- type


    if(length(gbov) == 0) {
      assign(hash, val, envir=gbov)
    } else {
      duplicate <- which(unlist(lapply(as.list(gbov), function(x) identical(x, val))))
      if(length(duplicate) == 0) { 
        assign(hash, val, envir=gbov)
      } else {
        duplicate_hash <- attr(duplicate, "names")
        values_sources_df[values_sources_df$value_hash == hash, "value_hash"] <- duplicate_hash
      }
    }
  }
  
  joined <- left_join(values_sources_df, sources_df, by = "source_hash")

  meta <- rbind(meta, joined)
}

cat(sprintf("merging done.\n\n"))
tictoc::toc()

saveRDS(meta, file = paste0(run_dir, "/", "merged-meta.RDS"))
saveRDS(gbov, file = paste0(run_dir, "/", "merged-gbov.RDS"))
