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

gbov_index <- 1

for (i in seq_along(values_files)) {
  values_list <- readRDS(paste0(run_dir, "/", values_files[[i]]))
  if(length(values_list) == 0) {
    next
  }

  unique_values <- unique(values_list)

  values_sources_df <- readRDS(paste0(run_dir, "/", values_sources_files[[i]]))
  sources_df <- readRDS(paste0(run_dir, "/", sources_files[[i]]))

  for(j in seq_along(unique_values)) {
    hash <- unique_values[[j]][[1]]
    value <- unique_values[[j]][[3]]
    type <- unique_values[[j]][[2]]

    values_sources_df[values_sources_df$value_hash == hash, "type"] <- type

    duplicate_found <- find_duplicates(as.list(gbov), value, type)
    if(is.null(duplicate_found)) {
      next()
    } else if(find_duplicates(as.list(gbov), value, type)) {
      next()
    } else {
      assign(toString(gbov_index), list(hash, value), envir=gbov)
      values_sources_df[values_sources_df$value_hash == hash, "index"] <- gbov_index
      assign("gbov_index", gbov_index + 1, envir=.GlobalEnv)
    }
  }

  joined <- left_join(values_sources_df, sources_df, by = "source_hash")

  meta <- rbind(meta, joined)
}

cat(sprintf("merging done.\n\n"))
tictoc::toc()

saveRDS(meta, file = paste0(run_dir, "/", "merged-meta.RDS"))
saveRDS(gbov, file = paste0(run_dir, "/", "merged-gbov.RDS"))
