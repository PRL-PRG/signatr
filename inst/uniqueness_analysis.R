#!/usr/bin/env Rscript

library(signatr)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  message("Usage: record-vals.R <dir> [... <fileN>]")
  q(status=1)
}

run_dir <- args[1]

if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

val_files <- list.files(path = run_dir, pattern = "values.RDS", recursive = TRUE)
## src_files <- list.files(path = run_dir, pattern = "sources.RDS", recursive = TRUE)
count_files <- list.files(path = run_dir, pattern = "count.RDS", recursive = TRUE)

print(length(val_files))
print(length(count_files))

tictoc::tic("Collecting started ...\n\n")

gbov <- new.env(parent=emptyenv())
meta <- data.frame(value_hash = character(), source_hash = character(), count = integer(), type = character())

for (i in seq_along(val_files)) {
  val_list <- readRDS(paste0(run_dir, "/", val_files[[i]]))
  meta_df <- readRDS(paste0(run_dir, "/", count_files[[i]]))

  if(length(val_list) == 0) {
    next
  }

  for(val in val_list) {
    hash <- val$hash

    if(!exists(hash, envir = gbov)) {
      assign(hash, val$value, envir=gbov)
      meta_df[meta_df$value_hash == hash, "type"] <- val$type
      ## meta_df[meta_df$value_hash == hash, "source_hash"] <- sub(":.*", "", meta_df$source_hash[[1]])
      meta <- rbind(meta, meta_df[meta_df$value_hash == hash,])
    } else {
      meta[meta$value_hash == hash, "count"] <- meta[meta$value_hash == hash, "count"] + 1
    }
  }
}

print(paste0("total of ", length(as.list(gbov)), " unique values are collected"))

saveRDS(gbov, file = paste0(run_dir, "/gbov.RDS"))
saveRDS(meta, file = paste0(run_dir, "/meta.RDS"))

tictoc::toc("Collecting done ...")
