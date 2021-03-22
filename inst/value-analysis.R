#!/usr/bin/env Rscript

library(signatr)
library(pryr)

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
count_files <- list.files(path = run_dir, pattern = "counts.RDS", recursive = TRUE)

tictoc::tic("Collecting started ...\n\n")

gbov <- new.env(parent=emptyenv())
meta <- data.frame(value_hash = character(), source_hash = character(), count = integer(), type = character(), size = numeric())

for (i in seq_along(val_files)) {
  val_list <- readRDS(paste0(run_dir, "/", val_files[[i]])) # already sha1 unique
  meta_df <- readRDS(paste0(run_dir, "/", count_files[[i]]))

  if(length(val_list) == 0) {
    next
  }

  for(val in val_list) {
    # combining vals from different programs
    if(!exists(val$hash, envir = gbov)) {
      assign(val$hash, val$value, envir=gbov)
    }
    bytes <- as.numeric(object_size(val$value))
    meta_df[meta_df$value_hash == val$hash, "type"] <- val$type
    meta_df[meta_df$value_hash == val$hash, "size"] <- bytes
    ## meta_df[meta_df$value_hash == val$hash, "source_hash"] <- sub(":.*", "", meta_df$source_hash[[1]])
    meta <- rbind(meta, meta_df[meta_df$value_hash == val$hash,])
  }
}

print(paste0("total of ", length(as.list(gbov)), " unique values are collected"))

saveRDS(gbov, file = paste0(run_dir, "/gbov.RDS"))
saveRDS(meta, file = paste0(run_dir, "/meta.RDS"))

tictoc::toc("Collecting done ...")
