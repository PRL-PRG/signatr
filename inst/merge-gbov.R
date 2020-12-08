#!/usr/bin/env Rscript

library(signatr)
library(dplyr)

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  message("Usage: merge-gbov.R <dir> [... <fileN>]")
  q(status=1)
}

run_dir <- args[1]
if (!dir.exists(run_dir)) {
  stop(run_dir, ": no such a directory")
}

values <- list.files(path = run_dir, pattern = "values.RDS", recursive = TRUE)
sources <- list.files(path = run_dir, pattern = "sources.RDS", recursive = TRUE)
values_sources <- list.files(path = run_dir, pattern = "counts.RDS", recursive = TRUE)


tictoc::tic("merging started")
cat(sprintf("merging %s files started ...\n\n", length(values)))

## joined <- readRDS(paste0(run_dir, "/", values_sources[[1]]))
## gbov <- readRDS(paste0(run_dir, "/", values[[1]]))
## meta <- data.frame()

gbov_df <- data.frame(character(), character(), charater(), stringsAsFactors = FALSE)
colnames(gbov_df) <- c("value_hash", "type", "raw_value")

joined <- readRDS(paste0(run_dir, "/", values_sources[[1]]))
meta <- data.frame()

for (i in seq_along(values)) {
  values_df <- readRDS(paste0(run_dir, "/", values[[i]]))
  if (nrow(value) == 0) {
    next 
  }
  gbov_df <- full_join(gbov_df, values_df, by = "value_hash") %>% mutate(type = coalesce(type.x, type.y)) %>% select(-type.x, -type.y)

  gbov_df$raw_value.x[gbov_df$raw_value.x == 'NULL'] <- NA
  gbov_df$raw_value.y[gbov_df$raw_value.y == 'NULL'] <- NA

  gbov_df <- gbov_df %>% mutate(raw_value = coalesce(raw_value.x, raw_value.y)) %>% select(-raw_value.x, -raw_value.y)

  value_source <- readRDS(paste0(run_dir, "/", values_sources[[i]]))
  source <- readRDS(paste0(run_dir, "/", sources[[i]]))
  joined <- left_join(value_source, source, by = "source_hash")

  meta <- rbind(meta, joined)
}

cat(sprintf("merging done.\n\n"))
tictoc::toc()

saveRDS(meta, file = paste0(run_dir, "/", "merged-meta.RDS"))
saveRDS(gbov_df, file = paste0(run_dir, "/", "merged-gbov.RDS"))


#################################################################
## merged <- load_gbov(paste0(run_dir, "/", files_v[[1]]))

## for (v in files_v[-1]) {
##   gbov <- load_gbov(paste0(run_dir, "/", v))
##   if(length(gbov) == 0) {
##     next
##   }

##   merged <- full_join(merged, gbov, by = "value_hash") %>% mutate(type = coalesce(type.x, type.y)) %>% select(-type.x, -type.y)
##   # fill empty cells with NA for coalescing
##   # joined[joined == 'NULL'] <- NA is not good because of NULL type
##   merged$value.x[merged$value.x == 'NULL'] <- NA
##   merged$value.y[merged$value.y == 'NULL'] <- NA

##   merged <- mutate(value = coalesce(value.x, value.y)) %>% select(-value.x, -value.y)
## }

## print("merging values done.")
## tictoc::toc()

## saveRDS(joined, file = paste0(run_dir, "/", "merged-gbov.RDS"))



