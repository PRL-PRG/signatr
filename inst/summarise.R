#!/usr/bin/env Rscript
options(error = function() { traceback(3); q(status=1) })

summary <- data.frame(package = character(0),
                 instru_time = double(0),
                 trace_time = double(0),
                 record_time = double(0),
                 total = integer(0),
                 unique = integer(0),
                 int = integer(0))

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Missing a path to packages to summarise")
}
run_dir <- args[1]

instru_csv <- list.files(path = run_dir, pattern = "instru-time.csv", recursive = TRUE)
run_csv <- list.files(path = run_dir, pattern = "run.csv", recursive = TRUE)
record_csv <- list.files(path = run_dir, pattern = "record-time.csv", recursive = TRUE)

trace_counts_csv <- list.files(path = run_dir, pattern = "trace-counts.csv", recursive = TRUE)

instru_ls <- lapply(instru_csv, read.csv)
trace_ls <- lapply(run_csv, read.csv)
record_ls <- lapply(record_csv, read.csv)

trace_counts_ls <- lapply(trace_counts_csv, read.csv)



for(i in seq_along(instru_csv)) {
  package <- stringr::str_split(instru_csv[[i]], "/")[[1]][[1]]

  instru_time <- instru_ls[[i]]$toc - instru_ls[[i]]$tic
  trace_time <- dplyr::summarise(trace_ls[[i]], total = sum(time, na.rm=TRUE))$total
  record_time <- record_ls[[i]]$toc - record_ls[[i]]$tic

  total_vals <- trace_counts_ls[[i]]$total
  unique_vals <- trace_counts_ls[[i]]$unique
  int_vals <- trace_counts_ls[[i]]$int

  obs <- data.frame(package=package, instru_time=instru_time, trace_time=trace_time, record_time=record_time,total=total_vals, unique=unique_vals, int=int_vals)

  summary <- rbind(obs, summary)
}

write.csv(summary, "job-summary.csv", row.names=FALSE)
