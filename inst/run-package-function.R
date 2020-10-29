#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

library(stringr)
library(signatr)
library(instrumentr)
library(R.utils)
library(digest)


if (length(args) != 3) {
  print("USAGE: ./run-package-function.R [package_path] [GBOV_path] [number-of-run]")
  stop()
}

package_path <- args[1]
package <- basename(package_path)

functions <- ls(sprintf("package:%s", package))

# Assume GBOV exists at the path
gbov_path <- args[2]
GBOV <- load_gbov(gbov_path)

thismany <- as.numeric(args[3])

calls_record <- data.frame(call_id = integer(0),
                            source_hash = character(0),
                            value_hash = integer(0),
                            stringsAsFactors = FALSE)

results_record <- data.frame(call_id = integer(0),
                              result = character(0),
                              stdout = character(0), # constant for now
                              stderr = character(0), # constant for now
                              stringsAsFactors = FALSE)

call_id <- 1
counter <- thismany

for (f in functions) {
  fun = signatr::get_function(package, f)
  if (is.null(fun)) next
  params = formals(fun)
  param_names = names(params)

  repeat {
    if(counter == 0) break

    counter <- counter - 1
    # assign random arguments from GBOV to each parameter
    for(name in param_names) {
      arg = get_random_value(GBOV)
      # cannot coerce type 'closure' to vector of type 'list'
      if(typeof(arg) == "closure" || length(arg) == 0) {
        arg = list(1,2,3,4)
      }
      params[name] = arg
    }

    if(length(param_names) == 0) {
      src_hash = paste(package, f, "NO_PARAMS", sep="::")
      calls_record[nrow(calls_record)+1,] <- c(call_id, src_hash, "NO_VALUE")
    } else {
      for(name in param_names) {
        src_hash = paste(package, f, name, sep="::")
        val_hash = sha1(params[[name]])

        calls_record[nrow(calls_record)+1,] <- c(call_id, src_hash, val_hash)
      }
    }

    tryCatch({
      result = withTimeout(do.call(fun, as.list(params)), timeout=3.1)
      results_record[nrow(results_record)+1,] <- c(call_id, result, "stdout", "stderr")},
      ## TimeoutException = function(ex) {"TimedOut"},
      ## warning = function(warn) {
      ##   print(call_id)
      ##   print(as.character(warn))
      ##   results_record[nrow(results_record)+1,] <<- c(call_id, as.character(warn), "stdout", "stderr")},
      error = function(err) {
        print(as.character(err))
        results_record[nrow(results_record)+1,] <<- c(call_id, as.character(err), "stdout", "stderr")})

    call_id <- call_id + 1
  }
  counter <- thismany
}

if(!dir.exists(package)) {
  dir.create(package)
}
write.csv(calls_record, paste(package, "calls.csv", sep = "/"), row.names = FALSE)
write.csv(results_record, paste(package, "results.csv", sep = "/"), row.names = FALSE)
## save.gbov(GBOV, dir)
