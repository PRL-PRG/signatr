#!/usr/bin/env Rscript
args <- commandArgs(TRUE)

source("../R/helper.R")
source("../R/gbov.R")

# library(signatr)

add1 <- function(x) { return(x + 1) }


if (length(args) != 6) {
  print("USAGE: ./run-fun.R [package_source_path] [function] [GBOV_path] [timeout] [output_path] [call #]")
  stop()
}

package = NULL
if (args[1] != "NULL" && args[1] != "null") {
  package = basename(args[1])
}

f <- get_function(package, args[2])
if (is.null(f) && !is.null(package)) {
  install.packages(package) # Might not be a good idea in actual runs?
  f <- get_function(package, args[2])
}

if (is.null(f)) {
  print("Could not locate specified function")
  stop()
}

GBOV <- load(args[3])
if (is.null(GBOV)) {
  print("Could not locate the Great Book of Values")
  stop()
}

timeout = as.integer(args[4])
if (timeout <= 0) {
  print("Timeout is required to be a positive number.")
  stop()
}

calls = as.integer(args[6])
if (calls < 1) {
  print("Must perform at least 1 call")
  stop()
}

# Create global data.frame calls_record and results_record
calls_record <<- data.frame(call_id = integer(0),
                            source_hash = character(0),
                            value_hash = integer(0),
                            stringsAsFactors = FALSE)

results_record <<- data.frame(call_id = integer(0),
                              result = character(0),
                              stdout = character(0), # constant for now
                              stderr = character(0), # constant for now
                              stringsAsFactors = FALSE)

print("Initalized")

call_id <<- 1
run_until_timeout_or_death(timeout, function() {
  for (i in seq(calls)) {
    # Deal with arguments used for function call
    fargs = formals(f)
    fargs_names = names(fargs)
    
    for (name in fargs_names) {
      src_hash = paste(package, args[2], name, sep="::")
      value = get_value(GBOV)
      val_hash = digest::sha1(value) # Need to make sure this is consistent
      calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, val_hash)
      fargs[name] = value
    }
    
    tryCatch(
      {
        print(paste("*** Call ID:", call_id, "***\n", sep=" "))
        ret = do.call(f, fargs)
        
        # Add the result value to GBOV if it's not in there already
        # so that it can be used in future tests
        # gbov <<- add_value(GBOV, ret)
        
        results_record[nrow(calls_record) + 1, ] <<- c(call_id, ret, "stdout", "stderr")
      }, warning = function(warn) {
        results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
      },error = function(err) {
        results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
      }
    )
    
    call_id <<- call_id + 1
  }},
  function () {
    print("Completed")
    # write.csv2(calls_record, paste(package, args[2], "calls.csv", sep = "::"))
    # write.csv2(results_record, paste(package, args[2], "results.csv", sep = "::"))
    write.csv2(calls_record, paste(args[5], "calls.csv", sep = "::"))
    write.csv2(results_record, paste(args[5], "results.csv", sep = "::"))
  }
)
