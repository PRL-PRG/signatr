#!/usr/bin/env Rscript
arguments <- commandArgs(TRUE)

# library(signatr)

source("../R/helper.R")
source("../R/gbov.R")

if (length(arguments) != 2) {
  print("USAGE: ./run-one.R [package_name] [GBOV_path]")
  stop()
}

package = NULL
if (arguments[1] != "NULL" && arguments[1] != "null") {
  package = basename(arguments[1])
}

if (is.null(package)) {
  print(paste("Invalid package name entered", arguments[1], sep=": "))
  stop()
}

# Assume GBOV exists at the path
GBOV <- load(arguments[2])

# Globals
functions <<- get_all_functions_from(package)
params <<- get_all_formals(functions)

calls_record <<- data.frame(call_id = integer(0),
                            source_hash = character(0),
                            value_hash = integer(0),
                            stringsAsFactors = FALSE)

results_record <<- data.frame(call_id = integer(0),
                              result = character(0),
                              stdout = character(0), # constant for now
                              stderr = character(0), # constant for now
                              stringsAsFactors = FALSE)

call_id <<- 1
circuit <<- 1

# Main
run_until_killed(function() {
  while (TRUE) {
    for (i in seq(length(functions))) {
      if (circuit > 1) {
        for (name in params[[i]]) {
          value = get_random_value(GBOV)
          params[[i]][name] <<- value
        }
      }
      
      counter = 1
      param_names = names(params[[i]])
      if (length(param_names) == 0) {
        src_hash = paste(package, names(functions)[i], "NO_PARAMS", sep="::")
        calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, "NO_VALUE")
      } else {
        for (name in param_names) {
          src_hash = paste(package, names(functions)[i], name, sep="::")
          
          tryCatch({
            ## Janky test for no default value
            if (is.symbol(params[[i]][[name]]) &&
                          params[[i]][[name]] == params[[i]][params[[i]][[name]]]) {
              val_hash = "NO_VALUE"
            } else {
              if (is.null(params[[i]][[name]])) {
                val_hash = "NULL"
              } else {
                val_hash = digest::sha1(params[[i]][[name]])
              }
            }
          }, error = function(err) {
            if (is.null(params[[i]][[name]])) {
              val_hash = "NULL"
            } else
            {
              val_hash = digest::sha1(params[[i]][[name]])
            }
          })
          
          # calls_record[nrow(calls_record) + 1, ] <<- c("WRITE", "WRITE", "WRITE")
          calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, val_hash)
          # calls_record[nrow(calls_record) + 1, ] <<- c("WRITE", "WRITE", "WRITE")
          counter = counter + 1
        }
      }
      
      tryCatch(
        {
          print(paste("*** Call ID:", call_id, "***\n", sep=" "))
          if (names(functions)[i] == "square") {
            stop()
          }
          
          ret = do.call(functions[[i]], as.list(params[[i]]))
            
          # GBOV <<- add_value(GBOV, ret)
          
          results_record[nrow(calls_record) + 1, ] <<- c("RES", "RES", "RES", "RES")
          # results_record[nrow(calls_record) + 1, ] <<- c(call_id, ret, "stdout", "stderr")
          # results_record[nrow(calls_record) + 1, ] <<- c("RES", "RES", "RES", "RES")
        }, warning = function(warn) {
          print("WARN")
          # results_record[nrow(calls_record) + 1, ] <<- c("WARN", "WARN", "WARN", "WARN")
          results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
          # results_record[nrow(calls_record) + 1, ] <<- c("WARN", "WARN", "WARN", "WARN")
        },error = function(err) {
          print("ERR")
          print(as.character(err))
          # results_record[nrow(calls_record) + 1, ] <<- c("ERROR", "ERROR", "ERROR", "ERROR")
          results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
          # results_record[nrow(calls_record) + 1, ] <<- c("ERROR", "ERROR", "ERROR", "ERROR")
          
        })
      
      call_id <<- call_id + 1
    }
    circuit <<- circuit + 1
  }
}, function() {
  dir.create(package)
  write.csv(calls_record, paste(package, "calls.csv", sep = "/"), row.names = F)
  write.csv(results_record, paste(package, "results.csv", sep = "/"), row.names = F)
  save.gbov(GBOV)
})

