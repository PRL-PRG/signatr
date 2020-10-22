#!/usr/bin/env Rscript
arguments <- commandArgs(TRUE)

# library(signatr)
library(digest)

source("../R/helper.R")
source("../R/gbov.R")

add1 <- function(x) {x + 1}

if (length(arguments) != 3) {
  print("USAGE: ./run-one.R [package] [function] [GBOV_path]")
  print(arguments)
  stop()
}

# Get pass in just package
# Get loop through all the functions in the package
# Use getnamespace, get0

# Get package name or NULL
package = NULL
if (arguments[1] != "NULL" && arguments[1] != "null") {
  package = basename(arguments[1])
}

# Get function based on package and function
# Install package if the package is not found
# Should be able to assume package has been installed later

func <- get_function(package, arguments[2])
if (is.null(func)) {
  install.packages(package)
  func <- get_function(package, arguments[2])
}

if (is.null(func)) {
  print("Please specify a valid function")
  stop()
}

# Load GBOV
# Assume GBOV exists at the path
GBOV <- load(arguments[3])


## __Main__ ##

# Create global data.frames
calls_record <<- data.frame(call_id = integer(0),
                            source_hash = character(0),
                            value_hash = integer(0),
                            stringsAsFactors = FALSE)

results_record <<- data.frame(call_id = integer(0),
                             result = character(0),
                             stdout = character(0), # constant for now
                             stderr = character(0), # constant for now
                             stringsAsFactors = FALSE)

# Create global call id
call_id <<- 1

run_until_killed(function() { # function to be run
  # Deal with arguments used for function call
  fargs = formals(func)
  fargs_names = names(fargs)
    
  for (name in fargs_names) {
    src_hash = paste(package, arguments[2], name, sep="::")
    value = get_random_value(GBOV)
    val_hash = digest::sha1(value) # Need to make sure this is consistent
    calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, val_hash)
    fargs[name] = value
  }
  
  tryCatch(
  {
    print(paste("*** Call ID:", call_id, "***\n", sep=" "))
    ret = do.call(func, fargs)
    
    # Add the result value to GBOV if it's not in there already
    # so that it can be used in future tests
    # gbov <<- add_value(GBOV, ret)
    
    results_record[nrow(calls_record) + 1, ] <<- c(call_id, ret, "stdout", "stderr")
  }, warning = function(warn) {
    print("WARN")
    results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
  },error = function(err) {
    print("ERR")
    print(as.character(err))
    results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
  })
  
  call_id <<- call_id + 1

}, function() { # function run at death
  dir.create(paste(package, arguments[2], sep = "/"), recursive = TRUE)
  write.csv(calls_record, paste(package, arguments[2], "calls.csv", sep = "/"), row.names = F)
  write.csv(results_record, paste(package, arguments[2], "results.csv", sep = "/"), row.names = F)
})
