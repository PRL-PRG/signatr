#!/usr/bin/env Rscript
arguments <- commandArgs(TRUE)

library(signatr)

if (length(arguments) != 3) {
  print("USAGE: ./run-fun.R [package_source_path] [function] [GBOV_path]")
  stop()
}

# Get package name or NULL
package = NULL
if (arguments[1] != "NULL" && arguments[1] != "null") {
  package = basename(args[1])
}

# Get function based on package and function
# Assume the package has already been install
if (is.null(package)) { # Function is in the environment
  func <- get(arguments[2])
} else {
  ns <- getNamespace(package)
  func <- get0(arguments[2], envir=ns, mode="function", isnotfound=NULL) 
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
                             stringAsFactors = FALSE) 

# Create global call id
call_id <<- 1

run_until_killed(function() { # function to be run
  # Deal with arguments used for function call
  fargs = formals(func)
  fargs_names = names(fargs)
    
  for (name in fargs_names) {
    src_hash = paste(arguments[1], arguments[2], names(arg), sep="::")
    value = get_value(GBOV)
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
    gbov <<- add_value(gbov, ret)
    
    result_record[nrow(calls_record) + 1, ] <<- c(call_id, ret, "stdout", "stderr")
  }, warning = function(warn) {
    result_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
  },error = function(err) {
    result_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
  })
  
  call_id <<- call_id + 1
}, function() { # function run at death
  write.csv2(calls_record, paste(arguments[1], arguments[2], "calls.csv", sep = "_"))
  write.csv2(results_record, paste(arguments[1], arguments[2], "results.csv", sep = "_"))
})
