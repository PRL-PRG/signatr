#!/usr/bin/env Rscript
arguments <- commandArgs(TRUE)

library(signatr)

add1 <- function(x) {x + 1}

if (length(arguments) != 4) {
  print("USAGE: ./run-one.R [package] [function] [GBOV_path] [joker_file]")
  print(arguments)
  stop()
}

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
  func <- signatr::get_function(package, arguments[2])
}

if (is.null(func)) {
  print("Please specify a valid function")
  stop()
}

# Load GBOV
# Assume GBOV exists at the path
GBOV <- load_gbov(arguments[3])


## __Main__ ##

# Create global data.frames
calls_record <<- data.frame(call_id = integer(0),
                            source_hash = character(0),
                            value_hash = character(0),
                            stringsAsFactors = FALSE)

results_record <<- data.frame(call_id = integer(0),
                             result = character(0),
                             stdout = character(0), # constant for now
                             stderr = character(0), # constant for now
                             stringsAsFactors = FALSE)

# Create global call id and successes
call_id <<- 1
successes <<- 0

run_until_killed(function() { # function to be run
  # Deal with arguments used for function call
  fargs = formals(func)
  fargs_names = names(fargs)
    
  tryCatch(
  {
    if (length(fargs) == 0) {
        src_hash = paste(package, arguments[2], "NO_PARAMS", sep="::")
        calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, "NO_VALUE")
    } else {
      for (name in fargs_names) {
    	src_hash = paste(package, arguments[2], name, sep="::")
    	hash = get_random_hash(GBOV)
        value = look_up(GBOV, hash)
        # val_hash = digest::sha1(value) # Need to make sure this is consistent
        calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, hash)
        fargs[name] = value
      }
    }
  
    print(paste("*** Call ID:", call_id, "***\n", sep=" "))
    ret = do.call(func, as.list(fargs))
    print(typeof(ret))
    
    # Add the result value to GBOV if it's not in there already
    # so that it can be used in future tests
    # gbov <<- add_value(GBOV, ret)
    
    results_record[nrow(calls_record) + 1, ] <<- c(call_id, typeof(ret), "stdout", "stderr")
    successes <<- successes + 1
  }, warning = function(warn) {
    print("WARN")
    # results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
  },error = function(err) {
    print("ERR")
    print(as.character(err))
    # results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
  })
  
  call_id <<- call_id + 1
  if (call_id == 301) {
    stop()
  }
}, function() { # function run at death
  if (call_id < 1001) { # Encountered joker function
    jokers <- readRDS(arguments[4])
    jokers <- unique(append(jokers, paste(package, arguments[2], sep = "::")))
    saveRDS(jokers, file = arguments[4])
  }
  # dir.create(paste(package, arguments[2], sep = "/"), recursive = TRUE)
  write.csv(calls_record, paste(package, arguments[2], "calls.csv", sep = "::"), row.names = F)
  write.csv(results_record, paste(package, arguments[2], "results.csv", sep = "::"), row.names = F)
})
