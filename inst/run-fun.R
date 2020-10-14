#!/usr/bin/env Rscript
args <- commandArgs(TRUE)

source("../R/helper.R")
source("../R/gbov.R")

add1 <- function(x) { return(x + 1) }


if (length(args) != 6) {
  print("USAGE: ./run-fun.R [package] [function] [GBOV_path] [timeout] [output_path] [call #]")
  stop()
}

## Script Set Up
if (args[1] == "NULL" || args[1] == "null") {
  package = NULL
} else {
  package = args[1]
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

gbov <- gbov_load(args[3])
if (is.null(gbov)) {
  print("Could not locate the Great Book of Values")
  stop()
}

timeout = as.integer(args[4])
if (timeout <= 0) {
  print("Timeout is required to be a positive number.")
  stop()
}

calls = as.integer(args[6]) # TODO: Use this in the run_until... function
if (calls < 1) {
  print("Must perform at least 1 call")
  stop()
}

## Result Set Up
# call : int - autoincremented id which identifies all rows that belongs together
# param : int - parameter index (0 is for the return value)
# hash : chr - the hash of the value that was used
# type : chr - the type of the value
# error : chr - the error message, only for the row at p_idx == 0

PARAM = 1 # Only deal with function with 1 argument for now
ERROR_HASH = 0
ERROR_TYPE = 0

# Create global data.frame successes and failures
successes <<- data.frame(call = integer(0),
                        param = integer(0), # constant for now 
                        hash = character(0), 
                        type = character(0),
                        stringsAsFactors = FALSE)

failures <<- data.frame(call = integer(0),
                       param = integer(0),  # constant for now
                       hash = character(0), 
                       type = character(0),
                       error = character(0),
                       stringsAsFactors = FALSE)

print("Initalized")

ncall <<- 1
run_until_timeout_or_death(timeout, function() {
  for (i in seq(calls)) {
    tryCatch(
      {
        arg_list = get_args_for_function(f, gbov)
        ret = do.call(f, arg_list)
  
        gbov <<- add_value(gbov, ret)
  
        successes[nrow(successes) + 1, ] <<- c(ncall, 0, length(gbov), 
                                               signatr_typeof(ret))
        for (j in seq(l)) {
          successes[nrow(successes) + 1, ] <<- c(ncall, j, 
                                                digest::sha1(arg_list[i]), 
                                                typeof(arg_list[i]))
        }
        ncall <<- ncall + 1
      }, error = function(err) {
        failures[nrow(failures) + 1, ] <<- c(ncall, 0, ERROR_HASH, ERROR_TYPE, as.character(err))
        
        l = length(arg_list)
        for (j in seq(l)) {
          failures[nrow(successes) + 1, ] <<- c(ncall, j, 
                                                 digest::sha1(arg_list[i]), 
                                                 ERROR_TYPE, as.character(err))
        }
        ncall <<- ncall + 1
      }
    )}
  },
  function () {
    write.csv2(successes, paste(args[5], "successes.csv", sep = "_"))
    write.csv2(failures, paste(args[5], "failures.csv", sep = "_"))
  }  
)
