#!/usr/bin/env Rscript
args <- commandArgs(TRUE)

if (length(args) != 5) {
  print("USAGE: ./run-fun.R [package] [function] [GBOV_path] [timeout] [output_path]")
}

## Script Set Up
f <- get_function(args[1], args[2])
if (is.null(f)) {
  print("Could not locate specified function")
}

gbov <- gbov_load(args[3])
if (is.null(gbov)) {
  print("Could not locate the Great Book of Values")
}

timeout = as.integer(args[4])
if (timeout <= 0) {
  print("Timeout is required to be a positive number.")
}

## Result Set Up
# call : int - autoincremented id which identifies all rows that belongs together
# param : int - parameter index (0 is for the return value)
# hash : chr - the hash of the value that was used
# type : chr - the type of the value
# error : chr - the error message, only for the row at p_idx == 0

CALL = 1
PARAM = 1
ERROR_HASH = 0
ERROR_TYPE = 0

successes <- data.frame(call = integer(0), # constant for now
                        param = integer(0), # constant for now 
                        hash = character(0), 
                        type = character(0))

failures <- data.frame(call = integer(0), # constant for now
                       param = integer(0),  # constant for now
                       hash = character(0), 
                       type = character(0),
                       error = character(0))

run_until_timeout_or_death(timeout, {
  hash <- gbov_get_random_hash(gbov)
  value <- gbov_get_value(gbov, hash)
  tryCatch(
    {
      ret = do.call(f, value)
      successes[nrow(successes) + 1, ] = c(CALL, 0, gbov_add_value(ret), signatr_typeof(ret))
      successes[nrow(successes) + 1, ] = c(CALL, PARAM, hash, signatr_typeof(value))
    }, error = function(err) {
      failures[nrow(failures) + 1, ] = c(CALL, 0, ERROR_HASH, ERROR_TYPE, as.character(err))
      failures[nrow(failures) + 1, ] = c(CALL, PARAM, hash, ERROR_TYPE, as.character(err))
    }
  )
})

## Output result
write.csv(successes, paste(args[5], "successes.csv", sep = "_"))
write.csv(failures, paste(args[5], "failures.csv", sep = "_")