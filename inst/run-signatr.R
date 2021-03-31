#!/usr/bin/env Rscript

library(signatr)
library(record)

args <- commandArgs(TRUE)

if (length(arguments) < 1) {
  message("USAGE: ./run-one-new.R [csv_path] [db_path] [num_fun] [num_run]")
  q(status=1)
}

functions_df <- read.csv(args[1])
db <- open_db(args[2], create = FALSE)

if(!is.null(db)) {
  stop("could not open db")
}

num_fun <- args[3]
num_run <- args[4]

rand_id <- sample.int(nrow(functions_df), num_fun)

package_list <- lapply(rand_id, function(id) basename(functions_df[id,]$package))
fun_list <- lapply(rand_id, function(id) functions_df[id,]$fun)
params_list <- lapply(rand_id, function(id) strsplit(functions_df[id,]$params, ";"))


## lapply(package_list, function(pckg) install.packages(pckg))


## calls <- data.frame(call_id = integer(0),
##                     source_hash = character(0),
##                     value_hash = character(0),
##                     stringsAsFactors = FALSE)

run_results <<- data.frame(package = character(0),
                         fun = character(0),
                         input = character(0),
                         output = character(0),
                         result = integer(0),
                         errmsg = character(0),
                         stringsAsFactors = FALSE)

for(i in seq_along(fun_list)) {
  package <- package_list[[i]]
  fun <- get(fun_list[[i]], envir=getNamespace(package), mode="function")
  params <- params_list[[i]]

  for (j in 1:num_run) {
    id <- sample.int(size_db(), length(params[[1]]))
    arg_list <- lapply(id, function(id) get_random_val())

    input_type <- toString(lapply(arg_list, typeof))

    tryCatch ({
      res <- do.call(fun, arg_list)
      output_type <- toString(typeof(res))

      obs <- data.frame(package = package, fun = fun_list[[i]], input = input_type, output = output_type, result = 0, errmsg = NULL)
      run_results <<- rbind(obs, run_results)
    }, error = function(e) {
      obs <- data.frame(package = package, fun = fun_list[[i]], intput = input_type, output = NA, result = 1, errmsg = as.character(e))
      run_results <<- rbind(obs, run_results)
    })
  }
}


write.csv(run_results, "signatr_results.csv")



## run_until_killed(function() { # function to be run
##   # Deal with arguments used for function call
##   fargs = formals(func)
##   fargs_names = names(fargs)

##   tryCatch(
##   {
##     if (length(fargs) == 0) {
##         src_hash = paste(package, arguments[2], "NO_PARAMS", sep="::")
##         calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, "NO_VALUE")
##     } else {
##       for (name in fargs_names) {
##     	src_hash = paste(package, arguments[2], name, sep="::")
##     	hash = get_random_hash(GBOV)
##         value = look_up(GBOV, hash)
##         # val_hash = digest::sha1(value) # Need to make sure this is consistent
##         calls_record[nrow(calls_record) + 1, ] <<- c(call_id, src_hash, hash)
##         fargs[name] = value
##       }
##     }

##     print(paste("*** Call ID:", call_id, "***\n", sep=" "))
##     ret = do.call(func, as.list(fargs))
##     print(typeof(ret))

##     # Add the result value to GBOV if it's not in there already
##     # so that it can be used in future tests
##     # gbov <<- add_value(GBOV, ret)

##     results_record[nrow(calls_record) + 1, ] <<- c(call_id, typeof(ret), "stdout", "stderr")
##     successes <<- successes + 1
##   }, warning = function(warn) {
##     print("WARN")
##     # results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(warn), "stdout", "stderr")
##   },error = function(err) {
##     print("ERR")
##     print(as.character(err))
##     # results_record[nrow(calls_record) + 1, ] <<- c(call_id, as.character(err), "stdout", "stderr")
##   })

##   call_id <<- call_id + 1
##   if (call_id == 301) {
##     stop()
##   }
## }, function() { # function run at death
  ## if (call_id < 1001) { # encountered joker function
  ##   jokers <- readrds(arguments[4])
  ##   jokers <- unique(append(jokers, paste(package, arguments[2], sep = "::")))
  ##   saverds(jokers, file = arguments[4])
  ## }
  # dir.create(paste(package, arguments[2], sep = "/"), recursive = TRUE)
##   write.csv(calls_record, paste(package, arguments[2], "calls.csv", sep = "::"), row.names = F)
##   write.csv(results_record, paste(package, arguments[2], "results.csv", sep = "::"), row.names = F)
## })
