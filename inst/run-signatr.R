#!/usr/bin/env Rscript

library(signatr)
library(record)

args <- commandArgs(TRUE)

if (length(args) < 1) {
  message("USAGE: ./run-signatr.R [functions_path] [db_path] [num_funs] [num_runs] [package] [fun-arity]")
  q(status=1)
}

functions_df <- read.csv(args[1]) # nrow(functions_df) = 551,665
functions_df <- na.omit(functions_df) #nrow(functions_df) = 537,272

db <- open_db(args[2], create = FALSE)
if(!is.null(db)) {
  stop("could not open db")
}

if(length(args) >= 5) {
  package_name <- paste0("/mnt/nvme1/R/project-signatR/run/package-metadata/", args[5])
  functions_df <- functions_df[functions_df$package == package_name, ]
  if(length(args) == 6) {
    arity <- args[6]
    functions_df <- functions_df[nchar(gsub('[^;]', '', functions_df$params))+1==arity,,drop=FALSE]
  }
}

total_funs <- nrow(functions_df)
num_funs <- as.integer(args[3])

if (total_funs >= num_funs) {
  rand_id <- sample.int(total_funs, num_funs)
} else {
  num_funs <- total_funs
  rand_id <- sample.int(num_funs, num_funs)
}

num_runs <- args[4]


package_list <- lapply(rand_id, function(id) basename(functions_df[id,]$package))


fun_list <- lapply(rand_id, function(id) functions_df[id,]$fun)
params_list <- lapply(rand_id, function(id) strsplit(functions_df[id,]$params, ";"))

run_results <<- data.frame(package = character(0),
                         fun = character(0),
                         input = character(0),
                         output = character(0),
                         exitval = integer(0),
                         errmsg = character(0),
                         stringsAsFactors = FALSE)

for(i in seq_along(fun_list)) {
  package <- package_list[[i]]
  fun <- get(fun_list[[i]], envir=getNamespace(package), mode="function")
  params <- params_list[[i]]

  for (j in 1:num_runs) {
    stopifnot(size_db() >= length(params[[1]]))

    id <- sample.int(size_db(), length(params[[1]]))
    arg_list <- lapply(id, function(id) get_random_val())

    input_type <- toString(lapply(arg_list, typeof))

    tryCatch ({
      res <- do.call(fun, arg_list)
      output_type <- toString(typeof(res))

      obs <- data.frame(package = package, fun = fun_list[[i]], input = input_type, output = output_type, exitval = 0L, errmsg = NA)
      run_results <<- rbind(obs, run_results)
    }, error = function(e) {
      obs <- data.frame(package = package, fun = fun_list[[i]], intput = input_type, output = NA, exitval = 1L, errmsg = as.character(e))
      run_results <<- rbind(obs, run_results)
    })
  }
}

end <- close_db()

if(!is.null(end)) {
  stop("could not close db")
}

write.csv(run_results, "signatr-results.csv", row.names=FALSE)
