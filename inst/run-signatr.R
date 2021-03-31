#!/usr/bin/env Rscript

library(signatr)
library(record)

args <- commandArgs(TRUE)

if (length(args) < 1) {
  message("USAGE: ./run-signatr.R [functions_path] [db_path] [num_fun] [num_run]")
  q(status=1)
}

functions_df <- read.csv(args[1])

package_name <- paste0("/mnt/nvme1/R/project-signatR/run/package-metadata/", args[2])
package_df <- functions_df[functions_df$package == package_name, ]

db <- open_db(args[3], create = FALSE)
if(!is.null(db)) {
  stop("could not open db")
}

num_fun <- args[4]
num_run <- args[5]

rand_id <- sample.int(nrow(package_df), num_fun)

package_list <- lapply(rand_id, function(id) basename(package_df[id,]$package))
fun_list <- lapply(rand_id, function(id) package_df[id,]$fun)
params_list <- lapply(rand_id, function(id) strsplit(package_df[id,]$params, ";"))


## lapply(package_list, function(pckg) install.packages(pckg))


## calls <- data.frame(call_id = integer(0),
##                     source_hash = character(0),
##                     value_hash = character(0),
##                     stringsasfactors = false)

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

      obs <- data.frame(package = package, fun = fun_list[[i]], input = input_type, output = output_type, result = 0, errmsg = null)
      run_results <<- rbind(obs, run_results)
    }, error = function(e) {
      obs <- data.frame(package = package, fun = fun_list[[i]], intput = input_type, output = NA, result = 1, errmsg = as.character(e))
      run_results <<- rbind(obs, run_results)
    })
  }
}

end <- close_db()

if(!is.null(end)) {
  stop("could not close db")
}

write.csv(run_results, "signatr_results.csv", row.names=FALSE)
