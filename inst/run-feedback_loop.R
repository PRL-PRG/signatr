#!/usr/bin/env Rscript
library(tictoc)
library(signatr)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("wrong number of arguments!")
}

package <- args[[1]]
strategy <- args[[2]]
budget <- as.integer(args[[3]])
## tolerance <- as.integer(args[[4]])

print(paste0("Package: ", package))

ns <- getNamespace(package)

namespace_objects <- lapply(getNamespace(package), typeof)
namespace_functions <- names(namespace_objects[namespace_objects == "closure"])

exported_objects <- getNamespaceExports(package)
exported_functions <- intersect(namespace_functions, exported_objects)
closures <- lapply(exported_functions, get0, envir=ns)
num_params <- lapply(closures, function(f) length(formals(f)))

fun_df <- data.frame(
  fun = exported_functions,
  num_param = unlist(num_params))

filtered <- fun_df[fun_df$num_param < 10, ]

param1 <- nrow(fun_df[fun_df$num_param == 1,])
param2 <- nrow(fun_df[fun_df$num_param == 2,])
param3 <- nrow(fun_df[fun_df$num_param == 3,])


print(paste0("Number of functions: ", length(exported_functions)))
print(paste0("Number of one argument functions: ", param1))
print(paste0("Number of two argument functions: ", param2))
print(paste0("Number of three argument functions: ", param3))

tic("running functions")
data <- lapply(filtered$fun, function(fun) feedback_loop(package = package,
                                                         fun_name = fun,
                                                         strategy = strategy,
                                                         budget = budget))

running <- toc()

data_with_sigs <- lapply(data, function(fun) add_sigs(fun))

package_data_with_sigs <- do.call(rbind, data_with_sigs)
saveRDS(package_data_with_sigs, file = paste0(package, "_data.RDS"))


suc_rates <- lapply(data_with_sigs, compute_suc)

res <- cbind(package = package, fun = filtered$fun, success = suc_rates)
write.csv(res, paste0(package, "_success.csv"), row.names=FALSE)




#TODO benchmarking here
# some success analysis
## tic("analyzing success")

## print(paste0("Total of function calls made: ", nrow(data)))

## success <- data[data[,6] == 0L,]
## success_rate <- round(nrow(success) / nrow(data) * 100, digits = 1)                # 21.9% | 30.9%

## print(paste0("Over all success rate: ", success_rate))

## calls_param1 <- data[data[,3] == 1,]                   #nrow(param1) = 21
## calls_param2 <- data[data[,3] == 2,]                   #nrow(param2) = 980
## calls_param3 <- data[data[,3] == 3,]                   #nrow(param3) = 5145

## calls_param123 <- do.call(rbind, list(calls_param1, calls_param2, calls_param3))       # 6146
## calls_param123_success <- calls_param123[calls_param123[,6] == 0L,]              # 1526
## calls_param123_success_rate <- round(nrow(calls_param123_success) / nrow(calls_param123) * 100, digits = 1)    # 24.8% | 30.9%


## print(paste0("Number of calls to one argument functions: ", nrow(calls_param1)))
## print(paste0("Number of calls to two argument functions: ", nrow(calls_param2)))
## print(paste0("Number of calls to three argument functions: ", nrow(calls_param3)))

## print(paste0("Success rate for up to three argument functions: ", calls_param123_success_rate))

## analyzing <- toc()

## meta_df <- data.frame(packge = package, num_fun = length(exported_functions), budget = budget, tolerance = tolerance, total_calls = nrow(data), success_all = success_rate, num_fun_param123 = param1 + param2 + param3, calls_to_param123 = nrow(calls_param123), success_param123 = calls_param123_success_rate, time_running = (running$toc - running$tic), time_analyzing = (analyzing$toc - analyzing$tic))

## write.csv(meta_df, paste0(package, "_metadata.csv"), row.names=FALSE)

