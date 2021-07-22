#!/usr/bin/env Rscript
library(tictoc)

tic("total")
library(signatr)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Missing an argument!")
}

package <- args[[1]]   # stringr | reshape2
budget <- as.double(args[[2]])    # 343
tolerance <- as.double(args[[3]])  # 1

tic("generating data")
ns <- getNamespace(package)

namespace_objects <- lapply(getNamespace(package), typeof)
namespace_functions <- names(namespace_objects[namespace_objects == "closure"])

exported_objects <- getNamespaceExports(package)
exported_functions <- intersect(namespace_functions, exported_objects)     # 49 | 6
closures <- lapply(exported_functions, get0, envir=ns)
num_params <- lapply(closures, function(f) length(formals(f)))

fun_df <- data.frame(
  fun = exported_functions,
  num_param = unlist(num_params))

filtered <- fun_df[fun_df$num_param < 10, ]

state <- lapply(filtered$fun, function(fun) feedback_loop(package = package,
                                                                fun_name = fun,
                                                                budget = budget,
                                                                tolerance = tolerance))

data <- do.call(rbind, state)                    # nrow(data) = 9919


saveRDS(data, file = paste0(package, "_data.RDS"))
generating <- toc()

tic("data analysis")
print(paste0("number of functions in ", package, ": ", length(exported_functions)))
print(paste0("total of function calls made: ", nrow(data)))

success <- data[data[,6] == 0L,]
success_rate <- nrow(success) / nrow(data) * 100                # 21.9% | 30.9%

print(paste0("success rate for ", package, ": ", success_rate))

param1 <- data[data[,3] == 1,]                   #nrow(param1) = 21
param2 <- data[data[,3] == 2,]                   #nrow(param2) = 980
param3 <- data[data[,3] == 3,]                   #nrow(param3) = 5145
                                                 #              ------

param123 <- do.call(rbind, list(param1, param2, param3))       # 6146
param123_success <- param123[param123[,6] == 0L,]              # 1526
param123_success_rate <- nrow(param123_success) / nrow(param123) * 100       # 24.8% | 30.9%



print(paste0("number of calls to one argument functions: ", nrow(param1)))
print(paste0("number of calls to two argument functions: ", nrow(param2)))
print(paste0("number of calls to three argument functions: ", nrow(param3)))

print(paste0("success rate for upto three argument functions for ", package, ": ", param123_success_rate))

param_above_10 <- data[data[,3] > 10,]


toc()
total <- toc()

meta_df <- data.frame(packge = package, num_fun = length(exported_functions), total_calls = nrow(data), success_all = success_rate, param123 = nrow(param123), success_param123 = param123_success_rate, time_total = (total$toc - total$tic), time_generating = (generating$toc - generating$tic))

write.csv(meta_df, paste0(package, "_metadata.csv"), row.names=FALSE)
