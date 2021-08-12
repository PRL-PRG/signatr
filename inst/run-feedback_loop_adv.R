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

print(paste0("Package: ", package))

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

param1 <- nrow(fun_df[fun_df$num_param == 1,])
param2 <- nrow(fun_df[fun_df$num_param == 2,])
param3 <- nrow(fun_df[fun_df$num_param == 3,])


print(paste0("Number of functions: ", length(exported_functions)))
print(paste0("Number of one argument functions: ", param1))
print(paste0("Number of two argument functions: ", param2))
print(paste0("Number of three argument functions: ", param3))


state <- lapply(filtered$fun, function(fun) feedback_loop_adv(package = package,
                                                              fun_name = fun,
                                                              value_generator = generate_val_adv,
                                                              budget = budget,
                                                              tolerance = tolerance))

data <- do.call(rbind, state)                    # nrow(data) = 9919


saveRDS(data, file = paste0(package, "_data_adv.RDS"))
generating <- toc()

tic("data analysis")

print(paste0("Total of function calls made: ", nrow(data)))

success <- data[data[,6] == 0L,]
success_rate <- round(nrow(success) / nrow(data) * 100, digits = 1)                # 21.9% | 30.9%

print(paste0("Over all success rate: ", success_rate))

calls_param1 <- data[data[,3] == 1,]                   #nrow(param1) = 21
calls_param2 <- data[data[,3] == 2,]                   #nrow(param2) = 980
calls_param3 <- data[data[,3] == 3,]                   #nrow(param3) = 5145

calls_param123 <- do.call(rbind, list(calls_param1, calls_param2, calls_param3))       # 6146
calls_param123_success <- calls_param123[calls_param123[,6] == 0L,]              # 1526
calls_param123_success_rate <- round(nrow(calls_param123_success) / nrow(calls_param123) * 100, digits = 1)    # 24.8% | 30.9%


print(paste0("Number of calls to one argument functions: ", nrow(calls_param1)))
print(paste0("Number of calls to two argument functions: ", nrow(calls_param2)))
print(paste0("Number of calls to three argument functions: ", nrow(calls_param3)))

print(paste0("Success rate for up to three argument functions: ", calls_param123_success_rate))


toc()
total <- toc()

meta_df <- data.frame(packge = package, num_fun = length(exported_functions), budget = budget, tolerance = tolerance, total_calls = nrow(data), success_all = success_rate, num_fun_param123 = param1 + param2 + param3, calls_to_param123 = nrow(calls_param123), success_param123 = calls_param123_success_rate, time_total = (total$toc - total$tic), time_generating = (generating$toc - generating$tic))

write.csv(meta_df, paste0(package, "_metadata_adv.csv"), row.names=FALSE)

