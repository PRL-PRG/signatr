#!/usr/bin/env Rscript

library(signatr)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("Missing an argument!")
}

package <- args[[1]]   # stringr | reshape2
budget <- as.double(args[[2]])    # 343    
tolerance <- as.double(args[[3]])  # 1

namespace_objects <- lapply(getNamespace(package), typeof)
namespace_functions <- names(namespace_objects[namespace_objects == "closure"])

exported_objects <- getNamespaceExports(package)

exported_functions <- intersect(namespace_functions, exported_objects)
                                                 # 49 | 6

state <- lapply(exported_functions, function(fun) feedback_loop(package = package,
                                                                fun_name = fun,
                                                                budget = budget,
                                                                tolerance = tolerance))
data <- do.call(rbind, state)                    # nrow(data) = 9919

saveRDS(data, file = paste0(package, "_data.RDS"))

# data analysis
print(paste0("the number of functions in ", package, ": ", length(exported_functions)))
print(paste0("total of function calls made: ", nrow(data)))

success <- data[data[,6] == 0L,]
success_rate <- nrow(success) / nrow(data) * 100                # 21.9% | 30.9%

print(paste0("the success rate for ", package, ": ", success_rate))

param1 <- data[data[,3] == 1,]                   #nrow(param1) = 21
param2 <- data[data[,3] == 2,]                   #nrow(param2) = 980
param3 <- data[data[,3] == 3,]                   #nrow(param3) = 5145
                                                 #              ------

param123 <- do.call(rbind, list(param1, param2, param3))       # 6146
param123_success <- param123[param123[,6] == 0L,]              # 1526
param123_success_rate <- nrow(param123_success) / nrow(param123) * 100       # 24.8% | 30.9%

print(paste0("the number of one argument functions: ", nrow(param1)))
print(paste0("the number of two argument functions: ", nrow(param2)))
print(paste0("the number of three argument functions: ", nrow(param3)))

print(paste0("the success rate for upto three argument functions for ", package, ": ", param123_success_rate))


