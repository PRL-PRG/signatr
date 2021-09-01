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

package_data <- purrr::map_dfr(data, function(x)x)
saveRDS(package_data, file = paste0(package, "_data.RDS"))


suc_rates <- lapply(data, compute_suc)

res <- cbind(package = package, fun = filtered$fun, success = suc_rates)
write.csv(res, paste0(package, "_success.csv"), row.names=FALSE)


meta_df <- data.frame(packge = package,
                      num_fun = length(exported_functions),
                      budget = budget,
                      total_calls = nrow(package_data),
                      success_all = round(sum(success$success) / nrow(success), digits=1),
                      ## num_fun_param123 = param1 + param2 + param3,
                      ## calls_to_param123 = nrow(calls_param123),
                      ## success_param123 = calls_param123_success_rate,
                      time_running = round(running$toc - running$tic, digits=1))


write.csv(meta_df, paste0(package, "_metadata.csv"), row.names=FALSE)

