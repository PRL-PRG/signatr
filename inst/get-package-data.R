#!/usr/bin/env Rscript
library(rmarkdown)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) != 2) stop("required arguments: [package] [strategy]")

package <- args[[1]]
strategy <- args[[2]]

print(paste0("Package: ", package))
print(paste0("Strategy: ", strategy))

render("../signatr.Rmd", params = list(package = package, strategy = strategy))

