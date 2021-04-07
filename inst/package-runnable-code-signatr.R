#!/usr/bin/env Rscript

options(error = function() { traceback(3); q(status=1) })

library(glue)
library(runr)
library(tictoc)

tic("instrumenting")
print("instrumenting started...")

wrap <- function(package, file, type, body) {
  body <- paste("      ", body, collapse="\n")
  glue(
    "signatr::trace(code = {{",
    "{body}",
    "}},",
    "path=file.path(Sys.getenv('RUNR_CWD'), basename('{file}')),",
    "package = {paste0(all, collapse=',')}",
    ")",
    .sep = "\n"
  )
}

OUTPUT_FILE <- "runnable-code.csv"

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Missing a path to the package source")
}

package_path <- args[1]
package <- basename(package_path)

all <- tools::package_dependencies(package, recursive = TRUE)
all[[1]] <- c(package, all[[1]])

df <- runr::extract_package_code(
  package,
  package_path,
  types="all",
  output_dir=".",
  compute_sloc=TRUE,
  wrap_fun=wrap
  )

time <- toc()
print(paste0("instrumenting ", package, " done..."))

write.csv(df, OUTPUT_FILE, row.names=FALSE)
write.csv(time$toc - time$tic, "instrumenting-time.csv", row.names=FALSE)


