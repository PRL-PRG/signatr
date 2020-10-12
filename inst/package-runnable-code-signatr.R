#!/usr/bin/env Rscript

options(error = function() { traceback(3); q(status=1) })

library(glue)
library(runr)

wrap <- function(package, file, type, body) {
  body <- paste("      ", body, collapse="\n")
  glue(
    "signatr::trace(code = {{",
    "{body}",
    "}},",
    "path=file.path(Sys.getenv('RUNR_CWD'), basename('{file}')),",
    ## "path= \"./gbov\",",
    "package = '{package}'",
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

df <- runr::extract_package_code(
  package,
  package_path,
  types="all",
  output_dir=".",
  compute_sloc=TRUE,
  wrap_fun=wrap
)

write.csv(df, OUTPUT_FILE, row.names=FALSE)
