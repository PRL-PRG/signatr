#!/usr/bin/env Rscript

options(error = function() { traceback(3); q(status=1) })

library(glue)

wrap <- function(package, file, type, body) {
  body <- paste("      ", body, collapse="\n")
  glue(
    "record::open_db('{db_path}', create = FALSE)",
    "argtracer::trace_args(code = {{",
    "{body}",
    "}},",
    ## "path=file.path(Sys.getenv('RUNR_CWD'), basename('{file}')),",
    ## "package = {paste0(all, collapse=',')}",
    ")",
    "record::size_db()",
    "record::close_db()",
  .sep = "\n"
  )
}

OUTPUT_FILE <- "runnable-code.csv"

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("Missing a path to the package source or db")
}

package_path <- args[1]
package <- basename(package_path)

db_path <- args[2]

all <- tools::package_dependencies(package, recursive = TRUE)
all[[1]] <- c(all[[1]], package)

df <- runr::extract_package_code(
  package,
  package_path,
  types="all",
  output_dir=".",
  compute_sloc=TRUE,
  wrap_fun=wrap,
  )

write.csv(df, OUTPUT_FILE, row.names=FALSE)


