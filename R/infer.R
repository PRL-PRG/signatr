#' @export
# run the given function with values from value generator
# @param package a string representation of package name
# @param fun  a string representation of function name
# @param params number of formals 
# @param vg value generator 
# @param runs number of runs to try plugging sampled vals
# @return a list of type signature of fun if successful
#' @importFrom record open_db close_db sample_val size_db

run_fun <- function(package = NULL, fun, params, vg = c("random_int", "random_char", "random_double", "db"), db = NULL, runs) {

  result <- data.frame(package = character(0),
                       fun = character(0),
                       input = character(0),
                       output = character(0),
                       exitval = integer(0),
                       msg = character(0),
                       stringsAsFactors = FALSE)

  clo <- get(fun, envir=getNamespace(package), mode="function")

  run_results <- lapply(seq(runs), function(r) {
    switch (vg,
            "random_int" = {
              args <- sample.int(100, params, replace = FALSE)},
            "random_double" = {
              args <- runif(params, 0, 100)},
            "random_char" = {
              args <- lapply(runif(params, 0, 100), as.character)},
            {
              if(dir.exists(db)) {
                db <- open_db(db, create = FALSE)
                stopifnot(size_db() >= params)

                id <- sample.int(size_db(), params)
                args <- lapply(id, function(id) sample_val())
              } else {
                stop("db doesn't exist.")
              }
            })

    ## input_type <- toString(lapply(args, typeof))
    res <- data.frame(package = package, fun = fun, input = paste(args, collapse=','), output = NA, exitval = NA, errmsg = NA, stringsAsFactors = FALSE)

    tryCatch ({
      output <- do.call(clo, as.list(args))
      ## output_type <- toString(typeof(res))

      res$output <- output
      res$exitval <- 0L
      res
    }, warning = function(w) {
      res$exitval <- 1L
      res$errmsg <- as.character(w)
      res
      ## data.frame(package = package, fun = fun, intput = args, output = NA, exitval = 1L, msg = as.character(w))
    }, error = function(e) {
      res$exitval <- 2L
      res$errmsg <- as.character(e)
      res
      ## data.frame(package = package, fun = fun, intput = args, output = NA, exitval = 2L, errmsg = as.character(e))
    })
  })

  result <- do.call(rbind, run_results)

  result
}

infer_sig <- function(res) {
  not_errored <- 0L %in% res$exitval

  if(not_errored) {
    runs <- which(res$exitval == 0L)
    types <- lapply(runs, function(r) data.frame(intput = res[r,]$input, output = res[r,]$output))
  } else {
    types <- character(0)
  }
  types
}
