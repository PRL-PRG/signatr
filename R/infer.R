#' @export
# infer type signature of a given function
# @param package a string representation of package name
# @param fun  a string representation of function name
# @param params number of formals 
# @param db path to database to sample values from
# @param runs number of runs to try plugging sampled vals
# @return a list of type signature of fun if successful
#' @importFrom record open_db close_db sample_val size_db

infer_signature <- function(package = NULL, fun, params, db, runs) {
  if(dir.exists(db)) {
    db <- open_db(db, create = FALSE)
  } else {
    stop("db doesn't exist.")
  }

  run_results <<- data.frame(package = character(0),
                             fun = character(0),
                             input = character(0),
                             output = character(0),
                             exitval = integer(0),
                             errmsg = character(0),
                             stringsAsFactors = FALSE)

  clo <- get(fun, envir=getNamespace(package), mode="function")

  for (j in 1:runs) {
    stopifnot(size_db() >= params)

    id <- sample.int(size_db(), params)
    arg_list <- lapply(id, function(id) sample_val())

    input_type <- toString(lapply(arg_list, typeof))

    tryCatch ({
      res <- do.call(clo, arg_list)
      output_type <- toString(typeof(res))

      obs <- data.frame(package = package, fun = fun, input = input_type, output = output_type, exitval = 0L, errmsg = NA)
      run_results <<- rbind(obs, run_results)
    }, error = function(e) {
      obs <- data.frame(package = package, fun = fun, intput = input_type, output = NA, exitval = 1L, errmsg = as.character(e))
      run_results <<- rbind(obs, run_results)
    })
  }

  print(run_results)
  
  is_successful <- 0L %in% run_results$exitval

  if(is_successful) {
    successful_runs <- which(run_results$exitval == 0L)
    res <- lapply(successful_runs, function(id) c(run_results[id,]$input, fun_results[id,]$output))
  } else {
    res <- character(0)
  }
  res
}







