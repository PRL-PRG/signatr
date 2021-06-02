#' @export
# infer type signature of a given function
# @param package package name
# @f  function name
# @num_params number of formals 
# @db path to database to sample values from
# @num_runs number of runs to try inferring
# @return a list of type signature of function f
#' @importFrom record open_db close_db sample_val size_db


infer_signature <- function(package = NULL, f, num_params, db, num_runs) {
  db <- open_db(db, create = FALSE)
  if(!is.null(db)) {
    stop("could not open db")
  }

  run_results <<- data.frame(package = character(0),
                             fun = character(0),
                             input = character(0),
                             output = character(0),
                             exitval = integer(0),
                             errmsg = character(0),
                             stringsAsFactors = FALSE)

  fun <- get(f, envir=getNamespace(package), mode="function")

  for (j in 1:num_runs) {
    stopifnot(size_db() >= num_params)

    id <- sample.int(size_db(), num_params)
    arg_list <- lapply(id, function(id) sample_val())

    input_type <- toString(lapply(arg_list, typeof))

    tryCatch ({
      res <- do.call(fun, arg_list)
      output_type <- toString(typeof(res))

      obs <- data.frame(package = package, fun = f, input = input_type, output = output_type, exitval = 0L, errmsg = NA)
      run_results <<- rbind(obs, run_results)
    }, error = function(e) {
      obs <- data.frame(package = package, fun = f, intput = input_type, output = NA, exitval = 1L, errmsg = as.character(e))
      run_results <<- rbind(obs, run_results)
    })
  }

  run_results
}
