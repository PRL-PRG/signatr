builtin_types <- c("logical", "integer", "double", "complex", "character", "raw", "list")
all_types <- c(builtin_types, "db")


#' runs the given function with values from value generator
#' @param package package name
#' @param fun     function name
#' @param vg      value generator 
#' @param runs    number of runs to try plugging sampled vals
#' @return
#' @importFrom record open_db close_db sample_val size_db
#' @export
feedback_loop <- function (package = NA,
                           fun_name,
                           fun,
                           value_generator = generate_val,
                           types = builtin_types,
                           db = NULL,
                           budget,
                           tolerance) {

  state <- list()

  if (!is.na(package)) {
    fun <- get(fun_name, envir=getNamespace(package), mode="function")
  }

  params <- formals(fun)                  #TODO: we can inspect default arg here
  num_params <- length(params)

  if (num_params == 0) {
    new_states <- lapply(seq(budget), function(x) run_fun(package, fun_name, fun, list()))
    state <- do.call(rbind, new_states)

  } else {
    param_names <- names(params)
    history <- list()
    tol <- tolerance

    perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE) # complexity: n^r
    id <- 1

    while (budget > 0 && id < nrow(perms) + 1) {
      if (feedback(history, tol, state)) {
        new_types <- perms[id,]

        if (tol == 0 ) tol <- tolerance

        mapply(function(name, type) {params[name] <<- value_generator(type)}, param_names, new_types)

        id <- id + 1

      } else {
        old_types <- history[[length(history)]]

        mapply(function(name, type) {params[name] <<- value_generator(type)}, param_names, old_types)

        tol <- tol - 1
      }

      args <- params
      new_state <- run_fun(package, fun_name, fun, args)
      state <- rbind(state, new_state)
      budget <- budget - 1
    }
  }

  rownames(state) <- NULL
  state
}

#' decides whether to use a new set of types in the next run or not
#' @param history    sets of types used so far
#' @param tolerance  how many times to try the same set of types
#' @param state      current state
#' @return           TRUE if a new set of types should be tried
feedback <- function(history, tolerance, state) {
  length(history) == 0 || tol == 0 || (nrow(state) != 0 && state[nrow(state),]$exitval == 0L)
}


#' runs given function with args and stores the result in a list
#' @param package    package name if the function is a library function
#' @param fun_name   function name
#' @param fun        closure
#' @param args       arguments to run the function with
#' @return           list of metadata and running result
run_fun <- function(package, fun_name, fun, args) {
  res <- tryCatch ({
    output <- do.call(fun, as.list(args))
    list(package, fun_name, length(args), args, output, 0L, NA, NA)
  }, warning = function(w) {
    list(package, fun_name, length(args), args, NA, 1L, w, NA)
  }, error = function(e) {
    list(package, fun_name, length(args), args, NA, 2L, NA, e)
  })

  names(res) <- c("package", "fun_name", "num_param", "input", "output", "exitval", "warnmsg", "errmsg")
  res
}


#' generates values based on the type
#' @param type    which type of value to generate
#' @param db_path (optional) if types is "db", it's the path to the db
#' @return
#' @export
generate_val <- function(type, db_path) {
  val <- sample.int(255, size = 1)
  switch (type,
          "logical" =  as.logical(val),   #TODO: probability of TRUE is too high 
          "integer" = val,
          "double" = as.double(val),      # three special values: Inf, -Inf, NaN 
          "complex" = as.complex(val),
          "character" = as.character(val),
          "raw" = as.raw(val),
          "list" = as.list(val),
          "db" =  {
            if(dir.exists(db_path)) {
              db <- open_db(db_path, create = FALSE)
              val <- sample_val()         #TODO: could use new api to generate targeted val
              close_db()
              return(val)
            } else {
              stop("db doesn't exist.")
            }
          })
}

