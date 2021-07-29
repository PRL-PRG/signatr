# Types
builtin <- c("logical", "integer", "double", "complex", "character", "raw", "list")
all <- c(builtin, "db")


#' runs the given function with values from value generator
#' @param package package name
#' @param fun     function name
#' @param vg      value generator 
#' @param runs    number of runs to try plugging sampled vals
#' @return
#' @export
feedback_loop <- function (package = NA,
                           fun_name,
                           fun,
                           value_generator = generate_val,
                           types = builtin,
                           db = NULL,
                           budget,
                           tolerance) {

  state <- list()

  if (!is.na(package)) {
    fun <- get(fun_name, envir=getNamespace(package), mode="function")
  }

  params <- formals(fun)
  num_params <- length(params)

  if (num_params == 0) {
    new_states <- lapply(seq(budget), function(x) run_fun(package, fun_name, fun, list()))
    state <- do.call(rbind, new_states)

  } else if (num_params > 9) {
    state <- list(package, fun_name, num_params, NA, NA, 3L, NA, NA)
    names(state) <- c("package", "fun_name", "num_param", "input", "output", "exitval", "warnmsg", "errmsg")
    
  } else {
    param_names <- names(params)
    tol_left <- tolerance

    perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE) # complexity: n^r (7^10 is the limit)

    perm_id <- 1

    while (budget > 0 && perm_id < nrow(perms) + 1) {
      if (feedback(state, tol_left)) {
        types_to_try <- perms[perm_id,]

        if (tol_left == 0 ) tol_left <- tolerance
        perm_id <- perm_id + 1

      } else {
        types_to_try <- lapply(state[nrow(state),]$input, typeof)

        tol_left <- tol_left - 1
      }

      mapply(function(name, type) {params[name] <<- value_generator(type)}, param_names, types_to_try)

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
#' @param state      current state
#' @param tolerance  how many times to try the same set of types
#' @return           TRUE if a new set of types should be tried
feedback <- function(state, tolerance) {
  length(state) == 0 || tolerance == 0 || nrow(state) != 0 && state[nrow(state),]$exitval == 0L
}


#' runs given function with args and stores the result in a list
#' @param package    package name if the function is a library function
#' @param fun_name   function name
#' @param fun        closure
#' @param args       arguments to run the function with
#' @return           list of metadata and running result
run_fun <- function(package = NA, fun_name, fun, args) {
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
generate_val <- function(type, db_path) {
  val <- sample.int(255, size = 1)
  l <- list()

  switch (type,
          "logical" =  as.logical(val),   # TODO: sample_val(logical)
          "integer" = val,
          "double" = as.double(val),      # TODO: add three special values: Inf, -Inf, NaN
          "complex" = as.complex(val),
          "character" = as.character(val),
          "raw" = as.raw(val),
          "list" = lapply(seq(val), function(i) l[[i]] <- val),
          "db" =  {
            if(dir.exists(db_path)) {
              db <- record::open_db(db_path, create = FALSE)
              val <- record::sample_val()

              record::close_db()
              return(val)
            } else {
              stop("db doesn't exist.")
            }
          })
}

