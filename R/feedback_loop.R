# Types
builtin <- c("logical", "integer", "double", "complex", "character", "raw", "list")
all <- c(builtin, "dim1", "dim2") #TODO: add "db"


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

  params <- formals(fun)                  #TODO: we can inspect default arg here
  num_params <- length(params)

  if (num_params == 0) {
    new_states <- lapply(seq(budget), function(x) run_fun(package, fun_name, fun, list()))
    state <- do.call(rbind, new_states)

  } else if (num_params > 9) {
    state <- list(package, fun_name, num_params, NA, NA, 3L, NA, NA)
    names(state) <- c("package", "fun_name", "num_param", "input", "output", "exitval", "warnmsg", "errmsg")
  } else {

    param_names <- names(params)
    history <- list()
    tol_left <- tolerance

    perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE) # complexity: n^r (7^10 is the limit)

    id <- 1

    while (budget > 0 && id < nrow(perms) + 1) {
      if (feedback(state, tol_left)) {
        types_to_try <- perms[id,]

        if (tol_left == 0 ) tol_left <- tolerance

        id <- id + 1

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


feedback_loop_adv <- function (package = NA,
                               fun_name,
                               fun,
                               value_generator = generate_val,
                               types = all,
                               typing = function(x) typeof(x),
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

  } else if (num_params > 9) {
    state <- list(package, fun_name, num_params, NA, NA, 3L, NA, NA)
    names(state) <- c("package", "fun_name", "num_param", "input", "output", "exitval", "warnmsg", "errmsg")
  } else {
    param_names <- names(params)

    ## history <- list() # TODO: history used to be needed for generating unique permutation
    tol_left <- tolerance

    ## manipulate_tol <- function(new_tol) { tol_left <<- new_tol }

    while (budget > 0) {

      if (length(state) == 0) {
        types_to_try <- generate_types_randomly()
      } else {
        last_state <- state[nrow(state,)]

        types_to_try <- switch(feedback_adv(last_state),
                               "get a new permutation randomly" = generate_types_randomly(types, num_params),
                               "get a new permutation guided by message" = generate_types_by_msg(last_state, types, param_names),
                               "try again" = {
                                 tol_left <- tol_left - 1
                                 lapply(last_state$input, typing)
                               }
                               )

        ## generate_fun <- feedback_adv(last_state)
        ## types_to_try <- generate_fun() # TODO
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

## #' decides whether to use a new set of types in the next run or not
## #' @param history    sets of types used so far
## #' @param tolerance  how many times to try the same set of types
## #' @param state      current state
## #' @param fun        function to manipulate tolerance left
## #' @return           a set of types to try
## feedback_adv <- function(history, tolerance, tol_left, state, fun) {
##   last_state <- state[nrow(state),]

##   # 1) at the very beginning of the loop
##   # 2) no more tolerance left
##   # 3) last run was successful so want a new set of types to try
##   if (length(history) == 0 || tol_left == 0 || (nrow(state) != 0 && was_successful(last_state))) {
##     if (tol_left == 0) fun(tolerance)
##     return(generate_type(last_state$input))

##   } else if (!was_successful(last_state)) {
##     return(lapply(seq(history[[1]]), function(x) generate_type(last_state$errmsg)))

##   } else {
##     fun(tolerance - 1)
##     return(history[[length(history)]])
##   }
## }

feedback_adv <- function(state) {
  if (length(history) == 0 || tol_left == 0 || state$exitval == 0L) {
    return ("get a new permutation randomly")
  } else if (state$exitval == 1L || state$exitval == 2L) {
    return ("get a new permutation guided by message")
  } else {
    return ("try again")
  }
}


## feedback_adv <- function(state) {
##   if (length(history) == 0 || tol_left == 0 || (nrow(state) != 0 && state$exitval == 0L)) {
##     return (generate_types_randomly)
##   } else if (state$exitval == 1L || state$exitval == 2L) {
##     return (generate_types_by_msg)
##   } else {
##     return (generate_types_again)
##   }
## }


generate_types_randomly <- function(types, num_params) {
  indices <- sample.int(length(types), num_params)
  random_perm <- lapply(indices, function(id) types[[id]])

  random_perm #TODO: generate unique_perm?
}

generate_types_by_msg <- function(warn_or_err, types, params) {
  num_params <- length(params)

  msg <- warn_or_err$message
  splited_msg <- strsplit(msg, "[` ]+")

  param_match <- which(params %in% splited_msg)
  type_match <- which(types %in% splited_msg)

  guided_perm <- list()

  # TODO: assuming length(param_match) = 1
  if (param_match && type_match) {
    indices <- sample.int(length(types), num_params)

    mapply(function(x, y) {
      if (x == param_match)
        perm[[x]] <- type_match
      else perm[[x]] <- types[[y]]
    }, seq(num_params), indices)

    return (guided_perm)
  } else {
    return(generate_types_randomly(types, num_params))
  }
}

## generate_types_again <- function(history) {
##   history[[length(history)]]
## }



## #' generates a type based on the previous run
## #' @param list       either list of inputs or list of error message
## #' @return           a set of types to try
## generate_type <- function(list) {
##   if(any(c("simpleerror", "error") %in% class(list))) {
##     msg <- list$message
##     generate_val_by_errmsg(msg)
##   } else {
##     inputs <- list$input
##     types <- lapply(inputs, typeof)
##   }
## }


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
              db <- record::open_db(db_path, create = FALSE)
              val <- record::sample_val()         #TODO: could use new api to generate targeted val
              record::close_db()
              return(val)
            } else {
              stop("db doesn't exist.")
            }
          })
}

## generate_val_by_errmsg <- function(msg) {
##   r <- sample.int(255, size = 1)
##   c <- sample.int(255, size = 1)

##   if (any(c("dataframe", "data frame", "data.frame") %in% msg)) {
##     return(data.frame(A = c(1,2,3), B = c(4,5,6)))
##   } else if ("matrix" %in% msg) {
##     return(matrix(1:6, nrow = r, ncol = c))
##   } else if ("is.function" %in% msg) {
##     return(function(x) x)
##   } else if (all(c("invalid","atomic vectors") %in% msg)) {
##     return(1:r)
##   }

## }
