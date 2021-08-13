# Types
DIM0 <- c("logical", "integer", "double", "complex", "character", "raw")
DIM1 <- c("list", "dim1")
DIM2 <- c("dim2")
DB <- "db"
TYPES <- c(DIM0, DIM1, DIM2)

#' runs the given function with values from value generator
#' @param  package         package name
#' @param  fun_name        function name
#' @param  fun             function to infer type signature
#' @param  types           a list of types for typing
#' @param  value_generator a function that provides arguments
#' @param  budget          number of calls to fun
#' @param  tolerance       number of calls that applies values of the same type permutation
#' @return
#' @export
feedback_loop_adv <- function (package = NA,
                               fun_name,
                               fun,
                               types = TYPES,
                               value_generator = generate_val_adv,
                               budget,
                               tolerance,
                               db_path = NULL) {

  state <- list()

  if (!is.na(package)) {
    fun <- get(fun_name, envir=getNamespace(package), mode="function")
  }

  params <- formals(fun)               ## TODO: we can inspect default arg here
  num_params <- length(params)

  if (num_params == 0) {
    new_states <- lapply(seq(budget), function(x) run_fun(package, fun_name, fun, list()))
    state <- do.call(rbind, new_states)

  } else if (num_params > 9) {
    state <- list(package, fun_name, num_params, NA, NA, 3L, NA, NA)
    names(state) <- c("package", "fun_name", "num_param", "input", "output", "exitval", "warnmsg", "errmsg")
  } else {
    param_names <- names(params)

    ## history <- list()                  TODO: history used to be needed for generating unique permutation
    tol_left <- tolerance

    while (budget > 0) {

      if (length(state) == 0) {
        types_to_try <- generate_types_randomly(types, num_params)
        ## types_to_try <- lapply(seq(num_params), generate_types_randomly)
      } else {
        last_state <- state[nrow(state),]

        ## types_to_try <- feedback_adv(last_state, tol_left)

        switch(feedback_adv(last_state, tol_left),
               "generate_types_randomly" = {
                 types_to_try <- generate_types_randomly(types, params);
                 if(tol_left == 0) tol_left <- tolerance
               },
               "generate_types_by_errmsg" = {
                 types_to_try <- generate_types_by_errmsg(last_state$errmsg, types, params);
                 if(tol_left == 0) tol_left <- tolerance
               },
               "generate_types_repeat" = tol_left <- tol_left - 1
               )
      }

      ## mapply(function(name, type){
      ##   params[[name]] <<- value_generator(type)
      ## }, param_names, types_to_try)
      mapply(function(name, type) {
        if (type %in% DIM1) {
          params[[name]] <<- value_generator(type, n = 5)
        } else if (type %in% DIM2) {
          params[[name]] <<- value_generator(type, n = 2, col = 3)
        }
        ## else if (type == "db") {
        ##   params[[name]] <<- sample_val_from_db(path = db_path)
        ## }
        else {
          params[[name]] <<- value_generator(type)
        }
      }, param_names, types_to_try)

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
#' @param fun        function to manipulate tolerance left
#' @return           a set of types to try
feedback_adv <- function(state, tolerance) {
  feedback <- character()

  if (tolerance == 0 || state$exitval == 0L || state$exitval == 1L) {
    feedback <- "generate_types_randomly"
  } else if(tolerance == 0 && state$exitval == 2L) {
    feedback <- "generate_types_randomly" #TODO: upgrade to by_errmsg 
  } else {
    feedback <- "generate_types_repeat"
  }

  feedback
}

## feedback_adv <- function(state, tolerance, ...) {
##   if(tolerance == 0 || state$exitval == 0L || state$exitval == 1L) {
##     return(generate_types_randomly(...))
##   } else if (tolernace == 0 && state$exitval == 2L) {
##     return(generate_types_randomly)       #TODO: generate_types_by_errmsg(state$errmsg, ...)
##   } else {
##     return(generate_types_again(state, tolerance))
##   }
## }

generate_types_randomly <- function(types, params) {
  indices <- sample.int(length(types), length(params))
  random_perm <- lapply(indices, function(id) types[[id]])

  random_perm #TODO: generating a unique permutation and exhausting all permutations
}

generate_types_again <- function(state, tolerance) {
  
}

generate_types_by_errmsg <- function(err, types, params) {
  keywords <- c("non-numeric")
  all <- c(types, keywords)

  num_params <- length(params)

  msg <- err$message
  splited_msg <- strsplit(msg, "[` ]+")

  param_match <- which(params %in% splited_msg)
  type_match <- which(all %in% splited_msg)

  guided_perm <- list()

  ## if(type_match) {
  ##   if (param_match) {

  ##   } else {

  ##   }

  ## }

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
#' @param type           which type of value to generate
#' @param n   (optional) length or number of rows
#' @param col (optional) number of columns
#' @return
#' @export
generate_val_adv <- function(type, inner_type=NULL, n=NULL, col=NULL) {
  val <- sample.int(255, size = 1)
  l <- list()
  t <- sample.int(length(DIM0), size = 1)

  switch (type,
          "logical" =  as.logical(val),
          "integer" = val,
          "double" = as.double(val),      # three special values: Inf, -Inf, NaN
          "complex" = as.complex(val),
          "character" = as.character(val),
          "raw" = as.raw(val),
          "list" = {
            random_ts <- sample.int(length(DIM0), size = n, replace = TRUE)
            if (n == 0) {
              return(l)
            } else {
              mapply(function(i, t) l[[i]] <<- generate_val(DIM0[[t]]), seq(n), random_ts)
              l
            }
          },
          "dim1" = {
            if(is.null(inner_type)) {
              return(rep(n, x = generate_val(DIM0[[t]])))
            } else {
              return(rep(n, x = generate_val(inner_type)))
            }
          },
          "dim2" = {
            if(is.null(inner_type)) {
              matrix(generate_val(DIM0[[t]]), nrow = n, ncol = col)   #TODO: data.frame
            } else {
              matrix(generate_val(inner_type), nrow = n, ncol = col)   #TODO: data.frame
            }
          }
          )
}

sample_val_from_db <- function(db_path) {
  record::open_db(db_path, create = FALSE)

  val <- record::sample_val()
  record::close_db()

  val
}

