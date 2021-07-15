# globals
builtin_types <- list("logical", "integer", "double", "complex", "character", "raw", "list")
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
    new_states <- lapply(seq(budget), function(x) run_fun(fun, list()))
    state <- do.call(rbind, new_states)

  } else {
    param_names <- names(params)
    history <- list()
    tol <- tolerance

    ## perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE) # n^r
    ## id <- 1

    while (budget > 0) {
      if (length(history) == 0 || tol == 0 || (nrow(state) != 0 && state[nrow(state),]$exitval == 0L)) {
        new_types <- generate_type(history, types, num_params)
        history <- append(history, list(new_types))
        ## new_types <- perms[id,]
        ## id <- id + 1

        if (tol == 0 ) tol <- tolerance

        if (length(new_types) == 0) {
          print("we've tried all types!")
          break
        } else {
          mapply(function(name, type) {params[name] <<- value_generator(type)}, param_names, new_types)
        }

      } else {
        old_types <- history[[length(history)]]
        mapply(function(name, type) {params[name] <<- value_generator(type)}, param_names, old_types)
        tol <- tol - 1
      }

      args <- params
      new_state <- run_fun(fun, args)
      state <- rbind(state, new_state)
      budget <- budget - 1
    }
  }

  rownames(state) <- NULL
  state
}


run_fun <- function(fun, args) {
  res <- tryCatch ({
    output <- do.call(fun, as.list(args))
    list(args, output, 0L, NA, NA)
  }, warning = function(w) {
    list(args, NA, 1L, w, NA)
  }, error = function(e) {
    list(args, NA, 2L, NA, e)
  })

  names(res) <- c("input", "output", "exitval", "warnmsg", "errmsg")
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


# history : list of lists
# types : list
# num : double
#' generates a new combination of types to try
#' @param history   types tried so far
#' @param types     to choose from
#' @param num       number of types to generate
#' @return          a list of types that haven't been tried
generate_type <- function(history, types, num) {
  combined <- list()

  for (i in seq(num)) {
    combined[[i]] <- lapply(history, function(old_types) old_types[[i]])
  }

  available_types <- lapply(combined, function(old_types) {
    types[!(types %in% old_types)]
  })

  not_exhausted <- purrr::reduce(available_types, function(x, y) x + length(y), .init=0)

  new_types <- list()

  if(not_exhausted) {
    new_types <- lapply(available_types, function(ts) {
      size <- length(ts)
      if (size == 0) {   # there is no more types left to try
        rand <- sample.int(length(types), 1)
        return(types[[rand]])
      } else {
        rand <- sample.int(size, 1)
        return(ts[[rand]])
      }
    })
  }

  return(new_types)
}


print_state <- function(x) {
  print(x)
}






