# Types
DIM0 <- c("logical", "integer", "double", "complex", "character", "raw")
DIM1 <- c("list", "dim1")
DIM2 <- c("dim2")
BUILTIN <- c(DIM0, "list")
DB <- "db"
TYPES <- c(DIM0, DIM1, DIM2)

#' runs the given function with values from value generator
#' @param  package         package name
#' @param  fun_name        function name
#' @param  fun             function to infer type signature
#' @param  types           a list of types for typing
#' @param  value_generator a function that provides arguments
#' @param  strategy        how to pick the types for each argument in the next run
#' @param  budget          number of calls to fun
## @param  tolerance       number of calls that applies values of the same type permutation
#' @return
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' feedback_loop(fun_name = "add_1", fun = function(x) {x+1}, strategy = "precompute", budget = 7)
#' feedback_loop("stringr", "str_detect", strategy = "random-feedback", budget = 343)
feedback_loop <- function (package = NA,
                           fun_name,
                           fun,
                           types = TYPES,
                           value_generator = generate_val,
                           strategy = c("precompute", "precompute-random", "random", "random-feedback", "random-db"),
                           budget,
                           ## tolerance,
                           db_path = NULL) {
  print(fun_name)

  states <- list()

  if (!is.na(package)) {
    fun <- get(fun_name, envir=getNamespace(package), mode="function")
  }

  params <- formals(fun)               ## TODO: we can inspect default arg here
  num_params <- length(params)

  if (num_params == 0) {
    new_states <- lapply(seq(budget), function(x) run_fun(package, fun_name, fun, list()))
    states <- do.call(rbind, new_states)

  } else {
    param_names <- names(params)

    types_to_try <- list()

    switch(strategy,
           "precompute" = {
             #matrix, array
             #complexity: n^r (7^10 is the limit)
             perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE)

             if (nrow(perms) > budget) perms <- perms[1:budget, ,drop=FALSE]

             ## 1. pick each permutation in the original order (perms row-by-row)
             apply(perms, MARGIN=1, FUN=function(perm) {
               types_to_try <- perm

               mapply(function(name, type) {params[[name]] <<- value_generator(type, n = 3, col = 2)}, param_names, types_to_try)

               states <<- update_states(states, package, fun_name, fun, params)
             })
           },
           "precompute-random" = {
             #matrix, array
             #complexity: n^r (7^10 is the limit)
             perms <- gtools::permutations(n=length(types), r=num_params, v=types, repeats.allowed=TRUE)

             if (nrow(perms) > budget) perms <- perms[1:budget, ,drop=FALSE] else budget <- nrow(perms)

             ## 2. randomize the pick of permutations
             order <- sample.int(nrow(perms), budget, replace=FALSE)

             lapply(seq(order), function(id) {
               types_to_try <- perms[id, ,drop=FALSE]

               mapply(function(name, type) {params[[name]] <<- value_generator(type, n = 3, col = 2)}, param_names, types_to_try)

               states <<- update_states(states, package, fun_name, fun, params)
             })
           },
           "random" = {
             while (budget > 0) {
               types_to_try <- generate_types_randomly(types, param_names)

               mapply(function(name, type){
                 params[[name]] <<- value_generator(type, n=3, col=2)
               }, param_names, types_to_try)

               states <- update_states(states, package, fun_name, fun, params)
               budget <- budget - 1
             }
           },
           "random-feedback" = {
             while(budget > 0) {
               if (length(states) == 0) {
                 generate_types <- generate_types_randomly
               } else {
                 last_state <- states[nrow(states), ]
                 generate_types <- feedback(last_state)
               }

               types_to_try <- generate_types(types, param_names, last_state)

               browser(expr = {length(types_to_try) > 10})

               if(is_permutations(types_to_try)) {
                 perms <- types_to_try

                 if (nrow(perms) > budget) perms <- perms[1:budget, ,drop=FALSE]

                 apply(perms, MARGIN=1, FUN=function(perm) {
                   types_to_try <- perm

                   mapply(function(name, type) {
                     browser()
                     params[[name]] <<- value_generator(type, n = 3, col = 2)},
                     param_names, types_to_try)

                   states <<- update_states(states, package, fun_name, fun, params)
                   budget <<- budget - 1
                 })
               } else {
                 mapply(function(name, type){
                   params[[name]] <<- value_generator(type, n=3, col=2)
                 }, param_names, types_to_try)

                 states <- update_states(states, package, fun_name, fun, params)
                 budget <- budget - 1
               }
             }
           },
           "random-db" = {
             if (is.null(db_path)) stop("missing the path to db")
             record::open_db(db_path, create = FALSE)

             while (budget > 0) {
               lapply(param_names, function(name) params[[name]] <<- value_generator(db=TRUE, db_path=db_path))

               states <- update_states(states, package, fun_name, fun, params)
               budget <- budget - 1
             }

             record::close_db()
           },
           )
  }

  rownames(states) <- NULL
  states
}

is_permutations <- function(sth) {
  !is.null(nrow(sth))
}

update_states <- function(states, package, fun_name, fun, params) {
  args <- params
  new_state <- run_fun(package, fun_name, fun, args)
  purrr::map_dfr(list(states, new_state), function(x) x)
}

#' decides whether to use a new set of types in the next run or not
## @param history    sets of types used so far
#' @param state      state of the last run
## @param tolerance  how many times to try the same set of types
#' @return           a set of types to try
feedback <- function(state) {
  strategy <- if(state$exitval == 0L || state$exitval == 1L) {
                generate_perms_fixed
              } else {
                generate_types_semi_randomly
              }
  strategy
}


generate_types_randomly <- function(types, param_names, state=NULL) {
  indices <- sample.int(length(types), length(param_names), replace = TRUE)
  random_perm <- lapply(indices, function(id) types[[id]])

  random_perm
}

generate_types_semi_randomly <- function(types, param_names, state=NULL) {
  input_type <- state$input_type
  input_types <- stringr::str_split(input_type, " x ")[[1]]

  lapply(input_types, function(type) sample(setdiff(types, type), 1))
}

generate_unique_types <- function(types, param_names, state=NULL) {
  #TODO
}


generate_perms_fixed <- function(types, param_names, state=NULL) {
  #TODO: length(param_names == 1)
  input_type <- state$input_type
  input_types <- stringr::str_split(input_type, " x ")[[1]]

  num_params <- length(param_names)
  temp_perms <- gtools::permutations(n=length(types), r=num_params-1, v=types, repeats.allowed=TRUE)

  res <- data.frame()

  mapply(function(type, i) {
    fixed_perms <- cbind(type, temp_perms)
    colnames(fixed_perms) <- c(param_names[[i]], param_names[-i])
    res <<- rbind(res, fixed_perms)
    }, input_types, seq(num_params))

  as.matrix(res)
}


generate_types_again <- function(types, param_names, state=NULL) {
  state$input_type
}


#' runs given function with args and stores the result in a list
#' @param package    package name if the function is a library function
#' @param fun_name   function name
#' @param fun        a closure
#' @param args       arguments to run the function with
#' @return           list of metadata and running result
#' @importFrom tibble  tibble
#' @importFrom contractr  infer_type
#' @export
run_fun <- function(package = NA, fun_name, fun, args) {
  input_type <- lapply(args, contractr::infer_type)
  combined <- paste0(input_type, collapse=" x ")

  res <- tibble::tibble(package=package, fun_name=fun_name, num_param=length(args), input_type=combined, output_type=NA, exitval=NA, warnmsg=NA, errmsg=NA, sig=NA)

  tryCatch (withCallingHandlers({
    output <- do.call(fun, as.list(args))
    output_type <- contractr::infer_type(output)

    res$output_type <- output_type
    if (is.na(res$exitval)) res$exitval <- 0L
    res$sig <- paste(combined, output_type, sep=" -> ")
  },
  warning = function(w) {
    res$exitval <<- 1L
    res$warnmsg <<- as.character(w)
    invokeRestart("muffleWarning")
  }),
  error = function(e) {
    res$exitval <<- 2L
    res$errmsg <<- as.character(e)
  })

  res
}


#' generates values based on the type
#' @param type                  type of value to generate
#' @param inner_type (optional) types of each element in heterogeneous types like list
#' @param n   (optional)        length or number of rows
#' @param col (optional)        number of columns
#' @return
#' @export
generate_val <- function(type=NULL, inner_type=NULL, n=NULL, col=NULL, db = FALSE, db_path=NULL) {
  val <- sample.int(255, size = 1)
  l <- list()
  ## t <- sample.int(length(DIM0), size = 1)
  T <- sample(DIM0, 1)

  if(db) {
    return(sample_val_from_db(db_path, type=type))
  }

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
              mapply(function(i, t) l[[i]] <<- generate_val(T, seq(n), random_ts))
              l
            }
          },
          "dim1" = {
            if(is.null(inner_type)) {
              return(rep(n, x = generate_val(T)))
            } else {
              return(rep(n, x = generate_val(inner_type)))
            }
          },
          "dim2" = {
            if(is.null(inner_type)) {
              matrix(generate_val(T), nrow = n, ncol = col)   #TODO: data.frame
            } else {
              matrix(generate_val(inner_type), nrow = n, ncol = col)   #TODO: data.frame
            }
          }
          )
}

sample_val_from_db <- function(db_path, type=NULL) {
  val <- if(is.null(type)) {
           record::sample_val()
         } else {
           record::sample_val(type=type)
         }

  val
}


#' @param      ddata  generated from add_signature
#' @return     the number of unique signatures
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' data <- feedback_loop("stringr", "str_detect", strategy = "perm", budget = 343)
#' res <- add_signature(data)
#' num_sigs(res)
num_suc_sigs <- function(data) {
  sucs <- data[data$exitval == 0L, ,drop=FALSE]
  warns <- data[data$exitval == 1L, ,drop=FALSE]

  sucs_or_warns <- purrr::map_dfr(list(sucs, warns), function(x) x)

  length(unique(sucs_or_warns$input_type))
}

#'@export
compute_suc <- function(data) {
  percent <- num_suc_sigs(data)/length(unique(data$input_type)) * 100

  round(percent, digits=1)
}
