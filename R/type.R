#' @importFrom sxpdb open_db get_value_idx close_db
#' @importFrom purrr map map_lgl
#' @importFrom dplyr everything mutate select filter bind_rows
#' @importFrom stringr str_c str_starts
#' @importFrom progress progress_bar
#' @importFrom tibble tibble as_tibble
#' @export
traces_type <- function(traces, type_system, args_db) {
  if (is.character(traces)) {
    traces <- traces_load(traces)
  }

  if (is.character(args_db)) {
    args_db <- sxpdb::open_db(args_db)
    on.exit(sxpdb::close_db(args_db))
  }
  
  pb <- progress::progress_bar$new(
    format = "typing :fun [:bar] :percent eta: :eta",
    total = nrow(filter(traces, status == 0))
  )
  
  type_one_call <- function(args_idx, result, dispatch, fun_name, id, rdb, ...) {
    tryCatch({
      args <- purrr::map(args_idx[[1]], ~sxpdb::get_value_idx(args_db, .))
      
      # ignore S4 and LANGSXP
      if (any(purrr::map_lgl(args, ~ isS4(.) || is.language(.)))) {
        return(tibble(fun_name = character(0), id = integer(0), signature = character(0)))
      }
      
      ret <- sxpdb::get_value_idx(rdb, result)
      types <- type_system(args, ret, fun_name, dispatch[[1]])
      types$id <- id
      
      tmp <- types$args
      if (length(tmp) > 0) {
        names(tmp) <- stringr::str_c("..", names(tmp))
      }
      types$args <- NULL
      types <- c(types, tmp)
      
      types$signature <- stringr::str_c("(", str_c(tmp, collapse=", "), ") => ", types$retrn)
      
      if (!is.null(types$type_params)) {
        type_params <- as.list(types$type_params)
        type_params <- type_params[sort(names(type_params))]
        types$type_values <- stringr::str_c(names(type_params), collapse = "; ")
        types$type_params <- stringr::str_c(type_params, collapse = ", ")

        #if (nchar(types$type_params)) {
          #  types$signature <- str_c("[", str_c(types$type_params, collapse=", "), "]", types$signature)
        #} else {
          types$type_params <- NA
          types$type_values <- NA
        #}
      } else {
        #types$type_params <- NA
        #types$type_values <- NA
      }
      
      tibble::as_tibble(types)
    }, error=function(e) {
      message("Unable to type ", id, " record: ", e$message)
      tibble::tibble(fun_name = character(0), id = integer(0), signature = character(0))
    }, finally = {
      pb$tick(tokens = list(fun = fun_name))
    })
  }
  
  type_result_one_fun <- function(df) {
    fun_name <- df$fun_name[1]
    rdb_path <- df$rdb_path[1]
    
    tryCatch({
      rdb <- sxpdb::open_db(rdb_path)
      
      on.exit(sxpdb::close_db(rdb), add = TRUE)
      
      types <- purrr::map(seq_along(df$args_idx), function(idx) {
        t <- do.call(type_one_call, c(as.list(df[idx, ]), rdb = rdb))
        t
      })
      
      dplyr::bind_rows(types)
    }, error=function(e) {
      message("Error when processing: ", fun_name, "from: ", rdb_path, ": ",e$message)
      tibble(fun_name = character(0), id = integer(0), signature = character(0))
    })
  }
  
  traces <- dplyr::filter(traces, status == 0)
  funs <- unique(traces$fun_name)
  res <- purrr::map(funs, function(x) {
    types <- traces %>%
      dplyr::filter(fun_name == x) %>%
      type_result_one_fun()
    types %>%
      dplyr::mutate(fun_name = x) %>%
      dplyr::select(fun_name, id, signature, dplyr::everything())
  })
  
  names(res) <- funs
  res
}

#' @importFrom purrr map
type_system_hof <- function(args, ret, fun) {
  args_types <- purrr::map(args, fun)
  ret_type <- fun(ret)
  c(args = list(args_types), retrn = ret_type)
}

type_system_typeof <- function(args, ret, ...) type_system_hof(args, ret, typeof)

#' @importFrom contractr infer_type
type_system_tastr <- function(args, ret, ...) type_system_hof(args, ret, contractr::infer_type)

is_class_type <- function(x) stringr::str_starts(x, fixed("class<"))

is_list_type <- function(x) stringr::str_starts(x, fixed("list<"))

#' @importFrom contractr parse_type
parse_tastr_type <- function(x) {
  tryCatch({
    tastr_type <- contractr::parse_type(x)
  }, error=function(e) {
    stop("Unable to parse type `", x, "`: ", e$message)
  })
}

#' @importFrom contractr infer_type
infer_classless_type <- function(x) {
  tmp <- x
  class(tmp) <- NULL
  dim(tmp) <- NULL
  contractr::infer_type(tmp)
}
