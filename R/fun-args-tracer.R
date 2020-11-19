#' @importFrom digest sha1
#' @importFrom purrr detect_index discard map_chr
#' @importFrom instrumentr is_successful is_vararg is_evaluated get_name get_parameters get_data get_result get_position get_arguments is_evaluated
trace_exit_callback <- function(context, application, package, func, call) {
  if (!instrumentr::is_successful(call)) {
    return()
  }

  fun_name <- get_name(func)
  package_name <- get_name(package)
  params <- get_parameters(call)

  data <- get_data(context)
  values <- data$values
  sources <- data$sources
  values_sources <- data$values_sources

  store_val <- function(val, pos) {
    ## if(class(val) != "instrumentr_parameter") {
    ##   pos <- 0 # return value pos
    ## }
    value_hash <- sha1(val)
    if (!exists(value_hash, envir=values)) {
      value_ser <- serialize(val, connection=NULL, ascii=FALSE)
      ## value <- list(value_hash, typeof(val), value_ser)
      value <- data.frame(value_hash, typeof(val), I(list(value_ser)))
      ## value <- data.frame(value_hash, typeof(val), I(list(val)))
      assign(value_hash, value, envir=values)
    }

    source_hash <- paste(package_name, fun_name, pos, sep=":")
    if (!exists(source_hash, envir=sources)) {
      source <- data.frame(source_hash, package_name, fun_name, pos)
      assign(source_hash, source, envir=sources)
    }

    value_source <- get0(value_hash, envir=values_sources)
    if (is.null(value_source)) {
      value_source <- new.env(parent=emptyenv())
      assign(value_hash, value_source, envir=values_sources)
    }

    count <- get0(source_hash, envir=value_source, ifnotfound=0)
    count <- count + 1
    assign(source_hash, count, envir=value_source)
  }

  return_val <- get_result(call)
  store_val(return_val, pos = 0)

  for (param in params) {
    pos <- get_position(param) + 1
    if (is_vararg(param)) {
      # for now we skip them
      next
    }

    args <- get_arguments(param)
    if (length(args) != 1) {
      next
    }

    arg <- args[[1]]
    if (!is_evaluated(arg)) {
      next
    }
    arg_value <- get_result(arg)

    store_val(arg_value, pos = pos)
  }
}

#' @export
#' @importFrom instrumentr create_context
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code
trace_fun_args <- function(package, code, substituted = FALSE) {
  context <- create_context(
    call_exit_callback = trace_exit_callback,
    packages = package
  )

  if(!substituted) {code <- substitute(code)}

  set_application_load_callback(context, function(context, application) {
    data <- new.env(parent=emptyenv())
    data$values <- new.env(parent=emptyenv())
    data$sources <- new.env(parent=emptyenv())
    data$values_sources <- new.env(parent=emptyenv())

    set_data(context, data)
  })

  set_application_unload_callback(context, process_traced_data)

  result <- trace_code(context, code, quote=FALSE)

  list(result=result, data=get_data(context))
}

#' @importFrom instrumentr get_data set_data
#' @importFrom purrr map_dfr
process_traced_data <- function(context, application) {
  data <- get_data(context)
  values <- data$values
  sources <- data$sources
  values_sources <- data$values_sources


  ## values <- as.list(values)
  ## values_df <- data.frame(Reduce(rbind, values), row.names = NULL)
  values_df <- do.call(rbind, as.list(values))

  if (!nrow(values_df) == 0) {
    rownames(values_df) <- NULL
    colnames(values_df) <- c("value_hash", "type", "raw_value")
  }

  sources_df <- do.call(rbind, as.list(sources))
  rownames(sources_df) <- NULL

  values_sources_df <- purrr::map_dfr(ls(values_sources), function(value_hash) {
    value_sources <- get(value_hash, envir=values_sources)
    purrr::map_dfr(ls(value_sources), function(source_hash) {
      count <- get(source_hash, value_sources)
      data.frame(value_hash, source_hash, count)
    })
  })

  data <- list(
    values=values_df,
    sources=sources_df,
    values_sources=values_sources_df
  )

  set_data(context, data)
}

#' @export
save_fun_args_data <- function(data, path) {
  # make sure you create the path if it does not exist
  if (!dir.exists(path)) {
    dir.create(path, recursive=TRUE)
  }

  # store data using saveRDS
  saveRDS(data$values, file = paste0(path, "/","values.RDS"))
  saveRDS(data$sources, file = paste0(path, "/","sources.RDS"))
  saveRDS(data$values_sources, file = paste0(path, "/","counts.RDS"))
}

#' @export
trace <- function(package, path, code) {
  code <- substitute(code)
  # call trace_fun_args
  result <- trace_fun_args(package, code, substituted = TRUE)
  # check results
  # store results using save_fun_args_data
  if(class(result$result$error) == "instrumentr_undefined"  && length(result$data) == 3){
    save_fun_args_data(result$data, path)
  }
  invisible(result)
}
