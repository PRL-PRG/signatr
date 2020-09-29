#' @importFrom digest sha1
#' @importFrom purrr detect_index discard map_chr
trace_exit_callback <- function(context, application, package, func, call) {
  #browser()
  if (!instrumentr::is_successful(call)) {
    return()
  }

  fun_name <- instrumentr::get_name(func)
  package_name <- instrumentr::get_name(package)
  params <- instrumentr::get_parameters(call)

  data <- instrumentr::get_data(context)
  values <- data$values
  sources <- data$sources
  values_sources <- data$values_sources

  # either returnValue() or instrumentr::get_result(call)
  # pos = -1

  for (param in params) {
    pos <- instrumentr::get_position(param)
    if (instrumentr::is_vararg(param)) {
      # for now we skip them
      next
    }

    args <- instrumentr::get_arguments(param)
    if (length(args) != 1) {
      next
    }

    arg <- args[[1]]
    if (!instrumentr::is_evaluated(arg)) {
      next
    }
    arg_value <- instrumentr::get_result(arg)

    # the value part
    value_hash <- sha1(arg_value)
    if (!exists(value_hash, envir=values)) {
      value_ser <- serialize(arg_value, connection=NULL, ascii=FALSE)
      value <- list(value_hash, typeof(arg_value), value_ser)
      assign(value_hash, value, envir=values)
    }

    source_hash <- paste(package_name, fun_name, pos, sep=":")
    if (!exists(source_hash, envir=sources)) {
      source <- data.frame(source_hash, package_name, fun_name, pos)
      assign(source_hash, source, envir=sources)
    }

    value_sources <- get0(value_hash, envir=values_sources)
    if (is.null(value_sources)) {
      value_sources <- new.env(parent=emptyenv())
      assign(value_hash, value_sources, envir=values_sources)
    }
    count <- get0(source_hash, value_sources, ifnotfound=0)
    count <- count + 1
    assign(source_hash, count, envir=value_sources)
  }
}

#' @export
#' @importFrom instrumentr create_context
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom instrumentr set_data get_data trace_code
trace_fun_args <- function(package, code) {
  context <- create_context(
    call_exit_callback = trace_exit_callback,
    packages = package
  )

  code <- substitute(code)

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

process_traced_data <- function(context, application) {
  data <- instrumentr::get_data(context)
  values <- data$values
  sources <- data$sources
  values_sources <- data$values_sources

  values <- as.list(values)

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
    values=values,
    sources=sources_df,
    values_sources=values_sources_df
  )

  instrumentr::set_data(context, data)
}

#' @export
save_fun_args_data <- function(data, dir) {
  # make sure you create the path if it does not exist
  if (dir.exists(dir)) {
    dir.create(dir, recursive=TRUE)
  }

  # TODO store data using saveRDS
}

#' @export
trace <- function(package, dir, code) {
  # TODO: call trace_fun_args
  # TODO: check results
  # TODO: store results using save_dun_args_data
}
