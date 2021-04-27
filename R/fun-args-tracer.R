save_val <- function(val, pos, package_name, fun_name, values, sources, values_sources) {
  value_hash <- sha1(deparse1(val))

  if (!exists(value_hash, envir=values)) {
    value_ser <- serialize(val, connection=NULL, ascii=FALSE)
    value <- list(hash = value_hash, type = typeof(val), value = val)
    assign(value_hash, value, envir=values)
  }

  source_hash <- paste(package_name, fun_name, pos, sep=":")
  if (!exists(source_hash, envir=sources)) {
    source <- list(source_hash, package_name, fun_name, pos)
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


#' @importFrom digest sha1
#' @importFrom purrr detect_index discard map_chr
#' @importFrom instrumentr is_successful is_vararg is_evaluated get_name get_parameters get_data get_result get_position get_arguments is_evaluated
trace_exit_callback <- function(context, application, package, func, call) {
  tictoc::tic(paste0("collecting values from ", get_name(package), "::", get_name(func)))
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

  return_val <- get_result(call)
  save_val(return_val, 0, package_name, fun_name, values, sources, values_sources)

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
    arg_val <- get_result(arg)

    save_val(arg_val, pos, package_name, fun_name, values, sources, values_sources)
  }

  time <- tictoc::toc()
  print(time$msg)

  df <- data.frame(pckg = package_name, f = fun_name, num_vals = length(params)+1, time = time$toc - time$tic)
  write.csv(df, paste0("csv/", package_name, "::", fun_name, ".csv"), row.names=FALSE)
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
  tictoc::tic("processing values")
  data <- get_data(context)
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

  set_data(context, data)
  time <- tictoc::toc()
  print(time$msg)
  write.csv(time, "process_time.csv", row.names=FALSE)
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
