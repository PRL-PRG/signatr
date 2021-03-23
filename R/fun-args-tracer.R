#' @importFrom instrumentr is_vararg is_evaluated get_name get_parameters get_result get_position get_arguments
#' @importFrom record add_val
trace_exit_callback <- function(context, application, package, func, call) {
  if (!instrumentr::is_successful(call)) {
    return()
  }

  ## fun_name <- get_name(func)
  ## package_name <- get_name(package)
  params <- get_parameters(call)

  return_val <- get_result(call)
  add_val(return_val)

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
    add_val(arg_val)
  }
}

#' @export
#' @importFrom instrumentr create_context trace_code get_data
#' @importFrom instrumentr set_application_load_callback set_application_unload_callback
#' @importFrom record open_db close_db
trace_fun_args <- function(package, code, substituted = FALSE) {
  context <- create_context(
    call_exit_callback = trace_exit_callback,
    packages = package
  )

  if(!substituted) {code <- substitute(code)}

  set_application_load_callback(context, function(context, application) {
    open_db(paste0("../tests/db/", package), create = TRUE)
  })

  set_application_unload_callback(context, function(context, application) {
    close_db()
  })

  result <- trace_code(context, code, quote=FALSE)

  list(result=result, data=get_data(context))
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
