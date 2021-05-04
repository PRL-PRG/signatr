#' @export
#' @importFrom instrumentr trace_code
trace_vals <- function(code,
                       environment = parent.frame(),
                       quote = TRUE) {
  val_tracer <- .Call(C_val_tracer_create)

  if(quote) {
    code <- substitute(code)
  }

  invisible(trace_code(val_tracer, code, environment = environment, quote = FALSE))
}

#' @export
trace_file <- function(file, environment = parent.frame()) {
  code <- parse(file = file)

  code <- as.call(c(`{`, code))

  invisible(trace_vals(code, quote = FALSE))
}
