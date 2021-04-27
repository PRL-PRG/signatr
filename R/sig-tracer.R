#' @export
#' @importFrom instrumentr trace_code
trace_expr <- function(code,
                       environment = parent.frame(),
                       quote = TRUE) {
  sig_tracer <- .Call(C_signatr_tracer_create)

  if(quote) {
    code <- substitute(code)
  }

  invisible(trace_code(sig_tracer, code, environment = environment, quote = FALSE))
}

#' @export
trace_file <- function(file, environment = parent.frame()) {
  code <- parse(file = file)

  code <- as.call(c(`{`, code))

  invisible(trace_expr(code, quote = FALSE))
}
