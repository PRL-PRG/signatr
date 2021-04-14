#' @export
hello_c <- function(name) {
	.Call(C_hello_c, name)
}
