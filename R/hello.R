hello <- function(name) {
	.Call(C_hello, name)
}
