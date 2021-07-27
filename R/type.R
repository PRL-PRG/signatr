# TODO: implement least upper bound for a list of types
# TODO: integrate with experimentation code

#' type: R Value -> list of strings
#' Types non-function related R values according to TypeR typing.
#' Everything else is typed to "any".
#'
#' Examples:
#' type(1) -> list("dbl")
#' type(function(x) {x + 1}) -> list("any")
#'
#' @param val is an arbitrary R value
#' @return a string that represents the type of the R value per TypeR typing
#'
type <- function(val) {
	if (is.null(val)) {
		return("null")
	} else if (is.s3(val)) {
		return(paste("class<", attributes(val)$class, ">", sep=""))
	} else if (is.environment(val)) {
		return("env")
	} else if (is.vector(val) && !is.list(val)) {
		res = ""
		if (is.raw(val)) {
			res = "raw"
		} else if (is.logical(val)) {
			res = "lgl"
		} else if (is.integer(val)) {
			res = "int"
		} else if (is.double(val)) {
			res = "dbl"
		} else if (is.complex(val)) {
			res = "clx"
		} else if (is.character(val)) {
			res = "chr"
		} else {
			stop("Unknown primitive vector type")
		}

		if (length(val) > 1) {
			## TODO: Deal with potential na values in the vector

			return(paste(res, "[", length(val), "]", sep=""))
		} else {
			return(res)
		}
	} else if (is.list(val)) {
			## TODO: Find a more precise typing for lists

		return("list<any>")
	} else {
		return("any")
	}
}

#' is.s3: R Value -> T|F
#' Assesses whether the value is a s3 object by checking for the class attribute
#'
#' Examples:
#' is.s3(1) -> F
#' is.s3(factor(c(1, 2, 3))) -> T
#'
#' @param val is an arbitrary R value
#' @return a logical that is either T or F
#'
is.s3 <- function(val) {
	if ("class" %in% attributes(attributes(val))$names) {
		return(T)
	}

	return(F)
}

#' lub: list of strings -> list of strings
#' Find the least upper bound for a list of strings that represent non-function
#' types
#'
#' Examples:
#' lub(list("raw", "lgl")) -> list("lgl")
#' lub(list("int", "clx", "chr")) -> list("chr")
#' lub(list("env", "int")) -> list("env", "int")
#' lub(list("null", "int")) -> list("?int")
#'
lub <- function(lot) {
	list("any")
}

#'
#'