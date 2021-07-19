# TODO: type lists, vectors, scalars
# TODO: implement least upper bound for a list of types
# TODO: integrate with experimentation code

#' type: R Value -> "string"
#' Types non-function related R values according to TypeR typing.
#' Everything else is typed to "any".
#'
#' Examples:
#' type(1) -> "dbl"
#' type(function(x) {x + 1}) -> "any"
#'
#' @param val is an arbitrary R value
#' @return a string that represents the type of the R value per TypeR typing
#'
type <- function(val) {
  if (is.null(val)) {
    return("null")
  } else if (is.environment(val)) {
    return("env")
  } else if (is.s3(val)) {
    return(paste("class<", attributes(val)$class, ">", sep=""))
  } else if (length(val) == 1) {
    if (is.raw(val)) {
      return("raw")
    }
  }

  else {
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
