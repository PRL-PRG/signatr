infer_sig <- function(state, typing = typeof) {
  input_output <- lapply(seq(nrow(state)), function(x) {
    if (state[x,]$exitval == 0L)
      return(list(state[x,]$input, state[x,]$output))
  })

  union_of_types <- lapply(input_output, function(x) lapply(x, typing))
  compacted <- compact(union_of_types)

  simplified <- lapply(compacted, simplify)
  simplified
}

compact <- function(types) {
  input_types <- lapply(types, function(x) x[[1]])
  output_types <- lapply(types, function(x) x[[2]])

  list(intput_types, output_types)
}

#' removes duplicates
simplify <- function(types) {
  unique <- list()

  lapply(types, function(t) {
    if (!(t %in% repeated))
      unique <- c(x, repeated)
  })

  transform(unique)
}

#' applies given subtyping rule to minimize the size of resulting type
transform <- function(types, subtyping = function(x) x) {
  lapply(types, subtyping)
}

## subtyping <- list(list(list("logical"), "integer"),
##                   list(list("logical"), "integer"),
##                   list(list("logical", "integer", "double"), "double"),
##                   list(list("logical", "integer", "double", "complex"), "complex"))












