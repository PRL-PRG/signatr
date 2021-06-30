#' Try to infer a function type by running it with generated values.
#'
#' @export
#' @param f is the function whose type we want to infer
#' @param budget is a non-negative integer that represents the number of trials
#' @param generate_value is a function that will return some value
#' @param type is a function that will find the type of R values
#' @param type_unify is a function that will find the LUB of a list of types
infer_function_signature <- function(f,
									 budget = 1,
									 generate_value = function() { 1 },
									 type = typeof,
									 type_unify = function(x) { x }) {

	params = formals(f)
	param_names = names(params)
	types = list()
	for (i in 1:budget) {
		current_type = list()
		if (length(params) == 0) {
			current_type[[1]] = NA

			tryCatch({
				output = f()
				current_type[[2]] = type(output)
			}, warning = function(warn) {
				print(warn)
			}, error = function(err) {
				print(err)
			})

			if (length(current_type) == 1) {
				current_type[[2]] = NA
			}

			types[[length(types) + 1]] = current_type
		} else {
			for (name in param_names) {
				params[name] = generate_value()
			}

			current_type[[1]] = lapply(params, type)

			tryCatch({
				output = do.call(f, as.list(params))
				current_type[[2]] = type(output)
			}, warning = function(warn) {
				print(warn)
			}, error = function(err) {
				print(err)
			})

			if (length(current_type) == 1) {
				current_type[[2]] = NA
			}

			types[[length(types) + 1]] = current_type
		}
	}

	return(type_unify(types))
}

#' Print the inferred function types
#'
#' @export
#' @param types are the types inferred by the infer_function_signature function
print_types <- function(types) {
	lapply(types, print_type)
}

#' Print the inferred function type
#'
#' @param type is a type inferred by the infer_function_signature function.
print_type <- function(type) {
	args = type[[1]]
	output = type[[2]]

	if (length(args) == 1 && is.na(args[1])) {
		print(paste("NA", output, sep = " -> "))
	} else {
		input_types = paste(args, collapse=" x ")
		print(paste(input_types, output, sep = " -> "))
	}
}