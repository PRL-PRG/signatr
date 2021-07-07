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
									 type_unify = function(x) { x },
									 initial_typing = list()) {

	params = formals(f)
	param_names = names(params)
	types = initial_typing
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
				# Do nothing
			}, error = function(err) {
				# Do nothing
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

#' Run the function signature inferer to find a fix point or exhaust budget.
#'
#' @export
#' @param f is the function whose type we want to infer
#' @param fix_point_budget is the budget of attempts to find the fix point
#' @param initial_typing is the current guess at the types of the function
#' @param type_infer is the method for infer a list of types for function f
#' @param attempt_budget is the number of attempts between fix point assessment
#' @param generate_value is a function that will return some value
#' @param type is a function that will find the type of R values
#' @param type_unify is a function that will find the LUB of a list of types
run_infer_function_signature <- function(f,
										 fix_point_budget = 1,
										 type_infer,
										 attempt_budget = 1,
										 generate_value = function() { 1 },
										 type = typeof,
										 type_unify = function(x) { x }) {
	current_typing = list()
	initial_typing = list()
	for (i in 1:fix_point_budget) {
		current_typing = type_infer(f,
									attempt_budget,
									generate_value,
									type,
									type_unify,
									initial_typing)

		done = T
		for (typing in current_typing) {
			if (!contain_type(initial_typing, typing)) {
				done = F
				initial_typing = current_typing
				break
			}
		}

		if (done) {
			return(current_typing)
		}
	}

	return(current_typing)
}

#' Check to see if two non-nested lists contain the same elements
#'
#' @param list1 is a non-nested list
#' @param list2 is another non-nested list
contain_same_elements <- function(list1, list2) {
	for (element in list1) {
		if (!(element %in% list2)) {
			return(F)
		}
	}

	for (element in list2) {
		if (!(element %in% list1)) {
			return(F)
		}
	}

	return(T)
}

#' Check to see if two types are the same.
#'
#' @param type1 is a type
#' @param type2 is another type
is_same_type <- function(type1, type2) {
	input1 = type1[[1]]
	output1 = type1[[2]]
	input2 = type2[[1]]
	output2 = type2[[2]]

	return(contain_same_elements(input1, input2) &&
		   contain_same_elements(output1, output2))
}

#' See if a type is in the list of types.
#'
#' @param types is a list of that contains many element of type
#' @param type is a function type, which is represented as a list of 2 elements
contain_type <- function(types, type) {
	for(t in types) {
		if (is_same_type(type, t)) {
			return(T)
		}
	}
	return(F)
}

