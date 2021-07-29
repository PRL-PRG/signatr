### Currently we are attempting to work on function signature inference for
### functions that do not take another function as its input. We'll call types
### for values that can be used as inputs for functions value types. We'll call
### types for functions function types.
###
### The representation for a value type is a character vector that contain only
### one element.
### e.g. "list", "real", "integer", "character"
###
### The representation for a function type is a list that contain two character
### vectors. The first character vector is a list of types, where each type is
### represents the type for an argument to the function in order of formals.
### The second character vector contain only 1 element that represent the type
### of the output value. "NA" is used for input types if the function does not
### take any arguments.
### e.g. [["NA", "character"]], [[c("real", "real"), "real"]]
###
### The representation for the call is a list that contain two elements. The
### first element is the formals list. The second element is the resulting
### value, or a special character value in case the function issued warning or
### error when ran with the formals.
###
### The call list is a list of call.
###
### The result of using our system is a list of function types.
###
### REACH GOALS: find ways to deal with side effects
###
### TODO: Get actual data

#' Runs the signatr inference system.
#'
#' @param f is the function whose type we want to infer.
#' @param budget is the budget that we are allocating to this attempt.
#' @param is_good_enough is a function that assess if our current result is good enough.
#' is_good_enough must understand the call list, type list, and budget
#' @return is a list of function types.
#'
#' Examples: run_inference_system(myAdd, 10, reached_fixed_point)
run_inference_system <- function(f, budget, is_good_enough) {
	populate_formals <- function(f, budget, initial_types, initial_calls) {
		# This part will be the most complicated part.
		# This function must understand what kinds of moves it has and how much
		# it costs to use a move.
		result = list()
		result[[1]] = budget - 1
		result[[2]] = formals(f)
		return(result)
	}

	run_function <- function(f, budget, params) {
		current_type = list()
		current_call = list()
		if (length(params) == 0) {
			current_type[[1]] = NA
			current_call[[1]] = "EMPTY"

			tryCatch({
				output = f()
				current_type[[2]] = type(output)
				current_call[[2]] = output
			}, warning = function(warn) {
				current_type[[2]] = "WARNING"
				current_call[[2]] = "WARNING"
			}, error = function(err) {
				current_type[[2]] =  "ERROR"
				current_call[[2]] =  "ERROR"
			})
		} else {
			current_type[[1]] = lapply(params, type)
			current_call[[1]] = params

			tryCatch({
				output = do.call(f, as.list(params))
				current_type[[2]] = type(output)
				current_call[[2]] = output
			}, warning = function(warn) {
				current_type[[2]] = "WARNING"
				current_call[[2]] = "WARNING"
			}, error = function(err) {
				current_type[[2]] = "ERROR"
				current_call[[2]] = "ERROR"
			})
		}

		result =  list()
		result[[1]] = budget - 1 # TODO: Adjust this if call takes a long time
		result[[2]] = current_type
		result[[3]] = current_call
	}

	infer_signature <- function(f, budget, initial_types, initial_calls) {
		if (budget > 0) {
			res = populate_formals(f, budget, initial_types, initial_calls)
			budget = res[[1]]
			params = res[[2]]

			output = run_function(f, budget, params)
			budget = output[[1]]
			initial_types[[ length(initial_types) + 1 ]] = output[[2]]
			initial_calls[[ length(initial_calls) + 1 ]] = output[[3]]
		}

		result = list()
		result[[1]] == budget
		result[[2]] == initial_types
		result[[3]] == initial_calls
		return(result)
	}

	is_good_enough <- function(budget,
							   initial_types,
							   initial_calls,
							   current_types,
							   current_calls) {
		if (budge > 0) {
			return(T)
		}
	}

	initial_types = list()
	initial_calls = list()
	current_types = list()
	current_calls = list()
	while(!is_good_enough(budget,
						  initial_types,
						  initial_calls,
						  current_types,
						  current_calls)) {
		initial_types = current_types
		initial_calls = current_calls

		result = infer_signature(f, budget, initial_types, initial_calls)
		budget = result[[1]]
		current_types = result[[2]]
		current_calls = result[[3]]
	}

	return(current_types)
}

#' Run the function signature inferer to find a fix point or exhaust budget.
#'
#' @export
#' @param f is the function whose type we want to infer
#' @param fix_point_budget is the number to tests to see if is at a fix point
#'
#' The type_infer memthod understands \code{attempt_budget} and \code{generate_value} and the type system use by other parameters
#' @param type_infer is the method for infer a list of types for function f
#' @param attempt_budget is the budget for \code{type_infer}
#' @param generate_value is a function that will return some value
#'
#' The following three params all share the same understanding of type
#' @param type is a function that will find the type of R values
#' @param type_unify is a function that will find the LUB of a list of types
#' @param initial_typing is a list of types that is a guess for type of f
#' @return a list of types
#'
run_infer_function_signature <- function(f,
										 fix_point_budget = 1,
										 type_infer,
										 attempt_budget = 1,
										 generate_value = function() { 1 },
										 type = typeof,
										 type_unify = function(x) { x },
										 initial_typing = list()) {
	current_typing = list()
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
			break
		}
	}

	return(current_typing)
}


#' Try to infer a function type by running it with generated values.
#'
#' @export
#' @param f is the function whose type we want to infer
#' @param budget is a non-negative integer that represents the number of trials
#' @param generate_value is a function that will return some value
#' @param type is a function that will find the type of R values
#' @param type_unify is a function that will find the LUB of a list of types
#' @param initial_typing is an initial guess for the type of \code{f}
#' @return a list of types for \code{f}
#'
infer_function_signature <- function(f,
									 budget = 1,
									 generate_value = function() { 1 },
									 type = typeof,
									 type_unify = function(x) { x },
									 initial_typing = list()) {
	params = populate_formals(f, list(), generate_value)
	types = initial_typing
	for (i in 1:budget) {
		current_type = list()
		if (length(params) == 0) {
			current_type[[1]] = NA

			tryCatch({
				output = f()
				current_type[[2]] = type(output)
			}, warning = function(warn) {
				current_type[[2]] = "WARNING"
			}, error = function(err) {
				current_type[[2]] =  "ERROR"
			})

			if (length(current_type) == 1) {
				current_type[[2]] = NA
			}

			types[[length(types) + 1]] = current_type
		} else {
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


#' Populate formal argument of the given function with test values
#'
#' @param f is a function that we want to fill the formal argument for.
#' @param previous_results is a list of results of using arguments provide by this function
#' @param generate_value is a function that generates R values
#' @param generator_state is a potential input for generate_value (TODO)
#' @return arguments that can be used with \code{f}
populate_formals <- function(f,
							 previous_results = list(),
							 generate_value = function() { 1 }) {
	# Create a set of values that can be passed into f
	params = formals(f)
	if (length(params) == 0) {
		NULL
	} else {
		param_names = names(params)
		for (name in param_names) {
			params[name] = generate_value()
		}

		return(params)
	}
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

