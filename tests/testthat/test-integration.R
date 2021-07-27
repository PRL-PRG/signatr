if (T) {

test_that("Simple Integration", {
	result = feedback_loop("stringr",
						   "str_detect",
						   budget = 343,
						   tolerance = 1)
	expect_true(length(result) > 1)

	rows = dim(result)[1]
	# cols = dim(result)[2]

	print(rows)
	# print(cols)

	argument_types = list()
	return_types = list()
	for(i in 1:rows) {
		arguments = result[i, 4]
		return_value = result[i, 5]

		n = length(arguments$input)
		arg_types = list()
		for (i in 1:n) {
			arg_types[[ length(arg_types) + 1 ]] = type(arguments$input[[i]])
		}

		argument_types[[ length(argument_types) + 1 ]] = arg_types

		if (is.na(return_value[[1]])) {
			return_types[[ length(return_types) + 1 ]] = NA
		} else  {
			return_types[[ length(return_types) + 1 ]] =  type(return_value[[1]])
		}
	}

	types = list()
	for (i in 1:rows) {
		if (!is.na(return_types[[i]])) {
			result = argument_types[[i]][[1]]
			for (j in 2:length(argument_types[[i]])) {
				result = paste(result, " x ", sep = "")
				result = paste(result, argument_types[[i]][[j]], sep = "")
			}

			result = paste(result, " -> ", return_types[[i]])

			types[[ length(types) + 1 ]] =  result
		}
	}

	print(unique(types))
})

}