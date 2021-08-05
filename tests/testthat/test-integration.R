if (T) {

test_that("Simple Integration", {
	result = feedback_loop("stringr",
						   "str_detect",
						   budget = 343,
						   tolerance = 1)
	expect_true(length(result) > 1)

	types = type_function(result)

	expect_true(length(types) >= 1)

	print(types)
	assess_score(result, result)
})

}