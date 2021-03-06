zero <- function() { 0 }
one <- function(x) { 1 }
add <- function(a, b) { a + b }
bad <- function() { stop("hammer time") }

if (T) {

test_that("basic function inference experiment 0 args", {
	expect_true(length(infer_function_signature(zero, 1)) == 1)
})

test_that("basic function inference experiment 1 arg", {
	expect_true(length(infer_function_signature(one, 2)) == 2)
})

test_that("basic function inference experiment 2 args", {
	expect_true(length(infer_function_signature(add, 3)) == 3)
})

test_that("new type unification experiment 0 args", {
	expect_true(length(infer_function_signature(zero, 10,
												type_unify = unique)) == 1)
})

test_that("new type unification experiment 1 arg", {
	expect_true(length(infer_function_signature(one, 10,
												type_unify = unique)) == 1)
})

test_that("new type unification experiment 2 arg", {
	expect_true(length(infer_function_signature(add, 10,
												type_unify = unique)) == 1)
})

test_that("erroring function experiment", {
	expect_true(length(infer_function_signature(bad, 10,
												type_unify = unique)) == 1)
})

test_that("printing function experiment", {
	types = infer_function_signature(zero, 10, type_unify = unique)
	print_types(types)

	types = infer_function_signature(one, 10, type_unify = unique)
	print_types(types)

	types = infer_function_signature(add, 10, type_unify = unique)
	print_types(types)

	types = infer_function_signature(bad, 10, type_unify = unique)
	print_types(types)

	expect_true(1 == 1)
})

test_that("simple runner test", {
	res = run_infer_function_signature(zero,
										fix_point_budget = 10,
										type_infer = infer_function_signature,
										attempt_budget = 10,
										type = typeof,
										type_unify = unique)
	expect_true(length(res) == 1)
	print_types(res)
})

}