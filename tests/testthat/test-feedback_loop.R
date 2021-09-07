test_that("feedback_loop constant_1 run 1", {
  res <- feedback_loop(fun_name = "constant_1",
                       fun = function() {1},
                       strategy = "perm",
                       budget = 1)


  expect_equal(nrow(res), 1)
  expect_equal(res$output_type, "double")
  expect_equal(res$sig, " -> double")
})

test_that("feedback_loop add_1 run 5", {
  res <- feedback_loop(fun_name = "add_1",
                       fun = function(x) {x+1},
                       strategy = "perm",
                       budget = 5)

  expect_equal(nrow(res), 5)
  expect_equal(res[5,]$output_type, "double")
})

## test_that("feedback_loop add_1 run 5", {
##   res <- feedback_loop(fun_name = "add_1",
##                        fun = function(x) {x+1},
##                        budget = 7,
##                        tolerance = 1)

##   expect_equal(!all(res[,6]), TRUE) # at least one success 
## })

## test_that("feedback_loop exhaust all types so early exit", {
##   res <- feedback_loop(fun_name = "add_1",
##                        fun = function(x, y) {x+7},
##                        budget = 100,
##                        tolerance = 1)
##   expect_equal(!all(res[,6]), TRUE) # at least one success 
## })
