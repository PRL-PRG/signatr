test_that("fun feedback_loop on constant_1 with budget 1", {
  res <- feedback_loop(fun_name = "constant_1",
                       fun = function() {1},
                       strategy = "perm",
                       budget = 1)


  expect_equal(nrow(res), 1)
  expect_equal(res$output_type, "double")
  expect_equal(res$sig, " -> double")
})

test_that("run feedback_loop on add_1 with budget 5", {
  res <- feedback_loop(fun_name = "add_1",
                       fun = function(x) {x+1},
                       strategy = "perm",
                       budget = 5)

  expect_equal(nrow(res), 5)
  expect_equal(res[5,]$output_type, "double")
})

test_that("infer sig of add_1 with random values from test_db", {
  res <- feedback_loop(fun_name = "add_1",
                       fun = function(x) {x+1},
                       strategy = "random-db",
                       budget = 5,
                       db_path = "db/test_db")

  success <- res[res$exitval == 0, ]
  print(success$sig)
})

test_that("infer sig of stringr::str_detect with random values from test_db", {
  res <- feedback_loop(package = "stringr",
                       fun_name = "str_detect",
                       strategy = "random-db",
                       budget = 100,
                       db_path = "db/test_db")

  success <- res[res$exitval == 0, ]
  print(success$sig)
})

## test_that("feedback_loop exhaust all types so early exit", {
##   res <- feedback_loop(fun_name = "add_1",
##                        fun = function(x, y) {x+7},
##                        budget = 100,
##                        tolerance = 1)
##   expect_equal(!all(res[,6]), TRUE) # at least one success 
## })
