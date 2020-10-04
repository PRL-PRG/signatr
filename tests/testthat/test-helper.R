# --------------------------------------------------------------------------- #

## Setting Up Test Environment
# Test Function
add1 <- function(x) {
  return(x + 1)
}

# Test Library
library(covr)

test_that("Test Get Function 1", {
  expect_equal({a = get_function(NULL, "add1"); add1(1)}, 2)
})

test_that("Test Get Function 2", {
  expect_equal(get_function("covr", "report"), covr::report)
})

test_that("Test Get Function 3", {
  expect_true({f = get_function("covr", "bad_function"); is.null(f)})
})

# --------------------------------------------------------------------------- #

