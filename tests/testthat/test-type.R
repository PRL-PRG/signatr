if (T) {

test_that("understand null type", {
  expect_equal(type(NULL), "null")
})

test_that("understand environment type", {
  expect_equal(type(as.environment(1)), "env")
})

test_that("understand class", {
  expect_equal(type(factor(c("a", "b", "c"))), "class<factor>")
})

test_that("understand scalar raw", {
  expect_equal(type(as.raw(1)), "raw")
})

}
