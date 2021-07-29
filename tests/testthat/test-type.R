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

test_that("understand scalar logical", {
  expect_equal(type(as.logical(1)), "lgl")
})

test_that("understand scalar integer", {
  expect_equal(type(as.integer(1)), "int")
})

test_that("understand scalar double", {
  expect_equal(type(1), "dbl")
})

test_that("understand scalar complex", {
  expect_equal(type(as.complex(1)), "clx")
})

test_that("understand scalar character", {
  expect_equal(type(as.character(1)), "chr")
})

test_that("understand raw vector", {
  expect_equal(type(as.raw(c(1, 2))), "raw[2]")
})

test_that("understand logical vector", {
  expect_equal(type(as.logical(c(1, 2, 3))), "lgl[3]")
})

test_that("understand integer vector", {
  expect_equal(type(as.integer(c(1, 2, 3, 4))), "int[4]")
})

test_that("understand double vector", {
  expect_equal(type(c(1, 2, 3, 4, 5)), "dbl[5]")
})

test_that("understand complex vector", {
  expect_equal(type(as.complex(c(1, 2, 3, 4, 5, 6))), "clx[6]")
})

test_that("understand character vector", {
  expect_equal(type(as.character(c(1, 2, 3, 4, 5, 6, 7))), "chr[7]")
})

test_that("understand list", {
  expect_equal(type(as.list(c(1, 2, 3, 4, 5, 6, 7))), "list<any>")
})

}
