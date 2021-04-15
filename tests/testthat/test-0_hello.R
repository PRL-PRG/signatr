test_that("test hello",{
	expect_equal(hello("c"), "hello c")
})

test_that("hello there", {
  r <- hello("there")
  expect_equal(r, "hello there")
})
