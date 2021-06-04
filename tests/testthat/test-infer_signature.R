test_that("infer type of str_detect", {
  r <- infer_signature(package = "stringr",
                  fun = "str_detect",
                  params = 3,
                  db = "db",
                  runs = 100)

  print(r)
  ## expect_equal(length(r), 0)
})
