test_that("infer type of str_detect", {
  r <- infer_signature(package = "stringr",
                  f = "str_detect",
                  num_params = 3,
                  db = "db",
                  num_runs = 100)

  print(r)
})
