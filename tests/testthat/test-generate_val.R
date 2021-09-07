test_that("generate a logical val", {
  log <- generate_val("logical")
  expect_equal(typeof(log), "logical")
})


test_that("generate a double val", {
  db <- generate_val("double")
  expect_equal(typeof(db), "double")
})


test_that("generate a complex val", {
  comp <- generate_val("complex")
  expect_equal(typeof(comp), "complex")
})


test_that("generate a_list of 5 things", {
  len5_list <- generate_val("list", n = 5)

  expect_equal(typeof(len5_list), "list")
  expect_equal(length(len5_list), 5)
})


test_that("generate a length 0 list", {
  len0_list <- generate_val("list", n = 0)

  expect_equal(typeof(len0_list), "list")
  expect_equal(length(len0_list), 0)
})


test_that("generate a vector of length 3 of any type", {
  len3_vec <- generate_val("dim1", n = 3)

  expect_equal(length(len3_vec), 3)
})


test_that("generate an empty vector of any type", {
  len0_vec <- generate_val("dim1", n = 0)

  expect_equal(length(len0_vec), 0)
})


test_that("generate a 2 by 3 matrix", {
  mat2_by_3 <- generate_val("dim2", n = 2, col = 3)

  expect_equal(nrow(mat2_by_3), 2)
  expect_equal(ncol(mat2_by_3), 3)
})

test_that("generate a double matrix with 5 rows and 7 columns", {
  db_mat <- generate_val("dim2", "double", n = 5, col = 7)

  expect_equal(nrow(db_mat), 5)
  expect_equal(typeof(db_mat), "double")
  expect_equal(class(db_mat)[[1]], "matrix")
})

test_that("generate a integer matrix with 0 rows and 3 columns", {
  int_mat_0_rows <- generate_val("dim2", "integer", n = 0, col = 3)

  expect_equal(nrow(int_mat_0_rows), 0)
  expect_equal(typeof(int_mat_0_rows), "integer")
  expect_equal(class(int_mat_0_rows)[[1]], "matrix")
})

test_that("generate a value from db", {
  record::open_db("db/test_db")
  db_vals <- view_db()

  db_val <- generate_val(db=TRUE, db_path="db/test_db")
  while(typeof(db_val) == "raw") {
    db_val <- generate_val(db=TRUE, db_path="db/test_db")
  }

  expect_true(all(db_val %in% db_vals))
  record::close_db()
})
