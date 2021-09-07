library(record)
open_db("db/test_db", create=FALSE)
db_vals <- view_db()

test_that("sample a random value from database", {
  val <- sample_val_from_db("test_db")
  while(typeof(val) == "raw") {
    val <- sample_val_from_db("test_db")
  }

  expect_true(all(val %in% db_vals))
})

test_that("sample a double from database", {
  val <- sample_val_from_db("test_db", type = "double")
  while(typeof(val) == "raw") {
    val <- sample_val_from_db("test_db")
  }

  expect_equal(typeof(val), "double")
})

close_db()
