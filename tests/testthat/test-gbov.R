stringr <- "../test/test_gbov_list/stringr"
dup_db <- signatr::load_gbov(paste0(stringr, "/str_dup.Rd.R/values.RDS"))
stringr_gbov <- signatr::load_gbov(paste0(stringr, "/merged-gbov.RDS"))
gbov <- load_gbov("../test/test_gbov_list/2packages/merged-gbov.RDS")

findit <- function(db, val) {
  if(class(db)[[2]] == "list") {
    sum(unlist(lapply(db, function(x) identical(x[[3]], val))))
  } else {
    sum(unlist(lapply(db, function(x) identical(x, val))))
  }
}

#' @examples
#' fruit <- c("apple", "pear", "banana")
#' str_dup(fruit, 2)
#' str_dup(fruit, 1:3)
#' str_c("ba", str_dup("na", 0:5))

test_that("test load_gbov",{
  expect_equal(class(stringr_gbov)[[1L]], "gbov")
  expect_equal(class(dup_db)[[1L]], "gbov")
})

test_that("test extracting values",{
  fruit <- c("apple", "pear", "banana")
  result <- str_dup(fruit, 2)
  expect_equal(findit(dup_db,fruit),1)
  expect_equal(findit(dup_db, result), 1)
  expect_equal(findit(dup_db, 2), 1)
  expect_equal(findit(dup_db, 1:3), 1)
  expect_equal(findit(dup_db, 0:5), 1)
  expect_equal(findit(dup_db, "na"), 1)
  ## expect_equal(findit(dup_db, "ba"), 1)
})

test_that("test length",{
  expect_true(length(dup_db) > 7)
  expect_true(length(stringr_gbov) > length(dup_db))
})

test_that("test get_random_value", {
  random_value <- get_random_value(dup_db)
  expect_equal(findit(dup_db, random_value[[3]]), 1)
})

test_that("test get_random_value_by_type", {
  stringr_meta <- load_meta(paste0(stringr, "/merged-meta.RDS"))
  random_value1 <- get_random_value_by_type(stringr_gbov, stringr_meta, type = "character")
  expect_equal(typeof(random_value1), "character")
  expect_equal(findit(stringr_gbov, random_value1), 1)

  random_value2 <- get_random_value_by_type(stringr_gbov, stringr_meta, "double")
  expect_equal(typeof(random_value2), "double")
  expect_equal(findit(stringr_gbov, random_value2), 1)
})

test_that("test get_random_value_by_package", {
  meta <- load_meta("../test/test_gbov_list/2packages/merged-meta.RDS")
  reshape2_gbov <- load_gbov("../test/test_gbov_list/reshape2/merged-gbov.RDS")

  random_value1 <- get_random_value_by_package(gbov, meta, "stringr")
  expect_equal(findit(stringr_gbov, random_value1), 1)

  random_value2 <- get_random_value_by_package(gbov, meta, "reshape2")
  expect_equal(findit(reshape2_gbov, random_value2), 1)

  random_value3 <- get_random_value_by_package(gbov, meta, "reshape2", not_by = TRUE)
  expect_equal(findit(stringr_gbov, random_value3), 1)
  expect_equal(findit(reshape2_gbov, random_value3), 0)
})

test_that("test exclude", {
  some_env <- emptyenv()
  some_list <- list(1, some_env)
  some_rec_list <- list(1, list(1, some_env))
  some_pairlist <- pairlist(1, some_env)
  some_rec_pairlist <- pairlist(1, pairlist(some_env))

  expect_true(exclude(some_env, typeof(some_env)))
  expect_equal(exclude(some_list, typeof(some_list)), 1)
  expect_equal(exclude(some_rec_list, typeof(some_rec_list)), 1)
  expect_equal(exclude(some_pairlist, typeof(some_pairlist)), 1)
  expect_equal(exclude(some_pairlist, typeof(some_rec_pairlist)), 1)
})

test_that("test uniqueness", {
  expect_equal(length(gbov),length(unique(gbov)))
})
