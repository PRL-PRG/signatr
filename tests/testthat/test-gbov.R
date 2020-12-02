reshape2_gbov <- signatr::load_gbov("tests/testthat/resources/reshape2/cast.Rd.R/values.RDS")
stringr_gbov <- signatr::load_gbov("tests/testthat/resources/stringr/str_match.Rd.R/values.RDS")

test_that("test load_gbov",{
  expect_equal(class(reshape2_gbov)[[1L]], "gbov")
})

test_that("test length",{
  expect_equal(length(reshape2_gbov), 185)
  expect_equal(length(stringr_gbov), 17)
})

test_that("test get_hash", {
  hash <- stringr_gbov$value_hash[[1]]
  expect_equal(get_hash(stringr_gbov, 1), hash)
})

test_that("test get_random_hash", {
  hash <- get_random_hash(stringr_gbov)
  gbov_df <- as.data.frame(stringr_gbov)
  index <- gbov_df[stringr_gbov$value_hash == hash,]
  expect_equal(get_value_by_hash(stringr_gbov, hash), 
})

test_that("test get_random_hash", {
  random_hash <- get_random_hash(gbov)
  value <- look_up(gbov, random_hash)
  expect_equal(sha1(value), random_hash)
})


test_that("test add_value", {
  newgbov <- add_value(gbov, "hyeyoung")
  expect_equal(length(newgbov), length(gbov) + 1)
  expect_equal(class(newgbov)[[1L]], "gbov")
  expect_equal(unserialize(newgbov[[1L]][[3L]]), "hyeyoung")
})
