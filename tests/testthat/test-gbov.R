gbov <- signatr::load("resources/values.RDS")

## [1] FALSE  TRUE FALSE FALSE
## [1] "^p"
## [1] TRUE
## NULL
## [1]  TRUE FALSE FALSE FALSE
## [1] FALSE
## [1] "a"
## [1] "^a"
## [1] "aecfg"
## [1] "b"
## [1] TRUE TRUE TRUE TRUE
## [1]  TRUE  TRUE FALSE FALSE
## [1] "[aeiou]"
## [1] "regex"
## [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
## [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [25] FALSE FALSE
## [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
## [20] "t" "u" "v" "w" "x" "y" "z"
## [1] "a$"
## [1] "apple"    "banana"   "pear"     "pinapple"

test_that("test load",{
  expect_equal(class(gbov)[[1L]], "gbov")
})

test_that("test length",{
  expect_equal(length(gbov), 18)
})

test_that("test get_hash",{
  expect_equal(get_hash(gbov, 2), sha1("^p"))
})

test_that("test look_up",{
  expect_equal(look_up(gbov, sha1("^p")), "^p")
  expect_equal(look_up(gbov, gbov[[3L]][[1L]]), TRUE)
  # hash not found case
  expect_equal(look_up(gbov, sha1("hyeyoung")), NULL)
})

test_that("test get_random_hash", {
  random_hash <- get_random_hash(gbov)
  value <- look_up(gbov, random_hash)
  expect_equal(sha1(value), random_hash)
})

test_that("test get_random_value", {
  random_value <- get_random_value(gbov)
  hash <- sha1(random_value)
  expect_equal(look_up(gbov, hash), random_value)
})

test_that("test add_value", {
  newgbov <- add_value(gbov, "hyeyoung")
  expect_equal(length(newgbov), length(gbov) + 1)
  expect_equal(class(newgbov)[[1L]], "gbov")
  expect_equal(unserialize(newgbov[[1L]][[3L]]), "hyeyoung")
})
