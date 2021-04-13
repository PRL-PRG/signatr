test_that("trace_fun_args str_detect", {
  r <- trace_fun_args("stringr", {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  })
  ## save_fun_args_data(r$data, ".")
  expect_equal(length(r), 7)
})

## test_that("trace str_trim", {
##   r <- signatr::trace(package="stringr", path=('./str_trim.Rd.R'), code = {
##     library(stringr)


## ### Name: str_trim
## ### Title: Trim whitespace from a string
## ### Aliases: str_trim str_squish

## ### ** Examples

##     str_trim("  String with trailing and leading white space\t")
##     str_trim("\n\nString with trailing and leading white space\n\n")

##     str_squish("  String with trailing,  middle, and leading white space\t")
##     str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
##   })
##   expect_equal(length(r), 20)
## })

