test_that("test value tracing", {
  r <- trace_fun_args("stringr", {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  }) # FALSE, "AB", TRUE, "regex", "A", NULL, "B" 

  record::open_db("../db/stringr", create = FALSE)

  expect_equal(record::have_seen("AB"), TRUE)
  expect_equal(record::have_seen(FALSE), TRUE)
  expect_equal(record::have_seen(0), FALSE)

  record::close_db()
})

test_that("trace test", {
  r <- signatr::trace(package="stringr", path=('./str_trim.Rd.Rs'), code = {
    library(stringr)


### Name: str_trim
### Title: Trim whitespace from a string
### Aliases: str_trim str_squish

### ** Examples

    str_trim("  String with trailing and leading white space\t")
    str_trim("\n\nString with trailing and leading white space\n\n")

    str_squish("  String with trailing,  middle, and leading white space\t")
    str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
  }) # 20 vals:
  # FALSE, "empty", "regex", "  String with trailing,  middle, and leading white space\t", [1] "both"  "left"  "right",
  # [[1]] [1] " ",  "\\s+", "String with excess, trailing and leading white space", " ",
  # "\n\nString with excess,  trailing and leading white   space\n\n",
  # " String with excess, trailing and leading white space ", ""
  # "String with trailing and leading white space"
  # $type "character", " String with trailing, middle, and leading white space ", Inf
  # "String with trailing, middle, and leading white space", NULL
  # "\n\nString with trailing and leading white space\n\n"
                                        # "  String with trailing and leading white space\t"
  record::open_db("../db/stringr", create = FALSE)

  expect_equal(record::have_seen("empty"), TRUE)
  expect_equal(record::have_seen("String with trailing and leading white space"), TRUE)
  expect_equal(record::have_seen("AB"), TRUE)

  record::close_db()
})
