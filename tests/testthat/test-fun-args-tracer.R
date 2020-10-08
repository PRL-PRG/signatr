test_that("test value tracing", {
  r <- trace_fun_args("stringr", {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  })
  browser()
  save_fun_args_data(r$data, ".")
})

## signatr::trace(package="", path="str_detect.R", {
##  <orginal code from example>
## })
#
