test_that("test value tracing", {
  r <- trace_fun_args("stringr", {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  })
  browser()
  str(r)
  #saveRDS(source_df)
  #saveRDS(as.list(values), "")
  #saveRDS(values_sources_df, "")
})

# signatr::trace(package="", path="str_detect.R", {
#  <orginal code from example>
#})
#
