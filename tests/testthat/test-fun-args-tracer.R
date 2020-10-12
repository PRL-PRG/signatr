test_that("test value tracing", {
  r <- trace_fun_args("stringr", {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  })
  save_fun_args_data(r$data, "save_here")
})

## test_that("test value tracing", {
##   r <- trace("stringr",
##              path=file.path(Sys.getenv('RUNR_CWD'), basename('./examples/str_trim.Rd.R')),
##              code = {
##     stringr::str_detect("AB", "B")
##     stringr::str_detect("AB", "A")
##   })
##   save_fun_args_data(r$data, "save_here")
## })

## signatr::trace(package="", path="str_detect.R", {
##  <orginal code from example>
## })

test_that("test tracing1", {
  r <- trace("stringr",
             path = "/home/hyeyoungshin/tmp",
             #path=file.path(Sys.getenv('RUNR_CWD'), basename('./examples/str_trim.Rd.R')),
             code = {
    library(stringr)


    ## Name: str_glue
    ## Title: Format and interpolate a string with glue
    ## Aliases: str_glue str_glue_data

    ## ** Examples

    name <- "Fred"
    age <- 50
    anniversary <- as.Date("1991-10-12")
    str_glue(
      "My name is {name}, ",
      "my age next year is {age + 1}, ",
      "and my anniversary is {format(anniversary, '%A, %B %d, %Y')}."
    )

  # single braces can be inserted by doubling them
    str_glue("My name is {name}, not {{name}}.")

  # You can also used named arguments
    str_glue(
      "My name is {name}, ",
      "and my age next year is {age + 1}.",
      name = "Joe",
      age = 40
    )
  # str_glue_data()` is useful in data pipelines
    mtcars %>% str_glue_data("{rownames(.)} has {hp} hp")
  })
})

## test_that("test tracing", {
##   r <- trace(package = "stringr",
##              path=file.path(Sys.getenv('RUNR_CWD'), basename('./examples/str_trim.Rd.R')),
##              code = {
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

##   browser()
##   str(r)
##   save_fun_args_data(r$data, "save_here")
## })





## signatr::trace(code = {
##   library(stringr)
  
  
## ### Name: str_trim
## ### Title: Trim whitespace from a string
## ### Aliases: str_trim str_squish
  
## ### ** Examples
  
##   str_trim("  String with trailing and leading white space\t")
##   str_trim("\n\nString with trailing and leading white space\n\n")
  
##   str_squish("  String with trailing,  middle, and leading white space\t")
##   str_squish("\n\nString with excess,  trailing and leading white   space\n\n")
  
  
  
## },
## path=file.path(Sys.getenv('RUNR_CWD'), basename('./examples/str_trim.Rd.R')),
## package = "stringr"
## )
