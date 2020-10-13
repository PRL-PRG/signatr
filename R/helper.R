## Function Resolution Helper Functions

# TODO: Implement
# Get a function based on the package name and function name
# @param package_name is the name of the package to look in
# @param function_name is the name of the function
# @return the specified function or NULL
get_function <- function(package_name = NULL, function_name) {
  tryCatch(
    {
      if (is.null(package_name)) {
        f = get(function_name)
      } else {
        f = get(function_name, envir=getNamespace(package_name))
      }
      return(f)
    }, error = function(err) {
      return(NULL)
    }
  )
}

## Type Identification Helper Functions

# TODO: Implement
# Get type of the input value # Based on TastR/TypeR?
# @param value is a R value that signatr_typeof can make sense of
# @return the type of the value
signatr_typeof <- function(value) {
  return(typeof(value))
}


## Run Helper Functions

# TODO: Implement
# Run a function until a specified amount of time elapsed, or until killed
# @param f is the function to be run
# @param timeout is the amount of time to run the function
run_until_timeout_or_death <- function(timeout, f) {
  end = as.integer(Sys.time()) + timeout
  while (end > as.integer(Sys.time())) {
    f()
  }
}


## Working with functions.csv

# Location of the function information
FUNCTIONS_CSV_PATH = "/var/lib/R/project-signatr/run/package-metadata/functions.csv"

# Load the function information into memory as a dataframe
# The information in the data are as follows
#   package -- package the function belongs in
#   function -- the name of the function
#   exported -- [TODO Find out what it is; it's just a normal function?]
#   is_s3_dispatch -- is a s3 dispatch
#   is_s3_method -- is a s3 method
#   params -- ";" seperate formals of the function
#   nparams -- number of formal a function takes -- added by this function
# @param path is the path to the function data
# @return a data.frame of function data
load_function_data <- function(path) {
  df = read.csv(FUNCTIONS_CSV_PATH)
  df$nparams <- apply(df["params"], 1, function(x) {
    length(strsplit(x, ";")[[1]])
  })
  
  return(df)
}
