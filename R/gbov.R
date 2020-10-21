## Great Book of Values Interface Functions

#' @export
# Load a gbov from its path
# @param dir the directory where values.RDS file is located
# @return a list of values each of which consists of 1) hash 2) type 3) serialized value
load <- function(path) {
  gbov <- readRDS(path)
  if((class(gbov) != "gbov")[[1L]]) class(gbov) <- c("gbov", class(gbov))
  gbov
}

#' @export
length.gbov <- function(gbov) {
  length(unclass(gbov))
}

# Get the hash from the gbov at the given index
# @param gbov 
# @param index 
# @return the R value from given gbov at the given index
get_hash <- function(gbov, index) {
  gbov[[index]][[1L]]
}

# Get a random hash from the gbov
# @param gbov 
# @return a random hash from the given gbov
get_random_hash <- function(gbov) {
  l = length(gbov)
  index = sample.int(l, 1)
  gbov[[index]][[1L]]
}

#' @export
# Get a random value from the gbov
# @param gbov
# @return a random value from the given gbov
get_random_value <- function (gbov) {
  random <- sample.int(length(gbov), 1)
  unserialize(gbov[[random]][[3L]])
}

#' @export
get_type <- function (value) {
  typeof(value)
}

# Add a new value to the Great Book of Values
# @param gbov
# @param val 
# @return the gbov with the added val
#' @export
add_value <- function(gbov, val) {
  if(is.environment(val)){
    hash <- digest::sha1(as.list(val))
  } else {
    hash <- sha1(val)
  }

  val_env <- as.environment(gbov)

  if (!exists(hash, envir=val_env)) {
    value_ser <- serialize(val, connection=NULL, ascii=FALSE)
    value <- list(hash, typeof(val), value_ser)
    assign(hash, value, envir=val_env)
  }

  new_book <- as.list(val_env)
  class(new_book) <- c("gbov", class(new_book))
  new_book
}

#' @export
print.gbov <- function(gbov) {
  values <- list(character(0))
  for(i in seq_along(gbov)) {
    value <- unserialize(gbov[[i]][[3L]])
    values[[i]] = value
    print(value)
  }
  invisible(values)
}

#' By default takes a gbov object and saves it in the given path with
#' the specified name and returns the path to the saved file
#' if new is FALSE, it overwrites gbov.RDS in the given directory(path)
#' #' @export
save.gbov <- function(gbov, path = ".", name = "new-gbov", new = TRUE) {
  if (!dir.exists(path)) dir.create(path, recursive=TRUE)

  if(!new) {
    name = "gbov"
    saveRDS(gbov, file = paste0(path, "/", name, ".RDS"))
  }
  where <- paste0(path, "/", name, ".RDS")
  saveRDS(gbov, file = where)
  where
}

#' Looks up the value stored in the given gbov by the specified hash
#' and returns it if it was found, otherwise NULL is returned
#' #' @export
look_up <- function(gbov, hash) {
  for(val in gbov) {
    if(val[[1]] == hash) {
      return(unserialize(val[[3]]))
    }
  }
  print("hash not found")
  invisible(NULL)
}
