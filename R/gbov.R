## Great Book of Values Interface Functions

#' @export
# Load a gbov from its path
# @param dir the directory where values.RDS file is located
# @return a list of values each of which consists of 1) hash 2) type 3) serialized value
load_gbov <- function(path) {
  gbov <- readRDS(path)
  if((class(gbov) != "gbov")[[1L]]) class(gbov) <- c("gbov", class(gbov))
  gbov
}

load_meta <- function(path) {
  meta <- readRDS(path)
}

#' @export
length.gbov <- function(gbov) {
  ## length(unclass(gbov))
  nrow(gbov)
}

# Do we need this?
# Get the hash from the gbov at the given index
get_hash <- function(gbov, id) { 
  gbov[id,1]
}

# Do we need this?
# Get a random hash from the gbov
get_random_hash <- function(gbov) {
  id = sample.int(size(gbov), 1)
  get_hash(gbov, id)
}

#' @export
# Get a random value from the gbov
get_random_value <- function (gbov) {
  id <- sample.int(nrow(gbov), 1)
  unserialize(gbov[id, 3][[1]])
}

get_value_from_hash <- function(gbov, hash) {
  unserialize(filter(gbov, value_hash == hash)$raw_value[[1]])
}


#' Get a subset of gbov containing only values of the specified type
get_value_by_type <- function (gbov, ty) {
  gbov_ty <- filter(gbov, type == ty)
  ger_random_value(gbov_ty)
}

get_value_by_package <- function (gbov, meta, name) {
  meta_p <- filter(meta, package_name == name)
  id <- sample.int(nrow(meta_p), 1)
  hash <- meta_p[id,1]
  get_value_from_hash(gbov, hash)
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

  if(nrow(filter(gbov, value_hash == hash)) == 0) {
    raw <- I(list(serialize(val, connection = NULL)))
    new <- data.frame(value_hash = hash, type = typeof(val), raw_value = raw)
    new_book <- rbind(gbov, new)
    new_book
  } else {
    gbov
  }

  ## val_env <- as.environment(gbov)

  ## if (!exists(hash, envir=val_env)) {
  ##   value_ser <- serialize(val, connection=null, ascii=false)
  ##   value <- list(hash, typeof(val), value_ser)
  ##   assign(hash, value, envir=val_env)
  ## }

  ## new_book <- as.list(val_env)
  ## class(new_book) <- c("gbov", class(new_book))
  ## new_book
}

#' @export
print.gbov <- function(gbov) {
  gbov$raw_value <- lapply(gbov$raw_value, unserialize)
  gbov
  ## values <- list(character(0))
  ## for(i in seq_along(gbov)) {
  ##   value <- unserialize(gbov[[i]][[3L]])
  ##   values[[i]] = value
  ##   print(value)
  ## }
  ## invisible(values)
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

## #' Looks up the value stored in the given gbov by the specified hash
## #' and returns it if it was found, otherwise NULL is returned
## #' 
## look_up <- function(gbov, hash) {
##   for(val in gbov) {
##     if(val[[1]] == hash) {
##       return(unserialize(val[[3]]))
##     }
##   }
##   print("hash not found")
##   invisible(NULL)
## }
