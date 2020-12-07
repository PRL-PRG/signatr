## Great Book of Values Interface Functions

# Load a gbov from its path 
#' param: path path to the value database e.g. [PATH-TO]/merged-gbov.RDS
#' return: data.frame(value_hash, type, raw_value)
#' @export
load_gbov <- function(path) {
  gbov <- readRDS(path)
  if((class(gbov) != "gbov")[[1L]]) class(gbov) <- c("gbov", class(gbov))
  gbov
}

# Load gbov meta data from its path
#' param: path path to the metadata file e.g. [PATH-TO]/merged-meta.RDS
#' return: data.frame(value_hash, source_hash, count, package_name, fun_name, pos)
#' @export
load_meta <- function(path) {
  meta <- readRDS(path)
  meta
}

#' @export
length.gbov <- function(gbov) {
  ## length(unclass(gbov))
  nrow(as.data.frame(gbov))
}

# Get hash at given index
get_hash_by_index <- function(gbov, index) {
  as.data.frame(gbov)[index,1]
}

# Get random hash
get_random_hash <- function(gbov) {
  random_index = sample.int(length(gbov), 1)
  get_hash(gbov, random_index)
}

# Get a random value from the gbov
#' @export
get_random_value <- function (gbov) {
  random_index <- sample.int(length(gbov), 1)
  if(class(gbov)[[1]] == "gbov") {
    gbov <- as.data.frame(gbov)
  }
  unserialize(gbov[random_index, 3][[1]])
}

# TODO: Assumed that a hash from metadata will be found in gbov,
# but this turned out to be not true; Need to figure out when this happens
get_value_by_hash <- function(gbov, hash) {
  gbov_df <- as.data.frame(gbov)
  match <- gbov_df[gbov_df$value_hash == hash,]
  if(nrow(match) == 0) {
    print(paste0("no value found by hash: ", hash))
  } else {
    unserialize(match$raw_value[[1]])
  }
}


#' param: type A string representing type e.g. "list", "double", etc
#' Get a subset of gbov containing only values of the specified type
#' @export
get_random_value_by_type <- function (gbov, type) {
  gbov_df <- as.data.frame(gbov)
  match_df <- gbov_df[gbov_df$type == type,]

  get_random_value(match_df)
}


#' param: package_name A string representing package_name e.g. "stringr", "dplyr", etc"
#' Get a subset of gbov containing only values from the specified package
#' @export
get_random_value_by_package <- function (gbov, meta, package_name) {
  match_df <- meta[meta$package_name == package_name,]
  if(nrow(match_df) == 0) {
    print(paste0("no value from ", package_name))
  } else {
    random_index <- sample.int(nrow(match_df), 1)
    hash <- match_df[random_index, ]$value_hash

    get_value_by_hash(gbov, hash)
  }
}


#' param: package_name A string representing package_name e.g. "stringr", "dplyr", etc"
#' Get a subset of gbov containing only values of the specified package
#' @export
get_random_value_not_from <- function (gbov, meta, package_name) {
  match_df <- meta[meta$package_name != package_name,]
  if(nrow(match_df) == 0) {
    print(paste0("no value not from ", package_name))
  } else {
    random_index <- sample.int(nrow(match_df), 1)
    hash <- match_df[random_index, ]$value_hash

    get_value_by_hash(gbov, hash)
  }
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

  gbov_df <- as.data.frame(gbov)

  if(nrow(gbov_df[gbov_df$value_hash == hash,]) == 0) {
    raw <- I(list(serialize(val, connection = NULL)))
    new_value <- data.frame(value_hash = hash, type = typeof(val), raw_value = raw)
    new_book <- rbind(gbov_df, new_value) # new value at the bottom of gbov
    class(new_book) <- c("gbov", class(new_book))
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
print <- function (gbov, ...) {
  UseMethod("print", gbov)
}

#' @export
print.gbov <- function(gbov) {
  gbov_df <- as.data.frame(gbov)
  gbov_df
  ## values <- list(character(0))
  ## for(i in seq_along(gbov)) {
  ##   value <- unserialize(gbov[[i]][[3L]])
  ##   values[[i]] = value
  ##   print(value)
  ## }
  ## invisible(values)
}

# Prints the first 10 rows of the given gbov
#' @export
less <- function (gbov, ...) {
  UseMethod("less", gbov)
}                                        #

#' @export
less.gbov <- function(gbov) {
  gbov_df <- as.data.frame(gbov)
  print(gbov_df[1:10,])
}


#' Saves gbov at specified directory by specified name
#' if new = FALSE, current gbov.RDS is overwritten
#' @export
save <- function (gbov, ...) {
  UseMethod("save", gbov)
}                                        #

#' @export
save.gbov <- function(gbov, dir = ".", name = "new-gbov", new = TRUE) {
  if (!dir.exists(path)) dir.create(path, recursive=TRUE)

  if(!new) {
    name = "gbov"
    saveRDS(gbov, file = paste0(path, "/", name, ".RDS"))
  }
  where <- paste0(path, "/", name, ".RDS")
  saveRDS(gbov, file = where)
  where
}
