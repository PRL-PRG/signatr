## Great Book of Values Interface Functions

# Load a gbov from its path 
#' param: path path to the value database e.g. [PATH-TO]/merged-gbov.RDS
#' return: an environment of values identified by unique index
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
  length(as.list(gbov))
}

# Get a random value from the gbov
#' @export
get_random_value <- function (gbov) {
  random_index <- sample.int(length(gbov), 1)
  get(toString(random_index), envir = gbov)
}

#' param: type is a string representing a type e.g. "list", "double", etc
#' Get a random value of specified type
#' @export
get_random_value_by_type <- function (gbov, meta, type) {
  match_df <- meta[meta$type == type,]
  random_index <- sample.int(nrow(match_df), 1)
  gbov_index <- match_df[random_index, "index"]
  get(toString(gbov_index), gbov)
}


#' param: package_name is a string representing package_name e.g. "stringr", "dplyr", etc"
#' Get a random value from specified package
#' @export
get_random_value_by_package <- function (gbov, meta, package_name, not_by = FALSE) {
  if (not_by) {
    match_df <- meta[meta$package_name != package_name,]
  } else {
    match_df <- meta[meta$package_name == package_name,]
  }

  if(nrow(match_df) == 0) {
    print(paste0("no value ", if(not_by) "not from " else "from ", package_name))
  } else {
    random_index <- sample.int(nrow(match_df), 1)
    gbov_index <- match_df[random_index, "index"]
    get(toString(gbov_index), gbov)
  }
}

# Add a new value to gbov (in-place modification) and metadatabase (return new)
# @param gbov
# @param val 
# @return a new metadatabase dataframe
#' @export
add_value <- function(gbov, meta, val) {
  if(is.environment(val)){
    hash <- digest::sha1(as.list(val))
  } else {
    hash <- sha1(val)
  }

  gbov_index <- length(gbov) + 1

  if(nrow(meta[meta$value_hash == hash,]) == 0) {
    new_val <- data.frame(source_hash = NA, count = 1, index = gbov_index, type = typeof(val), package_name = NA, fun_name = NA, pos = NA)
    new_meta <- rbind(meta, new_val)
    assign(toString(gbov_index), val, envir = gbov)
    new_meta
  } else {
    meta
  }
}

#' @export
print <- function (gbov, ...) {
  UseMethod("print", gbov)
}

#' @export
print.gbov <- function(gbov) {
 as.list(gbov)
}

# Prints the first 10 values of gbov
#' @export
less <- function (gbov, ...) {
  UseMethod("less", gbov)
}                                        #

#' @export
less.gbov <- function(gbov) {
  as.list(gbov)[1:10]
}


#' Saves gbov at specified directory by specified name
#' if new = FALSE, current gbov.RDS is overwritten
#' @export
save <- function (gbov, ...) {
  UseMethod("save", gbov)
}                                        #

#' @export
save.gbov <- function(gbov, meta, dir = ".", gbov_name, meta_name) {
  if (!dir.exists(dir))
    dir.create(dir, recursive=TRUE)

  saveRDS(gbov, file = paste0(dir, "/", gbov_name, ".RDS"))
  saveRDS(meta, file = paste0(dir, "/", meta_name, ".RDS"))
}

#' @export
unique.gbov <- function(gbov) {
  lgbov <- as.list(gbov)
  vgbov <- lapply(lgbov, function(x) x[[2]])
  unique(vgbov)
}

#' @export
values_only <- function(gbov) {
  vgbov <- lapply(as.list(gbov), function(x) x[[2]])
}
