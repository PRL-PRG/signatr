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
  if(sum(class(gbov) == "list")) {
    class(gbov) <- "list"
    length(gbov)
  } else {
    length(as.list(gbov))
  }
}

# Get a random value from the gbov
#' @export
get_random_value <- function (gbov) {
  random_index <- sample.int(length(gbov), 1)
  as.list(gbov)[[random_index]]
  ## get(toString(random_index), envir = gbov)
}

#' param: type is a string representing a type e.g. "list", "double", etc
#' Get a random value of specified type
#' @export
get_random_value_by_type <- function (gbov, meta, type) {
  exclude_list <- list("closure", "language", "environment")
  if(type %in% exclude_list) {
    stop("excluded type")
  }
  match_df <- meta[meta$type == type,]
  random_index <- sample.int(nrow(match_df), 1)
  hash <- match_df[random_index,]$value_hash
  get(hash, gbov)

  ## gbov_index <- match_df[random_index, "index"]
  ## get(toString(gbov_index), gbov)
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
    hash <- match_df[random_index,]$value_hash
    get(hash, gbov)
    ## gbov_index <- match_df[random_index, "index"]
    ## get(toString(gbov_index), gbov)
  }
}

# Adds a new value to gbov and update meta accordingly. The new gbov and meta are saved in a RDS file. TODO: Saving each time a value is added is too costly.
## add_value <- function(gbov, meta, val) {
##   type <- typeof(val)

##   if(exclude(val, type)) {
##     return()
##   }

##   hash <- sha1(deparse(val))
##   if (length(gbov) == 0) {
##     assign(hash, val, envir=gbov)
##   } else {
##     duplicate <- which(unlist(lapply(as.list(gbov), function(x) identical(x, val))))

##     if(length(duplicate) == 0) {
##       assign(hash, val, envir=gbov)
##       new_meta <- data.frame(value_hash = hash, source_hash = NA,  count = 1, index = 0, type = type, package_name = NA, fun_name = NA, pos = NA)
##       meta <- rbind(meta, new_meta)
##     } else {
##       duplicate_hash <- attr(duplicate, "names")
##       meta[meta$value_hash == hash,]$count <- meta[meta$value_hash == hash,]$count + 1
##     }
##   }
##   save(gbov, meta)
## }

#' @export
print.gbov <- function(gbov) {
 as.list(gbov)
}

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
save.gbov <- function(gbov, meta, dir = ".") {
  if (!dir.exists(dir))
    dir.create(dir, recursive=TRUE)

  saveRDS(gbov, file = paste0(dir, "/", "new_gbov", ".RDS"))
  saveRDS(meta, file = paste0(dir, "/", "new_meta", ".RDS"))
}

#' @export
unique.gbov <- function(gbov) {
  ugbov <- unique(as.list(gbov))
  class(ugbov) <- c("gbov", class(ugbov))
}

#' @export
exclude <- function(val, ty) {
  exclude_list <- list("closure", "language", "environment")

  if (ty %in%  list("list", "expression", "pairlist")) {
    sum(lapply(unlist(val), typeof) %in% exclude_list)
  } else {
    ty %in% exclude_list
  }
}
