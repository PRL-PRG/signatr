#' @export
pretty_print <- function(x, ...) {
  UseMethod("pretty_print")
}

#' @importFrom dplyr count mutate
#' @export
cs_count <- function(df, ...) {
  n_rows = nrow(df)
  dplyr::count(df, ..., sort = TRUE) %>%
    dplyr::mutate(p = n / n_rows * 100, cp = cumsum(p))
}

## Formatting

#' Formats given value.
#'
#' The `x` value is formatted according to its class with an additional
#' `prefix` and `suffix` prepended and appended respectively.
#'
#' @param x the value to be formatted
#' @param prefix the string to be prepended
#' @param suffix the string to be appended
#' @param ... parameters to be passed to the actual formatter. The number
#'     formatter accepts `floor` and `ceiling` to indicate that given value
#'     should be rounded down or up to a full integer before formatting or
#'     `digits` to indicate the number of digits the number should be rounded
#'     to.
#'
#' @export
fmt <- function(x, prefix="", suffix="", ...) {
  if (is.null(x)) {
    NULL
  } else {
    v <- as.character(.fmt(x, ...))
    stringr::str_c(prefix, v, suffix)
  }
}

.fmt <- function(x, ...) {
  UseMethod(".fmt", x)
}

.fmt.default <- function(x) {
  x
}

.fmt.integer <- function(x) {
  format(x, big.mark=",", trim=TRUE)
}

# TODO: make this parameter a global option so it can be set globally
# TODO: test c(0.0123, 1.123, -0.0123, -1.123)
.fmt.double <- function(x, digits=1, floor=FALSE, ceiling=FALSE) {
  if (floor) x <- floor(x)
  if (ceiling) x <- ceiling(x)
  
  x <- sapply(x, function(y) {
    y <- abs(y)
    
    if (is.na(y)) y
    else if (y < 1) signif(y, digits)
    else if (y > 1) round(y, digits)
    else y
  })

  prettyNum(x, big.mark=",", scientific=F)
}

.fmt.num_with_suffix <- function(x, floor=FALSE, ceiling=FALSE, digits=1) {
  suffix <- attr(x, "suffix")
  fmt(as.double(x), suffix=suffix, floor=floor, ceiling=ceiling, digits=digits)
}

#' Converts given value to a percent representation.
#'
#' It multiples the value by 100 and adds the `%` suffix attribute.
#'
#' @param x the value to convert
#'
#' @export
percent <- function(x) {
  x <- x*100
  class(x) <- "num_with_suffix"
  attr(x, "suffix") <- "%"
  x
}

#' @export
ratio <- function(x, y) {
  percent(to_ratio_num(x)/to_ratio_num(y))
}

#' @export
invratio <- function(x, y) {
  percent(1-to_ratio_num(x)/to_ratio_num(y))
}

to_ratio_num <- function(x) {
  UseMethod("to_ratio_num")
}

to_ratio_num.numeric <- identity

to_ratio_num.data.frame <- function(x) nrow(x)

