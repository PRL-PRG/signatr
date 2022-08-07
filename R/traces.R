#' @importFrom dplyr mutate n anti_join filter
#' @importFrom tibble as_tibble %>% tibble
#' @importFrom stringr str_replace
#' @importFrom qs qread
#' @export
traces_load <- function(path) {
  traces <- qread(path) %>%
    dplyr::mutate(
      id = 1:n(),
      rdb_path = file.path(dirname(path), rdb_path),
      # TODO: this should be only enabled for base package!
      fun_name = stringr::str_replace(fun_name, "wrap_", ""),
      fun_name = stringr::str_replace(fun_name, "div", "/"),
      fun_name = stringr::str_replace(fun_name, "basewrap", "base")
    ) %>%
    tibble::as_tibble()
  
  traces_success <- dplyr::filter(traces, status == 0)
  paths <- unique(traces_success$rdb_path)
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths)) {
    for (p in missing_paths) {
      warning("Missing path: ", p)
    }
    
    traces <- dplyr::anti_join(traces, tibble::tibble(rdb_path = missing_paths), by = "rdb_path")
  }
  
  invisible(traces)
}

#' @importFrom dplyr group_by n summarize
#' @export
traces_stats <- function(traces) {
  stats <- dplyr::group_by(traces, fun_name) %>%
    dplyr::summarize(
      num_traces = dplyr::n(),
      success = sum(status == 0),
      ratio_success = success / dplyr::n(),
      error = sum(status != 0), 
      ratio_error = error / dplyr::n()
    )
  
  class(stats) <- c("traces_stats", class(stats))
  stats
}

#' @importFrom knitr kable
#' @importFrom dplyr rename
#' @export
pretty_print.traces_stats <- function(x, ...) {
  x %>%
    dplyr::rename(
      `Function` = fun_name,
      `Traces` = num_traces,
      `Successfull traces` = success,
      `Success` = ratio_success,
      `Error traces` = error,
      `Error` = ratio_error
    ) %>%
    knitr::kable()
}

#' @importFrom stringr str_replace
#' @export
summary.traces_stats <- function(stats, ...) {
  pkg_names = stringr::str_replace(stats$fun_name, "(.*)::.*", "\\1")
  
  s <- list(
    num_traces = sum(stats$num_traces),
    num_success_traces = sum(stats$success),
    ratio_success_traces = sum(stats$success) / sum(stats$num_traces),
    num_pkgs = unique(pkg_names) %>% length,
    num_funs = unique(stats$fun_name) %>% length
  )
  
  class(s) <- "traces_stats_summary"
  s
}

#' @importFrom cli cli_ul cli_li cli_end
#' @export
print.traces_stats_summary <- function(x, ...) {
  cli::cli_ul()
  cli::cli_li("Number of traces: {fmt(x$num_traces)}")
  cli::cli_li("Number of successfull traces: {fmt(x$num_success_traces)} ({fmt(percent(x$ratio_success_traces))})")
  cli::cli_li("Number of packages: {fmt(x$num_pkgs)}")
  cli::cli_li("Number of functions: {fmt(x$num_funs)}")
  cli::cli_end()
}