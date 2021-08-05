#' score: run_result x type_result -> double
#' Calculate the score for a method for type inference based on how many times
#' the function was run and how many different intermediate typing we found.
#'
#' @param run_result is a list of run states and results
#' @param type_result is a list of different typings
#' @return a double that represent the how well we did
#'
assess_score <- function(run_result, type_result) {
	print(paste("score:", (100 * length(type_result) / length(run_result))))
}
