#' Estimate how many down-sampling replicates to use
#'
#' @description
#' Given data with an under-represented binary outcome, this function
#' estimates the number of times the data would need to be down-sampled
#' for the union of the resulting down-samples to contain a given
#' proportion of the over-represented class.
#'
#' @param x A `data.frame` containing the data to be down-sampled.
#' @param outcome A variable specified as:
#' - a literal variable name
#' - a positive integer, giving the position counting from the left
#' - a negative integer, giving the position counting from the right.
#' @param coverage The desired proportion of the over-represented class
#' to be represented in the aggregated down-samples.
#'
#' @return A positive integer
#' @export
default_replicates <- function(x, outcome, coverage = 0.8) {
  outcome_tab <-
    x %>%
    dplyr::pull({{outcome}}) %>%
    base::table()

  if(outcome_tab[[1]] >= outcome_tab[[2]])
    stop("The first level of the outcome is not under-represented in the data.")

  ceiling(log(1 - coverage)/log((outcome_tab[[2]] - outcome_tab[[1]])/(outcome_tab[[2]])))
}
