#' Generate replicate recipes that down-sample the same data
#'
#' @description
#' `step_multidownsample()` replicates a given recipe and adds a
#' `themis::step_downsample()` step to each replicate.
#'
#' @param recipe A `recipe` object.
#' @param ... One or more selector functions to choose which variable is used
#' to sample the data. See `recipes::selections()` for more details. The
#' selection should result in single factor variable.
#' @param .replicates A positive integer indicating how many times to replicate
#' the recipe.
#'
#' @return A list of replicates of the provided recipe, each with a new
#' `themis::step_downsample()` step added to the sequence of existing steps (if any).
#' @export
step_multidownsample <- function(recipe, ..., .replicates) {
  x <- rlang::enquos(...)
  purrr::map(1:.replicates, ~ themis::step_downsample(recipe, !!!x))
}
