#' Create a list of workflows
#'
#' @description
#' Creates a list of class `wflist` for use with the `fit()` and `predict()` functions
#' in this package.
#'
#' @param ... Any number of `workflow` objects with names if desired.
#'
#' @return A list of class `wflist`.
#' @export
wf_list <- function(...) {
  return(structure(list(...), class = "wflist"))
}
