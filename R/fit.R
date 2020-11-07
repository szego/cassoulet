#' @importFrom generics fit
#' @export
generics::fit

#' Fit a list of models
#'
#' @description
#' Fits a list of workflows to a provided `data.frame`.
#'
#' @param .workflows A named list of `workflow` objects created by `wf_list()`.
#' @param data A `data.frame` containing data on which to fit the workflows.
#'
#' @return A named list of fitted `workflow` objects.
#' @export
fit.wflist <- function(.workflows, data) {
  out <- purrr::map(.workflows, ~fit(.x, data))

  class(out) <- c("wflist", class(out))

  return(out)
}
