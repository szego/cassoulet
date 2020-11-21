#' @importFrom stats predict
#' @export
stats::predict

#' Predict using multiple models
#'
#' @description
#' Makes predictions from multiple `workflow` objects.
#'
#' @param objects A named list of fitted `workflow` objects created by `wf_list()`.
#' @param new_data A `data.frame` containing data with which to predict.
#' @param mode Either "regression" or "classification".
#'
#' @return A `data.frame` with one column for each fit's predictions.
#' @export
predict.wflist <- function(objects, new_data, mode) {
  if(mode == "regression") {
    type <- "numeric"
  } else if(mode == "classification") {
    type <- "prob"
  } else {
    stop("`mode` must be one of the following: regression, classification")
  }

  if(is.null(names(objects)))
    names(objects) <- 1:length(objects)

  purrr::map_dfc(
    1:length(objects),
    function(i)
      objects[[i]] %>%
      stats::predict(new_data, type = type) %>%
      dplyr::select(dplyr::starts_with(".pred")[1]) %>%
      stats::setNames(names(objects)[i])
  )
}
