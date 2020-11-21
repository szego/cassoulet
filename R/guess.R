#' Predict unseen data
#'
#' @description
#' An enhanced `predict()` that returns the provided data, predictions for that data,
#' and additional information about the predictions.
#'
#' @details
#' Setting mode = "regression" provides prediction intervals and setting
#' mode = "classification" provides class probabilities.
#'
#' @param .workflow A `workflow` object.
#' @param object A `workflow` object.
#' @param new_data A `data.frame` containing data with which to predict.
#' @param mode Either "regression" or "classification".
#'
#' @return A `data.frame` (new_data with new columns containing the predictions).
#' @export
guess <- function(.workflow, new_data, mode) {
guess <- function(object, new_data, mode) {
  if(mode == "regression") {
    type <- "pred_int"
  } else if(mode == "classification") {
    type <- "prob"
  } else {
    stop("Argument 'mode' must be one of the following: regression, classification")
    stop("`mode` must be one of the following: regression, classification")
  }

  dplyr::bind_cols(
    new_data,
    predict(.workflow, new_data = new_data),
    predict(.workflow, new_data = new_data, type = type)
    predict(object, new_data = new_data),
    predict(object, new_data = new_data, type = type)
  )
}
