#' Get variable importance metrics
#'
#' @description
#' This function calls the appropriate variable importance function
#' for the model engine used. See the Details section for the
#' of engines that are currently supported.
#'
#' @details
#' Currently only the `ranger` and `xgboost` engines are supported.
#'
#' @param object A fitted `workflow` object.
#'
#' @return Return type depends on model engine.
#' @export
variable_importance <- function(object) {
  if(!("workflow" %in% class(object)))
    stop("`object` must be a workflow.")

  engine <-
    object %>%
    workflows::pull_workflow_fit() %>%
    .$spec %>%
    .$engine

  if(!(engine %in% c("ranger", "xgboost")))
    stop("Model engine must be one of the following: ranger, xgboost")

  if(engine == "ranger") {
    object %>%
      workflows::pull_workflow_fit() %>%
      .$fit %>%
      ranger::importance() %>%
      return()
  } else if(engine == "xgboost") {
    xgboost::xgb.importance(
      model =
        object %>%
        workflows::pull_workflow_fit() %>%
        .$fit
    ) %>%
      return()
  }
}
