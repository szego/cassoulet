#' Get the number of predictors
#'
#' @description
#' Returns the number of variables with the "predictor" role in a recipe or workflow.
#'
#' @param object A `recipe` object or a `workflow` object with an attached recipe.
#'
#' @return An integer.
#' @export
count_predictors <- function(object) {
  UseMethod("count_predictors", object)
}

#' @describeIn count_predictors Get the number of predictors in a recipe.
#' @export
count_predictors.recipe <- function(object) {
  sum(object$var_info$role == "predictor")
}

#' @describeIn count_predictors Get the number of predictors in a workflow with a recipe.
#' @export
count_predictors.workflow <- function(object) {
  sum(workflows::pull_workflow_preprocessor(object)$var_info$role == "predictor")
}
