#' Get the number of predictors
#'
#' @description
#' Returns the number of variables with the "predictor" role in a recipe or workflow.
#'
#' @param x A `recipe` object or a `workflow` object with an attached recipe.
#'
#' @return An integer.
#' @export
count_predictors <- function(x) {
  UseMethod("count_predictors", x)
}

#' @describeIn count_predictors
#' @export
count_predictors.recipe <- function(x) {
  sum(x$var_info$role == "predictor")
}

#' @describeIn count_predictors
#' @export
count_predictors.workflow <- function(x) {
  sum(workflows::pull_workflow_preprocessor(x)$var_info$role == "predictor")
}
