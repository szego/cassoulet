#' Add a list of recipes to a workflow
#'
#' @description
#' This function replicates the provided workflow, adding one of the
#' provided recipes to each replicate.
#'
#' @param .workflow A `workflow` object.
#' @param .recipes A list of `recipe` objects.
#' @param ... Arguments passed on to `workflows::add_recipe()`.
#'
#' @return A `wflist` of replicate workflows, one for each provided recipe.
#' @export
add_recipes <- function(.workflow, .recipes, ...) {
  .recipes %>%
    purrr::map(function(.x) workflows::add_recipe(.workflow, .x, ...)) %>%
    structure(class = "wflist")
}
