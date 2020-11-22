#' Generate replicate recipes that down-sample the same data
#'
#' @description
#' `step_multidownsample()` replicates a given recipe and adds a
#' `themis::step_slice()` step to each replicate.
#'
#' @details
#' Each replicate recipe returned by this function includes a `step_slice()`
#' step that keeps all rows of the data from the under-represented class
#' and an equal number of rows from the over-represented class. The rows
#' from the over-represented class are chosen at random.
#'
#' Each row of the over-represented class can appear in at most one
#' replicate recipe's `step_slice()`.
#'
#' @param recipe A `recipe` object.
#' @param variable A single factor variable to be balanced in the
#' down-samples.
#'
#' @return A list of replicates of the provided recipe, each with a new
#' `themis::step_slice()` step added to the sequence of existing steps (if any).
#' @export
step_multidownsample <- function(recipe, variable) {
  if(length(variable) > 1)
    stop("`variable` must be a single factor variable.")

  underrep_class_rows <-
    recipe$template %>%
    dplyr::pull({{variable}}) %>%
    (function(y) which(y == levels(y)[1]))

  overrep_class_rows <-
    recipe$template %>%
    dplyr::pull({{variable}}) %>%
    (function(y) which(y == levels(y)[2])) %>%
    sample()

  replicates <- floor(length(overrep_class_rows)/length(underrep_class_rows))

  if(replicates == 0)
    stop("The first level of the variable is not under-represented in the data.")

  purrr::map(
    1:replicates,
    function(i) {
      rows_to_slice <- c(
        underrep_class_rows,
        overrep_class_rows[((i-1)*length(underrep_class_rows) + 1):(i*length(underrep_class_rows))]
      )

      recipes::step_slice(recipe, !!rows_to_slice)
    }
  )
}
