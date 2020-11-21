#' Predict resampled data using multiple models
#'
#' @description
#' Fits a list of `workflow` objects to resampled data. Its output can be used to train
#' higher models in a stack.
#'
#' @param objects A named list of `workflow` objects.
#' @param resamples A resample `rset` created from an `rsample` function such as `rsample::vfold_cv()`.
#'
#' @return A `data.frame` with one column for each fit's predictions.
#' @export
predict_resamples <- function(objects, resamples) {
  if(is.null(names(objects)))
    names(objects) <- 1:length(objects)

  outcome_variable <-
    objects[[1]] %>%
    workflows::pull_workflow_preprocessor() %>%
    .$var_info %>%
    dplyr::filter(role == "outcome") %>%
    dplyr::pull(variable)

  purrr::map_dfc(
    1:length(objects),
    function(i)
      objects[[i]] %>%
      tune::fit_resamples(resamples, control = tune::control_resamples(save_pred = TRUE)) %>%
      tune::collect_predictions() %>%
      dplyr::select(dplyr::starts_with(".pred")[1], dplyr::all_of(outcome_variable)) %>%
      stats::setNames(c(
        names(objects)[i],
        paste0(names(.)[ncol(.)], "_", i)
      ))
  ) %>%
    dplyr::select(ncol(.), dplyr::all_of(names(objects))) %>%
    stats::setNames(c(sub("_[0-9]+$", "", names(.)[1]), names(objects)))
}
