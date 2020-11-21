#' Predict resampled data using multiple models
#'
#' @description
#' Fits a list of `workflow` objects to resampled data. Its output can be used to train
#' higher models in a stack.
#'
#' @param .workflows A named list of `workflow` objects.
#' @param objects A named list of `workflow` objects.
#' @param resamples A resample `rset` created from an `rsample` function such as `rsample::vfold_cv()`.
#'
#' @return A `data.frame` with one column for each fit's predictions.
#' @export
predict_resamples <- function(.workflows, resamples) {
  if(is.null(names(.workflows)))
    names(.workflows) <- 1:length(.workflows)
predict_resamples <- function(objects, resamples) {
  if(is.null(names(objects)))
    names(objects) <- 1:length(objects)

  purrr::map_dfc(
    1:length(.workflows),
    ~ .workflows[[.x]] %>%
    1:length(objects),
    function(i)
      objects[[i]] %>%
      tune::fit_resamples(resamples, control = tune::control_resamples(save_pred = TRUE)) %>%
      tune::collect_predictions() %>%
      dplyr::select(dplyr::starts_with(".pred")[1], ncol(.)) %>%
      stats::setNames(c(
        names(.workflows)[.x],
        paste0(names(.)[ncol(.)], "_", .x)
        names(objects)[i],
        paste0(names(.)[ncol(.)], "_", i)
      ))
  ) %>%
    dplyr::select(ncol(.), dplyr::all_of(names(.workflows))) %>%
    stats::setNames(c(sub("_[0-9]+$", "", names(.)[1]), names(.workflows)))
    dplyr::select(ncol(.), dplyr::all_of(names(objects))) %>%
    stats::setNames(c(sub("_[0-9]+$", "", names(.)[1]), names(objects)))
}
