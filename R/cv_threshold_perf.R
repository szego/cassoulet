#' Title Generate cross-validated performance metrics across probability thresholds
#'
#' @description
#' This function calculates out-of-sample performance characteristics for class
#' probability thresholds for each provided split.
#'
#' @details
#' For each training-testing pair, this function fits a model on the training
#' set and generates class probability predictions for the testing set.
#' `probably::threshold_perf()` is called on those out-of-sample predictions.
#'
#' See `?probably::threshold_perf` for details.
#'
#' @param object A `workflow` object.
#' @param resamples A resample `rset` created from an `rsample` function such
#' as `rsample::vfold_cv()`.
#' @param outcome A character string containing the column identifier for the
#' outcome variable.
#' @param thresholds A numeric vector of values for the probability threshold.
#' If unspecified, a series of values between 0.5 and 1.0 are used. Note: if
#' this argument is used, it must be named.
#' @param ... Additional arguments passed on to `probably::threshold_perf()`.
#'
#' @return A `data.frame` containing the resulting metrics for each threshold
#' and fold.
#' @export
cv_threshold_perf <- function(object, resamples, outcome, thresholds = NULL, ...) {
  if(!("workflow" %in% class(object)))
    stop("`object` must be a workflow.")

  pred_col <-
    paste0(
      ".pred_",
      levels(resamples$splits[1][[1]]$data %>% dplyr::pull({{outcome}}))[1]
    ) %>%
    rlang::sym()

  out <- purrr::map_dfr(
    1:nrow(resamples),
    function(i)
      object %>%
      tune::last_fit(resamples$splits[i][[1]]) %>%
      tune::collect_predictions() %>%
      probably::threshold_perf(
        truth = {{outcome}},
        estimate = {{pred_col}},
        thresholds = thresholds,
        ...
      ) %>%
      dplyr::bind_cols(resamples %>% dplyr::select(-splits) %>% dplyr::slice(i))
  )

  class(out) <- c("cv_thresholds", class(out))

  return(out)
}
