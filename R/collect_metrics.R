#' @importFrom tune collect_metrics
#' @export
tune::collect_metrics

#' Summarize threshold metrics across folds
#'
#' @description
#' This function calculates means and standard deviations for each
#' metric at each threshold value across all folds.
#'
#' See `?cv_threshold_perf` and `probably::threshold_perf` for details.
#'
#' Can be piped into `autoplot()`.
#'
#' @param x The data.frame produced by `cv_threshold_perf()`.
#' @param ... Not currently used.
#'
#' @return A `data.frame` with class `cv_threshold_results`.
#' @export
collect_metrics.cv_thresholds <- function(x, ...) {
  out <-
    x %>%
    dplyr::group_by(.threshold, .metric, .estimator) %>%
    dplyr::summarize(
      mean = mean(.estimate),
      std_dev = stats::sd(.estimate),
      n = dplyr::n(),
      .groups = "drop"
    )

  class(out) <- c("cv_thresholds_results", class(out))

  return(out)
}
