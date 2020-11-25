#' @importFrom tune collect_metrics
#' @export
tune::collect_metrics

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
