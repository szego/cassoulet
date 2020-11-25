#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' Plot cross-validated threshold metrics
#'
#' @param object The data frame produced by `cv_threshold_perf()` or
#' by `cv_threshold_perf() %>% collect_metrics()`.
#' @param ... Not currently used.
#'
#' @return A `ggplot2` object.
#' @export
autoplot.cv_thresholds_results <- function(object, ...) {
  ggplot2::ggplot(object) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = .threshold,
        ymin = mean - 2*std_dev,
        ymax = mean + 2*std_dev,
        fill = .metric
      ),
      alpha = 0.2
    ) +
    ggplot2::geom_line(ggplot2::aes(.threshold, mean, color = .metric)) +
    ggplot2::labs(
      title = "Cross-validated metrics",
      subtitle = "Means and 95% prediction intervals",
      x = "threshold",
      y = "mean estimate",
      fill = "metric",
      color = "metric"
    )
}

#' @describeIn autoplot.cv_thresholds_results Can be used directly on the output
#' of `cv_threshold_perf()`.
#' @export
autoplot.cv_thresholds <- function(object, ...) {
  object %>%
    collect_metrics() %>%
    autoplot()
}
