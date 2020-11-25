#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

autoplot.cv_thresholds_results <- function(x, ...) {
  ggplot2::ggplot(x) +
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
