cv_threshold_perf <- function(object, resamples, outcome, thresholds, ...) {
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
