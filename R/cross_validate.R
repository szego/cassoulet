#' Cross-validate your workflows
#'
#' @description
#' This is a wrapper around `tune::fit_resamples()` that returns correct standard errors
#' for the cross-validation metrics.
#'
#' @details
#' Setting mode = "regression" calculates the cross-validated mean square error.
#'
#' Setting mode = "classification" calculates the cross-validated accuracy.
#'
#' For some reason, `tune::fit_resamples() %>% tune::collect_metrics()` does not return
#' the correct standard errors for its metrics. It's a bummer since the `tune` functions
#' are so easy to use.
#'
#' When called on a `wflist` of workflows, this function also provides information on how
#' the workflows compare to each other. In "regression" mode, the difference between each
#' workflow's mean square error and the best mean square error is shown. In
#' "classification" mode, the proportion of rows where each workflow disagrees with the
#' workflow with the highest accuracy is shown. Both of these additional metrics come
#' with standard errors.
#'
#' @param x A `workflow` object or a `wflist` of `workflow` objects created by `wf_list()`.
#' @param resamples A resample `rset` created from an `rsample` function such as `rsample::vfold_cv()`.
#' @param truth A character string containing the column identifier for the true class results.
#' @param mode Either "regression" or "classification".
#'
#' @return A `data.frame` containing the resulting metrics and their standard errors. When
#' `x` is a `wflist`, there is a row for each workflow's metrics.
#' @export
cross_validate <- function(x, resamples, truth, mode) {
  UseMethod("cross_validate", x)
}

#' @describeIn cross_validate Cross-validates a single workflow.
#' @export
cross_validate.workflow <- function(x, resamples, truth, mode) {
  if(mode == "regression") {
    x %>%
      tune::fit_resamples(
        resamples,
        control = tune::control_resamples(save_pred = TRUE)
      ) %>%
      tune::collect_predictions() %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(
        mse = mean((.pred - !!rlang::sym(truth))^2),
        mse_var = stats::var((.pred - !!rlang::sym(truth))^2),
        .groups = "drop"
      ) %>%
      dplyr::summarize(  # average the estimates from the folds
        mse = mean(mse),
        mse_se = sqrt(sum(mse_var))/dplyr::n()
      ) %>%
      return()
  } else if(mode == "classification") {
    x %>%
      tune::fit_resamples(
        resamples,
        control = tune::control_resamples(save_pred = TRUE)
      ) %>%
      tune::collect_predictions() %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(
        acc = mean(.pred_class == !!rlang::sym(truth)),
        acc_var = acc*(1-acc)/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::summarize(  # average the estimates from the folds
        acc = mean(acc),
        acc_se = sqrt(sum(acc_var))/dplyr::n()
      ) %>%
      return()
  } else {
    stop("`mode` must be one of the following: regression, classification")
  }
}

#' @describeIn cross_validate Cross-validates multiple workflows and generates comparison metrics.
#' @export
cross_validate.wflist <- function(x, resamples, truth, mode) {
  if(is.null(names(x)))
    names(x) <- 1:length(x)

  if(mode == "regression") {
    cvs <- purrr::map_dfr(
      1:length(x),
      function(i) {
        tune::fit_resamples(x[[i]], resamples, control = tune::control_resamples(save_pred = TRUE)) %>%
          tune::collect_predictions() %>%
          dplyr::mutate(
            .model = names(x)[i],
            .sq_residual = (.pred - !!rlang::sym(truth))^2
          ) %>%
          dplyr::select(.model, dplyr::everything())
      }
    )

    cvs_summary <-
      cvs %>%
      dplyr::group_by_at(dplyr::vars(.model, dplyr::matches("^id[0-9]*$"))) %>%
      dplyr::summarize(  # compute the MSE and standard error of the MSE for each fold
        mse = mean(.sq_residual),
        mse_var = stats::var(.sq_residual)/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(.model) %>%
      dplyr::summarize(  # average the estimates from the folds
        mse = mean(mse),
        mse_se = sqrt(sum(mse_var))/dplyr::n(),
        .groups = "drop"
      )

    lowest_mse_model <-
      cvs_summary %>%
      dplyr::slice(which.min(mse)) %>%
      dplyr::pull(.model)

    best_sq_residual <-
      cvs %>%
      dplyr::filter(.model == lowest_mse_model) %>%
      dplyr::pull(.sq_residual)

    cvs %>%
      dplyr::mutate(sq_r_diff = .sq_residual - rep_len(best_sq_residual, dplyr::n())) %>%
      dplyr::group_by_at(dplyr::vars(.model, dplyr::matches("^id[0-9]*$"))) %>%
      dplyr::summarize(
        mse_diff = mean(sq_r_diff),
        mse_diff_var = stats::var(sq_r_diff)/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(.model) %>%
      dplyr::summarize(
        mse_diff = mean(mse_diff),
        mse_diff_se = sqrt(sum(mse_diff_var))/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::left_join(cvs_summary, by = ".model") %>%
      dplyr::select(model = .model, mse, mse_se, mse_diff, mse_diff_se) %>%
      dplyr::arrange(match(model, names(x))) %>%
      return()
  } else if(mode == "classification") {
    cvs <- purrr::map_dfr(
      1:length(x),
      function(i) {
        tune::fit_resamples(x[[i]], resamples, control = tune::control_resamples(save_pred = TRUE)) %>%
          tune::collect_predictions() %>%
          dplyr::mutate(
            .model = names(x)[i],
            .correct = .pred_class == !!rlang::sym(truth)
          ) %>%
          dplyr::select(.model, dplyr::everything())
      }
    )

    cvs_summary <-
      cvs %>%
      dplyr::group_by_at(dplyr::vars(.model, dplyr::matches("^id[0-9]*$"))) %>%
      dplyr::summarize(  # compute the MSE and standard error of the MSE for each fold
        accuracy = mean(.correct),
        accuracy_var = accuracy*(1-accuracy)/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(.model) %>%
      dplyr::summarize(  # average the estimates from the folds
        accuracy = mean(accuracy),
        accuracy_se = sqrt(sum(accuracy_var))/dplyr::n(),
        .groups = "drop"
      )

    highest_accuracy_model <-
      cvs_summary %>%
      dplyr::slice(which.max(accuracy)) %>%
      dplyr::pull(.model)

    best_pred <-
      cvs %>%
      dplyr::filter(.model == highest_accuracy_model) %>%
      dplyr::pull(.pred_class)

    cvs %>%
      dplyr::mutate(disagreement = .pred_class != rep_len(best_pred, dplyr::n())) %>%
      dplyr::group_by_at(dplyr::vars(.model, dplyr::matches("^id[0-9]*$"))) %>%
      dplyr::summarize(
        disagreement = mean(disagreement),
        disagreement_var = disagreement*(1-disagreement)/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(.model) %>%
      dplyr::summarize(
        disagreement = mean(disagreement),
        disagreement_se = sqrt(sum(disagreement_var))/dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::left_join(cvs_summary, by = ".model") %>%
      dplyr::select(model = .model, accuracy, accuracy_se, disagreement, disagreement_se) %>%
      dplyr::arrange(match(model, names(x))) %>%
      return()
  } else {
    stop("`mode` must be one of the following: regression, classification")
  }
}
