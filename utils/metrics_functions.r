# Function to calculate micro and macro accuracy and macro accuracy with
# Bayesian smoothing
evaluation <- function(df, expected_col, predicted_col, adist_threshold) {
  # Keep rows where both expected and predicted groups are available.
  df_eval <- df %>%
    filter(!is.na(.data[[expected_col]]), !is.na(.data[[predicted_col]])) %>%
    mutate(
      expected_value = .data[[expected_col]],
      is_correct = mapply(
        function(pred, exp) adist(pred, exp)[1] < adist_threshold,
        .data[[predicted_col]],
        .data[[expected_col]]
      )
    )
  # Micro accuracy: global proportion of correct predictions.
  acc_micro <- mean(df_eval$is_correct, na.rm = TRUE)

  if (nrow(df_eval) == 0) {
    return(list(
      num_samples = 0,
      micro_accuracy = NA_real_,
      micro_accuracy_by_group = data.frame(
        expected_value = character(0),
        correct = integer(0),
        n = integer(0),
        n_samples = integer(0),
        acc_group = numeric(0),
        acc_group_bayes = numeric(0),
        stringsAsFactors = FALSE
      ),
      macro_accuracy = NA_real_,
      macro_accuracy_bayes = NA_real_
    ))
  }

  acc_micro_prior_strength <- df_eval %>%
    group_by(.data$expected_value) %>%
    summarise(
      correct = sum(.data$is_correct, na.rm = TRUE),
      total = sum(!is.na(.data$is_correct)),
      .groups = "drop"
    ) %>%
    mutate(acc_raw = .data$correct / .data$total)
  # Fit a beta-binomial model to estimate the prior strength for Bayesian smoothing.
  # If the fit is numerically unstable, keep a conservative fixed prior strength.
  prior_strength <- 2
  fit <- tryCatch(
    suppressWarnings(
      VGAM::vglm(
        cbind(correct, total - correct) ~ 1,
        family = VGAM::betabinomial,
        data = acc_micro_prior_strength
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(fit)) {
    coef_fit <- tryCatch(VGAM::Coef(fit), error = function(e) NULL)
    rho <- suppressWarnings(as.numeric(coef_fit["rho"]))
    if (is.finite(rho) && rho > 0 && rho < 1) {
      prior_strength <- (1 - rho) / rho
    }
  }

  # Calculate the alpha and beta parameters for the Beta distribution
  alpha <- acc_micro * prior_strength
  beta <- (1 - acc_micro) * prior_strength
  # Micro accuracy by group: 
  acc_group <- df_eval %>%
      group_by(.data$expected_value) %>%
      summarise(
      correct = sum(.data$is_correct, na.rm = TRUE),
      n = n(),
      n_samples = n(),
      acc_group = mean(.data$is_correct, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(acc_group_bayes = (.data$correct + alpha) / (.data$n + alpha + beta))
  # Macro accuracy: mean of the group accuracies.
  acc_macro <- mean(acc_group$acc_group, na.rm = TRUE)
  # Macro accuracy with Bayesian smoothing: mean of the group accuracies with Bayesian smoothing.
  macro_acc_bayes <- mean(acc_group$acc_group_bayes, na.rm = TRUE)
  return(list(
    num_samples = nrow(df_eval),
    micro_accuracy = acc_micro,
    micro_accuracy_by_group = acc_group,
    macro_accuracy = acc_macro,
    macro_accuracy_bayes = macro_acc_bayes
  ))
}