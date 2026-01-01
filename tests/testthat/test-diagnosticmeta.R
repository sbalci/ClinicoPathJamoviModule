testthat::test_that("diagnosticmeta pooled estimates align with mada", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  testthat::skip_if_not_installed("mada")
  testthat::skip_if_not_installed("metafor")

  data_path <- testthat::test_path("..", "..", "data", "diagnostic_meta_test.csv")
  diag_data <- utils::read.csv(data_path, stringsAsFactors = FALSE)

  results <- diagnosticmeta(
    data = diag_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = NULL,
    bivariate_analysis = TRUE,
    hsroc_analysis = FALSE,
    heterogeneity_analysis = FALSE,
    meta_regression = FALSE,
    publication_bias = FALSE,
    forest_plot = FALSE,
    sroc_plot = FALSE,
    funnel_plot = FALSE
  )

  bivariate_df <- results$bivariateresults$asDF

  mada_input <- data.frame(
    TP = diag_data$true_positives,
    FP = diag_data$false_positives,
    FN = diag_data$false_negatives,
    TN = diag_data$true_negatives
  )

  mada_fit <- mada::reitsma(mada_input, method = "reml")
  conf_level <- 0.95
  mada_summary <- summary(mada_fit, level = conf_level)
  coeff <- mada_summary[["coefficients"]]

  ci_lower_col <- grep("ci\\.lb$", colnames(coeff), value = TRUE)[1]
  ci_upper_col <- grep("ci\\.ub$", colnames(coeff), value = TRUE)[1]

  pooled_sens <- coeff["sensitivity", "Estimate"]
  sens_ci <- c(coeff["sensitivity", ci_lower_col], coeff["sensitivity", ci_upper_col])

  fpr_estimate <- coeff["false pos. rate", "Estimate"]
  fpr_ci <- c(coeff["false pos. rate", ci_lower_col], coeff["false pos. rate", ci_upper_col])
  pooled_spec <- 1 - fpr_estimate
  spec_ci <- c(1 - fpr_ci[2], 1 - fpr_ci[1])

  sens_row <- bivariate_df[bivariate_df$parameter == "Pooled Sensitivity", ]
  spec_row <- bivariate_df[bivariate_df$parameter == "Pooled Specificity", ]

  testthat::expect_equal(sens_row$estimate, pooled_sens * 100, tolerance = 1e-3)
  testthat::expect_equal(sens_row$ci_lower, sens_ci[1] * 100, tolerance = 1e-3)
  testthat::expect_equal(sens_row$ci_upper, sens_ci[2] * 100, tolerance = 1e-3)

  testthat::expect_equal(spec_row$estimate, pooled_spec * 100, tolerance = 1e-3)
  testthat::expect_equal(spec_row$ci_lower, spec_ci[1] * 100, tolerance = 1e-3)
  testthat::expect_equal(spec_row$ci_upper, spec_ci[2] * 100, tolerance = 1e-3)

  pooled_plr <- pooled_sens / (1 - pooled_spec)
  pooled_nlr <- (1 - pooled_sens) / pooled_spec
  pooled_dor <- pooled_plr / pooled_nlr

  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  vcov_mat <- mada_fit$vcov

  var_logit_sens <- vcov_mat[1, 1]
  var_logit_spec <- vcov_mat[2, 2]
  cov_sens_spec <- vcov_mat[1, 2]

  var_log_plr <- ((1 - pooled_sens)^2 * var_logit_sens) +
    (pooled_spec^2 * var_logit_spec) +
    (2 * (1 - pooled_sens) * pooled_spec * cov_sens_spec)

  var_log_nlr <- (pooled_sens^2 * var_logit_sens) +
    ((1 - pooled_spec)^2 * var_logit_spec) +
    (2 * pooled_sens * (1 - pooled_spec) * cov_sens_spec)

  var_log_dor <- var_logit_sens + var_logit_spec + 2 * cov_sens_spec

  lr_ci_plr <- exp(log(pooled_plr) + c(-1, 1) * z_crit * sqrt(var_log_plr))
  lr_ci_nlr <- exp(log(pooled_nlr) + c(-1, 1) * z_crit * sqrt(var_log_nlr))
  lr_ci_dor <- exp(log(pooled_dor) + c(-1, 1) * z_crit * sqrt(var_log_dor))

  plr_row <- bivariate_df[bivariate_df$parameter == "Positive Likelihood Ratio", ]
  nlr_row <- bivariate_df[bivariate_df$parameter == "Negative Likelihood Ratio", ]
  dor_row <- bivariate_df[bivariate_df$parameter == "Diagnostic Odds Ratio", ]

  testthat::expect_equal(plr_row$estimate, pooled_plr, tolerance = 1e-3)
  testthat::expect_equal(plr_row$ci_lower, lr_ci_plr[1], tolerance = 1e-3)
  testthat::expect_equal(plr_row$ci_upper, lr_ci_plr[2], tolerance = 1e-3)

  testthat::expect_equal(nlr_row$estimate, pooled_nlr, tolerance = 1e-3)
  testthat::expect_equal(nlr_row$ci_lower, lr_ci_nlr[1], tolerance = 1e-3)
  testthat::expect_equal(nlr_row$ci_upper, lr_ci_nlr[2], tolerance = 1e-3)

  testthat::expect_equal(dor_row$estimate, pooled_dor, tolerance = 1e-3)
  testthat::expect_equal(dor_row$ci_lower, lr_ci_dor[1], tolerance = 1e-3)
  testthat::expect_equal(dor_row$ci_upper, lr_ci_dor[2], tolerance = 1e-3)
})

testthat::test_that("diagnosticmeta respects optional analysis flags", {
  testthat::skip_if_not_installed("mada")
  testthat::skip_if_not_installed("metafor")

  data_path <- testthat::test_path("..", "..", "data", "diagnostic_meta_test.csv")
  diag_data <- utils::read.csv(data_path, stringsAsFactors = FALSE)

  disabled_results <- diagnosticmeta(
    data = diag_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = NULL,
    bivariate_analysis = FALSE,
    hsroc_analysis = FALSE,
    heterogeneity_analysis = FALSE,
    meta_regression = FALSE,
    publication_bias = FALSE,
    forest_plot = FALSE,
    sroc_plot = FALSE,
    funnel_plot = FALSE
  )

  testthat::expect_equal(disabled_results$bivariateresults$rowCount, 0)
  testthat::expect_equal(disabled_results$hsrocresults$rowCount, 0)
  testthat::expect_equal(disabled_results$heterogeneity$rowCount, 0)
  testthat::expect_equal(disabled_results$metaregression$rowCount, 0)
  testthat::expect_equal(disabled_results$publicationbias$rowCount, 0)
  testthat::expect_null(disabled_results$srocplot$state)

  enabled_results <- diagnosticmeta(
    data = diag_data,
    study = study_name,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    true_negatives = true_negatives,
    covariate = NULL,
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE,
    heterogeneity_analysis = TRUE,
    meta_regression = FALSE,
    publication_bias = TRUE,
    forest_plot = FALSE,
    sroc_plot = TRUE,
    funnel_plot = TRUE
  )

  testthat::expect_true(enabled_results$bivariateresults$rowCount >= 2)
  testthat::expect_true(enabled_results$hsrocresults$rowCount >= 1)
  testthat::expect_true(enabled_results$heterogeneity$rowCount >= 1)
  testthat::expect_true(enabled_results$publicationbias$rowCount >= 1)
  testthat::expect_false(is.null(enabled_results$srocplot$state))
})
