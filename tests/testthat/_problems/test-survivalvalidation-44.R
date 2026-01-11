# Extracted from test-survivalvalidation.R:44

# test -------------------------------------------------------------------------
skip_if_not_installed('jmvReadWrite')
devtools::load_all()
set.seed(123)
n <- 50
data <- data.frame(
    time = runif(n, 1, 100),
    status = sample(c('A', 'B'), n, replace = TRUE),
    predicted_risk = runif(n, 1, 100),
    cause_specific = sample(c('A', 'B'), n, replace = TRUE)
  )
model <- survivalvalidation(
    data = data,
    time = 'time',
    status = 'status',
    predicted_risk = 'predicted_risk',
    external_data = NULL,
    validation_method = 'cv',
    cv_folds = 10,
    bootstrap_samples = 100,
    concordance_index = TRUE,
    time_dependent_auc = TRUE,
    prediction_error = TRUE,
    integrated_brier = TRUE,
    calibration_plot = TRUE,
    decision_curve = TRUE,
    max_time = 0,
    plot_roc_curves = TRUE,
    plot_calibration = TRUE,
    plot_decision_curve = TRUE,
    plot_prediction_error = TRUE,
    confidence_level = 0.95,
    smoothing = TRUE,
    risk_groups = 4,
    competing_risks = FALSE,
    cause_specific = 'cause_specific',
    model_comparison = FALSE
  )
