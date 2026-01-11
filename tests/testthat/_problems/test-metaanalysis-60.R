# Extracted from test-metaanalysis.R:60

# test -------------------------------------------------------------------------
skip_if_not_installed('jmvReadWrite')
devtools::load_all()
set.seed(123)
n <- 50
data <- data.frame(
    effect_size = runif(n, 1, 100),
    variance = runif(n, 1, 100),
    study_id = sample(c('A', 'B'), n, replace = TRUE),
    sample_size = runif(n, 1, 100),
    year = runif(n, 1, 100),
    true_positives = runif(n, 1, 100),
    false_positives = runif(n, 1, 100),
    false_negatives = runif(n, 1, 100),
    true_negatives = runif(n, 1, 100),
    treatment_arm = sample(c('A', 'B'), n, replace = TRUE),
    comparison_arm = sample(c('A', 'B'), n, replace = TRUE),
    subgroup_var = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars1 = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars2 = sample(c('A', 'B'), n, replace = TRUE),
    moderator_vars3 = sample(c('A', 'B'), n, replace = TRUE)
  )
model <- metaanalysis(
    data = data,
    effect_size = 'effect_size',
    variance = 'variance',
    study_id = 'study_id',
    sample_size = 'sample_size',
    year = 'year',
    analysis_type = 'generic',
    model_type = 'random_effects',
    effect_measure = 'odds_ratio',
    heterogeneity_method = 'dersimonian_laird',
    true_positives = 'true_positives',
    false_positives = 'false_positives',
    false_negatives = 'false_negatives',
    true_negatives = 'true_negatives',
    dta_model_type = 'bivariate',
    treatment_arm = 'treatment_arm',
    comparison_arm = 'comparison_arm',
    network_method = 'frequentist',
    publication_bias = TRUE,
    bias_tests = 'all_tests',
    subgroup_var = 'subgroup_var',
    moderator_vars = c('moderator_vars1', 'moderator_vars2', 'moderator_vars3'),
    meta_regression = FALSE,
    sensitivity_analysis = TRUE,
    outlier_detection = TRUE,
    forest_plot_options = TRUE,
    prediction_interval = TRUE,
    confidence_level = 0.95,
    robust_methods = FALSE,
    small_sample_correction = TRUE,
    knha_adjustment = TRUE
  )
