% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/advancedtree.h.R
\name{advancedtree}
\alias{advancedtree}
\title{Advanced Decision Tree Analysis}
\usage{
advancedtree(
  data,
  vars,
  facs,
  target,
  targetLevel,
  algorithm = "rpart",
  validation = "cv",
  cv_folds = 5,
  test_split = 0.3,
  max_depth = 6,
  min_samples_split = 20,
  min_samples_leaf = 10,
  n_estimators = 100,
  learning_rate = 0.1,
  feature_selection = FALSE,
  importance_method = "gini",
  handle_imbalance = FALSE,
  imbalance_method = "weights",
  hyperparameter_tuning = FALSE,
  tuning_method = "random",
  show_tree_plot = TRUE,
  show_importance_plot = TRUE,
  show_performance_metrics = TRUE,
  show_validation_curves = TRUE,
  show_roc_curve = TRUE,
  show_calibration_plot = FALSE,
  show_confusion_matrix = TRUE,
  interpretability = FALSE,
  shap_analysis = FALSE,
  partial_dependence = FALSE,
  interaction_analysis = FALSE,
  clinical_context = "diagnosis",
  cost_sensitive_thresholds = FALSE,
  fn_fp_ratio = 1,
  missing_data_handling = "complete",
  export_model = FALSE,
  bootstrap_confidence = FALSE,
  n_bootstrap = 1000
)
}
\arguments{
\item{data}{The data as a data frame containing clinical variables,
biomarkers,  and patient outcomes for advanced decision tree analysis.}

\item{vars}{Continuous variables such as biomarker levels, age,  laboratory
values, or quantitative measurements.}

\item{facs}{Categorical variables such as tumor grade, stage,  histological
type, or patient demographics.}

\item{target}{Primary outcome variable: disease status, treatment response,
survival status, or diagnostic category.}

\item{targetLevel}{Level representing the positive outcome, event of
interest, or disease presence for binary classification.}

\item{algorithm}{Tree algorithm to use. Each offers different strengths:
CART (interpretable), ctree (unbiased), Random Forest (robust), XGBoost
(high performance), Extra Trees (fast), Ensemble (combined).}

\item{validation}{Validation approach for performance estimation.
Cross-validation provides robust estimates, bootstrap handles small
samples, holdout for large datasets, temporal for time-series data.}

\item{cv_folds}{Number of folds for cross-validation. 5-fold provides good
balance between bias and variance for most clinical datasets.}

\item{test_split}{Proportion of data reserved for testing (holdout
validation). Typical values: 20-30\\% for large datasets, 10-20\\% for small
ones.}

\item{max_depth}{Maximum depth of individual trees. Deeper trees capture
more interactions but may overfit. Clinical trees typically 2-8 levels.}

\item{min_samples_split}{Minimum number of samples required to split a
node. Higher values prevent overfitting in clinical data.}

\item{min_samples_leaf}{Minimum number of samples in leaf nodes. Important
for clinical validity - too few samples reduce reliability.}

\item{n_estimators}{Number of trees in ensemble methods (Random Forest,
XGBoost). More trees improve performance but increase computation time.}

\item{learning_rate}{Learning rate for gradient boosting. Lower values
require more trees but often achieve better performance.}

\item{feature_selection}{Perform automated feature selection using
tree-based importance. Helps identify most relevant clinical variables and
biomarkers.}

\item{importance_method}{Method for calculating feature importance.
Permutation and SHAP provide more reliable importance for clinical
interpretation.}

\item{handle_imbalance}{Address class imbalance common in clinical data
(rare diseases, adverse events). Uses algorithm-specific approaches.}

\item{imbalance_method}{Method for handling class imbalance. Class weights
are simple and effective, SMOTE generates synthetic samples.}

\item{hyperparameter_tuning}{Perform automated hyperparameter tuning using
grid search or random search. Improves model performance but increases
runtime.}

\item{tuning_method}{Hyperparameter optimization strategy. Random search is
efficient for most problems, Bayesian for complex scenarios.}

\item{show_tree_plot}{Display visual representation of the decision tree.
Most informative for single trees, less useful for ensembles.}

\item{show_importance_plot}{Display feature importance rankings. Critical
for understanding which clinical variables drive predictions.}

\item{show_performance_metrics}{Display comprehensive performance
evaluation including accuracy, sensitivity, specificity, AUC, and clinical
metrics.}

\item{show_validation_curves}{Display learning curves and validation
performance. Helps assess overfitting and training adequacy.}

\item{show_roc_curve}{Display ROC curve analysis for binary classification.
Essential for clinical decision making and threshold selection.}

\item{show_calibration_plot}{Display probability calibration plot.
Important for clinical applications requiring reliable probability
estimates.}

\item{show_confusion_matrix}{Display detailed confusion matrix with
clinical interpretations. Shows actual vs predicted classifications.}

\item{interpretability}{Perform advanced interpretability analysis
including SHAP values, partial dependence plots, and interaction effects.}

\item{shap_analysis}{Calculate SHAP (SHapley Additive exPlanations) values
for individual prediction explanations. Powerful for clinical decision
support.}

\item{partial_dependence}{Show how individual features affect predictions
across their value ranges. Helps understand clinical relationships.}

\item{interaction_analysis}{Analyze interactions between clinical
variables. Important for understanding combined effects of biomarkers.}

\item{clinical_context}{Clinical application context. Affects performance
thresholds, interpretation guidelines, and visualization emphasis.}

\item{cost_sensitive_thresholds}{Optimize decision thresholds considering
clinical costs of false positives vs false negatives.}

\item{fn_fp_ratio}{Relative cost of missing a positive case vs false alarm.
Screening (high ratio), confirmation tests (low ratio).}

\item{missing_data_handling}{Strategy for handling missing data. Tree-based
methods can handle missing values naturally in some algorithms.}

\item{export_model}{Export the trained model for external use or
deployment. Useful for clinical decision support system integration.}

\item{bootstrap_confidence}{Calculate bootstrap confidence intervals for
performance metrics. Provides uncertainty quantification for clinical
reporting.}

\item{n_bootstrap}{Number of bootstrap samples for confidence interval
calculation. More samples provide better estimates but increase computation
time.}
}
\value{
A results object containing:
\tabular{llllll}{
\code{results$todo} \tab \tab \tab \tab \tab a html \cr
\code{results$model_summary} \tab \tab \tab \tab \tab a html \cr
\code{results$tree_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$importance_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$performance_table} \tab \tab \tab \tab \tab a table \cr
\code{results$roc_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$validation_curves} \tab \tab \tab \tab \tab an image \cr
\code{results$confusion_matrix} \tab \tab \tab \tab \tab a html \cr
\code{results$calibration_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$shap_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$partial_dependence_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$interaction_plot} \tab \tab \tab \tab \tab an image \cr
\code{results$hyperparameter_results} \tab \tab \tab \tab \tab a html \cr
\code{results$clinical_interpretation} \tab \tab \tab \tab \tab a html \cr
\code{results$feature_selection_results} \tab \tab \tab \tab \tab a html \cr
\code{results$bootstrap_intervals} \tab \tab \tab \tab \tab a html \cr
\code{results$model_export} \tab \tab \tab \tab \tab a html \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$performance_table$asDF}

\code{as.data.frame(results$performance_table)}
}
\description{
Advanced decision tree analysis using modern algorithms for clinical
research and medical decision making.
Extends basic decision tree functionality with gradient boosting,
conditional inference trees,
ensemble methods, and enhanced interpretability. Provides comprehensive
performance evaluation,
feature importance analysis, and clinical-focused visualizations.
Complements existing
ClinicoPath decision tree modules with state-of-the-art algorithms.
}
\examples{
# Example: Advanced decision tree for cancer prognosis
data(cancer_data)
advancedtree(
    data = cancer_data,
    vars = c("age", "tumor_size", "biomarker1", "biomarker2"),
    facs = c("grade", "stage", "histology"),
    target = "outcome",
    targetLevel = "poor_prognosis",
    algorithm = "xgboost",
    validation = "cv",
    interpretability = TRUE
)

}
