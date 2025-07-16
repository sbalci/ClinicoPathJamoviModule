# Advanced Decision Tree Analysis Documentation

This document provides a comprehensive overview of the Advanced Decision Tree Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Advanced Decision Tree Analysis module is a powerful tool for building predictive models using state-of-the-art decision tree algorithms. It is specifically designed for clinical research and medical decision-making, offering advanced validation, feature analysis, and interpretability options.

The module's features can be broadly categorized as follows:

*   **Core Modeling:** Implementation of various decision tree algorithms (CART, Conditional Inference Trees, Random Forest, XGBoost, Extra Trees, Ensemble Voting).
*   **Validation:** Robust validation methods including K-Fold Cross-Validation, Bootstrap, Hold-Out, and Time-Based splits.
*   **Feature Analysis:** Automated feature selection and importance ranking.
*   **Interpretability:** Enhanced interpretability analysis with SHAP values, partial dependence plots, and interaction analysis.
*   **Clinical Focus:** Specialized handling for class imbalance, cost-sensitive learning, and clinical metrics.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Modeling**                |                                |                                        |                                     |                                      |
| Continuous Predictors            | `vars`                         | Continuous Predictors                  | `model_summary`                     | `.prepare_data`                      |
| Categorical Predictors           | `facs`                         | Categorical Predictors                 | `model_summary`                     | `.prepare_data`                      |
| Target Outcome                   | `target`                       | Target Outcome                         | `model_summary`                     | `.prepare_data`                      |
| Positive/Event Level             | `targetLevel`                  | Positive/Event Level                   | `model_summary`                     | `.prepare_data`                      |
| Tree Algorithm                   | `algorithm`                    | Tree Algorithm                         | `model_summary`, `tree_plot`        | `.train_model`, `.plot_tree`         |
| **Validation**                   |                                |                                        |                                     |                                      |
| Validation Method                | `validation`                   | Validation Method                      | `model_summary`, `validation_curves`| `.train_model`, `.perform_cv`        |
| Number of CV Folds               | `cv_folds`                     | Number of CV Folds                     | `validation_curves`                 | `.perform_cv`                        |
| Test Set Proportion              | `test_split`                   | Test Set Proportion                    | `model_summary`                     | `.train_model`                       |
| **Advanced Tree Parameters**     |                                |                                        |                                     |                                      |
| Maximum Tree Depth               | `max_depth`                    | Maximum Tree Depth                     | `tree_plot`                         | `.train_model`                       |
| Minimum Samples to Split         | `min_samples_split`            | Minimum Samples to Split               | `tree_plot`                         | `.train_model`                       |
| Minimum Samples per Leaf         | `min_samples_leaf`             | Minimum Samples per Leaf               | `tree_plot`                         | `.train_model`                       |
| Number of Trees (Ensemble)       | `n_estimators`                 | Number of Trees (Ensemble)             | `model_summary`                     | `.train_model`                       |
| Learning Rate (Boosting)         | `learning_rate`                | Learning Rate (Boosting)               | `model_summary`                     | `.train_model`                       |
| **Feature Analysis**             |                                |                                        |                                     |                                      |
| Automated Feature Selection      | `feature_selection`            | Automated Feature Selection            | `feature_selection_results`         | `.generate_feature_selection_results`|
| Feature Importance Method        | `importance_method`            | Feature Importance Method              | `importance_plot`                   | `.calculate_importance`              |
| **Class Imbalance Handling**     |                                |                                        |                                     |                                      |
| Handle Class Imbalance           | `handle_imbalance`             | Handle Class Imbalance                 | `model_summary`                     | Not implemented in `.b.R`            |
| Imbalance Handling Method        | `imbalance_method`             | Imbalance Handling Method              | `model_summary`                     | Not implemented in `.b.R`            |
| **Hyperparameter Optimization**  |                                |                                        |                                     |                                      |
| Hyperparameter Optimization      | `hyperparameter_tuning`        | Hyperparameter Optimization            | `hyperparameter_results`            | `.generate_hyperparameter_results`   |
| Tuning Method                    | `tuning_method`                | Tuning Method                          | `hyperparameter_results`            | Not implemented in `.b.R`            |
| **Output and Visualization**     |                                |                                        |                                     |                                      |
| Show Decision Tree Visualization | `show_tree_plot`               | Show Decision Tree Visualization       | `tree_plot`                         | `.plot_tree`                         |
| Show Feature Importance Plot     | `show_importance_plot`         | Show Feature Importance Plot           | `importance_plot`                   | `.plot_importance`                   |
| Show Performance Metrics         | `show_performance_metrics`     | Show Performance Metrics               | `model_summary`, `performance_table`| `.generate_model_summary`, `.generate_performance_table` |
| Show Validation Curves           | `show_validation_curves`       | Show Validation Curves                 | `validation_curves`                 | `.plot_validation`                   |
| Show ROC Curve                   | `show_roc_curve`               | Show ROC Curve                         | `roc_plot`                          | `.plot_roc`                          |
| Show Calibration Plot            | `show_calibration_plot`        | Show Calibration Plot                  | `calibration_plot`                  | `.plot_calibration`                  |
| Show Confusion Matrix            | `show_confusion_matrix`        | Show Confusion Matrix                  | `confusion_matrix`                  | `.generate_confusion_matrix`         |
| **Interpretability Options**     |                                |                                        |                                     |                                      |
| Enhanced Interpretability Analysis| `interpretability`             | Enhanced Interpretability Analysis     | `shap_plot`, `partial_dependence_plot`, `interaction_plot` | Not implemented in `.b.R`            |
| SHAP Value Analysis              | `shap_analysis`                | SHAP Value Analysis                    | `shap_plot`                         | `.plot_shap`                         |
| Partial Dependence Plots         | `partial_dependence`           | Partial Dependence Plots               | `partial_dependence_plot`           | `.plot_partial_dependence`           |
| Feature Interaction Analysis     | `interaction_analysis`         | Feature Interaction Analysis           | `interaction_plot`                  | `.plot_interactions`                 |
| **Clinical Context**             |                                |                                        |                                     |                                      |
| Clinical Context                 | `clinical_context`             | Clinical Context                       | `clinical_interpretation`           | `.generate_clinical_interpretation`  |
| Cost-Sensitive Threshold Optimization| `cost_sensitive_thresholds`    | Cost-Sensitive Threshold Optimization  | `clinical_interpretation`           | Not implemented in `.b.R`            |
| False Negative to False Positive Cost Ratio| `fn_fp_ratio`                  | False Negative to False Positive Cost Ratio| `clinical_interpretation`           | Not implemented in `.b.R`            |
| **Advanced Options**             |                                |                                        |                                     |                                      |
| Missing Data Strategy            | `missing_data_handling`        | Missing Data Strategy                  | `model_summary`                     | `.prepare_data`                      |
| Export Trained Model             | `export_model`                 | Export Trained Model                   | `model_export`                      | `.generate_export_info`              |
| Bootstrap Confidence Intervals   | `bootstrap_confidence`         | Bootstrap Confidence Intervals         | `bootstrap_intervals`               | `.generate_bootstrap_results`        |
| Number of Bootstrap Samples      | `n_bootstrap`                  | Number of Bootstrap Samples            | `bootstrap_intervals`               | Not implemented in `.b.R`            |
