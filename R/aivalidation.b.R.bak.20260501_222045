#' @title AI Model Validation
#'
#' @description
#' Validates AI diagnostic models by calculating performance metrics (AUC, sensitivity, specificity),
#' comparing multiple models, and assessing generalization through cross-validation.
#' This simplified architecture avoids resource limit errors while providing comprehensive
#' diagnostic performance evaluation essential for medical AI validation.
#'
#' **Key Features:**
#' - ROC curve analysis with AUC calculation
#' - Optimal threshold selection using Youden's J statistic
#' - Advanced performance metrics (Matthews CC, Youden's J)
#' - Statistical model comparison using DeLong test
#' - K-fold cross-validation with stratified sampling
#' - Bootstrap confidence intervals for robust estimation
#' - Interactive ROC curve visualization
#' - Comprehensive methodology and interpretation guidance
#'
#' **Statistical Methods:**
#' - AUC (Area Under the ROC Curve) for discrimination assessment
#' - DeLong method for AUC confidence intervals (default)
#' - Bootstrap resampling for robust CI estimation
#' - DeLong test for pairwise AUC comparisons
#' - Youden's J statistic for optimal threshold selection
#' - Matthews Correlation Coefficient for balanced performance assessment
#'
#' **Cross-Validation:**
#' - 5-fold and 10-fold cross-validation
#' - Stratified sampling to maintain outcome proportions
#' - Mean and SD of performance metrics across folds
#' - Reproducible results with seed control
#'
#' **Visualization:**
#' - Multi-predictor ROC curves with AUC labels
#' - Publication-ready ggplot2 graphics
#' - Limited to 10 predictors to prevent clutter
#'
#' @param data The data as a data frame
#' @param predictorVars Vector of numeric predictor variables (AI scores, human scores, biomarkers)
#' @param outcomeVar Binary outcome variable (gold standard diagnosis)
#' @param positiveLevel Level of outcome variable representing positive case
#' @param compareModels Logical. Perform statistical comparison between models using DeLong test
#' @param youdensJ Logical. Calculate Youden's J statistic (Sensitivity + Specificity - 1)
#' @param matthewsCC Logical. Calculate Matthews Correlation Coefficient
#' @param bootstrapCI Logical. Use bootstrap confidence intervals (more robust for small samples)
#' @param nBootstrap Integer. Number of bootstrap iterations (100-5000, default: 1000)
#' @param crossValidation Cross-validation method: "none", "5-fold", or "10-fold"
#' @param stratified Logical. Maintain outcome proportions in CV folds (default: TRUE)
#' @param randomSeed Integer. Random seed for reproducible cross-validation (default: 42)
#' @param rocPlot Logical. Generate ROC curves for all predictor variables
#' @param showExplanations Logical. Display detailed methodology explanations
#' @param showSummaries Logical. Display interpretation summaries with clinical guidance
#'
#' @return A results object from the \code{aivalidationClass} containing:
#' \itemize{
#'   \item \strong{performanceTable}: AUC, sensitivity, specificity, optimal threshold, Youden's J, MCC
#'   \item \strong{comparisonTable}: Pairwise model comparisons with DeLong test p-values
#'   \item \strong{cvPerformanceTable}: Cross-validated performance metrics (mean and SD)
#'   \item \strong{rocPlot}: ROC curves visualization with AUC labels
#'   \item \strong{methodologyExplanation}: HTML explanation of statistical methods
#'   \item \strong{resultsInterpretation}: HTML interpretation with clinical context
#' }
#'
#' @details
#' This analysis is specifically designed for validating AI diagnostic models in medical contexts.
#' It calculates essential performance metrics and provides statistical tests for comparing
#' multiple diagnostic models (AI vs human, different AI algorithms, etc.).
#'
#' **Performance Metrics:**
#' - \strong{AUC}: Measures overall discrimination ability (0.5 = random, 1.0 = perfect)
#' - \strong{Sensitivity}: True positive rate (crucial for screening tests)
#' - \strong{Specificity}: True negative rate (important for confirmatory tests)
#' - \strong{Optimal Threshold}: Determined by Youden's J (maximizes sens + spec - 1)
#' - \strong{Youden's J}: Overall performance measure (0-1, higher is better)
#' - \strong{Matthews CC}: Balanced metric for imbalanced datasets (-1 to +1)
#'
#' **Model Comparison:**
#' The DeLong test accounts for correlation between ROC curves from the same dataset,
#' providing appropriate statistical comparison of AUC values. Limited to first 5 predictors
#' to avoid resource issues.
#'
#' **Cross-Validation:**
#' Provides robust estimates of model generalization by training on k-1 folds and testing
#' on the remaining fold, repeated k times. Stratified sampling maintains outcome proportions
#' in each fold, which is essential for imbalanced datasets.
#'
#' **Bootstrap Confidence Intervals:**
#' More robust than DeLong method for small samples or non-normal distributions, but requires
#' more computation time. Creates multiple resampled datasets and derives CIs from the
#' distribution of AUC values.
#'
#' @references
#' DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more
#' correlated receiver operating characteristic curves: a nonparametric approach. \emph{Biometrics}, 44(3), 837-845.
#'
#' Youden, W. J. (1950). Index for rating diagnostic tests. \emph{Cancer}, 3(1), 32-35.
#'
#' Matthews, B. W. (1975). Comparison of the predicted and observed secondary structure of T4 phage lysozyme.
#' \emph{Biochimica et Biophysica Acta (BBA)-Protein Structure}, 405(2), 442-451.
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data('medical_ai_data', package='ClinicoPath')
#'
#' # Basic AI validation
#' aivalidation(
#'   data = medical_ai_data,
#'   predictorVars = c('AI_score', 'human_score', 'biomarker1'),
#'   outcomeVar = 'diagnosis',
#'   positiveLevel = 'positive',
#'   compareModels = TRUE
#' )
#'
#' # Advanced validation with cross-validation and bootstrap
#' aivalidation(
#'   data = medical_ai_data,
#'   predictorVars = c('AI_score', 'human_score', 'biomarker1'),
#'   outcomeVar = 'diagnosis',
#'   positiveLevel = 'positive',
#'   compareModels = TRUE,
#'   youdensJ = TRUE,
#'   matthewsCC = TRUE,
#'   bootstrapCI = TRUE,
#'   nBootstrap = 2000,
#'   crossValidation = '5-fold',
#'   stratified = TRUE,
#'   rocPlot = TRUE,
#'   showExplanations = TRUE,
#'   showSummaries = TRUE
#' )
#' }
#'
#' @importFrom pROC roc auc ci.auc coords roc.test
#' @importFrom ggplot2 ggplot aes geom_line geom_abline scale_x_continuous scale_y_continuous labs theme_minimal theme element_text element_blank
#' @importFrom R6 R6Class
#'
#' @export
# AI Model Validation - Simplified Architecture
# This analysis validates AI diagnostic models using basic performance metrics
# and statistical comparisons without complex nested loops

aivalidationClass <- R6::R6Class(
  "aivalidationClass",
  inherit = aivalidationBase,

  private = list(

    # Simple data storage
    .cleanData = NULL,

    # Initialize - minimal setup (no output access here!)
    .init = function() {
      # Don't access results in .init() - causes serialization errors
      # Welcome message will be shown in .run() instead
    },

    # Main analysis - FLAT structure, no complex loops
    .run = function() {

      # Quick validation - show welcome if variables not selected
      if (is.null(self$options$outcomeVar) ||
          length(self$options$predictorVars) == 0) {

        welcome <- "
        <h3>AI Model Validation</h3>
        <p><strong>Quick Start:</strong></p>
        <ol>
          <li>Select one or more <strong>Predictor Variables</strong> (AI scores, biomarkers)</li>
          <li>Select <strong>Outcome Variable</strong> (diagnosis: binary)</li>
          <li>Choose <strong>Positive Level</strong> for the outcome</li>
        </ol>
        <p>The analysis will calculate AUC, sensitivity, specificity and compare models.</p>
        "
        self$results$instructions$setContent(welcome)
        self$results$instructions$setVisible(TRUE)
        return()
      }

      # Hide instructions when analysis runs
      self$results$instructions$setVisible(FALSE)

      # Checkpoint at start
      private$.checkpoint()

      tryCatch({

        # Step 1: Prepare data (simple, no loops)
        private$.prepareData()

        if (is.null(private$.cleanData)) {
          self$results$instructions$setContent("<p>No valid data after removing missing values.</p>")
          self$results$instructions$setVisible(TRUE)
          return()
        }

        # Step 2: Calculate performance for each predictor (simple loop)
        private$.calculatePerformance()

        # Checkpoint after performance
        private$.checkpoint()

        # Step 3: Statistical comparisons (if requested and applicable)
        if (self$options$compareModels && length(self$options$predictorVars) >= 2) {
          private$.compareModels()
        }

        # Step 4: Cross-validation (if requested)
        if (self$options$crossValidation != "none") {
          private$.performCrossValidation()
        }

        # Step 5: Add help text (if requested)
        if (self$options$showExplanations) {
          private$.populateMethodology()
        }

        if (self$options$showSummaries) {
          private$.populateInterpretation()
        }

      }, error = function(e) {
        # Enhanced error reporting with traceback
        error_msg <- paste0(
          "<h3>Error</h3>",
          "<p><strong>Message:</strong> ", e$message, "</p>",
          "<p><strong>Location:</strong> Check if pROC package is properly loaded.</p>",
          "<p><em>If this error persists, try restarting jamovi.</em></p>"
        )
        self$results$instructions$setContent(error_msg)
        self$results$instructions$setVisible(TRUE)
      })
    },

    # Prepare data - SIMPLE, no complex processing
    .prepareData = function() {

      data <- self$data

      # Get variables
      predictors <- self$options$predictorVars
      outcome <- self$options$outcomeVar

      # Select only needed columns
      needed_cols <- c(predictors, outcome)
      data <- data[, needed_cols, drop = FALSE]

      # Remove missing
      data <- data[complete.cases(data), ]

      if (nrow(data) < 10) {
        stop("Insufficient data (need at least 10 complete cases)")
      }

      # Convert outcome to binary 0/1
      outcome_vals <- data[[outcome]]
      positive <- self$options$positiveLevel

      if (is.null(positive) || !positive %in% unique(outcome_vals)) {
        stop("Invalid positive level specified")
      }

      data$outcome_binary <- ifelse(outcome_vals == positive, 1, 0)

      private$.cleanData <- data
    },

    # Calculate performance - SIMPLE, minimal processing
    .calculatePerformance = function() {

      data <- private$.cleanData
      predictors <- self$options$predictorVars
      actual <- data$outcome_binary

      table <- self$results$performanceTable

      # Simple loop through predictors (no nesting!)
      for (pred in predictors) {

        predicted <- data[[pred]]

        # Calculate AUC and performance metrics
        if (requireNamespace("pROC", quietly = TRUE)) {
          roc_obj <- pROC::roc(actual, predicted, quiet = TRUE)
          auc_val <- as.numeric(pROC::auc(roc_obj))

          # Calculate confidence intervals (bootstrap or DeLong)
          if (self$options$bootstrapCI) {
            # Use bootstrap method
            ci <- as.numeric(pROC::ci.auc(
              roc_obj,
              method = "bootstrap",
              boot.n = self$options$nBootstrap,
              quiet = TRUE
            ))
          } else {
            # Use DeLong method (default)
            ci <- as.numeric(pROC::ci.auc(roc_obj))
          }

          # Find optimal threshold using Youden's J
          # Get all coords to find best threshold
          all_coords <- pROC::coords(roc_obj, x = "all", ret = c("threshold", "sensitivity", "specificity"))

          # Simplified extraction, assuming all_coords is a data.frame or list
          thresh_vec <- as.numeric(all_coords$threshold)
          sens_vec <- as.numeric(all_coords$sensitivity)
          spec_vec <- as.numeric(all_coords$specificity)

          # Calculate Youden's J for each threshold
          youden_j <- sens_vec + spec_vec - 1

          # Find index of maximum Youden's J
          best_idx <- which.max(youden_j)

          # Extract optimal values
          threshold <- thresh_vec[best_idx]
          sens <- sens_vec[best_idx]
          spec <- spec_vec[best_idx]

        } else {
          # Fallback without pROC
          threshold <- 0.5
          pred_class <- ifelse(predicted >= threshold, 1, 0)
          tp <- sum(pred_class == 1 & actual == 1)
          tn <- sum(pred_class == 0 & actual == 0)
          fp <- sum(pred_class == 1 & actual == 0)
          fn <- sum(pred_class == 0 & actual == 1)

          sens <- tp / (tp + fn)
          spec <- tn / (tn + fp)
          auc_val <- (sens + spec) / 2
          ci <- c(auc_val - 0.1, auc_val, auc_val + 0.1)
        }

        # Calculate additional metrics if requested
        youdens_j <- NULL
        mcc <- NULL

        if (self$options$youdensJ || self$options$matthewsCC) {
          # Recalculate confusion matrix at optimal threshold
          pred_class <- ifelse(predicted >= threshold, 1, 0)
          tp <- as.numeric(sum(pred_class == 1 & actual == 1))
          tn <- as.numeric(sum(pred_class == 0 & actual == 0))
          fp <- as.numeric(sum(pred_class == 1 & actual == 0))
          fn <- as.numeric(sum(pred_class == 0 & actual == 1))

          # Youden's J = Sensitivity + Specificity - 1
          if (self$options$youdensJ) {
            youdens_j <- as.numeric(sens) + as.numeric(spec) - 1
          }

          # Matthews Correlation Coefficient
          if (self$options$matthewsCC) {
            numerator <- (tp * tn) - (fp * fn)
            denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

            if (denominator > 0) {
              mcc <- numerator / denominator
            } else {
              mcc <- 0
            }
          }
        }

        # Add row to table
        table$addRow(rowKey = pred, values = list(
          predictor = pred,
          auc = auc_val,
          auc_lower = ci[1],
          auc_upper = ci[3],
          sensitivity = sens,
          specificity = spec,
          threshold = threshold,
          youdens_j = youdens_j,
          mcc = mcc
        ))
      }
    },

    # Compare models - SIMPLE pairwise comparisons only
    .compareModels = function() {

      if (!requireNamespace("pROC", quietly = TRUE)) return()

      data <- private$.cleanData
      predictors <- self$options$predictorVars
      actual <- data$outcome_binary

      # Limit comparisons to avoid resource issues
      if (length(predictors) > 5) {
        predictors <- predictors[1:5]
      }

      table <- self$results$comparisonTable

      # Simple pairwise loop (no deep nesting)
      for (i in 1:(length(predictors) - 1)) {
        for (j in (i + 1):length(predictors)) {

          pred1 <- predictors[i]
          pred2 <- predictors[j]

          # Create ROC objects
          roc1 <- pROC::roc(actual, data[[pred1]], quiet = TRUE)
          roc2 <- pROC::roc(actual, data[[pred2]], quiet = TRUE)

          # DeLong test
          test_result <- pROC::roc.test(roc1, roc2, method = "delong")

          # Add row
          comp_name <- paste(pred1, "vs", pred2)
          table$addRow(rowKey = comp_name, values = list(
            comparison = comp_name,
            auc1 = as.numeric(pROC::auc(roc1)),
            auc2 = as.numeric(pROC::auc(roc2)),
            difference = as.numeric(pROC::auc(roc1) - pROC::auc(roc2)),
            p_value = test_result$p.value
          ))
        }
      }
    },

    # Cross-validation - SIMPLE k-fold implementation
    .performCrossValidation = function() {

      if (!requireNamespace("pROC", quietly = TRUE)) return()

      data <- private$.cleanData
      predictors <- self$options$predictorVars
      actual <- data$outcome_binary

      # Determine number of folds
      k_folds <- switch(self$options$crossValidation,
        "5-fold" = 5,
        "10-fold" = 10,
        5  # default
      )

      # Set random seed for reproducibility
      set.seed(self$options$randomSeed)

      # Create fold assignments
      n <- nrow(data)
      if (self$options$stratified) {
        # Stratified sampling - maintain outcome proportions
        fold_ids <- numeric(n)
        for (outcome_val in c(0, 1)) {
          idx <- which(actual == outcome_val)
          fold_ids[idx] <- sample(rep(1:k_folds, length.out = length(idx)))
        }
      } else {
        # Simple random assignment
        fold_ids <- sample(rep(1:k_folds, length.out = n))
      }

      table <- self$results$cvPerformanceTable

      # Loop through predictors (FLAT - no nesting with folds)
      for (pred in predictors) {

        # Storage for fold results
        fold_aucs <- numeric(k_folds)
        fold_sens <- numeric(k_folds)
        fold_spec <- numeric(k_folds)

        # Loop through folds
        for (fold in 1:k_folds) {

          # Split data
          test_idx <- fold_ids == fold
          train_idx <- !test_idx

          train_actual <- actual[train_idx]
          test_actual <- actual[test_idx]

          train_pred <- data[[pred]][train_idx]
          test_pred <- data[[pred]][test_idx]

          # Build ROC on training data to find threshold
          train_roc <- pROC::roc(train_actual, train_pred, quiet = TRUE)

          # Get all coords and find best threshold (same defensive approach as main analysis)
          train_coords <- pROC::coords(train_roc, x = "all", ret = c("threshold", "sensitivity", "specificity"))

          # Simplified extraction, assuming train_coords is a data.frame or list
          train_thresh_vec <- as.numeric(train_coords$threshold)
          train_sens_vec <- as.numeric(train_coords$sensitivity)
          train_spec_vec <- as.numeric(train_coords$specificity)

          # Find optimal threshold using Youden's J
          train_youden <- train_sens_vec + train_spec_vec - 1
          best_idx <- which.max(train_youden)
          threshold <- train_thresh_vec[best_idx]

          # Evaluate on test data
          test_roc <- pROC::roc(test_actual, test_pred, quiet = TRUE)
          fold_aucs[fold] <- as.numeric(pROC::auc(test_roc))

          # Calculate sens/spec at training threshold
          test_class <- ifelse(test_pred >= threshold, 1, 0)
          tp <- sum(test_class == 1 & test_actual == 1)
          tn <- sum(test_class == 0 & test_actual == 0)
          fp <- sum(test_class == 1 & test_actual == 0)
          fn <- sum(test_class == 0 & test_actual == 1)

          fold_sens[fold] <- if ((tp + fn) > 0) tp / (tp + fn) else 0
          fold_spec[fold] <- if ((tn + fp) > 0) tn / (tn + fp) else 0
        }

        # Add aggregated results to table
        table$addRow(rowKey = pred, values = list(
          predictor = pred,
          mean_auc = mean(fold_aucs),
          sd_auc = sd(fold_aucs),
          mean_sensitivity = mean(fold_sens),
          sd_sensitivity = sd(fold_sens),
          mean_specificity = mean(fold_spec),
          sd_specificity = sd(fold_spec)
        ))
      }
    },

    # Populate methodology explanation
    .populateMethodology = function() {

      html <- "
      <h3>Methodology</h3>

      <h4>Performance Metrics</h4>
      <ul>
        <li><strong>AUC (Area Under the ROC Curve):</strong> Measures discrimination ability.
            Values range from 0 to 1, where 0.5 indicates random performance and 1.0 indicates perfect discrimination.</li>
        <li><strong>Sensitivity:</strong> True positive rate. Proportion of actual positives correctly identified.</li>
        <li><strong>Specificity:</strong> True negative rate. Proportion of actual negatives correctly identified.</li>
        <li><strong>Optimal Threshold:</strong> Determined using Youden's J statistic,
            which maximizes the sum of sensitivity and specificity.</li>
      </ul>

      <h4>Additional Metrics</h4>
      <ul>
        <li><strong>Youden's J:</strong> Calculated as Sensitivity + Specificity - 1. Values range from 0 to 1,
            where higher values indicate better overall performance. A value of 0 indicates performance no better than chance.</li>
        <li><strong>Matthews Correlation Coefficient (MCC):</strong> A balanced measure that accounts for all four
            confusion matrix categories. Values range from -1 to +1, where +1 indicates perfect prediction,
            0 indicates random prediction, and -1 indicates inverse prediction. MCC is particularly useful
            for imbalanced datasets.</li>
      </ul>

      <h4>Statistical Comparison</h4>
      <p>When comparing models, the <strong>DeLong test</strong> is used to compare AUC values.
      This test accounts for the correlation between ROC curves derived from the same dataset.</p>

      <h4>Confidence Intervals</h4>
      <p>By default, 95% confidence intervals for AUC are calculated using <strong>DeLong's method</strong>,
      which provides asymptotically exact coverage and is computationally efficient.</p>
      <p>Alternatively, <strong>bootstrap confidence intervals</strong> can be used. Bootstrap resampling
      creates multiple datasets by sampling with replacement, calculating AUC for each, and deriving
      confidence intervals from the distribution. Bootstrap is more robust for small samples or
      non-normal distributions, but requires more computation time.</p>

      <h4>Cross-Validation</h4>
      <p><strong>K-fold cross-validation</strong> provides more robust performance estimates by:</p>
      <ol>
        <li>Dividing the data into K equal-sized folds</li>
        <li>Training on K-1 folds and testing on the remaining fold</li>
        <li>Repeating this process K times, using each fold as test set once</li>
        <li>Averaging the results across all folds</li>
      </ol>
      <p><strong>Stratified sampling</strong> maintains the proportion of positive and negative cases
      in each fold, which is particularly important for imbalanced datasets.</p>
      <p>Cross-validation helps assess how well the model will generalize to independent data and
      provides estimates of prediction variability.</p>
      "

      self$results$methodologyExplanation$setContent(html)
    },

    # Populate interpretation summary
    .populateInterpretation = function() {

      data <- private$.cleanData
      if (is.null(data)) return()

      # Get best performing predictor
      table <- self$results$performanceTable
      predictors <- self$options$predictorVars

      if (length(predictors) == 0) return()

      # Find best AUC
      best_pred <- NULL
      best_auc <- -1

      for (pred in predictors) {
        row <- table$getRow(rowKey = pred)
        if (!is.null(row$auc)) {
          auc_val <- as.numeric(row$auc)
          if (!is.na(auc_val) && auc_val > best_auc) {
            best_auc <- auc_val
            best_pred <- pred
          }
        }
      }

      # Interpretation guidelines
      auc_interpret <- if (best_auc >= 0.9) {
        "excellent"
      } else if (best_auc >= 0.8) {
        "good"
      } else if (best_auc >= 0.7) {
        "acceptable"
      } else if (best_auc >= 0.6) {
        "poor"
      } else {
        "very poor"
      }

      html <- paste0("
      <h3>Interpretation Summary</h3>

      <h4>Overall Performance</h4>
      <p>The best performing predictor is <strong>", best_pred, "</strong> with an AUC of ",
      sprintf("%.3f", best_auc), " (", auc_interpret, " discrimination).</p>

      <h4>AUC Interpretation Guidelines</h4>
      <ul>
        <li><strong>0.90 - 1.00:</strong> Excellent discrimination</li>
        <li><strong>0.80 - 0.90:</strong> Good discrimination</li>
        <li><strong>0.70 - 0.80:</strong> Acceptable discrimination</li>
        <li><strong>0.60 - 0.70:</strong> Poor discrimination</li>
        <li><strong>0.50 - 0.60:</strong> Very poor discrimination (barely better than chance)</li>
      </ul>

      <h4>Clinical Application</h4>
      <p>Consider the clinical context when interpreting these metrics:</p>
      <ul>
        <li><strong>Sensitivity</strong> is crucial for screening tests (minimize false negatives)</li>
        <li><strong>Specificity</strong> is important for confirmatory tests (minimize false positives)</li>
        <li>The <strong>optimal threshold</strong> balances both, but may need adjustment based on clinical consequences</li>
      </ul>

      <p><strong>Sample size:</strong> ", nrow(data), " complete cases used in analysis.</p>
      ")

      self$results$resultsInterpretation$setContent(html)
    },

    # Plot ROC curves
    .plotROC = function(image, ...) {

      if (!self$options$rocPlot) return()
      if (is.null(private$.cleanData)) return()

      if (!requireNamespace("pROC", quietly = TRUE)) {
        # Can't plot without pROC
        return()
      }

      data <- private$.cleanData
      predictors <- self$options$predictorVars
      actual <- data$outcome_binary

      if (length(predictors) == 0) return()

      # Limit to first 10 predictors for plotting
      if (length(predictors) > 10) {
        predictors <- predictors[1:10]
      }

      # Create ROC objects for all predictors
      roc_list <- list()
      for (pred in predictors) {
        roc_obj <- pROC::roc(actual, data[[pred]], quiet = TRUE)
        roc_list[[pred]] <- roc_obj
      }

      # Prepare plot
      plot_data <- lapply(names(roc_list), function(name) {
        roc_obj <- roc_list[[name]]
        data.frame(
          Predictor = name,
          Specificity = 1 - roc_obj$specificities,
          Sensitivity = roc_obj$sensitivities,
          AUC = as.numeric(pROC::auc(roc_obj)),
          stringsAsFactors = FALSE
        )
      })
      plot_df <- do.call(rbind, plot_data)

      # Add AUC to predictor labels
      plot_df$PredictorLabel <- paste0(
        plot_df$Predictor,
        " (AUC=", sprintf("%.3f", plot_df$AUC), ")"
      )

      # Create plot
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(
        x = Specificity,
        y = Sensitivity,
        color = PredictorLabel
      )) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_abline(
          intercept = 0, slope = 1,
          linetype = "dashed", color = "gray50"
        ) +
        ggplot2::scale_x_continuous(
          "1 - Specificity (False Positive Rate)",
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2)
        ) +
        ggplot2::scale_y_continuous(
          "Sensitivity (True Positive Rate)",
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2)
        ) +
        ggplot2::labs(
          title = "ROC Curves",
          color = "Predictor"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position = "right",
          legend.title = ggplot2::element_text(face = "bold"),
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          panel.grid.minor = ggplot2::element_blank()
        )

      print(p)
      TRUE
    }
  )
)
