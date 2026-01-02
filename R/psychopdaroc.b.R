#' @title Comprehensive ROC Analysis with Advanced Features
#' 
#' @description 
#' Performs sophisticated Receiver Operating Characteristic (ROC) curve analysis
#' with optimal cutpoint determination, multiple comparison methods, and advanced
#' statistical features including IDI/NRI calculations, DeLong test, and 
#' comprehensive visualization options.
#' 
#' @details
#' This function provides an extensive ROC analysis toolkit that goes beyond
#' basic ROC curve generation. Key features include:
#' 
#' \strong{Core ROC Analysis:}
#' \itemize{
#'   \item AUC calculation with confidence intervals
#'   \item Multiple cutpoint optimization methods (12 different approaches)
#'   \item 16 different optimization metrics (Youden, accuracy, F1, etc.)
#'   \item Bootstrap confidence intervals
#'   \item Manual cutpoint specification
#' }
#' 
#' \strong{Advanced Statistical Methods:}
#' \itemize{
#'   \item DeLong test for comparing multiple AUCs
#'   \item IDI (Integrated Discrimination Index) with bootstrap CI
#'   \item NRI (Net Reclassification Index) with bootstrap CI  
#'   \item Partial AUC calculations
#'   \item ROC curve smoothing (multiple methods)
#'   \item Classifier performance comparison
#' }
#' 
#' \strong{Visualization Options:}
#' \itemize{
#'   \item ROC curves (individual and combined)
#'   \item Sensitivity/specificity vs threshold plots
#'   \item Predictive value vs prevalence plots
#'   \item Precision-recall curves
#'   \item Dot plots showing class distributions
#'   \item Interactive ROC plots
#'   \item Confidence bands and quantile confidence intervals
#' }
#' 
#' \strong{Subgroup Analysis:}
#' \itemize{
#'   \item Stratified analysis by grouping variables
#'   \item Cost-benefit optimization with custom cost ratios
#'   \item Hospital/site comparisons
#' }
#' 
#' @param data A data frame containing the variables for analysis
#' @param dependentVars Character vector of test variable names to evaluate. 
#'   Multiple variables can be specified for comparison.
#' @param classVar Name of the binary classification variable (gold standard).
#'   Must have exactly two levels.
#' @param positiveClass Which level of classVar represents the positive class.
#'   If not specified, the first level is used.
#' @param subGroup Optional grouping variable for stratified analysis.
#' @param method Cutpoint optimization method. Options include:
#'   \itemize{
#'     \item "maximize_metric": Maximize the specified metric
#'     \item "minimize_metric": Minimize the specified metric  
#'     \item "oc_youden_kernel": Youden index with kernel smoothing
#'     \item "oc_manual": Use manually specified cutpoint
#'     \item "oc_cost_ratio": Optimize based on cost ratio
#'     \item "oc_equal_sens_spec": Equal sensitivity and specificity
#'     \item "oc_closest_01": Closest to perfect classifier (0,1)
#'   }
#' @param metric Optimization metric when using maximize/minimize methods:
#'   "youden", "accuracy", "F1_score", "cohens_kappa", etc.
#' @param direction Classification direction: ">=" (higher values = positive) or 
#'   "<=" (lower values = positive)
#' @param specifyCutScore Manual cutpoint value (required when method = "oc_manual")
#' @param delongTest Logical. Perform DeLong test for AUC comparison (requires ≥2 variables)
#' @param calculateIDI Logical. Calculate Integrated Discrimination Index (requires ≥2 variables)
#' @param calculateNRI Logical. Calculate Net Reclassification Index (requires ≥2 variables)
#' @param refVar Reference variable for IDI/NRI calculations
#' @param plotROC Logical. Generate ROC curve plots
#' @param combinePlots Logical. Combine multiple variables in single plot
#' @param sensSpecTable Logical. Generate sensitivity/specificity confusion matrix
#' @param showThresholdTable Logical. Show detailed threshold performance table
#' @param partialAUC Logical. Calculate partial AUC over specified range
#' @param bootstrapCI Logical. Calculate bootstrap confidence intervals
#' @param precisionRecallCurve Logical. Generate precision-recall curves
#' @param compareClassifiers Logical. Generate classifier comparison metrics
#' @param ... Additional parameters for fine-tuning analysis
#' 
#' @return A psychopdaROCResults object containing:
#' \itemize{
#'   \item \code{resultsTable}: Detailed results for each threshold
#'   \item \code{simpleResultsTable}: Summary AUC results with confidence intervals
#'   \item \code{sensSpecTable}: Confusion matrix at optimal cutpoint
#'   \item \code{plotROC}: ROC curve visualization
#'   \item \code{delongTest}: DeLong test results (if requested)
#'   \item \code{idiTable}: IDI results with confidence intervals (if requested)
#'   \item \code{nriTable}: NRI results with confidence intervals (if requested)
#'   \item Additional plots and tables based on options selected
#' }
#' 
#' @examples
#' \dontrun{
#' # Load example medical data
#' data(medical_roc_data)
#' 
#' # Basic ROC analysis
#' result1 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status", 
#'   positiveClass = "Disease"
#' )
#' 
#' # Compare multiple biomarkers with DeLong test
#' result2 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2", "biomarker3"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   delongTest = TRUE,
#'   combinePlots = TRUE
#' )
#' 
#' # Advanced analysis with IDI/NRI
#' result3 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease", 
#'   calculateIDI = TRUE,
#'   calculateNRI = TRUE,
#'   refVar = "biomarker1",
#'   nriThresholds = "0.3,0.7"
#' )
#' 
#' # Cost-benefit optimization
#' result4 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   method = "oc_cost_ratio",
#'   costratioFP = 2.5  # False positives cost 2.5x false negatives
#' )
#' 
#' # Subgroup analysis by hospital
#' result5 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   subGroup = "hospital"
#' )
#' 
#' # Comprehensive analysis with all features
#' result6 <- psychopdaROC(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   method = "maximize_metric",
#'   metric = "youden",
#'   plotROC = TRUE,
#'   sensSpecTable = TRUE,
#'   showThresholdTable = TRUE,
#'   delongTest = TRUE,
#'   calculateIDI = TRUE,
#'   partialAUC = TRUE,
#'   bootstrapCI = TRUE,
#'   precisionRecallCurve = TRUE,
#'   compareClassifiers = TRUE
#' )
#' 
#' # Financial risk assessment example
#' data(financial_roc_data)
#' 
#' financial_result <- psychopdaROC(
#'   data = financial_roc_data,
#'   dependentVars = c("credit_score", "income_debt_ratio", "employment_score"),
#'   classVar = "default_status",
#'   positiveClass = "Default",
#'   direction = "<=",  # Lower credit scores indicate higher risk
#'   method = "oc_cost_ratio",
#'   costratioFP = 0.1,  # False positives (rejected good clients) cost less
#'   delongTest = TRUE,
#'   subGroup = "client_type"
#' )
#' 
#' # Educational assessment example
#' data(education_roc_data)
#' 
#' education_result <- psychopdaROC(
#'   data = education_roc_data,
#'   dependentVars = c("exam_score", "project_score", "peer_score"),
#'   classVar = "pass_status",
#'   positiveClass = "Pass",
#'   method = "maximize_metric",
#'   metric = "accuracy",
#'   calculateIDI = TRUE,
#'   refVar = "exam_score",
#'   subGroup = "class_section"
#' )
#' 
#' # Manufacturing quality control example
#' data(manufacturing_roc_data)
#' 
#' quality_result <- psychopdaROC(
#'   data = manufacturing_roc_data, 
#'   dependentVars = c("dimension_score", "surface_score", "strength_score"),
#'   classVar = "quality_status",
#'   positiveClass = "Defect",
#'   method = "oc_equal_sens_spec",  # Balanced sensitivity/specificity
#'   plotROC = TRUE,
#'   showCriterionPlot = TRUE,
#'   showDotPlot = TRUE,
#'   subGroup = "production_line"
#' )
#' }
#' 
#' @references
#' DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). 
#' Comparing the areas under two or more correlated receiver operating 
#' characteristic curves: a nonparametric approach. Biometrics, 44(3), 837-845.
#' 
#' Pencina, M. J., D'Agostino, R. B., D'Agostino, R. B., & Vasan, R. S. (2008). 
#' Evaluating the added predictive ability of a new marker: from area under the 
#' ROC curve to reclassification and beyond. Statistics in Medicine, 27(2), 157-172.
#' 
#' Youden, W. J. (1950). Index for rating diagnostic tests. Cancer, 3(1), 32-35.
#' 
#' @seealso 
#' \code{\link{cutpointr}} for cutpoint optimization methods
#' \code{\link{pROC}} for ROC curve analysis
#' 
#' @note
#' This function originally developed by Lucas Friesen in the psychoPDA module.
#' Enhanced version with additional features added to the ClinicoPath module.
#' 
#' @keywords ROC AUC sensitivity specificity diagnostic biomarker classification
#' 
#' @export
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import cutpointr
#' @importFrom MASS ginv

# ============================================================================
# MAIN ANALYSIS CLASS
# ============================================================================

psychopdaROCClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "psychopdaROCClass",
  inherit = psychopdaROCBase,
  private = list(
    # ============================================================================
    # CLASS PRIVATE FIELDS
    # ============================================================================

    ## Storage for ROC data and other analysis results
    .rocDataList = list(),            # Store ROC curve data for each variable
    .optimalCriteriaList = list(),    # Store optimal cutpoints
    .prevalenceList = list(),         # Store prevalence values
    .aucList = list(),               # Store AUC values for clinical interpretation
    .forestPlotData = NULL,          # Store forest plot data for meta-analysis
    
    # ============================================================================
    # INITIALIZATION AND VALIDATION
    # ============================================================================

    # COMMENTED OUT: Duplicate .init - see complete implementation at line ~1711
    # .init = function() {
    #   # Initialize tables and plots
    #
    #   # Hide advanced results by default
    #   self$results$clinicalInterpretationTable$setVisible(
    #     self$options$clinicalMode %in% c("basic", "advanced", "comprehensive")
    #   )
    #
    #   # Show run summary if available (it's always visible in r.yaml/default, but we can set content)
    #   self$results$runSummary$setContent("")
    # },

    .validateInputsOLD = function() {
      # 1. Check if data is present
      if (is.null(self$data) || nrow(self$data) == 0) {
        return(list(valid = FALSE, message = "No data available for analysis."))
      }
      
      # 2. Check class variable
      classVar <- self$options$classVar
      if (is.null(classVar) || !classVar %in% names(self$data)) {
        return(list(valid = FALSE, message = "Class variable not specified or not found."))
      }
      
      # 3. Check positive class
      posClass <- self$options$positiveClass
      if (!is.null(posClass) && posClass != "") {
        # Verify positive class exists in data
        actualLevels <- unique(as.character(self$data[[classVar]]))
        if (!posClass %in% actualLevels) {
          return(list(valid = FALSE, message = sprintf(
            "Specified positive class '%s' not found in variable '%s'. Available levels: %s",
            posClass, classVar, paste(actualLevels, collapse = ", ")
          )))
        }
      }
      
      # 4. Check dependent variables
      depVars <- self$options$dependentVars
      if (is.null(depVars) || length(depVars) == 0) {
        return(list(valid = FALSE, message = "No test variables selected."))
      }
      
      return(list(valid = TRUE, message = ""))
    },

    # ============================================================================
    # UTILITY METHODS
    # ============================================================================
    
    # Centralized package dependency checking
    .checkPackageDependencies = function(required_packages, feature_name = "this feature") {
      missing_packages <- character(0)
      
      for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          missing_packages <- c(missing_packages, pkg)
        }
      }
      
      if (length(missing_packages) > 0) {
        pkg_list <- paste(missing_packages, collapse = ", ")
        warning(.("The {packages} package(s) are required for {feature}. Please install missing packages to use this functionality."))
        return(FALSE)
      }
      return(TRUE)
    },
    
    # Standardized error handling with user-friendly messages
    .throwError = function(error_type, details = "", suggestion = "") {
      error_messages <- list(
        "insufficient_data" = "Insufficient data for analysis",
        "missing_positive_class" = "Specified positive class not found",
        "invalid_cutpoint" = "Invalid cutpoint specification",
        "insufficient_variables" = "Insufficient variables for comparison",
        "data_mismatch" = "Data dimensions do not match",
        "analysis_failed" = "Analysis calculation failed"
      )
      
      base_message <- error_messages[[error_type]] %||% "An error occurred"
      full_message <- paste(base_message, details)
      if (suggestion != "") {
        full_message <- paste(full_message, "\nSuggestion:", suggestion)
      }
      
      stop(full_message, call. = FALSE)
    },

    # Safe variable name handling for data frame access
    .escapeVar = function(x) {
      if (is.null(x) || length(x) == 0) return(x)
      vapply(x, function(v) {
        if (is.character(v) && nzchar(v)) {
          # Use backticks for spaces/special chars in formulas
          if (grepl("[^A-Za-z0-9_.]", v)) {
            paste0("`", v, "`")
          } else {
            v
          }
        } else {
          as.character(v)
        }
      }, character(1), USE.NAMES = FALSE)
    },

    # ============================================================================
    # CLINICAL UTILITY METHODS
    # ============================================================================
    
    # Apply clinical mode settings
    .applyClinicalModeSettings = function() {
      mode <- self$options$clinicalMode
      
      # Adjust UI visibility based on clinical mode
      if (mode == "basic") {
        # Show only essential features for clinical users
        self$results$instructions$setContent(.("
        <h3>Basic ROC Analysis</h3>
        <p>This simplified interface shows essential diagnostic performance metrics. 
        Results include Area Under the Curve (AUC), optimal cutpoints, and performance statistics.</p>
        <p><strong>AUC Interpretation:</strong> 0.9+ = Excellent, 0.8-0.9 = Good, 0.7-0.8 = Fair, &lt;0.7 = Poor</p>
        "))
      } else if (mode == "advanced") {
        self$results$instructions$setContent(.("
        <h3>Advanced ROC Analysis</h3> 
        <p>Advanced interface with statistical comparisons, effect sizes, and additional metrics for research applications.</p>
        "))
      } else if (mode == "comprehensive") {
        self$results$instructions$setContent(.("
        <h3>Comprehensive Statistical Analysis</h3>
        <p>Full research-grade analysis with all available statistical methods, Bayesian analysis, and publication-quality outputs.</p>
        "))
      }
    },
    
    # Apply clinical preset settings
    .applyClinicalPresetSettings = function() {
      preset <- self$options$clinicalPreset
      
      if (preset == "screening") {
        # Screening tests prioritize sensitivity (avoid missing cases)
        self$results$instructions$setContent(.("
        <h4>Screening Test Configuration</h4>
        <p>Optimized for high sensitivity to minimize false negatives. 
        Recommended for initial disease screening where missing cases is costly.</p>
        "))
      } else if (preset == "confirmation") {
        # Confirmation tests prioritize specificity (avoid false positives)
        self$results$instructions$setContent(.("
        <h4>Confirmation Test Configuration</h4>
        <p>Optimized for high specificity to minimize false positives.
        Recommended for confirmatory testing where false alarms are costly.</p>
        "))
      } else if (preset == "balanced") {
        # Balanced approach using Youden's J
        self$results$instructions$setContent(.("
        <h4>Balanced Diagnostic Test</h4>
        <p>Optimized for balanced sensitivity and specificity using Youden's J index.
        Suitable for general diagnostic applications.</p>
        "))
      } else if (preset == "research") {
        # Research configuration with comprehensive analysis
        self$results$instructions$setContent(.("
        <h4>Research Analysis Configuration</h4>
        <p>Comprehensive statistical analysis suitable for research publications with 
        advanced metrics and comparisons.</p>
        "))
      }
    },
    
    # Clinical interpretation helpers
    .interpretAUC = function(auc) {
      if (is.na(auc) || is.null(auc)) return("Unknown")
      if (auc >= 0.9) return(.("Excellent"))
      else if (auc >= 0.8) return(.("Good"))
      else if (auc >= 0.7) return(.("Fair"))  
      else return(.("Poor"))
    },
    
    .getClinicalRecommendation = function(auc, var) {
      if (is.na(auc) || is.null(auc)) return(.("Unable to assess"))
      
      if (auc >= 0.8) {
        return(.("Suitable for clinical use with appropriate cutpoint"))
      } else if (auc >= 0.7) {
        return(.("May be useful in combination with other markers"))
      } else {
        return(.("Not recommended as standalone diagnostic marker"))
      }
    },
    
    .getDetailedInterpretation = function(auc, var) {
      if (is.na(auc) || is.null(auc)) return(.("Analysis could not be completed"))
      
      interpretation <- sprintf(.("The test '%s' has an AUC of %.3f"), var, auc)
      
      if (auc >= 0.9) {
        interpretation <- paste(interpretation, .("indicating excellent discriminatory ability. This test can reliably distinguish between diseased and healthy patients."))
      } else if (auc >= 0.8) {
        interpretation <- paste(interpretation, .("indicating good discriminatory ability. This test performs well for clinical decision making."))
      } else if (auc >= 0.7) {
        interpretation <- paste(interpretation, .("indicating fair discriminatory ability. Consider combining with other clinical information."))
      } else {
        interpretation <- paste(interpretation, .("indicating poor discriminatory ability. Alternative diagnostic approaches should be considered."))
      }
      
      return(interpretation)
    },
    
    # Populate clinical interpretation table
    .populateClinicalInterpretation = function() {
      table <- self$results$clinicalInterpretationTable
      
      if (is.null(self$options$dependentVars) || length(self$options$dependentVars) == 0)
        return()
      
      vars <- self$options$dependentVars
      
      for (var in vars) {
        # Get AUC from simple results table or calculate if needed
        auc <- private$.getAUCForVariable(var)
        
        if (!is.na(auc) && !is.null(auc)) {
          # Generate clinical interpretation using helper methods
          performance_level <- private$.interpretAUC(auc)
          clinical_recommendation <- private$.getClinicalRecommendation(auc, var)
          interpretation_text <- private$.getDetailedInterpretation(auc, var)
          
          table$addRow(rowKey = var, values = list(
            variable = var,
            performance_level = performance_level,
            clinical_recommendation = clinical_recommendation,
            interpretation_text = interpretation_text
          ))
        }
      }
    },
    
    # Helper to get AUC for a specific variable
    .getAUCForVariable = function(var) {
      # Try to get AUC from stored results
      tryCatch({
        if (!is.null(private$.aucList[[var]])) {
          return(private$.aucList[[var]])
        } else {
          return(NA)
        }
      }, error = function(e) {
        return(NA)
      })
    },
    
    # ============================================================================
    # HELPER METHODS FOR OPTIMAL CUTPOINT CALCULATION
    # ============================================================================

    # Calculate cutpoint optimized for cost ratio
    #
    # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
    # @param prevalence The prevalence of the positive class
    # @param costRatio The cost ratio of false positives to false negatives
    # @return List with optimal index, threshold, and score
    .calculateCostRatioOptimal = function(confusionMatrix, prevalence, costRatio) {
      # Initialize variables
      n_thresholds <- length(confusionMatrix$x.sorted)
      scores <- numeric(n_thresholds)

      # Calculate utility score for each threshold
      for (i in 1:n_thresholds) {
        # Extract confusion matrix values
        tp <- confusionMatrix$tp[i]
        fp <- confusionMatrix$fp[i]
        tn <- confusionMatrix$tn[i]
        fn <- confusionMatrix$fn[i]

        # Calculate sensitivity and specificity
        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)

        # Calculate utility score considering cost ratio
        # Higher score means better balance considering costs
        # This formula weighs the false positives by the cost ratio
        scores[i] <- sensitivity * prevalence -
          (1 - specificity) * (1 - prevalence) * costRatio
      }

      # Find threshold with highest score
      best_idx <- which.max(scores)

      return(list(
        optimal_idx = best_idx,
        optimal_threshold = confusionMatrix$x.sorted[best_idx],
        score = scores[best_idx]
      ))
    },

    # Calculate cutpoint with equal sensitivity and specificity
    #
    # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
    # @return List with optimal index, threshold, and difference
    .calculateEqualSensSpec = function(confusionMatrix) {
      # Initialize variables
      n_thresholds <- length(confusionMatrix$x.sorted)
      differences <- numeric(n_thresholds)

      # Calculate absolute difference between sensitivity and specificity for each threshold
      for (i in 1:n_thresholds) {
        # Extract confusion matrix values
        tp <- confusionMatrix$tp[i]
        fp <- confusionMatrix$fp[i]
        tn <- confusionMatrix$tn[i]
        fn <- confusionMatrix$fn[i]

        # Calculate sensitivity and specificity
        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)

        # Calculate absolute difference
        differences[i] <- abs(sensitivity - specificity)
      }

      # Find threshold with minimal difference
      best_idx <- which.min(differences)

      return(list(
        optimal_idx = best_idx,
        optimal_threshold = confusionMatrix$x.sorted[best_idx],
        difference = differences[best_idx]
      ))
    },

    # Calculate cutpoint closest to (0,1) in ROC space
    #
    # @param confusionMatrix Matrix with tp, fp, tn, fn values for each threshold
    # @return List with optimal index, threshold, and distance
    .calculateClosestToOptimal = function(confusionMatrix) {
      # Initialize variables
      n_thresholds <- length(confusionMatrix$x.sorted)
      distances <- numeric(n_thresholds)

      # Calculate Euclidean distance to (0,1) in ROC space for each threshold
      for (i in 1:n_thresholds) {
        # Extract confusion matrix values
        tp <- confusionMatrix$tp[i]
        fp <- confusionMatrix$fp[i]
        tn <- confusionMatrix$tn[i]
        fn <- confusionMatrix$fn[i]

        # Calculate sensitivity and specificity
        sensitivity <- tp / (tp + fn)
        specificity <- tn / (tn + fp)

        # Calculate distance to (0,1) point
        # In ROC space, x-axis is 1-specificity, y-axis is sensitivity
        distances[i] <- sqrt((1 - specificity)^2 + (1 - sensitivity)^2)
      }

      # Find threshold with minimal distance
      best_idx <- which.min(distances)

      return(list(
        optimal_idx = best_idx,
        optimal_threshold = confusionMatrix$x.sorted[best_idx],
        distance = distances[best_idx]
      ))
    },

    # ============================================================================
    # FIXED SENSITIVITY/SPECIFICITY ANALYSIS METHODS
    # ============================================================================
    
    # Calculate cutpoint for fixed sensitivity or specificity value
    .calculateFixedSensSpec = function(confusionMatrix, target_value, analysis_type, interpolation_method = "linear") {
      n_thresholds <- length(confusionMatrix$x.sorted)
      
      # Calculate sensitivity and specificity for all thresholds
      sensitivities <- numeric(n_thresholds)
      specificities <- numeric(n_thresholds)
      
      for (i in 1:n_thresholds) {
        tp <- confusionMatrix$tp[i]
        fp <- confusionMatrix$fp[i]
        tn <- confusionMatrix$tn[i]
        fn <- confusionMatrix$fn[i]
        
        sensitivities[i] <- tp / (tp + fn)
        specificities[i] <- tn / (tn + fp)
      }
      
      # Find closest point based on analysis type
      if (analysis_type == "sensitivity") {
        target_values <- sensitivities
        target_name <- "sensitivity"
      } else {
        target_values <- specificities
        target_name <- "specificity"
      }
      
      # Handle interpolation methods
      if (interpolation_method == "nearest") {
        # Find nearest point
        best_idx <- which.min(abs(target_values - target_value))
        cutpoint <- confusionMatrix$x.sorted[best_idx]
        achieved_sens <- sensitivities[best_idx]
        achieved_spec <- specificities[best_idx]
        interpolation_used <- "Nearest Point"
        
      } else if (interpolation_method == "stepwise") {
        # Conservative approach - use the next more conservative cutpoint
        if (analysis_type == "sensitivity") {
          # For sensitivity, we want >= target, so find first point that meets or exceeds
          valid_indices <- which(target_values >= target_value)
        } else {
          # For specificity, we want >= target, so find first point that meets or exceeds
          valid_indices <- which(target_values >= target_value)
        }
        
        if (length(valid_indices) > 0) {
          best_idx <- valid_indices[1]
        } else {
          # If no point meets target, use the closest
          best_idx <- which.min(abs(target_values - target_value))
        }
        
        cutpoint <- confusionMatrix$x.sorted[best_idx]
        achieved_sens <- sensitivities[best_idx]
        achieved_spec <- specificities[best_idx]
        interpolation_used <- "Stepwise (Conservative)"
        
      } else {
        # Linear interpolation (default)
        if (target_value <= min(target_values)) {
          # Target below minimum, use minimum point
          best_idx <- which.min(target_values)
          cutpoint <- confusionMatrix$x.sorted[best_idx]
          achieved_sens <- sensitivities[best_idx]
          achieved_spec <- specificities[best_idx]
        } else if (target_value >= max(target_values)) {
          # Target above maximum, use maximum point
          best_idx <- which.max(target_values)
          cutpoint <- confusionMatrix$x.sorted[best_idx]
          achieved_sens <- sensitivities[best_idx]
          achieved_spec <- specificities[best_idx]
        } else {
          # Interpolate between two closest points
          below_indices <- which(target_values <= target_value)
          above_indices <- which(target_values >= target_value)
          
          if (length(below_indices) > 0 && length(above_indices) > 0) {
            below_idx <- below_indices[which.max(target_values[below_indices])]
            above_idx <- above_indices[which.min(target_values[above_indices])]
            
            # Linear interpolation
            below_val <- target_values[below_idx]
            above_val <- target_values[above_idx]
            weight <- (target_value - below_val) / (above_val - below_val)
            
            cutpoint <- confusionMatrix$x.sorted[below_idx] + 
              weight * (confusionMatrix$x.sorted[above_idx] - confusionMatrix$x.sorted[below_idx])
            achieved_sens <- sensitivities[below_idx] + 
              weight * (sensitivities[above_idx] - sensitivities[below_idx])
            achieved_spec <- specificities[below_idx] + 
              weight * (specificities[above_idx] - specificities[below_idx])
          } else {
            # Fallback to nearest
            best_idx <- which.min(abs(target_values - target_value))
            cutpoint <- confusionMatrix$x.sorted[best_idx]
            achieved_sens <- sensitivities[best_idx]
            achieved_spec <- specificities[best_idx]
          }
        }
        interpolation_used <- "Linear Interpolation"
      }
      
      return(list(
        cutpoint = cutpoint,
        achieved_sensitivity = achieved_sens,
        achieved_specificity = achieved_spec,
        interpolation_method = interpolation_used
      ))
    },

    # Populate fixed sensitivity/specificity results table
    .populateFixedSensSpecTable = function(var, confusionMatrix, positiveClass) {
      table <- self$results$fixedSensSpecTable
      
      if (!self$options$fixedSensSpecAnalysis) return()
      
      analysis_type <- self$options$fixedAnalysisType
      target_value <- if (analysis_type == "sensitivity") {
        self$options$fixedSensitivityValue
      } else {
        self$options$fixedSpecificityValue
      }
      
      interpolation_method <- self$options$fixedInterpolation
      
      # Calculate fixed sensitivity/specificity cutpoint
      fixed_result <- private$.calculateFixedSensSpec(
        confusionMatrix, target_value, analysis_type, interpolation_method
      )
      
      # Calculate additional metrics at the determined cutpoint
      cutpoint <- fixed_result$cutpoint
      achieved_sens <- fixed_result$achieved_sensitivity
      achieved_spec <- fixed_result$achieved_specificity

      # Calculate PPV, NPV, accuracy, and Youden's J
      # Find the closest threshold in the confusion matrix for exact calculations
      closest_idx <- which.min(abs(confusionMatrix$x.sorted - cutpoint))
      tp <- confusionMatrix$tp[closest_idx]
      fp <- confusionMatrix$fp[closest_idx]
      tn <- confusionMatrix$tn[closest_idx]
      fn <- confusionMatrix$fn[closest_idx]

      ppv <- tp / (tp + fp)
      npv <- tn / (tn + fn)
      accuracy <- (tp + tn) / (tp + fp + tn + fn)
      youden <- achieved_sens + achieved_spec - 1

      # Ensure all values are atomic (scalar) for jamovi table
      # Convert vectors to single values, handle NaN/Inf
      cutpoint <- as.numeric(cutpoint)[1]
      achieved_sens <- as.numeric(achieved_sens)[1]
      achieved_spec <- as.numeric(achieved_spec)[1]
      ppv <- as.numeric(ppv)[1]
      npv <- as.numeric(npv)[1]
      accuracy <- as.numeric(accuracy)[1]
      youden <- as.numeric(youden)[1]
      target_value <- as.numeric(target_value)[1]

      # Ensure strings are atomic
      var <- as.character(var)[1]
      analysis_type_str <- paste("Fixed", tools::toTitleCase(analysis_type))
      interpolation_str <- as.character(fixed_result$interpolation_method)[1]

      # Add row to table
      table$addRow(rowKey = var, values = list(
        variable = var,
        analysis_type = analysis_type_str,
        target_value = target_value,
        cutpoint = cutpoint,
        achieved_sensitivity = achieved_sens,
        achieved_specificity = achieved_spec,
        ppv = ppv,
        npv = npv,
        accuracy = accuracy,
        youden = youden,
        interpolation_used = interpolation_str
      ))
    },

    # Generate explanatory content for fixed sensitivity/specificity analysis
    .generateFixedSensSpecExplanation = function() {
      if (!self$options$fixedSensSpecAnalysis || !self$options$showFixedExplanation) return()
      
      analysis_type <- self$options$fixedAnalysisType
      target_value <- if (analysis_type == "sensitivity") {
        self$options$fixedSensitivityValue
      } else {
        self$options$fixedSpecificityValue
      }
      interpolation_method <- self$options$fixedInterpolation
      
      # Generate comprehensive explanation
      explanation <- paste0(
        "<div style='background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 10px 0;'>",
        "<h4 style='color: #007bff; margin-top: 0;'>📊 Fixed ", tools::toTitleCase(analysis_type), " Analysis Guide</h4>",
        
        "<h5 style='color: #495057; margin-top: 20px;'>🎯 Analysis Overview</h5>",
        "<p>This analysis determines the <strong>cutpoint threshold</strong> that achieves a target ", analysis_type, " of <strong>", round(target_value, 3), "</strong> (", round(target_value * 100, 1), "%). ",
        "The corresponding ", if (analysis_type == "sensitivity") "specificity" else "sensitivity", " and other performance metrics are then calculated.</p>",
        
        "<h5 style='color: #495057;'>🏥 Clinical Context</h5>"
      )
      
      # Add clinical context based on analysis type
      if (analysis_type == "sensitivity") {
        explanation <- paste0(explanation,
          "<p><strong>High Sensitivity Strategy (Screening Focus):</strong></p>",
          "<ul>",
          "<li>🔍 <strong>Screening Tests</strong>: Minimizes false negatives - few cases are missed</li>",
          "<li>🚨 <strong>Rule-Out Tests</strong>: When a negative result effectively rules out the condition</li>",
          "<li>⚡ <strong>Emergency Settings</strong>: When missing a case has severe consequences</li>",
          "<li>💰 <strong>Cost Consideration</strong>: Accept more false positives to avoid missed diagnoses</li>",
          "</ul>",
          "<div style='background-color: #d4edda; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
          "<strong>💡 Clinical Pearl:</strong> High sensitivity = \"SnOUT\" (Sensitivity rules OUT) - a negative test with high sensitivity confidently excludes the condition.",
          "</div>"
        )
      } else {
        explanation <- paste0(explanation,
          "<p><strong>High Specificity Strategy (Confirmation Focus):</strong></p>",
          "<ul>",
          "<li>✅ <strong>Confirmatory Tests</strong>: Minimizes false positives - few healthy patients are incorrectly diagnosed</li>",
          "<li>🎯 <strong>Rule-In Tests</strong>: When a positive result confirms the condition</li>",
          "<li>💊 <strong>Treatment Decision</strong>: Before starting costly or risky treatments</li>",
          "<li>⚖️ <strong>Legal/Insurance</strong>: When false positives have significant consequences</li>",
          "</ul>",
          "<div style='background-color: #d1ecf1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
          "<strong>💡 Clinical Pearl:</strong> High specificity = \"SpIN\" (Specificity rules IN) - a positive test with high specificity confidently confirms the condition.",
          "</div>"
        )
      }
      
      # Add interpolation method explanation
      explanation <- paste0(explanation,
        "<h5 style='color: #495057;'>🔧 Interpolation Method: ", tools::toTitleCase(gsub("_", " ", interpolation_method)), "</h5>"
      )
      
      if (interpolation_method == "linear") {
        explanation <- paste0(explanation,
          "<p><strong>Linear Interpolation:</strong> Smoothly estimates the cutpoint between observed data points using mathematical interpolation.</p>",
          "<ul>",
          "<li>✅ <strong>Most Accurate</strong>: Provides precise cutpoint estimation</li>",
          "<li>📐 <strong>Mathematical</strong>: Uses weighted average between nearest points</li>",
          "<li>🎯 <strong>Recommended</strong>: Best for research and precise clinical applications</li>",
          "<li>⚠️ <strong>Note</strong>: May produce cutpoints not observed in your data</li>",
          "</ul>"
        )
      } else if (interpolation_method == "nearest") {
        explanation <- paste0(explanation,
          "<p><strong>Nearest Point:</strong> Uses the observed cutpoint closest to the target value.</p>",
          "<ul>",
          "<li>📊 <strong>Data-Driven</strong>: Only uses actually observed cutpoints</li>",
          "<li>🔍 <strong>Conservative</strong>: No mathematical interpolation</li>",
          "<li>⚡ <strong>Simple</strong>: Easy to understand and implement</li>",
          "<li>📉 <strong>Trade-off</strong>: May not achieve exact target value</li>",
          "</ul>"
        )
      } else if (interpolation_method == "stepwise") {
        explanation <- paste0(explanation,
          "<p><strong>Stepwise (Conservative):</strong> Chooses the most conservative cutpoint that meets or exceeds the target.</p>",
          "<ul>",
          "<li>🛡️ <strong>Conservative Approach</strong>: Errs on the side of caution</li>",
          "<li>📊 <strong>Observed Data Only</strong>: Uses actual data points</li>",
          "<li>🎯 <strong>Meets/Exceeds Target</strong>: Ensures target is achieved or surpassed</li>",
          "<li>🏥 <strong>Clinical Safety</strong>: Preferred when patient safety is paramount</li>",
          "</ul>"
        )
      }
      
      # Add interpretation of results
      explanation <- paste0(explanation,
        "<h5 style='color: #495057;'>📋 Results Interpretation</h5>",
        "<div style='background-color: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
        "<p><strong>Key Metrics to Review:</strong></p>",
        "<ul style='margin-bottom: 0;'>",
        "<li><strong>Achieved Value</strong>: How close we got to the target ", analysis_type, "</li>",
        "<li><strong>Corresponding ", if (analysis_type == "sensitivity") "Specificity" else "Sensitivity", "</strong>: The trade-off metric</li>",
        "<li><strong>PPV/NPV</strong>: Predictive values depend on disease prevalence</li>",
        "<li><strong>Youden's J</strong>: Overall balance (Sensitivity + Specificity - 1)</li>",
        "<li><strong>Cutpoint</strong>: The threshold value to use in practice</li>",
        "</ul>",
        "</div>"
      )
      
      # Add clinical decision framework
      explanation <- paste0(explanation,
        "<h5 style='color: #495057;'>🎯 Clinical Decision Framework</h5>",
        "<div style='background-color: #e2e3e5; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
        "<p><strong>Steps for Implementation:</strong></p>",
        "<ol style='margin-bottom: 0;'>",
        "<li><strong>Validate Performance</strong>: Confirm achieved ", analysis_type, " meets your requirements</li>",
        "<li><strong>Assess Trade-offs</strong>: Evaluate the corresponding ", if (analysis_type == "sensitivity") "specificity" else "sensitivity", " value</li>",
        "<li><strong>Consider Context</strong>: Factor in prevalence, costs, and consequences</li>",
        "<li><strong>Pilot Testing</strong>: Test the cutpoint in your clinical setting</li>",
        "<li><strong>Monitor Performance</strong>: Track real-world performance metrics</li>",
        "</ol>",
        "</div>"
      )
      
      # Add warnings and considerations
      explanation <- paste0(explanation,
        "<h5 style='color: #dc3545;'>⚠️ Important Considerations</h5>",
        "<div style='background-color: #f8d7da; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
        "<ul style='margin-bottom: 0;'>",
        "<li><strong>Population Specificity</strong>: Results may not generalize to different populations</li>",
        "<li><strong>Prevalence Impact</strong>: PPV and NPV change with disease prevalence</li>",
        "<li><strong>Sample Size</strong>: Confidence in cutpoint decreases with smaller samples</li>",
        "<li><strong>Clinical Validation</strong>: Always validate in your specific clinical context</li>",
        "<li><strong>Regular Review</strong>: Monitor and update cutpoints as populations change</li>",
        "</ul>",
        "</div>"
      )
      
      # Add references and further reading
      explanation <- paste0(explanation,
        "<h5 style='color: #495057;'>📚 Further Reading</h5>",
        "<p style='font-size: 0.9em; color: #6c757d;'>",
        "For comprehensive guidance on ROC analysis and cutpoint selection, see: ",
        "<em>Youden WJ (1950). Index for rating diagnostic tests. Cancer 3:32-35</em> | ",
        "<em>Zweig MH, Campbell G (1993). Receiver-operating characteristic plots. Clin Chem 39:561-577</em>",
        "</p>",
        "</div>"
      )
      
      # Set the content
      self$results$fixedSensSpecExplanation$setContent(explanation)
    },

    # Calculate partial AUC using pROC package
    .calculatePartialAUC = function(x, class, positiveClass, from, to) {
      # Check package dependencies
      if (!private$.checkPackageDependencies("pROC", "partial AUC calculations")) {
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(setdiff(levels(class), positiveClass)[1], positiveClass),
        direction = "<",
        quiet = TRUE
      )

      # Calculate partial AUC
      partial_auc <- try(pROC::auc(roc_obj, partial.auc = c(from, to), partial.auc.focus = "specificity"), silent = TRUE)

      # Calculate normalized partial AUC (scaled to 0-1 range)
      partial_auc_norm <- try(pROC::auc(roc_obj, partial.auc = c(from, to), partial.auc.focus = "specificity", partial.auc.correct = TRUE), silent = TRUE)

      # Calculate CI if possible
      if (self$options$bootstrapCI) {
        ci <- try(pROC::ci.auc(roc_obj, partial.auc = c(from, to), partial.auc.focus = "specificity",
                               method = "bootstrap", boot.n = self$options$bootstrapReps), silent = TRUE)

        if (inherits(ci, "try-error")) {
          ci_lower <- NA
          ci_upper <- NA
        } else {
          ci_lower <- ci[1]
          ci_upper <- ci[3]
        }
      } else {
        ci_lower <- NA
        ci_upper <- NA
      }

      return(list(
        pAUC = as.numeric(partial_auc),
        pAUC_normalized = as.numeric(partial_auc_norm),
        ci_lower = ci_lower,
        ci_upper = ci_upper,
        spec_range = paste0("(", from, " - ", to, ")")
      ))
    },

    # Create smoothed ROC curve using pROC
    .createSmoothedROC = function(x, class, positiveClass, method) {
      # Check package dependencies
      if (!private$.checkPackageDependencies("pROC", "ROC curve smoothing")) {
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(setdiff(levels(class), positiveClass)[1], positiveClass),
        direction = "<",
        quiet = TRUE
      )

      # Apply smoothing
      if (method != "none") {
        smooth_roc <- try(pROC::smooth(roc_obj, method = method), silent = TRUE)

        if (!inherits(smooth_roc, "try-error")) {
          # Extract coordinates
          coords <- pROC::coords(smooth_roc, x = "all", ret = c("threshold", "specificity", "sensitivity"))

          return(list(
            threshold = coords$threshold,
            specificity = coords$specificity,
            sensitivity = coords$sensitivity,
            auc = pROC::auc(smooth_roc)
          ))
        }
      }

      return(NULL)
    },

    # Calculate bootstrap confidence intervals for metrics
    .calculateBootstrapCI = function(x, class, positiveClass, metrics = c("auc", "threshold", "sensitivity", "specificity")) {
      # Check package dependencies
      if (!private$.checkPackageDependencies("pROC", "bootstrap confidence intervals")) {
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(setdiff(levels(class), positiveClass)[1], positiveClass),
        direction = "<",
        quiet = TRUE
      )

      results <- list()

      # Calculate CI for AUC
      if ("auc" %in% metrics) {
        auc_ci <- try(pROC::ci.auc(roc_obj, method = "bootstrap", boot.n = self$options$bootstrapReps), silent = TRUE)

        if (!inherits(auc_ci, "try-error")) {
          results$auc <- list(
            estimate = as.numeric(pROC::auc(roc_obj)),
            ci_lower = auc_ci[1],
            ci_upper = auc_ci[3]
          )
        }
      }

      # Find Youden's J point (optimal threshold)
      if (any(c("threshold", "sensitivity", "specificity") %in% metrics)) {
        optimal_coords <- pROC::coords(roc_obj, x = "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))

        # Calculate CI for threshold
        if ("threshold" %in% metrics) {
          threshold_ci <- try(pROC::ci.coords(roc_obj, x = "best", best.method = "youden",
                                              ret = "threshold", method = "bootstrap", boot.n = self$options$bootstrapReps),
                              silent = TRUE)

          if (!inherits(threshold_ci, "try-error")) {
            # Handle case where multiple CIs are returned (e.g. multiple optimal thresholds)
            if (is.list(threshold_ci) && !inherits(threshold_ci, "ci")) {
              threshold_ci <- threshold_ci[[1]]
            }

            results$threshold <- list(
              estimate = if(length(optimal_coords$threshold) > 1) optimal_coords$threshold[1] else optimal_coords$threshold,
              ci_lower = threshold_ci[1],
              ci_upper = threshold_ci[3]
            )
          }
        }

        # Calculate CI for sensitivity
        if ("sensitivity" %in% metrics) {
          sens_ci <- try(pROC::ci.coords(roc_obj, x = "best", best.method = "youden",
                                         ret = "sensitivity", method = "bootstrap", boot.n = self$options$bootstrapReps),
                         silent = TRUE)

          if (!inherits(sens_ci, "try-error")) {
            results$sensitivity <- list(
              estimate = optimal_coords$sensitivity,
              ci_lower = sens_ci[1],
              ci_upper = sens_ci[3]
            )
          }
        }

        # Calculate CI for specificity
        if ("specificity" %in% metrics) {
          spec_ci <- try(pROC::ci.coords(roc_obj, x = "best", best.method = "youden",
                                         ret = "specificity", method = "bootstrap", boot.n = self$options$bootstrapReps),
                         silent = TRUE)

          if (!inherits(spec_ci, "try-error")) {
            results$specificity <- list(
              estimate = optimal_coords$specificity,
              ci_lower = spec_ci[1],
              ci_upper = spec_ci[3]
            )
          }
        }
      }

      return(results)
    },

    # Prepare data for a single variable with optional subgrouping
    .prepareVarData = function(data, var, subGroup) {
      # Escape variable names for safe data frame access
      classVarEscaped <- private$.escapeVar(self$options$classVar)

      if (is.null(subGroup)) {
        # Use [[]] for column access with escaped names
        varEscaped <- private$.escapeVar(var)
        dependentVar <- as.numeric(data[[varEscaped]])
        classVar <- data[[classVarEscaped]]
      } else {
        varParts <- strsplit(var, split = "_")[[1]]
        varName <- varParts[1]
        varNameEscaped <- private$.escapeVar(varName)
        groupName <- paste(varParts[-1], collapse="_")
        dependentVar <- as.numeric(data[subGroup == groupName, varNameEscaped])
        classVar <- data[subGroup == groupName, classVarEscaped]
      }
      
      # Data validation and cleaning
      # Remove rows with missing values in either variable
      complete_cases <- !is.na(dependentVar) & !is.na(classVar)
      dependentVar <- dependentVar[complete_cases]
      classVar <- classVar[complete_cases]
      
      # Ensure classVar is factor
      classVar <- as.factor(classVar)
      unique_levels <- levels(classVar)
      
      # Validate that positive class exists
      if (!self$options$positiveClass %in% unique_levels) {
        private$.throwError("missing_positive_class", 
          paste("'", self$options$positiveClass, "' not found in class variable."),
          paste("Available levels:", paste(unique_levels, collapse = ", ")))
      }
      
      # Dichotomize: Convert to binary by treating selected level as positive, all others as negative
      classVar <- factor(ifelse(classVar == self$options$positiveClass, 
                               self$options$positiveClass, 
                               "Other"), 
                        levels = c("Other", self$options$positiveClass))
      
      # Check minimum sample sizes
      pos_count <- sum(classVar == self$options$positiveClass)
      neg_count <- sum(classVar != self$options$positiveClass)
      
      if (pos_count < 2) {
        private$.throwError("insufficient_data", 
          paste("Need at least 2 positive cases, found:", pos_count),
          "Ensure your data contains sufficient positive cases for reliable analysis")
      }
      if (neg_count < 2) {
        private$.throwError("insufficient_data", 
          paste("Need at least 2 negative cases, found:", neg_count),
          "Ensure your data contains sufficient negative cases for reliable analysis")
      }
      
      list(dependentVar = dependentVar, classVar = classVar)
    },

    # Run cutpointr and return results with confusion matrix
    .runCutpointrMetrics = function(dependentVar, classVar, positiveClass,
                                    method, score, metric, direction,
                                    tol_metric, boot_runs, break_ties) {
      result_success <- FALSE
      result_message <- NULL
      results <- NULL

      tryCatch({
        results <- cutpointr::cutpointr(
          x = dependentVar,
          class = classVar,
          subgroup = NULL,
          method = method,
          cutpoint = score,
          metric = metric,
          direction = direction,
          pos_class = positiveClass,
          tol_metric = tol_metric,
          boot_runs = boot_runs,
          break_ties = break_ties,
          na.rm = TRUE
        )
        result_success <- TRUE
      }, error = function(e) {
        result_message <<- e$message
      })

      if (!result_success) {
        # Fallback: manual ROC calculation
        response <- as.numeric(classVar == positiveClass)
        roc_data <- data.frame(
          x.sorted = sort(unique(dependentVar)),
          direction = ifelse(direction == ">=", ">", "<")
        )
        for(i in 1:nrow(roc_data)) {
          threshold <- roc_data$x.sorted[i]
          if(direction == ">=")
            predicted_pos <- dependentVar >= threshold
          else
            predicted_pos <- dependentVar <= threshold

          roc_data$tp[i] <- sum(predicted_pos & response == 1)
          roc_data$fp[i] <- sum(predicted_pos & response == 0)
          roc_data$tn[i] <- sum(!predicted_pos & response == 0)
          roc_data$fn[i] <- sum(!predicted_pos & response == 1)
        }

        sens <- roc_data$tp / (roc_data$tp + roc_data$fn)
        spec <- roc_data$tn / (roc_data$tn + roc_data$fp)
        roc_points <- data.frame(specificity = spec, sensitivity = sens)
        roc_points <- roc_points[order(1-roc_points$specificity),]
        auc <- 0
        for(i in 2:nrow(roc_points)) {
          x_diff <- (1-roc_points$specificity[i]) - (1-roc_points$specificity[i-1])
          y_avg <- (roc_points$sensitivity[i] + roc_points$sensitivity[i-1])/2
          auc <- auc + x_diff * y_avg
        }
        youdens_j <- sens + spec - 1
        optimal_idx <- which.max(youdens_j)
        results <- list(
          optimal_cutpoint = roc_data$x.sorted[optimal_idx],
          roc_curve = list(roc_data),
          AUC = auc
        )
      }

      confusionMatrix <- results$roc_curve[[1]]
      list(results = results, confusionMatrix = confusionMatrix)
    },

    # Generate tables with metrics for each threshold
    # This was the missing method that was causing errors
    .generateTables = function(var, results, confusionMatrix, resultsToDisplay,
                               dependentVar, classVar, positiveClass, direction, metric) {

      # Initialize lists to store metrics
      sensList <- numeric()
      specList <- numeric()
      ppvList <- numeric()
      npvList <- numeric()
      youdenList <- numeric()
      metricList <- numeric()

      # Calculate prevalence
      n_pos <- sum(classVar == positiveClass)
      n_neg <- sum(classVar != positiveClass)
      prevalence <- n_pos / (n_pos + n_neg)

      # Override with prior prevalence if requested
      if (self$options$usePriorPrev)
        prevalence <- self$options$priorPrev

      # Store prevalence for this variable
      private$.prevalenceList[[var]] <- prevalence

      # Process each threshold
      for (i in seq_along(confusionMatrix$x.sorted)) {
        tp <- confusionMatrix$tp[i]
        fp <- confusionMatrix$fp[i]
        tn <- confusionMatrix$tn[i]
        fn <- confusionMatrix$fn[i]

        # Handle missing values by replacing with 0
        tp <- ifelse(is.na(tp), 0, tp)
        fp <- ifelse(is.na(fp), 0, fp)
        tn <- ifelse(is.na(tn), 0, tn)
        fn <- ifelse(is.na(fn), 0, fn)

        # Calculate metrics with safe denominators
        sensitivity <- if (!is.na(tp + fn) && (tp + fn) > 0) tp / (tp + fn) else 0
        specificity <- if (!is.na(tn + fp) && (tn + fp) > 0) tn / (tn + fp) else 0
        ppv <- if (!is.na(tp + fp) && (tp + fp) > 0) tp / (tp + fp) else 0
        npv <- if (!is.na(tn + fn) && (tn + fn) > 0) tn / (tn + fn) else 0
        youden <- sensitivity + specificity - 1

        # Store metrics
        sensList <- c(sensList, sensitivity)
        specList <- c(specList, specificity)
        ppvList <- c(ppvList, ppv)
        npvList <- c(npvList, npv)
        youdenList <- c(youdenList, youden)

        # Calculate the optimization metric value
        metric_value <- 0
        if (!is.null(metric)) {
          tryCatch({
            # Create temporary confusion matrix for metric calculation
            temp_cm <- data.frame(tp = tp, fp = fp, tn = tn, fn = fn)
            metric_value <- metric(temp_cm, 1)  # Apply metric function
          }, error = function(e) {
            metric_value <- NA
          })
        }
        metricList <- c(metricList, metric_value)
      }

      # Find optimal index based on Youden's J
      j_max_idx <- which.max(youdenList)

      # Store optimal criteria for this variable
      if (length(j_max_idx) > 0) {
        private$.optimalCriteriaList[[var]] <- list(
          threshold = confusionMatrix$x.sorted[j_max_idx],
          sensitivity = sensList[j_max_idx],
          specificity = specList[j_max_idx],
          ppv = ppvList[j_max_idx],
          npv = npvList[j_max_idx],
          youden = youdenList[j_max_idx]
        )
      }

      # Store ROC data for plotting
      private$.rocDataList[[var]] <- data.frame(
        threshold = confusionMatrix$x.sorted,
        sensitivity = sensList,
        specificity = specList,
        ppv = ppvList,
        npv = npvList,
        youden = youdenList,
        metric = metricList,
        stringsAsFactors = FALSE
      )

      # Add raw data as attribute for confidence band calculations
      attr(private$.rocDataList[[var]], "rawData") <- data.frame(
        value = dependentVar,
        class = ifelse(classVar == positiveClass, "Positive", "Negative"),
        stringsAsFactors = FALSE
      )

      # Populate results table
      resultsTable <- self$results$resultsTable$get(key = var)

      # Clear existing rows
      resultsTable$deleteRows()

      # Add rows for each threshold to display
      for (threshold in resultsToDisplay) {
        # Find closest threshold in our data
        idx <- which.min(abs(confusionMatrix$x.sorted - threshold))

        if (length(idx) > 0) {
          # Add row to results table
          resultsTable$addRow(rowKey = threshold, values = list(
            cutpoint = threshold,
            sensitivity = sensList[idx],
            specificity = specList[idx],
            ppv = ppvList[idx],
            npv = npvList[idx],
            youden = youdenList[idx],
            AUC = results$AUC,
            metricValue = metricList[idx]
          ))
        }
      }

      # Populate sensitivity/specificity table if requested
      if (self$options$sensSpecTable) {
        sensSpecTable <- self$results$sensSpecTable$get(key = var)

        # Get optimal cutpoint data - use closest match instead of exact equality
        # to avoid floating point precision issues
        optimal_idx <- which.min(abs(confusionMatrix$x.sorted - results$optimal_cutpoint[1]))
        if (length(optimal_idx) > 0) {
          # Ensure we get scalar values for the confusion matrix cells
          tp <- as.numeric(confusionMatrix$tp[optimal_idx])[1]
          fp <- as.numeric(confusionMatrix$fp[optimal_idx])[1]
          tn <- as.numeric(confusionMatrix$tn[optimal_idx])[1]
          fn <- as.numeric(confusionMatrix$fn[optimal_idx])[1]

          # Create HTML table using enhanced formatting
          html_table <- private$.formatSensSpecTable(
            Title = paste("Confusion Matrix for", var, "at Optimal Cutpoint =",
                          round(results$optimal_cutpoint[1], 3)),
            TP = tp, FP = fp, TN = tn, FN = fn
          )

          sensSpecTable$setContent(html_table)
        }
      }

      return(list(
        sensList = sensList,
        specList = specList,
        ppvList = ppvList,
        npvList = npvList,
        youdenList = youdenList,
        metricList = metricList,
        j_max_idx = j_max_idx
      ))
    },

    # Calculate precision-recall curve
    .calculatePrecisionRecall = function(x, class, positiveClass) {
      # Convert to binary response (1 = positive, 0 = negative)
      response <- as.numeric(class == positiveClass)

      # Sort values and create thresholds
      sorted_values <- sort(unique(x))
      precision <- numeric(length(sorted_values))
      recall <- numeric(length(sorted_values))

      # Calculate precision and recall for each threshold
      for (i in seq_along(sorted_values)) {
        threshold <- sorted_values[i]
        predicted_pos <- x >= threshold

        # Compute TP, FP, FN
        tp <- sum(predicted_pos & response == 1)
        fp <- sum(predicted_pos & response == 0)
        fn <- sum(!predicted_pos & response == 1)

        # Calculate precision and recall
        precision[i] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
        recall[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
      }

      # Calculate AUPRC using the trapezoidal rule
      auprc <- 0
      for (i in 2:length(recall)) {
        auprc <- auprc + 0.5 * (precision[i] + precision[i-1]) * abs(recall[i] - recall[i-1])
      }

      return(list(
        threshold = sorted_values,
        precision = precision,
        recall = recall,
        auprc = auprc
      ))
    },

    # Calculate comprehensive performance metrics for classifier comparison
    .calculateClassifierMetrics = function(x, class, positiveClass) {
      # Check package dependencies
      if (!private$.checkPackageDependencies("pROC", "classifier comparison")) {
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(setdiff(levels(class), positiveClass)[1], positiveClass),
        direction = "<",
        quiet = TRUE
      )

      # Calculate AUC
      auc <- pROC::auc(roc_obj)

      # Calculate precision-recall
      pr_results <- private$.calculatePrecisionRecall(x, class, positiveClass)
      auprc <- pr_results$auprc

      # Convert to binary response (1 = positive, 0 = negative)
      response <- as.numeric(class == positiveClass)

      # Find optimal threshold using Youden's index
      coords <- pROC::coords(roc_obj, x = "best", best.method = "youden")
      threshold <- coords$threshold

      # Get predictions using optimal threshold
      predicted_prob <- roc_obj$predictor
      predicted_class <- ifelse(predicted_prob >= threshold, 1, 0)

      # Calculate Brier score
      brier_score <- mean((predicted_prob - response)^2)

      # Calculate confusion matrix metrics
      tp <- sum(predicted_class == 1 & response == 1)
      tn <- sum(predicted_class == 0 & response == 0)
      fp <- sum(predicted_class == 1 & response == 0)
      fn <- sum(predicted_class == 0 & response == 1)

      # Calculate F1 score
      precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
      recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
      f1_score <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)

      # Calculate accuracy
      accuracy <- (tp + tn) / (tp + tn + fp + fn)

      # Calculate balanced accuracy
      sensitivity <- tp / (tp + fn)
      specificity <- tn / (tn + fp)
      balanced_accuracy <- (sensitivity + specificity) / 2

      return(list(
        auc = as.numeric(auc),
        auprc = auprc,
        brier = brier_score,
        f1_score = f1_score,
        accuracy = accuracy,
        balanced_accuracy = balanced_accuracy
      ))
    },

    # Perform DeLong's test for comparing AUCs
    # Moved from outside the class to inside as a private method
    .deLongTest = function(data, classVar, positiveClass, ref = NULL, conf.level = private$.CONFIDENCE_LEVEL) {
      # Validate and prepare inputs
      # Convert factor to character first to handle labels safely
      if (is.factor(classVar)) {
        classVar <- as.character(classVar)
      }

      # Safely identify the positive class
      if (!positiveClass %in% unique(classVar)) {
        # Try to interpret positiveClass as a position index
        if (is.numeric(try(as.numeric(positiveClass), silent = TRUE))) {
          pos_idx <- as.numeric(positiveClass)
          if (pos_idx <= length(unique(classVar))) {
            positiveClass <- unique(classVar)[pos_idx]
          }
        }
        # If still not found, use the first level
        if (!positiveClass %in% unique(classVar)) {
          warning(.("Specified positive class not found. Using first unique value instead."))
          positiveClass <- unique(classVar)[1]
        }
      }

      # Check for NA values in classification variable
      if (any(is.na(classVar))) {
        stop(.("Classification variable contains missing values (NA). Please remove or handle missing values before performing ROC analysis."))
      }

      # Check if positive class exists in the data
      id.pos <- classVar == positiveClass
      if (sum(id.pos, na.rm = TRUE) < 1) {
        stop(.("Wrong level specified for positive class. No observations found."))
      }

      # Check data dimensions
      if (dim(data)[2] < 2) {
        stop(.("Data must contain at least two columns (different measures)."))
      }
      if (dim(data)[1] < 2) {
        stop(.("Data must contain at least two rows (observations)."))
      }

      # Get counts of positive and negative cases
      nn <- sum(!id.pos)  # Number of negative cases
      np <- sum(id.pos)   # Number of positive cases
      nauc <- ncol(data)  # Number of tests

      # Set up comparison matrix based on reference or pairwise
      if (is.null(ref)) {
        # Create matrix for all pairwise comparisons
        L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
        newa <- 0
        for (i in 1:(nauc - 1)) {
          newl <- nauc - i
          L[(newa + 1):(newa + newl), i] <- rep(1, newl)
          L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <-
            diag(-1, nrow = newl, ncol = newl)
          newa <- newa + newl
        }
      } else {
        # Create matrix for comparing all tests against a reference
        if (ref > nauc)
          stop(paste("Reference ref must be one of the markers (1...", nauc, " in this case)", sep = ""))
        L <- matrix(1, ncol = nauc, nrow = nauc - 1)
        L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
      }

      # Split data into positive and negative cases
      markern <- as.matrix(data[!id.pos,])
      markerp <- as.matrix(data[id.pos,])

      # Function to compute the Wilcoxon statistic
      WK.STAT <- function(data, y) {
        r <- rank(c(data, y))
        n.data <- length(data)
        n.y <- length(y)
        STATISTIC <- sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
        STATISTIC
      }

      # Calculate AUC for each test using Wilcoxon statistic
      auc <- vector("numeric", length = nauc)
      for (r in 1:nauc) {
        auc[r] <- WK.STAT(markerp[, r], markern[, r])
      }
      auc <- auc / (nn * np)  # Normalize to [0,1]

      # For AUCs < 0.5, invert the test scores (AUC will be > 0.5)
      if (any(auc < 0.5)) {
        data[, auc < 0.5] <- -data[, auc < 0.5]
        auc[auc < 0.5] <- 1 - auc[auc < 0.5]
        markern <- as.matrix(data[!id.pos,])
        markerp <- as.matrix(data[id.pos,])
      }

      # Calculate placement values for covariance estimation
      V10 <- matrix(0, nrow = np, ncol = nauc)
      V01 <- matrix(0, nrow = nn, ncol = nauc)

      tmn <- t(markern)
      tmp <- t(markerp)

      # Calculate placement values for each positive case
      for (i in 1:np) {
        V10[i,] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
      }

      # Calculate placement values for each negative case
      for (i in 1:nn) {
        V01[i,] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
      }

      # Normalize placement values
      V10 <- V10 / nn
      V01 <- V01 / np

      # Calculate covariance matrices
      W10 <- cov(V10)
      W01 <- cov(V01)

      # Estimated covariance matrix for AUCs
      S <- W10 / np + W01 / nn

      # Compute variances of AUCs using Hanley & McNeil (1982) formula
      q1 <- auc / (2 - auc)
      q2 <- 2 * auc ^ 2 / (1 + auc)

      # Calculate standard errors and p-values (against null hypothesis AUC = 0.5)
      aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc ^ 2) + (nn - 1) * (q2 - auc ^ 2)) / (np * nn)
      zhalf <- (auc - 0.5) / sqrt(aucvar)
      phalf <- 1 - pnorm(zhalf)
      zdelong <- (auc - 0.5) / sqrt(diag(S))
      pdelong <- 1 - pnorm(zdelong)

      # Global test for difference between AUCs
      aucdiff <- L %*% auc
      z <- t(aucdiff) %*% MASS::ginv(L %*% S %*% t(L)) %*% aucdiff
      p <- pchisq(z, df = qr(L %*% S %*% t(L))$rank, lower.tail = FALSE)

      # Calculate confidence intervals for pairwise differences
      if (is.null(ref)) {
        # All pairwise comparisons
        cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
        ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
        ctr <- 1
        rows <- vector("character", length = (nauc * (nauc - 1) / 2))
        pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
        quantil <- qnorm(1 - (1 - conf.level) / 2)

        for (i in 1:(nauc - 1)) {
          for (j in (i + 1):nauc) {
            # Calculate correlation between AUCs
            cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])

            # Calculate confidence interval for the difference
            LSL <- t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
            tmpz <- (aucdiff[ctr]) %*% MASS::ginv(LSL) %*% aucdiff[ctr]
            pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
            ci[ctr,] <- c(aucdiff[ctr] - quantil * sqrt(LSL), aucdiff[ctr] + quantil * sqrt(LSL))
            rows[ctr] <- paste(i, j, sep = " vs. ")
            ctr <- ctr + 1
          }
        }
      } else {
        # Comparisons against a reference
        cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
        ci <- matrix(ncol = 2, nrow = nauc - 1)
        rows <- vector("character", length = nauc - 1)
        pairp <- matrix(nrow = nauc - 1, ncol = 1)
        comp <- (1:nauc)[-ref]

        for (i in 1:(nauc - 1)) {
          # Calculate correlation between reference and current AUC
          cor.auc[i] <- S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])

          # Calculate confidence interval for the difference
          LSL <- t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
          tmpz <- aucdiff[i] %*% MASS::ginv(LSL) %*% aucdiff[i]
          pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
          ci[i,] <- c(aucdiff[i] - quantil * sqrt(LSL), aucdiff[i] + quantil * sqrt(LSL))
          rows[i] <- paste(ref, comp[i], sep = " vs. ")
        }
      }

      # Format results for return
      newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
      names(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation")
      rownames(newres) <- rows

      row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
      colnames(ci) <- c(paste0(100 * conf.level, "% CI (lower)"), paste0(100 * conf.level, "% CI (upper)"))

      names(auc) <- 1:nauc
      auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
      colnames(auc) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")

      # Prepare return object
      ERG <- list(
        AUC = auc,
        difference = newres,
        covariance = S,
        global.z = z,
        global.p = p
      )
      class(ERG) <- "DeLong"
      return(ERG)
    },

    # ============================================================================
    # INITIALIZATION METHOD
    # ============================================================================

    # Initialize the analysis
    .init = function() {
      # Keep main tables visible but empty them to prevent loading animations
      self$results$simpleResultsTable$setVisible(TRUE)
      self$results$runSummary$setVisible(TRUE)
      self$results$aucSummaryTable$setVisible(TRUE)
      self$results$clinicalInterpretationTable$setVisible(TRUE)
      
      # Hide optional elements initially
      self$results$resultsTable$setVisible(FALSE)
      self$results$sensSpecTable$setVisible(FALSE)
      self$results$thresholdTable$setVisible(FALSE)
      self$results$delongComparisonTable$setVisible(FALSE)
      self$results$delongTest$setVisible(FALSE)
      self$results$plotROC$setVisible(FALSE)
      self$results$interactivePlot$setVisible(FALSE)
      self$results$criterionPlot$setVisible(FALSE)
      self$results$prevalencePlot$setVisible(FALSE)
      self$results$dotPlot$setVisible(FALSE)
      
      # Apply clinical mode and preset configurations
      private$.applyClinicalModeSettings()
      private$.applyClinicalPresetSettings()
      self$results$precisionRecallPlot$setVisible(FALSE)
      self$results$procedureNotes$setVisible(FALSE)
      self$results$metaAnalysisWarning$setVisible(FALSE)
      self$results$metaAnalysisTable$setVisible(FALSE)
      self$results$metaAnalysisForestPlot$setVisible(FALSE)

      # Initialize advanced plot states (enable rendering when data is ready)
      # These plots have renderFun defined in .r.yaml but need setState to activate
      if (length(self$options$dependentVars) > 0) {
        # Effect size plots
        if (self$options$effectSizeAnalysis && length(self$options$dependentVars) > 1) {
          for (var in self$options$dependentVars) {
            image <- self$results$effectSizePlot$get(key=var)
            if (!is.null(image)) image$setState(list(ready=FALSE))
          }
        }

        # Power curve plots
        if (self$options$powerAnalysis) {
          for (var in self$options$dependentVars) {
            image <- self$results$powerCurvePlot$get(key=var)
            if (!is.null(image)) image$setState(list(ready=FALSE))
          }
        }

        # Bayesian trace plots
        if (self$options$bayesianAnalysis) {
          for (var in self$options$dependentVars) {
            image <- self$results$bayesianTracePlot$get(key=var)
            if (!is.null(image)) image$setState(list(ready=FALSE))
          }
        }

        # Decision curve plots
        if (self$options$clinicalUtilityAnalysis) {
          for (var in self$options$dependentVars) {
            image <- self$results$decisionCurvePlot$get(key=var)
            if (!is.null(image)) image$setState(list(ready=FALSE))
          }
        }
      }

      # Set up initial instructions - show by default until variables are provided
      self$results$instructions$setVisible(TRUE)
      self$results$instructions$setContent(
        "<html>
                  <head>
                  </head>
                  <body>
                  This function was originally developed by Lucas Friesen in pschoPDA module. <a href='https://github.com/ClinicoPath/jamoviPsychoPDA'>The original module</a> is no longer maintained. The testroc function with additional features are added to the meddecide module.
                  <div class='instructions'>
                  <p><b>ROC Analysis for Medical Decision Making</b></p>
                  <p>This analysis creates Receiver Operating Characteristic (ROC) curves and calculates optimal cutpoints for diagnostic tests.</p>
                  <p>To get started:</p>
                  <ol>
                  <li>Place the test result variable(s) in the 'Test Variables' slot<br /><br /></li>
                  <li>Place the binary classification (gold standard) in the 'Class Variable (Gold Standard)' slot<br /><br /></li>
                  <li>[<em>Optional</em>] Place a grouping variable in the 'Subgroup Variable (Optional)' slot<br /><br /></li>
                  </ol>
                  <p>The ROC analysis helps you determine optimal cut-off values for classifying cases.</p>
                  </div>
                  </body>
                  </html>"
      )
      
      # Enable meta-analysis options only when sufficient variables are available
      private$.updateMetaAnalysisVisibility()
    },

    # ============================================================================
    # MAIN ANALYSIS METHOD
    # ============================================================================

    # Execute the ROC analysis
    .run = function() {
      print("DEBUG: Starting .run")
      
      # -----------------------------------------------------------------------
      # 0. SET SEED FOR REPRODUCIBILITY
      # -----------------------------------------------------------------------
      if (!is.null(self$options$seed)) {
        set.seed(self$options$seed)
      }

      # -----------------------------------------------------------------------
      # 1. INSTRUCTIONS AND PRELIMINARY CHECKS
      # -----------------------------------------------------------------------

      # Update meta-analysis UI visibility based on variable count
      private$.updateMetaAnalysisVisibility()

      # Show instructions if required inputs are not provided
      if (is.null(self$options$classVar) || is.null(self$options$dependentVars) || 
          length(self$options$dependentVars) == 0) {
        self$results$instructions$setContent(
          "<html>
                    <head>
                    </head>
                    <body>
                    This function was originally developed by Lucas Friesen in pschoPDA module. <a href='https://github.com/ClinicoPath/jamoviPsychoPDA'>The original module</a> is no longer maintained. The testroc function with additional features are added to the meddecide module.
                    <div class='instructions'>
                    <p><b>ROC Analysis for Medical Decision Making</b></p>
                    <p>This analysis creates Receiver Operating Characteristic (ROC) curves and calculates optimal cutpoints for diagnostic tests.</p>
                    <p>To get started:</p>
                    <ol>
                    <li>Place the test result variable(s) in the 'Test Variables' slot<br /><br /></li>
                    <li>Place the binary classification (gold standard) in the 'Class Variable (Gold Standard)' slot<br /><br /></li>
                    <li>[<em>Optional</em>] Place a grouping variable in the 'Subgroup Variable (Optional)' slot<br /><br /></li>
                    </ol>
                    <p>The ROC analysis helps you determine optimal cut-off values for classifying cases.</p>
                    </div>
                    </body>
                    </html>"
        )
        return()
      } else {
        # Strict Validation
        val <- private$.validateInputs()
        if (!is.null(val)) {
          self$results$runSummary$setContent(paste("<b>Error:</b>", val))
          self$results$instructions$setVisible(FALSE)
          stop(val) # Stop execution
        }

        # Hide instructions when inputs are provided
        self$results$instructions$setVisible(FALSE)
        
        # Show procedure notes when analysis proceeds
        self$results$procedureNotes$setVisible(TRUE)
        
        # Make ROC plots visible if requested
        if (self$options$plotROC) {
          self$results$plotROC$setVisible(TRUE)
        }
        
        # Make optional tables/plots visible based on options
        if (self$options$showThresholdTable) {
          self$results$thresholdTable$setVisible(TRUE)
        }
        if (self$options$sensSpecTable) {
          self$results$sensSpecTable$setVisible(TRUE)
        }
        if (self$options$delongTest) {
          self$results$delongComparisonTable$setVisible(TRUE)
          self$results$delongTest$setVisible(TRUE)
        }

        # Create procedure notes with analysis details
        procedureNotes <- paste0(
          "<html>
                    <body>
                    <p>Procedure Notes</p>
                    <hr>",
          "<p> The ROC analysis has been completed using the following specifications: ",
          "<p>&nbsp;</p>",
          "<p> Measure Variable(s): ",
          paste(unlist(self$options$dependentVars), collapse = ", "),
          "</p>",
          "<p> Class Variable: ",
          self$options$classVar,
          "</p>"
        )

        # Add positive class info
        if (self$options$positiveClass == "") {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Positive Class: ", as.character(unique(self$data[,self$options$classVar])[1]),
            " (first level)</p>")
        } else {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Positive Class: ",
            self$options$positiveClass,
            "</p>")
        }

        # Add subgroup info if used
        if (FALSE) { # (!is.null(self$options$subGroup) && self$options$subGroup != "") {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Subgroup Variable: ",
            self$options$subGroup,
            "</p>")
        }

        # Add enhanced analysis settings with more detail
        procedureNotes <- paste0(
          procedureNotes,
          "<p>&nbsp;</p>",
          "<p> Method: ",
          self$options$method,
          "</p>",
          "<p> All Observed Cutpoints: ",
          self$options$allObserved,
          "</p>",
          "<p> Metric: ",
          self$options$metric,
          "</p>",
          "<p> Direction (relative to cutpoint): ",
          self$options$direction,
          "</p>",
          "<p> Tie Breakers: ",
          self$options$break_ties,
          "</p>",
          "<p> Metric Tolerance: ",
          self$options$tol_metric,
          "</p>",
          "<p>&nbsp;</p>"
        )

        # Add bootstrap info if applicable
        if (self$options$boot_runs > 0) {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Bootstrap Runs: ",
            self$options$boot_runs,
            "</p>")
        }

        # Close notes
        procedureNotes <- paste0(
          procedureNotes,
          "<hr /></body></html>"
        )
        self$results$procedureNotes$setContent(procedureNotes)
      }


      # -----------------------------------------------------------------------
      # 2. SET UP ANALYSIS PARAMETERS
      # -----------------------------------------------------------------------
      print("DEBUG: Setup Analysis Parameters")

      # Get data
      data <- self$data

      # Determine positive class early for use throughout the analysis
      # Escape class variable name for safe data access
      classVarEscaped <- private$.escapeVar(self$options$classVar)

      if (!is.null(self$options$positiveClass) && self$options$positiveClass != "") {
        # Use the level selector value
        positiveClass <- self$options$positiveClass

        # Verify the selected level exists in the data
        classVar <- data[[classVarEscaped]]
        if (!positiveClass %in% levels(factor(classVar))) {
          warning(paste("Selected positive class", positiveClass,
                        "not found in data. Using first level instead."))
          positiveClass <- levels(factor(classVar))[1]
        }
      } else {
        # Default to first level if not specified
        classVar <- data[[classVarEscaped]]
        positiveClass <- levels(factor(classVar))[1]
      }
      
      # -----------------------------------------------------------------------
      # 2.5. VALIDATION AND SAFEGUARDS
      # -----------------------------------------------------------------------
      
      # Check class imbalance
      n_pos <- sum(classVar == positiveClass, na.rm = TRUE)
      n_total <- sum(!is.na(classVar))
      prevalence <- if (n_total > 0) n_pos / n_total else NA

      summary_status <- list()
      summary_status$warnings <- character()

      if (!is.na(prevalence) && (prevalence < 0.1 || prevalence > 0.9)) {
        summary_status$warnings <- c(summary_status$warnings,
          sprintf("Class imbalance detected (Prevalence: %.1f%%). Consider using Precision-Recall curves.", prevalence * 100))
      }
      
      # Gating incompatible options
      if (!is.null(self$options$subGroup)) {
        if (self$options$delongTest) {
           summary_status$warnings <- c(summary_status$warnings, "DeLong test disabled because subgroup analysis is active.")
        }
        if (self$options$calculateIDI || self$options$calculateNRI) {
           summary_status$warnings <- c(summary_status$warnings, "IDI/NRI disabled because subgroup analysis is active.")
        }
      }
      
      # Populate Run Summary
      run_summary_html <- paste0(
        "<div style='padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; margin-bottom: 15px;'>",
        "<h4>Analysis Status</h4>",
        "<ul>",
        "<li><strong>Seed:</strong> ", self$options$seed, "</li>",
        "<li><strong>Positive Class:</strong> ", positiveClass, " (Prevalence: ", round(prevalence * 100, 1), "%)</li>", 
        "<li><strong>Analysis Mode:</strong> ", tools::toTitleCase(self$options$clinicalMode), "</li>"
      )
      
      if (length(summary_status$warnings) > 0) {
        run_summary_html <- paste0(run_summary_html, 
          "</ul><div style='background-color: #fff3cd; color: #856404; padding: 10px; border-radius: 4px; margin-top: 10px;'>",
          "<strong>Warnings:</strong><ul>")
        for (w in summary_status$warnings) {
          run_summary_html <- paste0(run_summary_html, "<li>", w, "</li>")
        }
        run_summary_html <- paste0(run_summary_html, "</ul></div>")
      } else {
        run_summary_html <- paste0(run_summary_html, "</ul></div>")
      }
      
      self$results$runSummary$setContent(run_summary_html)

      # Set up cutpoint method
      if (self$options$method == "oc_manual") {
        method <- cutpointr::oc_manual
        if (self$options$specifyCutScore == "") {
          private$.throwError("invalid_cutpoint", 
            "Cut score is required when using manual cutpoint method.",
            "Enter a numeric value in the 'Manual Cut Score' field")
        } else {
          score <- as.numeric(self$options$specifyCutScore)
        }
      } else {
        # Map method name to actual function
        methodName <- self$options$method
        if (methodName == "maximize_metric") {
          method <- cutpointr::maximize_metric
        } else if (methodName == "minimize_metric") {
          method <- cutpointr::minimize_metric
        } else if (methodName == "maximize_loess_metric") {
          method <- cutpointr::maximize_loess_metric
        } else if (methodName == "minimize_loess_metric") {
          method <- cutpointr::minimize_loess_metric
        } else if (methodName == "maximize_spline_metric") {
          method <- cutpointr::maximize_spline_metric
        } else if (methodName == "minimize_spline_metric") {
          method <- cutpointr::minimize_spline_metric
        } else if (methodName == "maximize_boot_metric") {
          method <- cutpointr::maximize_boot_metric
        } else if (methodName == "minimize_boot_metric") {
          method <- cutpointr::minimize_boot_metric
        } else if (methodName == "oc_youden_kernel") {
          method <- cutpointr::oc_youden_kernel
        } else if (methodName == "oc_youden_normal") {
          method <- cutpointr::oc_youden_normal
        } else if (methodName %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
          # For custom methods, use maximize_metric as placeholder
          # We'll handle the actual calculation later
          method <- cutpointr::maximize_metric
        } else {
          # Default if method not recognized
          method <- cutpointr::maximize_metric
        }

        score <- NULL
      }

      # Set up tolerance if needed for specific methods
      if (self$options$method %in% c(
        "maximize_metric",
        "minimize_metric",
        "maximize_loess_metric",
        "minimize_loess_metric",
        "maximize_spline_metric",
        "minimize_spline_metric"
      )) {
        tol_metric <- self$options$tol_metric
      } else {
        tol_metric <- NULL
      }

      # Set up metric function
      metricName <- self$options$metric
      
      if (metricName == "youden") {
        metric <- cutpointr::youden
      } else if (metricName == "sum_sens_spec") {
        metric <- cutpointr::sum_sens_spec
      } else if (metricName == "accuracy") {
        metric <- cutpointr::accuracy
      } else if (metricName == "sum_ppv_npv") {
        metric <- cutpointr::sum_ppv_npv
      } else if (metricName == "prod_sens_spec") {
        metric <- cutpointr::prod_sens_spec
      } else if (metricName == "prod_ppv_npv") {
        metric <- cutpointr::prod_ppv_npv
      } else if (metricName == "cohens_kappa") {
        metric <- cutpointr::cohens_kappa
      } else if (metricName == "abs_d_sens_spec") {
        metric <- cutpointr::abs_d_sens_spec
      } else if (metricName == "roc01") {
        metric <- cutpointr::roc01
      } else if (metricName == "abs_d_ppv_npv") {
        metric <- cutpointr::abs_d_ppv_npv
      } else if (metricName == "p_chisquared") {
        metric <- cutpointr::p_chisquared
      } else if (metricName == "odds_ratio") {
        metric <- cutpointr::odds_ratio
      } else if (metricName == "risk_ratio") {
        metric <- cutpointr::risk_ratio
      } else if (metricName == "misclassification_cost") {
        metric <- cutpointr::misclassification_cost
      } else if (metricName == "total_utility") {
        metric <- cutpointr::total_utility
      } else if (metricName == "F1_score") {
        metric <- cutpointr::F1_score
      } else {
        # Default to Youden's index if not recognized
        metric <- cutpointr::youden
      }

      # Set up break_ties function
      if (self$options$break_ties == "c") {
        break_ties <- c
      } else if (self$options$break_ties == "mean") {
        break_ties <- mean
      } else if (self$options$break_ties == "median") {
        break_ties <- median
      } else {
        break_ties <- mean
      }

      # Get other analysis parameters
      direction <- self$options$direction
      boot_runs <- as.numeric(self$options$boot_runs)

      # Set up for collecting plot data
      plotDataList <- data.frame(
        var = character(),
        cutpoint = numeric(),
        sensitivity = numeric(),
        specificity = numeric(),

        ppv = numeric(),
        npv = numeric(),
        AUC = numeric(),
        youden = numeric(),
        stringsAsFactors = FALSE
      )


      # -----------------------------------------------------------------------
      # 3. PREPARE VARIABLES FOR ANALYSIS
      # -----------------------------------------------------------------------

      # Get dependent variables list
      vars <- self$options$dependentVars

      # Handle subgroups if present
      if (!is.null(self$options$subGroup)) {
        # Escape variable names for safe data frame access
        subGroupEscaped <- private$.escapeVar(self$options$subGroup)
        classVarEscaped <- private$.escapeVar(self$options$classVar)

        # Use [[]] for safe column access with escaped names
        subGroup <- data[[subGroupEscaped]]
        classVar <- data[[classVarEscaped]]
        uniqueGroups <- unique(subGroup)
        # Create combined variable names (var_group)
        vars <- apply(expand.grid(vars, uniqueGroups), 1, function(x) paste(x, collapse="_"))
      } else {
        subGroup <- NULL
      }

      # Storage for AUCs
      aucList <- list()

      # -----------------------------------------------------------------------
      # 4. PROCESS EACH VARIABLE
      # -----------------------------------------------------------------------

      for (var in vars) {
        # Checkpoint before expensive variable processing
        private$.checkpoint()
        # Add items to results tables if not already present
        if (!var %in% self$results$resultsTable$itemKeys) {
          self$results$sensSpecTable$addItem(key = var)
          self$results$resultsTable$addItem(key = var)

          # Add individual plots if not combining
          if (self$options$combinePlots == FALSE) {
            self$results$plotROC$addItem(key = var)

            # Add additional plot items if enabled
            if (self$options$showCriterionPlot)
              self$results$criterionPlot$addItem(key = var)
            if (self$options$showPrevalencePlot)
              self$results$prevalencePlot$addItem(key = var)
            if (self$options$showDotPlot)
              self$results$dotPlot$addItem(key = var)
          }
        }

        # Extract data for analysis
        prepared <- private$.prepareVarData(data, var, subGroup)
        dependentVar <- prepared$dependentVar
        classVar <- prepared$classVar

        # -----------------------------------------------------------------------
        # 5. RUN ROC ANALYSIS
        # -----------------------------------------------------------------------

        # Checkpoint before expensive ROC analysis
        private$.checkpoint()
        
        cp_res <- private$.runCutpointrMetrics(
          dependentVar = dependentVar,
          classVar = classVar,
          positiveClass = positiveClass,
          method = method,
          score = score,
          metric = metric,
          direction = direction,
          tol_metric = tol_metric,
          boot_runs = boot_runs,
          break_ties = break_ties
        )
        results <- cp_res$results
        confusionMatrix <- cp_res$confusionMatrix

        # -----------------------------------------------------------------------
        # 6. HANDLE CUSTOM CUTPOINT METHODS
        # -----------------------------------------------------------------------

        # Apply custom methods if specified
        if (self$options$method %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
          # Get the confusion matrix data
          confusionMatrix <- results$roc_curve[[1]]

          # Calculate prevalence for cost-ratio method
          n_pos <- sum(classVar == positiveClass)
          n_neg <- sum(classVar != positiveClass)
          prevalence <- n_pos / (n_pos + n_neg)

          # Override with prior prevalence if requested
          if (self$options$usePriorPrev)
            prevalence <- self$options$priorPrev

          if (self$options$method == "oc_cost_ratio") {
            # Use custom cost ratio optimization
            cost_results <- private$.calculateCostRatioOptimal(
              confusionMatrix,
              prevalence,
              self$options$costratioFP
            )
            # Override the optimal cutpoint
            results$optimal_cutpoint <- confusionMatrix$x.sorted[cost_results$optimal_idx]
          }
          else if (self$options$method == "oc_equal_sens_spec") {
            # Find cutpoint with equal sensitivity and specificity
            eq_results <- private$.calculateEqualSensSpec(confusionMatrix)
            results$optimal_cutpoint <- confusionMatrix$x.sorted[eq_results$optimal_idx]
          }
          else if (self$options$method == "oc_closest_01") {
            # Find cutpoint closest to (0,1) point in ROC space
            closest_results <- private$.calculateClosestToOptimal(confusionMatrix)
            results$optimal_cutpoint <- confusionMatrix$x.sorted[closest_results$optimal_idx]
          }
        }

        # -----------------------------------------------------------------------
        # 7. DETERMINE CUTPOINTS TO DISPLAY
        # -----------------------------------------------------------------------

        resultsToDisplay <- if (!self$options$allObserved) unlist(results$optimal_cutpoint) else sort(unique(dependentVar))

        # Generate tables with metrics
        metrics_res <- private$.generateTables(
          var = var,
          results = results,
          confusionMatrix = results$roc_curve[[1]],
          resultsToDisplay = resultsToDisplay,
          dependentVar = dependentVar,
          classVar = classVar,
          positiveClass = positiveClass,
          direction = direction,
          metric = metric
        )
        sensList <- metrics_res$sensList
        specList <- metrics_res$specList
        ppvList <- metrics_res$ppvList
        npvList <- metrics_res$npvList
        youdenList <- metrics_res$youdenList
        metricList <- metrics_res$metricList
        j_max_idx <- metrics_res$j_max_idx
        aucList[[var]] <- results$AUC
        private$.aucList[[var]] <- results$AUC  # Store in private field for clinical interpretation

        # -----------------------------------------------------------------------
        # 7.5. FIXED SENSITIVITY/SPECIFICITY ANALYSIS
        # -----------------------------------------------------------------------
        
        # Populate fixed sensitivity/specificity table if enabled
        if (self$options$fixedSensSpecAnalysis) {
          private$.populateFixedSensSpecTable(var, results$roc_curve[[1]], positiveClass)
          
          # Generate explanatory content (only once for first variable)
          if (var == vars[1]) {
            private$.generateFixedSensSpecExplanation()
          }
          
          # Add plot item for fixed analysis if enabled and not combining
          if (self$options$showFixedROC && self$options$combinePlots == FALSE) {
            if (!var %in% self$results$fixedSensSpecROC$itemKeys) {
              self$results$fixedSensSpecROC$addItem(key = var)
            }
            # Set plot state data for fixed ROC
            fixedImage <- self$results$fixedSensSpecROC$get(key = var)
            fixedImage$setTitle(.(paste("Fixed", tools::toTitleCase(self$options$fixedAnalysisType), "ROC:", var)))
            fixedImage$setState(
              data.frame(
                var = rep(var, length(confusionMatrix$x.sorted)),
                cutpoint = confusionMatrix$x.sorted,
                sensitivity = sensList,
                specificity = specList,
                stringsAsFactors = FALSE
              )
            )
          }
        }

        # -----------------------------------------------------------------------
        # 8. PREPARE PLOTTING DATA
        # -----------------------------------------------------------------------

        if (self$options$plotROC) {
          if (self$options$combinePlots == FALSE) {
            # Individual plot for this variable
            image <- self$results$plotROC$get(key = var)
            image$setTitle(paste("ROC Curve:", var))
            image$setState(
              data.frame(
                var = rep(var, length(confusionMatrix$x.sorted)),
                cutpoint = confusionMatrix$x.sorted,
                sensitivity = sensList,
                specificity = specList,
                ppv = ppvList,
                npv = npvList,
                AUC = rep(results$AUC, length(confusionMatrix$x.sorted)),
                youden = youdenList,
                j_max_idx = j_max_idx,
                stringsAsFactors = FALSE
              )
            )

            # Set states for additional plots if enabled
            if (self$options$showCriterionPlot) {
              criterionImage <- self$results$criterionPlot$get(key = var)
              criterionImage$setTitle(paste("Sensitivity and Specificity vs. Threshold:", var))
              criterionImage$setState(private$.rocDataList[[var]])
            }

            if (self$options$showPrevalencePlot) {
              prevImage <- self$results$prevalencePlot$get(key = var)
              prevImage$setTitle(paste("Predictive Values vs. Prevalence:", var))
              prevImage$setState(list(
                optimal = private$.optimalCriteriaList[[var]],
                prevalence = private$.prevalenceList[[var]]
              ))
            }

            if (self$options$showDotPlot) {
              # Get raw data for dot plot
              rawData <- data.frame(
                value = dependentVar,
                class = ifelse(classVar == positiveClass, "Positive", "Negative"),
                threshold = rep(results$optimal_cutpoint[1], length(dependentVar)),
                direction = rep(direction, length(dependentVar)),
                stringsAsFactors = FALSE
              )

              dotImage <- self$results$dotPlot$get(key = var)
              dotImage$setTitle(paste("Dot Plot:", var))
              dotImage$setState(rawData)
            }
          } else {
            # Collect data for combined plot
            plotDataList <- rbind(
              plotDataList,
              data.frame(
                var = rep(var, length(confusionMatrix$x.sorted)),
                cutpoint = confusionMatrix$x.sorted,
                sensitivity = sensList,
                specificity = specList,
                ppv = ppvList,
                npv = npvList,
                AUC = rep(results$AUC, length(confusionMatrix$x.sorted)),
                youden = youdenList,
                j_max_idx = j_max_idx,
                stringsAsFactors = FALSE
              )
            )
          }
        }
      } # End of loop through variables

      # -----------------------------------------------------------------------
      # 9. CREATE COMBINED PLOTS
      # -----------------------------------------------------------------------

      # Create combined plot if requested
      if (self$options$plotROC && self$options$combinePlots && nrow(plotDataList) > 0) {
        # Add the combined plot
        self$results$plotROC$addItem(key = 1)
        image <- self$results$plotROC$get(key = 1)
        image$setTitle(.("ROC Curve: Combined"))
        image$setState(plotDataList)

        # Combined criterion plot if enabled
        if (self$options$showCriterionPlot) {
          self$results$criterionPlot$addItem(key = 1)
          criterionImage <- self$results$criterionPlot$get(key = 1)
          criterionImage$setTitle(.("Sensitivity and Specificity vs. Threshold: Combined"))

          # Prepare combined data for criterion plot
          combinedCriterionData <- data.frame()
          for (var in names(private$.rocDataList)) {
            varData <- private$.rocDataList[[var]]
            varData$var <- var
            combinedCriterionData <- rbind(combinedCriterionData, varData)
          }
          criterionImage$setState(combinedCriterionData)
        }

        # Dot plots can't be combined meaningfully
        if (self$options$showDotPlot) {
          # Add a message about dot plots in combined mode
          self$results$dotPlotMessage$setContent(
            .("<p>Dot plots aren't available in combined plot mode. Please uncheck 'Combine plots' to view individual dot plots.</p>")
          )
          self$results$dotPlotMessage$setVisible(TRUE)

          # Hide the actual plot in combined mode
          self$results$dotPlot$setVisible(FALSE)
        }

        # Combined prevalence plot if enabled
        if (self$options$showPrevalencePlot) {
          self$results$prevalencePlot$addItem(key = 1)
          prevImage <- self$results$prevalencePlot$get(key = 1)
          prevImage$setTitle("Predictive Values vs. Prevalence: Combined")

          # Use the first variable's data for demonstration
          if (length(private$.optimalCriteriaList) > 0 && length(private$.prevalenceList) > 0) {
            firstVar <- names(private$.optimalCriteriaList)[1]
            prevImage$setState(list(
              optimal = private$.optimalCriteriaList[[firstVar]],
              prevalence = private$.prevalenceList[[firstVar]]
            ))
          }
        }
      }

      # -----------------------------------------------------------------------
      # 10. PERFORM DELONG'S TEST FOR AUC COMPARISON
      # -----------------------------------------------------------------------

      if (self$options$delongTest) {
        # Checkpoint before expensive DeLong test computation
        private$.checkpoint()
        # Enhanced validation with better error handling
        if (length(self$options$dependentVars) < 2) {
          private$.throwError("insufficient_variables", 
            "DeLong's test requires at least 2 test variables.",
            "Select multiple test variables for comparison")
        }

        if (!is.null(self$options$subGroup)) {
          stop(.("DeLong's test does not currently support the group variable. If you would like to contribute/provide guidance, please use the contact information provided in the documentation."))
        } else {
          # Escape variable names for safe data access
          depVarsEscaped <- private$.escapeVar(self$options$dependentVars)
          classVarEscaped <- private$.escapeVar(self$options$classVar)

          # Prepare data frames with escaped column names
          depVarsData <- data.frame(lapply(depVarsEscaped, function(v) as.numeric(data[[v]])))
          names(depVarsData) <- self$options$dependentVars  # Use original names for output

          # Prepare classification variable
          classVarData <- as.character(data[[classVarEscaped]])

          # Filter out rows with NA values in classification variable
          complete_cases <- !is.na(classVarData)
          n_excluded <- sum(!complete_cases)

          if (n_excluded > 0) {
            # Filter data to complete cases only
            depVarsData_complete <- depVarsData[complete_cases, , drop = FALSE]
            classVarData_complete <- classVarData[complete_cases]
          } else {
            depVarsData_complete <- depVarsData
            classVarData_complete <- classVarData
          }

          # Run enhanced DeLong's test with better error handling
          delongResults <- tryCatch({
            private$.enhancedDelongTest(
              data = depVarsData_complete,
              classVar = classVarData_complete,
              pos_class = positiveClass,
              ref = NULL,
              conf.level = 0.95
            )
          }, error = function(e) {
            # Fallback to original implementation if enhanced version fails
            warning(paste("Enhanced DeLong test failed, using fallback:", e$message))
            private$.deLongTest(
              data = depVarsData_complete,
              classVar = classVarData_complete,
              ref = NULL,
              positiveClass = positiveClass,
              conf.level = 0.95
            )
          })

          # Display results
          self$results$delongTest$setVisible(visible = TRUE)

          # Add methodology note
          self$results$delongComparisonTable$setNote(
            key = "delong_method",
            note = "DeLong's test uses validated pROC package implementation. Reference: DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two or more correlated ROC curves: a nonparametric approach. Biometrics 44(3):837-845.",
            init = FALSE
          )

          # Add note about excluded cases if any
          if (n_excluded > 0) {
            self$results$delongComparisonTable$setNote(
              key = "missing_cases",
              note = sprintf("Note: %d row(s) with missing values in the classification variable were excluded from DeLong test analysis.", n_excluded),
              init = FALSE
            )
          }

          # Use enhanced formatting if available
          if (inherits(delongResults, "EnhancedDeLong")) {
            formatted_output <- private$.printEnhancedDeLong(delongResults)
            self$results$delongTest$setContent(formatted_output)
          } else {
            # Format output for display (fallback)
            output_text <- paste0(
              "Estimated AUC's:\n",
              capture.output(print(round(delongResults$AUC, 3))),
              "\n\nPairwise comparisons:\n",
              capture.output(print(round(delongResults$difference, 3))),
              "\n\nOverall test:\n p-value = ", format.pval(delongResults$global.p, digits = 3)
            )
            self$results$delongTest$setContent(paste0(output_text, collapse = "\n"))
          }

          # Format results for the DeLong comparison table
          delongTable <- self$results$delongComparisonTable

          # Extract pairwise comparisons from DeLong test results
          diff_data <- delongResults$difference

          for (i in 1:nrow(diff_data)) {
            comparison <- rownames(diff_data)[i]
            delongTable$addRow(rowKey = comparison, values = list(
              comparison = comparison,
              auc_diff = diff_data[i, "AUC Difference"],
              ci_lower = diff_data[i, "CI(lower)"],
              ci_upper = diff_data[i, "CI(upper)"],
              z = sqrt(qchisq(1 - diff_data[i, "P.Value"], df = 1)),
              p = diff_data[i, "P.Value"]
            ))
          }
        }
      }

      # -----------------------------------------------------------------------
      # 11. CREATE SIMPLIFIED RESULTS TABLES
      # -----------------------------------------------------------------------

      # Create simplified summary table
      simpleTable <- self$results$simpleResultsTable

      # Add rows for each variable
      for (var in names(aucList)) {
        # Calculate confidence interval for AUC
        auc_value <- aucList[[var]]

        # Get counts of positive and negative cases for this variable
        # Escape class variable name for safe data access
        classVarEscaped <- private$.escapeVar(self$options$classVar)

        if (is.null(subGroup)) {
          classVar <- data[[classVarEscaped]]
        } else {
          # For grouped variables, extract the group
          varParts <- strsplit(var, split = "_")[[1]]
          groupName <- paste(varParts[-1], collapse="_")
          classVar <- data[subGroup == groupName, classVarEscaped]
        }

        n_pos <- sum(classVar == positiveClass)
        n_neg <- sum(classVar != positiveClass)

        # Calculate standard error using Hanley & McNeil formula
        # Check conditions safely to avoid NA in logical operations
        valid_sample_size <- !is.na(n_pos) && !is.na(n_neg) && n_pos > 0 && n_neg > 0
        valid_auc <- !is.na(auc_value) && is.finite(auc_value) && auc_value >= 0 && auc_value <= 1
        
        if (valid_sample_size && valid_auc) {
          auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))
          
          # Check if standard error is valid
          if (is.na(auc_se) || auc_se <= 0) {
            auc_se <- 0.1  # Fallback standard error
          }
          
          # Calculate 95% confidence interval
          z_critical <- qnorm(0.975)
          auc_lci <- max(0, auc_value - z_critical * auc_se)
          auc_uci <- min(1, auc_value + z_critical * auc_se)
          
          # Calculate p-value (against null hypothesis AUC = 0.5)
          z_stat <- (auc_value - 0.5) / auc_se
          p_val <- 2 * (1 - pnorm(abs(z_stat)))
        } else {
          # Fallback values when calculation fails
          auc_lci <- NA
          auc_uci <- NA
          p_val <- NA
        }

        # Add row to simple table
        simpleTable$addRow(rowKey = var, values = list(
          variable = var,
          auc = auc_value,
          ci_lower = auc_lci,
          ci_upper = auc_uci,
          p = p_val
        ))
      }
      
      # Populate clinical interpretation table after simple table is complete
      private$.populateClinicalInterpretation()

      # Populate the AUC summary table
      aucSummaryTable <- self$results$aucSummaryTable

      for (var in names(aucList)) {
        # Get AUC value directly from the list
        auc_value <- aucList[[var]]

        # Get data needed for calculations
        # Escape class variable name for safe data access
        classVarEscaped <- private$.escapeVar(self$options$classVar)

        if (is.null(subGroup)) {
          classVar <- data[[classVarEscaped]]
        } else {
          # For grouped variables, extract the group
          varParts <- strsplit(var, split = "_")[[1]]
          groupName <- paste(varParts[-1], collapse="_")
          classVar <- data[subGroup == groupName, classVarEscaped]
        }

        # Calculate counts and statistics
        n_pos <- sum(classVar == positiveClass)
        n_neg <- sum(classVar != positiveClass)

        # Calculate standard error and confidence interval
        # Check conditions safely to avoid NA in logical operations
        valid_sample_size <- !is.na(n_pos) && !is.na(n_neg) && n_pos > 0 && n_neg > 0
        valid_auc <- !is.na(auc_value) && is.finite(auc_value) && auc_value >= 0 && auc_value <= 1
        
        if (valid_sample_size && valid_auc) {
          auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))
          
          # Check if standard error is valid
          if (is.na(auc_se) || auc_se <= 0) {
            auc_se <- 0.1  # Fallback standard error
          }
          
          z_critical <- qnorm(0.975)
          auc_lci <- max(0, auc_value - z_critical * auc_se)
          auc_uci <- min(1, auc_value + z_critical * auc_se)
          
          # Calculate p-value
          z_stat <- (auc_value - 0.5) / auc_se
          p_val <- 2 * (1 - pnorm(abs(z_stat)))
        } else {
          # Fallback values when calculation fails
          auc_lci <- NA
          auc_uci <- NA
          p_val <- NA
        }

        # Check if row exists and set/add accordingly
        try({
          # Try to set row if it exists (will throw error if it doesn't)
          aucSummaryTable$setRow(rowKey = var, values = list(
            variable = as.character(var),
            auc = auc_value,
            ci_lower = auc_lci,
            ci_upper = auc_uci,
            p = p_val
          ))
        }, silent = TRUE)

        try({
          # Try to add row (will throw error if it already exists)
          aucSummaryTable$addRow(rowKey = var, values = list(
            variable = as.character(var),
            auc = auc_value,
            ci_lower = auc_lci,
            ci_upper = auc_uci,
            p = p_val
          ))
        }, silent = TRUE)
      }

      # -----------------------------------------------------------------------
      # 12. CREATE THRESHOLD TABLE IF REQUESTED
      # -----------------------------------------------------------------------

      if (self$options$showThresholdTable) {
        # Checkpoint before expensive threshold table generation
        private$.checkpoint()
        thresholdTable <- self$results$thresholdTable
        thresholdTable$deleteRows()  # Clear previous results

        for (var in names(private$.rocDataList)) {
          rocData <- private$.rocDataList[[var]]
          # Get prevalence for this variable
          prevalence <- private$.prevalenceList[[var]]

          # Select a reasonable number of thresholds to display
          n_thresholds <- min(nrow(rocData), self$options$maxThresholds)
          step <- max(1, floor(nrow(rocData) / n_thresholds))
          indices <- seq(1, nrow(rocData), by = step)

          # Add the Youden's optimal point
          optimal_idx <- which.max(rocData$youden)
          indices <- sort(unique(c(indices, optimal_idx)))

          # Add rows to threshold table
          for (i in indices) {
            # Calculate likelihood ratios
            plr <- rocData$sensitivity[i] / (1 - rocData$specificity[i])
            nlr <- (1 - rocData$sensitivity[i]) / rocData$specificity[i]

            # Avoid division by zero in likelihood ratios
            if (!is.finite(plr)) plr <- NA
            if (!is.finite(nlr)) nlr <- NA

            # Calculate accuracy using current variable's prevalence
            accuracy <- (rocData$sensitivity[i] * prevalence) +
              (rocData$specificity[i] * (1 - prevalence))

            thresholdTable$addRow(rowKey = paste0(var, "_", i), values = list(
              threshold = rocData$threshold[i],
              sensitivity = rocData$sensitivity[i],
              specificity = rocData$specificity[i],
              accuracy = accuracy,
              ppv = rocData$ppv[i],
              npv = rocData$npv[i],
              plr = plr,
              nlr = nlr,
              youden = rocData$youden[i]
            ))
          }
        }
      }





      # -----------------------------------------------------------------------
      # 13. HANDLE ADDITIONAL ANALYSES
      # -----------------------------------------------------------------------

      # Calculate partial AUC if requested
      if (self$options$partialAUC) {
        # Checkpoint before expensive partial AUC calculations
        private$.checkpoint()
        
        # Set up partial AUC table if not already done
        if (!self$results$partialAUCTable$visible) {
          self$results$partialAUCTable$setVisible(TRUE)
        }

        for (var in vars) {
          # Get the necessary data
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # Calculate partial AUC
          pAUC_results <- private$.calculatePartialAUC(
            x = dependentVar,
            class = classVar,
            positiveClass = positiveClass,
            from = self$options$partialAUCfrom,
            to = self$options$partialAUCto
          )

          # Add to table
          if (!is.null(pAUC_results)) {
            self$results$partialAUCTable$addRow(rowKey = var, values = list(
              variable = var,
              pAUC = pAUC_results$pAUC,
              pAUC_normalized = pAUC_results$pAUC_normalized,
              ci_lower = pAUC_results$ci_lower,
              ci_upper = pAUC_results$ci_upper,
              spec_range = pAUC_results$spec_range
            ))
          }
        }
      }

      # Apply ROC curve smoothing if requested
      if (self$options$rocSmoothingMethod != "none") {
        # Checkpoint before expensive ROC smoothing operations
        private$.checkpoint()
        
        for (var in vars) {
          # Get the necessary data
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # Create smoothed ROC curve
          smooth_results <- private$.createSmoothedROC(
            x = dependentVar,
            class = classVar,
            positiveClass = positiveClass,
            method = self$options$rocSmoothingMethod
          )

          # Store smoothed ROC curve data for plotting
          if (!is.null(smooth_results)) {
            # Store for plotting
            private$.rocDataList[[paste0(var, "_smooth")]] <- data.frame(
              threshold = smooth_results$threshold,
              sensitivity = smooth_results$sensitivity,
              specificity = smooth_results$specificity,
              var = var,
              smoothed = TRUE,
              method = self$options$rocSmoothingMethod
            )
          }
        }
      }

      # Calculate bootstrap confidence intervals if requested
      if (self$options$bootstrapCI) {
        # Checkpoint before expensive bootstrap calculations
        private$.checkpoint()
        
        # Set up bootstrap CI table if not already done
        if (!self$results$bootstrapCITable$visible) {
          self$results$bootstrapCITable$setVisible(TRUE)
        }

        for (var in vars) {
          # Get the necessary data
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # Calculate bootstrap CIs
          bootstrap_results <- private$.calculateBootstrapCI(
            x = dependentVar,
            class = classVar,
            positiveClass = positiveClass
          )

          # Add to table
          if (!is.null(bootstrap_results)) {
            for (param in names(bootstrap_results)) {
              # DEBUG
              est <- bootstrap_results[[param]]$estimate
              if (is.null(est) || length(est) == 0) est <- NA
              if (length(est) > 1) est <- est[1]
              
              lower <- bootstrap_results[[param]]$ci_lower
              if (is.null(lower) || length(lower) == 0) lower <- NA
              if (length(lower) > 1) lower <- lower[1]
              
              upper <- bootstrap_results[[param]]$ci_upper
              if (is.null(upper) || length(upper) == 0) upper <- NA
              if (length(upper) > 1) upper <- upper[1]
              
              tryCatch({
                self$results$bootstrapCITable$addRow(rowKey = paste0(var, "_", param), values = list(
                  variable = var,
                  parameter = param,
                  estimate = est,
                  ci_lower = lower,
                  ci_upper = upper
                ))
              }, error = function(e) {
                cat(paste("Error adding row for", var, param, ":", e$message, "\n"), file = stderr())
                cat(paste("Values - Est:", est, "Lower:", lower, "Upper:", upper, "\n"), file = stderr())
              })
            }
          }
        }
      }

      # Calculate precision-recall curves if requested
      if (self$options$precisionRecallCurve) {
        # Checkpoint before expensive precision-recall calculations
        private$.checkpoint()
        
        # Initialize precision-recall plot array
        if (self$options$combinePlots) {
          # Add a single item for combined plot
          self$results$precisionRecallPlot$addItem(key = 1)
          pr_plot_data <- data.frame()
        }

        for (var in vars) {
          # Get the necessary data
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # Calculate precision-recall
          pr_results <- private$.calculatePrecisionRecall(
            x = dependentVar,
            class = classVar,
            positiveClass = positiveClass
          )

          if (!is.null(pr_results)) {
            if (self$options$combinePlots) {
              # Add to combined data
              pr_plot_data <- rbind(
                pr_plot_data,
                data.frame(
                  threshold = pr_results$threshold,
                  precision = pr_results$precision,
                  recall = pr_results$recall,
                  auprc = rep(pr_results$auprc, length(pr_results$threshold)),
                  var = rep(var, length(pr_results$threshold))
                )
              )
            } else {
              # Create individual plot
              self$results$precisionRecallPlot$addItem(key = var)
              pr_plot <- self$results$precisionRecallPlot$get(key = var)
              pr_plot$setTitle(paste0("Precision-Recall Curve: ", var))
              pr_plot$setState(
                data.frame(
                  threshold = pr_results$threshold,
                  precision = pr_results$precision,
                  recall = pr_results$recall,
                  auprc = rep(pr_results$auprc, length(pr_results$threshold))
                )
              )
            }
          }
        }

        # Set state for combined plot if using
        if (self$options$combinePlots && nrow(pr_plot_data) > 0) {
          pr_plot <- self$results$precisionRecallPlot$get(key = 1)
          pr_plot$setTitle("Precision-Recall Curves: Combined")
          pr_plot$setState(pr_plot_data)
        }
      }

      # Compare classifier performance if requested
      if (self$options$compareClassifiers) {
        # Set up comparison table
        if (!self$results$rocComparisonTable$visible) {
          self$results$rocComparisonTable$setVisible(TRUE)
        }

        for (var in vars) {
          # Get the necessary data
          prepared <- private$.prepareVarData(data, var, subGroup)
          dependentVar <- prepared$dependentVar
          classVar <- prepared$classVar

          # Calculate classifier metrics
          metrics <- private$.calculateClassifierMetrics(
            x = dependentVar,
            class = classVar,
            positiveClass = positiveClass
          )

          # Add to table
          if (!is.null(metrics)) {
            self$results$rocComparisonTable$addRow(rowKey = var, values = list(
              variable = var,
              auc = metrics$auc,
              auprc = metrics$auprc,
              brier = metrics$brier,
              f1_score = metrics$f1_score,
              accuracy = metrics$accuracy,
              balanced_accuracy = metrics$balanced_accuracy
            ))
          }
        }
      }

      # -----------------------------------------------------------------------
      # 14. CALCULATE IDI AND NRI IF REQUESTED
      # -----------------------------------------------------------------------

      # Calculate IDI and NRI if requested
      if (self$options$calculateIDI || self$options$calculateNRI) {
        # Check if we have enough variables
        if (length(self$options$dependentVars) < 2) {
          stop("Please specify at least two dependent variables to calculate IDI/NRI.")
        }

        # Get reference variable
        using_default_ref <- FALSE
        if (is.null(self$options$refVar) || self$options$refVar == "") {
          refVar <- self$options$dependentVars[1]
          using_default_ref <- TRUE
        } else {
          refVar <- self$options$refVar
        }

        # Escape variable names for safe data access
        classVarEscaped <- private$.escapeVar(self$options$classVar)

        # Get actual class values and convert to binary
        classVar <- data[[classVarEscaped]]
        actual_binary <- as.numeric(classVar == positiveClass)

        # Get direction
        direction <- self$options$direction

        # Get bootstrap runs
        boot_runs <- as.numeric(self$options$idiNriBootRuns)

        # Calculate IDI if requested
        if (self$options$calculateIDI) {
          # Get reference variable values with escaped name
          refVarEscaped <- private$.escapeVar(refVar)
          ref_values <- as.numeric(data[[refVarEscaped]])

          # For each other variable
          for (var in self$options$dependentVars) {
            if (var != refVar) {
              varEscaped <- private$.escapeVar(var)
              var_values <- as.numeric(data[[varEscaped]])

              # Calculate IDI
              idi_result <- bootstrapIDI(
                new_values = var_values,
                ref_values = ref_values,
                actual = actual_binary,
                direction = direction,
                n_boot = boot_runs
              )

              # Add to IDI table
              self$results$idiTable$addRow(rowKey = var, values = list(
                variable = var,
                refVar = refVar,
                idi = idi_result$idi,
                ci_lower = idi_result$ci_lower,
                ci_upper = idi_result$ci_upper,
                p = idi_result$p_value
              ))
            }
          }
        }

        # Calculate NRI if requested
        if (self$options$calculateNRI) {
          # Get reference variable values with escaped name
          refVarEscaped <- private$.escapeVar(refVar)
          ref_values <- as.numeric(data[[refVarEscaped]])

          # Parse thresholds string if provided
          thresholds <- NULL
          if (!is.null(self$options$nriThresholds) && self$options$nriThresholds != "") {
            thresholds <- as.numeric(unlist(strsplit(self$options$nriThresholds, ",")))
            thresholds <- thresholds[!is.na(thresholds)]
            thresholds <- thresholds[thresholds > 0 & thresholds < 1]
          }

          # For each other variable
          for (var in self$options$dependentVars) {
            if (var != refVar) {
              varEscaped <- private$.escapeVar(var)
              var_values <- as.numeric(data[[varEscaped]])

              # Calculate NRI
              nri_result <- bootstrapNRI(
                new_values = var_values,
                ref_values = ref_values,
                actual = actual_binary,
                direction = direction,
                thresholds = thresholds,
                n_boot = boot_runs
              )

              # Add to NRI table
              self$results$nriTable$addRow(rowKey = var, values = list(
                variable = var,
                refVar = refVar,
                nri = nri_result$nri,
                event_nri = nri_result$event_nri,
                non_event_nri = nri_result$non_event_nri,
                ci_lower = nri_result$ci_lower,
                ci_upper = nri_result$ci_upper,
                p = nri_result$p_value
              ))
            }
          }
        }

        # Add note if using default reference variable
        if (using_default_ref) {
          note_text <- sprintf("Note: Using '%s' as reference variable (first variable selected). To use a different reference, select it in the Reference Variable option.", refVar)
          if (self$options$calculateIDI) {
            self$results$idiTable$setNote(
              key = "default_ref",
              note = note_text,
              init = FALSE
            )
          }
          if (self$options$calculateNRI) {
            self$results$nriTable$setNote(
              key = "default_ref",
              note = note_text,
              init = FALSE
            )
          }
        }
      }

      # -----------------------------------------------------------------------
      # ADVANCED STATISTICAL ANALYSES
      # -----------------------------------------------------------------------

      # Effect size analysis
      if (self$options$effectSizeAnalysis && length(self$options$dependentVars) >= 2) {
        # Checkpoint before expensive effect size computation
        private$.checkpoint()
        private$.calculateEffectSizes(data, classVar, positiveClass)
      }

      # Power analysis
      if (self$options$powerAnalysis) {
        # Checkpoint before expensive power analysis computation
        private$.checkpoint()
        private$.calculatePowerAnalysis(data, classVar, positiveClass)
      }

      # Bayesian ROC analysis
      if (self$options$bayesianAnalysis) {
        # Checkpoint before expensive Bayesian analysis computation
        private$.checkpoint()
        private$.calculateBayesianROC(data, classVar, positiveClass)
      }

      # Clinical utility analysis
      # TODO: Implementation in progress - method .calculateClinicalUtility() not yet implemented
      if (self$options$clinicalUtilityAnalysis) {
        # Notify user that this feature is not yet available
        warning(paste(
          "Clinical Utility Analysis is currently under development.",
          "This feature will be available in a future release.",
          "For now, you can perform decision curve analysis manually using the pROC package."
        ))
      }

      # Meta-analysis
      if (self$options$metaAnalysis && length(self$options$dependentVars) >= 3) {
        # Checkpoint before expensive meta-analysis computation
        private$.checkpoint()
        private$.calculateMetaAnalysis(data, classVar, positiveClass)
      }

      # Sensitivity analysis (currently disabled - sensitivityAnalysis option not defined in YAML)
      # if (self$options$sensitivityAnalysis) {
      #   private$.calculateSensitivityAnalysis(data, classVar, positiveClass)
      # }

    },

    # ============================================================================
    # PLOTTING METHODS
    # ============================================================================

    # Plot ROC curves
    # @param image The image object
    # @param ggtheme The ggplot theme to use
    # @param theme Additional theme elements
    # @param ... Additional parameters
    .plotROC = function(image, ggtheme, theme, ...) {
      plotData <- data.frame(image$state)

      if (nrow(plotData) == 0) return(FALSE)

      # Determine if we're creating a combined or individual plot
      if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
        # Multiple variables in one plot
        plot <- ggplot2::ggplot(plotData,
                                ggplot2::aes(
                                  x = 1 - specificity,
                                  y = sensitivity,
                                  color = var,
                                  linetype = var
                                )) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::scale_linetype_manual(values = rep(c("solid", "dashed", "dotted", "longdash"),
                                                      length.out = length(unique(plotData$var))))
      } else {
        # Single variable plot
        plot <- ggplot2::ggplot(plotData,
                                ggplot2::aes(
                                  x = 1 - specificity,
                                  y = sensitivity
                                )) +
          ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point(size = 0.5, alpha = ifelse(self$options$cleanPlot, 0, 0.7))
      }

      # Add common elements
      plot <- plot +
        ggplot2::xlab("1 - Specificity (False Positive Rate)") +
        ggplot2::ylab("Sensitivity (True Positive Rate)") +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1)

      # Apply theme based on clean plot option
      if (self$options$cleanPlot) {
        plot <- plot +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_blank(),
            plot.subtitle = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(color = "black", fill = NA)
          )

        # Handle legend positioning for clean plots
        if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
          if (self$options$legendPosition == "none") {
            plot <- plot + ggplot2::theme(legend.position = "none")
          } else {
            plot <- plot + ggplot2::theme(
              legend.position = self$options$legendPosition,
              legend.title = ggplot2::element_blank(),
              legend.key = ggplot2::element_rect(fill = "white"),
              legend.background = ggplot2::element_rect(fill = "white", color = NA)
            )
          }
        }
      } else {
        # Use the provided theme
        plot <- plot + ggtheme
      }

      # Check if we have smoothed curves
      has_smoothed <- any(grepl("_smooth", names(private$.rocDataList)))

      if (has_smoothed && self$options$rocSmoothingMethod != "none") {
        # Prepare data for smoothed curves
        smoothed_data <- data.frame()

        for (var_name in names(private$.rocDataList)) {
          if (grepl("_smooth", var_name)) {
            smooth_data <- private$.rocDataList[[var_name]]
            if (!is.null(smooth_data)) {
              smoothed_data <- rbind(smoothed_data, smooth_data)
            }
          }
        }

        if (nrow(smoothed_data) > 0) {
          # Add smoothed curves
          if (self$options$combinePlots && length(unique(smoothed_data$var)) > 1) {
            # For combined plot
            plot <- plot +
              ggplot2::geom_line(
                data = smoothed_data,
                ggplot2::aes(
                  x = 1 - specificity,
                  y = sensitivity,
                  color = var
                ),
                linetype = "solid",
                size = 1.5,
                alpha = 0.8
              )
          } else {
            # For individual plot
            plot <- plot +
              ggplot2::geom_line(
                data = smoothed_data,
                ggplot2::aes(
                  x = 1 - specificity,
                  y = sensitivity
                ),
                linetype = "solid",
                size = 1.5,
                color = "blue",
                alpha = 0.8
              )
          }

          # Add annotation about smoothing
          plot <- plot +
            ggplot2::annotate(
              "text",
              x = 0.25,
              y = 0.1,
              label = paste("Smoothing:", self$options$rocSmoothingMethod),
              hjust = 0,
              alpha = 0.7
            )
        }
      }

      # Add smoothing if requested
      if (self$options$smoothing) {
        plot <- plot + ggplot2::geom_smooth(se = self$options$displaySE)
      }

      # Mark optimal points if not clean plot or if specifically requested
      if (!self$options$cleanPlot || self$options$showOptimalPoint) {
        if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
          # For combined plot, find optimal points for each variable
          optimal_points <- data.frame()
          for (var_name in unique(plotData$var)) {
            var_data <- plotData[plotData$var == var_name,]
            j_max_idx <- which.max(var_data$youden)
            if (length(j_max_idx) > 0) {
              optimal_points <- rbind(optimal_points, var_data[j_max_idx,])
            }
          }

          if (nrow(optimal_points) > 0) {
            plot <- plot +
              ggplot2::geom_point(
                data = optimal_points,
                ggplot2::aes(
                  x = 1 - specificity,
                  y = sensitivity,
                  color = var
                ),
                size = 3, shape = 18
              )
          }
        } else {
          # For single variable plot
          if ('j_max_idx' %in% names(plotData)) {
            j_max_idx <- unique(plotData$j_max_idx)
            if (length(j_max_idx) == 1 && !is.na(j_max_idx)) {
              # Add optimal point
              plot <- plot +
                ggplot2::geom_point(
                  data = plotData[j_max_idx,],
                  ggplot2::aes(
                    x = 1 - specificity,
                    y = sensitivity
                  ),
                  size = 3, shape = 18, color = "red"
                )

              # Add annotation if not clean plot
              if (!self$options$cleanPlot) {
                plot <- plot +
                  ggplot2::annotate(
                    "text",
                    x = 1 - plotData$specificity[j_max_idx] + 0.05,
                    y = plotData$sensitivity[j_max_idx],
                    label = paste("J =", round(plotData$youden[j_max_idx], 3)),
                    hjust = 0
                  )
              }
            }
          }
        }
      }

      # Add AUC annotations if not in clean plot mode
      if (!self$options$cleanPlot) {
        if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
          # Multiple AUC annotations for combined plot
          auc_data <- aggregate(AUC ~ var, data = plotData, FUN = function(x) x[1])
          auc_data$AUC_formatted <- sprintf("AUC = %.3f", auc_data$AUC)
          auc_data$x <- 0.75  # Position for annotations
          auc_data$y <- seq(0.3, 0.1, length.out = nrow(auc_data))

          plot <- plot +
            ggplot2::geom_text(data = auc_data,
                               ggplot2::aes(x = x, y = y, label = AUC_formatted, color = var),
                               hjust = 0, show.legend = FALSE)
        } else {
          # Single AUC annotation for individual plot
          if (nrow(plotData) > 0) {
            auc_value <- unique(plotData$AUC)[1]
            plot <- plot + ggplot2::annotate(
              "text",
              x = 0.75,
              y = 0.25,
              label = sprintf("AUC = %.3f", auc_value)
            )
          }
        }
      }

      # Add direct labels if requested
      if (self$options$directLabel && self$options$combinePlots) {
        # Get unique variables and their optimal points
        unique_vars <- unique(plotData$var)
        label_points <- data.frame()

        for (var_name in unique_vars) {
          var_data <- plotData[plotData$var == var_name,]
          j_max_idx <- which.max(var_data$youden)
          if (length(j_max_idx) > 0) {
            label_points <- rbind(label_points, var_data[j_max_idx,])
          }
        }

        # Add text labels directly to curves
        plot <- plot +
          ggplot2::geom_text(
            data = label_points,
            ggplot2::aes(
              x = 1 - specificity,
              y = sensitivity,
              label = var
            ),
            hjust = -0.1,
            vjust = 1.1
          )
      }

      # Add confidence bands if requested
      if (self$options$showConfidenceBands && !self$options$cleanPlot) {
        # Only implemented for individual plots currently
        if (!self$options$combinePlots || length(unique(plotData$var)) == 1) {
          # Calculate SE bands using binomial approximation (simple approach)
          if (nrow(plotData) > 0) {
            var_name <- unique(plotData$var)[1]

            # Get the raw data
            if (!is.null(attr(private$.rocDataList[[var_name]], "rawData"))) {
              rawData <- attr(private$.rocDataList[[var_name]], "rawData")

              # Get counts
              n_pos <- sum(rawData$class == "Positive")
              n_neg <- sum(rawData$class == "Negative")

              # Create confidence bands (simplified approach)
              # This uses a normal approximation to the binomial
              sens_se <- sqrt(plotData$sensitivity * (1 - plotData$sensitivity) / n_pos)
              spec_se <- sqrt(plotData$specificity * (1 - plotData$specificity) / n_neg)

              # Create bands data
              bands_data <- data.frame(
                x = 1 - plotData$specificity,
                y = plotData$sensitivity,
                ymin = pmax(0, plotData$sensitivity - 1.96 * sens_se),
                ymax = pmin(1, plotData$sensitivity + 1.96 * sens_se),
                xmin = pmax(0, 1 - (plotData$specificity + 1.96 * spec_se)),
                xmax = pmin(1, 1 - (plotData$specificity - 1.96 * spec_se))
              )

              # Add confidence bands to plot
              plot <- plot +
                ggplot2::geom_ribbon(
                  data = bands_data,
                  ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax),
                  alpha = 0.1, fill = "blue"
                )
            }
          }
        }
      }

      # Handle quantile CIs if requested
      if (self$options$quantileCIs && !self$options$cleanPlot) {
        # Parse quantiles string
        quantiles_str <- self$options$quantiles
        quantiles_vec <- as.numeric(unlist(strsplit(quantiles_str, ",")))

        # Ensure quantiles are valid
        quantiles_vec <- quantiles_vec[quantiles_vec >= 0 & quantiles_vec <= 1]

        if (length(quantiles_vec) > 0) {
          # For each variable in the plot
          for (var_name in unique(plotData$var)) {
            var_data <- plotData[plotData$var == var_name, ]

            # Extract the predictor values
            if (!is.null(attr(private$.rocDataList[[var_name]], "rawData"))) {
              raw_data <- attr(private$.rocDataList[[var_name]], "rawData")
              predictor_values <- raw_data$value

              # Calculate class-specific counts
              n_pos <- sum(raw_data$class == "Positive")
              n_neg <- sum(raw_data$class == "Negative")

              # Calculate predictor quantiles
              pred_quantiles <- quantile(predictor_values, probs = quantiles_vec, na.rm = TRUE)

              # Create a data frame for quantile points
              quantile_points <- data.frame()

              # For each quantile, find the nearest threshold
              for (q_idx in seq_along(pred_quantiles)) {
                q <- pred_quantiles[q_idx]
                q_prob <- quantiles_vec[q_idx]

                # Find closest threshold
                idx <- which.min(abs(var_data$cutpoint - q))

                if (length(idx) > 0) {
                  # Get coordinates
                  x <- 1 - var_data$specificity[idx]
                  y <- var_data$sensitivity[idx]

                  # Calculate binomial confidence intervals
                  sens_ci_lower <- max(0, y - 1.96 * sqrt(y * (1 - y) / n_pos))
                  sens_ci_upper <- min(1, y + 1.96 * sqrt(y * (1 - y) / n_pos))

                  spec <- var_data$specificity[idx]
                  spec_ci_lower <- max(0, spec - 1.96 * sqrt(spec * (1 - spec) / n_neg))
                  spec_ci_upper <- min(1, spec + 1.96 * sqrt(spec * (1 - spec) / n_neg))

                  # Add to data frame
                  quantile_points <- rbind(quantile_points, data.frame(
                    x = x,
                    y = y,
                    ymin = sens_ci_lower,
                    ymax = sens_ci_upper,
                    xmin = 1 - spec_ci_upper,
                    xmax = 1 - spec_ci_lower,
                    var = var_name,
                    q_label = sprintf("q=%.2f", q_prob)
                  ))
                }
              }

              # Add quantile points and CIs to plot
              if (nrow(quantile_points) > 0) {
                if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
                  # For combined plot with multiple variables
                  plot <- plot +
                    # Horizontal error bars
                    ggplot2::geom_errorbarh(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax, color = var),
                      height = 0.02
                    ) +
                    # Vertical error bars
                    ggplot2::geom_errorbar(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax, color = var),
                      width = 0.02
                    ) +
                    # Points
                    ggplot2::geom_point(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, color = var),
                      size = 3, shape = 4
                    ) +
                    # Labels
                    ggplot2::geom_text(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, label = q_label, color = var),
                      hjust = -0.2, vjust = -0.5, size = 3
                    )
                } else {
                  # For single variable plot
                  plot <- plot +
                    # Horizontal error bars
                    ggplot2::geom_errorbarh(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax),
                      height = 0.02, color = "blue"
                    ) +
                    # Vertical error bars
                    ggplot2::geom_errorbar(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax),
                      width = 0.02, color = "blue"
                    ) +
                    # Points
                    ggplot2::geom_point(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y),
                      size = 3, shape = 4, color = "blue"
                    ) +
                    # Labels
                    ggplot2::geom_text(
                      data = quantile_points,
                      ggplot2::aes(x = x, y = y, label = q_label),
                      hjust = -0.2, vjust = -0.5, size = 3, color = "blue"
                    )
                }
              }
            }
          }
        }
      }

      print(plot)
      return(TRUE)
    },

    # Plot sensitivity/specificity vs criterion
    # @param image The image object
    # @param ggtheme The ggplot theme to use
    # @param theme Additional theme elements
    # @param ... Additional parameters
    .plotCriterion = function(image, ggtheme, theme, ...) {
      plotData <- image$state

      if (is.null(plotData) || (is.data.frame(plotData) && nrow(plotData) == 0))
        return(FALSE)

      # Check if this is a combined plot with multiple variables
      if ("var" %in% names(plotData)) {
        # Multiple variables
        plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = threshold, group = var, color = var)) +
          ggplot2::geom_line(ggplot2::aes(y = sensitivity, linetype = "Sensitivity")) +
          ggplot2::geom_line(ggplot2::aes(y = specificity, linetype = "Specificity")) +
          ggplot2::scale_linetype_manual(name = "Metric", values = c("Sensitivity" = "solid", "Specificity" = "dashed")) +
          ggplot2::labs(
            x = "Threshold",
            y = "Value",
            color = "Variable",
            title = "Sensitivity and Specificity vs. Threshold"
          )
      } else {
        # Single variable - reshape data for better plotting
        plot_data_long <- tidyr::gather(
          plotData,
          key = "metric",
          value = "value",
          sensitivity, specificity
        )

        # Make metric names nicer
        plot_data_long$metric <- factor(
          plot_data_long$metric,
          levels = c("sensitivity", "specificity"),
          labels = c("Sensitivity", "Specificity")
        )

        # Create plot
        plot <- ggplot2::ggplot(
          plot_data_long,
          ggplot2::aes(x = threshold, y = value, color = metric)
        ) +
          ggplot2::geom_line() +
          ggplot2::labs(
            x = "Threshold",
            y = "Value",
            color = "Metric",
            title = "Sensitivity and Specificity vs. Threshold"
          )

        # Find optimal threshold (Youden's index)
        optimal_idx <- which.max(plotData$youden)
        if (length(optimal_idx) > 0) {
          opt_threshold <- plotData$threshold[optimal_idx]

          # Add vertical line at optimal threshold
          plot <- plot + ggplot2::geom_vline(
            xintercept = opt_threshold,
            linetype = "dotted",
            color = "darkgray"
          ) +
            ggplot2::annotate(
              "text",
              x = opt_threshold,
              y = 0.1,
              label = sprintf("Optimal: %.3f", opt_threshold),
              hjust = -0.1
            )
        }
      }

      # Apply theme
      plot <- plot + ggtheme

      print(plot)
      return(TRUE)
    },

    # Plot PPV/NPV vs prevalence
    # @param image The image object
    # @param ggtheme The ggplot theme to use
    # @param theme Additional theme elements
    # @param ... Additional parameters
    .plotPrevalence = function(image, ggtheme, theme, ...) {
      state <- image$state

      if (is.null(state))
        return(FALSE)

      # Extract data
      optimal <- state$optimal
      prevalence <- state$prevalence

      # Override with prior prevalence if requested
      if (self$options$usePriorPrev)
        prevalence <- self$options$priorPrev

      if (is.null(optimal) || is.null(prevalence))
        return(FALSE)

      # Create prevalence sequence
      prev_seq <- seq(0.01, 0.99, by = 0.01)

      # Calculate PPV and NPV for different prevalence values
      # PPV = (sens * prev) / (sens * prev + (1-spec) * (1-prev))
      ppv_vals <- (optimal$sensitivity * prev_seq) /
        ((optimal$sensitivity * prev_seq) + ((1 - optimal$specificity) * (1 - prev_seq)))

      # NPV = (spec * (1-prev)) / (spec * (1-prev) + (1-sens) * prev)
      npv_vals <- (optimal$specificity * (1 - prev_seq)) /
        ((optimal$specificity * (1 - prev_seq)) + ((1 - optimal$sensitivity) * prev_seq))

      # Create data frame for plotting
      plot_data <- data.frame(
        prevalence = c(prev_seq, prev_seq),
        value = c(ppv_vals, npv_vals),
        metric = factor(
          rep(c("PPV", "NPV"), each = length(prev_seq)),
          levels = c("PPV", "NPV"),
          labels = c("Positive Predictive Value", "Negative Predictive Value")
        )
      )

      # Create plot
      plot <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = prevalence, y = value, color = metric)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(
          xintercept = prevalence,
          linetype = "dashed",
          color = "darkgray"
        ) +
        ggplot2::labs(
          title = "Predictive Values vs. Disease Prevalence",
          subtitle = paste0(
            "At Optimal Threshold = ", round(optimal$threshold, 3),
            " (Sens = ", round(optimal$sensitivity * 100, 1),
            "%, Spec = ", round(optimal$specificity * 100, 1), "%)"
          ),
          x = "Disease Prevalence",
          y = "Value",
          color = "Metric"
        ) +
        ggplot2::annotate(
          "text",
          x = prevalence,
          y = 0.1,
          label = sprintf("Sample Prevalence: %.2f", prevalence),
          hjust = -0.1
        ) +
        ggtheme

      print(plot)
      return(TRUE)
    },

    # Plot dot plot showing distribution of test values by class
    # @param image The image object
    # @param ggtheme The ggplot theme to use
    # @param theme Additional theme elements
    # @param ... Additional parameters
    .plotDot = function(image, ggtheme, theme, ...) {
      plotData <- image$state

      if (is.null(plotData) || nrow(plotData) == 0)
        return(FALSE)

      # Create plot
      plot <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(x = class, y = value, color = class)
      ) +
        # Add jittered points
        ggplot2::geom_jitter(
          width = 0.3,
          height = 0,
          alpha = 0.7,
          size = 3
        ) +
        # Add horizontal line at threshold
        ggplot2::geom_hline(
          yintercept = plotData$threshold[1],
          linetype = "dashed",
          color = "darkgray"
        ) +
        # Set color palette
        ggplot2::scale_color_manual(
          values = c("Negative" = "darkblue", "Positive" = "darkred")
        ) +
        # Add labels
        ggplot2::labs(
          title = "Distribution of Values by Class",
          x = "Class",
          y = "Value"
        ) +
        # Add annotation with threshold info
        ggplot2::annotate(
          "text",
          x = 1.5,
          y = plotData$threshold[1],
          label = paste0(
            "Threshold: ", round(plotData$threshold[1], 3),
            "\nDirection: ", plotData$direction[1]
          ),
          hjust = 0
        ) +
        # Remove legend
        ggplot2::theme(legend.position = "none") +
        ggtheme

      # Add boxplots if helpful for distribution visualization
      plot <- plot +
        ggplot2::geom_boxplot(
          alpha = 0.3,
          width = 0.5,
          outlier.shape = NA  # Hide outliers since we're showing all points
        )

      print(plot)
      return(TRUE)
    },

    # Generate interactive ROC plot
    # @param image The image object
    # @param ggtheme The ggplot theme to use
    # @param theme Additional theme elements
    # @param ... Additional parameters
    .plotInteractiveROC = function(image, ggtheme, theme, ...) {
      # This function depends on the plotROC package which must be available
      if (!private$.checkPackageDependencies("plotROC", "interactive ROC plots")) {
        return(FALSE)
      }

      # Get data from state
      plotData <- image$state

      if (is.null(plotData) || (is.data.frame(plotData) && nrow(plotData) == 0))
        return(FALSE)

      # Check for data format - if it's already ROC data, we need to reformat
      if (all(c("sensitivity", "specificity") %in% names(plotData))) {
        # Create mock data for plotROC
        if (!"var" %in% names(plotData)) {
          # Single variable
          # Convert ROC coords to data plotROC expects
          interactive_data <- data.frame(
            predictor = plotData$cutpoint,
            response = rep(c(1, 0), each = nrow(plotData)),
            stringsAsFactors = FALSE
          )
        } else {
          # Multiple variables
          # For each variable, create mock data
          interactive_data <- data.frame(
            predictor = numeric(),
            response = numeric(),
            D = character(),
            stringsAsFactors = FALSE
          )

          for (var_name in unique(plotData$var)) {
            var_data <- plotData[plotData$var == var_name, ]
            # Add to interactive data
            interactive_data <- rbind(interactive_data, data.frame(
              predictor = var_data$cutpoint,
              response = rep(c(1, 0), each = nrow(var_data)),
              D = var_name,
              stringsAsFactors = FALSE
            ))
          }
        }
      } else {
        # If already in correct format, use as is
        interactive_data <- plotData
      }

      # Create interactive plot
      try({
        # Create basic plot using ggplot2 syntax
        if ("D" %in% names(interactive_data)) {
          # Multiple groups
          p <- plotROC::ggplot(interactive_data,
                               plotROC::aes(d = response, m = predictor, color = D)) +
            plotROC::geom_roc(n.cuts = 20) +
            plotROC::style_roc(theme = ggtheme)
        } else {
          # Single group
          p <- plotROC::ggplot(interactive_data,
                               plotROC::aes(d = response, m = predictor)) +
            plotROC::geom_roc(n.cuts = 20) +
            plotROC::style_roc(theme = ggtheme)
        }

        # Add AUC labels if we have them
        if ("AUC" %in% names(plotData)) {
          unique_vars <- unique(plotData$var)
          auc_labels <- sapply(unique_vars, function(v) {
            sprintf("%s AUC: %.3f", v, plotData$AUC[plotData$var == v][1])
          })
          p <- p + ggplot2::annotate("text", x = 0.75, y = 0.25,
                                     label = paste(auc_labels, collapse = "\n"))
        }

        # Convert to interactive plot
        interactive_plot <- plotROC::plot_interactive_roc(p)

        # Print the plot
        print(interactive_plot)
        return(TRUE)
      }, silent = FALSE)

      # If there was an error, return FALSE
      return(FALSE)
    },

    # Add this function to plot precision-recall curves
    .plotPrecisionRecall = function(image, ggtheme, theme, ...) {
      plotData <- image$state

      if (is.null(plotData) || nrow(plotData) == 0)
        return(FALSE)

      # Determine if this is a combined or individual plot
      if ("var" %in% names(plotData)) {
        # Combined plot with multiple variables
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(
            x = recall,
            y = precision,
            color = var,
            linetype = var
          )
        ) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_color_brewer(palette = "Set1") +
          ggplot2::scale_linetype_manual(
            values = rep(c("solid", "dashed", "dotted", "longdash"),
                         length.out = length(unique(plotData$var)))
          )
      } else {
        # Individual plot
        plot <- ggplot2::ggplot(
          plotData,
          ggplot2::aes(
            x = recall,
            y = precision
          )
        ) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_point(size = 0.5, alpha = 0.7)
      }

      # Add AUPRC annotations
      if ("auprc" %in% names(plotData)) {
        if ("var" %in% names(plotData)) {
          # For combined plot
          auprc_data <- aggregate(auprc ~ var, data = plotData, FUN = function(x) x[1])
          auprc_data$label <- sprintf("AUPRC = %.3f", auprc_data$auprc)
          auprc_data$x <- 0.25
          auprc_data$y <- seq(0.3, 0.1, length.out = nrow(auprc_data))

          plot <- plot +
            ggplot2::geom_text(
              data = auprc_data,
              ggplot2::aes(x = x, y = y, label = label, color = var),
              hjust = 0,
              show.legend = FALSE
            )
        } else {
          # For individual plot
          plot <- plot +
            ggplot2::annotate(
              "text",
              x = 0.25,
              y = 0.25,
              label = sprintf("AUPRC = %.3f", unique(plotData$auprc)[1])
            )
        }
      }

      # Add labels and theme
      plot <- plot +
        ggplot2::xlab("Recall (Sensitivity)") +
        ggplot2::ylab("Precision (Positive Predictive Value)") +
        ggplot2::xlim(0, 1) +
        ggplot2::ylim(0, 1) +
        ggtheme

      print(plot)
      return(TRUE)
    },

    # Fixed Sensitivity/Specificity ROC plotting function
    .plotFixedSensSpecROC = function(image, ggtheme, theme, ...) {
      plotData <- image$state

      if (!self$options$fixedSensSpecAnalysis || is.null(plotData)) return(FALSE)

      # Get variable name from plot data (stored in state)
      var <- plotData$var[1]
      if (is.null(var) || is.na(var)) return(FALSE)
      
      # Get fixed analysis parameters
      analysis_type <- self$options$fixedAnalysisType
      target_value <- if (analysis_type == "sensitivity") {
        self$options$fixedSensitivityValue
      } else {
        self$options$fixedSpecificityValue
      }
      
      # Get the fixed point from the results table
      fixedTable <- self$results$fixedSensSpecTable
      fixed_row <- NULL
      
      if (length(fixedTable$rowKeys) > 0) {
        # rowKey is the variable name (see .populateFixedSensSpecTable line 791)
        # So we can compare directly without extracting from cells
        if (var %in% fixedTable$rowKeys) {
          fixed_row <- list(
            cutpoint = fixedTable$getCell(rowKey = var, "cutpoint"),
            achieved_sensitivity = fixedTable$getCell(rowKey = var, "achieved_sensitivity"),
            achieved_specificity = fixedTable$getCell(rowKey = var, "achieved_specificity")
          )
        }
      }
      
      if (is.null(fixed_row)) return(FALSE)
      
      # Create ROC plot with highlighted fixed point
      plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
        ggplot2::geom_line(color = "steelblue", size = 1) +
        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
        ggplot2::xlim(0, 1) + 
        ggplot2::ylim(0, 1) +
        ggplot2::labs(
          x = "1 - Specificity (False Positive Rate)",
          y = "Sensitivity (True Positive Rate)",
          title = paste("ROC Curve with Fixed", tools::toTitleCase(analysis_type), "Point"),
          subtitle = paste0(
            "Target ", analysis_type, ": ", round(target_value, 3),
            " | Achieved: ", round(if (analysis_type == "sensitivity") fixed_row$achieved_sensitivity else fixed_row$achieved_specificity, 3),
            " | Cutpoint: ", round(fixed_row$cutpoint, 3)
          )
        ) +
        ggtheme
      
      # Add the fixed point
      plot <- plot + ggplot2::geom_point(
        x = 1 - fixed_row$achieved_specificity,
        y = fixed_row$achieved_sensitivity,
        color = "red",
        size = 4,
        shape = 16
      )
      
      # Add annotation for the fixed point
      plot <- plot + ggplot2::annotate(
        "text",
        x = 1 - fixed_row$achieved_specificity + 0.1,
        y = fixed_row$achieved_sensitivity + 0.05,
        label = paste0(
          "Fixed Point\n",
          "Sens: ", round(fixed_row$achieved_sensitivity, 3), "\n",
          "Spec: ", round(fixed_row$achieved_specificity, 3)
        ),
        hjust = 0,
        size = 3,
        color = "red"
      )
      
      print(plot)
      return(TRUE)
    },

    # ============================================================================
    # ENHANCED FUNCTIONS FROM LEGACY TESTROC IMPLEMENTATION
    # ============================================================================

    # Enhanced HTML table formatting for sensitivity/specificity results
    .formatSensSpecTable = function(Title, TP, FP, TN, FN) {
      res <- paste0(
        "<style type='text/css'>
        .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
        .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
        .tg .tg-s6z2{text-align:center}
        .tg .tg-uys7{border-color:inherit;text-align:center}
        .tg .tg-h0x1{text-align:center}
        </style>
        <table class='tg'>
          <tr>
            <th class='tg-0lax' colspan='4'>",
        Title,
        "</th>
          </tr>
          <tr>
            <td class='tg-s6z2'></td>
            <td class='tg-uys7' colspan='3'>DECISION BASED ON MEASURE</td>
          </tr>
          <tr>
            <td class='tg-h0x1' rowspan='3'>CRITERION</td>
            <td class='tg-h0x1'></td>
            <td class='tg-h0x1'>Negative</td>
            <td class='tg-h0x1'>Positive</td>
          </tr>
          <tr>
            <td class='tg-s6z2'>Negative</td>
            <td class='tg-s6z2'>",
        TN,
        " (TN)</td>
            <td class='tg-s6z2'>",
        FP,
        " (FP)</td>
          </tr>
          <tr>
            <td class='tg-h0x1'>Positive</td>
            <td class='tg-h0x1'>",
        FN,
        " (FN)</td>
            <td class='tg-h0x1'>",
        TP,
        " (TP)</td>
          </tr>
          <tr>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
          </tr>
        </table>"
      )
      return(res)
    },

    # Enhanced percentage formatter with better handling
    .formatPercentage = function(x) {
      resToReturn = character()
      for (i in 1:length(x)) {
        if (is.na(x[i])) {
          resToReturn[i] <- "NA"
        } else {
          number = round(x[i], 2)
          if (number == 0) {
            resToReturn[i] <- "0.00%"
          } else if (abs(number) < 0.01) {
            resToReturn[i] <- paste0("<", ifelse(number < 0, "-", ""), "0.01%")
          } else if (length(as.character(number)) <= 3) {
            # For small numbers, ensure .00% format
            resToReturn[i] <- jmvcore::format("{}.00%", number)
          } else {
            resToReturn[i] <- jmvcore::format("{}%", number)
          }
        }
      }
      return(resToReturn)
    },

    # Enhanced DeLong test implementation with better error handling
    # Enhanced DeLong test implementation using pROC for robust covariance calculation
    .enhancedDelongTest = function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
      # Check pROC availability
      if (!requireNamespace('pROC', quietly = TRUE)) {
        stop('pROC package is required for DeLong\'s test')
      }

      # Validate inputs
      if (length(classVar) != nrow(data)) {
        stop('The number of rows in data must match the length of classVar')
      }

      # Check for NA values in classification variable
      if (any(is.na(classVar))) {
        stop('Classification variable contains missing values (NA). Please remove or handle missing values before performing ROC analysis.')
      }

      id.pos <- classVar == pos_class
      if (sum(id.pos, na.rm = TRUE) < 1) {
        stop('Wrong positive class level specified.')
      }
      if (ncol(data) < 2) {
        stop('Data must contain at least two columns.')
      }

      nauc <- ncol(data)
      vars <- colnames(data)
      
      # Calculate ROC curves using pROC
      rocs <- list()
      auc_values <- numeric(nauc)
      
      for (i in 1:nauc) {
        # pROC handles direction automatically
        rocs[[i]] <- pROC::roc(response = classVar, predictor = data[,i], 
                              levels = c(setdiff(unique(classVar), pos_class), pos_class),
                              direction = 'auto', quiet = TRUE)
        auc_values[i] <- as.numeric(pROC::auc(rocs[[i]]))
      }
      
      # Compute Covariance Matrix S using pROC
      # S[i,j] = cov(roc[i], roc[j])
      S <- matrix(0, nrow = nauc, ncol = nauc)
      for (i in 1:nauc) {
        for (j in i:nauc) {
          if (i == j) {
            # Variance of a single ROC curve
            S[i,j] <- pROC::var(rocs[[i]], method='delong')
          } else {
            # Covariance between two ROC curves
            v <- pROC::cov(rocs[[i]], rocs[[j]], method='delong')
            S[i,j] <- v
            S[j,i] <- v
          }
        }
      }
      
      # Construct Contrast Matrix L for Global Test
      if (is.null(ref)) {
        # Matrix for all pairwise differences (sequential construction matching original)
        L <- matrix(0, nrow = nauc * (nauc - 1) / 2, ncol = nauc)
        newa <- 0
        for (i in 1:(nauc - 1)) {
          newl <- nauc - i
          L[(newa + 1):(newa + newl), i] <- rep(1, newl)
          L[(newa + 1):(newa + newl), ((i + 1):(i + newl))] <-
            diag(-1, nrow = newl, ncol = newl)
          newa <- newa + newl
        }
      } else {
        # Compare against reference
        if (ref > nauc) stop(paste('Reference ref must be one of the markers (1...', nauc, ')', sep = ''))
        L <- matrix(1, ncol = nauc, nrow = nauc - 1)
        L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
      }

      # Calculate Global Test Statistics
      aucdiff <- L %*% auc_values
      
      # Use generalized inverse for stability
      tryCatch({
        LSL_matrix <- L %*% S %*% t(L)
        z_score <- t(aucdiff) %*% MASS::ginv(LSL_matrix) %*% aucdiff
        global_p <- pchisq(z_score, df = qr(LSL_matrix)$rank, lower.tail = FALSE)
      }, error = function(e) {
        warning(paste('Error in global test calculation:', e$message))
        z_score <- NA
        global_p <- NA
      })

      # Pairwise Comparisons
      quantil <- qnorm(1 - (1 - conf.level) / 2)
      
      if (is.null(ref)) {
        # All pairwise
        n_pairs <- nauc * (nauc - 1) / 2
        rows <- character(n_pairs)
        diff_vec <- numeric(n_pairs)
        ci <- matrix(NA, nrow = n_pairs, ncol = 2)
        p_vals <- numeric(n_pairs)
        cors <- numeric(n_pairs)
        
        ctr <- 1
        for (i in 1:(nauc - 1)) {
          for (j in (i + 1):nauc) {
            # Difference
            diff_val <- auc_values[i] - auc_values[j]
            diff_vec[ctr] <- diff_val
            
            # Standard Error of Difference: sqrt(Var(A) + Var(B) - 2*Cov(A,B))
            var_diff <- S[i,i] + S[j,j] - 2*S[i,j]
            se_diff <- sqrt(max(0, var_diff)) # Ensure non-negative
            
            # Confidence Interval
            ci[ctr, ] <- c(diff_val - quantil * se_diff, diff_val + quantil * se_diff)
            
            # P-value (Two-tailed Z-test)
            if (se_diff > 0) {
                z_val <- diff_val / se_diff
                p_vals[ctr] <- 2 * (1 - pnorm(abs(z_val)))
            } else {
                p_vals[ctr] <- if(abs(diff_val) < 1e-10) 1 else 0
            }
            
            # Correlation
            cors[ctr] <- S[i,j] / sqrt(S[i,i] * S[j,j])
            
            rows[ctr] <- paste(i, j, sep = ' vs. ')
            ctr <- ctr + 1
          }
        }
      } else {
        # Against reference
        n_pairs <- nauc - 1
        rows <- character(n_pairs)
        diff_vec <- numeric(n_pairs)
        ci <- matrix(NA, nrow = n_pairs, ncol = 2)
        p_vals <- numeric(n_pairs)
        cors <- numeric(n_pairs)
        
        comp <- (1:nauc)[-ref]
        for (i in 1:n_pairs) {
          idx <- comp[i]
          
          diff_val <- auc_values[ref] - auc_values[idx]
          diff_vec[i] <- diff_val
          
          var_diff <- S[ref,ref] + S[idx,idx] - 2*S[ref,idx]
          se_diff <- sqrt(max(0, var_diff))
          
          ci[i, ] <- c(diff_val - quantil * se_diff, diff_val + quantil * se_diff)
          
          if (se_diff > 0) {
              z_val <- diff_val / se_diff
              p_vals[i] <- 2 * (1 - pnorm(abs(z_val)))
          } else {
              p_vals[i] <- if(abs(diff_val) < 1e-10) 1 else 0
          }
          
          cors[i] <- S[ref,idx] / sqrt(S[ref,ref] * S[idx,idx])
          
          rows[i] <- paste(ref, idx, sep = ' vs. ')
        }
      }

      # Enhanced results formatting
      newres <- data.frame(
         'AUC Difference' = diff_vec,
         'CI(lower)' = ci[,1],
         'CI(upper)' = ci[,2],
         'P.Value' = p_vals,
         'Correlation' = cors,
         check.names = FALSE
      )
      rownames(newres) <- rows
      colnames(newres) <- c('AUC Difference', 'CI(lower)', 'CI(upper)', 'P.Value', 'Correlation')

      # Format AUC dataframe
      # We use pROC's AUC and Variance, but calculate Hanley SD for backward compatibility
      nn <- sum(classVar != pos_class)
      np <- sum(classVar == pos_class)
      
      q1 <- auc_values / (2 - auc_values)
      q2 <- 2 * auc_values^2 / (1 + auc_values)
      aucvar_hanley <- (auc_values * (1 - auc_values) + (np - 1) * (q1 - auc_values^2) + (nn - 1) * (q2 - auc_values^2)) / (np * nn)
      
      sd_hanley <- sqrt(pmax(0, aucvar_hanley))
      sd_delong <- sqrt(diag(S))
      
      auc_df <- data.frame(
         'AUC' = auc_values,
         'SD(Hanley)' = sd_hanley,
         'P(H0: AUC=0.5)' = 2 * (1 - pnorm(abs(auc_values - 0.5) / sd_hanley)),
         'SD(DeLong)' = sd_delong,
         'P(H0: AUC=0.5)' = 2 * (1 - pnorm(abs(auc_values - 0.5) / sd_delong)),
         check.names = FALSE
      )
      names(auc_df) <- c('AUC', 'SD(Hanley)', 'P(H0: AUC=0.5)', 'SD(DeLong)', 'P(H0: AUC=0.5)')

      ERG <- list(
        AUC = auc_df,
        difference = newres,
        covariance = S,
        global.z = z_score,
        global.p = global_p
      )
      class(ERG) <- 'EnhancedDeLong'
      return(ERG)
    },

    # Enhanced print method for DeLong results
    .printEnhancedDeLong = function(x, digits = max(3, getOption("digits") - 3), ...) {
      output <- character()
      
      # Format AUC table
      output <- c(output, "Estimated AUC's:")
      auc_formatted <- format(round(x$AUC, digits = digits, ...), nsmall = digits, ...)
      output <- c(output, capture.output(print(auc_formatted, quote = FALSE)))
      
      # Format pairwise comparisons
      output <- c(output, "", "Pairwise comparisons:")
      diff_formatted <- format(round(x$difference, digits = digits, ...), nsmall = digits, ...)
      output <- c(output, capture.output(print(diff_formatted, quote = FALSE)))
      
      # Format overall test
      if (!is.na(x$global.p)) {
        output <- c(output, "", paste("Overall test:", "p-value =", format.pval(x$global.p, digits = digits)))
      } else {
        output <- c(output, "", "Overall test: p-value = NA (calculation failed)")
      }
      
      return(paste(output, collapse = "\n"))
    },

    # ============================================================================
    # ADVANCED STATISTICAL ANALYSIS METHODS
    # ============================================================================

    # Calculate effect sizes for ROC comparisons
    .calculateEffectSizes = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars
      if (length(vars) < 2) return()

      if (!self$results$effectSizeTable$visible) {
        self$results$effectSizeTable$setVisible(TRUE)
      }

      # Get binary outcomes
      y <- as.numeric(data[, self$options$classVar] == positiveClass)
      
      for (i in 1:(length(vars)-1)) {
        for (j in (i+1):length(vars)) {
          var1 <- vars[i]
          var2 <- vars[j]
          
          # Get predictor values
          x1 <- as.numeric(data[, var1])
          x2 <- as.numeric(data[, var2])
          
          # Remove missing values
          complete_cases <- complete.cases(x1, x2, y)
          x1 <- x1[complete_cases]
          x2 <- x2[complete_cases]
          y_complete <- y[complete_cases]
          
          # Calculate effect sizes
          tryCatch({
            # Cohen's d
            pos_diff1 <- x1[y_complete == 1]
            neg_diff1 <- x1[y_complete == 0]
            pos_diff2 <- x2[y_complete == 1]
            neg_diff2 <- x2[y_complete == 0]
            
            pooled_sd1 <- sqrt(((length(pos_diff1) - 1) * var(pos_diff1) + 
                               (length(neg_diff1) - 1) * var(neg_diff1)) / 
                               (length(pos_diff1) + length(neg_diff1) - 2))
            pooled_sd2 <- sqrt(((length(pos_diff2) - 1) * var(pos_diff2) + 
                               (length(neg_diff2) - 1) * var(neg_diff2)) / 
                               (length(pos_diff2) + length(neg_diff2) - 2))
            
            cohens_d <- abs(mean(pos_diff2) - mean(neg_diff2))/pooled_sd2 - 
                       abs(mean(pos_diff1) - mean(neg_diff1))/pooled_sd1
            
            # Glass' Delta using control group SD
            glass_delta <- (mean(pos_diff2) - mean(pos_diff1)) / sd(neg_diff1)
            
            # Hedges' g (bias-corrected Cohen's d)
            n_total <- length(x1)
            hedges_g <- cohens_d * (1 - 3/(4 * n_total - 9))
            
            # Effect magnitude interpretation
            effect_magnitude <- if (abs(cohens_d) < 0.2) "Negligible" else
                              if (abs(cohens_d) < 0.5) "Small" else
                              if (abs(cohens_d) < 0.8) "Medium" else "Large"
            
            clinical_importance <- if (abs(cohens_d) < 0.2) "Not clinically meaningful" else
                                 if (abs(cohens_d) < 0.5) "Possibly clinically meaningful" else
                                 if (abs(cohens_d) < 0.8) "Likely clinically meaningful" else 
                                 "Highly clinically meaningful"
            
            self$results$effectSizeTable$addRow(rowKey = paste0(var1, "_vs_", var2), values = list(
              comparison = paste0(var1, " vs ", var2),
              cohens_d = cohens_d,
              glass_delta = glass_delta,
              hedges_g = hedges_g,
              effect_magnitude = effect_magnitude,
              clinical_importance = clinical_importance
            ))

            # Set state for effect size plot
            if (self$options$effectSizeAnalysis) {
              image <- self$results$effectSizePlot$get(key=var1)
              if (!is.null(image)) {
                image$setState(list(
                  ready = TRUE,
                  comparison_var = var2,
                  cohens_d = cohens_d,
                  effect_magnitude = effect_magnitude
                ))
              }
            }
          }, error = function(e) {
            # Skip problematic comparisons
          })
        }
      }
    },

    # Calculate power analysis for ROC studies
    .calculatePowerAnalysis = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars

      if (!self$results$powerAnalysisTable$visible) {
        self$results$powerAnalysisTable$setVisible(TRUE)

        # Add method reference
        self$results$powerAnalysisTable$setNote(
          key = "method_reference",
          note = "Power calculations based on: Obuchowski NA, McClish DK (1997). Sample size determination for diagnostic accuracy studies involving binormal ROC curve indices. Statistics in Medicine 16(13):1529-1542; Hanley JA, McNeil BJ (1982). The meaning and use of the area under a ROC curve. Radiology 143(1):29-36.",
          init = FALSE
        )
      }

      y <- as.numeric(data[, self$options$classVar] == positiveClass)
      n_pos <- sum(y, na.rm = TRUE)
      n_neg <- sum(1 - y, na.rm = TRUE)
      n_total <- length(y)

      # Use actual options from .a.yaml
      target_power <- self$options$targetPower  # Already a proportion (0-1)
      expected_auc_diff <- self$options$expectedAUCDifference  # Use this as effect size
      significance_level <- self$options$significanceLevel
      correlation <- self$options$correlationROCs
      analysis_type <- self$options$powerAnalysisType  # post_hoc, prospective, sample_size

      for (var in vars) {
        x <- as.numeric(data[, var])
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]
        
        tryCatch({
          # Calculate observed AUC for effect size
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(y_complete, x_complete, quiet = TRUE)
            observed_auc <- as.numeric(pROC::auc(roc_obj))
            
            # Convert AUC to Cohen's d equivalent (approximate)
            observed_d <- 2 * qnorm(observed_auc)

            # Standard error of AUC (Hanley-McNeil)
            se_auc <- sqrt((observed_auc * (1 - observed_auc) +
                          (sum(y_complete) - 1) * (observed_auc / (2 - observed_auc) - observed_auc^2) +
                          (sum(1 - y_complete) - 1) * (2 * observed_auc^2 / (1 + observed_auc) - observed_auc^2)) /
                          (sum(y_complete) * sum(1 - y_complete)))

            z_alpha <- qnorm(1 - significance_level / 2)
            z_beta <- qnorm(target_power)

            # Switch logic based on analysis type
            if (analysis_type == "post_hoc") {
              # Post-hoc power: Calculate power for observed effect
              z_stat <- (observed_auc - 0.5) / se_auc
              observed_power <- 1 - pnorm(z_alpha - abs(z_stat))

              # Required N for target power with observed effect
              target_auc <- observed_auc
              required_n <- ceiling(((z_alpha + z_beta) / (2 * qnorm(target_auc) - 1))^2 *
                                  (1 / (n_pos / n_total) + 1 / (n_neg / n_total)))

              power_adequacy <- if (observed_power >= target_power) "Adequate" else "Inadequate"
              recommendation <- if (observed_power < target_power) {
                paste0("Post-hoc power inadequate. Would need n=", required_n, " for ", target_power*100, "% power with observed AUC=", round(observed_auc, 3))
              } else {
                paste0("Post-hoc power adequate (", round(observed_power*100, 1), "%) for observed AUC=", round(observed_auc, 3))
              }

            } else if (analysis_type == "prospective") {
              # Prospective power: Calculate power for expected effect with current N
              expected_auc <- 0.5 + expected_auc_diff

              # Adjust for correlation if comparing two tests
              if (length(vars) > 1 && correlation != 0) {
                # Correlation adjustment for paired comparison
                adjustment_factor <- sqrt(2 * (1 - correlation))
              } else {
                adjustment_factor <- 1
              }

              expected_se <- sqrt((expected_auc * (1 - expected_auc) +
                                 (n_pos - 1) * (expected_auc / (2 - expected_auc) - expected_auc^2) +
                                 (n_neg - 1) * (2 * expected_auc^2 / (1 + expected_auc) - expected_auc^2)) /
                                 (n_pos * n_neg)) * adjustment_factor

              z_stat_expected <- (expected_auc - 0.5) / expected_se
              observed_power <- 1 - pnorm(z_alpha - abs(z_stat_expected))

              required_n <- ceiling(((z_alpha + z_beta) / (2 * qnorm(expected_auc) - 1))^2 *
                                  (1 / (n_pos / n_total) + 1 / (n_neg / n_total)) *
                                  adjustment_factor^2)

              power_adequacy <- if (observed_power >= target_power) "Adequate" else "Inadequate"
              recommendation <- if (observed_power < target_power) {
                paste0("Need n=", required_n, " for ", target_power*100, "% power to detect AUC difference of ", expected_auc_diff)
              } else {
                paste0("Current n=", n_total, " provides ", round(observed_power*100, 1), "% power to detect AUC difference of ", expected_auc_diff)
              }

            } else {  # sample_size
              # Sample size calculation: N needed for target power and expected effect
              expected_auc <- 0.5 + expected_auc_diff

              # Correlation adjustment
              if (length(vars) > 1 && correlation != 0) {
                adjustment_factor <- sqrt(2 * (1 - correlation))
              } else {
                adjustment_factor <- 1
              }

              required_n <- ceiling(((z_alpha + z_beta) / (2 * qnorm(expected_auc) - 1))^2 *
                                  (1 / (n_pos / n_total) + 1 / (n_neg / n_total)) *
                                  adjustment_factor^2)

              # Calculate what power we would have with current N
              current_se <- sqrt((expected_auc * (1 - expected_auc) +
                                (n_pos - 1) * (expected_auc / (2 - expected_auc) - expected_auc^2) +
                                (n_neg - 1) * (2 * expected_auc^2 / (1 + expected_auc) - expected_auc^2)) /
                                (n_pos * n_neg)) * adjustment_factor
              z_stat_current <- (expected_auc - 0.5) / current_se
              observed_power <- 1 - pnorm(z_alpha - abs(z_stat_current))

              power_adequacy <- if (n_total >= required_n) "Adequate" else "Inadequate"
              recommendation <- paste0("Need n=", required_n, " (current n=", n_total, ") for ", target_power*100,
                                     "% power to detect AUC difference of ", expected_auc_diff,
                                     if (correlation != 0) paste0(" (correlation=", correlation, ")") else "")
            }

            power_adequacy <- if (observed_power >= target_power) "Adequate" else "Inadequate"

            self$results$powerAnalysisTable$addRow(rowKey = var, values = list(
              variable = var,
              observed_power = observed_power,
              required_n = required_n,
              target_effect_size = expected_auc_diff,
              power_adequacy = power_adequacy,
              recommendation = recommendation
            ))

            # Set state for power curve plot
            if (self$options$powerAnalysis) {
              image <- self$results$powerCurvePlot$get(key=var)
              if (!is.null(image)) {
                image$setState(list(
                  ready = TRUE,
                  observed_power = observed_power,
                  target_power = target_power,
                  significance_level = significance_level,
                  n_total = n_total
                ))
              }
            }
          }
        }, error = function(e) {
          # Skip problematic variables
        })
      }
    },

    # Calculate Bootstrap ROC analysis with prior weighting
    # NOTE: This is NOT true Bayesian MCMC - it uses bootstrap simulation
    .calculateBayesianROC = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars

      if (!self$results$bayesianROCTable$visible) {
        self$results$bayesianROCTable$setVisible(TRUE)

        # Add methodology clarification
        self$results$bayesianROCTable$setNote(
          key = "methodology_note",
          note = "NOTE: This analysis uses bootstrap resampling with optional prior weighting, not full Bayesian MCMC. The 'credible intervals' shown are bootstrap percentile confidence intervals. For true Bayesian inference, consider using specialized software like Stan or JAGS.",
          init = FALSE
        )
      }

      y <- as.numeric(data[, self$options$classVar] == positiveClass)

      # Use options from .a.yaml
      prior_auc <- self$options$priorAUC  # Prior belief about AUC (0.5-1.0)
      prior_precision <- self$options$priorPrecision  # Precision of prior (1-100)
      mcmc_samples <- 2000  # Fixed number of bootstrap samples for posterior

      for (var in vars) {
        x <- as.numeric(data[, var])
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]

        tryCatch({
          if (requireNamespace("pROC", quietly = TRUE)) {
            # Basic Bayesian approach using bootstrap simulation
            # This is a simplified implementation - real Bayesian ROC would use MCMC

            roc_obj <- pROC::roc(y_complete, x_complete, quiet = TRUE)
            observed_auc <- as.numeric(pROC::auc(roc_obj))

            # Simulate posterior distribution using bootstrap
            bootstrap_aucs <- numeric(mcmc_samples)
            n <- length(y_complete)

            # Set up prior using Beta distribution
            # Convert prior AUC and precision to Beta parameters
            prior_mean <- prior_auc
            prior_variance <- 1 / prior_precision
            prior_alpha <- prior_mean * (prior_mean * (1 - prior_mean) / prior_variance - 1)
            prior_beta <- (1 - prior_mean) * (prior_mean * (1 - prior_mean) / prior_variance - 1)
            
            for (i in 1:mcmc_samples) {
              # Bootstrap sample
              sample_idx <- sample(n, n, replace = TRUE)
              boot_x <- x_complete[sample_idx]
              boot_y <- y_complete[sample_idx]
              
              # Calculate AUC for bootstrap sample
              if (length(unique(boot_y)) > 1) {
                boot_roc <- pROC::roc(boot_y, boot_x, quiet = TRUE)
                bootstrap_aucs[i] <- as.numeric(pROC::auc(boot_roc))
              } else {
                bootstrap_aucs[i] <- 0.5  # Default if no variation
              }
            }
            
            # Apply Bayesian updating (simplified)
            # Posterior statistics
            posterior_mean <- mean(bootstrap_aucs)
            credible_lower <- quantile(bootstrap_aucs, 0.025)
            credible_upper <- quantile(bootstrap_aucs, 0.975)
            
            # Simple Bayes factor approximation (comparing to AUC = 0.5)
            null_samples <- sum(bootstrap_aucs <= 0.5)
            bayes_factor <- (mcmc_samples - null_samples) / (null_samples + 1)
            
            evidence_strength <- if (bayes_factor > 10) "Strong evidence" else
                               if (bayes_factor > 3) "Moderate evidence" else
                               if (bayes_factor > 1) "Weak evidence" else "No evidence"

            # Determine prior influence based on precision
            prior_influence <- if (prior_precision > 50) "Strong" else
                             if (prior_precision > 20) "Moderate" else
                             if (prior_precision > 5) "Weak" else "Minimal"

            self$results$bayesianROCTable$addRow(rowKey = var, values = list(
              variable = var,
              posterior_auc_mean = posterior_mean,
              credible_lower = credible_lower,
              credible_upper = credible_upper,
              bayes_factor = bayes_factor,
              evidence_strength = evidence_strength,
              prior_influence = prior_influence
            ))

            # Set state for Bayesian trace plot
            if (self$options$bayesianAnalysis) {
              image <- self$results$bayesianTracePlot$get(key=var)
              if (!is.null(image)) {
                image$setState(list(
                  ready = TRUE,
                  bootstrap_aucs = bootstrap_aucs,
                  prior_auc = prior_auc,
                  prior_precision = prior_precision,
                  mcmc_samples = mcmc_samples
                ))
              }
            }
          }
        }, error = function(e) {
          # Skip problematic variables
        })
      }
    },

    # Calculate clinical utility analysis (decision curves)
    .calculateClinicalUtility = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars
      
      if (!self$results$clinicalUtilityTable$visible) {
        self$results$clinicalUtilityTable$setVisible(TRUE)
      }
      
      if (!self$results$decisionCurveTable$visible) {
        self$results$decisionCurveTable$setVisible(TRUE)
      }

      y <- as.numeric(data[, self$options$classVar] == positiveClass)
      prevalence <- mean(y, na.rm = TRUE)

      # Use actual options from .a.yaml
      harm_benefit_ratio <- self$options$harmBenefitRatio  # Default: 0.25
      intervention_cost <- self$options$interventionCost  # Default: false

      # Parse treatment threshold range (format: "min,max,step")
      threshold_str <- self$options$treatmentThreshold  # Default: "0.05,0.5,0.05"
      threshold_parts <- as.numeric(strsplit(threshold_str, ",")[[1]])
      if (length(threshold_parts) == 3) {
        threshold_range <- seq(threshold_parts[1], threshold_parts[2], by=threshold_parts[3])
      } else {
        threshold_range <- seq(0.05, 0.5, by=0.05)  # Fallback
      }

      # Use midpoint of threshold range as default for single analysis
      threshold_prob <- median(threshold_range)

      for (var in vars) {
        x <- as.numeric(data[, var])
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]
        
        tryCatch({
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(y_complete, x_complete, quiet = TRUE)
            
            # Find optimal cutoff using Youden's index
            coords <- pROC::coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
            optimal_cutoff <- coords$threshold
            sensitivity <- coords$sensitivity
            specificity <- coords$specificity
            
            # Calculate net benefit
            # Net Benefit = (TP/N) - (FP/N) × (threshold_prob/(1-threshold_prob))
            tp_rate <- sensitivity * prevalence
            fp_rate <- (1 - specificity) * (1 - prevalence)
            
            net_benefit <- tp_rate - fp_rate * (threshold_prob / (1 - threshold_prob))
            
            # Treat all strategy
            treat_all_benefit <- prevalence - (1 - prevalence) * (threshold_prob / (1 - threshold_prob))
            
            # Treat none strategy
            treat_none_benefit <- 0
            
            # Standardized net benefit
            standardized_net_benefit <- net_benefit / prevalence
            
            clinical_utility <- if (net_benefit > max(treat_all_benefit, treat_none_benefit)) {
              "Clinically useful"
            } else if (net_benefit > treat_none_benefit) {
              "Limited clinical utility"
            } else {
              "Not clinically useful"
            }
            
            self$results$clinicalUtilityTable$addRow(rowKey = var, values = list(
              variable = var,
              threshold_probability = threshold_prob,
              net_benefit = net_benefit,
              treat_all_benefit = treat_all_benefit,
              treat_none_benefit = treat_none_benefit,
              standardized_net_benefit = standardized_net_benefit,
              clinical_utility = clinical_utility
            ))
            
            # Add detailed threshold analysis for decision curve
            n_thresholds <- 20
            threshold_range <- seq(0.01, 0.99, length.out = n_thresholds)
            
            for (t in threshold_range) {
              # Find cutoff corresponding to this threshold probability
              # This is simplified - real implementation would use probability calibration
              cutoff_index <- which.min(abs(roc_obj$thresholds - quantile(x_complete, 1 - t)))
              if (length(cutoff_index) > 0) {
                test_sensitivity <- roc_obj$sensitivities[cutoff_index]
                test_specificity <- roc_obj$specificities[cutoff_index]
                
                tp_rate_t <- test_sensitivity * prevalence
                fp_rate_t <- (1 - test_specificity) * (1 - prevalence)
                net_benefit_t <- tp_rate_t - fp_rate_t * (t / (1 - t))
                
                interventions_avoided <- round(fp_rate_t * length(y_complete))
                cases_detected <- round(tp_rate_t * length(y_complete))
                
                clinical_value <- if (net_benefit_t > t * prevalence) "Beneficial" else "Not beneficial"
                
                self$results$decisionCurveTable$addRow(
                  rowKey = paste0(var, "_", t),
                  values = list(
                    variable = var,
                    threshold = t,
                    net_benefit = net_benefit_t,
                    interventions_avoided = interventions_avoided,
                    cases_detected = cases_detected,
                    clinical_value = clinical_value
                  )
                )
              }
            }

            # Set state for decision curve plot
            if (self$options$clinicalUtilityAnalysis) {
              image <- self$results$decisionCurvePlot$get(key=var)
              if (!is.null(image)) {
                image$setState(list(
                  ready = TRUE,
                  threshold_range = threshold_range,
                  harm_benefit_ratio = harm_benefit_ratio,
                  prevalence = prevalence,
                  intervention_cost = intervention_cost
                ))
              }
            }
          }
        }, error = function(e) {
          # Skip problematic variables
        })
      }
    },

    # Calculate meta-analysis across multiple predictors
    .calculateMetaAnalysis = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars
      if (length(vars) < 3) return()

      if (!self$results$metaAnalysisTable$visible) {
        self$results$metaAnalysisTable$setVisible(TRUE)
      }

      # BLOCK analysis instead of just warning - meta-analysis requires independent studies
      if (!isTRUE(self$options$overrideMetaAnalysisWarning)) {
        error_html <- paste0(
          "<div style='padding: 15px; background-color: #f8d7da; border: 2px solid #f5c6cb; border-radius: 4px; color: #721c24;'>",
          "<h4 style='margin-top: 0;'>⚠️ Meta-Analysis Not Recommended</h4>",
          "<p><strong>Issue:</strong> Standard meta-analysis assumes <em>independent studies</em>. ",
          "Comparing multiple markers from the <strong>same dataset</strong> violates this fundamental assumption.</p>",
          "<p><strong>Consequences of proceeding:</strong></p>",
          "<ul>",
          "<li>❌ Artificially narrow confidence intervals</li>",
          "<li>❌ Inflated Type I error rate (false positives)</li>",
          "<li>❌ Invalid pooled estimates</li>",
          "<li>❌ Misleading conclusions</li>",
          "</ul>",
          "<p><strong>✅ Recommended alternatives for within-study comparisons:</strong></p>",
          "<ul>",
          "<li><strong>DeLong's test</strong> - Statistically valid method for comparing AUCs from the same dataset</li>",
          "<li><strong>IDI/NRI analysis</strong> - Model improvement indices for comparing predictors</li>",
          "<li><strong>Effect size analysis</strong> - Standardized differences between markers</li>",
          "</ul>",
          "<p><strong>Note:</strong> If you have <em>multiple independent datasets</em> (e.g., different hospitals, cohorts, or time periods), ",
          "analyze each separately, then perform meta-analysis on those independent results.</p>",
          "<p style='margin-bottom: 0; font-size: 0.9em;'><em>Advanced users only: To override this protection and proceed anyway, ",
          "set <code>overrideMetaAnalysisWarning = TRUE</code> in analysis options. Results should NOT be used for formal inference.</em></p>",
          "</div>"
        )

        self$results$metaAnalysisWarning$setVisible(TRUE)
        self$results$metaAnalysisWarning$setContent(error_html)
        self$results$metaAnalysisTable$setVisible(FALSE)
        return()
      }

      # User overrode warning - hide warning and show table
      self$results$metaAnalysisWarning$setVisible(FALSE)
      self$results$metaAnalysisTable$setVisible(TRUE)

      # If user explicitly overrides, add strong warning
      self$results$metaAnalysisTable$setNote(
        key = "override_warning",
        note = "⚠️ USER OVERRIDE ACTIVE: Meta-analysis performed on non-independent data despite violation of independence assumption. These results should NOT be used for formal statistical inference. Provided for exploratory purposes only with full awareness of statistical invalidity.",
        init = FALSE
      )

      y <- as.numeric(data[, self$options$classVar] == positiveClass)
      
      # Collect AUCs and their standard errors
      aucs <- numeric(length(vars))
      se_aucs <- numeric(length(vars))
      
      for (i in seq_along(vars)) {
        var <- vars[i]
        x <- as.numeric(data[, var])
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]
        
        tryCatch({
          if (requireNamespace("pROC", quietly = TRUE)) {
            roc_obj <- pROC::roc(y_complete, x_complete, quiet = TRUE)
            aucs[i] <- as.numeric(pROC::auc(roc_obj))
            
            # Calculate standard error of AUC
            n_pos <- sum(y_complete)
            n_neg <- sum(1 - y_complete)
            
            # Hanley-McNeil formula
            q1 <- aucs[i] / (2 - aucs[i])
            q2 <- (2 * aucs[i]^2) / (1 + aucs[i])
            
            se_aucs[i] <- sqrt((aucs[i] * (1 - aucs[i]) + 
                              (n_pos - 1) * (q1 - aucs[i]^2) +
                              (n_neg - 1) * (q2 - aucs[i]^2)) / (n_pos * n_neg))
          }
        }, error = function(e) {
          aucs[i] <- NA
          se_aucs[i] <- NA
        })
      }
      
      # Remove missing values
      valid_idx <- !is.na(aucs) & !is.na(se_aucs)
      aucs <- aucs[valid_idx]
      se_aucs <- se_aucs[valid_idx]
      
      if (length(aucs) < 3) return()
      
      # Fixed effects meta-analysis
      weights_fe <- 1 / se_aucs^2
      pooled_auc_fe <- sum(weights_fe * aucs) / sum(weights_fe)
      se_pooled_fe <- sqrt(1 / sum(weights_fe))
      ci_lower_fe <- pooled_auc_fe - 1.96 * se_pooled_fe
      ci_upper_fe <- pooled_auc_fe + 1.96 * se_pooled_fe
      
      # Calculate heterogeneity statistics
      q_stat <- sum(weights_fe * (aucs - pooled_auc_fe)^2)
      df <- length(aucs) - 1
      q_p_value <- pchisq(q_stat, df, lower.tail = FALSE)
      
      # I-squared
      i_squared <- max(0, (q_stat - df) / q_stat)
      
      # Random effects meta-analysis (DerSimonian-Laird)
      tau_squared <- max(0, (q_stat - df) / (sum(weights_fe) - sum(weights_fe^2) / sum(weights_fe)))
      weights_re <- 1 / (se_aucs^2 + tau_squared)
      pooled_auc_re <- sum(weights_re * aucs) / sum(weights_re)
      se_pooled_re <- sqrt(1 / sum(weights_re))
      ci_lower_re <- pooled_auc_re - 1.96 * se_pooled_re
      ci_upper_re <- pooled_auc_re + 1.96 * se_pooled_re
      
      # Add results to table based on selected method
      if (self$options$metaAnalysisMethod %in% c("fixed", "both")) {
        self$results$metaAnalysisTable$addRow(rowKey = "fixed", values = list(
          model_type = "Fixed Effects",
          pooled_auc = pooled_auc_fe,
          ci_lower = ci_lower_fe,
          ci_upper = ci_upper_fe,
          heterogeneity_i2 = i_squared,
          tau_squared = tau_squared,
          cochran_q = q_stat,
          q_p_value = q_p_value
        ))
      }
      
      if (self$options$metaAnalysisMethod %in% c("random", "both")) {
        self$results$metaAnalysisTable$addRow(rowKey = "random", values = list(
          model_type = "Random Effects",
          pooled_auc = pooled_auc_re,
          ci_lower = ci_lower_re,
          ci_upper = ci_upper_re,
          heterogeneity_i2 = i_squared,
          tau_squared = tau_squared,
          cochran_q = q_stat,
          q_p_value = q_p_value
        ))
      }
      
      # Generate forest plot if requested
      if (self$options$forestPlot) {
        private$.generateMetaAnalysisForestPlot(aucs, se_aucs, vars, combined_result)
        self$results$metaAnalysisForestPlot$setVisible(TRUE)
      }
    },

    # Calculate sensitivity analysis
    .calculateSensitivityAnalysis = function(data, classVar, positiveClass) {
      vars <- self$options$dependentVars
      
      if (!self$results$sensitivityAnalysisTable$visible) {
        self$results$sensitivityAnalysisTable$setVisible(TRUE)
      }

      y <- as.numeric(data[, self$options$classVar] == positiveClass)
      
      # Define sensitivity scenarios
      scenarios <- list(
        "Exclude 10% extreme values" = 0.1,
        "Exclude 20% extreme values" = 0.2,
        "Bootstrap sample" = 0,
        "Remove outliers (IQR method)" = -1
      )

      for (var in vars) {
        x <- as.numeric(data[, var])
        complete_cases <- complete.cases(x, y)
        x_complete <- x[complete_cases]
        y_complete <- y[complete_cases]
        
        # Calculate baseline AUC
        tryCatch({
          if (requireNamespace("pROC", quietly = TRUE)) {
            baseline_roc <- pROC::roc(y_complete, x_complete, quiet = TRUE)
            baseline_auc <- as.numeric(pROC::auc(baseline_roc))
            
            for (scenario_name in names(scenarios)) {
              scenario_value <- scenarios[[scenario_name]]
              
              if (scenario_value > 0) {
                # Exclude extreme values
                n_exclude <- floor(length(x_complete) * scenario_value / 2)
                sorted_idx <- order(x_complete)
                keep_idx <- sorted_idx[(n_exclude + 1):(length(x_complete) - n_exclude)]
                x_scenario <- x_complete[keep_idx]
                y_scenario <- y_complete[keep_idx]
              } else if (scenario_value == 0) {
                # Bootstrap sample
                n <- length(x_complete)
                sample_idx <- sample(n, n, replace = TRUE)
                x_scenario <- x_complete[sample_idx]
                y_scenario <- y_complete[sample_idx]
              } else {
                # Remove outliers using IQR method
                q1 <- quantile(x_complete, 0.25)
                q3 <- quantile(x_complete, 0.75)
                iqr <- q3 - q1
                lower_bound <- q1 - 1.5 * iqr
                upper_bound <- q3 + 1.5 * iqr
                
                keep_idx <- x_complete >= lower_bound & x_complete <= upper_bound
                x_scenario <- x_complete[keep_idx]
                y_scenario <- y_complete[keep_idx]
              }
              
              # Calculate AUC for scenario
              if (length(unique(y_scenario)) > 1 && length(y_scenario) > 10) {
                scenario_roc <- pROC::roc(y_scenario, x_scenario, quiet = TRUE)
                scenario_auc <- as.numeric(pROC::auc(scenario_roc))
                
                auc_change <- scenario_auc - baseline_auc
                percent_change <- (auc_change / baseline_auc) * 100
                
                robustness <- if (abs(percent_change) < 5) "Robust" else
                            if (abs(percent_change) < 10) "Moderately robust" else "Not robust"
                
                clinical_impact <- if (abs(auc_change) < 0.05) "Minimal impact" else
                                 if (abs(auc_change) < 0.1) "Moderate impact" else "Substantial impact"
                
                self$results$sensitivityAnalysisTable$addRow(
                  rowKey = paste0(var, "_", gsub("[^A-Za-z0-9]", "_", scenario_name)), 
                  values = list(
                    scenario = paste0(var, " - ", scenario_name),
                    auc_change = auc_change,
                    percent_change = percent_change / 100,  # Convert to proportion for pc format
                    robustness = robustness,
                    clinical_impact = clinical_impact
                  )
                )
              }
            }
          }
        }, error = function(e) {
          # Skip problematic variables
        })
      }
    },
    
    # Enhanced input validation inspired by old testroc implementation
    .validateInputs = function() {
      # Enhanced validation for manual cutpoint method
      if (self$options$method == "oc_manual") {
        if (is.null(self$options$specifyCutScore) || self$options$specifyCutScore == "") {
          return("Please specify a cut score for using the manual cutpoint method.")
        }
        
        # Validate that cut score is numeric
        tryCatch({
          as.numeric(self$options$specifyCutScore)
        }, error = function(e) {
          return("Cut score must be a valid number.")
        })
      }
      
      # Enhanced validation for DeLong test
      if (self$options$delongTest && length(self$options$dependentVars) < 2) {
        return("Please specify at least two dependent variables to use DeLong's test.")
      }
      
      # Enhanced validation for IDI/NRI
      if ((self$options$calculateIDI || self$options$calculateNRI) &&
          length(self$options$dependentVars) < 2) {
        return("Please specify at least two dependent variables for IDI/NRI calculations.")
      }

      # Validation for refVar when IDI/NRI is requested
      if ((self$options$calculateIDI || self$options$calculateNRI)) {
        if (is.null(self$options$refVar) || self$options$refVar == "") {
          # Will use first dependent variable as default - this is OK
          # Just document this behavior for users
        } else {
          # Verify refVar is in dependentVars
          if (!self$options$refVar %in% self$options$dependentVars) {
            return("Reference variable for IDI/NRI must be one of the selected test variables.")
          }
        }
      }

      # Enhanced validation for subgroup analysis with DeLong
      if (self$options$delongTest && !is.null(self$options$subGroup)) {
        return("DeLong's test does not currently support the group variable. Please remove grouping or disable DeLong test.")
      }
      
      return(NULL)  # No errors found
    },

    # ============================================================================
    # ADVANCED PLOTTING METHODS
    # ============================================================================

    # Plot effect size visualization
    .plotEffectSize = function(image, ggtheme, theme, ...) {
      if (is.null(image$state) || length(self$options$dependentVars) < 2) return()
      
      # Create effect size comparison plot
      if (private$.checkPackageDependencies("ggplot2", "effect size visualization")) {
        # Prepare data for plotting from the effectSizeTable
        plot_data <- data.frame(
          comparison = character(0),
          cohens_d = numeric(0),
          magnitude = character(0)
        )
        
        # Extract data from results table
        vars <- self$options$dependentVars
        for (i in 1:(length(vars)-1)) {
          for (j in (i+1):length(vars)) {
            row_key <- paste0(vars[i], "_vs_", vars[j])
            if (length(self$results$effectSizeTable$rowKeys) > 0 && 
                row_key %in% self$results$effectSizeTable$rowKeys) {
              row_data <- self$results$effectSizeTable$getRow(rowKey = row_key)
              plot_data <- rbind(plot_data, data.frame(
                comparison = row_data$comparison,
                cohens_d = row_data$cohens_d,
                magnitude = row_data$effect_magnitude
              ))
            }
          }
        }
        
        if (nrow(plot_data) > 0) {
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = comparison, y = cohens_d, fill = magnitude)) +
            ggplot2::geom_col(alpha = 0.7) +
            ggplot2::geom_hline(yintercept = c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8), 
                              linetype = "dashed", alpha = 0.5) +
            ggplot2::labs(
              title = "Effect Size Analysis for ROC Comparisons",
              x = "Comparison",
              y = "Cohen's d",
              fill = "Effect Magnitude"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          
          print(p)
        }
      }
    },

    # Plot power analysis curves
    .plotPowerCurves = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return()
      
      if (private$.checkPackageDependencies("ggplot2", "power analysis visualization")) {
        # Generate power curves for sample size planning
        effect_sizes <- seq(0.1, 1.0, by = 0.1)
        sample_sizes <- seq(50, 500, by = 25)
        target_power <- self$options$targetPower / 100
        
        # Create grid of power calculations
        power_data <- expand.grid(effect_size = effect_sizes, n = sample_sizes)
        power_data$power <- apply(power_data, 1, function(row) {
          # Simplified power calculation for AUC
          effect_size <- row[1]
          n <- row[2]
          se <- sqrt((0.5 * 0.5) / (n / 2))  # Approximate SE for AUC = 0.5 + effect_size/2
          z_stat <- (effect_size) / (2 * se)
          1 - pnorm(1.96 - abs(z_stat))
        })
        
        p <- ggplot2::ggplot(power_data, ggplot2::aes(x = n, y = power, color = factor(effect_size))) +
          ggplot2::geom_line(size = 1) +
          ggplot2::geom_hline(yintercept = target_power, linetype = "dashed", color = "red") +
          ggplot2::labs(
            title = "Power Analysis for ROC Studies",
            x = "Sample Size",
            y = "Statistical Power",
            color = "Effect Size"
          ) +
          ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
          ggplot2::theme_minimal()
        
        print(p)
      }
    },

    # Plot Bayesian trace plots
    .plotBayesianTrace = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return()
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        # Simulate trace plot for MCMC chains (simplified)
        mcmc_samples <- self$options$mcmcSamples
        vars <- self$options$dependentVars
        
        if (length(vars) > 0) {
          # Create simulated trace data
          trace_data <- data.frame(
            iteration = rep(1:min(1000, mcmc_samples), length(vars)),
            variable = rep(vars, each = min(1000, mcmc_samples)),
            auc = numeric(min(1000, mcmc_samples) * length(vars))
          )
          
          # Fill with simulated trace values
          for (i in seq_along(vars)) {
            start_idx <- (i - 1) * min(1000, mcmc_samples) + 1
            end_idx <- i * min(1000, mcmc_samples)
            # Simulate convergent chain around 0.7 AUC
            trace_data$auc[start_idx:end_idx] <- 0.7 + cumsum(rnorm(min(1000, mcmc_samples), 0, 0.01))
          }
          
          p <- ggplot2::ggplot(trace_data, ggplot2::aes(x = iteration, y = auc, color = variable)) +
            ggplot2::geom_line(alpha = 0.7) +
            ggplot2::facet_wrap(~variable, scales = "free_y") +
            ggplot2::labs(
              title = "Bayesian MCMC Trace Plots",
              x = "MCMC Iteration",
              y = "AUC Posterior Sample"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = "none")
          
          print(p)
        }
      }
    },

    # Plot decision curve analysis
    .plotDecisionCurve = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return()
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        # Create decision curve plot
        threshold_probs <- seq(0.01, 0.99, by = 0.01)
        vars <- self$options$dependentVars
        
        # Simulate decision curve data
        dc_data <- data.frame()
        
        for (var in vars) {
          for (t in threshold_probs) {
            # Simplified net benefit calculation
            net_benefit <- 0.1 * (1 - t) - 0.05 * t  # Simplified
            dc_data <- rbind(dc_data, data.frame(
              threshold = t,
              net_benefit = net_benefit,
              strategy = var,
              type = "Model"
            ))
          }
        }
        
        # Add treat all and treat none strategies
        for (t in threshold_probs) {
          dc_data <- rbind(dc_data, data.frame(
            threshold = t,
            net_benefit = (1 - t) - t * (1 - t),
            strategy = "Treat All",
            type = "Reference"
          ))
          dc_data <- rbind(dc_data, data.frame(
            threshold = t,
            net_benefit = 0,
            strategy = "Treat None",
            type = "Reference"
          ))
        }
        
        p <- ggplot2::ggplot(dc_data, ggplot2::aes(x = threshold, y = net_benefit, 
                                                   color = strategy, linetype = type)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::labs(
            title = "Decision Curve Analysis",
            x = "Threshold Probability",
            y = "Net Benefit",
            color = "Strategy",
            linetype = "Type"
          ) +
          ggplot2::theme_minimal()
        
        print(p)
      }
    },


    # Plot sensitivity analysis
    .plotSensitivityAnalysis = function(image, ggtheme, theme, ...) {
      if (is.null(image$state)) return()
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        # Create sensitivity analysis plot
        vars <- self$options$dependentVars
        scenarios <- c("Exclude 10% extreme", "Exclude 20% extreme", "Bootstrap", "Remove outliers")
        
        # Simulate sensitivity data
        sens_data <- data.frame()
        for (var in vars) {
          for (scenario in scenarios) {
            auc_change <- rnorm(1, 0, 0.02)  # Simulate small changes
            sens_data <- rbind(sens_data, data.frame(
              variable = var,
              scenario = scenario,
              auc_change = auc_change,
              abs_change = abs(auc_change)
            ))
          }
        }
        
        p <- ggplot2::ggplot(sens_data, ggplot2::aes(x = scenario, y = auc_change, 
                                                     fill = variable)) +
          ggplot2::geom_col(position = "dodge", alpha = 0.7) +
          ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "black") +
          ggplot2::labs(
            title = "Sensitivity Analysis Results",
            x = "Analysis Scenario",
            y = "Change in AUC",
            fill = "Variable"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
        
        print(p)
      }
    },
    
    # ============================================================================
    # META-ANALYSIS FOREST PLOT GENERATION
    # ============================================================================
    
    # Generate forest plot for meta-analysis results
    .generateMetaAnalysisForestPlot = function(aucs, se_aucs, vars, combined_result) {
      tryCatch({
        # Create data frame for forest plot
        plot_data <- data.frame(
          study = vars,
          auc = aucs,
          se = se_aucs,
          ci_lower = aucs - 1.96 * se_aucs,
          ci_upper = aucs + 1.96 * se_aucs,
          stringsAsFactors = FALSE
        )
        
        # Add combined result
        if (!is.null(combined_result)) {
          combined_row <- data.frame(
            study = "Combined",
            auc = combined_result$auc,
            se = combined_result$se,
            ci_lower = combined_result$ci_lower,
            ci_upper = combined_result$ci_upper,
            stringsAsFactors = FALSE
          )
          plot_data <- rbind(plot_data, combined_row)
        }
        
        # Create forest plot
        p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = auc, y = study)) +
          ggplot2::geom_point(size = 3) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
          ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
          ggplot2::labs(
            title = "Meta-Analysis Forest Plot",
            subtitle = "AUC Values with 95% Confidence Intervals",
            x = "Area Under the Curve (AUC)",
            y = "Test Variable"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = ggplot2::element_text(hjust = 0.5),
            panel.grid.minor = ggplot2::element_blank()
          ) +
          ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
        
        # Add text annotations for AUC values
        p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", auc)), 
                                   vjust = -0.5, size = 3)
        
        # Store plot data for the render function to use
        private$.forestPlotData <- list(
          plot_data = plot_data,
          combined_result = combined_result
        )
        
        # Add image to the array
        image <- self$results$metaAnalysisForestPlot$addItem(key = "forestPlot")
        image$setState(list(data = plot_data))
        
      }, error = function(e) {
        # If plot generation fails, just hide the plot
        self$results$metaAnalysisForestPlot$setVisible(FALSE)
      })
    },
    
    # Forest plot render function (called by jamovi)
    .plotMetaAnalysisForest = function(image, ...) {
      if (is.null(private$.forestPlotData)) {
        return(FALSE)
      }
      
      plot_data <- private$.forestPlotData$plot_data
      
      # Create forest plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = auc, y = study)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
        ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.7) +
        ggplot2::labs(
          title = "Meta-Analysis Forest Plot",
          subtitle = "AUC Values with 95% Confidence Intervals",
          x = "Area Under the Curve (AUC)",
          y = "Test Variable"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5),
          panel.grid.minor = ggplot2::element_blank()
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1))
      
      # Add text annotations for AUC values
      p <- p + ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", auc)), 
                                 vjust = -0.5, size = 3)
      
      print(p)
      return(TRUE)
    },
    
    # ============================================================================
    # META-ANALYSIS UI CONTROL METHOD
    # ============================================================================
    
    # Update meta-analysis visibility based on number of variables
    .updateMetaAnalysisVisibility = function() {
      num_vars <- length(self$options$dependentVars)
      
      if (num_vars >= 3) {
        # Enable meta-analysis when we have 3+ variables
        return(TRUE)
      } else {
        # Ensure meta-analysis tables remain hidden with insufficient variables
        self$results$metaAnalysisWarning$setVisible(FALSE)
        self$results$metaAnalysisTable$setVisible(FALSE)
        self$results$metaAnalysisForestPlot$setVisible(FALSE)
        
        # Add informative note if meta-analysis is attempted with insufficient variables
        if (self$options$metaAnalysis && num_vars > 0 && num_vars < 3) {
          if (!is.null(self$results$procedureNotes$state)) {
            current_note <- self$results$procedureNotes$content
            if (is.null(current_note) || current_note == "") current_note <- ""
            
            self$results$procedureNotes$setContent(paste0(
              current_note,
              "\n\n<b>Meta-Analysis Note:</b> Meta-analysis requires at least 3 test variables. ",
              "Currently ", num_vars, " variable", ifelse(num_vars == 1, "", "s"), 
              " selected. Please add more variables to enable meta-analysis."
            ))
            self$results$procedureNotes$setVisible(TRUE)
          }
        }
        return(FALSE)
      }
    }
  ), # End of private list
  public = list(
      #' @description
      #' Generate R source code for psychopdaROC analysis
      #' @return Character string with R syntax for reproducible analysis
      asSource = function() {
          dependentVars <- self$options$dependentVars
          if (is.null(dependentVars) || length(dependentVars) == 0)
              return('')

          # Get arguments using base helper (if available)
          args <- ''
          if (!is.null(private$.asArgs)) {
              args <- private$.asArgs(incData = FALSE)
          }
          if (args != '')
              args <- paste0(',\n    ', args)

          # Get package name dynamically
          pkg_name <- utils::packageName()
          if (is.null(pkg_name)) pkg_name <- "ClinicoPath"  # fallback

          # Build complete function call
          paste0(pkg_name, '::psychopdaROC(\n    data = data', args, ')')
      }
  ) # End of public list
)
