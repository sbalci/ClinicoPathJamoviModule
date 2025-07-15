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
#' @return A psychopdarocResults object containing:
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
#' result1 <- psychopdaroc(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status", 
#'   positiveClass = "Disease"
#' )
#' 
#' # Compare multiple biomarkers with DeLong test
#' result2 <- psychopdaroc(
#'   data = medical_roc_data,
#'   dependentVars = c("biomarker1", "biomarker2", "biomarker3"),
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   delongTest = TRUE,
#'   combinePlots = TRUE
#' )
#' 
#' # Advanced analysis with IDI/NRI
#' result3 <- psychopdaroc(
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
#' result4 <- psychopdaroc(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   method = "oc_cost_ratio",
#'   costratioFP = 2.5  # False positives cost 2.5x false negatives
#' )
#' 
#' # Subgroup analysis by hospital
#' result5 <- psychopdaroc(
#'   data = medical_roc_data,
#'   dependentVars = "biomarker1",
#'   classVar = "disease_status",
#'   positiveClass = "Disease",
#'   subGroup = "hospital"
#' )
#' 
#' # Comprehensive analysis with all features
#' result6 <- psychopdaroc(
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
#' financial_result <- psychopdaroc(
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
#' education_result <- psychopdaroc(
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
#' quality_result <- psychopdaroc(
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

psychopdarocClass <- if (requireNamespace('jmvcore')) R6::R6Class(
  "psychopdarocClass",
  inherit = psychopdarocBase,
  private = list(
    # ============================================================================
    # CLASS PRIVATE FIELDS
    # ============================================================================

    ## Storage for ROC data and other analysis results
    .rocDataList = list(),            # Store ROC curve data for each variable
    .optimalCriteriaList = list(),    # Store optimal cutpoints
    .prevalenceList = list(),         # Store prevalence values

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

    # Calculate partial AUC using pROC package
    .calculatePartialAUC = function(x, class, positiveClass, from, to) {
      # Need to load pROC package
      if (!requireNamespace("pROC", quietly = TRUE)) {
        warning("The pROC package is required for partial AUC calculations.")
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(unique(class)[unique(class) != positiveClass][1], positiveClass),
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
      # Need to load pROC package
      if (!requireNamespace("pROC", quietly = TRUE)) {
        warning("The pROC package is required for ROC curve smoothing.")
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(unique(class)[unique(class) != positiveClass][1], positiveClass),
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
      # Need to load pROC package
      if (!requireNamespace("pROC", quietly = TRUE)) {
        warning("The pROC package is required for bootstrap confidence intervals.")
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(unique(class)[unique(class) != positiveClass][1], positiveClass),
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
            results$threshold <- list(
              estimate = optimal_coords$threshold,
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
      if (is.null(subGroup)) {
        dependentVar <- as.numeric(data[, var])
        classVar <- data[, self$options$classVar]
      } else {
        varParts <- strsplit(var, split = "_")[[1]]
        varName <- varParts[1]
        groupName <- paste(varParts[-1], collapse="_")
        dependentVar <- as.numeric(data[subGroup == groupName, varName])
        classVar <- data[subGroup == groupName, self$options$classVar]
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

        # Calculate metrics
        sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else 0
        specificity <- if ((tn + fp) > 0) tn / (tn + fp) else 0
        ppv <- if ((tp + fp) > 0) tp / (tp + fp) else 0
        npv <- if ((tn + fn) > 0) tn / (tn + fn) else 0
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

        # Get optimal cutpoint data
        optimal_idx <- which(confusionMatrix$x.sorted == results$optimal_cutpoint[1])
        if (length(optimal_idx) > 0) {
          tp <- confusionMatrix$tp[optimal_idx]
          fp <- confusionMatrix$fp[optimal_idx]
          tn <- confusionMatrix$tn[optimal_idx]
          fn <- confusionMatrix$fn[optimal_idx]

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
      # Need to load pROC package
      if (!requireNamespace("pROC", quietly = TRUE)) {
        warning("The pROC package is required for classifier comparison.")
        return(NULL)
      }

      # Create ROC object
      roc_obj <- pROC::roc(
        response = class,
        predictor = x,
        levels = c(unique(class)[unique(class) != positiveClass][1], positiveClass),
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
      predicted_prob <- pROC::predict.roc(roc_obj)
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
    .deLongTest = function(data, classVar, positiveClass, ref = NULL, conf.level = 0.95) {
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
          warning("Specified positive class not found. Using first unique value instead.")
          positiveClass <- unique(classVar)[1]
        }
      }

      # Check if positive class exists in the data
      id.pos <- classVar == positiveClass
      if (sum(id.pos) < 1) {
        stop("Wrong level specified for positive class. No observations found.")
      }

      # Check data dimensions
      if (dim(data)[2] < 2) {
        stop("Data must contain at least two columns (different measures).")
      }
      if (dim(data)[1] < 2) {
        stop("Data must contain at least two rows (observations).")
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
      # Add additional plot items based on user options
      if (self$options$showCriterionPlot)
        self$results$criterionPlot$setVisible(TRUE)
      if (self$options$showPrevalencePlot)
        self$results$prevalencePlot$setVisible(TRUE)
      if (self$options$showDotPlot)
        self$results$dotPlot$setVisible(TRUE)
    },

    # ============================================================================
    # MAIN ANALYSIS METHOD
    # ============================================================================

    # Execute the ROC analysis
    .run = function() {

      # -----------------------------------------------------------------------
      # 1. INSTRUCTIONS AND PRELIMINARY CHECKS
      # -----------------------------------------------------------------------

      # Show instructions if required inputs are not provided
      if (is.null(self$options$classVar) || is.null(self$options$dependentVars)) {
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
                    <li>Place the test result variable(s) in the 'Dependent Variable' slot<br /><br /></li>
                    <li>Place the binary classification (gold standard) in the 'Class Variable' slot<br /><br /></li>
                    <li>[<em>Optional</em>] Place a grouping variable in the 'Group Variable' slot<br /><br /></li>
                    </ol>
                    <p>The ROC analysis helps you determine optimal cut-off values for classifying cases.</p>
                    </div>
                    </body>
                    </html>"
        )
        return()
      } else {
        # Hide instructions when inputs are provided
        self$results$instructions$setVisible(visible = FALSE)

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
        if (!is.null(self$options$subGroup)) {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Sub-Group Variable: ",
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

      # Get data
      data <- self$data

      # Determine positive class early for use throughout the analysis
      if (!is.null(self$options$positiveClass) && self$options$positiveClass != "") {
        # Use the level selector value
        positiveClass <- self$options$positiveClass

        # Verify the selected level exists in the data
        classVar <- data[, self$options$classVar]
        if (!positiveClass %in% levels(factor(classVar))) {
          warning(paste("Selected positive class", positiveClass,
                        "not found in data. Using first level instead."))
          positiveClass <- levels(factor(classVar))[1]
        }
      } else {
        # Default to first level if not specified
        classVar <- data[, self$options$classVar]
        positiveClass <- levels(factor(classVar))[1]
      }

      # Set up cutpoint method
      if (self$options$method == "oc_manual") {
        method <- cutpointr::oc_manual
        if (self$options$specifyCutScore == "") {
          stop("Please specify a cut score when using the 'Custom cut score' method.")
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
        subGroup <- data[, self$options$subGroup]
        classVar <- data[, self$options$classVar]
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

        # -----------------------------------------------------------------------
        # 8. PREPARE PLOTTING DATA
        # -----------------------------------------------------------------------

        if (self$options$plotROC) {
          if (self$options$combinePlots == FALSE) {
            # Individual plot for this variable
            image <- self$results$plotROC$get(key = var)
            image$setTitle(paste0("ROC Curve: ", var))
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
              criterionImage$setTitle(paste0("Sensitivity and Specificity vs. Threshold: ", var))
              criterionImage$setState(private$.rocDataList[[var]])
            }

            if (self$options$showPrevalencePlot) {
              prevImage <- self$results$prevalencePlot$get(key = var)
              prevImage$setTitle(paste0("Predictive Values vs. Prevalence: ", var))
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
              dotImage$setTitle(paste0("Dot Plot: ", var))
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
        image$setTitle("ROC Curve: Combined")
        image$setState(plotDataList)

        # Combined criterion plot if enabled
        if (self$options$showCriterionPlot) {
          self$results$criterionPlot$addItem(key = 1)
          criterionImage <- self$results$criterionPlot$get(key = 1)
          criterionImage$setTitle("Sensitivity and Specificity vs. Threshold: Combined")

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
            "<p>Dot plots aren't available in combined plot mode. Please uncheck 'Combine plots' to view individual dot plots.</p>"
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
        # Enhanced validation with better error handling
        if (length(self$options$dependentVars) < 2) {
          stop("Please specify at least two dependent variables to use DeLong's test.")
        }

        if (!is.null(self$options$subGroup)) {
          stop("DeLong's test does not currently support the group variable. If you would like to contribute/provide guidance, please use the contact information provided in the documentation.")
        } else {
          # Run enhanced DeLong's test with better error handling
          delongResults <- tryCatch({
            private$.enhancedDelongTest(
              data = data.frame(lapply(data[, self$options$dependentVars], as.numeric)),
              classVar = as.character(data[, self$options$classVar]),
              pos_class = positiveClass,
              ref = NULL,
              conf.level = 0.95
            )
          }, error = function(e) {
            # Fallback to original implementation if enhanced version fails
            warning(paste("Enhanced DeLong test failed, using fallback:", e$message))
            private$.deLongTest(
              data = data.frame(lapply(data[, self$options$dependentVars], as.numeric)),
              classVar = as.character(data[, self$options$classVar]),
              ref = NULL,
              positiveClass = positiveClass,
              conf.level = 0.95
            )
          })

          # Display results
          self$results$delongTest$setVisible(visible = TRUE)

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
        if (is.null(subGroup)) {
          classVar <- data[, self$options$classVar]
        } else {
          # For grouped variables, extract the group
          varParts <- strsplit(var, split = "_")[[1]]
          groupName <- paste(varParts[-1], collapse="_")
          classVar <- data[subGroup == groupName, self$options$classVar]
        }

        n_pos <- sum(classVar == positiveClass)
        n_neg <- sum(classVar != positiveClass)

        # Calculate standard error using Hanley & McNeil formula
        auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))

        # Calculate 95% confidence interval
        z_critical <- qnorm(0.975)
        auc_lci <- max(0, auc_value - z_critical * auc_se)
        auc_uci <- min(1, auc_value + z_critical * auc_se)

        # Calculate p-value (against null hypothesis AUC = 0.5)
        z_stat <- (auc_value - 0.5) / auc_se
        p_val <- 2 * (1 - pnorm(abs(z_stat)))

        # Add row to simple table
        simpleTable$addRow(rowKey = var, values = list(
          variable = var,
          auc = auc_value,
          ci_lower = auc_lci,
          ci_upper = auc_uci,
          p = p_val
        ))
      }

      # Populate the AUC summary table
      aucSummaryTable <- self$results$aucSummaryTable

      for (var in names(aucList)) {
        # Get AUC value directly from the list
        auc_value <- aucList[[var]]

        # Get data needed for calculations
        if (is.null(subGroup)) {
          classVar <- data[, self$options$classVar]
        } else {
          # For grouped variables, extract the group
          varParts <- strsplit(var, split = "_")[[1]]
          groupName <- paste(varParts[-1], collapse="_")
          classVar <- data[subGroup == groupName, self$options$classVar]
        }

        # Calculate counts and statistics
        n_pos <- sum(classVar == positiveClass)
        n_neg <- sum(classVar != positiveClass)

        # Calculate standard error and confidence interval
        auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))
        z_critical <- qnorm(0.975)
        auc_lci <- max(0, auc_value - z_critical * auc_se)
        auc_uci <- min(1, auc_value + z_critical * auc_se)

        # Calculate p-value
        z_stat <- (auc_value - 0.5) / auc_se
        p_val <- 2 * (1 - pnorm(abs(z_stat)))

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
        # Set up partial AUC table if not already done
        if (!self$results$partialAUCTable$isVisible) {
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
        # Set up bootstrap CI table if not already done
        if (!self$results$bootstrapCITable$isVisible) {
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
              self$results$bootstrapCITable$addRow(rowKey = paste0(var, "_", param), values = list(
                variable = var,
                parameter = param,
                estimate = bootstrap_results[[param]]$estimate,
                ci_lower = bootstrap_results[[param]]$ci_lower,
                ci_upper = bootstrap_results[[param]]$ci_upper
              ))
            }
          }
        }
      }

      # Calculate precision-recall curves if requested
      if (self$options$precisionRecallCurve) {
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
        if (!self$results$rocComparisonTable$isVisible) {
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
        if (is.null(self$options$refVar) || self$options$refVar == "") {
          refVar <- self$options$dependentVars[1]
        } else {
          refVar <- self$options$refVar
        }

        # Get actual class values and convert to binary
        classVar <- data[, self$options$classVar]
        actual_binary <- as.numeric(classVar == positiveClass)

        # Get direction
        direction <- self$options$direction

        # Get bootstrap runs
        boot_runs <- as.numeric(self$options$idiNriBootRuns)

        # Calculate IDI if requested
        if (self$options$calculateIDI) {
          # Get reference variable values
          ref_values <- as.numeric(data[, refVar])

          # For each other variable
          for (var in self$options$dependentVars) {
            if (var != refVar) {
              var_values <- as.numeric(data[, var])

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
          # Get reference variable values
          ref_values <- as.numeric(data[, refVar])

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
              var_values <- as.numeric(data[, var])

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
      }

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
      if (!requireNamespace("plotROC", quietly = TRUE)) {
        warning("The plotROC package is not available. Cannot create interactive ROC plot.")
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
    .enhancedDelongTest = function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
      # Enhanced validation
      if (length(classVar) != nrow(data)) {
        stop("The number of rows in data must match the length of classVar")
      }

      id.pos <- classVar == pos_class
      
      if (sum(id.pos) < 1) {
        stop("Wrong positive class level specified.")
      }
      if (ncol(data) < 2) {
        stop("Data must contain at least two columns.")
      }
      if (nrow(data) < 2) {
        stop("Data must contain at least two dependent variables for DeLong's test.")
      }

      nn <- sum(!id.pos)
      np <- sum(id.pos)
      nauc <- ncol(data)

      # Enhanced matrix setup with better error handling
      if (is.null(ref)) {
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
        if (ref > nauc) {
          stop(paste("Reference ref must be one of the markers (1...", nauc, " in this case)", sep = ""))
        }
        L <- matrix(1, ncol = nauc, nrow = nauc - 1)
        L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
      }

      markern <- as.matrix(data[!id.pos, ])
      markerp <- as.matrix(data[id.pos, ])

      # Enhanced Wilcoxon statistic calculation
      WK.STAT <- function(data, y) {
        if (length(data) == 0 || length(y) == 0) {
          return(NA)
        }
        r <- rank(c(data, y))
        n.data <- length(data)
        n.y <- length(y)
        STATISTIC <- sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
        return(STATISTIC)
      }

      auc <- numeric(nauc)
      for (r in 1:nauc) {
        stat_result <- WK.STAT(markerp[, r], markern[, r])
        if (is.na(stat_result)) {
          auc[r] <- NA
        } else {
          auc[r] <- stat_result
        }
      }
      auc <- auc / (nn * np)

      # Handle AUCs smaller than 0.5 with enhanced logic
      if (any(auc < 0.5, na.rm = TRUE)) {
        flip_indices <- which(auc < 0.5)
        data[, flip_indices] <- -data[, flip_indices]
        auc[flip_indices] <- 1 - auc[flip_indices]
        markern <- as.matrix(data[!id.pos, ])
        markerp <- as.matrix(data[id.pos, ])
      }

      # Enhanced covariance calculation with better error handling
      V10 <- matrix(0, nrow = np, ncol = nauc)
      V01 <- matrix(0, nrow = nn, ncol = nauc)

      tryCatch({
        tmn <- t(markern)
        tmp <- t(markerp)
        for (i in 1:np) {
          V10[i, ] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
        }
        for (i in 1:nn) {
          V01[i, ] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
        }
      }, error = function(e) {
        stop(paste("Error in covariance calculation:", e$message))
      })

      V10 <- V10 / nn
      V01 <- V01 / np

      W10 <- cov(V10)
      W01 <- cov(V01)

      # Estimated covariance matrix
      S <- W10 / np + W01 / nn

      # Enhanced variance calculations
      q1 <- auc / (2 - auc)
      q2 <- 2 * auc^2 / (1 + auc)

      # Hanley, McNeil (1982) / Bamber (1975) variance estimates
      aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc^2) + (nn - 1) * (q2 - auc^2)) / (np * nn)
      zhalf <- (auc - 0.5) / sqrt(aucvar)
      phalf <- 1 - pnorm(zhalf)
      zdelong <- (auc - 0.5) / sqrt(diag(S))
      pdelong <- 1 - pnorm(zdelong)

      # Enhanced global p-value calculation
      aucdiff <- L %*% auc
      
      # Use enhanced matrix inversion with better error handling
      tryCatch({
        LSL_matrix <- L %*% S %*% t(L)
        z <- t(aucdiff) %*% MASS::ginv(LSL_matrix) %*% aucdiff
        p <- pchisq(z, df = qr(LSL_matrix)$rank, lower.tail = FALSE)
      }, error = function(e) {
        warning(paste("Error in global test calculation:", e$message))
        z <- NA
        p <- NA
      })

      # Enhanced pairwise comparisons
      if (is.null(ref)) {
        cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
        ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
        ctr <- 1
        rows <- character(nauc * (nauc - 1) / 2)
        pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
        quantil <- qnorm(1 - (1 - conf.level) / 2)
        
        for (i in 1:(nauc - 1)) {
          for (j in (i + 1):nauc) {
            tryCatch({
              cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])
              LSL <- t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
              tmpz <- (aucdiff[ctr]) %*% MASS::ginv(LSL) %*% aucdiff[ctr]
              pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
              ci[ctr, ] <- c(aucdiff[ctr] - quantil * sqrt(LSL),
                           aucdiff[ctr] + quantil * sqrt(LSL))
              rows[ctr] <- paste(i, j, sep = " vs. ")
            }, error = function(e) {
              cor.auc[ctr] <- NA
              pairp[ctr] <- NA
              ci[ctr, ] <- c(NA, NA)
              rows[ctr] <- paste(i, j, sep = " vs. ")
            })
            ctr <- ctr + 1
          }
        }
      } else {
        cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
        ci <- matrix(ncol = 2, nrow = nauc - 1)
        rows <- character(nauc - 1)
        pairp <- matrix(nrow = nauc - 1, ncol = 1)
        comp <- (1:nauc)[-ref]
        quantil <- qnorm(1 - (1 - conf.level) / 2)
        
        for (i in 1:(nauc - 1)) {
          tryCatch({
            cor.auc[i] <- S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])
            LSL <- t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
            tmpz <- aucdiff[i] %*% MASS::ginv(LSL) %*% aucdiff[i]
            pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
            ci[i, ] <- c(aucdiff[i] - quantil * sqrt(LSL),
                        aucdiff[i] + quantil * sqrt(LSL))
            rows[i] <- paste(ref, comp[i], sep = " vs. ")
          }, error = function(e) {
            cor.auc[i] <- NA
            pairp[i] <- NA
            ci[i, ] <- c(NA, NA)
            rows[i] <- paste(ref, comp[i], sep = " vs. ")
          })
        }
      }

      # Enhanced results formatting
      newres <- as.data.frame(cbind(aucdiff, ci, pairp, cor.auc))
      names(newres) <- c("AUC Difference", "CI(lower)", "CI(upper)", "P.Value", "Correlation")
      rownames(newres) <- rows
      row.names(ci) <- row.names(cor.auc) <- row.names(aucdiff) <- row.names(pairp) <- rows
      colnames(ci) <- c(paste0(100 * conf.level, "% CI (lower)"), paste0(100 * conf.level, "% CI (upper)"))
      names(auc) <- 1:nauc
      auc <- as.data.frame(cbind(auc, sqrt(aucvar), phalf, sqrt(diag(S)), pdelong))
      colnames(auc) <- c("AUC", "SD(Hanley)", "P(H0: AUC=0.5)", "SD(DeLong)", "P(H0: AUC=0.5)")

      ERG <- list(
        AUC = auc,
        difference = newres,
        covariance = S,
        global.z = z,
        global.p = p
      )
      class(ERG) <- "EnhancedDeLong"
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
      
      # Enhanced validation for subgroup analysis with DeLong
      if (self$options$delongTest && !is.null(self$options$subGroup)) {
        return("DeLong's test does not currently support the group variable. Please remove grouping or disable DeLong test.")
      }
      
      return(NULL)  # No errors found
    }
  )
)
