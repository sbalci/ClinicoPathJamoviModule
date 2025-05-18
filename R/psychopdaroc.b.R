#' @title ROC Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import cutpointr
#' @importFrom MASS ginv


# ============================================================================
# UTILITY FUNCTIONS - OUTSIDE THE MAIN CLASS
# ============================================================================

#' @title Generate a formatted HTML table for sensitivity/specificity results
#' @param TP Number of true positives
#' @param FP Number of false positives
#' @param TN Number of true negatives
#' @param FN Number of false negatives
#' @return HTML string containing the formatted table
print.sensSpecTable = function(Title, TP, FP, TN, FN) {
  # Create HTML table with the confusion matrix results
  html <- paste0(
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
            <th class='tg-0lax' colspan='4'>", Title, "</th>
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
            <td class='tg-s6z2'>", TN, " (TN)</td>
            <td class='tg-s6z2'>", FP, " (FP)</td>
          </tr>
          <tr>
            <td class='tg-h0x1'>Positive</td>
            <td class='tg-h0x1'>", FN, " (FN)</td>
            <td class='tg-h0x1'>", TP, " (TP)</td>
          </tr>
          <tr>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
            <td class='tg-tf2e'></td>
          </tr>
        </table>"
  )
  return(html)
}

#' Perform DeLong's test for comparing AUCs
#'
#' @param data Matrix or data frame with test scores (columns = different tests)
#' @param classVar Vector of class labels (factor or character)
#' @param pos_class The positive class label
#' @param ref Reference test for comparisons (NULL = all pairwise comparisons)
#' @param conf.level Confidence level for intervals
#' @return List with test results, AUCs, etc.
deLong.test = function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
  # Validate and prepare inputs
  # Convert factor to character first to handle labels safely
  if (is.factor(classVar)) {
    classVar <- as.character(classVar)
  }

  # Safely identify the positive class
  if (!pos_class %in% unique(classVar)) {
    # Try to interpret pos_class as a position index
    if (is.numeric(try(as.numeric(pos_class), silent = TRUE))) {
      pos_idx <- as.numeric(pos_class)
      if (pos_idx <= length(unique(classVar))) {
        pos_class <- unique(classVar)[pos_idx]
      }
    }
    # If still not found, use the first level
    if (!pos_class %in% unique(classVar)) {
      warning("Specified positive class not found. Using first unique value instead.")
      pos_class <- unique(classVar)[1]
    }
  }

  # Check if positive class exists in the data
  id.pos <- classVar == pos_class
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
}

#' Print method for DeLong test results
#' @export print.DeLong
#' @param x DeLong test object
#' @param digits Number of digits for display
#' @param ... Additional arguments for print methods
print.DeLong = function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Estimated AUC's:\n")
  print(format(round(x$AUC, digits = digits, ...), nsmall = digits, ...))
  cat("\nPairwise comparisons:\n")
  print(format(round(x$difference, digits = digits, ...), nsmall = digits, ...))
  cat(paste("\nOverall test:\n p-value =", format.pval(x$global.p, digits = digits), "\n"))
}

# ============================================================================
# MAIN ANALYSIS CLASS
# ============================================================================

psychopdarocClass = if (requireNamespace('jmvcore'))
  R6::R6Class(
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

          # Add analysis settings
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

        # Add this information to procedure notes
        if (!is.null(self$options$positiveClass) && self$options$positiveClass != "") {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Positive Class: ",
            positiveClass,
            "</p>"
          )
        } else {
          procedureNotes <- paste0(
            procedureNotes,
            "<p> Positive Class: ",
            positiveClass,
            " (first level)</p>"
          )
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
          if (is.null(subGroup)) {
            # Standard case - no grouping
            dependentVar <- as.numeric(data[, var])
            classVar <- data[, self$options$classVar]
          } else {
            # Case with grouping - extract data for this group
            varParts <- strsplit(var, split = "_")[[1]]
            varName <- varParts[1]
            groupName <- paste(varParts[-1], collapse="_")

            # Filter data for this group
            dependentVar <- as.numeric(data[subGroup == groupName, varName])
            classVar <- data[subGroup == groupName, self$options$classVar]
          }

          # Use the already determined positive class
          pos_class <- positiveClass

          # -----------------------------------------------------------------------
          # 5. RUN ROC ANALYSIS
          # -----------------------------------------------------------------------

          # Try to use cutpointr package; handle errors gracefully
          result_success <- FALSE
          result_message <- NULL

          tryCatch({
            # Primary method using cutpointr
            results <- cutpointr::cutpointr(
              x = dependentVar,
              class = classVar,
              subgroup = NULL,
              method = method,
              cutpoint = score,
              metric = metric,
              direction = direction,
              pos_class = pos_class,
              tol_metric = tol_metric,
              boot_runs = boot_runs,
              break_ties = break_ties,
              na.rm = TRUE
            )
            result_success <- TRUE
          }, error = function(e) {
            result_message <- e$message
          })

          # If cutpointr failed, use alternative implementation
          if (!result_success) {
            self$results$procedureNotes$setContent(paste0(
              self$results$procedureNotes$content,
              "<p><strong>Note:</strong> Standard ROC analysis failed with error: '",
              result_message,
              "'. Using alternative implementation.</p>"
            ))

            # Convert to binary response (1 = positive, 0 = negative)
            response <- as.numeric(classVar == pos_class)

            # Create minimal ROC data structure
            roc_data <- data.frame(
              x.sorted = sort(unique(dependentVar)),
              direction = ifelse(direction == ">=", ">", "<")
            )

            # Calculate confusion matrix for each threshold
            for(i in 1:nrow(roc_data)) {
              threshold <- roc_data$x.sorted[i]
              if(direction == ">=") {
                predicted_pos <- dependentVar >= threshold
              } else {
                predicted_pos <- dependentVar <= threshold
              }

              # Compute TP, FP, TN, FN
              roc_data$tp[i] <- sum(predicted_pos & response == 1)
              roc_data$fp[i] <- sum(predicted_pos & response == 0)
              roc_data$tn[i] <- sum(!predicted_pos & response == 0)
              roc_data$fn[i] <- sum(!predicted_pos & response == 1)
            }

            # Calculate sensitivity and specificity
            sens <- roc_data$tp / (roc_data$tp + roc_data$fn)
            spec <- roc_data$tn / (roc_data$tn + roc_data$fp)

            # Compute ROC curve points
            roc_points <- data.frame(
              specificity = spec,
              sensitivity = sens
            )

            # Order by increasing 1-specificity for AUC calculation
            roc_points <- roc_points[order(1-roc_points$specificity),]

            # Calculate AUC using trapezoidal rule
            auc <- 0
            for(i in 2:nrow(roc_points)) {
              # Area of trapezoid
              x_diff <- (1-roc_points$specificity[i]) - (1-roc_points$specificity[i-1])
              y_avg <- (roc_points$sensitivity[i] + roc_points$sensitivity[i-1])/2
              auc <- auc + x_diff * y_avg
            }

            # Find optimal cutpoint using Youden's index
            youdens_j <- sens + spec - 1
            optimal_idx <- which.max(youdens_j)

            # Build minimal result object
            results <- list(
              optimal_cutpoint = roc_data$x.sorted[optimal_idx],
              roc_curve = list(roc_data),
              AUC = auc
            )
          }

          # -----------------------------------------------------------------------
          # 6. HANDLE CUSTOM CUTPOINT METHODS
          # -----------------------------------------------------------------------

          # Apply custom methods if specified
          if (self$options$method %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
            # Get the confusion matrix data
            confusionMatrix <- results$roc_curve[[1]]

            # Calculate prevalence for cost-ratio method
            n_pos <- sum(classVar == pos_class)
            n_neg <- sum(classVar != pos_class)
            prevalence <- n_pos / (n_pos + n_neg)

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

          # Determine which cutpoints to display
          if (!self$options$allObserved) {
            # Just show the optimal cutpoint(s)
            resultsToDisplay <- unlist(results$optimal_cutpoint)
          } else {
            # Show all observed values as potential cutpoints
            resultsToDisplay <- sort(unique(dependentVar))
          }

          # Get ROC curve data
          confusionMatrix <- results$roc_curve[[1]]

          # Filter confusion matrix for display
          if (!self$options$allObserved) {
            confusionMatrixForTable <- confusionMatrix[which(confusionMatrix$x.sorted %in% resultsToDisplay),]
          } else {
            confusionMatrixForTable <- confusionMatrix
          }

          # -----------------------------------------------------------------------
          # 8. GENERATE SENSITIVITY-SPECIFICITY TABLES
          # -----------------------------------------------------------------------

          if (self$options$sensSpecTable) {
            # Generate individual tables for each cutpoint
            for (i in seq_along(resultsToDisplay)) {
              cp <- resultsToDisplay[i]
              # Find the closest cutpoint in the confusion matrix
              idx <- which.min(abs(confusionMatrix$x.sorted - cp))

              # Create HTML table with confusion matrix
              sensSpecRes <- print.sensSpecTable(
                Title = paste0("Scale: ", var, " | Cut Score: ", round(confusionMatrix$x.sorted[idx], 4)),
                TP = confusionMatrix$tp[idx],
                FP = confusionMatrix$fp[idx],
                TN = confusionMatrix$tn[idx],
                FN = confusionMatrix$fn[idx]
              )

              # Add to the results
              sensTable <- self$results$sensSpecTable$get(key = var)
              sensTable$setContent(sensSpecRes)
            }
          }

          # -----------------------------------------------------------------------
          # 9. CALCULATE PERFORMANCE METRICS
          # -----------------------------------------------------------------------

          # Calculate metrics based on confusion matrix
          sensList <- cutpointr::sensitivity(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          specList <- cutpointr::specificity(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          ppvList <- cutpointr::ppv(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          npvList <- cutpointr::npv(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          youdenList <- cutpointr::youden(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          # Calculate the selected metric
          metricList <- metric(
            tp = confusionMatrix$tp,
            fp = confusionMatrix$fp,
            tn = confusionMatrix$tn,
            fn = confusionMatrix$fn
          )

          # -----------------------------------------------------------------------
          # 10. CREATE RESULTS TABLE
          # -----------------------------------------------------------------------

          # Get the table and set title
          table <- self$results$resultsTable$get(key = var)
          table$setTitle(paste0("Scale: ", var))

          # Add rows to the results table for each cutpoint to display
          for (i in seq_along(resultsToDisplay)) {
            cp <- resultsToDisplay[i]

            # Find the closest cutpoint in the confusion matrix
            idx <- which.min(abs(confusionMatrix$x.sorted - cp))

            # Get the values for this cutpoint
            row <- list(
              cutpoint = confusionMatrix$x.sorted[idx],
              sensitivity = sensList[idx],
              specificity = specList[idx],
              ppv = ppvList[idx],
              npv = npvList[idx],
              youden = youdenList[idx],
              AUC = results$AUC,
              metricValue = metricList[idx]
            )

            # Add row to the table
            table$addRow(rowKey = as.character(i), values = row)
          }

          # Save AUC value for summary tables
          aucList[[var]] <- results$AUC

          # -----------------------------------------------------------------------
          # 11. STORE DATA FOR ADDITIONAL PLOTS
          # -----------------------------------------------------------------------

          # Store ROC data for additional plots
          rocData <- data.frame(
            threshold = confusionMatrix$x.sorted,
            sensitivity = sensList,
            specificity = specList,
            ppv = ppvList,
            npv = npvList,
            youden = youdenList,
            stringsAsFactors = FALSE
          )
          private$.rocDataList[[var]] <- rocData

          # Save optimal criterion data (based on Youden's index)
          j_max_idx <- which.max(youdenList)
          optimalCriterion <- list(
            threshold = confusionMatrix$x.sorted[j_max_idx],
            sensitivity = sensList[j_max_idx],
            specificity = specList[j_max_idx],
            ppv = ppvList[j_max_idx],
            npv = npvList[j_max_idx],
            youden = youdenList[j_max_idx]
          )
          private$.optimalCriteriaList[[var]] <- optimalCriterion

          # Calculate and store prevalence
          n_pos <- sum(classVar == pos_class)
          n_neg <- sum(classVar != pos_class)
          prevalence <- n_pos / (n_pos + n_neg)
          private$.prevalenceList[[var]] <- prevalence

          # Store raw data for dot plot
          rawData <- data.frame(
            value = dependentVar,
            class = as.factor(ifelse(classVar == pos_class, "Positive", "Negative")),
            threshold = optimalCriterion$threshold,
            direction = direction,
            stringsAsFactors = FALSE
          )
          attr(private$.rocDataList[[var]], "rawData") <- rawData

          # -----------------------------------------------------------------------
          # 12. PREPARE PLOTTING DATA
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
        # 13. CREATE COMBINED PLOTS
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
            if (!"dotPlotMessage" %in% names(self$results)) {
              self$results$dotPlotMessage <- jmvcore::Html$new(
                options = self$options,
                name = "dotPlotMessage",
                title = "Dot Plot Note",
                visible = (self$options$showDotPlot && self$options$combinePlots)
              )
            }

            # Set content and make visible
            self$results$dotPlotMessage$setContent(
              "<p>Dot plots aren't available in combined plot mode. Please uncheck 'Combine plots' to view individual dot plots.</p>"
            )
            self$results$dotPlotMessage$setVisible(TRUE)

            # Hide the actual plot in combined mode
            self$results$dotPlot$setVisible(FALSE)
          }
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

        # -----------------------------------------------------------------------
        # 14. PERFORM DELONG'S TEST FOR AUC COMPARISON
        # -----------------------------------------------------------------------

        if (self$options$delongTest) {
          # Check if we have enough variables to compare
          if (length(self$options$dependentVars) < 2) {
            stop("Please specify at least two dependent variables to use DeLong's test.")
          }

          if (!is.null(self$options$subGroup)) {
            stop("DeLong's test does not currently support the group variable.")
          } else {
            # Get positive class
            if (self$options$positiveClass == "") {
              pos_class <- as.character(unique(data[, self$options$classVar])[1])
            } else {
              pos_class <- self$options$positiveClass
            }

            # Run DeLong's test
            delongResults <- deLong.test(
              data = data.frame(lapply(data[, self$options$dependentVars], as.numeric)),
              classVar = as.character(data[, self$options$classVar]),
              ref = NULL,
              pos_class = positiveClass,
              conf.level = 0.95
            )

            # Display results
            self$results$delongTest$setVisible(visible = TRUE)
            self$results$delongTest$setContent(paste0(capture.output(print.DeLong(delongResults)), collapse = "\n"))

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
        # 15. CREATE SIMPLIFIED RESULTS TABLES
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

          # Get positive class
          if (self$options$positiveClass == "") {
            pos_class <- as.character(unique(classVar)[1])
          } else {
            pos_class <- self$options$positiveClass
          }

          n_pos <- sum(classVar == pos_class)
          n_neg <- sum(classVar != pos_class)

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



        # Populate the AUC summary table without clearing it
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

          # Determine positive class
          if (self$options$positiveClass == "") {
            pos_class <- as.character(unique(classVar)[1])
          } else {
            pos_class <- self$options$positiveClass
          }

          # Calculate counts and statistics
          n_pos <- sum(classVar == pos_class)
          n_neg <- sum(classVar != pos_class)

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
        # 16. CREATE THRESHOLD TABLE IF REQUESTED
        # -----------------------------------------------------------------------

        if (self$options$showThresholdTable) {
          thresholdTable <- self$results$thresholdTable
          thresholdTable$clear()  # Clear previous results

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
      }
    )
  )
