#' @title ROC Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import cutpointr
#' @importFrom MASS ginv

# Utility functions ----

## print.sensSpecTable ----
print.sensSpecTable = function(Title, TP, FP, TN, FN) {
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
}

## deLong.test ----
deLong.test = function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
        # Convert factor to character first to handle labels safely
        if (is.factor(classVar)) {
            classVar <- as.character(classVar)
        }

        # Try to interpret pos_class flexibly
        if (!pos_class %in% unique(classVar)) {
            # Look for the class by position if specified as a number
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




    id.pos <- classVar == pos_class

    if (sum(id.pos) < 1) {
        stop("\n wrong level specified.\n")
    }
    if (dim(data)[2] < 2) {
        stop("\n data must contain at least two columns.\n")
    }
    if (dim(data)[1] < 2) {
        stop("\n data must contain at least two dependent variables for DeLong's test.\n")
    }

    nn <- sum(!id.pos)
    np <- sum(id.pos)
    nauc <- ncol(data)

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
        if (ref > nauc)
            stop(paste("Reference ref must be one of the markers (1...", nauc, " in this case)", sep = ""))
        L <- matrix(1, ncol = nauc, nrow = nauc - 1)
        L[, -ref] <- diag(-1, nrow = nauc - 1, ncol = nauc - 1)
    }

    markern <- as.matrix(data[!id.pos,])
    markerp <- as.matrix(data[id.pos,])

    # Compute Wilcoxon statistic
    WK.STAT <- function(data, y) {
        r <- rank(c(data, y))
        n.data <- length(data)
        n.y <- length(y)
        STATISTIC <- sum(r[seq_along(data)]) - n.data * (n.data + 1) / 2
        STATISTIC
    }

    auc <- vector("numeric", length = nauc)
    for (r in 1:nauc) {
        auc[r] <- WK.STAT(markerp[, r], markern[, r])
    }
    auc <- auc / (nn * np)

    # If AUCs smaller than 0.5: 1-auc
    if (any(auc < 0.5)) {
        data[, auc < 0.5] <- -data[, auc < 0.5]
        auc[auc < 0.5] <- 1 - auc[auc < 0.5]
        markern <- as.matrix(data[!id.pos,])
        markerp <- as.matrix(data[id.pos,])
    }

    V10 <- matrix(0, nrow = np, ncol = nauc)
    V01 <- matrix(0, nrow = nn, ncol = nauc)

    tmn <- t(markern)
    tmp <- t(markerp)
    for (i in 1:np) {
        V10[i,] <- rowSums(tmn < tmp[, i]) + 0.5 * rowSums(tmn == tmp[, i])
    }
    for (i in 1:nn) {
        V01[i,] <- rowSums(tmp > tmn[, i]) + 0.5 * rowSums(tmp == tmn[, i])
    }
    V10 <- V10 / nn
    V01 <- V01 / np

    W10 <- cov(V10)
    W01 <- cov(V01)

    # Estimated covariance matrix
    S <- W10 / np + W01 / nn

    # Compute variances of AUCs and test for AUC > 0.5
    # Hanley, McNeil (1982)
    q1 <- auc / (2 - auc)
    q2 <- 2 * auc ^ 2 / (1 + auc)

    # Haney, McNeil (1982) / Bamber (1975)
    aucvar <- (auc * (1 - auc) + (np - 1) * (q1 - auc ^ 2) + (nn - 1) * (q2 - auc ^ 2)) / (np * nn)
    zhalf <- (auc - 0.5) / sqrt(aucvar)
    phalf <- 1 - pnorm(zhalf)
    zdelong <- (auc - 0.5) / sqrt(diag(S))
    pdelong <- 1 - pnorm(zdelong)

    # Global p-value
    aucdiff <- L %*% auc
    z <- t(aucdiff) %*% MASS::ginv(L %*% S %*% t(L)) %*% aucdiff
    p <- pchisq(z, df = qr(L %*% S %*% t(L))$rank, lower.tail = FALSE)

    if (is.null(ref)) {
        cor.auc <- matrix(ncol = 1, nrow = nauc * (nauc - 1) / 2)
        ci <- matrix(ncol = 2, nrow = nauc * (nauc - 1) / 2)
        ctr <- 1
        rows <- vector("character", length = (nauc * (nauc - 1) / 2))
        pairp <- matrix(nrow = nauc * (nauc - 1) / 2, ncol = 1)
        quantil <- qnorm(1 - (1 - conf.level) / 2)
        for (i in 1:(nauc - 1)) {
            for (j in (i + 1):nauc) {
                cor.auc[ctr] <- S[i, j] / sqrt(S[i, i] * S[j, j])
                LSL <- t(c(1, -1)) %*% S[c(j, i), c(j, i)] %*% c(1, -1)
                tmpz <- (aucdiff[ctr]) %*% MASS::ginv(LSL) %*% aucdiff[ctr]
                pairp[ctr] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
                ci[ctr,] <- c(aucdiff[ctr] - quantil * sqrt(LSL), aucdiff[ctr] + quantil * sqrt(LSL))
                rows[ctr] <- paste(i, j, sep = " vs. ")
                ctr <- ctr + 1
            }
        }
    } else {
        cor.auc <- matrix(ncol = 1, nrow = nauc - 1)
        ci <- matrix(ncol = 2, nrow = nauc - 1)
        rows <- vector("character", length = nauc - 1)
        pairp <- matrix(nrow = nauc - 1, ncol = 1)
        comp <- (1:nauc)[-ref]
        for (i in 1:(nauc - 1)) {
            cor.auc[i] <- S[ref, comp[i]] / sqrt(S[ref, ref] * S[comp[i], comp[i]])
            LSL <- t(c(1, -1)) %*% S[c(ref, comp[i]), c(ref, comp[i])] %*% c(1, -1)
            tmpz <- aucdiff[i] %*% MASS::ginv(LSL) %*% aucdiff[i]
            pairp[i] <- 1 - pchisq(tmpz, df = qr(LSL)$rank)
            ci[i,] <- c(aucdiff[i] - quantil * sqrt(LSL), aucdiff[i] + quantil * sqrt(LSL))
            rows[i] <- paste(ref, comp[i], sep = " vs. ")
        }
    }

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
    class(ERG) <- "DeLong"
    ERG
}

## print.DeLong ----
#' @export print.DeLong
print.DeLong = function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("Estimated AUC's:\n")
    print(format(round(x$AUC, digits = digits, ...), nsmall = digits, ...))
    cat("\n Pairwise comparisons:\n")
    print(format(round(x$difference, digits = digits, ...), nsmall = digits, ...))
    cat(paste("\n Overall test:\n p-value =", format.pval(x$global.p, digits = digits), "\n"))
}

# Main analysis class ----
psychopdarocClass = if (requireNamespace('jmvcore'))
    R6::R6Class(
        "psychopdarocClass",
        inherit = psychopdarocBase,
        private = list(
            # Private fields to store result objects
            ## rocDataList ----
            .rocDataList = list(),

            ## optimalCriteriaList ----
            .optimalCriteriaList = list(),
            ## prevalenceList ----
            .prevalenceList = list(),

            ## Cost-ratio optimized cutpoint method ----
            .calculateCostRatioOptimal = function(confusionMatrix, prevalence, costRatio) {
              # Initialize variables to store results
              best_score <- -Inf
              best_idx <- 1

              # Calculate sensitivity and specificity for each possible threshold
              n_thresholds <- length(confusionMatrix$x.sorted)
              scores <- numeric(n_thresholds)

              for (i in 1:n_thresholds) {
                # Calculate sensitivity and specificity
                tp <- confusionMatrix$tp[i]
                fp <- confusionMatrix$fp[i]
                tn <- confusionMatrix$tn[i]
                fn <- confusionMatrix$fn[i]

                sensitivity <- tp / (tp + fn)
                specificity <- tn / (tn + fp)

                # Calculate utility score considering cost ratio
                # Higher score means better balance considering costs
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

            ## Equal sensitivity/specificity method ----
            .calculateEqualSensSpec = function(confusionMatrix) {
                # Initialize variables to store results
                n_thresholds <- length(confusionMatrix$x.sorted)
                differences <- numeric(n_thresholds)

                for (i in 1:n_thresholds) {
                    # Calculate sensitivity and specificity
                    tp <- confusionMatrix$tp[i]
                    fp <- confusionMatrix$fp[i]
                    tn <- confusionMatrix$tn[i]
                    fn <- confusionMatrix$fn[i]

                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)

                    # Calculate absolute difference between sensitivity and specificity
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

            ## Closest to (0,1) point method ----
            .calculateClosestToOptimal = function(confusionMatrix) {
                # Initialize variables to store results
                n_thresholds <- length(confusionMatrix$x.sorted)
                distances <- numeric(n_thresholds)

                for (i in 1:n_thresholds) {
                    # Calculate sensitivity and specificity
                    tp <- confusionMatrix$tp[i]
                    fp <- confusionMatrix$fp[i]
                    tn <- confusionMatrix$tn[i]
                    fn <- confusionMatrix$fn[i]

                    sensitivity <- tp / (tp + fn)
                    specificity <- tn / (tn + fp)

                    # Calculate Euclidean distance to (0,1) point in ROC space
                    # In ROC space, the x-axis is 1-specificity, so we use (1-specificity, sensitivity)
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








            ## init ----
            .init = function() {
                # Add new plot items
                if (self$options$showCriterionPlot)
                    self$results$criterionPlot$setVisible(TRUE)
                if (self$options$showPrevalencePlot)
                    self$results$prevalencePlot$setVisible(TRUE)
                if (self$options$showDotPlot)
                    self$results$dotPlot$setVisible(TRUE)
            },

            ## run ----
            .run = function() {

                ### Description & Messages ----
                if (is.null(self$options$classVar) ||
                    is.null(self$options$dependentVars)) {
                    self$results$instructions$setContent(
                        "<html>
            <head>
            </head>
            <body>
            This function was originally developed by Lucas Friesen in pschoPDA module. <a href='https://github.com/ClinicoPath/jamoviPsychoPDA'>The original module</a> is no longer maintained. The testroc function with aditional features are added to the meddecide module.
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
                    self$results$instructions$setVisible(visible = FALSE)

                    # Create procedure notes
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
                            "<p> Positive Class: ", as.character(unique(self$data[,self$options$classVar])[1]), "</p>")
                    } else {
                        procedureNotes <- paste0(procedureNotes,
                                                 "<p> Positive Class: ",
                                                 self$options$positiveClass,
                                                 "</p>")
                    }

                    # Was there subgrouping?
                    if (!is.null(self$options$subGroup)) {
                        procedureNotes <- paste0(procedureNotes,
                                                 "<p> Sub-Group Variable: ",
                                                 self$options$subGroup,
                                                 "</p>")
                    }

                    # Add other analysis settings
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

                    # If bootstrapping happened
                    if (self$options$boot_runs != 0) {
                        procedureNotes <- paste0(procedureNotes,
                                                 "<p> Bootstrap Runs: ",
                                                 self$options$boot_runs,
                                                 "</p>")
                    }

                    # Close the notes
                    procedureNotes <- paste0(
                        procedureNotes,
                        "<hr /></body></html>"
                    )
                    self$results$procedureNotes$setContent(procedureNotes)
                }

                ### Get data ----
                data <- self$data

                ### Set up method ----
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
                    } else if (methodName == "oc_cost_ratio") {
                      # For custom methods, we'll handle this specially in the analysis section
                      method <- cutpointr::maximize_metric  # Use a placeholder, we'll override later
                    } else if (methodName == "oc_equal_sens_spec") {
                      # Custom method, will handle specially
                      method <- cutpointr::maximize_metric  # Placeholder
                    } else if (methodName == "oc_closest_01") {
                      # Custom method, will handle specially
                      method <- cutpointr::maximize_metric  # Placeholder
                    } else {
                      method <- cutpointr::maximize_metric
                    }

                    score <- NULL
                }


                # Calculate prevalence - either from data or user-specified
                if (self$options$usePriorPrev) {
                  prevalence <- self$options$priorPrev
                } else {
                  n_pos <- sum(classVar == pos_class)
                  n_neg <- sum(classVar != pos_class)
                  prevalence <- n_pos / (n_pos + n_neg)
                }


                ### Set up tolerance metric if needed ----
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

                ### Set up metric function ----
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
                    metric <- cutpointr::youden
                }

                ### Set up break_ties function ----
                if (self$options$break_ties == "c") {
                    break_ties <- c
                } else if (self$options$break_ties == "mean") {
                    break_ties <- mean
                } else if (self$options$break_ties == "median") {
                    break_ties <- median
                } else {
                    break_ties <- mean
                }

                ### Get other parameters ----
                direction <- self$options$direction
                boot_runs <- as.numeric(self$options$boot_runs)

                ### Set up for plot data collection ----
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

                #### Process variables ----
                vars <- self$options$dependentVars

                #### Handle subgroups if present ----
                if (!is.null(self$options$subGroup)) {
                    subGroup <- data[, self$options$subGroup]
                    classVar <- data[, self$options$classVar]
                    uniqueGroups <- unique(subGroup)
                    vars <- apply(expand.grid(vars, uniqueGroups), 1, function(x) paste(x, collapse="_"))
                } else {
                    subGroup <- NULL
                }

                #### Storage for AUCs ----
                aucList <- list()

                #### Process each variable ----
                for (var in vars) {
                    ##### Add items to results tables ----
                    if (!var %in% self$results$resultsTable$itemKeys) {
                        self$results$sensSpecTable$addItem(key = var)
                        self$results$resultsTable$addItem(key = var)
                        if (self$options$combinePlots == FALSE) {
                            self$results$plotROC$addItem(key = var)

                            ##### Add the additional plot items if enabled ----
                            if (self$options$showCriterionPlot)
                                self$results$criterionPlot$addItem(key = var)
                            if (self$options$showPrevalencePlot)
                                self$results$prevalencePlot$addItem(key = var)
                            if (self$options$showDotPlot)
                                self$results$dotPlot$addItem(key = var)
                        }
                    }

                    ##### Extract data for analysis ----
                    if (is.null(subGroup)) {
                        dependentVar <- as.numeric(data[, var])
                        classVar <- data[, self$options$classVar]
                    } else {
                        # Split the combined var_group name
                        varParts <- strsplit(var, split = "_")[[1]]
                        varName <- varParts[1]
                        groupName <- paste(varParts[-1], collapse="_")

                        # Filter data for this group
                        dependentVar <- as.numeric(data[subGroup == groupName, varName])
                        classVar <- data[subGroup == groupName, self$options$classVar]
                    }

                    ##### Get positive class ----
                    if (self$options$positiveClass == "") {
                        pos_class <- as.character(unique(classVar)[1])
                    } else {
                        pos_class <- self$options$positiveClass
                    }


                    #### Run the ROC analysis ----
                    result_success <- FALSE
                    result_message <- NULL

                    ##### First attempt - use cutpointr ----
                    tryCatch({
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

                    ##### If cutpointr failed, try alternative implementation ----
                    if (!result_success) {
                        self$results$procedureNotes$setContent(paste0(
                            self$results$procedureNotes$content,
                            "<p><strong>Note:</strong> Standard ROC analysis failed with error: '",
                            result_message,
                            "'. Using alternative implementation.</p>"
                        ))

                        ###### Convert to binary response ----
                        response <- as.numeric(classVar == pos_class)

                        ###### Create minimal ROC data ----
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

                        # Calculate simple AUC using trapezoidal rule
                        sens <- roc_data$tp / (roc_data$tp + roc_data$fn)
                        spec <- roc_data$tn / (roc_data$tn + roc_data$fp)

                        # Compute ROC curve points
                        roc_points <- data.frame(
                            specificity = spec,
                            sensitivity = sens
                        )
                        # Order by increasing 1-specificity
                        roc_points <- roc_points[order(1-roc_points$specificity),]

                        # Calculate AUC using trapezoidal rule
                        auc <- 0
                        for(i in 2:nrow(roc_points)) {
                            # Area of trapezoid
                            x_diff <- (1-roc_points$specificity[i]) - (1-roc_points$specificity[i-1])
                            y_avg <- (roc_points$sensitivity[i] + roc_points$sensitivity[i-1])/2
                            auc <- auc + x_diff * y_avg
                        }

                        # Find optimal cutpoint (using Youden's index)
                        youdens_j <- sens + spec - 1
                        optimal_idx <- which.max(youdens_j)

                        # Build minimal result object
                        results <- list(
                            optimal_cutpoint = roc_data$x.sorted[optimal_idx],
                            roc_curve = list(roc_data),
                            AUC = auc
                        )
                    }



                    # If using a custom method that isn't directly supported by cutpointr
                    if (!result_success && self$options$method %in% c("oc_cost_ratio", "oc_equal_sens_spec", "oc_closest_01")) {
                      # Get the confusion matrix data
                      confusionMatrix <- results$roc_curve[[1]]

                      if (self$options$method == "oc_cost_ratio") {
                        # Use the custom cost ratio optimization function
                        cost_results <- private$.calculateCostRatioOptimal(
                          confusionMatrix,
                          prevalence,
                          self$options$costratioFP
                        )
                        # Override the optimal cutpoint
                        results$optimal_cutpoint <- confusionMatrix$x.sorted[cost_results$optimal_idx]
                      }
                      else if (self$options$method == "oc_equal_sens_spec") {
                        eq_results <- private$.calculateEqualSensSpec(confusionMatrix)
                        results$optimal_cutpoint <- confusionMatrix$x.sorted[eq_results$optimal_idx]
                      }
                      else if (self$options$method == "oc_closest_01") {
                        closest_results <- private$.calculateClosestToOptimal(confusionMatrix)
                        results$optimal_cutpoint <- confusionMatrix$x.sorted[closest_results$optimal_idx]
                      }
                    }



                    # Determine which cutpoints to display
                    if (!self$options$allObserved) {
                        resultsToDisplay <- unlist(results$optimal_cutpoint)
                    } else {
                        resultsToDisplay <- sort(unique(dependentVar))
                    }

                    # Get ROC curve data
                    confusionMatrix <- results$roc_curve[[1]]

                    # Filter confusion matrix to show only relevant cutpoints
                    if (!self$options$allObserved) {
                        confusionMatrixForTable <- confusionMatrix[which(confusionMatrix$x.sorted %in% resultsToDisplay),]
                    } else {
                        confusionMatrixForTable <- confusionMatrix
                    }

                    # Generate sensitivity-specificity tables if requested
                    if (self$options$sensSpecTable) {
                        # Generate individual tables for each cutpoint
                        for (i in seq_along(resultsToDisplay)) {
                            cp <- resultsToDisplay[i]
                            # Find the closest cutpoint in the confusion matrix
                            idx <- which.min(abs(confusionMatrix$x.sorted - cp))

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

                    # Calculate performance metrics
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

                    metricList <- metric(
                        tp = confusionMatrix$tp,
                        fp = confusionMatrix$fp,
                        tn = confusionMatrix$tn,
                        fn = confusionMatrix$fn
                    )

                    # Create results table
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

                    # Save AUC value
                    aucList[[var]] <- results$AUC

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

                    # Save optimal criterion data
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

                    # Calculate prevalence
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

                    # Prepare plotting data
                    if (self$options$plotROC == TRUE) {
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
                }

                # Create combined plot if requested
                if (self$options$plotROC == TRUE && self$options$combinePlots == TRUE) {
                    if (nrow(plotDataList) > 0) {
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
                            # Add a message item if it doesn't exist
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
                }

                # Combined prevalence plot if enabled ----
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



                # Perform DeLong's test if requested ----
                if (self$options$delongTest == TRUE) {
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
                            pos_class = pos_class,
                            conf.level = 0.95
                        )

                        # Display results
                        self$results$delongTest$setVisible(visible = TRUE)
                        self$results$delongTest$setContent(paste0(capture.output(print.DeLong(delongResults)), collapse = "\n"))
                    }
                }


                # Create simplified results table ----
                simpleTable <- self$results$simpleResultsTable

                # Add rows for each variable
                for (var in names(aucList)) {
                    # Calculate CI if not already available
                    auc_value <- aucList[[var]]
                    n_pos <- sum(data[, self$options$classVar] == pos_class)
                    n_neg <- sum(data[, self$options$classVar] != pos_class)
                    auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))

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


                # Create AUC summary table ----
                aucSummaryTable <- self$results$aucSummaryTable

                # Add rows for each variable
                for (var in names(aucList)) {
                    # Calculate CI if not already available
                    auc_value <- aucList[[var]]
                    n_pos <- sum(data[, self$options$classVar] == pos_class)
                    n_neg <- sum(data[, self$options$classVar] != pos_class)
                    auc_se <- sqrt((auc_value * (1 - auc_value)) / (n_pos * n_neg))

                    z_critical <- qnorm(0.975)
                    auc_lci <- max(0, auc_value - z_critical * auc_se)
                    auc_uci <- min(1, auc_value + z_critical * auc_se)

                    # Calculate p-value against null hypothesis AUC = 0.5
                    z_stat <- (auc_value - 0.5) / auc_se
                    p_val <- 2 * (1 - pnorm(abs(z_stat)))

                    # Add row to AUC summary table
                    aucSummaryTable$addRow(rowKey = var, values = list(
                        variable = var,
                        auc = auc_value,
                        ci_lower = auc_lci,
                        ci_upper = auc_uci,
                        p = p_val
                    ))
                }

                # Create threshold table if requested ----

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




                # Format DeLong test results if available ----
                if (self$options$delongTest && !is.null(delongResults)) {
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



                # Determine which prevalence to use ----
                if (self$options$usePriorPrev) {
                    # Use specified prevalence
                    prevalence <- self$options$usePriorPrevrob
                } else {
                    # Calculate from data
                    n_pos <- sum(classVar == pos_class)
                    n_neg <- sum(classVar != pos_class)
                    prevalence <- n_pos / (n_pos + n_neg)
                }

                # Calculate optimal criteria using different methods
                if (self$options$optimcrit) {
                    # 1. Youden Index (J) - maximize sensitivity + specificity - 1
                    j_max_idx <- which.max(coords$J)

                    # 2. Cost ratio optimized
                    cost_results <- private$.calculateCostRatioOptimal(
                        confusionMatrix,
                        prevalence,
                        self$options$costratioFP
                    )
                    cost_max_idx <- cost_results$optimal_idx

                    # 3. Closest to (0,1) point
                    closest_results <- private$.calculateClosestToOptimal(confusionMatrix)
                    closest_idx <- closest_results$optimal_idx

                    # 4. Equal sensitivity and specificity
                    eq_results <- private$.calculateEqualSensSpec(confusionMatrix)
                    eq_idx <- eq_results$optimal_idx

                    # Store optimal criteria
                    optimalCriteria <- data.frame(
                        type = c(
                            "Youden Index (J)",
                            "Cost-Ratio Optimized",
                            "Closest to (0,1)",
                            "Equal Sensitivity & Specificity"
                        ),
                        threshold = c(
                            coords$threshold[j_max_idx],
                            coords$threshold[cost_max_idx],
                            coords$threshold[closest_idx],
                            coords$threshold[eq_idx]
                        ),
                        sens = c(
                            coords$sens[j_max_idx],
                            coords$sens[cost_max_idx],
                            coords$sens[closest_idx],
                            coords$sens[eq_idx]
                        ),
                        spec = c(
                            coords$spec[j_max_idx],
                            coords$spec[cost_max_idx],
                            coords$spec[closest_idx],
                            coords$spec[eq_idx]
                        ),
                        ppv = c(
                            coords$ppv[j_max_idx],
                            coords$ppv[cost_max_idx],
                            coords$ppv[closest_idx],
                            coords$ppv[eq_idx]
                        ),
                        npv = c(
                            coords$npv[j_max_idx],
                            coords$npv[cost_max_idx],
                            coords$npv[closest_idx],
                            coords$npv[eq_idx]
                        ),
                        j = c(
                            coords$J[j_max_idx],
                            coords$J[cost_max_idx],
                            coords$J[closest_idx],
                            coords$J[eq_idx]
                        )
                    )

                    private$.optimalCriteria <- optimalCriteria
                }












                # Check if quantile CIs are requested
                if (self$options$quantileCIs) {
                    # Parse the quantiles string
                    quantiles_str <- self$options$quantiles
                    quantiles_vec <- as.numeric(unlist(strsplit(quantiles_str, ",")))

                    # Ensure quantiles are valid
                    quantiles_vec <- quantiles_vec[quantiles_vec >= 0 & quantiles_vec <= 1]

                    if (length(quantiles_vec) > 0) {
                        # For each variable in the plot
                        for (var_name in unique(plotData$var)) {
                            var_data <- plotData[plotData$var == var_name, ]

                            # Extract the predictor values for this variable
                            predictor_values <- NULL
                            if (!is.null(attr(private$.rocDataList[[var_name]], "rawData"))) {
                                predictor_values <- attr(private$.rocDataList[[var_name]], "rawData")$value
                            } else {
                                # If raw data not available, skip CI calculation
                                next
                            }

                            # Calculate predictor quantiles
                            pred_quantiles <- quantile(predictor_values, probs = quantiles_vec, na.rm = TRUE)

                            # For each quantile, find the nearest threshold in our ROC data
                            for (q in pred_quantiles) {
                                # Find closest threshold
                                idx <- which.min(abs(var_data$cutpoint - q))

                                if (length(idx) > 0) {
                                    # Get coordinates
                                    x <- 1 - var_data$specificity[idx]
                                    y <- var_data$sensitivity[idx]

                                    # Calculate binomial confidence intervals
                                    n_pos <- sum(response == 1)
                                    n_neg <- sum(response == 0)

                                    sens_ci <- binom::binom.confint(round(y * n_pos), n_pos,
                                                                    methods = "wilson", conf.level = 0.95)
                                    spec_ci <- binom::binom.confint(round(var_data$specificity[idx] * n_neg), n_neg,
                                                                    methods = "wilson", conf.level = 0.95)

                                    # Convert specificity CI to 1-specificity for plotting
                                    spec_ci_x <- 1 - c(spec_ci$lower, spec_ci$upper)

                                    # Add CI to plot
                                    plot <- plot +
                                        # Horizontal line for sensitivity CI
                                        ggplot2::geom_errorbar(
                                            data = data.frame(x = x, y = y,
                                                              ymin = sens_ci$lower,
                                                              ymax = sens_ci$upper,
                                                              var = var_name),
                                            ggplot2::aes(x = x, y = y, ymin = ymin, ymax = ymax, color = var),
                                            width = 0.02
                                        ) +
                                        # Vertical line for specificity CI
                                        ggplot2::geom_errorbarh(
                                            data = data.frame(x = x, y = y,
                                                              xmin = min(spec_ci_x),
                                                              xmax = max(spec_ci_x),
                                                              var = var_name),
                                            ggplot2::aes(x = x, y = y, xmin = xmin, xmax = xmax, color = var),
                                            height = 0.02
                                        ) +
                                        # Point at the quantile
                                        ggplot2::geom_point(
                                            data = data.frame(x = x, y = y, var = var_name),
                                            ggplot2::aes(x = x, y = y, color = var),
                                            size = 3
                                        ) +
                                        # Label with quantile value
                                        ggplot2::geom_text(
                                            data = data.frame(x = x, y = y, var = var_name,
                                                              label = paste0("q=", round(quantiles_vec[which(pred_quantiles == q)], 2))),
                                            ggplot2::aes(x = x, y = y, label = label, color = var),
                                            hjust = -0.3, vjust = -0.3,
                                            size = 3
                                        )
                                }
                            }
                        }
                    }
                }




            },


            # plotROC ----
            .plotROC = function(image, ggtheme, theme, ...) {
                    plotData <- data.frame(image$state)

                    if (nrow(plotData) == 0) return(FALSE)

                    ## Base plot setup ----
                    if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
                        ### Multiple variables in one plot ----
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
                        ### Single variable plot ----
                        plot <- ggplot2::ggplot(plotData,
                                                ggplot2::aes(
                                                    x = 1 - specificity,
                                                    y = sensitivity
                                                )) +
                            ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
                            ggplot2::geom_line(size = 1) +
                            ggplot2::geom_point(size = 0.5, alpha = ifelse(self$options$cleanPlot, 0, 0.7))
                    }

                    ## Common plot elements ----
                    plot <- plot +
                        ggplot2::xlab("1 - Specificity (False Positive Rate)") +
                        ggplot2::ylab("Sensitivity (True Positive Rate)") +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1)

                    ## Apply theme based on clean plot option ----
                    if (self$options$cleanPlot) {
                        plot <- plot +
                            ggplot2::theme_minimal() +
                            ggplot2::theme(
                                panel.grid.minor = ggplot2::element_blank(),
                                plot.title = ggplot2::element_blank(),
                                plot.subtitle = ggplot2::element_blank(),
                                panel.border = ggplot2::element_rect(color = "black", fill = NA)
                            )

                        ## Handle legend positioning for clean plots ----
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
                        plot <- plot + ggtheme
                    }

                    ## Add smoothing if requested ----
                    if (self$options$smoothing) {
                        plot <- plot + ggplot2::geom_smooth(se = self$options$displaySE)
                    }

                    ## Mark optimal points if not clean plot or if specifically requested ----
                    if (!self$options$cleanPlot || self$options$showOptimalPoint) {
                        if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
                            ### For combined plot, find optimal points for each variable ----
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
                            ### For single variable plot ----
                            if ('j_max_idx' %in% names(plotData)) {
                                j_max_idx <- unique(plotData$j_max_idx)
                                if (length(j_max_idx) == 1 && !is.na(j_max_idx)) {
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

                    ## Add AUC annotations for both individual and combined plots ----
                    if (!self$options$cleanPlot) {
                        if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
                            # Create a data frame for annotations
                            auc_data <- aggregate(AUC ~ var, data = plotData, FUN = function(x) x[1])
                            auc_data$AUC_formatted <- sprintf("AUC = %.3f", auc_data$AUC)
                            auc_data$x <- 0.75  # Position for annotations
                            auc_data$y <- seq(0.3, 0.1, length.out = nrow(auc_data))

                            plot <- plot +
                                ggplot2::geom_text(data = auc_data,
                                                   ggplot2::aes(x = x, y = y, label = AUC_formatted, color = var),
                                                   hjust = 0, show.legend = FALSE)
                        } else {
                            # For single variable, add AUC in the corner
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

                    ## Add direct labels if requested ----
                    if (self$options$directLabel && self$options$combinePlots) {
                        # Get unique variables and their AUCs
                        unique_vars <- unique(plotData$var)
                        label_points <- data.frame()

                        for (var_name in unique_vars) {
                            var_data <- plotData[plotData$var == var_name,]
                            j_max_idx <- which.max(var_data$youden)
                            if (length(j_max_idx) > 0) {
                                label_points <- rbind(label_points, var_data[j_max_idx,])
                            }
                        }

                        # Add text labels to the plot
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

                    print(plot)
                    return(TRUE)
                },

            # Sensitivity/Specificity vs. Criterion ----
            .plotCriterion = function(image, ggtheme, theme, ...) {
                plotData <- image$state

                if (is.null(plotData) || (is.data.frame(plotData) && nrow(plotData) == 0))
                    return(FALSE)

                # Check if we have a combined plot
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
                    # Single variable
                    plot_data_long <- tidyr::gather(
                        plotData,
                        key = "metric",
                        value = "value",
                        sensitivity, specificity
                    )

                    plot_data_long$metric <- factor(
                        plot_data_long$metric,
                        levels = c("sensitivity", "specificity"),
                        labels = c("Sensitivity", "Specificity")
                    )

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

            # Predictive Values vs. Prevalence ----
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
                ppv_vals <- (optimal$sensitivity * prev_seq) /
                    ((optimal$sensitivity * prev_seq) + ((1 - optimal$specificity) * (1 - prev_seq)))

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

            # Dot Plot ----
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
                    ggplot2::scale_color_manual(
                        values = c("Negative" = "darkblue", "Positive" = "darkred")
                    ) +
                    ggplot2::labs(
                        title = "Distribution of Values by Class",
                        x = "Class",
                        y = "Value"
                    ) +
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

                print(plot)
                return(TRUE)
            },

            # Interactive ROC Plot ----
            .plotInteractiveROC = function(image, ggtheme, theme, ...) {
                # Get data from state
                plotData <- image$state

                # Create basic plot using ggplot2
                p <- ggplot2::ggplot(plotData,
                                     ggplot2::aes(d = response, m = predictor)) +
                    plotROC::geom_roc(n.cuts = 10) +
                    plotROC::style_roc(theme = ggtheme)

                # Convert to interactive plot
                interactive_plot <- plotROC::plot_interactive_roc(p)

                print(interactive_plot)
                return(TRUE)
            }



        )
    )
