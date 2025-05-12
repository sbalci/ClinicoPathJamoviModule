#' @title ROC Analysis
#' @return Table
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @import cutpointr
#' @importFrom MASS ginv

# Utility functions
print.sensSpecTable <- function(Title, TP, FP, TN, FN) {
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

deLong.test <- function(data, classVar, pos_class, ref = NULL, conf.level = 0.95) {
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

print.DeLong <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("Estimated AUC's:\n")
    print(format(round(x$AUC, digits = digits, ...), nsmall = digits, ...))
    cat("\n Pairwise comparisons:\n")
    print(format(round(x$difference, digits = digits, ...), nsmall = digits, ...))
    cat(paste("\n Overall test:\n p-value =", format.pval(x$global.p, digits = digits), "\n"))
}

# Main analysis class
psychopdarocClass <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "psychopdarocClass",
        inherit = psychopdarocBase,
        private = list(
            .init = function() {
                # Initialization code if needed
            },

            .run = function() {
                if (is.null(self$options$classVar) ||
                    is.null(self$options$dependentVars)) {
                    self$results$instructions$setContent(
                        "<html>
            <head>
            </head>
            <body>
            <div class='instructions'>
            <p><b>ROC Analysis for Medical Decision Making</b></p>
            <p>This analysis creates Receiver Operating Characteristic (ROC) curves and calculates optimal cutpoints for diagnostic tests.</p>
            <p>To get started:</p>
            <ol>
            <li>Place the test result variable(s) in the 'Dependent Variable' slot<br /><br /></li>
            <li>Place the binary classification (gold standard) in the 'Class Variable' slot<br /><br /></li>
            <li>[<em>Optional</em>] Place a grouping variable in the 'Group Variable' slot<br /><br /></li>
            </ol>
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

                # Get data
                data <- self$data

                # Set up method
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
                    } else {
                        method <- cutpointr::maximize_metric
                    }
                    score <- NULL
                }

                # Set up tolerance metric if needed
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

                # Get other parameters
                direction <- self$options$direction
                boot_runs <- as.numeric(self$options$boot_runs)

                # Set up for plot data collection
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

                # Process variables
                vars <- self$options$dependentVars

                # Handle subgroups if present
                if (!is.null(self$options$subGroup)) {
                    subGroup <- data[, self$options$subGroup]
                    classVar <- data[, self$options$classVar]
                    uniqueGroups <- unique(subGroup)
                    vars <- apply(expand.grid(vars, uniqueGroups), 1, function(x) paste(x, collapse="_"))
                } else {
                    subGroup <- NULL
                }

                # Storage for AUCs
                aucList <- list()

                # Process each variable
                for (var in vars) {
                    # Add items to results tables
                    if (!var %in% self$results$resultsTable$itemKeys) {
                        self$results$sensSpecTable$addItem(key = var)
                        self$results$resultsTable$addItem(key = var)
                        if (self$options$combinePlots == FALSE) {
                            self$results$plotROC$addItem(key = var)
                        }
                    }

                    # Extract data for analysis
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

                    # Get positive class
                    if (self$options$positiveClass == "") {
                        pos_class <- as.character(unique(classVar)[1])
                    } else {
                        pos_class <- self$options$positiveClass
                    }

                    # Run the ROC analysis
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
                                    stringsAsFactors = FALSE
                                )
                            )
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
                    }
                }

                # Perform DeLong's test if requested
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
            },

            .plotROC = function(image, ggtheme, theme, ...) {
                plotData <- data.frame(image$state)

                # Create the ROC plot
                if (nrow(plotData) > 0) {
                    if (self$options$combinePlots == TRUE && length(unique(plotData$var)) > 1) {
                        # Multiple variables in one plot
                        plot <- ggplot2::ggplot(plotData,
                                                ggplot2::aes(
                                                    x = 1 - specificity,
                                                    y = sensitivity,
                                                    color = var
                                                ))
                    } else {
                        # Single variable plot
                        plot <- ggplot2::ggplot(plotData,
                                                ggplot2::aes(
                                                    x = 1 - specificity,
                                                    y = sensitivity
                                                ))
                    }

                    # Build the plot
                    plot <- plot +
                        ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.7) +
                        ggplot2::geom_point() +
                        ggplot2::geom_line() +
                        ggplot2::xlab("1 - Specificity (False Positive Rate)") +
                        ggplot2::ylab("Sensitivity (True Positive Rate)") +
                        ggplot2::xlim(0, 1) +
                        ggplot2::ylim(0, 1) +
                        ggtheme +
                        ggplot2::theme(plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5))

                    # Add smoothing if requested
                    if (self$options$smoothing) {
                        if (self$options$displaySE) {
                            plot <- plot + ggplot2::geom_smooth(se = TRUE)
                        } else {
                            plot <- plot + ggplot2::geom_smooth(se = FALSE)
                        }
                    }

                    # Add AUC annotations for combined plot
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
                    }

                    print(plot)
                    return(TRUE)
                }

                return(FALSE)
            }
        )
    )
