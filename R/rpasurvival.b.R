
rpasurvivalClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "rpasurvivalClass",
    inherit = rpasurvivalBase,
    private = list(
        # Escape variable names for safe formula construction
        .escapeVar = function(x) {
            # Use jmvcore::composeTerm for safe variable names
            jmvcore::composeTerm(x)
        },

        # Get time multiplier for 5-year survival calculation
        .getTime5Year = function() {
            switch(self$options$time_unit,
                "days" = 365.25 * 5,
                "months" = 60,
                "years" = 5,
                60  # default months
            )
        },

        # Initialize notice list
        .noticeList = list(),

        # Add a notice
        .addNotice = function(type, title, content) {
            notice <- list(type = type, title = title, content = content)
            private$.noticeList[[length(private$.noticeList) + 1]] <- notice
        },

        # Render all notices as HTML
        .renderNotices = function() {
            if (length(private$.noticeList) == 0) {
                self$results$notices$setContent("")
                return()
            }

            html <- '<div style="font-family: Arial, sans-serif; margin: 10px;">'

            for (notice in private$.noticeList) {
                color <- switch(notice$type,
                    "ERROR" = "#d9534f",
                    "WARNING" = "#f0ad4e",
                    "INFO" = "#5bc0de",
                    "#777"
                )

                icon <- switch(notice$type,
                    "ERROR" = "&#x2716;",
                    "WARNING" = "&#x26A0;",
                    "INFO" = "&#x2139;",
                    "&#x2022;"
                )

                html <- paste0(html,
                    '<div style="margin: 8px 0; padding: 10px; background-color: ', color, '22; border-left: 4px solid ', color, '; border-radius: 3px;">',
                    '<strong style="color: ', color, ';">', icon, ' ', notice$title, '</strong><br/>',
                    '<span style="color: #333;">', notice$content, '</span>',
                    '</div>'
                )
            }

            html <- paste0(html, '</div>')
            self$results$notices$setContent(html)
        },

        # Hide all analytical results (used when analysis fails)
        .hideResults = function() {
            self$results$summary$setVisible(FALSE)
            self$results$interpretation$setVisible(FALSE)
            self$results$report$setVisible(FALSE)
            self$results$treeplot$setVisible(FALSE)
            self$results$riskgrouptable$setVisible(FALSE)
            self$results$kmplot$setVisible(FALSE)
            self$results$logranktest$setVisible(FALSE)
            self$results$cptable$setVisible(FALSE)
            self$results$varimp$setVisible(FALSE)
            self$results$coxmodel$setVisible(FALSE)
        },

        # Show all analytical results (used when analysis succeeds)
        .showResults = function() {
            # Show main results
            self$results$summary$setVisible(self$options$showSummary)
            self$results$interpretation$setVisible(self$options$showInterpretation)
            self$results$report$setVisible(self$options$showReport)

            # Show outputs based on user options
            self$results$treeplot$setVisible(self$options$treeplot)
            self$results$riskgrouptable$setVisible(self$options$riskgrouptable)
            self$results$kmplot$setVisible(self$options$kmplot)
            self$results$logranktest$setVisible(self$options$kmplot)
            self$results$cptable$setVisible(self$options$cptable)
            self$results$varimp$setVisible(self$options$variableimportance)
            self$results$coxmodel$setVisible(self$options$riskgrouptable)
        },

        # Initialize function
        .init = function() {
            # Set instructions
            instructions <- '<div style="font-family: Arial; padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin: 10px 0;">
                <h3 style="color: #0066cc; margin-top: 0;">Recursive Partitioning Analysis for Survival</h3>
                <p><strong>Purpose:</strong> Develop risk stratification groups using binary tree partitioning on survival data.</p>
                <p><strong>Method:</strong> CART (Classification and Regression Trees) for survival endpoints.</p>
                <p><strong>Output:</strong> Terminal nodes become risk groups (e.g., Stage I, II, III).</p>
                <h4 style="color: #0066cc;">Usage Notes:</h4>
                <ul>
                    <li>Requires <strong>Survival Time</strong> (numeric, non-negative) and <strong>Event Status</strong> (0/1 or TRUE/FALSE).</li>
                    <li>Select predictor variables: categorical (stage, grade) or continuous (age, biomarker).</li>
                    <li>Tree is pruned using cross-validation unless disabled.</li>
                    <li>Minimum node size (default=20) controls tree complexity.</li>
                    <li>Log-rank criterion recommended for survival splitting.</li>
                </ul>
                <h4 style="color: #0066cc;">Example Application:</h4>
                <p>Integrate LVI status + ypTNM stage to create RPA staging (as in Liu et al., Br J Cancer 2026).</p>
            </div>'

            self$results$instructions$setContent(instructions)

            # Initialize tables
            self$results$riskgrouptable$setVisible(self$options$riskgrouptable)
            self$results$kmplot$setVisible(self$options$kmplot)
            self$results$treeplot$setVisible(self$options$treeplot)
            self$results$cptable$setVisible(self$options$cptable)
            self$results$varimp$setVisible(self$options$variableimportance)
        },

        # Main run function
        .run = function() {
            # Check for required inputs
            if (is.null(self$options$time) || is.null(self$options$event) ||
                length(self$options$predictors) == 0) {
                # Show instructions when inputs are missing
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("INFO", "Awaiting Input",
                    "Please select Survival Time, Event Status, and at least one Predictor Variable.")
                private$.renderNotices()
                return()
            }

            # Hide instructions and prepare to show results
            self$results$instructions$setVisible(FALSE)

            # Get data
            timeVar <- jmvcore::toNumeric(self$data[[self$options$time]])
            eventVar <- self$data[[self$options$event]]

            # Handle event value
            if (is.factor(eventVar)) {
                eventVar <- as.character(eventVar)
            }

            # Convert event to numeric (1 = event, 0 = censored)
            if (self$options$eventValue == 'TRUE') {
                eventNumeric <- as.numeric(eventVar == 'TRUE' | eventVar == 'true' | eventVar == '1')
            } else {
                eventNumeric <- as.numeric(eventVar == self$options$eventValue)
            }

            # Check for negative times
            if (any(timeVar < 0, na.rm = TRUE)) {
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("ERROR", "Invalid Time Values",
                    "Survival time must be non-negative. Check your data.")
                private$.renderNotices()
                return()
            }

            # Check for minimum sample size
            nTotal <- sum(!is.na(timeVar) & !is.na(eventNumeric))
            nEvents <- sum(eventNumeric, na.rm = TRUE)

            if (nTotal < 50) {
                private$.addNotice("WARNING", "Small Sample Size",
                    paste0("Only ", nTotal, " complete observations. RPA works best with n ≥ 50. Results may be unstable."))
            }

            if (nEvents < 10) {
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("ERROR", "Insufficient Events",
                    paste0("Only ", nEvents, " events observed. Need at least 10 events for RPA."))
                private$.renderNotices()
                return()
            }

            # Check events per predictor
            nPredictors <- length(self$options$predictors)
            eventsPerPredictor <- nEvents / nPredictors
            if (eventsPerPredictor < 10) {
                private$.addNotice("WARNING", "Low Events-Per-Variable Ratio",
                    paste0("Only ", round(eventsPerPredictor, 1), " events per predictor. ",
                           "Recommend ≥10 events per predictor to avoid overfitting."))
            }

            # Critical overfit check: too many predictors
            if (nPredictors > nEvents / 10) {
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("ERROR", "Severe Overfit Risk",
                    paste0("You have ", nPredictors, " predictors but only ", nEvents, " events. ",
                           "This will cause severe overfitting. Recommend ≤ ", floor(nEvents/10),
                           " predictors. Please reduce the number of predictor variables."))
                private$.renderNotices()
                return()
            }

            # Warning for very small minbucket
            if (self$options$minbucket < 10) {
                private$.addNotice("WARNING", "Very Small Node Size",
                    paste0("Minimum node size (", self$options$minbucket,
                           ") is very small. Risk groups may be unstable. Consider increasing to ≥ 20."))
            }

            # Warning for overly complex trees
            if (self$options$maxdepth > 5) {
                private$.addNotice("WARNING", "Very Deep Tree",
                    paste0("Maximum depth (", self$options$maxdepth,
                           ") may produce an overly complex tree that is hard to interpret clinically. Consider limiting to ≤ 5."))
            }

            # Add proportional hazards assumption notice
            private$.addNotice("INFO", "Statistical Assumptions",
                "RPA assumes proportional hazards (constant hazard ratio over time). For competing risks or crossing hazards, consider alternative methods.")

            # Build predictor data frame with escaped names
            predictorData <- data.frame(lapply(self$options$predictors, function(varName) {
                col <- self$data[[varName]]
                # Handle labelled factors: preserve factor levels but drop labels if present
                if (is.factor(col) && !is.null(attr(col, "jmv-desc"))) {
                    # Keep factor structure, labels will be preserved automatically
                    col
                } else if (is.factor(col)) {
                    col
                } else {
                    col
                }
            }))
            # Use escaped names for safe formula construction
            escapedNames <- sapply(self$options$predictors, private$.escapeVar)
            names(predictorData) <- escapedNames

            # Create complete case dataset
            completeIdx <- complete.cases(timeVar, eventNumeric, predictorData)
            if (sum(!completeIdx) > 0) {
                private$.addNotice("WARNING", "Missing Data",
                    paste0(sum(!completeIdx), " rows with missing data excluded (",
                           round(100*sum(!completeIdx)/length(completeIdx), 1), "%)."))
            }

            timeVar <- timeVar[completeIdx]
            eventNumeric <- eventNumeric[completeIdx]
            predictorData <- predictorData[completeIdx, , drop = FALSE]

            # Create Surv object
            survObj <- survival::Surv(timeVar, eventNumeric)

            # Build formula with escaped variable names
            formula <- as.formula(paste("survObj ~", paste(escapedNames, collapse = " + ")))

            # Combine data
            analysisData <- cbind(data.frame(survObj = survObj), predictorData)

            # Fit rpart with survival outcome
            tryCatch({
                tree <- rpart::rpart(
                    formula,
                    data = analysisData,
                    method = "exp",  # exponential survival (proportional hazards)
                    control = rpart::rpart.control(
                        minbucket = self$options$minbucket,
                        cp = self$options$cp,
                        maxdepth = self$options$maxdepth,
                        xval = self$options$nfolds
                    )
                )
            }, error = function(e) {
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("ERROR", "Model Fitting Failed",
                    paste0("Error: ", e$message))
                private$.renderNotices()
                return()
            })

            # Prune tree if requested
            if (self$options$prunetree && self$options$nfolds > 0) {
                # Find optimal CP based on cross-validation error
                cpTable <- tree$cptable
                optimalCP <- cpTable[which.min(cpTable[, "xerror"]), "CP"]
                tree <- rpart::prune(tree, cp = optimalCP)

                private$.addNotice("INFO", "Tree Pruned",
                    paste0("Tree pruned using ", self$options$nfolds, "-fold cross-validation. Optimal CP = ",
                           round(optimalCP, 4), ". This helps prevent overfitting."))
            } else if (!self$options$prunetree) {
                private$.addNotice("WARNING", "Tree Not Pruned",
                    "Cross-validation pruning is disabled. Tree may overfit the data. Consider enabling pruning for better generalization.")
            }

            # Check if tree has splits
            if (nrow(tree$frame) == 1) {
                self$results$instructions$setVisible(TRUE)
                private$.hideResults()
                private$.addNotice("WARNING", "No Splits Found",
                    "Tree has no splits. Try reducing minbucket or cp parameters.")
                private$.renderNotices()
                return()
            }

            # Assign risk groups (terminal nodes)
            nodeID <- tree$where
            uniqueNodes <- sort(unique(nodeID))
            nGroups <- length(uniqueNodes)

            # Create risk group labels
            riskGroup <- factor(nodeID, levels = uniqueNodes)

            if (self$options$riskgrouplabels == 'auto') {
                # Use Roman numerals for staging (I, II, III, IV...)
                romanNumerals <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
                labels <- paste0("RPA Stage ", romanNumerals[1:nGroups])
            } else if (self$options$riskgrouplabels == 'risk') {
                if (nGroups == 2) {
                    labels <- c("Low Risk", "High Risk")
                } else if (nGroups == 3) {
                    labels <- c("Low Risk", "Intermediate Risk", "High Risk")
                } else {
                    labels <- paste0("Risk Group ", 1:nGroups)
                }
            } else {
                labels <- paste0("Group ", 1:nGroups)
            }

            levels(riskGroup) <- labels

            # Reorder risk groups by median OS (best prognosis = Stage/Group 1)
            medianOS <- numeric(nGroups)
            for (i in 1:nGroups) {
                groupIdx <- riskGroup == labels[i]
                kmFit_temp <- survival::survfit(survival::Surv(timeVar[groupIdx], eventNumeric[groupIdx]) ~ 1)
                medianOS[i] <- summary(kmFit_temp)$table["median"]
            }

            # Order by decreasing median OS (best = highest median)
            orderIdx <- order(medianOS, decreasing = TRUE)
            orderedLabels <- labels[orderIdx]

            # Recreate factor with ordered levels
            riskGroup <- factor(as.character(riskGroup), levels = labels[orderIdx])
            levels(riskGroup) <- orderedLabels

            # Update labels for consistent use
            labels <- orderedLabels

            # Populate risk group table
            if (self$options$riskgrouptable) {
                riskSummary <- data.frame(
                    stage = labels,
                    n = as.numeric(table(riskGroup)),
                    events = tapply(eventNumeric, riskGroup, sum),
                    censored = tapply(1 - eventNumeric, riskGroup, sum),
                    stringsAsFactors = FALSE
                )

                # Calculate median OS and 5-year survival per group
                for (i in 1:nrow(riskSummary)) {
                    groupIdx <- riskGroup == labels[i]
                    kmFit <- survival::survfit(survival::Surv(timeVar[groupIdx], eventNumeric[groupIdx]) ~ 1)

                    # Median OS
                    riskSummary$medianos[i] <- summary(kmFit)$table["median"]

                    # 5-year survival (using specified time unit)
                    time_5yr <- private$.getTime5Year()
                    if (max(timeVar[groupIdx]) >= time_5yr) {
                        survEst <- summary(kmFit, times = time_5yr)
                        riskSummary$os_5yr[i] <- 100 * survEst$surv
                        riskSummary$os_95ci[i] <- paste0("(", round(100*survEst$lower, 1), "-", round(100*survEst$upper, 1), ")")
                    } else {
                        riskSummary$os_5yr[i] <- NA
                        riskSummary$os_95ci[i] <- "Insufficient follow-up"
                    }
                }

                # Populate risk group table row by row
                for (i in seq_len(nrow(riskSummary))) {
                    self$results$riskgrouptable$addRow(
                        rowKey = i,
                        values = as.list(riskSummary[i, ])
                    )
                }
            }

            # Cox regression by risk group
            coxFit <- survival::coxph(survival::Surv(timeVar, eventNumeric) ~ riskGroup)
            coxSummary <- summary(coxFit)

            # Populate Cox table
            coxTable <- data.frame(
                comparison = paste0(labels[-1], " vs. ", labels[1]),
                hr = exp(coef(coxFit)),
                ci95 = paste0("(", round(coxSummary$conf.int[, 3], 2), "-", round(coxSummary$conf.int[, 4], 2), ")"),
                pvalue = coxSummary$coefficients[, 5],
                stringsAsFactors = FALSE
            )

            # Populate Cox model table row by row
            for (i in seq_len(nrow(coxTable))) {
                self$results$coxmodel$addRow(
                    rowKey = i,
                    values = as.list(coxTable[i, ])
                )
            }

            # Log-rank test
            logrank <- survival::survdiff(survival::Surv(timeVar, eventNumeric) ~ riskGroup)
            logrankTable <- data.frame(
                chisq = logrank$chisq,
                df = length(logrank$n) - 1,
                pvalue = 1 - pchisq(logrank$chisq, length(logrank$n) - 1)
            )
            # Populate log-rank test table row by row
            for (i in seq_len(nrow(logrankTable))) {
                self$results$logranktest$addRow(
                    rowKey = i,
                    values = as.list(logrankTable[i, ])
                )
            }

            # Variable importance
            if (self$options$variableimportance) {
                if (!is.null(tree$variable.importance)) {
                    varImp <- tree$variable.importance

                    # Map escaped names back to original names for display
                varNames <- names(varImp)
                originalNames <- character(length(varNames))
                for (i in seq_along(varNames)) {
                    # Find matching escaped name
                    matchIdx <- which(escapedNames == varNames[i])
                    if (length(matchIdx) > 0) {
                        originalNames[i] <- self$options$predictors[matchIdx[1]]
                    } else {
                        originalNames[i] <- varNames[i]
                    }
                }

                varImpTable <- data.frame(
                    variable = originalNames,
                    importance = as.numeric(varImp),
                    relimportance = 100 * as.numeric(varImp) / sum(varImp),
                    stringsAsFactors = FALSE
                )
                    varImpTable <- varImpTable[order(-varImpTable$importance), ]

                    # Populate variable importance table row by row
                    for (i in seq_len(nrow(varImpTable))) {
                        self$results$varimp$addRow(
                            rowKey = i,
                            values = as.list(varImpTable[i, ])
                        )
                    }
                } else {
                    private$.addNotice("INFO", "Variable Importance Unavailable",
                        "Tree structure doesn't support variable importance calculation (e.g., too few splits or no improvement from predictors).")
                }
            }

            # CP table
            if (self$options$cptable) {
                if (self$options$nfolds > 0) {
                    cpTableRaw <- as.data.frame(tree$cptable)
                # Match .r.yaml column names exactly
                cpTable <- data.frame(
                    cp = cpTableRaw$CP,
                    nsplit = cpTableRaw$nsplit,
                    relerror = cpTableRaw$`rel error`,
                    xerror = cpTableRaw$xerror,
                    xstd = cpTableRaw$xstd,
                    stringsAsFactors = FALSE
                    )

                    # Populate complexity parameter table row by row
                    for (i in seq_len(nrow(cpTable))) {
                        self$results$cptable$addRow(
                            rowKey = i,
                            values = as.list(cpTable[i, ])
                        )
                    }
                } else {
                    private$.addNotice("INFO", "CP Table Unavailable",
                        "Complexity parameter table requires cross-validation. Set Cross-Validation Folds > 0 to enable.")
                }
            }

            # Store tree for plotting
            self$results$treeplot$setState(list(tree = tree))

            # Create data frame for survminer (required by ggsurvplot)
            # IMPORTANT: Create this BEFORE survfit so variables are in data frame
            kmData <- data.frame(
                time = timeVar,
                event = eventNumeric,
                riskGroup = riskGroup
            )

            # Store KM fit for plotting
            # Use kmData with column names so ggsurvplot can find variables
            kmFit <- survival::survfit(survival::Surv(time, event) ~ riskGroup, data = kmData)

            self$results$kmplot$setState(list(
                fit = kmFit,
                data = kmData,
                labels = labels
            ))

            # Create new variable if requested
            if (self$options$createnewvar) {
                newVarName <- self$options$newvarname
                if (newVarName == "" || is.null(newVarName)) {
                    newVarName <- "rpa_stage"
                }

                # Create full-length variable (with NAs for excluded rows)
                fullRiskGroup <- rep(NA, nrow(self$data))
                fullRiskGroup[completeIdx] <- as.character(riskGroup)

                # Add to dataset
                self$results$.addColumn(
                    name = newVarName,
                    title = paste0("RPA Stage (", nGroups, " groups)"),
                    type = 'text',
                    values = fullRiskGroup
                )

                private$.addNotice("INFO", "Variable Created",
                    paste0("New variable '", newVarName, "' created with ", nGroups, " risk groups."))
            }

            # Add success summary
            private$.addNotice("INFO", "Analysis Complete",
                paste0("RPA identified ", nGroups, " risk groups from ", nPredictors, " predictor(s). ",
                       "Log-rank p = ", format.pval(logrankTable$pvalue, digits = 3), ". ",
                       "Review decision tree and survival curves for clinical interpretation."))

            # Plain-language summary (if enabled)
            if (self$options$showSummary) {
                summary_html <- '<div style="font-family: Arial; padding: 15px; background-color: #e7f3ff; border-left: 4px solid #0066cc; border-radius: 3px; margin: 10px 0;">'
                summary_html <- paste0(summary_html, '<h4 style="color: #0066cc; margin-top: 0;">📊 Analysis Summary</h4>')
                summary_html <- paste0(summary_html, sprintf('<p><b>RPA identified %d prognostic risk groups</b> from %d predictor variable(s).</p>', nGroups, nPredictors))

                # Add details for each group
                summary_html <- paste0(summary_html, '<table style="width: 100%; border-collapse: collapse; margin-top: 10px;">')
                summary_html <- paste0(summary_html, '<tr style="background-color: #f0f8ff; font-weight: bold;"><td>Risk Group</td><td>Patients</td><td>Events</td><td>Median OS</td><td>5-yr OS</td></tr>')

                for (i in 1:nGroups) {
                    summary_html <- paste0(summary_html,
                        sprintf('<tr style="border-bottom: 1px solid #ddd;"><td><b>%s</b></td><td>%d</td><td>%d</td><td>%.1f %s</td><td>%s</td></tr>',
                                labels[i],
                                riskSummary$n[i],
                                riskSummary$events[i],
                                riskSummary$medianos[i],
                                self$options$time_unit,
                                ifelse(is.na(riskSummary$os_5yr[i]), "N/A",
                                      paste0(round(riskSummary$os_5yr[i], 1), "% ", riskSummary$os_95ci[i]))))
                }
                summary_html <- paste0(summary_html, '</table>')

                summary_html <- paste0(summary_html,
                    sprintf('<p style="margin-top: 15px;"><b>Statistical test:</b> Log-rank χ² = %.2f (df = %d), p %s, indicating <b>%s differences</b> between risk groups.</p>',
                            logrankTable$chisq,
                            logrankTable$df,
                            ifelse(logrankTable$pvalue < 0.001, "< 0.001", paste0("= ", round(logrankTable$pvalue, 4))),
                            ifelse(logrankTable$pvalue < 0.05, "significant", "no significant")))

                summary_html <- paste0(summary_html, '</div>')
                self$results$summary$setContent(summary_html)
            }

            # Interpretation guide (if enabled)
            if (self$options$showInterpretation) {
                interp_html <- '<div style="font-family: Arial; padding: 15px; background-color: #fff8e1; border-left: 4px solid #ffa726; border-radius: 3px; margin: 10px 0;">'
                interp_html <- paste0(interp_html, '<h4 style="color: #f57c00; margin-top: 0;">📖 How to Interpret These Results</h4>')

                interp_html <- paste0(interp_html, '<p><b>Decision Tree:</b></p><ul>')
                interp_html <- paste0(interp_html, '<li>Shows how RPA split patients into risk groups based on predictor variables</li>')
                interp_html <- paste0(interp_html, '<li>Each split uses the most informative variable at that point</li>')
                interp_html <- paste0(interp_html, '<li>Terminal nodes (leaves) become risk groups</li>')
                interp_html <- paste0(interp_html, '<li>Numbers show sample size and event rate in each node</li></ul>')

                interp_html <- paste0(interp_html, '<p><b>Risk Group Table:</b></p><ul>')
                interp_html <- paste0(interp_html, '<li><b>Risk groups are ordered by prognosis</b> (best → worst survival)</li>')
                interp_html <- paste0(interp_html, '<li><b>Median OS</b> = time when 50% of patients in that group are still alive</li>')
                interp_html <- paste0(interp_html, '<li><b>5-yr OS</b> = percentage of patients alive at 5 years (with 95% CI)</li>')
                interp_html <- paste0(interp_html, '<li>Groups with higher median OS and 5-yr OS have better prognosis</li></ul>')

                interp_html <- paste0(interp_html, '<p><b>Kaplan-Meier Curves:</b></p><ul>')
                interp_html <- paste0(interp_html, '<li>Show survival probability over time for each risk group</li>')
                interp_html <- paste0(interp_html, '<li>Curves that separate clearly indicate good risk stratification</li>')
                interp_html <- paste0(interp_html, '<li>Log-rank test p-value indicates if differences are statistically significant</li></ul>')

                interp_html <- paste0(interp_html, '<p><b>Cox Regression:</b></p><ul>')
                interp_html <- paste0(interp_html, '<li><b>Hazard Ratio (HR)</b> compares risk of event between groups</li>')
                interp_html <- paste0(interp_html, '<li>HR > 1 means higher risk; HR < 1 means lower risk</li>')
                interp_html <- paste0(interp_html, '<li>HR = 2.0 means twice the risk; HR = 0.5 means half the risk</li>')
                interp_html <- paste0(interp_html, '<li>95% CI that excludes 1.0 indicates statistical significance</li></ul>')

                interp_html <- paste0(interp_html, '<p><b>Clinical Use:</b></p><ul>')
                interp_html <- paste0(interp_html, '<li>RPA-derived groups can guide treatment decisions and patient counseling</li>')
                interp_html <- paste0(interp_html, '<li>Consider external validation before clinical implementation</li>')
                interp_html <- paste0(interp_html, '<li>Use "Create New Variable" to save risk groups for future analyses</li></ul>')

                interp_html <- paste0(interp_html, '</div>')
                self$results$interpretation$setContent(interp_html)
            }

            # Report sentence (if enabled)
            if (self$options$showReport) {
                report_html <- '<div style="font-family: Arial; padding: 15px; background-color: #e8f5e9; border-left: 4px solid #66bb6a; border-radius: 3px; margin: 10px 0;">'
                report_html <- paste0(report_html, '<h4 style="color: #2e7d32; margin-top: 0;">📋 Report Sentence (Copy-Ready)</h4>')
                report_html <- paste0(report_html, '<p style="background: white; padding: 10px; border: 1px dashed #66bb6a; border-radius: 3px;">')

                report_text <- sprintf(
                    "Recursive partitioning analysis of %d patients with %d predictor variable%s identified %d prognostic risk groups (log-rank χ² = %.2f, df = %d, p %s). ",
                    nTotal,
                    nPredictors,
                    ifelse(nPredictors > 1, "s", ""),
                    nGroups,
                    logrankTable$chisq,
                    logrankTable$df,
                    ifelse(logrankTable$pvalue < 0.001, "< 0.001", paste0("= ", round(logrankTable$pvalue, 4)))
                )

                for (i in 1:nGroups) {
                    if (i == 1) {
                        report_text <- paste0(report_text,
                            sprintf("%s (n = %d) had a median overall survival of %.1f %s",
                                    labels[i],
                                    riskSummary$n[i],
                                    riskSummary$medianos[i],
                                    self$options$time_unit))
                    } else if (i == nGroups) {
                        report_text <- paste0(report_text,
                            sprintf(", and %s (n = %d) had %.1f %s",
                                    labels[i],
                                    riskSummary$n[i],
                                    riskSummary$medianos[i],
                                    self$options$time_unit))
                    } else {
                        report_text <- paste0(report_text,
                            sprintf(", %s (n = %d) had %.1f %s",
                                    labels[i],
                                    riskSummary$n[i],
                                    riskSummary$medianos[i],
                                    self$options$time_unit))
                    }

                    # Add 5-year survival if available
                    if (!is.na(riskSummary$os_5yr[i])) {
                        report_text <- paste0(report_text,
                            sprintf(" (5-year OS: %.1f%%, 95%% CI %s)",
                                    riskSummary$os_5yr[i],
                                    riskSummary$os_95ci[i]))
                    }
                }

                report_text <- paste0(report_text, ".")

                report_html <- paste0(report_html, report_text)
                report_html <- paste0(report_html, '</p>')
                report_html <- paste0(report_html, '<p style="font-size: 0.9em; color: #666; margin-top: 10px;"><i>💡 Tip: Click in the box above and press Ctrl+A (or Cmd+A) to select all, then Ctrl+C (or Cmd+C) to copy.</i></p>')
                report_html <- paste0(report_html, '</div>')

                self$results$report$setContent(report_html)
            }

            # Show all results (analysis succeeded)
            private$.showResults()

            # Render notices
            private$.renderNotices()
        },

        # Plot tree
        .plotTree = function(image, ...) {
            if (is.null(image$state) || is.null(image$state$tree)) {
                return(FALSE)
            }

            tree <- image$state$tree

            # Plot using rpart.plot
            rpart.plot::rpart.plot(
                tree,
                type = 4,
                extra = 101,  # Show n and % events
                box.palette = "RdYlGn",
                shadow.col = "gray",
                fallen.leaves = TRUE,
                branch.lty = 3,
                split.cex = 0.9,
                nn.cex = 0.8,
                main = "Recursive Partitioning Tree for Survival",
                cex.main = 1.2
            )

            return(TRUE)
        },

        # Plot Kaplan-Meier curves
        .plotKM = function(image, ...) {
            if (is.null(image$state)) {
                return(FALSE)
            }

            state <- image$state

            # Create KM plot using survminer
            p <- survminer::ggsurvplot(
                state$fit,
                data = state$data,  # Pass data frame (required by survminer)
                risk.table = self$options$risktable,
                pval = self$options$pval,
                conf.int = self$options$kmci,
                palette = "jco",
                ggtheme = ggplot2::theme_minimal(),
                title = "Kaplan-Meier Curves by RPA Risk Group",
                xlab = "Time (months)",
                ylab = "Overall Survival Probability",
                legend.title = "Risk Group",
                legend.labs = state$labels,
                risk.table.height = 0.25,
                risk.table.y.text = FALSE,
                fontsize = 3.5,
                tables.theme = survminer::theme_cleantable()
            )

            print(p)

            return(TRUE)
        }
    )
)
