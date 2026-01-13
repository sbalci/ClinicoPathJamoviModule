
# This file is a generated template, your changes will not be overwritten

methodvalidationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "methodvalidationClass",
    inherit = methodvalidationBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$measurement)) {
                
                todo <- "
                    <br>Welcome to Laboratory Method Validation
                    <br><br>
                    This tool evaluates method performance (precision, accuracy) following CLSI and ISO guidelines.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Measurement Variable</b>
                    <br>2. (Optional) Provide <b>Reference/Target Value</b> for accuracy analysis
                    <br>3. (Optional) Provide <b>Concentration Level</b> to analyze multiple levels
                    <br><br>
                    Accepted limits can be configured in the options (e.g., CV% limit, Bias% limit).
                "
                self$results$instructions$setContent(todo)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # 2. Get and clean data
            mydata <- self$data
            measVar <- self$options$measurement
            refVar <- self$options$reference_value
            levelVar <- self$options$concentration_level
            
            vars <- measVar
            if (!is.null(refVar)) vars <- c(vars, refVar)
            if (!is.null(levelVar)) vars <- c(vars, levelVar)
            
            mydata <- jmvcore::naOmit(mydata[vars])
            
            if (nrow(mydata) == 0) {
                self$results$instructions$setContent("No valid data rows found after removing missing values.")
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # 3. Data Summary Table
            tableSummary <- self$results$summary
            tableSummary$setRow(rowNo=1, values=list(
                characteristic = "Valid Observations (N)",
                value = as.character(nrow(mydata))
            ))

            # 4. Precision Analysis
            if (self$options$precision_analysis) {
                tablePrec <- self$results$precisionResults
                tablePrec$deleteRows()
                
                if (!is.null(levelVar)) {
                    levels <- unique(mydata[[levelVar]])
                    for (i in seq_along(levels)) {
                        lv <- levels[i]
                        subdata <- mydata[mydata[[levelVar]] == lv, ]
                        m <- mean(subdata[[measVar]])
                        s <- sd(subdata[[measVar]])
                        cv <- (s / m) * 100
                        limit <- self$options$custom_cv_limit
                        status <- if (cv <= limit) "Pass" else "Fail"
                        
                        tablePrec$addRow(rowKey=i, values=list(
                            level = as.character(lv),
                            n = nrow(subdata),
                            mean = m,
                            sd = s,
                            cv = cv,
                            status = status
                        ))
                    }
                } else {
                    m <- mean(mydata[[measVar]])
                    s <- sd(mydata[[measVar]])
                    cv <- (s / m) * 100
                    limit <- self$options$custom_cv_limit
                    status <- if (cv <= limit) "Pass" else "Fail"
                    
                    tablePrec$addRow(rowKey=1, values=list(
                        level = "Overall",
                        n = nrow(mydata),
                        mean = m,
                        sd = s,
                        cv = cv,
                        status = status
                    ))
                }
            }

            # 5. Accuracy Analysis
            if (self$options$accuracy_analysis && !is.null(refVar)) {
                tableAcc <- self$results$accuracyResults
                tableAcc$deleteRows()
                
                if (!is.null(levelVar)) {
                    levels <- unique(mydata[[levelVar]])
                    for (i in seq_along(levels)) {
                        lv <- levels[i]
                        subdata <- mydata[mydata[[levelVar]] == lv, ]
                        m_obs <- mean(subdata[[measVar]])
                        m_ref <- mean(subdata[[refVar]])
                        bias_abs <- m_obs - m_ref
                        bias_rel <- (bias_abs / m_ref) * 100
                        limit <- self$options$custom_bias_limit
                        status <- if (abs(bias_rel) <= limit) "Pass" else "Fail"
                        
                        tableAcc$addRow(rowKey=i, values=list(
                            level = as.character(lv),
                            target = m_ref,
                            observed = m_obs,
                            bias_abs = bias_abs,
                            bias_rel = bias_rel,
                            status = status
                        ))
                    }
                } else {
                    m_obs <- mean(mydata[[measVar]])
                    m_ref <- mean(mydata[[refVar]])
                    bias_abs <- m_obs - m_ref
                    bias_rel <- (bias_abs / m_ref) * 100
                    limit <- self$options$custom_bias_limit
                    status <- if (abs(bias_rel) <= limit) "Pass" else "Fail"
                    
                    tableAcc$addRow(rowKey=1, values=list(
                        level = "Overall",
                        target = m_ref,
                        observed = m_obs,
                        bias_abs = bias_abs,
                        bias_rel = bias_rel,
                        status = status
                    ))
                }
            }

            # 6. Method Explanation
            explanation <- "
                <h3>Analytical Method Validation</h3>
                <p>Method validation is the process used to confirm that the analytical procedure employed for a specific test is suitable for its intended use.</p>
                <ul>
                    <li><b>Precision:</b> Expresses the closeness of agreement between a series of measurements. Usually report as Coefficient of Variation (CV%).</li>
                    <li><b>Accuracy (Bias):</b> Expresses the closeness of agreement between the value which is accepted either as a conventional true value or an accepted reference value and the value found.</li>
                </ul>
                <p>Following ISO 15189, laboratories must validate non-standard methods, laboratory-designed/developed methods, and standard methods used outside their intended scope.</p>
            "
            self$results$methodExplanation$setContent(explanation)

        })
)
