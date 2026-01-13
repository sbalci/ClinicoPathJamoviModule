
# This file is a generated template, your changes will not be overwritten

sigmametricsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "sigmametricsClass",
    inherit = sigmametricsBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$bias) || is.null(self$options$cv) || is.null(self$options$tea)) {
                
                todo <- "
                    <br>Welcome to Sigma Metrics Analysis
                    <br><br>
                    This tool calculates Six Sigma metrics for laboratory performance evaluation.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Bias (%)</b> variable
                    <br>2. Select the <b>Coefficient of Variation (%)</b> variable
                    <br>3. Select the <b>Total Allowable Error (%)</b> variable
                    <br>4. (Optional) Select identifying variables like Analyte, Method, or Laboratory
                    <br><br>
                    Sigma Metric is calculated as: <b>(TEa% - Bias%) / CV%</b>
                "
                self$results$instructions$setContent(todo)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # 2. Get and clean data
            mydata <- self$data
            
            biasVar <- self$options$bias
            cvVar <- self$options$cv
            teaVar <- self$options$tea
            analyteVar <- self$options$analyte
            
            # Use jmvcore::naOmit to handle missing values
            vars <- c(biasVar, cvVar, teaVar)
            if (!is.null(analyteVar)) vars <- c(vars, analyteVar)
            
            mydata <- jmvcore::naOmit(mydata[vars])
            
            if (nrow(mydata) == 0) {
                self$results$instructions$setContent("No valid data rows found after removing missing values.")
                self$results$instructions$setVisible(TRUE)
                return()
            }

            # 3. Perform Calculations
            # Sigma = (TEa - Bias) / CV
            mydata$sigma <- (mydata[[teaVar]] - abs(mydata[[biasVar]])) / mydata[[cvVar]]
            
            # Quality Rating and Levels
            # >6: World Class
            # 5-6: Excellent
            # 4-5: Good
            # 3-4: Marginal
            # 2-3: Poor
            # <2: Unacceptable
            
            mydata$rating <- cut(mydata$sigma, 
                                breaks = c(-Inf, 2, 3, 4, 5, 6, Inf),
                                labels = c("Unacceptable", "Poor", "Marginal", "Good", "Excellent", "World Class"))
            
            # Defect Rate (ppm) - approximated
            # For a given sigma level, the defect rate can be derived from the normal distribution
            # ppm = (1 - pnorm(sigma - 1.5)) * 10^6  (assuming 1.5 sigma shift)
            mydata$defect_rate <- (1 - pnorm(mydata$sigma - 1.5)) * 1e6

            # 4. Populate Tables
            
            # Data Summary Table
            tableInfo <- self$results$dataInfo
            tableInfo$setRow(rowNo=1, values=list(
                characteristic = "Observations",
                value = as.character(nrow(mydata))
            ))
            
            # Sigma Results Table
            tableResults <- self$results$sigmaResults
            tableResults$deleteRows()
            
            for (i in seq_len(nrow(mydata))) {
                analyte_val <- if (!is.null(analyteVar)) as.character(mydata[i, analyteVar]) else paste("Row", i)
                
                tableResults$addRow(rowKey=i, values=list(
                    analyte = analyte_val,
                    sigma_value = mydata$sigma[i],
                    sigma_level = sprintf("%.1f Sigma", mydata$sigma[i]),
                    quality_rating = as.character(mydata$rating[i]),
                    defect_rate_ppm = mydata$defect_rate[i],
                    yield_percentage = pnorm(mydata$sigma[i] - 1.5) * 100
                ))
            }
            
            # Quality Goal Index (QGI) Analysis
            # QGI = Bias / (1.5 * CV)
            # QGI < 0.8: Imprecision is the main problem
            # QGI > 1.2: Inaccuracy (Bias) is the main problem
            # 0.8 - 1.2: Both imprecision and inaccuracy
            if (self$options$quality_goal_index) {
                tableQGI <- self$results$qualityGoalIndex
                tableQGI$deleteRows()
                
                for (i in seq_len(nrow(mydata))) {
                    qgi_val <- abs(mydata[i, biasVar]) / (1.5 * mydata[i, cvVar])
                    interpretation <- if (qgi_val < 0.8) "Mainly Imprecision" else if (qgi_val > 1.2) "Mainly Inaccuracy" else "Imprecision and Inaccuracy"
                    if (mydata$sigma[i] >= 6) interpretation <- "Performance within goals"
                    
                    analyte_val <- if (!is.null(analyteVar)) as.character(mydata[i, analyteVar]) else paste("Row", i)
                    
                    tableQGI$addRow(rowKey=i, values=list(
                        analyte = analyte_val,
                        bias_qgi = abs(mydata[i, biasVar]),
                        precision_qgi = mydata[i, cvVar],
                        total_qgi = qgi_val,
                        qgi_interpretation = interpretation
                    ))
                }
            }

            # Defect Rate Analysis
            if (self$options$defect_rates) {
                tableDefects <- self$results$defectRateAnalysis
                tableDefects$deleteRows()
                
                for (i in seq_len(nrow(mydata))) {
                    analyte_val <- if (!is.null(analyteVar)) as.character(mydata[i, analyteVar]) else paste("Row", i)
                    
                    tableDefects$addRow(rowKey=i, values=list(
                        error_type = analyte_val,
                        current_rate_ppm = mydata$defect_rate[i],
                        target_rate_ppm = 3.4, # 6 sigma goal
                        monthly_volume = 1000, # Placeholder volume
                        annual_errors = as.integer(mydata$defect_rate[i] * 12 * 1000 / 1e6),
                        clinical_impact = if (mydata$sigma[i] < 3) "Critical Risk" else "Managed Risk"
                    ))
                }
            }

            # Method Explanation
            explanation <- "
                <h3>Sigma Metric in Laboratory Medicine</h3>
                <p>The Sigma Metric provides a quantitative measure of process quality. A higher sigma level indicates lower probability of defects (errors).</p>
                <ul>
                    <li><b>Sigma Metric = (TEa - Bias) / CV</b></li>
                    <li><b>6 Sigma:</b> World-class performance (3.4 defects per million)</li>
                    <li><b>3 Sigma:</b> Minimum acceptable performance for laboratory analysis</li>
                </ul>
            "
            self$results$methodExplanation$setContent(explanation)

        })
)
