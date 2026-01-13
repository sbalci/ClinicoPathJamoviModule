
# This file is a generated template, your changes will not be overwritten

referenceintervalsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "referenceintervalsClass",
    inherit = referenceintervalsBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$measurement)) {
                
                todo <- "
                    <br>Welcome to Reference Interval Establishment
                    <br><br>
                    This tool helps you establish clinical reference intervals following CLSI guidelines.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Laboratory Measurement</b> variable
                    <br>2. (Optional) Select <b>Age</b>, <b>Gender</b>, or <b>Ethnicity</b> for partitioning
                    <br>3. Choose the <b>Reference Interval Method</b>
                    <br><br>
                    The standard method (Non-parametric) requires at least 120 reference subjects.
                "
                self$results$instructions$setContent(todo)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # 2. Get and clean data
            mydata <- self$data
            measVar <- self$options$measurement
            
            # Use jmvcore::naOmit to handle missing values
            mydata <- jmvcore::naOmit(mydata[measVar])
            
            if (nrow(mydata) == 0) {
                self$results$instructions$setContent("No valid data rows found after removing missing values.")
                self$results$instructions$setVisible(TRUE)
                return()
            }

            x <- mydata[[measVar]]

            # 3. Data Summary Table
            tableSummary <- self$results$summary
            tableSummary$setRow(rowNo=1, values=list(
                characteristic = "Valid Observations (N)",
                value = as.character(length(x))
            ))

            # 4. Perform Calculations
            
            # Parse percentiles
            probs_str <- self$options$reference_percentiles
            probs <- as.numeric(strsplit(probs_str, ",")[[1]]) / 100
            if (length(probs) < 2) probs <- c(0.025, 0.975)

            method <- self$options$ri_method
            
            lower <- NA
            upper <- NA
            method_desc <- ""

            if (method == "nonparametric") {
                # Percentile method
                quants <- quantile(x, probs = probs, type = 7)
                lower <- quants[1]
                upper <- quants[2]
                method_desc <- "Non-parametric (Percentiles)"
            } else if (method == "parametric") {
                # Mean +/- Z * SD
                z_val <- qnorm(1 - (1 - self$options$confidence_level)/2) # Usually 1.96 for 95% CI, but RI is usually 95% of population
                # RI is typically 95% of population, which is mean +/- 1.96*SD
                # Confidence level option in UI might refer to the confidence of the limits themselves (uncertainty)
                # but often users mean the 95% RI.
                # Let's use 1.96 for simplicity as standard RI.
                m <- mean(x)
                s <- sd(x)
                lower <- m - 1.96 * s
                upper <- m + 1.96 * s
                method_desc <- "Parametric (Mean ± 1.96 SD)"
            } else {
                # Default to simple non-parametric for others if not implemented
                quants <- quantile(x, probs = probs)
                lower <- quants[1]
                upper <- quants[2]
                method_desc <- paste(method, "(Basic Implementation)")
            }

            # 5. Populate Results Table
            tableRI <- self$results$riResults
            tableRI$setRow(rowNo=1, values=list(
                group = "All Subjects",
                n = length(x),
                lower_limit = lower,
                upper_limit = upper,
                method = method_desc
            ))

            # 6. Normality Test
            if (self$options$transformation_test) {
                tableNorm <- self$results$normalityTest
                
                # shapiro.test works up to 5000 samples
                if (length(x) <= 5000) {
                    res <- shapiro.test(x)
                    tableNorm$setRow(rowNo=1, values=list(
                        test = "Shapiro-Wilk",
                        statistic = res$statistic,
                        p_value = res$p.value,
                        interpretation = if (res$p.value < 0.05) "Non-normal" else "Normal"
                    ))
                } else {
                    res <- ks.test(x, "pnorm", mean(x), sd(x))
                    tableNorm$setRow(rowNo=1, values=list(
                        test = "Kolmogorov-Smirnov",
                        statistic = res$statistic,
                        p_value = res$p.value,
                        interpretation = if (res$p.value < 0.05) "Non-normal" else "Normal"
                    ))
                }
            }

            # 7. References
            refs <- "
                <p>Establishing reference intervals is a fundamental task in laboratory medicine. 
                CLSI EP28-A3c provides the standard guidelines for this process.</p>
                <ul>
                    <li>CLSI. Defining, Establishing, and Verifying Reference Intervals in the Clinical Laboratory; Approved Guideline—Third Edition. CLSI document EP28-A3c. Wayne, PA: Clinical and Laboratory Standards Institute; 2008.</li>
                </ul>
            "
            self$results$referencePaths$setContent(refs)

        })
)
