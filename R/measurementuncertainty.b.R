
# This file is a generated template, your changes will not be overwritten

measurementuncertaintyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "measurementuncertaintyClass",
    inherit = measurementuncertaintyBase,
    private = list(
        .run = function() {

            # 1. Provide instructions if inputs are missing
            if (is.null(self$options$measurement)) {
                
                todo <- "
                    <br>Welcome to Measurement Uncertainty Estimation
                    <br><br>
                    This tool evaluates measurement uncertainty following ISO GUM guidelines.
                    <br><br>
                    To get started:
                    <br>1. Select the <b>Measurement Variable</b>
                    <br>2. (Optional) Provide <b>Type B</b> uncertainty estimates (Calibration, Reference Materials, etc.)
                    <br>3. Set the <b>Coverage Factor (k)</b> - usually 2.0 for 95% confidence
                "
                self$results$instructions$setContent(todo)
                return()
            } else {
                self$results$instructions$setVisible(FALSE)
            }

            # 2. Get and clean data
            mydata <- self$data
            measVar <- self$options$measurement
            
            mydata <- jmvcore::naOmit(mydata[measVar])
            
            if (nrow(mydata) == 0) {
                self$results$instructions$setContent("No valid data rows found after removing missing values.")
                self$results$instructions$setVisible(TRUE)
                return()
            }

            x <- mydata[[measVar]]
            mean_x <- mean(x)
            
            # 3. Type A Uncertainty (Repeatability)
            u_a <- sd(x) / sqrt(length(x)) # Standard uncertainty of the mean

            # 4. Type B Uncertainties (from Options)
            # These are usually provided as relative percentages (%) in the UI
            u_cal <- (self$options$calibration_uncertainty / 100) * mean_x
            u_ref <- (self$options$reference_material_uncertainty / 100) * mean_x
            u_temp <- (self$options$temperature_uncertainty / 100) * mean_x
            
            # 5. Combined Uncertainty
            u_c <- sqrt(u_a^2 + u_cal^2 + u_ref^2 + u_temp^2)
            
            # 6. Expanded Uncertainty
            k <- self$options$coverage_factor
            U <- k * u_c
            
            rel_U <- (U / mean_x) * 100

            # 7. Populate Data Summary
            tableSummary <- self$results$summary
            tableSummary$setRow(rowNo=1, values=list(
                characteristic = "Mean Measurement Value",
                value = sprintf("%.4f", mean_x)
            ))

            # 8. Populate Uncertainty Results
            tableResults <- self$results$uncertaintyResults
            tableResults$deleteRows()
            
            tableResults$addRow(rowKey=1, values=list(
                parameter = "Standard Uncertainty (Combined)",
                value = u_c,
                unit = "Units",
                interpretation = "1 Standard Deviation"
            ))
            
            tableResults$addRow(rowKey=2, values=list(
                parameter = "Expanded Uncertainty (U)",
                value = U,
                unit = "Units",
                interpretation = sprintf("Confidence Interval (k=%g)", k)
            ))
            
            tableResults$addRow(rowKey=3, values=list(
                parameter = "Relative Expanded Uncertainty",
                value = rel_U,
                unit = "%",
                interpretation = "Percentage of Mean"
            ))

            # 9. Populate Budget Table
            tableBudget <- self$results$budgetTable
            tableBudget$deleteRows()
            
            sources <- c("Repeatability (Type A)", "Calibration (Type B)", "Reference Material (Type B)", "Temperature (Type B)")
            types <- c("A", "B", "B", "B")
            dists <- c("Normal", "Normal", "Normal", "Rectangular") # Simplified
            vals <- c(u_a, u_cal, u_ref, u_temp)
            contributions <- (vals^2 / u_c^2) * 100
            
            for (i in seq_along(sources)) {
                tableBudget$addRow(rowKey=i, values=list(
                    source = sources[i],
                    type = types[i],
                    distribution = dists[i],
                    value = vals[i],
                    contribution = contributions[i]
                ))
            }

            # 10. Method Explanation
            explanation <- "
                <h3>Measurement Uncertainty (GUM Approach)</h3>
                <p>Following ISO/IEC Guide 98-3 (GUM), measurement uncertainty is classified into two types:</p>
                <ul>
                    <li><b>Type A:</b> Evaluated by statistical analysis of series of observations.</li>
                    <li><b>Type B:</b> Evaluated by means other than statistical analysis (e.g., certificates, manufacturer specs).</li>
                </ul>
                <p>The <b>Expanded Uncertainty (U)</b> represents an interval about the measurement result that may be expected to encompass a large fraction of the distribution of values that could reasonably be attributed to the measurand.</p>
            "
            self$results$methodExplanation$setContent(explanation)

        })
)
