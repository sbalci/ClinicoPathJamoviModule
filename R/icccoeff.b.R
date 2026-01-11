#' @title Intraclass Correlation Coefficient
#' @importFrom R6 R6Class
#' @import jmvcore
#' @export

icccoeffClass <- R6::R6Class(
    "icccoeffClass",
    inherit = icccoeffBase,
    private = list(
        .icc_results = NULL,
        .descriptive_data = NULL,
        
        .init = function() {
            # Check for required variables and show instructions
            if (is.null(self$options$vars) || length(self$options$vars) < 2) {
                private$.showInstructions("setup")
            } else {
                private$.showInstructions("ready")
            }
        },
        
        .run = function() {
            # Check if we have required variables
            if (!private$.hasRequiredVariables()) {
                return()
            }
            
            # Prepare data for ICC analysis
            data <- private$.prepareICCData()
            if (is.null(data)) return()
            
            # Calculate ICC
            private$.calculateICC(data)
            
            # Populate results
            private$.populateICCResults()
            private$.populateAPAFormat()
            private$.populateDescriptiveStats(data)
            private$.populateReliabilityAssessment()
            private$.populateInterpretation()
        },
        
        .hasRequiredVariables = function() {
            !is.null(self$options$vars) && length(self$options$vars) >= 2
        },
        
        .showInstructions = function(type) {
            instructions <- switch(type,
                "setup" = private$.createInstructionsHTML("setup"),
                "ready" = private$.createInstructionsHTML("ready")
            )
            
            self$results$instructions$setContent(instructions)
        },
        
        .createInstructionsHTML = function(type) {
            base_style <- "
            <html>
            <head>
            <style>
                .main {
                    margin: 20px;
                    font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Arial,sans-serif;
                }
                .header {
                    color: #1976d2;
                    font-size: 18px;
                    font-weight: bold;
                    margin-bottom: 10px;
                }
                .description {
                    margin-bottom: 20px;
                    line-height: 1.5;
                }
                .requirements {
                    background-color: #f5f5f5;
                    padding: 15px;
                    border-left: 4px solid #2196f3;
                    margin-bottom: 20px;
                }
                .note {
                    background-color: #fff3e0;
                    padding: 15px;
                    border-left: 4px solid #ff9800;
                    margin-bottom: 20px;
                }
            </style>
            </head>
            <body>
            <div class='main'>
                <div class='header'>📊 Intraclass Correlation Coefficient (ICC)</div>"
            
            content <- switch(type,
                "setup" = "
                <div class='description'>
                    Calculate ICC for inter-rater reliability and agreement studies. 
                    ICC measures the consistency or agreement between raters or measurements.
                </div>
                <div class='requirements'>
                    <strong>Required:</strong><br>
                    • At least 2 rater variables (columns represent different raters)<br>
                    • Subjects/cases as rows<br>
                    • Numeric rating scales
                </div>
                <div class='note'>
                    <strong>Data Structure:</strong><br>
                    Each row = one subject/case<br>
                    Each column = one rater's scores<br>
                    Missing values are handled according to your selection
                </div>",
                
                "ready" = "
                <div class='description'>
                    ICC analysis ready! Your data includes ratings from multiple raters. 
                    Choose the appropriate ICC type based on your study design.
                </div>
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50;'>
                    <strong>✓ Analysis configured correctly</strong><br>
                    Select ICC type and review results below.
                </div>"
            )
            
            footer <- "
                <div style='margin-top: 20px; font-style: italic; color: #666;'>
                    Based on Shrout & Fleiss (1979) and McGraw & Wong (1996) frameworks.
                </div>
            </div>
            </body>
            </html>"
            
            paste0(base_style, content, footer)
        },
        
        .prepareICCData = function() {
            data <- self$data
            vars <- self$options$vars
            
            # Extract rater data
            icc_data <- data[vars]
            
            # Handle missing values
            if (self$options$missing_values == "complete") {
                icc_data <- icc_data[complete.cases(icc_data), ]
            }
            
            # Check if we have sufficient data
            if (nrow(icc_data) < 2) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ Insufficient data</strong><br>
                    Need at least 2 complete cases for ICC calculation.
                    </div>"
                )
                return(NULL)
            }
            
            # Check if all columns are numeric
            if (!all(sapply(icc_data, is.numeric))) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ Non-numeric data</strong><br>
                    All rater variables must be numeric.
                    </div>"
                )
                return(NULL)
            }
            
            private$.descriptive_data <- icc_data
            return(icc_data)
        },
        
        .calculateICC = function(data) {
            # Check if irr package is available
            if (!requireNamespace("irr", quietly = TRUE)) {
                self$results$instructions$setContent(
                    "<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ Package Required</strong><br>
                    The 'irr' package is required for ICC calculation.
                    </div>"
                )
                return()
            }
            
            tryCatch({
                # Determine ICC model and unit based on options
                icc_type <- self$options$icc_type
                agreement_type <- self$options$agreement_type
                confidence_level <- as.numeric(self$options$confidence_level)
                
                # Map ICC type to irr package parameters
                icc_params <- private$.getICCParameters(icc_type, agreement_type)
                
                # Calculate ICC using irr package
                icc_result <- irr::icc(
                    data, 
                    model = icc_params$model,
                    type = icc_params$type,
                    unit = icc_params$unit,
                    r0 = 0,
                    conf.level = confidence_level
                )
                
                private$.icc_results <- list(
                    icc_type = private$.formatICCType(icc_type, agreement_type),
                    icc_value = icc_result$value,
                    ci_lower = icc_result$lbound,
                    ci_upper = icc_result$ubound,
                    f_value = icc_result$Fvalue,
                    df1 = icc_result$df1,
                    df2 = icc_result$df2,
                    p_value = icc_result$p.value,
                    n_subjects = icc_result$subjects,
                    n_raters = icc_result$raters,
                    conf_level = confidence_level
                )
                
            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<div style='padding: 20px; color: #d32f2f;'>
                    <strong>❌ ICC Calculation Error</strong><br>
                    ", e$message, "
                    </div>")
                )
                private$.icc_results <- NULL
            })
        },
        
        .getICCParameters = function(icc_type, agreement_type) {
            # Convert jamovi options to irr package parameters
            switch(icc_type,
                "icc1_1" = list(model = "oneway", type = agreement_type, unit = "single"),
                "icc2_1" = list(model = "twoway", type = agreement_type, unit = "single"),
                "icc3_1" = list(model = "twoway", type = agreement_type, unit = "single"),
                "icc1_k" = list(model = "oneway", type = agreement_type, unit = "average"),
                "icc2_k" = list(model = "twoway", type = agreement_type, unit = "average"),
                "icc3_k" = list(model = "twoway", type = agreement_type, unit = "average"),
                list(model = "twoway", type = "consistency", unit = "single")  # default
            )
        },
        
        .formatICCType = function(icc_type, agreement_type) {
            type_names <- list(
                "icc1_1" = "ICC(1,1)",
                "icc2_1" = "ICC(2,1)", 
                "icc3_1" = "ICC(3,1)",
                "icc1_k" = "ICC(1,k)",
                "icc2_k" = "ICC(2,k)",
                "icc3_k" = "ICC(3,k)"
            )
            
            base_type <- type_names[[icc_type]]
            agreement_suffix <- ifelse(agreement_type == "consistency", "(C)", "(A)")
            
            paste0(base_type, " ", agreement_suffix)
        },
        
        .populateICCResults = function() {
            if (is.null(private$.icc_results)) return()
            
            table <- self$results$icc_results
            results <- private$.icc_results
            
            table$addRow(rowKey = 1, values = list(
                icc_type = results$icc_type,
                icc_value = results$icc_value,
                ci_lower = if (self$options$show_confidence_intervals) results$ci_lower else NULL,
                ci_upper = if (self$options$show_confidence_intervals) results$ci_upper else NULL,
                f_value = if (self$options$show_f_test) results$f_value else NULL,
                df1 = if (self$options$show_f_test) results$df1 else NULL,
                df2 = if (self$options$show_f_test) results$df2 else NULL,
                p_value = if (self$options$show_f_test) results$p_value else NULL
            ))
        },
        
        .populateAPAFormat = function() {
            if (!self$options$show_apa_format || is.null(private$.icc_results)) return()
            
            results <- private$.icc_results
            dp <- self$options$decimal_places
            
            # Create APA-style reporting
            icc_formatted <- round(results$icc_value, dp)
            ci_formatted <- paste0(round(results$ci_lower, dp), ", ", round(results$ci_upper, dp))
            
            # Determine significance
            alpha <- self$options$alpha_level
            sig_text <- if (results$p_value < alpha) "significant" else "non-significant"
            
            apa_text <- paste0(
                "<div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>",
                "<h3 style='color: #1976d2; margin-bottom: 15px;'>📝 APA Style Results</h3>",
                "<div style='background-color: #f5f5f5; padding: 15px; border-radius: 8px; font-family: monospace;'>",
                "<strong>", results$icc_type, "</strong> = ", icc_formatted,
                ", ", sprintf("%.0f", results$conf_level * 100), "% CI [", ci_formatted, "]",
                ", <em>F</em>(", results$df1, ", ", results$df2, ") = ", 
                sprintf("%.3f", results$f_value), ", <em>p</em> ",
                if (results$p_value < 0.001) "< .001" else paste("=", sprintf("%.3f", results$p_value)),
                "</div>",
                "<div style='margin-top: 15px; padding: 10px; background-color: #e3f2fd; border-radius: 6px;'>",
                "<strong>Interpretation:</strong> The ICC indicates ", 
                private$.interpretICCValue(results$icc_value), " reliability and is ",
                sig_text, " at α = ", alpha, ".",
                "</div>",
                "</div>"
            )
            
            self$results$apa_format$setContent(apa_text)
        },
        
        .populateDescriptiveStats = function(data) {
            if (!self$options$show_descriptive_stats) return()
            
            table <- self$results$descriptive_stats
            
            for (i in 1:ncol(data)) {
                col_data <- data[, i]
                rater_name <- names(data)[i]
                
                table$addRow(rowKey = i, values = list(
                    rater = rater_name,
                    n = sum(!is.na(col_data)),
                    mean = mean(col_data, na.rm = TRUE),
                    sd = sd(col_data, na.rm = TRUE),
                    min = min(col_data, na.rm = TRUE),
                    max = max(col_data, na.rm = TRUE),
                    missing = sum(is.na(col_data))
                ))
            }
        },
        
        .populateReliabilityAssessment = function() {
            if (is.null(private$.icc_results)) return()
            
            table <- self$results$reliability_assessment
            icc_value <- private$.icc_results$icc_value
            
            # Standard ICC interpretation guidelines
            assessments <- list(
                list(criteria = "Poor reliability", threshold = "< 0.50", 
                     assessment = if (icc_value < 0.50) "✓ Poor" else ""),
                list(criteria = "Moderate reliability", threshold = "0.50 - 0.75", 
                     assessment = if (icc_value >= 0.50 && icc_value < 0.75) "✓ Moderate" else ""),
                list(criteria = "Good reliability", threshold = "0.75 - 0.90", 
                     assessment = if (icc_value >= 0.75 && icc_value < 0.90) "✓ Good" else ""),
                list(criteria = "Excellent reliability", threshold = "> 0.90", 
                     assessment = if (icc_value >= 0.90) "✓ Excellent" else "")
            )
            
            for (i in 1:length(assessments)) {
                table$addRow(rowKey = i, values = assessments[[i]])
            }
        },
        
        .populateInterpretation = function() {
            if (!self$options$show_interpretation) return()
            
            interpretation_html <- "
            <div style='font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Arial,sans-serif; padding: 20px;'>
                <h3 style='color: #1976d2; margin-bottom: 15px;'>📚 ICC Interpretation Guide</h3>
                
                <div style='background-color: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4caf50; margin-bottom: 15px;'>
                    <strong>ICC Value Interpretation (Koo & Li, 2016):</strong><br>
                    • <strong>< 0.50:</strong> Poor reliability<br>
                    • <strong>0.50-0.75:</strong> Moderate reliability<br>
                    • <strong>0.75-0.90:</strong> Good reliability<br>
                    • <strong>> 0.90:</strong> Excellent reliability
                </div>
                
                <div style='background-color: #f3e5f5; padding: 15px; border-radius: 8px; border-left: 4px solid #9c27b0; margin-bottom: 15px;'>
                    <strong>ICC Types:</strong><br>
                    • <strong>ICC(1,1):</strong> One-way random, single measures<br>
                    • <strong>ICC(2,1):</strong> Two-way random, single measures<br>
                    • <strong>ICC(3,1):</strong> Two-way mixed, single measures<br>
                    • <strong>ICC(1,k):</strong> One-way random, average measures<br>
                    • <strong>ICC(2,k):</strong> Two-way random, average measures<br>
                    • <strong>ICC(3,k):</strong> Two-way mixed, average measures
                </div>
                
                <div style='background-color: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #ff9800;'>
                    <strong>Agreement Types:</strong><br>
                    • <strong>Consistency (C):</strong> Measures if raters maintain consistent relative ranking<br>
                    • <strong>Absolute Agreement (A):</strong> Measures if raters give identical scores<br><br>
                    <strong>Model Selection:</strong><br>
                    • <strong>One-way:</strong> Raters are randomly selected from population<br>
                    • <strong>Two-way:</strong> Same raters rate all subjects<br>
                    • <strong>Mixed:</strong> Raters are specifically chosen, not random
                </div>
            </div>"
            
            self$results$interpretation$setContent(interpretation_html)
        },
        
        .interpretICCValue = function(icc_value) {
            if (icc_value < 0.50) {
                "poor"
            } else if (icc_value < 0.75) {
                "moderate"
            } else if (icc_value < 0.90) {
                "good"
            } else {
                "excellent"
            }
        }
    )
)