#' @title Clinical Hemodynamic Monitoring
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import magrittr
#' @importFrom clintools clinmon

clinmonClass <- if (requireNamespace("jmvcore")) {
    R6::R6Class(
        "clinmonClass",
        inherit = clinmonBase,
        private = list(

            # init ----
            .init = function() {
                if (is.null(self$options$time_var)) {
                    self$results$detailed_results$setVisible(FALSE)
                    self$results$summary_stats$setVisible(FALSE)
                }
                
                if (!self$options$show_summary) {
                    self$results$summary_stats$setVisible(FALSE)
                }
                
                if (!self$options$show_detailed) {
                    self$results$detailed_results$setVisible(FALSE)
                }
            },

            # run ----
            .run = function() {
                
                # Check for required packages
                if (!requireNamespace("clintools", quietly = TRUE)) {
                    self$results$instructions$setContent(
                        "<div style='color: red; font-weight: bold;'>
                        Error: The 'clintools' package is required but not installed.
                        <br><br>
                        Please install it using: install.packages('clintools')
                        </div>"
                    )
                    return()
                }
                
                # Early return with instructions if no time variable selected
                if (is.null(self$options$time_var)) {
                    instructions_html <- "
                    <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px; margin: 10px 0;'>
                        <h3>Clinical Hemodynamic Monitoring</h3>
                        <p><strong>This module calculates hemodynamic indices from continuous monitoring data using the clintools package.</strong></p>
                        
                        <h4>Required Setup:</h4>
                        <ol>
                            <li><strong>Time Variable:</strong> Select your time variable (must be in seconds)</li>
                            <li><strong>Physiological Variables:</strong> Select at least one variable:
                                <ul>
                                    <li>ABP (Arterial Blood Pressure)</li>
                                    <li>MCAv (Middle Cerebral Artery Velocity)</li>
                                    <li>ICP (Intracranial Pressure)</li>
                                    <li>CPP (Cerebral Perfusion Pressure)</li>
                                    <li>HR (Heart Rate)</li>
                                </ul>
                            </li>
                        </ol>
                        
                        <h4>Calculated Indices:</h4>
                        <ul>
                            <li><strong>COest:</strong> Estimated cardiac output (requires ABP + HR)</li>
                            <li><strong>CPPopt:</strong> Optimal cerebral perfusion pressure (requires ABP + ICP)</li>
                            <li><strong>CVRi:</strong> Cardiovascular resistance index (requires ABP + MCAv)</li>
                            <li><strong>Dx:</strong> Diastolic flow index (requires CPP/ABP + MCAv)</li>
                            <li><strong>Mx:</strong> Mean flow index (requires CPP/ABP + MCAv)</li>
                            <li><strong>PI:</strong> Gosling index of pulsatility (requires MCAv)</li>
                            <li><strong>PRx:</strong> Pressure reactivity index (requires ABP + ICP)</li>
                            <li><strong>PWA:</strong> Pulse wave amplitude (any variable)</li>
                            <li><strong>RI:</strong> Pourcelots resistive index (requires MCAv)</li>
                            <li><strong>Sx:</strong> Systolic flow index (requires CPP/ABP + MCAv)</li>
                        </ul>
                        
                        <p><em>Note: Your data should have time in the first column (in seconds) followed by continuous physiological measurements.</em></p>
                    </div>"
                    
                    self$results$instructions$setContent(instructions_html)
                    return()
                } else {
                    self$results$instructions$setContent("")
                }
                
                # Get data
                data <- self$data
                if (nrow(data) == 0) {
                    stop("Data contains no rows")
                }
                
                # Prepare variables list for clintools
                variables_list <- list()
                variable_names <- c()
                
                # Add time variable (must be first)
                if (!is.null(self$options$time_var)) {
                    time_col <- self$options$time_var
                    if (!time_col %in% names(data)) {
                        stop(paste("Time variable", time_col, "not found in data"))
                    }
                    # Time should be first column for clintools
                    time_data <- data[[time_col]]
                    if (!is.numeric(time_data)) {
                        stop("Time variable must be numeric (in seconds)")
                    }
                }
                
                # Build variables list based on selected options
                if (!is.null(self$options$abp)) {
                    variables_list <- append(variables_list, "abp")
                    variable_names <- c(variable_names, self$options$abp)
                }
                
                if (!is.null(self$options$mcav)) {
                    variables_list <- append(variables_list, "mcav")
                    variable_names <- c(variable_names, self$options$mcav)
                }
                
                if (!is.null(self$options$icp)) {
                    variables_list <- append(variables_list, "icp")
                    variable_names <- c(variable_names, self$options$icp)
                }
                
                if (!is.null(self$options$cpp)) {
                    variables_list <- append(variables_list, "cpp")
                    variable_names <- c(variable_names, self$options$cpp)
                }
                
                if (!is.null(self$options$hr)) {
                    variables_list <- append(variables_list, "hr")
                    variable_names <- c(variable_names, self$options$hr)
                }
                
                # Check if we have at least one physiological variable
                if (length(variables_list) == 0) {
                    self$results$instructions$setContent(
                        "<div style='color: orange; font-weight: bold;'>
                        Warning: Please select at least one physiological variable (ABP, MCAv, ICP, CPP, or HR) to calculate hemodynamic indices.
                        </div>"
                    )
                    return()
                }
                
                # Prepare data frame for clintools (time must be first column)
                clinic_data <- data.frame(
                    time = data[[self$options$time_var]]
                )
                
                # Add selected variables
                for (i in seq_along(variable_names)) {
                    var_name <- variable_names[i]
                    if (var_name %in% names(data)) {
                        clinic_data[[var_name]] <- data[[var_name]]
                    } else {
                        stop(paste("Variable", var_name, "not found in data"))
                    }
                }
                
                # Validate numeric data
                for (col in names(clinic_data)) {
                    if (!is.numeric(clinic_data[[col]])) {
                        stop(paste("All variables must be numeric. Variable", col, "is not numeric."))
                    }
                }
                
                # Run clinmon analysis
                tryCatch({
                    results <- clintools::clinmon(
                        df = clinic_data,
                        variables = variables_list,
                        trigger = NULL,  # Could be enhanced later
                        deleter = NULL,  # Could be enhanced later
                        blocksize = self$options$blocksize,
                        epochsize = self$options$epochsize,
                        overlapping = self$options$overlapping,
                        freq = self$options$freq,
                        blockmin = self$options$blockmin,
                        epochmin = self$options$epochmin,
                        output = self$options$output_level,
                        fast = self$options$fast_processing
                    )
                    
                    # Generate summary if requested
                    if (self$options$show_summary) {
                        private$.generateSummary(results, variables_list)
                    }
                    
                    # Generate detailed results if requested
                    if (self$options$show_detailed) {
                        private$.generateDetailedResults(results)
                    }
                    
                }, error = function(e) {
                    error_msg <- paste0(
                        "<div style='color: red; font-weight: bold;'>",
                        "Error in clinmon analysis: ", e$message,
                        "<br><br>",
                        "Please check your data format and variable selections.",
                        "</div>"
                    )
                    self$results$instructions$setContent(error_msg)
                })
            },
            
            # Generate summary statistics
            .generateSummary = function(results, variables_list) {
                if (is.null(results) || nrow(results) == 0) {
                    return()
                }
                
                # Find numeric columns (hemodynamic indices)
                numeric_cols <- sapply(results, is.numeric)
                index_cols <- names(results)[numeric_cols]
                
                # Remove basic columns
                basic_cols <- c("period", "epoch", "block", "time_min", "time_max", "missing_percent")
                index_cols <- index_cols[!index_cols %in% basic_cols]
                
                # Remove mean/min/max columns (focus on indices)
                index_cols <- index_cols[!grepl("_mean$|_min$|_max$", index_cols)]
                
                if (length(index_cols) == 0) {
                    return()
                }
                
                # Calculate summary statistics
                summary_stats <- data.frame(
                    Index = character(),
                    Mean = numeric(),
                    SD = numeric(),
                    Min = numeric(),
                    Max = numeric(),
                    N = integer(),
                    Description = character(),
                    stringsAsFactors = FALSE
                )
                
                # Index descriptions
                descriptions <- list(
                    "COest" = "Estimated Cardiac Output",
                    "CPPopt" = "Optimal Cerebral Perfusion Pressure",
                    "CVRi" = "Cardiovascular Resistance Index",
                    "Dx" = "Diastolic Flow Index", 
                    "Dxc" = "Diastolic Flow Index (CPP)",
                    "Dxa" = "Diastolic Flow Index (ABP)",
                    "Mx" = "Mean Flow Index",
                    "Mxc" = "Mean Flow Index (CPP)",
                    "Mxa" = "Mean Flow Index (ABP)",
                    "PI" = "Gosling Index of Pulsatility",
                    "PRx" = "Pressure Reactivity Index",
                    "PWA" = "Pulse Wave Amplitude",
                    "RI" = "Pourcelots Resistive Index",
                    "Sx" = "Systolic Flow Index",
                    "Sxc" = "Systolic Flow Index (CPP)",
                    "Sxa" = "Systolic Flow Index (ABP)"
                )
                
                for (col in index_cols) {
                    if (col %in% names(results)) {
                        values <- results[[col]]
                        values <- values[!is.na(values)]
                        
                        if (length(values) > 0) {
                            desc <- if (col %in% names(descriptions)) descriptions[[col]] else col
                            
                            summary_stats <- rbind(summary_stats, data.frame(
                                Index = col,
                                Mean = round(mean(values), 3),
                                SD = round(sd(values), 3),
                                Min = round(min(values), 3),
                                Max = round(max(values), 3),
                                N = length(values),
                                Description = desc,
                                stringsAsFactors = FALSE
                            ))
                        }
                    }
                }
                
                # Create HTML summary
                if (nrow(summary_stats) > 0) {
                    html_content <- "<div style='font-family: Arial, sans-serif;'>"
                    html_content <- paste0(html_content, "<h3>Hemodynamic Indices Summary</h3>")
                    html_content <- paste0(html_content, "<table border='1' cellpadding='5' cellspacing='0' style='border-collapse: collapse;'>")
                    html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                    html_content <- paste0(html_content, "<th>Index</th><th>Description</th><th>Mean</th><th>SD</th><th>Min</th><th>Max</th><th>N</th></tr>")
                    
                    for (i in 1:nrow(summary_stats)) {
                        row <- summary_stats[i, ]
                        html_content <- paste0(html_content, "<tr>")
                        html_content <- paste0(html_content, "<td><strong>", row$Index, "</strong></td>")
                        html_content <- paste0(html_content, "<td>", row$Description, "</td>")
                        html_content <- paste0(html_content, "<td>", row$Mean, "</td>")
                        html_content <- paste0(html_content, "<td>", row$SD, "</td>")
                        html_content <- paste0(html_content, "<td>", row$Min, "</td>")
                        html_content <- paste0(html_content, "<td>", row$Max, "</td>")
                        html_content <- paste0(html_content, "<td>", row$N, "</td>")
                        html_content <- paste0(html_content, "</tr>")
                    }
                    
                    html_content <- paste0(html_content, "</table>")
                    
                    # Add analysis parameters
                    html_content <- paste0(html_content, "<br><h4>Analysis Parameters</h4>")
                    html_content <- paste0(html_content, "<ul>")
                    html_content <- paste0(html_content, "<li><strong>Output Level:</strong> ", self$options$output_level, "</li>")
                    html_content <- paste0(html_content, "<li><strong>Block Size:</strong> ", self$options$blocksize, " seconds</li>")
                    html_content <- paste0(html_content, "<li><strong>Epoch Size:</strong> ", self$options$epochsize, " blocks</li>")
                    html_content <- paste0(html_content, "<li><strong>Sampling Frequency:</strong> ", self$options$freq, " Hz</li>")
                    html_content <- paste0(html_content, "<li><strong>Total Analysis Periods:</strong> ", nrow(results), "</li>")
                    html_content <- paste0(html_content, "</ul>")
                    html_content <- paste0(html_content, "</div>")
                    
                    self$results$summary_stats$setContent(html_content)
                }
            },
            
            # Generate detailed results table
            .generateDetailedResults = function(results) {
                if (is.null(results) || nrow(results) == 0) {
                    return()
                }
                
                # Format the results table as HTML for better display
                html_content <- "<div style='font-family: Arial, sans-serif; overflow-x: auto;'>"
                html_content <- paste0(html_content, "<h3>Detailed Results (First 100 rows)</h3>")
                
                # Limit to first 100 rows for display
                display_results <- head(results, 100)
                
                # Create table
                html_content <- paste0(html_content, "<table border='1' cellpadding='3' cellspacing='0' style='border-collapse: collapse; font-size: 12px;'>")
                
                # Header
                html_content <- paste0(html_content, "<tr style='background-color: #f0f0f0; font-weight: bold;'>")
                for (col_name in names(display_results)) {
                    html_content <- paste0(html_content, "<th>", col_name, "</th>")
                }
                html_content <- paste0(html_content, "</tr>")
                
                # Data rows
                for (i in 1:nrow(display_results)) {
                    html_content <- paste0(html_content, "<tr>")
                    for (j in 1:ncol(display_results)) {
                        value <- display_results[i, j]
                        if (is.numeric(value)) {
                            value <- round(value, 3)
                        }
                        if (is.na(value)) {
                            value <- "NA"
                        }
                        html_content <- paste0(html_content, "<td>", value, "</td>")
                    }
                    html_content <- paste0(html_content, "</tr>")
                }
                
                html_content <- paste0(html_content, "</table>")
                
                if (nrow(results) > 100) {
                    html_content <- paste0(html_content, 
                        "<p><em>Note: Showing first 100 rows of ", nrow(results), " total rows.</em></p>")
                }
                
                html_content <- paste0(html_content, "</div>")
                
                self$results$detailed_results$setContent(html_content)
            }
        )
    )
}