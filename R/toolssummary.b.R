#' @title Tools for data summary with summarytools integration
#' @return Table
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#' @importFrom stats median sd quantile
#' @importFrom utils head tail
#' @importFrom summarytools dfSummary freq descr ctable

toolssummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "toolssummaryClass",
    inherit = toolssummaryBase,
    private = list(
        .run = function() {

            # Check if data and variables are provided
            if (length(self$options$vars) == 0) {
                # Show welcome message if no variables selected
                todo <- "
                <br>Welcome to ClinicoPath Data Summary Tool with summarytools Integration
                <br><br>
                This enhanced tool provides comprehensive summaries of your data:
                <br>
                <strong>Standard Features:</strong>
                <br>- Basic variable information (type, missing values, unique values)
                <br>- Missing value analysis
                <br>- Frequency distributions for categorical variables
                <br>- Summary statistics for numeric variables
                <br><br>
                <strong>summarytools Enhanced Features:</strong>
                <br>- Data Frame Summary (dfSummary): Complete overview with plots and statistics
                <br>- Enhanced Descriptive Statistics (descr): Comprehensive numeric summaries
                <br>- Professional Frequency Tables (freq): Publication-ready frequency analysis
                <br>- Cross-tabulation Tables (ctable): Cross-tabs for categorical analysis
                <br>- Grouped Analysis: Stratified summaries by grouping variables
                <br><br>
                Select variables from your dataset to begin the analysis.
                <br>Use the 'Use summarytools package' option for enhanced professional output.
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {
                # Clear the todo message when variables are selected
                html <- self$results$todo
                html$setContent("")
            }

            # Check for data
            if (nrow(self$data) == 0) {
                stop('Data contains no (complete) rows')
            }

            # Prepare Data
            data <- self$data
            varsNames <- self$options$vars
            mydata <- jmvcore::select(data, varsNames)

            # Handle missing values for analysis (but keep original data for frequency tables)
            if (self$options$excludeNA) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Basic summary for all variables
            for (var in varsNames) {
                # Get variable type and basic info
                varType <- class(data[[var]])[1]
                nMissing <- sum(is.na(data[[var]]))
                nUnique <- length(unique(data[[var]]))
                nTotal <- length(data[[var]])

                # Calculate appropriate statistics based on type
                if (is.numeric(data[[var]])) {
                    stats <- sprintf(
                        "Mean: %.2f, SD: %.2f, Median: %.2f",
                        mean(data[[var]], na.rm=TRUE),
                        sd(data[[var]], na.rm=TRUE),
                        median(data[[var]], na.rm=TRUE)
                    )
                } else {
                    # Get top categories for categorical variables
                    varTable <- table(data[[var]], useNA="no")
                    if (length(varTable) > 0) {
                        topCats <- names(sort(varTable, decreasing=TRUE)[1:min(3, length(varTable))])
                        stats <- sprintf(
                            "Most common: %s",
                            paste(topCats, collapse=", ")
                        )
                    } else {
                        stats <- "No valid categories"
                    }
                }

                # Add to summary table
                self$results$summary$addRow(rowKey=var, values=list(
                    variable=var,
                    type=varType,
                    missing=nMissing,
                    unique=nUnique,
                    stats=stats
                ))
            }

            # Detailed numeric statistics
            if (self$options$showStats) {
                for (var in varsNames) {
                    if (is.numeric(data[[var]])) {
                        self$results$numericStats$addRow(rowKey=var, values=list(
                            variable=var,
                            mean=round(mean(data[[var]], na.rm=TRUE), 3),
                            sd=round(sd(data[[var]], na.rm=TRUE), 3),
                            median=round(median(data[[var]], na.rm=TRUE), 3),
                            min=round(min(data[[var]], na.rm=TRUE), 3),
                            max=round(max(data[[var]], na.rm=TRUE), 3)
                        ))
                    }
                }
            }

            # Enhanced frequency tables for categorical variables
            if (self$options$showFreq) {
                freqTables <- ""

                # Add CSS styling for better table appearance
                freqTables <- paste0(
                    freqTables,
                    "<style>
                    .freq-table {
                        width: 100%;
                        border-collapse: collapse;
                        margin-bottom: 25px;
                        font-family: Arial, sans-serif;
                        border: 1px solid #ddd;
                    }
                    .freq-table th {
                        background-color: #f8f9fa;
                        border: 1px solid #ddd;
                        padding: 12px 8px;
                        text-align: left;
                        font-weight: bold;
                        color: #333;
                    }
                    .freq-table td {
                        border: 1px solid #ddd;
                        padding: 10px 8px;
                        text-align: left;
                    }
                    .freq-table tr:nth-child(even) {
                        background-color: #f9f9f9;
                    }
                    .freq-table tr:hover {
                        background-color: #f5f5f5;
                    }
                    .variable-title {
                        font-size: 16px;
                        font-weight: bold;
                        margin: 20px 0 10px 0;
                        color: #333;
                        border-bottom: 2px solid #007bff;
                        padding-bottom: 5px;
                    }
                    .numeric-right {
                        text-align: right;
                    }
                    .missing-row {
                        font-style: italic;
                        color: #666;
                    }
                    </style>"
                )

                for (var in varsNames) {
                    # Process both factor and character variables, and numeric with few unique values
                    if (is.factor(data[[var]]) || is.character(data[[var]]) ||
                        (is.numeric(data[[var]]) && length(unique(data[[var]])) <= 10)) {

                        # Get frequency table including NA values
                        freq <- table(data[[var]], useNA="ifany")
                        nTotal <- length(data[[var]])
                        prop <- (freq / nTotal) * 100

                        # Start variable section with title
                        freqTables <- paste0(
                            freqTables,
                            sprintf('<div class="variable-title">%s</div>', var),
                            '<table class="freq-table">',
                            '<thead>',
                            '<tr><th style="width: 40%;">Category</th><th style="width: 20%;">Count</th><th style="width: 25%;">Percentage</th><th style="width: 15%;">Valid %</th></tr>',
                            '</thead>',
                            '<tbody>'
                        )

                        # Calculate valid percentage (excluding NA)
                        nValid <- sum(!is.na(data[[var]]))

                        # Add each category row
                        for (i in seq_along(freq)) {
                            categoryName <- names(freq)[i]
                            count <- freq[i]
                            percentage <- prop[i]

                            # Calculate valid percentage (excluding NAs from denominator)
                            if (is.na(categoryName) || categoryName == "NA") {
                                validPercentage <- "—"
                                rowClass <- "missing-row"
                                categoryDisplay <- "Missing (NA)"
                            } else {
                                validPercentage <- sprintf("%.1f%%", (count / nValid) * 100)
                                rowClass <- ""
                                categoryDisplay <- categoryName
                            }

                            freqTables <- paste0(
                                freqTables,
                                sprintf(
                                    '<tr class="%s"><td>%s</td><td class="numeric-right">%d</td><td class="numeric-right">%.1f%%</td><td class="numeric-right">%s</td></tr>',
                                    rowClass,
                                    categoryDisplay,
                                    count,
                                    percentage,
                                    validPercentage
                                )
                            )
                        }

                        # Add total row
                        freqTables <- paste0(
                            freqTables,
                            '<tr style="border-top: 2px solid #333; font-weight: bold;">',
                            sprintf('<td>Total</td><td class="numeric-right">%d</td><td class="numeric-right">100.0%%</td><td class="numeric-right">100.0%%</td>', nTotal),
                            '</tr>',
                            '</tbody>',
                            '</table>'
                        )
                    }
                }

                # Set the formatted content
                self$results$frequencies$setContent(freqTables)
            }

            # summarytools Integration
            if (self$options$useSummarytools) {
                private$.generateSummaryToolsOutput()
            }
        },

        .generateSummaryToolsOutput = function() {
            # Check if summarytools is available
            if (!requireNamespace("summarytools", quietly = TRUE)) {
                warning("summarytools package not available. Skipping enhanced summaries.")
                return()
            }

            data <- self$data
            varsNames <- self$options$vars
            mydata <- jmvcore::select(data, varsNames)
            
            # Add grouping variable if specified
            groupVar <- self$options$groupVar
            if (!is.null(groupVar) && length(groupVar) > 0) {
                mydata[[groupVar]] <- data[[groupVar]]
            }

            # Handle missing values if requested
            if (self$options$excludeNA) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Data Frame Summary using dfSummary
            if (self$options$showDfSummary) {
                tryCatch({
                    if (!is.null(groupVar) && length(groupVar) > 0) {
                        # Grouped dfSummary
                        dfsum_result <- summarytools::dfSummary(
                            mydata[varsNames], 
                            style = "grid",
                            graph.magnif = 0.75,
                            valid.col = TRUE,
                            tmp.img.dir = "/tmp"
                        )
                    } else {
                        # Standard dfSummary
                        dfsum_result <- summarytools::dfSummary(
                            mydata[varsNames], 
                            style = "grid",
                            graph.magnif = 0.75,
                            valid.col = TRUE,
                            tmp.img.dir = "/tmp"
                        )
                    }
                    
                    # Convert to HTML
                    dfsum_html <- print(dfsum_result, 
                                      method = "render", 
                                      file = tempfile(fileext = ".html"),
                                      report.title = "Data Frame Summary")
                    
                    # Read and set content
                    dfsum_content <- paste(readLines(dfsum_html, warn = FALSE), collapse = "\n")
                    self$results$dfSummary$setContent(dfsum_content)
                    
                }, error = function(e) {
                    warning(paste("Error generating dfSummary:", e$message))
                    self$results$dfSummary$setContent(paste("Error generating data frame summary:", e$message))
                })
            }

            # Enhanced Descriptive Statistics using descr
            if (self$options$showDescr) {
                tryCatch({
                    # Get only numeric variables for descr
                    numeric_vars <- varsNames[sapply(mydata[varsNames], is.numeric)]
                    
                    if (length(numeric_vars) > 0) {
                        if (!is.null(groupVar) && length(groupVar) > 0) {
                            # Grouped descriptive statistics
                            descr_result <- summarytools::descr(
                                mydata[numeric_vars], 
                                stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv", "skewness", "se.skewness", "kurtosis"),
                                transpose = TRUE,
                                headings = TRUE,
                                style = "rmarkdown"
                            )
                        } else {
                            # Standard descriptive statistics
                            descr_result <- summarytools::descr(
                                mydata[numeric_vars], 
                                stats = c("mean", "sd", "min", "q1", "med", "q3", "max", "mad", "iqr", "cv", "skewness", "se.skewness", "kurtosis"),
                                transpose = TRUE,
                                headings = TRUE,
                                style = "rmarkdown"
                            )
                        }
                        
                        # Convert to HTML
                        descr_html <- print(descr_result, method = "render", file = tempfile(fileext = ".html"))
                        descr_content <- paste(readLines(descr_html, warn = FALSE), collapse = "\n")
                        self$results$descrStats$setContent(descr_content)
                    } else {
                        self$results$descrStats$setContent("No numeric variables selected for descriptive statistics.")
                    }
                    
                }, error = function(e) {
                    warning(paste("Error generating descriptive statistics:", e$message))
                    self$results$descrStats$setContent(paste("Error generating descriptive statistics:", e$message))
                })
            }

            # Enhanced Frequency Tables using freq
            if (self$options$showFreq) {
                tryCatch({
                    freq_html_content <- ""
                    
                    # Get categorical and low-cardinality numeric variables
                    cat_vars <- varsNames[sapply(mydata[varsNames], function(x) {
                        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
                    })]
                    
                    if (length(cat_vars) > 0) {
                        for (var in cat_vars) {
                            freq_result <- summarytools::freq(
                                mydata[[var]], 
                                style = "rmarkdown",
                                plain.ascii = FALSE,
                                justify = "left",
                                cumul = TRUE,
                                report.nas = TRUE,
                                headings = TRUE
                            )
                            
                            # Convert to HTML
                            freq_html <- print(freq_result, method = "render", file = tempfile(fileext = ".html"))
                            freq_var_content <- paste(readLines(freq_html, warn = FALSE), collapse = "\n")
                            freq_html_content <- paste(freq_html_content, 
                                                     sprintf("<h3>%s</h3>", var),
                                                     freq_var_content,
                                                     "<br>", sep = "\n")
                        }
                        
                        self$results$summaryToolsFreq$setContent(freq_html_content)
                    } else {
                        self$results$summaryToolsFreq$setContent("No categorical variables selected for frequency analysis.")
                    }
                    
                }, error = function(e) {
                    warning(paste("Error generating frequency tables:", e$message))
                    self$results$summaryToolsFreq$setContent(paste("Error generating frequency tables:", e$message))
                })
            }

            # Cross-tabulation Tables using ctable
            if (self$options$showCrosstabs && !is.null(groupVar) && length(groupVar) > 0) {
                tryCatch({
                    crosstab_html_content <- ""
                    
                    # Get categorical variables for cross-tabulation
                    cat_vars <- varsNames[sapply(mydata[varsNames], function(x) {
                        is.factor(x) || is.character(x) || (is.numeric(x) && length(unique(x)) <= 10)
                    })]
                    
                    if (length(cat_vars) > 0) {
                        for (var in cat_vars) {
                            if (var != groupVar) {  # Don't cross-tabulate with itself
                                ctable_result <- summarytools::ctable(
                                    mydata[[var]], 
                                    mydata[[groupVar]],
                                    style = "rmarkdown",
                                    prop = "r",  # Row proportions
                                    chisq = TRUE,
                                    headings = TRUE
                                )
                                
                                # Convert to HTML
                                ctable_html <- print(ctable_result, method = "render", file = tempfile(fileext = ".html"))
                                ctable_var_content <- paste(readLines(ctable_html, warn = FALSE), collapse = "\n")
                                crosstab_html_content <- paste(crosstab_html_content,
                                                             sprintf("<h3>%s by %s</h3>", var, groupVar),
                                                             ctable_var_content,
                                                             "<br>", sep = "\n")
                            }
                        }
                        
                        self$results$crosstabs$setContent(crosstab_html_content)
                    } else {
                        self$results$crosstabs$setContent("No categorical variables available for cross-tabulation.")
                    }
                    
                }, error = function(e) {
                    warning(paste("Error generating cross-tabulation tables:", e$message))
                    self$results$crosstabs$setContent(paste("Error generating cross-tabulation tables:", e$message))
                })
            }
        }
    )
)
