#' @title Tools for data summary
#' @return Table
#' @importFrom R6 R6Class
#' @importFrom jmvcore toNumeric
#' @importFrom stats median sd quantile
#' @importFrom utils head tail

toolssummaryClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "toolssummaryClass",
    inherit = toolssummaryBase,
    private = list(
        .run = function() {

            # Check if data and variables are provided
            if (length(self$options$vars) == 0) {
                # Show welcome message if no variables selected
                todo <- "
                <br>Welcome to ClinicoPath Data Summary Tool
                <br><br>
                This tool provides comprehensive summaries of your data:
                <br>
                - Basic variable information (type, missing values, unique values)
                - Missing value analysis
                - Frequency distributions for categorical variables
                - Summary statistics for numeric variables
                <br><br>
                Select variables from your dataset to begin the analysis.
                <br><br>
                The frequency tables will show counts and percentages for each category,
                properly formatted for clinical research presentations.
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
        }
    )
)
