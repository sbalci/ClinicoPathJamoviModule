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
                - Basic variable information
                - Missing value analysis
                - Frequency distributions
                - Summary statistics
                <br><br>
                Select variables to begin the analysis.
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            }

            # Prepare Data
            data <- self$data
            varsNames <- self$options$vars
            mydata <- jmvcore::select(data, varsNames)

            # Handle missing values
            if (self$options$excludeNA) {
                mydata <- jmvcore::naOmit(mydata)
            }

            # Basic summary for all variables
            for (var in varsNames) {
                # Get variable type and basic info
                varType <- class(data[[var]])[1]
                nMissing <- sum(is.na(data[[var]]))
                nUnique <- length(unique(data[[var]]))

                # Calculate appropriate statistics based on type
                if (is.numeric(data[[var]])) {
                    stats <- sprintf(
                        "Mean: %.2f, SD: %.2f, Median: %.2f",
                        mean(data[[var]], na.rm=TRUE),
                        sd(data[[var]], na.rm=TRUE),
                        median(data[[var]], na.rm=TRUE)
                    )
                } else {
                    topCats <- names(sort(table(data[[var]]), decreasing=TRUE)[1:3])
                    stats <- sprintf(
                        "Most common: %s",
                        paste(topCats, collapse=", ")
                    )
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
                            mean=mean(data[[var]], na.rm=TRUE),
                            sd=sd(data[[var]], na.rm=TRUE),
                            median=median(data[[var]], na.rm=TRUE),
                            min=min(data[[var]], na.rm=TRUE),
                            max=max(data[[var]], na.rm=TRUE)
                        ))
                    }
                }
            }

            # Frequency tables for categorical variables
            if (self$options$showFreq) {
                freqTables <- ""
                for (var in varsNames) {
                    if (is.factor(data[[var]]) || is.character(data[[var]])) {
                        freq <- table(data[[var]], useNA="ifany")
                        prop <- prop.table(freq) * 100

                        freqTables <- paste0(
                            freqTables,
                            sprintf("<h4>%s</h4>", var),
                            "<table style='width:100%; margin-bottom:20px'>",
                            "<tr><th>Category</th><th>Count</th><th>Percentage</th></tr>"
                        )

                        for (i in seq_along(freq)) {
                            freqTables <- paste0(
                                freqTables,
                                sprintf(
                                    "<tr><td>%s</td><td>%d</td><td>%.1f%%</td></tr>",
                                    names(freq)[i],
                                    freq[i],
                                    prop[i]
                                )
                            )
                        }
                        freqTables <- paste0(freqTables, "</table>")
                    }
                }
                self$results$frequencies$setContent(freqTables)
            }
        }
    )
)
