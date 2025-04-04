#' @title Chi-Square Post-Hoc Tests
#' @importFrom R6 R6Class
#' @import jmvcore
#'

chisqposttestClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "chisqposttestClass",
    inherit = chisqposttestBase,
    private = list(
        .run = function() {

            # ToDo Message ----
            if (is.null(self$options$rows) || is.null(self$options$cols)) {
                todo <- "
                <br>Welcome to ClinicoPath Chi-Square Post-Hoc Tests
                <br><br>
                This tool performs a Chi-Square test followed by pairwise post-hoc tests for all combinations of category levels when the overall Chi-Square test is significant.
                <br><br>
                The post-hoc tests help identify which specific group combinations contribute to the significant overall effect.
                <hr><br>
                "
                html <- self$results$todo
                html$setContent(todo)
                return()
            } else {
                todo <- ""
                html <- self$results$todo
                html$setContent(todo)
            }

            # Error Message ----
            if (nrow(self$data) == 0) stop("Data contains no (complete) rows")

            # Prepare Data ----
            data <- self$data
            rows <- self$options$rows
            cols <- self$options$cols

            # Exclude NA ----
            excl <- self$options$excl
            if (excl) {
                data <- jmvcore::naOmit(data)
            }

            # Create the contingency table
            contTable <- table(data[[rows]], data[[cols]], useNA = if(excl) "no" else "ifany")

            # Perform Chi-Square Test ----
            chiSqTest <- stats::chisq.test(contTable, correct = FALSE)

            # Add chi-square results to the table ----
            self$results$chisqTable$setRow(
                rowNo = 1,
                values = list(
                    stat = "Chi-Square",
                    value = chiSqTest$statistic,
                    df = chiSqTest$parameter,
                    p = chiSqTest$p.value
                )
            )

            # Format contingency table with optional expected values ----
            if (self$options$exp) {
                # Include expected values
                formattedTable <- vcd::structable(contTable)
                expValues <- chiSqTest$expected

                # Create HTML table with observed and expected values
                tableHtml <- "<table border='1' style='border-collapse: collapse;'>"
                tableHtml <- paste0(tableHtml, "<tr><th></th>")

                # Add column headers
                colNames <- colnames(contTable)
                for (col in colNames) {
                    tableHtml <- paste0(tableHtml, "<th>", col, "</th>")
                }
                tableHtml <- paste0(tableHtml, "</tr>")

                # Add rows with observed and expected values
                rowNames <- rownames(contTable)
                for (i in 1:length(rowNames)) {
                    tableHtml <- paste0(tableHtml, "<tr><th>", rowNames[i], "</th>")
                    for (j in 1:length(colNames)) {
                        obs <- contTable[i, j]
                        exp <- round(expValues[i, j], 1)
                        tableHtml <- paste0(tableHtml, "<td>", obs, "<br><small>(", exp, ")</small></td>")
                    }
                    tableHtml <- paste0(tableHtml, "</tr>")
                }

                tableHtml <- paste0(tableHtml, "</table>")
                tableHtml <- paste0("<p>Values shown as: Observed<br><small>(Expected)</small></p>", tableHtml)
            } else {
                # Show only observed values
                formattedTable <- vcd::structable(contTable)

                # Convert to HTML table
                tableHtml <- "<table border='1' style='border-collapse: collapse;'>"
                tableHtml <- paste0(tableHtml, "<tr><th></th>")

                # Add column headers
                colNames <- colnames(contTable)
                for (col in colNames) {
                    tableHtml <- paste0(tableHtml, "<th>", col, "</th>")
                }
                tableHtml <- paste0(tableHtml, "</tr>")

                # Add rows with observed values
                rowNames <- rownames(contTable)
                for (i in 1:length(rowNames)) {
                    tableHtml <- paste0(tableHtml, "<tr><th>", rowNames[i], "</th>")
                    for (j in 1:length(colNames)) {
                        obs <- contTable[i, j]
                        tableHtml <- paste0(tableHtml, "<td>", obs, "</td>")
                    }
                    tableHtml <- paste0(tableHtml, "</tr>")
                }

                tableHtml <- paste0(tableHtml, "</table>")
            }

            self$results$contingencyTable$setContent(tableHtml)

            # Perform post-hoc tests if chi-square is significant ----
            if (chiSqTest$p.value < self$options$sig) {
                # Set adjustment method based on user selection
                adjustMethod <- self$options$posthoc
                if (adjustMethod == "none") {
                    adjustMethod <- "none"
                }

                # Perform post-hoc tests using chisq.posthoc.test package
                posthocResults <- try({
                    chisq.posthoc.test::chisq.posthoc.test(contTable, method = adjustMethod)
                }, silent = TRUE)

                if (!inherits(posthocResults, "try-error")) {
                    # Extract pairwise comparisons
                    pairwiseComparisons <- posthocResults$pairwise.p.values
                    rawPValues <- posthocResults$raw.p.values

                    # Add each comparison to the results table
                    rowKey <- 1
                    for (i in 1:nrow(pairwiseComparisons)) {
                        pair <- rownames(pairwiseComparisons)[i]

                        # Parse the comparison label
                        parts <- strsplit(pair, ":")[[1]]
                        group1 <- gsub("^\\{|\\}$", "", parts[1])
                        group2 <- gsub("^\\{|\\}$", "", parts[2])

                        # Get p-values and chi-square
                        rawP <- rawPValues[pair]
                        adjP <- pairwiseComparisons[i, "p adj"]

                        # Calculate chi-square value (approximate)
                        chiVal <- qchisq(1 - rawP, df = 1)

                        # Determine significance
                        sigStatus <- ifelse(adjP < self$options$sig, "Yes", "No")

                        # Add to table
                        self$results$posthocTable$addRow(
                            rowKey = rowKey,
                            values = list(
                                comparison = paste(group1, "vs", group2),
                                chi = chiVal,
                                p = rawP,
                                padj = adjP,
                                sig = sigStatus
                            )
                        )

                        rowKey <- rowKey + 1
                    }
                } else {
                    # Fallback to manual pairwise calculations if package fails
                    # This calculates all possible 2x2 tables from the original table

                    rowLevels <- rownames(contTable)
                    colLevels <- colnames(contTable)
                    rowKey <- 1

                    # Perform all pairwise comparisons between row categories
                    if (length(rowLevels) >= 2) {
                        for (i in 1:(length(rowLevels)-1)) {
                            for (j in (i+1):length(rowLevels)) {
                                # Create 2x2 contingency table
                                subtable <- contTable[c(i,j),, drop=FALSE]

                                # Skip if table is degenerate
                                if (any(dim(subtable) < 2)) next

                                # Perform chi-square test on this 2x2 table
                                subtest <- stats::chisq.test(subtable, correct=FALSE)

                                # Store results
                                self$results$posthocTable$addRow(
                                    rowKey = rowKey,
                                    values = list(
                                        comparison = paste("Row:", rowLevels[i], "vs", rowLevels[j]),
                                        chi = subtest$statistic,
                                        p = subtest$p.value,
                                        padj = p.adjust(subtest$p.value, method = adjustMethod),
                                        sig = ifelse(p.adjust(subtest$p.value, method = adjustMethod) < self$options$sig, "Yes", "No")
                                    )
                                )

                                rowKey <- rowKey + 1
                            }
                        }
                    }

                    # Perform all pairwise comparisons between column categories
                    if (length(colLevels) >= 2) {
                        for (i in 1:(length(colLevels)-1)) {
                            for (j in (i+1):length(colLevels)) {
                                # Create 2x2 contingency table
                                subtable <- contTable[,c(i,j), drop=FALSE]

                                # Skip if table is degenerate
                                if (any(dim(subtable) < 2)) next

                                # Perform chi-square test on this 2x2 table
                                subtest <- stats::chisq.test(subtable, correct=FALSE)

                                # Store results
                                self$results$posthocTable$addRow(
                                    rowKey = rowKey,
                                    values = list(
                                        comparison = paste("Col:", colLevels[i], "vs", colLevels[j]),
                                        chi = subtest$statistic,
                                        p = subtest$p.value,
                                        padj = p.adjust(subtest$p.value, method = adjustMethod),
                                        sig = ifelse(p.adjust(subtest$p.value, method = adjustMethod) < self$options$sig, "Yes", "No")
                                    )
                                )

                                rowKey <- rowKey + 1
                            }
                        }
                    }
                }
            }
        },

        .plot = function(image, ggtheme, theme, ...) {
            # Only run if plot option is selected
            if (!self$options$plot) {
                return(FALSE)
            }

            # Get data
            data <- self$data
            rows <- self$options$rows
            cols <- self$options$cols

            # Exclude NA if selected
            excl <- self$options$excl
            if (excl) {
                data <- jmvcore::naOmit(data)
            }

            # Create the contingency table
            contTable <- table(data[[rows]], data[[cols]], useNA = if(excl) "no" else "ifany")

            # Perform Chi-Square Test
            chiSqTest <- stats::chisq.test(contTable, correct = FALSE)

            # Create mosaic plot with residuals
            # Use vcd package for visualization of standardized residuals
            vcd::mosaic(contTable, shade = TRUE,
                        legend = TRUE,
                        gp = vcd::gpar(fill = vcd::hcl_palettes("Blue-Red", n = 5)))

            # Return TRUE to indicate the plot was created successfully
            return(TRUE)
        }
    )
)
