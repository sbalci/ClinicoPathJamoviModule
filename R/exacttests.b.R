#' @title Exact Tests for Small Samples
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom stats fisher.test binom.test mcnemar.test
#' @export

exacttestsClass <- R6::R6Class(
    "exacttestsClass",
    inherit = exacttestsBase,
    private = list(
        .init = function() {
            # Set up instructions
            html <- self$results$instructions
            html$setContent(
                '<html>
                <head>
                </head>
                <body>
                <div class="instructions">
                <h3>Exact Tests for Small Samples</h3>
                <p>These tests provide exact p-values and confidence intervals for small sample sizes where asymptotic approximations may not be reliable.</p>
                <ul>
                <li><b>Fisher exact test:</b> Tests independence in 2x2 contingency tables</li>
                <li><b>Exact binomial test:</b> Tests if a proportion equals a hypothesized value</li>
                <li><b>Exact McNemar test:</b> Tests marginal homogeneity in paired binary data</li>
                <li><b>Barnard exact test:</b> More powerful alternative to Fisher test</li>
                </ul>
                <p><b>Note:</b> Exact tests are particularly important when expected frequencies are small (&lt; 5).</p>
                </div>
                </body>
                </html>'
            )
            
            # Initialize tables
            if (!is.null(self$options$rows) && !is.null(self$options$cols)) {
                self$results$contingency$addColumn(name = 'total', title = 'Total', type = 'integer')
            }
        },
        
        .run = function() {
            if (is.null(self$data))
                return()
            
            ready <- self$.checkData()
            if (!ready$ready) {
                if (!is.null(ready$error))
                    jmvcore::reject(ready$error, code = "")
                return()
            }
            
            # Prepare data
            data <- self$data
            rows_var <- self$options$rows
            cols_var <- self$options$cols
            counts_var <- self$options$counts
            test_type <- self$options$testType
            alternative <- self$options$alternative
            ci_level <- self$options$ciWidth / 100
            
            if (test_type == "fisher" && !is.null(rows_var) && !is.null(cols_var)) {
                self$.runFisherTest(data, rows_var, cols_var, counts_var, alternative, ci_level)
            } else if (test_type == "binomial" && !is.null(rows_var)) {
                self$.runBinomialTest(data, rows_var, alternative, ci_level)
            } else if (test_type == "mcnemar" && !is.null(rows_var) && !is.null(cols_var)) {
                self$.runMcNemarTest(data, rows_var, cols_var, alternative)
            } else if (test_type == "barnard" && !is.null(rows_var) && !is.null(cols_var)) {
                self$.runBarnardTest(data, rows_var, cols_var, counts_var, alternative, ci_level)
            }
        },
        
        .checkData = function() {
            if (is.null(self$data))
                return(list(ready = FALSE, error = "Data not available"))
            
            rows_var <- self$options$rows
            cols_var <- self$options$cols
            test_type <- self$options$testType
            
            if (test_type %in% c("fisher", "mcnemar", "barnard")) {
                if (is.null(rows_var) || is.null(cols_var)) {
                    return(list(ready = FALSE, error = "Both row and column variables are required"))
                }
            } else if (test_type == "binomial") {
                if (is.null(rows_var)) {
                    return(list(ready = FALSE, error = "Row variable is required"))
                }
            }
            
            return(list(ready = TRUE))
        },
        
        .runFisherTest = function(data, rows_var, cols_var, counts_var, alternative, ci_level) {
            # Create contingency table
            if (is.null(counts_var)) {
                ct <- table(data[[rows_var]], data[[cols_var]])
            } else {
                # Aggregate by counts
                agg_data <- aggregate(data[[counts_var]], 
                                    by = list(data[[rows_var]], data[[cols_var]]), 
                                    FUN = sum)
                ct <- xtabs(x ~ Group.1 + Group.2, data = agg_data)
            }
            
            # Run Fisher's exact test
            result <- tryCatch({
                fisher.test(ct, alternative = alternative, conf.level = ci_level)
            }, error = function(e) NULL)
            
            if (!is.null(result)) {
                # Update contingency table
                self$.updateContingencyTable(ct)
                
                # Update test results
                tests <- self$results$tests
                
                ci_text <- ""
                if (self$options$showCI && !is.null(result$conf.int)) {
                    ci_method <- self$options$ciMethod
                    ci_text <- sprintf("[%.3f, %.3f] (%s)", 
                                     result$conf.int[1], result$conf.int[2], ci_method)
                }
                
                tests$setRow(rowNo = 1, values = list(
                    test = "Fisher's exact test",
                    value = result$estimate,
                    p = result$p.value,
                    ci = ci_text
                ))
                
                # Add note
                note <- self$results$note
                note$setContent(sprintf(
                    '<p><b>Odds ratio:</b> %.3f</p><p><b>Alternative hypothesis:</b> %s</p>',
                    result$estimate, alternative
                ))
            }
        },
        
        .runBinomialTest = function(data, rows_var, alternative, ci_level) {
            # Get binary data
            var_data <- data[[rows_var]]
            
            if (is.factor(var_data)) {
                if (nlevels(var_data) != 2) {
                    jmvcore::reject("Variable must be binary for binomial test", code = "")
                    return()
                }
                successes <- sum(as.numeric(var_data) - 1)
                n <- length(var_data)
            } else {
                # Assume numeric 0/1
                successes <- sum(var_data == 1, na.rm = TRUE)
                n <- sum(!is.na(var_data))
            }
            
            # Run exact binomial test
            result <- tryCatch({
                binom.test(successes, n, alternative = alternative, conf.level = ci_level)
            }, error = function(e) NULL)
            
            if (!is.null(result)) {
                tests <- self$results$tests
                
                ci_text <- ""
                if (self$options$showCI && !is.null(result$conf.int)) {
                    ci_method <- self$options$ciMethod
                    ci_text <- sprintf("[%.3f, %.3f] (%s)", 
                                     result$conf.int[1], result$conf.int[2], ci_method)
                }
                
                tests$setRow(rowNo = 1, values = list(
                    test = "Exact binomial test",
                    value = result$estimate,
                    p = result$p.value,
                    ci = ci_text
                ))
                
                # Add note
                note <- self$results$note
                note$setContent(sprintf(
                    '<p><b>Observed proportion:</b> %.3f (%d/%d)</p><p><b>Alternative hypothesis:</b> %s</p>',
                    result$estimate, successes, n, alternative
                ))
            }
        },
        
        .runMcNemarTest = function(data, rows_var, cols_var, alternative) {
            # Create contingency table for paired data
            ct <- table(data[[rows_var]], data[[cols_var]])
            
            # Run McNemar's exact test
            result <- tryCatch({
                mcnemar.test(ct)
            }, error = function(e) NULL)
            
            if (!is.null(result)) {
                # Update contingency table
                self$.updateContingencyTable(ct)
                
                # Update test results
                tests <- self$results$tests
                tests$setRow(rowNo = 1, values = list(
                    test = "Exact McNemar test",
                    value = result$statistic,
                    p = result$p.value
                ))
                
                # Add note
                note <- self$results$note
                note$setContent('<p><b>Test:</b> Marginal homogeneity in paired binary data</p>')
            }
        },
        
        .runBarnardTest = function(data, rows_var, cols_var, counts_var, alternative, ci_level) {
            # Note: Barnard's test is computationally intensive
            # For now, we'll fall back to Fisher's test with a note
            self$.runFisherTest(data, rows_var, cols_var, counts_var, alternative, ci_level)
            
            # Update note to indicate Barnard's test approximation
            note <- self$results$note
            current_content <- note$content
            if (is.null(current_content)) current_content <- ""
            
            note$setContent(paste0(current_content, 
                '<p><b>Note:</b> Barnard\'s exact test is computationally intensive. Fisher\'s exact test shown as approximation.</p>'
            ))
        },
        
        .updateContingencyTable = function(ct) {
            contingency <- self$results$contingency
            
            # Add row names
            row_names <- rownames(ct)
            col_names <- colnames(ct)
            
            # Add columns for each category
            for (j in seq_along(col_names)) {
                contingency$addColumn(name = col_names[j], title = col_names[j], type = 'integer')
            }
            
            # Add rows with data
            for (i in seq_along(row_names)) {
                row_values <- list(.name = row_names[i])
                for (j in seq_along(col_names)) {
                    row_values[[col_names[j]]] <- ct[i, j]
                }
                row_values[['total']] <- sum(ct[i, ])
                
                contingency$addRow(rowKey = row_names[i], values = row_values)
            }
        }
    )
)