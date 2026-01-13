
# This file is a generated template, your changes will not be overwritten

missingdataexplorerClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "missingdataexplorerClass",
    inherit = missingdataexplorerBase,
    private = list(
        .run = function() {
            # 1. Variable validation
            analysis_vars <- self$options$analysis_vars
            if (length(analysis_vars) == 0) {
                return()
            }

            # 2. Data preparation
            data <- self$data
            data_subset <- data[, analysis_vars, drop = FALSE]

            # 3. Perform Analysis
            tryCatch({
                # a. Summary Table
                private$.populateSummaryTable(data_subset)
                
                # b. MCAR Test
                if (self$options$mechanism_testing) {
                    private$.populateMCARTest(data_subset)
                }
                
                # c. Pattern Analysis
                if (self$options$pattern_analysis) {
                    private$.populatePatternTable(data_subset)
                }
                
                # d. Correlation Analysis
                if (self$options$correlation_analysis) {
                    private$.populateCorrelationTable(data_subset)
                }

                # Store for plots
                private$.data_subset <- data_subset
                
            }, error = function(e) {
                jmvcore::reject(paste("Analysis error: ", e$message))
            })
        },

        .data_subset = NULL,

        .populateSummaryTable = function(data) {
            table <- self$results$summary_statistics
            
            n_obs <- nrow(data)
            for (var in colnames(data)) {
                n_miss <- sum(is.na(data[[var]]))
                pct_miss <- (n_miss / n_obs) * 100
                
                table$addRow(rowKey = var, values = list(
                    variable = var,
                    n_missing = n_miss,
                    pct_missing = pct_miss,
                    n_complete = n_obs - n_miss,
                    pct_complete = 100 - pct_miss
                ))
            }
        },

        .populateMCARTest = function(data) {
            if (!requireNamespace("naniar", quietly = TRUE)) {
                self$results$mcar_test_results$setNote("pkg_missing", "The 'naniar' package is required for Little's MCAR test.")
                return()
            }
            
            # Little's MCAR test (only for numeric data mostly, but naniar handles it)
            # It requires at least some missing data
            if (sum(is.na(data)) == 0) {
                self$results$mcar_test_results$setNote("no_missing", "No missing data detected. MCAR test not applicable.")
                return()
            }

            try({
                mcar_res <- naniar::mcar_test(data)
                table <- self$results$mcar_test_results
                table$addRow(rowKey = "little", values = list(
                    test = "Little's MCAR Test",
                    statistic = mcar_res$statistic,
                    df = mcar_res$df,
                    p_value = mcar_res$p.value,
                    interpretation = if (mcar_res$p.value < 0.05) "Reject MCAR (Missingness may be MAR or MNAR)" else "Fail to reject MCAR"
                ))
            }, silent = TRUE)
        },

        .populatePatternTable = function(data) {
            # Use mice::md.pattern for efficiency
            if (!requireNamespace("mice", quietly = TRUE)) return()
            
            patterns <- mice::md.pattern(data, plot = FALSE)
            # patterns is a matrix where rows are patterns, columns are variables, 
            # last column is number of missing variables in pattern, 
            # first column is frequency of pattern.
            
            table <- self$results$missing_patterns
            
            # Sort by frequency
            n_patterns <- nrow(patterns) - 1 # exclude the variable-wise count row
            freqs <- patterns[1:n_patterns, 1]
            vars_miss <- patterns[1:n_patterns, ncol(patterns)]
            
            # Only show top patterns or those above threshold
            threshold <- self$options$min_pattern_freq * nrow(data)
            
            for (i in seq_len(min(n_patterns, self$options$max_patterns_display))) {
                if (freqs[i] < threshold) next
                
                # Identify which variables are missing in this pattern (they have 0 in patterns matrix)
                row_vals <- patterns[i, 2:(ncol(patterns)-1)]
                missing_vars <- names(row_vals)[row_vals == 0]
                
                table$addRow(rowKey = i, values = list(
                    pattern_id = i,
                    n_cases = freqs[i],
                    pct_cases = (freqs[i] / nrow(data)) * 100,
                    n_vars_missing = vars_miss[i],
                    variables_missing = paste(missing_vars, collapse = ", ")
                ))
            }
        },

        .populateCorrelationTable = function(data) {
            # Correlation of missingness indicators
            miss_idx <- as.data.frame(is.na(data))
            # Remove columns with zero variance (always present or always missing)
            valid_cols <- sapply(miss_idx, function(x) length(unique(x)) > 1)
            if (sum(valid_cols) < 2) return()
            
            miss_idx <- miss_idx[, valid_cols]
            cor_mat <- cor(miss_idx)
            
            table <- self$results$correlation_matrix
            for (v1 in colnames(miss_idx)) {
                for (v2 in colnames(miss_idx)) {
                    if (v1 == v2) next
                    table$addRow(rowKey = paste(v1, v2), values = list(
                        var1 = v1,
                        var2 = v2,
                        correlation = cor_mat[v1, v2]
                    ))
                }
            }
        },

        .plotPatterns = function(image, ...) {
            if (is.null(private$.data_subset)) return(FALSE)
            if (!requireNamespace("visdat", quietly = TRUE)) return(FALSE)
            
            p <- visdat::vis_miss(private$.data_subset) + 
                 ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
            
            print(p)
            TRUE
        },
        
        .plotUpSet = function(image, ...) {
            if (is.null(private$.data_subset)) return(FALSE)
            if (!requireNamespace("naniar", quietly = TRUE)) return(FALSE)
            
            # UpSet plot for missingness
            p <- naniar::gg_miss_upset(private$.data_subset)
            
            print(p)
            TRUE
        }
    )
)
