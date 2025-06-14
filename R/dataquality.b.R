#' @title Data Quality Assessment
#' @return HTML summary of data quality issues including duplicates and missing values
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom dplyr n_distinct
#' @importFrom htmltools HTML

dataqualityClass <- if (requireNamespace("jmvcore")) R6::R6Class("dataqualityClass",
    inherit = dataqualityBase, private = list(.run = function() {

        # Check if variables have been selected. If not, display a welcoming message.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to Data Quality Assessment!</h3>
          <p>This tool helps you identify data quality issues in your dataset.</p>
          <p>Select variables to check, or leave empty to analyze the entire dataset.</p>
          <p>Enable options for duplicate detection and missing value analysis.</p>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            self$results$todo$setContent("")
        }

        # Validate that the dataset contains complete rows.
        if (nrow(self$data) == 0) {
            stop("Error: The provided dataset contains no rows. Please check your data and try again.")
        }

        dataset <- self$data
        
        # Determine variables to analyze
        if (length(self$options$vars) > 0) {
            var_formula <- jmvcore::constructFormula(terms = self$options$vars)
            var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))
            analysis_data <- dataset[var_list]
        } else {
            var_list <- names(dataset)
            analysis_data <- dataset
        }

        quality_results <- list()

        # Missing value analysis
        if (self$options$check_missing) {
            missing_summary <- sapply(analysis_data, function(x) {
                total <- length(x)
                missing <- sum(is.na(x))
                missing_pct <- round(missing / total * 100, 1)
                
                paste0("Missing: ", missing, "/", total, " (", missing_pct, "%)")
            })
            
            quality_results$missing <- paste0(
                "<h4>Missing Value Analysis</h4>",
                paste(names(missing_summary), missing_summary, sep = ": ", collapse = "<br>")
            )
        }

        # Duplicate analysis
        if (self$options$check_duplicates) {
            if (self$options$complete_cases_only && length(var_list) > 1) {
                # Check for duplicate rows across all selected variables
                total_rows <- nrow(analysis_data)
                unique_rows <- nrow(unique(analysis_data))
                duplicate_rows <- total_rows - unique_rows
                duplicate_pct <- round(duplicate_rows / total_rows * 100, 1)
                
                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Row Analysis</h4>",
                    "Total rows: ", total_rows, "<br>",
                    "Unique rows: ", unique_rows, "<br>",
                    "Duplicate rows: ", duplicate_rows, " (", duplicate_pct, "%)"
                )
            } else {
                # Check for duplicates within each variable
                dup_summary <- sapply(analysis_data, function(x) {
                    total <- length(x)
                    unique_vals <- length(unique(na.omit(x)))
                    duplicates <- total - unique_vals - sum(is.na(x))
                    dup_pct <- round(duplicates / total * 100, 1)
                    
                    paste0("Unique: ", unique_vals, ", Duplicates: ", duplicates, " (", dup_pct, "%)")
                })
                
                quality_results$duplicates <- paste0(
                    "<h4>Duplicate Value Analysis</h4>",
                    paste(names(dup_summary), dup_summary, sep = ": ", collapse = "<br>")
                )
            }
        }

        # Complete cases analysis
        if (length(var_list) > 1) {
            complete_cases <- sum(complete.cases(analysis_data))
            total_cases <- nrow(analysis_data)
            complete_pct <- round(complete_cases / total_cases * 100, 1)
            
            quality_results$completeness <- paste0(
                "<h4>Data Completeness</h4>",
                "Complete cases: ", complete_cases, "/", total_cases, " (", complete_pct, "%)"
            )
        }

        # Combine all results
        final_results <- paste(unlist(quality_results), collapse = "<br><br>")
        self$results$text$setContent(final_results)

        # Simple visualization using gt summary (similar to existing modules)
        if (self$options$check_duplicates || self$options$check_missing) {
            plot_dataset <- analysis_data %>%
                gtExtras::gt_plt_summary()
            plot_html <- htmltools::HTML(print(plot_dataset)[["children"]][[2]])
            self$results$plot$setContent(plot_html)
        }

    }))