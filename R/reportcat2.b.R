#' @title Summary of Categorical Variables
#' @description Generates a comprehensive summary of categorical variables including 
#' frequency counts, percentages, missing value information, and optional visual 
#' summaries. Supports multiple output formats and sorting options for enhanced 
#' data exploration.
#' @return A results object containing HTML-formatted text summaries and visual tables
#' @examples 
#' \donttest{
#' # Example 1: Basic categorical summary
#' data <- data.frame(
#'   treatment = factor(c("A", "B", "A", "C", "B", "A")),
#'   grade = factor(c("High", "Low", "Medium", "High", "Low", "Medium"))
#' )
#' result <- reportcat(data = data, vars = c("treatment", "grade"))
#' 
#' # Example 2: Enhanced summary with cumulative percentages  
#' result_enhanced <- reportcat(
#'   data = data, 
#'   vars = "treatment",
#'   sumvar_style = TRUE,
#'   show_proportions = TRUE
#' )
#' 
#' # Example 3: Sort categories by frequency
#' result_sorted <- reportcat(
#'   data = data,
#'   vars = "grade", 
#'   sort_by_frequency = TRUE
#' )
#' }
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gtExtras gt_plt_summary
#'
# Improved version of reportcatClass with enhanced messages and formatting
reportcat2Class <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reportcat2Class",
    inherit = reportcat2Base,
    private = list(
        .run = function() {

            # Check if any variables have been selected.
            # Enhanced welcome message with HTML formatting for a more user-friendly experience.
            if (length(self$options$vars) == 0) {
                todo <- "
        <div style='font-family: Arial, sans-serif; color: #2c3e50;'>
          <h2>Welcome to ClinicoPath</h2>
          <p>This tool generates a summary of your selected categorical variables.</p>
          <p><strong>Instructions:</strong> Please select the <em>Variables</em> you wish to analyze.
          Only Nominal, Ordinal, or Categorical variables (factors) are allowed.</p>
          <hr>
        </div>"
                self$results$todo$setContent(todo)
                return()
            } else {
                # Clear the to-do message if variables are selected.
                self$results$todo$setContent("")
            }

            # Check if the provided data contains any rows.
            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            mydata <- self$data

            # Construct a formula from the selected variables.
            formula <- jmvcore::constructFormula(terms = self$options$vars)
            myvars <- jmvcore::decomposeFormula(formula = formula)
            myvars <- unlist(myvars)

            # Function to generate a summary for a single categorical variable.
            catsummary <- function(myvar) {
                # Validate variable exists
                if (!myvar %in% names(mydata)) {
                    return(paste0("<strong>Error:</strong> Variable '", myvar, "' not found in dataset."))
                }
                
                # Calculate total observations, missing values, and valid (non-missing) count.
                total_obs <- length(mydata[[myvar]])
                missing_obs <- sum(is.na(mydata[[myvar]]))
                valid_obs <- total_obs - missing_obs
                
                # Handle edge case: all values are missing
                if (valid_obs == 0) {
                    return(paste0("<strong>", myvar, "</strong>: All ", total_obs, " observations are missing."))
                }
                
                num_levels <- nlevels(as.factor(mydata[[myvar]]))
                
                # Handle edge case: only one level
                if (num_levels <= 1) {
                    unique_val <- unique(mydata[[myvar]][!is.na(mydata[[myvar]])])
                    return(paste0("<strong>", myvar, "</strong>: Only one category ('", unique_val, "') with ", valid_obs, " observations. Missing: ", missing_obs, "."))
                }

                # Create a summary table for the variable with safer filtering.
                summar <- tryCatch({
                    summary(as.factor(mydata[[myvar]])) %>%
                        as.table() %>%
                        tibble::as_tibble(.name_repair = "unique") %>%
                        dplyr::filter(!is.na(.[[1]]) & .[[1]] != "NA's")
                }, error = function(e) {
                    # Fallback for edge cases
                    table(mydata[[myvar]], useNA = "no") %>%
                        tibble::as_tibble(.name_repair = "unique")
                })
                
                # Apply sorting if requested
                if (self$options$sort_by_frequency) {
                    summar <- summar %>% dplyr::arrange(dplyr::desc(n))
                } else {
                    summar <- summar %>% dplyr::arrange(.[[1]])
                }
                
                summar$validtotal <- valid_obs

                # Enhanced sumvar-style output
                if (self$options$sumvar_style) {
                    # Create comprehensive frequency table similar to sumvar's tab1()
                    freq_table <- summar %>%
                        dplyr::mutate(
                            percent = round(n / validtotal * 100, 1),
                            cumulative = cumsum(n),
                            cum_percent = round(cumulative / validtotal * 100, 1),
                            table_row = paste0(
                                .[[1]], ": ", n, " (", percent, "%) ",
                                if(self$options$show_proportions) paste0("[Cumulative: ", cumulative, " (", cum_percent, "%)]") else ""
                            )
                        ) %>%
                        dplyr::pull(table_row)
                    
                    # Create comprehensive summary
                    header <- paste0("<strong>", myvar, "</strong> - Categorical Variable Summary<br>")
                    basic_info <- paste0(
                        "Total observations: ", total_obs, "<br>",
                        "Valid responses: ", valid_obs, " (", round(valid_obs/total_obs*100, 1), "%)<br>",
                        "Missing values: ", missing_obs, " (", round(missing_obs/total_obs*100, 1), "%)<br>",
                        "Number of categories: ", num_levels, "<br><br>"
                    )
                    freq_section <- paste0(
                        "<em>Frequency Distribution:</em><br>",
                        paste(freq_table, collapse = "<br>")
                    )
                    
                    full_description <- paste0(header, basic_info, freq_section)
                    
                } else {
                    # Original format (preserved for backward compatibility)
                    description <- summar %>%
                        dplyr::mutate(
                            percent = n / validtotal,
                            level_description = glue::glue(
                                "{...1}: n = {n}, {scales::percent(percent)} of valid cases. "
                            )
                        ) %>%
                        dplyr::pull(level_description)

                    # Create overall summary sentences with HTML tags for styling.
                    sentence1 <- paste0("<strong>", myvar, "</strong> has ", total_obs, " observations and ", num_levels, " levels.")
                    sentence2 <- paste0("Missing values: ", missing_obs, ".")
                    full_description <- paste(c(sentence1, description, sentence2), collapse = "<br>")
                }
                
                return(full_description)
            }

            # Generate summaries for all selected variables and combine them.
            summaries <- purrr::map(.x = myvars, .f = catsummary)
            summary_text <- paste(summaries, collapse = "<br><br>")
            self$results$text$setContent(summary_text)

            # CORRECT IMPLEMENTATION: Use gt properly for categorical data
            plot_dataset <- tryCatch({
                # gtExtras works better with numeric data, so for categorical we use basic gt
                cat_vars <- myvars[sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]
                
                if (length(cat_vars) > 0) {
                    clean_data <- mydata[cat_vars]
                    
                    # Convert character to factor for better handling
                    clean_data <- as.data.frame(lapply(clean_data, function(x) {
                        if (is.character(x)) as.factor(x) else x
                    }))
                    
                    # For categorical data, create summary stats first
                    summary_stats <- data.frame(
                        Variable = names(clean_data),
                        Type = sapply(clean_data, function(x) "categorical"),
                        Levels = sapply(clean_data, function(x) length(levels(x))),
                        N = sapply(clean_data, function(x) sum(!is.na(x))),
                        Missing = sapply(clean_data, function(x) sum(is.na(x)))
                    )
                    
                    # Create gt table with enhanced styling
                    gt_table <- summary_stats %>%
                        gt::gt() %>%
                        gt::tab_header(
                            title = "Enhanced Categorical Variables Summary"
                        ) %>%
                        gt::tab_style(
                            style = gt::cell_text(weight = "bold"),
                            locations = gt::cells_column_labels()
                        )
                    
                    # Convert to HTML
                    html_output <- as.character(gt::as_raw_html(gt_table))
                    return(htmltools::HTML(html_output))
                } else {
                    return(htmltools::HTML("<p>No categorical variables found.</p>"))
                }
            }, error = function(e) {
                # Fallback to simple table if gt fails
                return(private$.create_simple_cat_summary_table(mydata, myvars))
            })
            
            self$results$text1$setContent(plot_dataset)
        },

        # Simple categorical summary table without resource-intensive operations
        .create_simple_cat_summary_table = function(dataset, var_list) {
            # Filter to categorical/factor variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML("<p>No categorical variables available for summary table.</p>"))
            }
            
            # Create simple HTML table
            html <- "<table style='border-collapse: collapse; margin: 10px 0; width: 100%;'>"
            html <- paste0(html, "<tr style='background-color: #f8f9fa;'>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Variable</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Levels</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>N</th>")
            html <- paste0(html, "<th style='border: 1px solid #ccc; padding: 8px;'>Missing</th>")
            html <- paste0(html, "</tr>")
            
            for (var in cat_vars) {
                data_col <- dataset[[var]]
                
                # Convert to factor if character
                if (is.character(data_col)) {
                    data_col <- factor(data_col)
                }
                
                levels_count <- length(levels(data_col))
                n_valid <- sum(!is.na(data_col))
                n_missing <- sum(is.na(data_col))
                
                html <- paste0(html, "<tr>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; font-weight: bold;'>", var, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", levels_count, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_valid, "</td>")
                html <- paste0(html, "<td style='border: 1px solid #ccc; padding: 8px; text-align: center;'>", n_missing, "</td>")
                html <- paste0(html, "</tr>")
            }
            
            html <- paste0(html, "</table>")
            return(htmltools::HTML(html))
        }
    )
)


