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

            # Generate a visually appealing summary plot using gtExtras with error handling.
            tryCatch({
                if (requireNamespace("gtExtras", quietly = TRUE) && requireNamespace("gt", quietly = TRUE)) {
                    plot_obj <- mydata %>%
                        gtExtras::gt_plt_summary() %>%
                        gt::cols_hide(columns = c("Mean", "Median", "SD"))
                    
                    # Safely convert the gt object to HTML for display.
                    plot_html_raw <- as.character(gt::as_raw_html(plot_obj))
                    plot_html <- htmltools::HTML(plot_html_raw)
                    self$results$text1$setContent(plot_html)
                } else {
                    self$results$text1$setContent(htmltools::HTML(
                        "<p><em>Visual summary table requires 'gtExtras' and 'gt' packages to be installed.</em></p>"
                    ))
                }
            }, error = function(e) {
                self$results$text1$setContent(htmltools::HTML(
                    paste0("<p><em>Could not generate visual summary: ", e$message, "</em></p>")
                ))
            })
        }
    )
)


