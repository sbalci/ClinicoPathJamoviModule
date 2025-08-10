#' @title Summary of Categorical Variables
#' @return Text
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gtExtras gt_plt_summary
#' @importFrom utils packageVersion
#'
# Improved version of reportcatClass with enhanced messages and formatting
reportcatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "reportcatClass",
    inherit = reportcatBase,
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
                
                # Check if the provided data contains any rows.
                if (nrow(self$data) == 0)
                    stop('Data contains no (complete) rows')

                mydata <- self$data

                # Construct a formula from the selected variables.
                formula <- jmvcore::constructFormula(terms = self$options$vars)
                myvars <- jmvcore::decomposeFormula(formula = formula)
                myvars <- unlist(myvars)
                
                # Validate that selected variables are actually categorical
                non_categorical <- myvars[!sapply(mydata[myvars], function(x) is.factor(x) || is.character(x))]
                if (length(non_categorical) > 0) {
                    stop(paste("Non-categorical variables detected:", paste(non_categorical, collapse = ", "), 
                              ". Please select only categorical (factor or character) variables."))
                }

                # Function to generate a summary for a single categorical variable.
                catsummary <- function(myvar) {
                    # Calculate total observations, missing values, and valid (non-missing) count.
                    total_obs <- length(mydata[[myvar]])
                    missing_obs <- sum(is.na(mydata[[myvar]]))
                    valid_obs <- total_obs - missing_obs
                    num_levels <- nlevels(as.factor(mydata[[myvar]]))

                    # Create a summary table for the variable.
                    tbl <- summary(as.factor(mydata[[myvar]]))
                    summar <- data.frame(
                        level = names(tbl),
                        n = as.numeric(tbl),
                        stringsAsFactors = FALSE
                    ) %>%
                        dplyr::filter(level != "NA's") %>%
                        dplyr::arrange(dplyr::desc(n))
                    summar$validtotal <- valid_obs

                    # Build a description for each level showing count and percentage.
                    description <- summar %>%
                        dplyr::mutate(
                            percent = n / validtotal,
                            level_description = glue::glue(
                                "{level}: n = {n}, {scales::percent(percent)} of valid cases. "
                            )
                        ) %>%
                        dplyr::pull(level_description)

                    # Create overall summary sentences with HTML tags for styling.
                    sentence1 <- paste0("<strong>", myvar, "</strong> has ", total_obs, " observations and ", num_levels, " levels.")
                    sentence2 <- paste0("Missing values: ", missing_obs, ".")
                    full_description <- paste(c(sentence1, description, sentence2), collapse = "<br>")
                    return(full_description)
                }

                # Generate summaries for all selected variables and combine them.
                summaries <- purrr::map(.x = myvars, .f = catsummary)
                summary_text <- paste(summaries, collapse = "<br><br>")
                self$results$text$setContent(summary_text)

                # Version-compatible gtExtras implementation with fallbacks for categorical data
                plot_dataset <- tryCatch({
                    # Primary approach: Use gtExtras with version compatibility for categorical data
                    private$.safe_gt_plt_summary_cat(mydata, myvars)
                }, error = function(e) {
                    # Secondary approach: Custom gt table with gtExtras-like styling for categorical data
                    tryCatch({
                        private$.gtExtras_style_fallback_cat(mydata, myvars)
                    }, error = function(e2) {
                        # Final fallback: Simple HTML message
                        htmltools::HTML(paste0(
                            "<div style='padding: 20px; border-left: 4px solid #dc3545; background-color: #f8d7da; margin: 10px 0;'>",
                            "<h5 style='color: #721c24; margin-top: 0;'>Summary Table Error</h5>",
                            "<p><strong>gtExtras error:</strong> ", e$message, "</p>",
                            "<p><strong>Fallback error:</strong> ", e2$message, "</p>",
                            "<p>Please check your data types and ensure variables are categorical.</p>",
                            "</div>"
                        ))
                    })
                })
                
                self$results$text1$setContent(plot_dataset)
            }
        },

        # Version-compatible gtExtras wrapper for categorical data
        .safe_gt_plt_summary_cat = function(dataset, var_list) {
            # Filter to categorical/factor variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML("<p>No categorical variables selected for gtExtras summary.</p>"))
            }
            
            # Prepare clean dataset with only categorical variables
            clean_data <- dataset[cat_vars]
            
            # Ensure proper data types (convert character to factor)
            clean_data <- as.data.frame(lapply(clean_data, function(x) {
                if (is.character(x)) as.factor(x) else x
            }))
            
            # Check gt version and handle missing value formatting
            gt_version <- utils::packageVersion("gt")
            
            # Try gtExtras with version compatibility
            summary_table <- tryCatch({
                if (gt_version >= "0.6.0") {
                    # For newer gt versions, gtExtras should handle sub_missing internally
                    clean_data %>% gtExtras::gt_plt_summary()
                } else {
                    # For older gt versions, use fmt_missing approach
                    clean_data %>% gtExtras::gt_plt_summary()
                }
            }, error = function(e) {
                # If gtExtras fails, create basic summary without problematic columns
                clean_data %>% gtExtras::gt_plt_summary()
            })
            
            # Safely hide numeric columns that might not be relevant for categorical data
            summary_table <- tryCatch({
                # Get all column names in the table
                table_cols <- names(summary_table$`_data`)
                # Define numeric columns that should be hidden for categorical data
                numeric_cols_to_hide <- c("Mean", "Median", "SD", "Min", "Max", "Q25", "Q75")
                # Only hide columns that actually exist in the table
                existing_cols_to_hide <- intersect(numeric_cols_to_hide, table_cols)
                
                if (length(existing_cols_to_hide) > 0) {
                    summary_table %>% gt::cols_hide(columns = existing_cols_to_hide)
                } else {
                    summary_table
                }
            }, error = function(e) {
                # If hiding columns fails for any reason, return table as-is
                summary_table
            })
            
            # Convert to HTML
            print_table <- print(summary_table)
            return(htmltools::HTML(print_table[["children"]][[2]]))
        },

        # Fallback with gtExtras-style appearance for categorical data
        .gtExtras_style_fallback_cat = function(dataset, var_list) {
            # Get categorical variables only
            cat_vars <- var_list[sapply(dataset[var_list], function(x) is.factor(x) || is.character(x))]
            
            if (length(cat_vars) == 0) {
                return(htmltools::HTML("<p>No categorical variables available for summary table.</p>"))
            }
            
            # Calculate comprehensive summary statistics for categorical data
            summary_stats <- data.frame(
                Variable = cat_vars,
                Type = rep("categorical", length(cat_vars)),
                N = sapply(dataset[cat_vars], function(x) sum(!is.na(x))),
                Missing = sapply(dataset[cat_vars], function(x) sum(is.na(x))),
                Levels = sapply(dataset[cat_vars], function(x) {
                    if (is.factor(x)) nlevels(x) else length(unique(x[!is.na(x)]))
                }),
                Most_Common = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) > 0) names(tbl)[which.max(tbl)] else "NA"
                }),
                Most_Common_N = sapply(dataset[cat_vars], function(x) {
                    tbl <- table(x, useNA = "no")
                    if (length(tbl) > 0) max(tbl) else 0
                }),
                stringsAsFactors = FALSE
            )
            
            # Create gtExtras-style table for categorical data
            gt_table <- summary_stats %>%
                gt::gt() %>%
                gt::tab_header(
                    title = gt::md("**Categorical Variables Summary**"),
                    subtitle = gt::md("*Comprehensive statistics for categorical variables*")
                ) %>%
                gt::cols_label(
                    Variable = "Variable",
                    Type = "Type",
                    N = "N",
                    Missing = "Missing",
                    Levels = "Levels",
                    Most_Common = "Most Common",
                    Most_Common_N = "Count"
                ) %>%
                gt::tab_style(
                    style = gt::cell_fill(color = "#f8f9fa"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::tab_style(
                    style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels()
                ) %>%
                gt::opt_stylize(style = 6, color = "blue") %>%
                gt::tab_options(
                    table.font.size = 12,
                    heading.title.font.size = 16,
                    heading.subtitle.font.size = 12
                )
            
            # Convert to HTML
            print_table <- print(gt_table)
            return(htmltools::HTML(print_table[["children"]][[2]]))
        }
    )
)



