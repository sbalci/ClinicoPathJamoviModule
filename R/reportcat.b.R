#' @title Summary of Categorical Variables
#' @return Text
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gtExtras gt_plt_summary
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
                # Calculate total observations, missing values, and valid (non-missing) count.
                total_obs <- length(mydata[[myvar]])
                missing_obs <- sum(is.na(mydata[[myvar]]))
                valid_obs <- total_obs - missing_obs
                num_levels <- nlevels(as.factor(mydata[[myvar]]))

                # Create a summary table for the variable.
                summar <- summary(as.factor(mydata[[myvar]])) %>%
                    as.table() %>%
                    tibble::as_tibble(.name_repair = "unique") %>%
                    dplyr::filter(.[[1]] != "NA's")
                
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

            # Generate a visually appealing summary plot using gtExtras.
            plot_obj <- mydata %>%
                gtExtras::gt_plt_summary() %>%
                gt::cols_hide(columns = c("Mean", "Median", "SD"))


            # Convert the gt object to HTML for display.
            plot_html <- htmltools::HTML(print(plot_obj)[["children"]][[2]])
            self$results$text1$setContent(plot_html)
        }
    )
)



# med <- self$options$med
# cent <- self$options$cent
# disp <- self$options$disp
# ran <- self$options$ran
# distr <- self$options$distr
# lev <- self$options$lev
# n_ch <- self$options$n_ch
# mis <- self$options$mis
#
#

# myreport <- mydata %>%
#     select(myvars) %>%
#     report::report(.,
#                    median = FALSE,
#                    centrality = TRUE,
#                    dispersion = TRUE,
#                    range = TRUE,
#                    distribution = FALSE,
#                    levels_percentage = FALSE,
#                    n_entries = 3,
#                    missing_percentage = FALSE
# #                    median = med,
# #                    centrality = cent,
# #                    dispersion = disp,
# #                    range = ran,
# #                    distribution = distr,
# #                    levels_percentage = lev,
# #                    n_characters = n_ch,
# #                    missing_percentage = mis
#                    )
#
# results1 <- myreport



# results1 <- mydata %>%
#     explore::describe(.) %>%
#     dplyr::filter(na > 0)


# for (fac in facs)
#     data[[fac]] <- as.factor(data[[fac]])

