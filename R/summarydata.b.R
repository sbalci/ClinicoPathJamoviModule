#' @title Summary of Continuous Variables with Distribution Diagnostics
#' @return Text and an HTML summary table (with optional distribution diagnostics)
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt
#' @importFrom htmltools HTML
#' @importFrom gtExtras gt_plt_summary
#' @import moments

summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(.run = function() {


        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric variables.
          Please select one or more continuous variables from the options panel.</p>
          <p>If you want to inspect distribution characteristics, enable the 'Distribution Diagnostics' option.</p>"
            self$results$todo$setContent(intro_msg)
            return()
        } else {
            # Clear any introductory message if variables are selected.
            self$results$todo$setContent("")

            # Validate that the dataset contains complete rows.
            if (nrow(self$data) == 0) {
                stop("Error: The provided dataset contains no complete rows. Please check your data and try again.")
            }

            # Retrieve the data and construct the list of variables.
            dataset <- self$data
            var_formula <- jmvcore::constructFormula(terms = self$options$vars)
            var_list <- unlist(jmvcore::decomposeFormula(formula = var_formula))

            # mysummary function
            mysummary <- function(myvar) {

                mean_x <- round(mean(jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                sd_x <- round(sd(x = jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                median_x <- round(median(jmvcore::toNumeric(dataset[[myvar]]),
                  na.rm = TRUE), digits = 1)

                min_x <- round(min(jmvcore::toNumeric(dataset[[myvar]]), na.rm = TRUE),
                  digits = 1)

                max_x <- round(max(jmvcore::toNumeric(dataset[[myvar]]), na.rm = TRUE),
                  digits = 1)



                dist_text <- ""

                # If the distribution diagnostics option is enabled, add additional tests.
                if (self$options$distr) {
                    # Shapiro-Wilk test (only valid if 3 <= sample size <= 5000)

                    numeric_data <- jmvcore::toNumeric(dataset[[myvar]])

                    valid_data <- na.omit(numeric_data)
                    if (length(valid_data) >= 3 && length(valid_data) <= 5000) {
                        sw_test <- shapiro.test(valid_data)
                        p_val <- round(sw_test$p.value, 3)
                    } else {
                        p_val <- NA
                    }

                    # Calculate skewness and kurtosis using the moments package.
                    skew_val <- round(moments::skewness(numeric_data, na.rm = TRUE), 2)
                    kurt_val <- round(moments::kurtosis(numeric_data, na.rm = TRUE), 2)

                    # Interpret normality based on the Shapiro-Wilk p-value.
                    norm_status <- if (!is.na(p_val)) {
                        if (p_val > 0.05) "appears to be normally distributed" else "does not appear to be normally distributed. Please use relevant visualisation and tests to verify the characteristics of distribution."
                    } else {
                        "Normality test not applicable due to sample size"
                    }

                    dist_text <- paste0(
                        "<br><em>Distribution Diagnostics for ", myvar ,":</em> Shapiro-Wilk p-value = ", p_val,
                        "; Skewness = ", skew_val, "; Kurtosis = ", kurt_val,
                        " (Data ", norm_status, ")."
                    )
                }

                    # Append the distribution diagnostics to the summary text.
                    print(paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " \U00B1 ", sd_x,
                                 ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                                 max_x, "]) <br>", dist_text, "<br><br>", collapse = " "))

            }

            results <- purrr::map(.x = var_list, .f = mysummary)
            results <- unlist(results)
            self$results$text$setContent(results)


            plot_dataset <- dataset %>%
                gtExtras::gt_plt_summary()
            print_plot_dataset <- print(plot_dataset)
            plot_dataset <- htmltools::HTML(print_plot_dataset[["children"]][[2]])
            self$results$text1$setContent(plot_dataset)

        }


    }))
