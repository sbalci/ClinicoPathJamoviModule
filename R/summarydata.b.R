#' @title Summary of Continuous Variables
#' @return Text and an HTML summary table
#'
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom magrittr %>%
#' @importFrom gt gt
#' @importFrom htmltools HTML
#' @importFrom gtExtras gt_plt_summary


summarydataClass <- if (requireNamespace("jmvcore")) R6::R6Class("summarydataClass",
    inherit = summarydataBase, private = list(.run = function() {


        # Check if variables have been selected. If not, display a welcoming message with instructions.
        if (length(self$options$vars) == 0) {
            intro_msg <- "
          <h3>Welcome to ClinicoPath Descriptives!</h3>
          <p>This tool helps you generate descriptive statistics for your numeric variables.</p>
          <p>Please select one or more continuous variables from the options panel to get started.
          Remember to cite the used packages and jamovi in your reports.</p>
        "
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

                print(paste0("Mean of <strong>", myvar, "</strong> is: ", mean_x, " \U00B1 ", sd_x,
                  ". (Median: ", median_x, " [Min: ", min_x, " - ", "Max: ",
                  max_x, "]) <br>", collapse = " "))
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
