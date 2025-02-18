#' #' Perform cell residuals analysis
#' #' @param cont_table Contingency table
#' #' @param method Method for p-value adjustment
#' #' @return List with residuals and p-values
#' perform_residuals_analysis <- function(cont_table, method = "bonferroni") {
#'   results <- chisq.posthoc.test::chisq.posthoc.test(
#'     x = cont_table,
#'     method = method,
#'     round = 3
#'   )
#'   return(results)
#' }
#'
#' #' Perform pairwise chi-square tests
#' #' @param data Data frame
#' #' @param group_var Group variable name
#' #' @param response_var Response variable name
#' #' @param method Method for p-value adjustment
#' #' @return List of pairwise comparison results
#' perform_pairwise_tests <- function(data, group_var, response_var, method = "bonferroni") {
#'   group_levels <- unique(data[[group_var]])
#'   n_groups <- length(group_levels)
#'   results <- list()
#'
#'   if(n_groups > 2) {
#'     # Perform pairwise comparisons
#'     for(i in 1:(n_groups-1)) {
#'       for(j in (i+1):n_groups) {
#'         subset_data <- data[data[[group_var]] %in% c(group_levels[i], group_levels[j]),]
#'         subtable <- table(subset_data[[group_var]], subset_data[[response_var]])
#'
#'         test_result <- suppressWarnings(chisq.test(subtable))
#'
#'         comparison_name <- paste(group_levels[i], "vs", group_levels[j])
#'         results[[comparison_name]] <- list(
#'           comparison = comparison_name,
#'           chi_square = test_result$statistic,
#'           df = test_result$parameter,
#'           p_value = test_result$p.value
#'         )
#'       }
#'     }
#'
#'     # Adjust p-values
#'     p_values <- sapply(results, function(x) x$p_value)
#'     adjusted_p_values <- p.adjust(p_values, method = method)
#'
#'     # Add adjusted p-values
#'     for(i in seq_along(results)) {
#'       results[[i]]$adjusted_p_value <- adjusted_p_values[i]
#'     }
#'   }
#'
#'   return(results)
#' }
#'
#' #' Format post-hoc results as HTML
#' #' @param residuals_results Results from residuals analysis
#' #' @param pairwise_results Results from pairwise tests
#' #' @return HTML string
#' format_posthoc_html <- function(residuals_results = NULL, pairwise_results = NULL) {
#'   html_parts <- list()
#'
#'   if(!is.null(residuals_results)) {
#'     # Format residuals table
#'     res_table <- kableExtra::kable(
#'       residuals_results,
#'       format = "html",
#'       caption = "Cell Residuals Analysis",
#'       digits = 3
#'     ) %>%
#'       kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
#'
#'     html_parts$residuals <- res_table
#'   }
#'
#'   if(!is.null(pairwise_results) && length(pairwise_results) > 0) {
#'     # Create data frame from pairwise results
#'     pair_df <- do.call(rbind, lapply(pairwise_results, function(x) {
#'       data.frame(
#'         Comparison = x$comparison,
#'         ChiSquare = round(x$chi_square, 3),
#'         df = x$df,
#'         p_value = round(x$p_value, 3),
#'         adjusted_p = round(x$adjusted_p_value, 3)
#'       )
#'     }))
#'
#'     # Format pairwise table
#'     pair_table <- kableExtra::kable(
#'       pair_df,
#'       format = "html",
#'       caption = "Pairwise Comparisons",
#'       col.names = c("Comparison", "Chi-Square", "df", "p-value", "Adjusted p-value")
#'     ) %>%
#'       kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
#'
#'     html_parts$pairwise <- pair_table
#'   }
#'
#'   # Combine results based on what's available
#'   if(length(html_parts) == 2) {
#'     # Both analyses
#'     html_output <- paste0(
#'       '<div class="posthoc-analyses">',
#'       '<div class="residuals-section">',
#'       html_parts$residuals,
#'       '</div><br><br>',
#'       '<div class="pairwise-section">',
#'       html_parts$pairwise,
#'       '</div>',
#'       '</div>'
#'     )
#'   } else {
#'     # Single analysis
#'     html_output <- paste0(
#'       '<div class="posthoc-analyses">',
#'       html_parts[[1]],
#'       '</div>'
#'     )
#'   }
#'
#'   return(html_output)
#' }
