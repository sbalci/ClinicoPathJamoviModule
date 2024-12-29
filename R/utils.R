#' Perform pairwise chi-square tests with proper handling of contingency tables
#' @param data Data frame containing the variables
#' @param group_var Name of the grouping variable
#' @param response_var Name of the response variable
#' @param adjust_method Method for p-value adjustment (default: "bonferroni")
#' @return List containing summary and detailed results
# Enhanced pairwise chi-square test function with improved formatting
perform_pairwise_chisq <- function(data, group_var, response_var, adjust_method = "bonferroni") {
    # Get unique groups (excluding NA)
    groups <- sort(unique(data[[group_var]]))
    groups <- groups[!is.na(groups)]
    n_groups <- length(groups)

    # Initialize results storage
    results <- list()
    pvalues <- c()
    comparisons <- c()
    chi_squares <- c()
    dfs <- c()

    # Perform pairwise comparisons
    for (i in 1:(n_groups-1)) {
        for (j in (i+1):n_groups) {
            # Subset data for current pair
            pair_data <- subset(data, data[[group_var]] %in% c(groups[i], groups[j]))

            # Create contingency table
            cont_table <- table(pair_data[[group_var]], pair_data[[response_var]])

            # Perform chi-square test
            test_result <- suppressWarnings(chisq.test(cont_table, correct = TRUE))

            # Store results
            comparison_name <- paste(groups[i], "vs", groups[j])
            chi_squares <- c(chi_squares, round(test_result$statistic, 3))
            pvalues <- c(pvalues, test_result$p.value)
            dfs <- c(dfs, test_result$parameter)
            comparisons <- c(comparisons, comparison_name)
        }
    }

    # Adjust p-values
    adjusted_pvalues <- p.adjust(pvalues, method = adjust_method)

    # Create formatted table
    formatted_results <- data.frame(
        Comparison = comparisons,
        ChiSquare = chi_squares,
        df = dfs,
        p_value = round(pvalues, 3),
        adjusted_p_value = round(adjusted_pvalues, 3),
        stringsAsFactors = FALSE
    )

    # Add significance symbols
    formatted_results$significance <- ifelse(
        formatted_results$adjusted_p_value < 0.001, "***",
        ifelse(formatted_results$adjusted_p_value < 0.01, "**",
        ifelse(formatted_results$adjusted_p_value < 0.05, "*", "ns"))
    )

    # Format as HTML table
    html_table <- kableExtra::kable(formatted_results,
        format = "html",
        caption = "Pairwise Chi-square Tests",
        col.names = c("Comparison", "Chi-Square", "df", "p-value", "Adjusted p-value", "Significance"),
        align = c("l", "r", "r", "r", "r", "c")
    ) %>%
    kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        position = "left"
    ) %>%
    kableExtra::column_spec(2:5, width = "100px") %>%
    kableExtra::add_footnote(
        c("Significance codes: *** p<0.001, ** p<0.01, * p<0.05, ns p≥0.05",
          paste("P-values adjusted using", adjust_method, "method")),
        notation = "symbol"
    )

    return(list(
        table = html_table,
        raw_results = formatted_results
    ))
}

# Function to combine arsenal table with pairwise results
format_arsenal_output <- function(mytable, pairwise_results = NULL) {
    # Get arsenal table HTML
    main_table <- summary(mytable, text = "html")

    if (!is.null(pairwise_results)) {
        # Combine tables with proper spacing and styling
        html_output <- paste0(
            '<div class="arsenal-output">',
            main_table,
            '<br><div class="pairwise-tests">',
            pairwise_results$table,
            '</div></div>'
        )
    } else {
        html_output <- paste0(
            '<div class="arsenal-output">',
            main_table,
            '</div>'
        )
    }

    return(html_output)
}
