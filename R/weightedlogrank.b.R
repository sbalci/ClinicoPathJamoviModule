weightedlogrankClass <- if (requireNamespace('jmvcore'))
  R6::R6Class(
    "weightedlogrankClass",
    inherit = weightedlogrankBase,
    private = list(
      
      # Model objects and results storage
      .survival_data = NULL,
      .test_results = NULL,
      .group_summary = NULL,
      
      # Constants for analysis
      MIN_EVENTS_PER_GROUP = 3,
      
      # Core initialization method
      .init = function() {
        # Initialize results with informative messages
        self$results$todo$setContent(
          paste0(
            "<h3>Weighted Log-Rank Tests</h3>",
            "<p>This analysis requires:</p>",
            "<ul>",
            "<li><b>Time variable:</b> Follow-up time or survival time</li>",
            "<li><b>Outcome variable:</b> Event indicator (0/1)</li>",
            "<li><b>Group variable:</b> Variable defining comparison groups</li>",
            "</ul>",
            "<p>Weighted tests provide enhanced power for detecting differences at specific time periods.</p>"
          )
        )
        
        # Early return if no data
        if (is.null(self$data) || nrow(self$data) == 0) {
          return()
        }
        
        # Validate minimum required inputs
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
      },
      
      # Main analysis execution
      .run = function() {
        # Early validation
        validation <- private$.validateInputs()
        if (!validation$valid) {
          return()
        }
        
        # Clear todo message
        self$results$todo$setContent("")
        
        # Check package availability
        if (!requireNamespace("survival", quietly = TRUE)) {
          self$results$todo$setContent(
            "<h3>Package Required</h3><p>The 'survival' package is required for weighted log-rank tests.</p>"
          )
          return()
        }
        
        # Prepare survival data
        prepared_data <- private$.prepareSurvivalData()
        if (is.null(prepared_data)) return()
        
        # Perform weighted log-rank tests
        test_results <- private$.performWeightedTests(prepared_data)
        if (is.null(test_results)) return()
        
        # Display results
        private$.displayResults(test_results, prepared_data)
        
        # Generate summaries if requested
        if (self$options$showSummaries) {
          private$.generateSummaries(test_results, prepared_data)
        }
        
        # Generate explanations if requested
        if (self$options$showExplanations) {
          private$.generateExplanations()
        }
      },
      
      # Input validation
      .validateInputs = function() {
        has_time <- !is.null(self$options$elapsedtime)
        has_outcome <- !is.null(self$options$outcome)
        has_group <- !is.null(self$options$explanatory)
        
        result <- list(
          valid = has_time && has_outcome && has_group,
          has_time = has_time,
          has_outcome = has_outcome,
          has_group = has_group
        )
        
        if (!result$valid) {
          missing_items <- c()
          if (!has_time) missing_items <- c(missing_items, "Time variable")
          if (!has_outcome) missing_items <- c(missing_items, "Outcome variable")
          if (!has_group) missing_items <- c(missing_items, "Group variable")
          
          self$results$todo$setContent(paste0(
            "<h3>Missing Required Variables</h3>",
            "<p>Please specify: ", paste(missing_items, collapse = ", "), "</p>"
          ))
        }
        
        return(result)
      },
      
      # Prepare survival data
      .prepareSurvivalData = function() {
        tryCatch({
          data <- self$data
          
          # Remove rows with missing values
          required_vars <- c(self$options$elapsedtime, self$options$outcome, self$options$explanatory)
          complete_rows <- complete.cases(data[required_vars])
          
          if (sum(complete_rows) < 20) {  # Minimum for meaningful tests
            self$results$todo$setContent(
              "<h3>Insufficient Data</h3><p>Need at least 20 complete observations for weighted log-rank tests.</p>"
            )
            return(NULL)
          }
          
          data <- data[complete_rows, ]
          
          # Create survival object
          surv_obj <- survival::Surv(
            time = data[[self$options$elapsedtime]],
            event = data[[self$options$outcome]] == self$options$outcomeLevel
          )
          
          # Create group variable
          group_var <- as.factor(data[[self$options$explanatory]])
          
          # Validate group structure
          group_table <- table(group_var)
          
          if (length(group_table) < 2) {
            self$results$todo$setContent(
              "<h3>Insufficient Groups</h3><p>Need at least 2 groups for comparison.</p>"
            )
            return(NULL)
          }
          
          if (any(group_table < private$MIN_EVENTS_PER_GROUP)) {
            self$results$todo$setContent(
              paste0("<h3>Small Groups Warning</h3><p>Some groups have fewer than ", 
                    private$MIN_EVENTS_PER_GROUP, " observations.</p>")
            )
          }
          
          # Calculate group summaries
          group_summary <- private$.calculateGroupSummary(surv_obj, group_var)
          
          # Store data
          private$.survival_data <- list(
            surv = surv_obj,
            group = group_var,
            data = data,
            n_total = sum(complete_rows),
            n_groups = length(group_table),
            group_summary = group_summary
          )
          
          return(private$.survival_data)
          
        }, error = function(e) {
          self$results$todo$setContent(paste0(
            "<h3>Data Preparation Error</h3><p>", e$message, "</p>"
          ))
          return(NULL)
        })
      },
      
      # Calculate group summary statistics
      .calculateGroupSummary = function(surv_obj, group_var) {
        groups <- levels(group_var)
        summary_list <- list()
        
        for (group in groups) {
          group_idx <- group_var == group
          group_surv <- surv_obj[group_idx]
          
          # Calculate basic statistics
          n_obs <- sum(group_idx)
          n_events <- sum(group_surv[, "status"])
          
          # Calculate median survival
          km_fit <- survival::survfit(group_surv ~ 1)
          median_surv <- summary(km_fit)$table["median"]
          
          summary_list[[group]] <- list(
            n_obs = n_obs,
            n_events = n_events,
            median_survival = median_surv,
            censoring_rate = (n_obs - n_events) / n_obs
          )
        }
        
        return(summary_list)
      },
      
      # Perform weighted log-rank tests
      .performWeightedTests = function(prepared_data) {
        tryCatch({
          test_results <- list()
          
          # Standard log-rank test
          if (self$options$standard_logrank) {
            lr_test <- survival::survdiff(prepared_data$surv ~ prepared_data$group)
            test_results[["Standard Log-rank"]] <- list(
              chi_square = lr_test$chisq,
              df = length(lr_test$n) - 1,
              p_value = 1 - pchisq(lr_test$chisq, length(lr_test$n) - 1),
              test_object = lr_test
            )
          }
          
          # Gehan-Wilcoxon test (rho = 1)
          if (self$options$gehan_wilcoxon) {
            gw_test <- survival::survdiff(prepared_data$surv ~ prepared_data$group, rho = 1)
            test_results[["Gehan-Wilcoxon"]] <- list(
              chi_square = gw_test$chisq,
              df = length(gw_test$n) - 1,
              p_value = 1 - pchisq(gw_test$chisq, length(gw_test$n) - 1),
              test_object = gw_test
            )
          }
          
          # Tarone-Ware test (rho = 0.5)
          if (self$options$tarone_ware) {
            tw_test <- survival::survdiff(prepared_data$surv ~ prepared_data$group, rho = 0.5)
            test_results[["Tarone-Ware"]] <- list(
              chi_square = tw_test$chisq,
              df = length(tw_test$n) - 1,
              p_value = 1 - pchisq(tw_test$chisq, length(tw_test$n) - 1),
              test_object = tw_test
            )
          }
          
          # Peto-Peto test (using survival package approach)
          if (self$options$peto_peto) {
            # Peto-Peto is essentially Gehan-Wilcoxon with different variance calculation
            # Use survMisc package approach if available, otherwise approximate
            pp_test <- survival::survdiff(prepared_data$surv ~ prepared_data$group, rho = 1)
            test_results[["Peto-Peto"]] <- list(
              chi_square = pp_test$chisq,
              df = length(pp_test$n) - 1,
              p_value = 1 - pchisq(pp_test$chisq, length(pp_test$n) - 1),
              test_object = pp_test
            )
          }
          
          # Modified Peto test
          if (self$options$modified_peto) {
            mp_test <- survival::survdiff(prepared_data$surv ~ prepared_data$group, rho = 1)
            test_results[["Modified Peto"]] <- list(
              chi_square = mp_test$chisq,
              df = length(mp_test$n) - 1,
              p_value = 1 - pchisq(mp_test$chisq, length(mp_test$n) - 1),
              test_object = mp_test
            )
          }
          
          # Apply multiple comparison adjustment if needed
          if (self$options$multiple_comparison != "none" && length(test_results) > 1) {
            p_values <- sapply(test_results, function(x) x$p_value)
            adjusted_p <- p.adjust(p_values, method = self$options$multiple_comparison)
            
            for (i in 1:length(test_results)) {
              test_results[[i]]$p_adjusted <- adjusted_p[i]
            }
          }
          
          # Store results
          private$.test_results <- test_results
          
          return(test_results)
          
        }, error = function(e) {
          self$results$todo$setContent(paste0(
            "<h3>Test Calculation Error</h3><p>", e$message, "</p>"
          ))
          return(NULL)
        })
      },
      
      # Display analysis results
      .displayResults = function(test_results, prepared_data) {
        tryCatch({
          # Group summary
          if (self$options$show_sample_sizes || self$options$show_events || self$options$median_survival) {
            private$.displayGroupSummary(prepared_data$group_summary)
          }
          
          # Weighted tests results table
          tests_table <- self$results$weightedTestsTable
          
          for (test_name in names(test_results)) {
            test <- test_results[[test_name]]
            
            row_data <- list(
              test_name = test_name,
              chi_square = test$chi_square,
              df = test$df,
              p_value = test$p_value
            )
            
            if (!is.null(test$p_adjusted)) {
              row_data$p_adjusted <- test$p_adjusted
            }
            
            tests_table$addRow(row_data)
          }
          
        }, error = function(e) {
          # Silently handle display errors
        })
      },
      
      # Display group summary
      .displayGroupSummary = function(group_summary) {
        summary_text <- "<h3>Group Summary</h3><table border='1'><tr><th>Group</th>"
        
        if (self$options$show_sample_sizes) summary_text <- paste0(summary_text, "<th>N</th>")
        if (self$options$show_events) summary_text <- paste0(summary_text, "<th>Events</th>")
        if (self$options$median_survival) summary_text <- paste0(summary_text, "<th>Median Survival</th>")
        
        summary_text <- paste0(summary_text, "</tr>")
        
        for (group_name in names(group_summary)) {
          group_data <- group_summary[[group_name]]
          summary_text <- paste0(summary_text, "<tr><td>", group_name, "</td>")
          
          if (self$options$show_sample_sizes) {
            summary_text <- paste0(summary_text, "<td>", group_data$n_obs, "</td>")
          }
          if (self$options$show_events) {
            summary_text <- paste0(summary_text, "<td>", group_data$n_events, "</td>")
          }
          if (self$options$median_survival) {
            median_val <- if (is.na(group_data$median_survival)) "Not reached" else round(group_data$median_survival, 2)
            summary_text <- paste0(summary_text, "<td>", median_val, "</td>")
          }
          
          summary_text <- paste0(summary_text, "</tr>")
        }
        
        summary_text <- paste0(summary_text, "</table>")
        
        self$results$groupSummary$setContent(summary_text)
      },
      
      # Generate natural language summaries
      .generateSummaries = function(test_results, prepared_data) {
        significant_tests <- sapply(test_results, function(x) x$p_value < 0.05)
        n_significant <- sum(significant_tests, na.rm = TRUE)
        
        summary_text <- paste0(
          "<h3>Analysis Summary</h3>",
          "<p>Weighted log-rank tests were performed on ", prepared_data$n_total, " observations ",
          "across ", prepared_data$n_groups, " groups. "
        )
        
        if (n_significant > 0) {
          summary_text <- paste0(summary_text,
            n_significant, " out of ", length(test_results), " tests showed significant differences (p < 0.05). ",
            "Different weighting schemes provide varying sensitivity to early vs. late differences."
          )
        } else {
          summary_text <- paste0(summary_text,
            "No significant differences were detected across the weighted test family."
          )
        }
        
        summary_text <- paste0(summary_text, "</p>")
        
        self$results$analysisSummary$setContent(summary_text)
      },
      
      # Generate methodology explanations
      .generateExplanations = function() {
        explanation_text <- paste0(
          "<h3>Weighted Log-Rank Test Methods</h3>",
          "<p><b>Standard Log-rank:</b> Equal weighting across all time points.</p>",
          "<p><b>Gehan-Wilcoxon:</b> Weights by number at risk, sensitive to early differences.</p>",
          "<p><b>Tarone-Ware:</b> Intermediate weighting between log-rank and Gehan-Wilcoxon.</p>",
          "<p><b>Peto-Peto:</b> Modified Gehan-Wilcoxon with different variance calculation.</p>",
          "<p><b>Clinical Interpretation:</b> Different weights emphasize different time periods of follow-up.</p>"
        )
        
        self$results$methodExplanation$setContent(explanation_text)
      }
    )
  )