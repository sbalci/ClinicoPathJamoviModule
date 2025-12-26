#' @title Enhanced Outcome Organizer for Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

outcomeorganizerClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "outcomeorganizerClass",
    inherit = outcomeorganizerBase,
    private = list(

        .notices = list(),

        # Notice management helpers ----
        .addNotice = function(type, message, name = NULL) {
            if (is.null(name)) {
                name <- paste0('notice', length(private$.notices) + 1)
            }
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = name,
                type = type
            )
            notice$setContent(message)
            priority <- switch(
                as.character(type),
                "1" = 1,  # ERROR
                "2" = 2,  # STRONG_WARNING
                "3" = 3,  # WARNING
                "4" = 4,  # INFO
                3         # Default to WARNING
            )
            private$.notices[[length(private$.notices) + 1]] <- list(
                notice = notice,
                priority = priority
            )
        },

        .insertNotices = function() {
            if (length(private$.notices) == 0) return()
            notices_sorted <- private$.notices[order(sapply(private$.notices, function(x) x$priority))]
            position <- 1
            for (n in notices_sorted) {
                self$results$insert(position, n$notice)
                position <- position + 1
            }
        },

        .resetNotices = function() {
            private$.notices <- list()
        },

        .init = function() {
            # Initialize table structures
            if (self$options$outputTable) {
                table <- self$results$outputTable
                if (is.null(table$rowKeys) || length(table$rowKeys) == 0) {
                    # Table will be populated in the .run() method
                }
            }

            # Initialize diagnostics table if enabled
            if (self$options$diagnostics) {
                table <- self$results$diagnosticsTable
                if (is.null(table$rowKeys) || length(table$rowKeys) == 0) {
                    # Will be populated with validation checks
                }
            }
        },

        # getData function to properly handle the data labels with robust mapping
        .getData = function() {
            # Get the data
            mydata <- self$data

            # Add row names if missing
            if (is.null(rownames(mydata))) {
                mydata$row_names <- seq_len(nrow(mydata))
            } else {
                mydata$row_names <- rownames(mydata)
            }

            # Get original names from the dataframe passed by jamovi
            original_names <- names(mydata)
            
            # Create a mapping of original names to cleaned names using INDICES
            # This is robust because janitor::clean_names preserves column order
            
            # Clean names safely
            mydata_cleaned <- try({
                janitor::clean_names(mydata)
            }, silent = TRUE)

            if (inherits(mydata_cleaned, "try-error")) {
                stop('Error cleaning variable names. Please check column names.')
            }

            # Map original variables to cleaned variables by INDEX
            # This avoids ambiguity if multiple original vars map to similar clean names
            # or if labels are not unique
            
            get_cleaned_var <- function(original_var_name) {
                if (is.null(original_var_name)) return(NULL)
                
                # Find index of original variable
                idx <- which(original_names == original_var_name)
                
                if (length(idx) == 0) return(NULL)
                
                # Return cleaned variable name at same index
                return(names(mydata_cleaned)[idx])
            }
            
            # Map the specific option variables
            outcome_var <- get_cleaned_var(self$options$outcome)
            recurrence_var <- get_cleaned_var(self$options$recurrence)
            id_var <- get_cleaned_var(self$options$patientID)

            # Apply labels using correct mapping
            # We use the original names as labels for the cleaned variables
            labels_list <- as.list(original_names)
            names(labels_list) <- names(mydata_cleaned)
            
            # Filter to only columns that exist (though they should all exist)
            valid_cols <- intersect(names(labels_list), names(mydata_cleaned))
            labels_list <- labels_list[valid_cols]
            
            mydata_labelled <- try({
                labelled::set_variable_labels(.data = mydata_cleaned, .labels = labels_list)
            }, silent = TRUE)

            if (inherits(mydata_labelled, "try-error")) {
                # Fallback if labelling fails
                mydata_labelled <- mydata_cleaned
            }

            return(list(
                "mydata_labelled" = mydata_labelled,
                "outcome_var" = outcome_var,
                "recurrence_var" = recurrence_var,
                "id_var" = id_var,
                "original_outcome" = self$options$outcome
            ))
        },

        # Enhanced input validation for outcome organization
        # Returns validation results with errors, warnings, and informational messages
        .validateInputs = function(mydata, outcome_var, recurrence_var = NULL, id_var = NULL, analysistype = "os", multievent = FALSE) {
            validation_results <- list(
                errors = character(0),
                warnings = character(0),
                info = character(0),
                should_stop = FALSE
            )
            
            # 1. Check if required variables exist in data
            if (!is.null(outcome_var) && length(outcome_var) > 0 && !outcome_var %in% names(mydata)) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Outcome variable '", outcome_var, "' not found in dataset (possibly lost during name cleaning).", sep=""))
                validation_results$should_stop <- TRUE
            }
            
            # 2. Check recurrence variable if specified
            if (!is.null(recurrence_var) && length(recurrence_var) > 0 && !recurrence_var %in% names(mydata)) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Recurrence variable '", recurrence_var, "' not found in dataset.", sep=""))
                validation_results$should_stop <- TRUE
            }
            
            # 3. Check patient ID variable if specified
            if (!is.null(id_var) && length(id_var) > 0 && !id_var %in% names(mydata)) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Patient ID variable '", id_var, "' not found in dataset.", sep=""))
                validation_results$should_stop <- TRUE
            }
            
            # Stop here if variables don't exist
            if (validation_results$should_stop) {
                return(validation_results)
            }
            
            # 4. Validate outcome variable
            if (!is.null(outcome_var) && length(outcome_var) > 0 && outcome_var %in% names(mydata)) {
                outcome_data <- mydata[[outcome_var]]
                outcome_data_clean <- outcome_data[!is.na(outcome_data)]
                
                # Check for ordered factors
                if (is.ordered(outcome_data)) {
                    validation_results$info <- c(validation_results$info,
                        "Outcome variable is an ordered factor. It will be treated as nominal (unordered) for analysis to avoid contrast issues.")
                }
                
                if (length(outcome_data_clean) == 0) {
                    validation_results$errors <- c(validation_results$errors,
                        "Outcome variable contains no non-missing values.")
                    validation_results$should_stop <- TRUE
                } else {
                    # Check unique values
                    unique_outcomes <- unique(outcome_data_clean)
                    outcome_count <- length(unique_outcomes)
                    
                    if (outcome_count < 2) {
                        validation_results$errors <- c(validation_results$errors,
                            "Outcome variable must have at least 2 different values.")
                        validation_results$should_stop <- TRUE
                    } else {
                        validation_results$info <- c(validation_results$info,
                            paste("Outcome variable has ", outcome_count, " unique values: ", 
                                  paste(head(unique_outcomes, 5), collapse=", "), 
                                  if(outcome_count > 5) "..." else "", sep=""))
                    }
                }
            }
            
            # 5. Validate analysis type compatibility
            valid_analysis_types <- c("os", "cause", "compete", "rfs", "pfs", "dfs", "ttp", "multistate")
            if (!analysistype %in% valid_analysis_types) {
                validation_results$errors <- c(validation_results$errors,
                    paste("Invalid analysis type '", analysistype, "'. Must be one of: ", 
                          paste(valid_analysis_types, collapse=", "), sep=""))
                validation_results$should_stop <- TRUE
            }
            
            # 6. Check analysis type requirements - STRICTER NOW
            if (analysistype %in% c("rfs", "pfs", "dfs", "ttp") && is.null(recurrence_var)) {
                # This remains a warning as technically one could abuse these modes without recurrence, 
                # but strongly advised against.
                validation_results$warnings <- c(validation_results$warnings,
                    paste("Analysis type '", analysistype, "' typically requires a recurrence/progression variable. You are analyzing death/event only.", sep=""))
            }

            if (analysistype == "multistate" && !multievent) {
                validation_results$errors <- c(validation_results$errors,
                    "Multistate models require multiple event types. Please enable 'Multiple Event Types' option.")
                validation_results$should_stop <- TRUE
            }
            
            if (analysistype == "compete" && !multievent) {
                validation_results$errors <- c(validation_results$errors,
                    "Competing risks analysis requires multiple event types. Please enable 'Multiple Event Types' option.")
                validation_results$should_stop <- TRUE
            }
            
            # 7. Combined data quality checks
            if (!validation_results$should_stop) {
                total_rows <- nrow(mydata)

                # Check for minimum sample size
                if (total_rows < 10) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Very small sample size: ", total_rows, " observations. Results may be unreliable.", sep=""))
                } else if (total_rows < 30) {
                    validation_results$warnings <- c(validation_results$warnings,
                        paste("Small sample size: ", total_rows, " observations. Consider larger sample for more reliable estimates.", sep=""))
                }

                # Check for missing data patterns
                if (!is.null(outcome_var) && length(outcome_var) > 0 && outcome_var %in% names(mydata)) {
                    missing_outcome <- sum(is.na(mydata[[outcome_var]]))
                    missing_proportion <- missing_outcome / total_rows

                    if (missing_proportion > 0.1) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste("Large amount of missing outcome data: ", round(missing_proportion * 100, 1),
                                  "% (", missing_outcome, " out of ", total_rows, " rows).", sep=""))
                    } else if (missing_proportion > 0) {
                        validation_results$info <- c(validation_results$info,
                            paste("Missing outcome data: ", round(missing_proportion * 100, 1),
                                  "% (", missing_outcome, " out of ", total_rows, " rows).", sep=""))
                    }
                    
                    # Check for rare events (separation issue warning)
                    if (length(unique(mydata[[outcome_var]])) == 2) {
                        tbl <- table(mydata[[outcome_var]])
                        min_cell <- min(tbl)
                        if (min_cell < 5) {
                            validation_results$warnings <- c(validation_results$warnings,
                                paste("Rare event detected (min cell count =", min_cell, "). Logistic regression may suffer from separation. Consider penalized methods."))
                        }
                    }
                }
            }

            # 8. Contextual warnings for potential misuse
            if (analysistype == "cause" && !multievent) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Cause-specific survival typically requires distinguishing between disease deaths and other deaths. Consider enabling 'Multiple Event Types' or switch to 'Overall Survival'.")
            }

            if (analysistype %in% c("rfs", "pfs", "dfs") && is.null(recurrence_var)) {
                validation_results$warnings <- c(validation_results$warnings,
                    paste0(toupper(analysistype), " analysis typically requires both a recurrence/progression variable AND an outcome variable. Currently only outcome is specified."))
            }
            
            return(validation_results)
        },

        # Main function to organize outcomes with enhanced functionality
        .organizeOutcomes = function() {
            # Get data and variables
            labelled_data <- private$.getData()
            mydata <- labelled_data$mydata_labelled
            outcome_var <- labelled_data$outcome_var
            recurrence_var <- labelled_data$recurrence_var
            id_var <- labelled_data$id_var

            # Check if required variables exist
            if (length(outcome_var) == 0 && !is.null(self$options$outcome)) {
                stop('Could not find outcome variable')
            }

            # Get parameters from UI options
            analysistype <- self$options$analysistype
            multievent <- self$options$multievent
            outcomeLevel <- self$options$outcomeLevel

            # Validation diagnostics - will be used if diagnostics are enabled
            diagnostics <- list()

            # Create a new outcome variable based on the analysis type
            if (!multievent) {
                # Check for simple binary outcome coding (0/1)
                outcome1 <- mydata[[outcome_var]]
                
                # Convert ordered factors to character/unordered factor to avoid issues
                if (is.ordered(outcome1)) {
                    outcome1 <- as.character(outcome1)
                }

                contin <- c("integer", "numeric", "double")

                if (inherits(outcome1, contin)) {
                    # FIX: Properly validate and recode binary outcomes
                    # Check unique values
                    unique_vals <- sort(unique(outcome1[!is.na(outcome1)]))

                    # Validate that we have exactly 2 values for binary analysis
                    if (length(unique_vals) < 2) {
                        stop('Outcome variable must have at least 2 different values. Found: ',
                             paste(unique_vals, collapse = ", "))
                    } else if (length(unique_vals) > 2) {
                        stop('Binary outcome variable must have exactly 2 unique values for non-multievent analysis. Found ',
                             length(unique_vals), ' values: ', paste(unique_vals, collapse = ", "),
                             '. Enable "Multiple Event Types" if you have more than 2 outcome categories.')
                    }

                    # Check if already properly coded as 0/1
                    if ((length(unique_vals) == 2) && all(unique_vals == c(0, 1))) {
                        # Perfect - already 0/1 coded
                        mydata[["myoutcome"]] <- mydata[[outcome_var]]
                        diagnostics$binary_check <- "Outcome already properly coded as 0/1"
                    } else {
                        # NOT 0/1 - need to recode based on outcomeLevel
                        # This prevents the critical bug where 1=alive, 2=dead gets passed through as-is
                        if (is.null(outcomeLevel)) {
                            stop('Outcome variable is not coded as 0/1 (found values: ',
                                 paste(unique_vals, collapse = ", "),
                                 '). Please select which value represents the event using the "Outcome Level" option.')
                        }

                        # Verify outcomeLevel exists in data
                        if (!outcomeLevel %in% unique_vals) {
                            stop('Selected outcome level "', outcomeLevel,
                                 '" not found in data. Available values: ',
                                 paste(unique_vals, collapse = ", "))
                        }

                        # Recode: outcomeLevel becomes 1, other value becomes 0
                        mydata[["myoutcome"]] <- ifelse(
                            test = outcome1 == outcomeLevel,
                            yes = 1,
                            no = 0
                        )

                        # Add diagnostic information about the recoding
                        other_val <- setdiff(unique_vals, outcomeLevel)
                        diagnostics$binary_check <- sprintf(
                            "Outcome recoded: '%s' (event) → 1, '%s' (non-event) → 0. Original coding was NOT 0/1.",
                            outcomeLevel, other_val
                        )
                    }

                } else if (inherits(outcome1, c("factor", "character"))) {
                    # Check if outcomeLevel is provided
                    if (is.null(outcomeLevel)) {
                        stop('Please select which value represents the event using the "Outcome Level" option.')
                    }
                    
                    # Convert to 1s and 0s based on the event level
                    mydata[["myoutcome"]] <- ifelse(
                        test = outcome1 == outcomeLevel,
                        yes = 1,
                        no = 0
                    )

                    # Add diagnostic information
                    diagnostics$event_levels <- paste("Event level:", outcomeLevel)
                    diagnostics$conversion <- "Factor converted to binary (0/1) coding"

                } else {
                    stop('Outcome variable must be numeric, factor, or character')
                }

                # Special handling for RFS/PFS/DFS if selected
                if (analysistype %in% c('rfs', 'pfs', 'dfs') && !is.null(recurrence_var)) {
                    # For these analyses, also consider recurrence/progression as events
                    recurrence_outcome <- mydata[[recurrence_var]]
                    recurrence_level <- self$options$recurrenceLevel

                    # Mark recurrences as events (1)
                    recurrence_events <- ifelse(
                        test = recurrence_outcome == recurrence_level,
                        yes = 1,
                        no = 0
                    )

                    # Combine with death events based on analysis type
                    if (analysistype == 'rfs') {
                        # Recurrence-free survival: event is recurrence or death from disease
                        mydata[["myoutcome"]] <- pmax(recurrence_events, mydata[["myoutcome"]], na.rm = TRUE)
                        diagnostics$rfs_handling <- "RFS: Events include recurrence and death"
                    } else if (analysistype == 'pfs') {
                        # Progression-free survival: event is progression or death from any cause
                        mydata[["myoutcome"]] <- pmax(recurrence_events, mydata[["myoutcome"]], na.rm = TRUE)
                        diagnostics$pfs_handling <- "PFS: Events include progression and death"
                    } else if (analysistype == 'dfs') {
                        # Disease-free survival: event is recurrence, second primary, or death from any cause
                        mydata[["myoutcome"]] <- pmax(recurrence_events, mydata[["myoutcome"]], na.rm = TRUE)
                        diagnostics$dfs_handling <- "DFS: Events include disease events and death"
                    }
                } else if (analysistype == 'ttp' && !is.null(recurrence_var)) {
                    # Time to progression: only progression counts as event, deaths are censored
                    recurrence_outcome <- mydata[[recurrence_var]]
                    recurrence_level <- self$options$recurrenceLevel

                    # Only progression counts as an event
                    mydata[["myoutcome"]] <- ifelse(
                        test = recurrence_outcome == recurrence_level,
                        yes = 1,
                        no = 0
                    )
                    diagnostics$ttp_handling <- "TTP: Only progression events counted, deaths censored"
                }

            } else {
                # Multiple event types
                outcome1 <- mydata[[outcome_var]]
                dod <- self$options$dod
                dooc <- self$options$dooc
                awd <- self$options$awd
                awod <- self$options$awod

                # Note: Validation of required level selections is now handled in .run()
                # with helpful notices instead of hard errors

                # Validate that selected levels actually exist in the data
                outcome_levels_in_data <- unique(outcome1[!is.na(outcome1)])
                selected_levels <- c(dod, dooc, awd, awod)
                missing_in_data <- selected_levels[!selected_levels %in% outcome_levels_in_data]

                if (length(missing_in_data) > 0) {
                    stop('Selected outcome levels not found in data: ',
                         paste(missing_in_data, collapse = ", "),
                         '. Available values in outcome variable: ',
                         paste(outcome_levels_in_data, collapse = ", "))
                }

                # Check for duplicate selections (same level selected for multiple categories)
                if (length(unique(selected_levels)) != 4) {
                    stop('Each outcome level must be unique. You have selected the same level for multiple categories. ',
                         'Selected levels: dod="', dod, '", dooc="', dooc, '", awd="', awd, '", awod="', awod, '"')
                }

                # Initialize all as NA
                mydata[["myoutcome"]] <- NA_integer_

                if (analysistype == 'os') {
                    # Overall survival: Dead (any cause) vs Alive
                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 1

                    diagnostics$overall_coding <- "Overall survival: All deaths coded as events (1)"

                } else if (analysistype == 'cause') {
                    # Cause-specific: Dead of disease vs Others
                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 0

                    diagnostics$cause_coding <- "Cause-specific: Only disease-related deaths as events (1)"

                } else if (analysistype == 'compete') {
                    # Competing risks: Multiple event types
                    mydata[["myoutcome"]][outcome1 == awd] <- 0
                    mydata[["myoutcome"]][outcome1 == awod] <- 0
                    mydata[["myoutcome"]][outcome1 == dod] <- 1
                    mydata[["myoutcome"]][outcome1 == dooc] <- 2

                    diagnostics$compete_coding <- "Competing risks: Disease deaths (1), other deaths (2)"

                } else if (analysistype == 'multistate') {
                    # Multistate model: Different states given different codes
                    mydata[["myoutcome"]][outcome1 == awod] <- 0  # Baseline state
                    mydata[["myoutcome"]][outcome1 == awd] <- 1   # Disease state
                    mydata[["myoutcome"]][outcome1 == dod] <- 2   # Death from disease
                    mydata[["myoutcome"]][outcome1 == dooc] <- 3  # Death from other causes

                    diagnostics$multistate_coding <- "Multistate: Healthy (0), Disease (1), Death-disease (2), Death-other (3)"
                }

                # FIX: Verify that recoding actually worked (not all NAs)
                # This catches cases where selected levels don't match any data values
                n_recoded <- sum(!is.na(mydata[["myoutcome"]]))
                if (n_recoded == 0) {
                    stop('Outcome recoding failed: all values are NA. This usually means the selected outcome levels ',
                         '("', dod, '", "', dooc, '", "', awd, '", "', awod, '") do not match the actual values in your data. ',
                         'Available values in outcome variable: ',
                         paste(unique(outcome1[!is.na(outcome1)]), collapse = ", "),
                         '. Please verify your level selections are correct.')
                } else if (n_recoded < length(outcome1) * 0.5) {
                    # Warn if more than 50% are NA (likely wrong level selection)
                    warning('More than 50% of outcomes are NA after recoding (',
                            round((1 - n_recoded/length(outcome1)) * 100, 1),
                            '%). This suggests your selected levels may not fully match your data. ',
                            'Check that all four level selections are correct.')
                    diagnostics$recoding_warning <- sprintf("Only %d/%d (%.1f%%) outcomes successfully recoded",
                                                             n_recoded, length(outcome1),
                                                             n_recoded/length(outcome1) * 100)
                }

                # Apply event hierarchy if specified
                if (self$options$useHierarchy) {
                    # If multiple events could be coded for the same patient, apply hierarchy
                    highest_priority <- self$options$eventPriority
                    if (!is.null(id_var) && !is.null(highest_priority)) {
                        # Validate that there are actually duplicate IDs
                        duplicate_ids <- mydata %>%
                            dplyr::group_by(!!dplyr::sym(id_var)) %>%
                            dplyr::filter(dplyr::n() > 1) %>%
                            dplyr::ungroup()

                        if (nrow(duplicate_ids) == 0) {
                            diagnostics$hierarchy <- "Event hierarchy requested but no duplicate patient IDs found. Each patient has only one record."
                        } else {
                            # Apply hierarchy
                            mydata <- mydata %>%
                                dplyr::group_by(!!dplyr::sym(id_var)) %>%
                                dplyr::mutate(
                                    myoutcome = ifelse(
                                        any(myoutcome == highest_priority, na.rm = TRUE),
                                        highest_priority,
                                        myoutcome
                                    )
                                ) %>%
                                dplyr::ungroup()

                            n_affected <- duplicate_ids %>%
                                dplyr::distinct(!!dplyr::sym(id_var)) %>%
                                nrow()

                            diagnostics$hierarchy <- sprintf(
                                "Event hierarchy applied (priority: %s) to %d patients with multiple records.",
                                highest_priority, n_affected
                            )
                        }
                    }
                }
            }

            # Cache labelled data lookups for efficiency (used multiple times)
            all_labels_cache <- labelled::var_label(mydata)

            # Apply interval censoring if specified
            if (self$options$intervalCensoring && !is.null(self$options$intervalStart) && !is.null(self$options$intervalEnd)) {
                # Get interval variables from cached labels
                start_var <- names(all_labels_cache)[all_labels_cache == self$options$intervalStart]
                end_var <- names(all_labels_cache)[all_labels_cache == self$options$intervalEnd]

                if (length(start_var) > 0 && length(end_var) > 0) {
                    # Add interval variables to output for use with survival::Surv()
                    mydata[["interval_L"]] <- mydata[[start_var[1]]]
                    mydata[["interval_R"]] <- mydata[[end_var[1]]]
                    diagnostics$interval_censoring <- "Interval censoring enabled. Use Surv(interval_L, interval_R, myoutcome, type='interval2') in survival analysis."
                } else {
                    diagnostics$interval_censoring <- "Interval censoring: variables not found in dataset"
                }
            }

            # Handle administrative censoring if specified
            if (self$options$adminCensoring && !is.null(self$options$adminDate)) {
                # Get admin date variable using cached labels
                admin_date_var_name <- self$options$adminDate

                # Find the admin date variable in cached labels
                admin_date_var <- NULL
                if (!is.null(admin_date_var_name)) {
                    admin_date_var <- names(all_labels_cache)[all_labels_cache == admin_date_var_name]
                }

                if (length(admin_date_var) > 0) {
                    # Store administrative censoring date for use in survival analysis
                    mydata[["admin_censor_date"]] <- mydata[[admin_date_var[1]]]
                    diagnostics$admin_censoring <- "Administrative censoring date stored. Apply in survival analysis: time_censored = pmin(time, admin_censor_date)"
                } else {
                    diagnostics$admin_censoring <- "Administrative censoring: date variable not found in dataset"
                }
            }

            # Create a data frame with row names and recoded outcome
            df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

            return(list(
                "df_outcome" = df_outcome,
                "mydata" = mydata,
                "diagnostics" = diagnostics
            ))
        },

        .getOutcomeLabel = function(value, analysistype, multievent) {
            val_str <- as.character(value)

            if (multievent && analysistype == 'multistate') {
                switch(val_str,
                       "0" = "Disease-free (0)",
                       "1" = "Disease state (1)",
                       "2" = "Death from disease (2)",
                       "3" = "Death from other causes (3)",
                       paste0("Unknown (", value, ")"))
            } else if (multievent && analysistype == 'compete') {
                switch(val_str,
                       "0" = "Censored (0)",
                       "1" = "Disease event (1)",
                       "2" = "Competing event (2)",
                       paste0("Unknown (", value, ")"))
            } else {
                switch(val_str,
                       "0" = "Censored (0)",
                       "1" = "Event (1)",
                       paste0("Unknown (", value, ")"))
            }
        },

        .showGlossary = function() {
            glossary_html <- "
            <div style='background-color: #f9f9f9; padding: 15px; border-radius: 8px;'>
            <h4>Survival Analysis Glossary</h4>
            <dl>
                <dt><b>Overall Survival (OS)</b></dt>
                <dd>Time from diagnosis/treatment to death from any cause. Patients alive at last follow-up are censored.</dd>

                <dt><b>Cause-Specific Survival</b></dt>
                <dd>Time to death from the disease of interest. Deaths from other causes are censored (treated as non-events).</dd>

                <dt><b>Competing Risks</b></dt>
                <dd>Analysis accounting for multiple types of events (e.g., disease death vs. other death). Competing events prevent the event of interest from occurring.</dd>

                <dt><b>Recurrence-Free Survival (RFS)</b></dt>
                <dd>Time to disease recurrence or death from disease. Used for cancers after curative treatment.</dd>

                <dt><b>Progression-Free Survival (PFS)</b></dt>
                <dd>Time to disease progression or death from any cause. Common endpoint in oncology trials.</dd>

                <dt><b>Disease-Free Survival (DFS)</b></dt>
                <dd>Time to recurrence, second primary cancer, or death from any cause.</dd>

                <dt><b>Time to Progression (TTP)</b></dt>
                <dd>Time to disease progression only. Deaths without progression are censored.</dd>

                <dt><b>Multistate Model</b></dt>
                <dd>Models transitions between health states (e.g., healthy → disease → death).</dd>

                <dt><b>Censoring</b></dt>
                <dd>Incomplete observation of survival time (patient still alive, lost to follow-up, or event not observed).</dd>

                <dt><b>Event Hierarchy</b></dt>
                <dd>When multiple events occur for the same patient, prioritize one event type over others.</dd>
            </dl>
            </div>
            "

            self$results$glossary$setContent(glossary_html)
        },

        .todo = function() {
            todo <- glue::glue(
                "
                <br>Welcome to Enhanced Outcome Organizer
                <br><br>
                This tool helps you prepare outcome variables for various types of survival analysis:
                <br>
                <ul>
                <li><b>Overall Survival (OS):</b> All deaths are events</li>
                <li><b>Cause-Specific Survival:</b> Only disease-related deaths are events</li>
                <li><b>Competing Risks:</b> Different event types have different codes</li>
                <li><b>Recurrence/Progression-Free Survival (RFS/PFS):</b> Events include disease recurrence and death</li>
                <li><b>Disease-Free Survival (DFS):</b> Events include any disease-related event or death</li>
                <li><b>Time to Progression (TTP):</b> Only disease progression events counted</li>
                <li><b>Multistate Models:</b> Multiple outcome states coded separately</li>
                </ul>
                <br>
                Advanced options allow for:
                <br>
                <ul>
                <li>Event hierarchies when multiple events occur</li>
                <li>Time-dependent outcomes</li>
                <li>Interval censoring</li>
                <li>Administrative censoring</li>
                </ul>
                <br>
                Select your outcome variables and analysis type to begin.
                "
            )

            html <- self$results$todo
            html$setContent(todo)
        },

        .run = function() {
            # Reset notices at start of each run
            private$.resetNotices()

            # Initial validation
            if (is.null(self$options$outcome)) {
                private$.todo()
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Create table if needed
            private$.checkpoint()

            # Perform input validation
            labelled_data <- private$.getData()
            mydata <- labelled_data$mydata_labelled
            outcome_var <- labelled_data$outcome_var
            recurrence_var <- labelled_data$recurrence_var
            id_var <- labelled_data$id_var

            validation_results <- private$.validateInputs(
                mydata, outcome_var, recurrence_var, id_var, self$options$analysistype, self$options$multievent
            )

            # Check multievent level selections if multievent is enabled
            if (self$options$multievent) {
                outcome1 <- mydata[[outcome_var]]
                unique_outcomes <- unique(outcome1[!is.na(outcome1)])

                # Check which level selections are missing
                missing_levels <- character(0)
                if (is.null(self$options$dod)) missing_levels <- c(missing_levels, "Dead of Disease")
                if (is.null(self$options$dooc)) missing_levels <- c(missing_levels, "Dead of Other Causes")
                if (is.null(self$options$awd)) missing_levels <- c(missing_levels, "Alive with Disease")
                if (is.null(self$options$awod)) missing_levels <- c(missing_levels, "Alive without Disease")

                if (length(missing_levels) > 0) {
                    # Add informative notice about available levels
                    private$.addNotice(jmvcore::NoticeType$INFO,
                        paste0("Outcome variable has ", length(unique_outcomes), " unique values: ",
                               paste(unique_outcomes, collapse = ", ")))

                    # Add strong warning about missing selections
                    private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                        paste0("Multiple Event Types analysis requires all four outcome level selections. ",
                               "Missing: ", paste(missing_levels, collapse = ", "), ". ",
                               "Please select the appropriate level from your outcome values for each category."))

                    # Add guidance notice
                    private$.addNotice(jmvcore::NoticeType$INFO,
                        "Guide: Use the dropdown menus to map your outcome values to the four standard categories: Dead of Disease, Dead of Other Causes, Alive with Disease, and Alive without Disease.")

                    private$.insertNotices()
                    return()
                }
            }

            # Handle validation errors - add as notices
            if (validation_results$should_stop) {
                for (error_msg in validation_results$errors) {
                    private$.addNotice(jmvcore::NoticeType$ERROR, error_msg)
                }
                private$.insertNotices()
                return()
            }

            # Add validation warnings
            if (length(validation_results$warnings) > 0) {
                for (warn_msg in validation_results$warnings) {
                    private$.addNotice(jmvcore::NoticeType$WARNING, warn_msg)
                }
            }

            # Add validation info
            if (length(validation_results$info) > 0) {
                for (info_msg in validation_results$info) {
                    private$.addNotice(jmvcore::NoticeType$INFO, info_msg)
                }
            }

            # Insert notices before main processing
            private$.insertNotices()

            # Organize outcomes
            results <- private$.organizeOutcomes()
            df_outcome <- results$df_outcome
            mydata <- results$mydata
            diagnostics <- results$diagnostics

            # Create summary text describing the recoding
            analysistype <- self$options$analysistype

            # Generate appropriate summary text based on analysis type
            summary_text <- ""

            if (self$options$multievent) {
                if (analysistype == 'os') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Overall Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({self$options$dod}): coded as 1<br>
                        - Dead of other causes ({self$options$dooc}): coded as 1<br>
                        - Alive with disease ({self$options$awd}): coded as 0<br>
                        - Alive without disease ({self$options$awod}): coded as 0<br>
                        <br>
                        <i>This coding compares all deaths vs. alive status for standard Kaplan-Meier or Cox regression.</i>
                        "
                    )
                } else if (analysistype == 'cause') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Cause-Specific Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({self$options$dod}): coded as 1<br>
                        - Dead of other causes ({self$options$dooc}): coded as 0<br>
                        - Alive with disease ({self$options$awd}): coded as 0<br>
                        - Alive without disease ({self$options$awod}): coded as 0<br>
                        <br>
                        <i>This coding compares disease-specific deaths vs. other outcomes for cause-specific analyses.</i>
                        "
                    )
                } else if (analysistype == 'compete') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Competing Risks Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({self$options$dod}): coded as 1<br>
                        - Dead of other causes ({self$options$dooc}): coded as 2<br>
                        - Alive with disease ({self$options$awd}): coded as 0<br>
                        - Alive without disease ({self$options$awod}): coded as 0<br>
                        <br>
                        <i>This coding enables competing risk analysis between disease-specific deaths and other causes using cmprsk or other packages.</i>
                        "
                    )
                } else if (analysistype == 'multistate') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Multistate Model Analysis</b><br>
                        Recoded outcome:<br>
                        - Alive without disease ({self$options$awod}): coded as 0<br>
                        - Alive with disease ({self$options$awd}): coded as 1<br>
                        - Dead of disease ({self$options$dod}): coded as 2<br>
                        - Dead of other causes ({self$options$dooc}): coded as 3<br>
                        <br>
                        <i>This coding allows for multistate modeling with transitions between health states.</i>
                        "
                    )
                }
            } else {
                if (analysistype == 'os') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Overall Survival (OS) Analysis</b><br>
                        Recoded outcome:<br>
                        - Death ({self$options$outcomeLevel}): coded as 1<br>
                        - Alive (other levels): coded as 0<br>
                        <br>
                        <i>This is standard coding for overall survival using Cox regression or Kaplan-Meier analysis.</i>
                        "
                    )
                } else if (analysistype %in% c('rfs', 'pfs', 'dfs') && !is.null(self$options$recurrence)) {
                    summary_text <- glue::glue(
                        "
                        <br><b>{toupper(analysistype)} Analysis</b><br>
                        Recoded outcome:<br>
                        - Death ({self$options$outcomeLevel}): coded as 1<br>
                        - Recurrence/Progression ({self$options$recurrenceLevel}): coded as 1<br>
                        - Event-free (other): coded as 0<br>
                        <br>
                        <i>This coding treats both disease events and death as events for {toupper(analysistype)} analysis.</i>
                        "
                    )
                } else if (analysistype == 'ttp' && !is.null(self$options$recurrence)) {
                    summary_text <- glue::glue(
                        "
                        <br><b>Time to Progression (TTP) Analysis</b><br>
                        Recoded outcome:<br>
                        - Progression ({self$options$recurrenceLevel}): coded as 1<br>
                        - No progression (including deaths): coded as 0<br>
                        <br>
                        <i>This coding only treats disease progression as events; deaths without progression are censored.</i>
                        "
                    )
                } else {
                    summary_text <- glue::glue(
                        "
                        <br><b>Binary Outcome Coding</b><br>
                        Recoded outcome:<br>
                        - Event ({self$options$outcomeLevel}): coded as 1<br>
                        - Non-event (other levels): coded as 0<br>
                        <br>
                        <i>This is standard coding for Cox regression and Kaplan-Meier analysis.</i>
                        "
                    )
                }
            }

            # Add information about special handling if applicable
            if (self$options$useHierarchy) {
                summary_text <- paste(summary_text, glue::glue(
                    "<br><b>Event Hierarchy Applied:</b> If multiple events occur, priority is given to type {self$options$eventPriority}.<br>"
                ))
            }

            if (self$options$intervalCensoring) {
                summary_text <- paste(summary_text, "<br><b>Interval Censoring:</b> Events are known to occur within time intervals rather than at exact times.<br>")
            }

            if (self$options$adminCensoring) {
                summary_text <- paste(summary_text, "<br><b>Administrative Censoring:</b> Observations are censored at a specified administrative date.<br>")
            }

            # Add recommendations for appropriate analyses
            summary_text <- paste(summary_text, "<br><b>Recommended Analysis Approaches:</b><br>")

            if (analysistype == 'os') {
                summary_text <- paste(summary_text, "- Kaplan-Meier method for univariate analysis<br>- Cox proportional hazards for multivariable analysis<br>")
            } else if (analysistype == 'cause') {
                summary_text <- paste(summary_text, "- Cause-specific hazard models (standard Cox regression)<br>- Cumulative incidence function with competing risks<br>")
            } else if (analysistype == 'compete') {
                summary_text <- paste(summary_text, "- Fine-Gray subdistribution hazard model<br>- Cumulative incidence function accounting for competing risks<br>")
            } else if (analysistype == 'multistate') {
                summary_text <- paste(summary_text, "- Multi-state models (e.g., illness-death model)<br>- Transition probabilities between states<br>")
            } else if (analysistype %in% c('rfs', 'pfs', 'dfs')) {
                summary_text <- paste(summary_text, "- Standard survival analysis (Kaplan-Meier, Cox)<br>- Consider competing risks if appropriate<br>")
            } else if (analysistype == 'ttp') {
                summary_text <- paste(summary_text, "- Standard survival analysis with death as censoring<br>- Consider sensitivity analysis treating death as competing risk<br>")
            }

            # Summary now only contains analysis description (validation moved to Notices)
            self$results$summary$setContent(summary_text)

            # Add data table if requested
            if (self$options$outputTable) {
                # Create frequency table of new outcomes
                outcome_counts <- table(mydata$myoutcome)
                outcome_table <- self$results$outputTable

                # Add rows for each unique outcome value
                for (i in seq_along(outcome_counts)) {
                    value <- names(outcome_counts)[i]
                    count <- outcome_counts[i]
                    label <- private$.getOutcomeLabel(value, analysistype, self$options$multievent)

                    outcome_table$addRow(rowKey=i, values=list(
                        outcome = value,
                        label = label,
                        count = count,
                        percentage = round(count / sum(outcome_counts) * 100, 1)
                    ))
                }
            }

            # Add diagnostics table if requested
            if (self$options$diagnostics && length(diagnostics) > 0) {
                diagnostics_table <- self$results$diagnosticsTable

                # Add each diagnostic as a row
                i <- 1
                for (key in names(diagnostics)) {
                    diagnostics_table$addRow(rowKey=i, values=list(
                        check = key,
                        result = diagnostics[[key]]
                    ))
                    i <- i + 1
                }
            }

            # Add visualization if requested
            if (self$options$visualization) {
                # Store outcome distribution data for the visualization
                image <- self$results$outcomeViz
                image$setState(list(
                    "table" = outcome_counts,
                    "analysis_type" = analysistype,
                    "multi_event" = self$options$multievent
                ))
            }

            # Add recoded outcome to data if requested
            if (self$options$addOutcome) {
                self$results$addOutcome$setRowNums(df_outcome$row_names)
                self$results$addOutcome$setValues(df_outcome$myoutcome)
            }

            # Natural language summary for reports (if requested)
            if (self$options$showNaturalSummary) {
                # Analysis type labels
                analysis_type_labels <- list(
                    os = "Overall Survival",
                    cause = "Cause-Specific Survival",
                    compete = "Competing Risks",
                    rfs = "Recurrence-Free Survival",
                    pfs = "Progression-Free Survival",
                    dfs = "Disease-Free Survival",
                    ttp = "Time to Progression",
                    multistate = "Multistate Model"
                )

                # Event description based on analysis type
                event_desc <- if (analysistype == "os") "death from any cause"
                    else if (analysistype == "cause") "death from the disease of interest"
                    else if (analysistype == "compete") "disease-specific death (competing events coded as 2)"
                    else if (analysistype %in% c("rfs", "pfs", "dfs")) "recurrence/progression or death"
                    else if (analysistype == "ttp") "disease progression only"
                    else "the selected event type"

                # Censor description
                censor_desc <- if (analysistype == "ttp") "patients who died without progression or remain event-free"
                    else "patients who remain alive or event-free"

                # Calculate frequencies
                n_events <- sum(mydata$myoutcome == 1, na.rm = TRUE)
                n_censored <- sum(mydata$myoutcome == 0, na.rm = TRUE)
                total_n <- n_events + n_censored
                event_pct <- if (total_n > 0) round(n_events / total_n * 100, 1) else 0
                censor_pct <- if (total_n > 0) round(n_censored / total_n * 100, 1) else 0

                natural_summary <- sprintf(
                    "<div style='background-color: #e7f3ff; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                    <b>📋 Copy-Ready Report Text:</b><br><br>
                    The outcome variable '<b>%s</b>' was recoded for <b>%s</b> analysis.
                    Events (coded as 1) represent %s.
                    Non-events (coded as 0) represent %s.
                    The recoded variable '<b>myoutcome</b>' contains <b>%d events (%.1f%%)</b> and <b>%d non-events (%.1f%%)</b>.
                    </div>",
                    self$options$outcome,
                    analysis_type_labels[[analysistype]],
                    event_desc,
                    censor_desc,
                    n_events, event_pct,
                    n_censored, censor_pct
                )

                self$results$naturalSummary$setContent(natural_summary)
            }

            # Show glossary if requested
            if (self$options$showGlossary) {
                private$.showGlossary()
            }

            # Add completion notice
            private$.addNotice(jmvcore::NoticeType$INFO,
                "Outcome recoding completed successfully. New variable 'myoutcome' is ready for survival analysis.")
        },

        # Plot function for outcome distribution visualization
        .plotOutcome = function(image, ggtheme, theme, ...) {
            if (!self$options$visualization)
                return()

            plotData <- image$state

            if (is.null(plotData))
                return()

            tryCatch({
                # Create data frame from outcome counts
                plot_df <- data.frame(
                    Outcome = names(plotData$table),
                    Count = as.numeric(plotData$table)
                )

                # Add proper labels based on analysis type using private method
                plot_df$Label <- sapply(plot_df$Outcome, function(val) {
                    private$.getOutcomeLabel(val, plotData$analysis_type, plotData$multi_event)
                })

                # Color-blind safe palette
                cb_palette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#56B4E9")

                # Create the plot
                plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Label, y = Count, fill = Label)) +
                    ggplot2::geom_bar(stat = "identity") +
                    ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5, size = 5) +
                    ggplot2::labs(
                        title = "Distribution of Recoded Outcome Values",
                        x = "Outcome Category",
                        y = "Count"
                    ) +
                    ggtheme +
                    ggplot2::scale_fill_manual(values = cb_palette) +
                    ggplot2::theme(
                        legend.position = "none",
                        axis.text = ggplot2::element_text(size = 12),
                        axis.title = ggplot2::element_text(size = 13, face = "bold"),
                        plot.title = ggplot2::element_text(size = 14, face = "bold")
                    )

                print(plot)
                TRUE
            }, error = function(e) {
                # Log error but don't crash the analysis
                warning("Failed to render outcome visualization: ", e$message)
                FALSE
            })
        }
    )
)
