#' @title Enhanced Outcome Organizer for Survival Analysis
#' @importFrom R6 R6Class
#'
#' @return An \code{R6} class generator object for the \code{outcomeorganizerClass} backend; used internally by the jamovi analysis wrapper and not called directly.

outcomeorganizerClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "outcomeorganizerClass",
    inherit = outcomeorganizerBase,
    private = list(

        # Result of .defineEventIndicator(), kept so .run() can render the
        # recode disclosure without redoing the work.
        .eventRecode = NULL,
        # Censored/Event/Competing factor for the competing-risk output, so the
        # coding survives the hand-off to survival / multisurvival instead of
        # being silently binarised back to cause-specific.
        .causeFactor = NULL,

        # Notice management helpers ----
        # Notices render to dedicated Html outputs (errors / strongWarnings /
        # warnings / infoMessages) to avoid the protobuf serialization error
        # caused by jmvcore::Notice objects passed to self$results$insert().
        .addNotice = function(type, message, name = NULL) {
            # jmvcore::NoticeType constants: ERROR=0, STRONG_WARNING=1, WARNING=2, INFO=3
            type_str <- switch(
                as.character(type),
                "0" = "error",
                "1" = "strongWarning",
                "2" = "warning",
                "3" = "info",
                "warning"
            )
            title <- switch(
                type_str,
                "error" = "Error",
                "strongWarning" = "Strong warning",
                "warning" = "Warning",
                "info" = "Information",
                "Notice"
            )
            private$.addHtmlMessage(type_str, title, message)
        },

        .addHtmlMessage = function(type, title, message) {
            output_name <- switch(type,
                "error" = "errors",
                "strongWarning" = "strongWarnings",
                "warning" = "warnings",
                "info" = "infoMessages",
                "warnings"
            )
            border_color <- switch(type,
                "error" = "#d9534f",
                "strongWarning" = "#e67e22",
                "warning" = "#f0ad4e",
                "info" = "#5bc0de",
                "#f0ad4e"
            )
            current_content <- self$results[[output_name]]$content
            if (is.null(current_content)) current_content <- ""
            new_message <- sprintf(
                '<div style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: #f8f9fa;"><strong>%s:</strong> %s</div>',
                border_color,
                htmltools::htmlEscape(title),
                htmltools::htmlEscape(message)
            )
            self$results[[output_name]]$setContent(paste0(current_content, new_message))
            self$results[[output_name]]$setVisible(TRUE)
        },

        # Legacy no-ops kept so existing call sites don't break
        .insertNotices = function() invisible(NULL),

        .resetNotices = function() {
            for (out in c("errors", "strongWarnings", "warnings", "infoMessages")) {
                if (!is.null(self$results[[out]])) {
                    self$results[[out]]$setContent("")
                    self$results[[out]]$setVisible(FALSE)
                }
            }
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
                jmvcore::reject('Error cleaning variable names. Please check column names.')
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
            followup_var <- get_cleaned_var(self$options$followupTime)

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
                "followup_var" = followup_var,
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
        .organizeOutcomes = function(labelled_data = NULL) {
            # Get data and variables. Reuse the labelled data already computed in
            # .run() when supplied, so janitor::clean_names + labelled::set_variable_labels
            # (the full name-clean/label pass) runs once per execution instead of twice.
            if (is.null(labelled_data)) {
                labelled_data <- private$.getData()
            }
            mydata <- labelled_data$mydata_labelled
            outcome_var <- labelled_data$outcome_var
            recurrence_var <- labelled_data$recurrence_var
            id_var <- labelled_data$id_var

            # Check if required variables exist
            if (length(outcome_var) == 0 && !is.null(self$options$outcome)) {
                jmvcore::reject('Could not find outcome variable')
            }

            # Get parameters from UI options
            analysistype <- self$options$analysistype
            multievent <- self$options$multievent
            outcomeLevel <- self$options$outcomeLevel

            # Validation diagnostics - will be used if diagnostics are enabled
            diagnostics <- list()

            # Track whether the optional operations actually ran, so the Summary text
            # in .run() reports only what was truly applied (these silently skip when
            # a patient ID / interval / admin-date variable is missing or no duplicate
            # IDs exist). Prevents the report from overstating what was done.
            applied <- list(hierarchy = FALSE, interval = FALSE, admin = FALSE)

            # Create a new outcome variable based on the analysis type
            if (!multievent) {
                # Delegated to the shared coder in survival_utils.R so the five
                # analyses that build an event indicator agree on validation.
                res <- .defineEventIndicator(
                    outcome      = mydata[[outcome_var]],
                    outcomeLevel = outcomeLevel,
                    multievent   = FALSE,
                    outcome_name = self$options$outcome
                )

                if (!is.null(res$error))
                    jmvcore::reject(res$error)

                private$.eventRecode <- res
                mydata[["myoutcome"]] <- res$status
                # Ask the returned VECTOR, not the options branch. .defineEventIndicator()'s
                # hand-off path fires on a Censored/Event/Competing factor regardless of
                # `multievent`, so this branch can return 0/1/2 with a non-NULL status_factor.
                # Leaving .causeFactor NULL made the export at ~line 1271 write the raw
                # numeric, which jamovi returns as a nominal factor with levels "0"/"1"/"2";
                # round-tripping that with outcomeLevel = "1" turned every competing event
                # into a CENSORED observation. NULL for any ordinary binary outcome, so this
                # is a no-op everywhere else.
                private$.causeFactor <- res$status_factor

                diagnostics$binary_check <- sprintf(
                    "Event level '%s' -> 1 (%d rows); %s -> 0 (%d rows); %d row(s) missing.",
                    res$event_label, res$n_event,
                    if (length(res$censored_labels)) paste(sprintf("'%s'", res$censored_labels), collapse = ", ") else "no other level",
                    res$n_censored, res$n_missing)

                # Special handling for RFS/PFS/DFS if selected
                if (analysistype %in% c('rfs', 'pfs', 'dfs') && !is.null(recurrence_var)) {
                    # For these analyses, also consider recurrence/progression as events
                    recurrence_outcome <- mydata[[recurrence_var]]
                    recurrence_level <- self$options$recurrenceLevel

                    # Guard: a recurrence/progression variable is selected but no event
                    # level was chosen. Without this, `recurrence_outcome == NULL` yields
                    # logical(0); ifelse -> length 0; pmax -> numeric(0); and the
                    # assignment mydata[["myoutcome"]] <- numeric(0) errors with
                    # "replacement has 0 rows, data has n".
                    if (is.null(recurrence_level)) {
                        jmvcore::reject(paste0(
                            'A recurrence/progression variable is selected for ',
                            toupper(analysistype),
                            ' analysis, but no recurrence event level was chosen. Please ',
                            'select the level that indicates recurrence/progression in the ',
                            '"Event Level" option under the Recurrence/Progression Variable.'))
                    }

                    # Mark recurrences as events (1)
                    recurrence_events <- ifelse(
                        test = recurrence_outcome == recurrence_level,
                        yes = 1,
                        no = 0
                    )

                    # Combine with death events. RFS/PFS/DFS all take the event as
                    # "recurrence OR death", so the composite is the elementwise max.
                    #
                    # `na.rm = TRUE` used to be passed here, and pmax(NA, 0, na.rm=TRUE)
                    # is 0 -- so a patient with a MISSING vital status or a missing
                    # recurrence status was silently recorded as event-free. That
                    # fabricates censoring out of missing data. Without na.rm the NA
                    # propagates and the row is dropped downstream, which is the
                    # honest complete-case behaviour.
                    #
                    # One asymmetry is still worth keeping: if either component is a
                    # known event, the composite is an event regardless of whether the
                    # other component is missing.
                    composite <- pmax(recurrence_events, mydata[["myoutcome"]])
                    known_event <- (!is.na(recurrence_events) & recurrence_events == 1) |
                                   (!is.na(mydata[["myoutcome"]]) & mydata[["myoutcome"]] == 1)
                    composite[known_event] <- 1
                    mydata[["myoutcome"]] <- composite

                    diagnostics[[paste0(analysistype, "_handling")]] <- sprintf(
                        "%s: events include recurrence/progression and death (%d events, %d censored, %d missing).",
                        toupper(analysistype),
                        sum(composite == 1, na.rm = TRUE),
                        sum(composite == 0, na.rm = TRUE),
                        sum(is.na(composite)))

                    # The shared coder built the indicator from the outcome alone
                    # and labelled it "overall survival". The composite is a
                    # different estimand and now has different counts, so correct
                    # both before the disclosure block reports them.
                    if (!is.null(private$.eventRecode)) {
                        private$.eventRecode$estimand <- switch(analysistype,
                            rfs = "recurrence-free survival",
                            pfs = "progression-free survival",
                            dfs = "disease-free survival",
                            private$.eventRecode$estimand)
                        private$.eventRecode$n_event    <- sum(composite == 1, na.rm = TRUE)
                        private$.eventRecode$n_censored <- sum(composite == 0, na.rm = TRUE)
                        private$.eventRecode$n_missing  <- sum(is.na(composite))
                        private$.eventRecode$event_label <- paste0(
                            private$.eventRecode$event_label, " or recurrence/progression")
                    }
                } else if (analysistype == 'ttp' && !is.null(recurrence_var)) {
                    # Time to progression: only progression counts as event, deaths are censored
                    recurrence_outcome <- mydata[[recurrence_var]]
                    recurrence_level <- self$options$recurrenceLevel

                    # Guard: recurrence variable selected but no progression event level
                    # chosen -> logical(0) recode that crashes the assignment (see above).
                    if (is.null(recurrence_level)) {
                        jmvcore::reject(paste0(
                            'A recurrence/progression variable is selected for TTP analysis, ',
                            'but no progression event level was chosen. Please select the level ',
                            'that indicates progression in the "Event Level" option under the ',
                            'Recurrence/Progression Variable.'))
                    }

                    # Only progression counts as an event
                    mydata[["myoutcome"]] <- ifelse(
                        test = recurrence_outcome == recurrence_level,
                        yes = 1,
                        no = 0
                    )
                    diagnostics$ttp_handling <- "TTP: Only progression events counted, deaths censored"

                    # TTP replaces the outcome-derived indicator entirely.
                    if (!is.null(private$.eventRecode)) {
                        ttp <- mydata[["myoutcome"]]
                        private$.eventRecode$estimand    <- "time to progression (deaths censored)"
                        private$.eventRecode$event_label <- "progression"
                        private$.eventRecode$n_event     <- sum(ttp == 1, na.rm = TRUE)
                        private$.eventRecode$n_censored  <- sum(ttp == 0, na.rm = TRUE)
                        private$.eventRecode$n_missing   <- sum(is.na(ttp))
                    }
                }

                # The event hierarchy is implemented only in the multi-event
                # branch below. Requesting it here used to be read and then
                # silently discarded, so the user got no hierarchy and no hint
                # that the option had been ignored.
                if (isTRUE(self$options$useHierarchy)) {
                    private$.addHtmlMessage(
                        "warning",
                        "Event hierarchy not applied",
                        paste0(
                            "Event hierarchy is only available with Multiple Event Levels ",
                            "enabled. It was requested but has been ignored for this ",
                            "single-event-level analysis, and the outcome is unchanged."))
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

                # os/cause/compete share their semantics with the four survival
                # analyses, so they go through the shared coder -- which is where
                # the "every observed level must be assigned" check lives. Without
                # it, an unassigned level became NA and jmvcore::naOmit() then
                # deleted those patients, shrinking the denominator silently.
                # multistate is unique to this analysis and stays local.
                if (analysistype %in% c('os', 'cause', 'compete')) {

                    res <- .defineEventIndicator(
                        outcome      = outcome1,
                        multievent   = TRUE,
                        analysistype = switch(analysistype, os = "overall", analysistype),
                        dod = dod, dooc = dooc, awd = awd, awod = awod,
                        outcome_name = self$options$outcome
                    )

                    if (!is.null(res$error))
                        jmvcore::reject(res$error)

                    private$.eventRecode <- res
                    mydata[["myoutcome"]] <- res$status
                    private$.causeFactor <- res$status_factor

                    diagnostics$multievent_coding <- sprintf(
                        "%s: %d event(s), %d censored, %d competing, %d missing.",
                        res$estimand, res$n_event, res$n_censored,
                        res$n_competing, res$n_missing)

                } else if (analysistype == 'multistate') {

                    unmapped <- setdiff(as.character(unique(outcome1[!is.na(outcome1)])),
                                        as.character(c(dod, dooc, awd, awod)))
                    if (length(unmapped) > 0)
                        jmvcore::reject(paste0(
                            'Outcome level(s) not assigned to any state: ',
                            paste(unmapped, collapse = ", "),
                            '. Assign every level to one of the four states; unassigned ',
                            'levels would otherwise be dropped from the analysis.'))

                    # Multistate model: Different states given different codes
                    mydata[["myoutcome"]] <- NA_integer_
                    mydata[["myoutcome"]][!is.na(outcome1) & outcome1 == awod] <- 0  # Baseline state
                    mydata[["myoutcome"]][!is.na(outcome1) & outcome1 == awd]  <- 1  # Disease state
                    mydata[["myoutcome"]][!is.na(outcome1) & outcome1 == dod]  <- 2  # Death from disease
                    mydata[["myoutcome"]][!is.na(outcome1) & outcome1 == dooc] <- 3  # Death from other causes

                    diagnostics$multistate_coding <- "Multistate: Healthy (0), Disease (1), Death-disease (2), Death-other (3)"

                } else {
                    # rfs / pfs / dfs / ttp are offered in the analysis-type list
                    # but are only implemented on the single-event-level path,
                    # where the recurrence variable supplies the second endpoint.
                    # Reaching here left myoutcome entirely NA and the run failed
                    # with the misleading "all values are NA", which reads as a
                    # data problem rather than an unsupported combination.
                    jmvcore::reject(paste0(
                        "'", toupper(analysistype), "' is not available with Multiple Event Levels ",
                        "enabled. Recurrence-based endpoints (RFS, PFS, DFS, TTP) are built from ",
                        "the Recurrence/Progression variable together with the outcome, not from ",
                        "the four vital-status categories. Turn Multiple Event Levels off and ",
                        "select a Recurrence/Progression variable, or choose Overall Survival, ",
                        "Cause-Specific, Competing Risks or Multistate."))
                }

                # FIX: Verify that recoding actually worked (not all NAs)
                # This catches cases where selected levels don't match any data values
                n_recoded <- sum(!is.na(mydata[["myoutcome"]]))
                if (n_recoded == 0) {
                    jmvcore::reject('Outcome recoding failed: all values are NA. This usually means the selected outcome levels ',
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
                            # One row per patient -- which is the normal jamovi
                            # layout -- so the hierarchy has nothing to collapse
                            # and the outcome is left exactly as recoded. Stay
                            # quiet rather than implying something was applied.
                            diagnostics$hierarchy <- paste0(
                                "Event hierarchy not applied: each patient has a single record, ",
                                "so there is nothing to collapse. The outcome is unchanged.")
                        } else {
                            # Apply hierarchy
                            # `any()` returns a single value, so ifelse() returned a
                            # length-1 vector that dplyr then recycled across the whole
                            # group. For a patient with no priority event that meant
                            # every record inherited the FIRST record's code -- e.g.
                            # (2, 0) came out as (2, 2). if/else keeps `myoutcome`
                            # whole when the hierarchy does not apply.
                            # With a follow-up time supplied, keep the EARLIEST
                            # priority event per patient: mark that row and
                            # censor the patient's later rows. Without one we can
                            # only stamp every row, which back-propagates a later
                            # event onto earlier records -- the behaviour the
                            # warning below describes.
                            fu_var_h <- labelled_data$followup_var
                            has_time <- !is.null(fu_var_h) && length(fu_var_h) > 0 &&
                                        fu_var_h[1] %in% names(mydata)

                            if (has_time) {
                                mydata[[".hier_t"]] <- jmvcore::toNumeric(mydata[[fu_var_h[1]]])
                                mydata <- mydata %>%
                                    dplyr::group_by(!!dplyr::sym(id_var)) %>%
                                    dplyr::mutate(
                                        .first_pri = suppressWarnings(min(
                                            .hier_t[myoutcome == highest_priority], na.rm = TRUE)),
                                        myoutcome = dplyr::case_when(
                                            !is.finite(.first_pri)          ~ myoutcome,
                                            .hier_t == .first_pri           ~ highest_priority,
                                            TRUE                            ~ 0
                                        )
                                    ) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-".first_pri", -".hier_t")
                            } else {
                                mydata <- mydata %>%
                                    dplyr::group_by(!!dplyr::sym(id_var)) %>%
                                    dplyr::mutate(
                                        myoutcome = if (any(myoutcome == highest_priority, na.rm = TRUE))
                                            highest_priority
                                        else
                                            myoutcome
                                    ) %>%
                                    dplyr::ungroup()
                            }

                            n_affected <- duplicate_ids %>%
                                dplyr::distinct(!!dplyr::sym(id_var)) %>%
                                nrow()

                            # The hierarchy stamps the priority code onto EVERY row
                            # belonging to a patient who has it on any row. It does
                            # not collapse the patient to one record and it cannot
                            # order the rows, because this analysis has no
                            # follow-up-time variable. So if a patient's rows are
                            # successive time points, an event occurring at the LAST
                            # one is written back onto the earlier ones -- which,
                            # fed into a survival model, would move the event
                            # earlier in time. Anyone using long-format data needs
                            # to know that before they trust the output.
                            diagnostics$hierarchy <- sprintf(
                                "Event hierarchy applied (priority: %s) to %d patient(s) with multiple records.",
                                highest_priority, n_affected)

                            # Duplicate IDs mean long-format data, which is the
                            # only situation where this option does anything --
                            # and the one where it can do harm. Say plainly what
                            # it did and what the consequence is, as a visible
                            # warning rather than a diagnostic the user may never
                            # open.
                            if (has_time) {
                                private$.addHtmlMessage(
                                    "info",
                                    "Event hierarchy applied using follow-up time",
                                    sprintf(paste0(
                                        "%d patient(s) have more than one row. For each, the EARLIEST ",
                                        "record carrying the priority outcome (%s) was kept as the event ",
                                        "and their later rows were censored, so the event is not moved ",
                                        "earlier in time."),
                                        n_affected, highest_priority))
                            } else
                            private$.addHtmlMessage(
                                "warning",
                                "Event hierarchy applied to repeated patient records",
                                sprintf(paste0(
                                    "%d patient(s) have more than one row. For each of them the ",
                                    "priority outcome (%s) has been written to EVERY one of their ",
                                    "rows. Because this analysis has no follow-up-time variable it ",
                                    "cannot order those rows, so if they are successive time points ",
                                    "a later event is copied back onto the earlier ones - which in a ",
                                    "survival model would place the event earlier than it happened, ",
                                    "and count it once per row. Reduce your data to one row per ",
                                    "patient (keeping the earliest priority event and its date) ",
                                    "before using this option."),
                                    n_affected, highest_priority))
                            applied$hierarchy <- TRUE
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
                    # type = "interval2" takes exactly two endpoints and infers
                    # the censoring type from them (NA on the left = left-censored,
                    # NA on the right = right-censored, equal = exact). Passing a
                    # third event argument, as this guidance used to, fails with
                    # "Wrong number of args for this type of survival data".
                    #
                    # These two columns are computed here but NOT written to the
                    # spreadsheet: this analysis has a single Output slot and it
                    # carries the recoded outcome. Say so rather than implying a
                    # column the user will go looking for.
                    diagnostics$interval_censoring <- paste0(
                        "Interval endpoints prepared (not written to the spreadsheet - this ",
                        "analysis exports only the recoded outcome). In survival analysis use ",
                        "Surv(<start>, <end>, type='interval2') with your two interval columns; ",
                        "note interval2 takes two time arguments and no event argument.")
                    applied$interval <- TRUE
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
                    mydata[["admin_censor_date"]] <- mydata[[admin_date_var[1]]]

                    # With a follow-up time supplied we can actually apply the
                    # cut-off: truncate follow-up at it and reset the status of
                    # anyone whose event falls after it. Without one this can only
                    # record the date, which is what it used to do while implying
                    # more.
                    fu_var <- labelled_data$followup_var

                    if (!is.null(fu_var) && length(fu_var) > 0 && fu_var[1] %in% names(mydata)) {
                        fu   <- jmvcore::toNumeric(mydata[[fu_var[1]]])
                        cut  <- jmvcore::toNumeric(mydata[["admin_censor_date"]])
                        keep <- !is.na(fu) & !is.na(cut)

                        n_trunc <- sum(keep & fu > cut)
                        n_reset <- sum(keep & fu > cut & !is.na(mydata[["myoutcome"]]) &
                                       mydata[["myoutcome"]] > 0)

                        mydata[["admin_time"]] <- fu
                        mydata[["admin_time"]][keep] <- pmin(fu[keep], cut[keep])
                        # Anyone whose event happened after the cut-off is
                        # censored at the cut-off, not counted as an event.
                        mydata[["myoutcome"]][keep & fu > cut] <- 0

                        diagnostics$admin_censoring <- sprintf(
                            "Administrative censoring applied at the supplied cut-off: follow-up truncated for %d patient(s); %d event(s) occurring after the cut-off were reset to censored.",
                            n_trunc, n_reset)
                        applied$admin <- TRUE
                    } else {
                        diagnostics$admin_censoring <- paste0(
                            "Administrative cut-off date read, but NOT applied: no Follow-up Time ",
                            "variable was selected, so no follow-up was truncated and no event ",
                            "status was reset. Select a Follow-up Time variable to apply it here.")
                        applied$admin <- FALSE
                    }
                } else {
                    diagnostics$admin_censoring <- "Administrative censoring: date variable not found in dataset"
                }
            }

            # Create a data frame with row names and recoded outcome
            df_outcome <- mydata %>% jmvcore::select(c("row_names", "myoutcome"))

            return(list(
                "df_outcome" = df_outcome,
                "mydata" = mydata,
                "diagnostics" = diagnostics,
                "applied" = applied
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
                <dd>Assigns each patient a state code (disease-free, disease, death from disease, death from other causes). Fitting an actual multistate model additionally needs transition times and a subject identifier in long format, which this analysis does not produce.</dd>

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

            # Reset the per-run recode state. .causeFactor persists on the R6
            # object between runs, so after a competing-risks run a switch to
            # binary or multistate mode would export the PREVIOUS run's
            # Censored/Event/Competing column instead of the current outcome.
            private$.causeFactor <- NULL
            private$.eventRecode <- NULL

            # Initial validation
            if (is.null(self$options$outcome)) {
                private$.todo()
                return()
            }

            if (nrow(self$data) == 0)
                jmvcore::reject('Data contains no (complete) rows')

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

            # Organize outcomes (reuse labelled_data computed above to avoid a second
            # janitor::clean_names + labelled pass)
            results <- private$.organizeOutcomes(labelled_data)
            df_outcome <- results$df_outcome
            mydata <- results$mydata
            diagnostics <- results$diagnostics
            applied <- results$applied

            # Create summary text describing the recoding
            analysistype <- self$options$analysistype

            # Generate appropriate summary text based on analysis type
            summary_text <- ""

            # Pre-escape OptionLevel factor labels (user-supplied via dropdown bound to a
            # column's factor levels) before HTML interpolation in the glue::glue blocks
            # below. glue does NOT HTML-escape interpolations.
            #
            # Coerce NULL/empty Level options to "" first: htmlEscape(NULL) returns
            # character(0), and a length-0 interpolation collapses the whole glue::glue
            # block to character(0), so self$results$summary$setContent() would receive
            # an empty vector (blank summary) for valid already-0/1 numeric outcomes
            # where no event level need be chosen.
            .lvl <- function(x) if (is.null(x) || length(x) == 0) "" else as.character(x)
            esc_dod <- htmltools::htmlEscape(.lvl(self$options$dod))
            esc_dooc <- htmltools::htmlEscape(.lvl(self$options$dooc))
            esc_awd <- htmltools::htmlEscape(.lvl(self$options$awd))
            esc_awod <- htmltools::htmlEscape(.lvl(self$options$awod))
            esc_outcomeLevel <- htmltools::htmlEscape(.lvl(self$options$outcomeLevel))
            esc_recurrenceLevel <- htmltools::htmlEscape(.lvl(self$options$recurrenceLevel))

            if (self$options$multievent) {
                if (analysistype == 'os') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Overall Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({esc_dod}): coded as 1<br>
                        - Dead of other causes ({esc_dooc}): coded as 1<br>
                        - Alive with disease ({esc_awd}): coded as 0<br>
                        - Alive without disease ({esc_awod}): coded as 0<br>
                        <br>
                        <i>This coding compares all deaths vs. alive status for standard Kaplan-Meier or Cox regression.</i>
                        "
                    )
                } else if (analysistype == 'cause') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Cause-Specific Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({esc_dod}): coded as 1<br>
                        - Dead of other causes ({esc_dooc}): coded as 0<br>
                        - Alive with disease ({esc_awd}): coded as 0<br>
                        - Alive without disease ({esc_awod}): coded as 0<br>
                        <br>
                        <i>This coding compares disease-specific deaths vs. other outcomes for cause-specific analyses.</i>
                        "
                    )
                } else if (analysistype == 'compete') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Competing Risks Survival Analysis</b><br>
                        Recoded outcome:<br>
                        - Dead of disease ({esc_dod}): coded as 1<br>
                        - Dead of other causes ({esc_dooc}): coded as 2<br>
                        - Alive with disease ({esc_awd}): coded as 0<br>
                        - Alive without disease ({esc_awod}): coded as 0<br>
                        <br>
                        <i>This coding enables competing risk analysis between disease-specific deaths and other causes using cmprsk or other packages.</i>
                        "
                    )
                } else if (analysistype == 'multistate') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Multistate Model Analysis</b><br>
                        Recoded outcome:<br>
                        - Alive without disease ({esc_awod}): coded as 0<br>
                        - Alive with disease ({esc_awd}): coded as 1<br>
                        - Dead of disease ({esc_dod}): coded as 2<br>
                        - Dead of other causes ({esc_dooc}): coded as 3<br>
                        <br>
                        <i>This is a state code, one row per patient. Multistate models additionally require transition times and from/to states in long format (id, tstart, tstop, from, to), which are not produced here.</i>
                        "
                    )
                }
            } else {
                if (analysistype == 'os') {
                    summary_text <- glue::glue(
                        "
                        <br><b>Overall Survival (OS) Analysis</b><br>
                        Recoded outcome:<br>
                        - Death ({esc_outcomeLevel}): coded as 1<br>
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
                        - Death ({esc_outcomeLevel}): coded as 1<br>
                        - Recurrence/Progression ({esc_recurrenceLevel}): coded as 1<br>
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
                        - Progression ({esc_recurrenceLevel}): coded as 1<br>
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
                        - Event ({esc_outcomeLevel}): coded as 1<br>
                        - Non-event (other levels): coded as 0<br>
                        <br>
                        <i>This is standard coding for Cox regression and Kaplan-Meier analysis.</i>
                        "
                    )
                }
            }

            # Add information about special handling ONLY when it was actually applied.
            # These operations silently skip when their prerequisites are missing (no
            # patient ID / no duplicate IDs for hierarchy; interval/admin variables not
            # found), so gate the report text on the applied flags returned by
            # .organizeOutcomes() rather than on the checkbox alone.
            if (isTRUE(applied$hierarchy)) {
                summary_text <- paste(summary_text, glue::glue(
                    "<br><b>Event Hierarchy Applied:</b> If multiple events occur, priority is given to type {self$options$eventPriority}.<br>"
                ))
            }

            if (isTRUE(applied$interval)) {
                summary_text <- paste(summary_text, "<br><b>Interval Censoring:</b> Events are known to occur within time intervals rather than at exact times.<br>")
            }

            if (isTRUE(applied$admin)) {
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
                summary_text <- paste(summary_text, "- A per-patient state code (0 = disease-free, 1 = disease, 2 = death from disease, 3 = death from other causes)<br>- Note: fitting a multistate or illness-death model requires transition times and a subject identifier in long format, which this analysis does not export<br>")
            } else if (analysistype %in% c('rfs', 'pfs', 'dfs')) {
                summary_text <- paste(summary_text, "- Standard survival analysis (Kaplan-Meier, Cox)<br>- Consider competing risks if appropriate<br>")
            } else if (analysistype == 'ttp') {
                summary_text <- paste(summary_text, "- Standard survival analysis with death as censoring<br>- Consider sensitivity analysis treating death as competing risk<br>")
            }

            # Summary now only contains analysis description (validation moved to Notices)
            self$results$summary$setContent(summary_text)

            # Frequency table of recoded outcomes. Computed unconditionally
            # because both the output table and the visualization state consume
            # it; previously it was defined only inside the outputTable block,
            # so enabling the visualization without the table crashed with
            # "object 'outcome_counts' not found".
            outcome_counts <- table(mydata$myoutcome)

            # Add data table if requested
            if (self$options$outputTable) {
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

            # Always disclose how the outcome was recoded. A silent recode is a
            # clinical-safety hazard: the reader cannot otherwise see which
            # levels were collapsed into "censored", nor which estimand the
            # downstream probability-scale outputs correspond to.
            if (!is.null(private$.eventRecode))
                self$results$eventRecodeInfo$setContent(
                    .describeEventIndicator(private$.eventRecode, self$options$outcome))

            # Add recoded outcome to data if requested.
            #
            # For competing risks the column is written as Censored/Event/Competing
            # rather than 0/1/2. The numeric form did not survive the hand-off: it
            # comes back as a nominal factor with levels "0"/"1"/"2", takes the
            # single-event-level branch in survival / multisurvival, and level "2"
            # silently becomes censored -- quietly turning a competing-risks
            # analysis back into a cause-specific one. The labelled form is the
            # representation those analyses already recognise ("Event" %in% levels).
            if (self$options$addOutcome) {
                self$results$addOutcome$setRowNums(df_outcome$row_names)
                # Rebuild the labels from the FINAL status vector.
                #
                # private$.causeFactor is a snapshot taken immediately after the
                # initial recode, before the event hierarchy and administrative
                # censoring mutate myoutcome. Exporting the snapshot meant the
                # spreadsheet column disagreed with the status this analysis had
                # itself just reported: a patient censored at the administrative
                # cut-off was written out as "Event". Downstream that is not
                # cosmetic -- survival and multisurvival re-decode the
                # Censored/Event/Competing hand-off and map "Event" to 1, so the
                # patient re-entered as an event, at untruncated follow-up,
                # silently inflating the event count.
                #
                # .causeFactor now serves only as the "this was a competing-risks
                # run" flag; both branches read the same post-mutation vector, so
                # they agree by construction.
                if (!is.null(private$.causeFactor)) {
                    .lbl <- c("Censored", "Event", "Competing")
                    .idx <- suppressWarnings(as.integer(df_outcome$myoutcome)) + 1L
                    .idx[is.na(.idx) | .idx < 1L | .idx > length(.lbl)] <- NA_integer_
                    # Export a FACTOR that DECLARES all three levels, not a bare
                    # character vector.
                    #
                    # jmvcore's Output$asProtoBuf() does `if (!is.factor(column))
                    # column <- as.factor(column)` and then serialises
                    # `levels(column)`. A character vector therefore reaches jamovi
                    # with only the levels that happen to OCCUR, so a competing-risks
                    # run on a cohort with no other-cause deaths shipped a column
                    # declaring just Censored/Event.
                    #
                    # That silently broke the hand-off. survival_utils.R identifies
                    # this interchange format with
                    #     setequal(levels(outcome), c("Censored","Event","Competing"))
                    # and its comment states the intent explicitly: the outcome
                    # "remains a competing-risk outcome when its Competing level is
                    # declared but unused in this particular cohort". Only a declared
                    # factor can carry that. Without it the downstream analysis fell
                    # through to the ordinary single-event branch and stopped with
                    # "Event Level is not selected", or -- if the user then picked
                    # "Event" -- ran plain Kaplan-Meier with the competing-risk flag
                    # lost.
                    #
                    # Declaring the levels also fixes their ORDER: as.factor() sorts
                    # alphabetically (Censored, Competing, Event), which no longer
                    # matches the 0/1/2 status codes these labels stand for.
                    self$results$addOutcome$setValues(factor(.lbl[.idx], levels = .lbl))
                } else {
                    self$results$addOutcome$setValues(df_outcome$myoutcome)
                }
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

                # Calculate frequencies. The denominator must be ALL non-missing coded
                # records: for competing risks (codes 0/1/2) and multistate (0/1/2/3)
                # the events (==1) plus censored (==0) counts do NOT sum to the total,
                # so using n_events + n_censored silently drops the competing/other-cause
                # states and reports percentages against a wrong total. For binary OS
                # coding this is identical to the previous denominator.
                n_events <- sum(mydata$myoutcome == 1, na.rm = TRUE)
                n_censored <- sum(mydata$myoutcome == 0, na.rm = TRUE)
                total_n <- sum(!is.na(mydata$myoutcome))
                event_pct <- if (total_n > 0) round(n_events / total_n * 100, 1) else 0
                censor_pct <- if (total_n > 0) round(n_censored / total_n * 100, 1) else 0

                # For competing-risks / multistate coding, append an explicit per-state
                # breakdown so the copy-ready text does not imply the non-event group
                # accounts for every remaining patient.
                state_breakdown <- ""
                if (analysistype %in% c("compete", "multistate")) {
                    state_tab <- table(mydata$myoutcome)
                    state_lines <- vapply(names(state_tab), function(v) {
                        cnt <- as.integer(state_tab[[v]])
                        pct <- if (total_n > 0) round(cnt / total_n * 100, 1) else 0
                        sprintf("%s: %d (%.1f%%)",
                                htmltools::htmlEscape(private$.getOutcomeLabel(v, analysistype, self$options$multievent)),
                                cnt, pct)
                    }, character(1))
                    state_breakdown <- paste0(
                        " Full state breakdown (of ", total_n, " coded records): ",
                        paste(state_lines, collapse = "; "), "."
                    )
                }

                natural_summary <- sprintf(
                    "<div style='background-color: #e7f3ff; padding: 15px; border-radius: 8px; margin: 10px 0;'>
                    <b> Copy-Ready Report Text:</b><br><br>
                    The outcome variable '<b>%s</b>' was recoded for <b>%s</b> analysis.
                    Events (coded as 1) represent %s.
                    Non-events (coded as 0) represent %s.
                    The recoded variable '<b>myoutcome</b>' contains <b>%d events (%.1f%%)</b> and <b>%d non-events (%.1f%%)</b> out of %d coded records.%s
                    </div>",
                    htmltools::htmlEscape(self$options$outcome),
                    analysis_type_labels[[analysistype]],
                    event_desc,
                    censor_desc,
                    n_events, event_pct,
                    n_censored, censor_pct,
                    total_n,
                    state_breakdown
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
