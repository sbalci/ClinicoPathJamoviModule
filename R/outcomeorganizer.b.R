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

        # Truncated follow-up produced by administrative censoring, carried out of
        # .organizeOutcomes() so .run() can write it to its own output column.
        # NULL whenever the cut-off was not actually applied.
        .adminTime = NULL,

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
            # The switch below matched "strongWarning" while every caller in this file
            # wrote "strong_warning", so both of them -- the date-serial no-op and the
            # "this cut-off would delete every event" refusal, the two loudest things
            # this analysis can say -- fell through to the default and rendered as an
            # ordinary amber warning. switch() has no unmatched-value signal, so it was
            # silent. Normalise here rather than at the call sites: it is one place, and
            # it cannot drift back the next time someone writes the name from memory.
            type <- switch(gsub("[^a-z]", "", tolower(type)),
                "error" = "error",
                "strongwarning" = "strongWarning",
                "warning" = "warning",
                "info" = "info",
                "infomessage" = "info",
                "warning")
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
                '<div style="margin: 10px 0; padding: 10px; border-left: 4px solid %s; background-color: rgba(138, 155, 172, 0.06); color: inherit;"><strong>%s:</strong> %s</div>',
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
                jmvcore::reject(.("Error cleaning variable names. Please check column names."))
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
                strong_warnings = character(0),
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
                    validation_results$strong_warnings <- c(validation_results$strong_warnings,
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
                    
                    # Few events: Kaplan-Meier / Cox estimates are unstable below ~5
                    # events. table() drops NA, so a 2-level outcome with missing
                    # values is still checked (unique() would have counted NA as a
                    # third level and skipped it). Count the selected event level
                    # when there is one; otherwise fall back to the rarest level.
                    tbl <- table(mydata[[outcome_var]])
                    n_nonmissing <- sum(tbl)
                    event_lvl <- if (multievent) NULL else self$options$outcomeLevel
                    if (!is.null(event_lvl) && as.character(event_lvl) %in% names(tbl)) {
                        n_ev <- as.integer(tbl[[as.character(event_lvl)]])
                        rare_lvl <- as.character(event_lvl)
                    } else if (length(tbl) == 2) {
                        n_ev <- as.integer(min(tbl))
                        rare_lvl <- names(tbl)[which.min(tbl)]
                    } else {
                        n_ev <- NA_integer_
                    }
                    if (!is.na(n_ev) && n_nonmissing > 0 && n_ev < 5) {
                        validation_results$warnings <- c(validation_results$warnings,
                            paste0("Few events: only ", n_ev, " of ", n_nonmissing,
                                   " non-missing outcomes (", round(100 * n_ev / n_nonmissing, 1),
                                   "%) are '", rare_lvl, "'. Kaplan-Meier and Cox estimates ",
                                   "are unstable with fewer than 5 events."))
                    }
                }
            }

            # 8. Contextual warnings for potential misuse
            if (analysistype == "cause" && !multievent) {
                validation_results$warnings <- c(validation_results$warnings,
                    "Cause-specific survival typically requires distinguishing between disease deaths and other deaths. Consider enabling 'Multiple Event Types' or switch to 'Overall Survival'.")
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
                jmvcore::reject(.("Could not find outcome variable"))
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
            private$.adminTime <- NULL   # per-run reset; only set when the cut-off truly applies

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
                        jmvcore::reject(.fmt(
                            .('A recurrence/progression variable is selected for {analysis} analysis, but no recurrence event level was chosen. Please select the level that indicates recurrence/progression in the "Event Level" option under the Recurrence/Progression Variable.'),
                            analysis = toupper(analysistype)))
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
                        "%s: events include recurrence/progression and death (%d events, %d censored, %d missing). This analysis produces the STATUS only - it has no recurrence-time variable and therefore cannot compute time to FIRST event.",
                        toupper(analysistype),
                        sum(composite == 1, na.rm = TRUE),
                        sum(composite == 0, na.rm = TRUE),
                        sum(is.na(composite)))

                    # THE TIME IS THE USER'S PROBLEM, AND THEY HAVE TO BE TOLD.
                    # A composite endpoint is time-to-FIRST-event, but this analysis
                    # has only one time variable (followupTime, for censoring) and no
                    # recurrence/progression date at all, so it cannot take the
                    # minimum. It ORs the indicators and hands back a status. Pair
                    # that status with the death/last-contact time -- the obvious
                    # thing to do -- and a patient who recurred at 6 months and died
                    # at 40 is recorded as having an event at 40. Recurrence-free
                    # survival is then overstated for exactly the patients the
                    # endpoint exists to count. Nothing on screen said so.
                    n_composite <- sum(composite == 1, na.rm = TRUE)
                    if (n_composite > 0) {
                        private$.addHtmlMessage(
                            "warning",
                            sprintf("%s status produced - pair it with time to FIRST event",
                                    toupper(analysistype)),
                            sprintf(paste0(
                                "The composite status marks %d patient(s) as having had recurrence/progression ",
                                "or death. This analysis writes the STATUS only: it has no recurrence or ",
                                "progression date, so it cannot work out which event came first or when. ",
                                "Pairing this status with a death or last-contact time overstates %s for every ",
                                "patient whose recurrence preceded their death or censoring - their event is ",
                                "recorded at the later time. Supply time to first event yourself, as the ",
                                "minimum of the recurrence date and the death/last-contact date."),
                                n_composite, toupper(analysistype)))
                    }

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
                        jmvcore::reject(.('A recurrence/progression variable is selected for TTP analysis, but no progression event level was chosen. Please select the level that indicates progression in the "Event Level" option under the Recurrence/Progression Variable.'))
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

                    # DUPLICATE ASSIGNMENT. This is the only multi-event path that
                    # does not go through .defineEventIndicator(), so it did not get
                    # that function's duplicate check (R/survival_utils.R). The four
                    # writes below are sequential, so assigning one level to two state
                    # slots silently lets the LAST write win and the patient lands in
                    # the wrong state -- with the Summary then printing two
                    # contradictory lines about the same level. `unmapped` cannot
                    # catch it: a duplicated level IS mapped, just twice. The same
                    # configuration under `compete` is correctly rejected.
                    .slots <- list(awod = awod, awd = awd, dod = dod, dooc = dooc)
                    .filled <- unlist(.slots[vapply(.slots,
                                       function(b) !is.null(b) && length(b) > 0, logical(1))],
                                      use.names = FALSE)
                    .dupes <- unique(.filled[duplicated(.filled)])
                    if (length(.dupes) > 0)
                        jmvcore::reject(.fmt(
                            .("Each outcome level may be assigned to only one state. Assigned to more than one: {levels}."),
                            levels = paste(.dupes, collapse = ", ")))

                    unmapped <- setdiff(as.character(unique(outcome1[!is.na(outcome1)])),
                                        as.character(c(dod, dooc, awd, awod)))
                    if (length(unmapped) > 0)
                        jmvcore::reject(.fmt(
                            .("Outcome level(s) not assigned to any state: {levels}. Assign every level to one of the four states; unassigned levels would otherwise be dropped from the analysis."),
                            levels = paste(unmapped, collapse = ", ")))

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
                    jmvcore::reject(.fmt(
                        .("'{analysis}' is not available with Multiple Event Levels enabled. Recurrence-based endpoints (RFS, PFS, DFS, TTP) are built from the Recurrence/Progression variable together with the outcome, not from the four vital-status categories. Turn Multiple Event Levels off and select a Recurrence/Progression variable, or choose Overall Survival, Cause-Specific, Competing Risks or Multistate."),
                        analysis = toupper(analysistype)))
                }

                # FIX: Verify that recoding actually worked (not all NAs)
                # This catches cases where selected levels don't match any data values
                n_recoded <- sum(!is.na(mydata[["myoutcome"]]))
                if (n_recoded == 0) {
                    jmvcore::reject(.fmt(
                        .('Outcome recoding failed: all values are NA. This usually means the selected outcome levels ("{dod}", "{dooc}", "{awd}", "{awod}") do not match the actual values in your data. Available values in outcome variable: {available}. Please verify your level selections are correct.'),
                        dod = dod, dooc = dooc, awd = awd, awod = awod,
                        available = paste(unique(outcome1[!is.na(outcome1)]), collapse = ", ")))
                } else if (n_recoded < length(outcome1) * 0.5) {
                    # LOSING HALF THE COHORT NEEDS A NOTICE, NOT A warning().
                    # An unrecoded row becomes NA and jmvcore::naOmit() then deletes
                    # it, so the denominator shrinks without the numbers ever saying
                    # so. The two places this was reported both fail the user: R
                    # warnings land in the undifferentiated "Analysis Notes" panel
                    # among package chatter, and `diagnostics$recoding_warning` goes
                    # to a Diagnostics table that is OFF BY DEFAULT.
                    pct_lost <- round((1 - n_recoded / length(outcome1)) * 100, 1)
                    # Blaming the level selections here would be wrong: both
                    # multi-event coders reject an unassigned level outright, so by
                    # the time execution reaches this line every OBSERVED level is
                    # mapped and the loss is missing data in the outcome column.
                    private$.addHtmlMessage(
                        "strong_warning",
                        "Most rows have no usable outcome",
                        sprintf(paste0(
                            "Only %d of %d row(s) (%.1f%%) have an outcome value; the other %.1f%% are ",
                            "missing in the outcome variable and are dropped from every number below, ",
                            "so the denominator is smaller than your dataset and any percentage is over ",
                            "the survivors of that filtering. The levels you assigned (%s) cover every ",
                            "value that IS present, so this is missing data rather than a mis-selected ",
                            "level. Check that the outcome column is complete before reporting these numbers."),
                            n_recoded, length(outcome1),
                            n_recoded / length(outcome1) * 100, pct_lost,
                            paste(unique(c(private$.lvlOrUnset(dod), private$.lvlOrUnset(dooc),
                                           private$.lvlOrUnset(awd), private$.lvlOrUnset(awod))),
                                  collapse = ", ")))
                    diagnostics$recoding_warning <- sprintf("Only %d/%d (%.1f%%) outcomes successfully recoded",
                                                             n_recoded, length(outcome1),
                                                             n_recoded/length(outcome1) * 100)
                }

                # Apply event hierarchy if specified
                if (self$options$useHierarchy) {
                    # If multiple events could be coded for the same patient, apply hierarchy
                    highest_priority <- self$options$eventPriority
                    # `min: 1` in the .a.yaml is enforced by the GUI and by the
                    # generated wrapper, but NOT by Options$new(), so a programmatic
                    # caller can still pass 0 -- which makes "censored" outrank every
                    # event and erases them all. The .a.yaml text promises this is
                    # impossible; make that true wherever the value arrives from.
                    if (!is.null(highest_priority) && isTRUE(highest_priority < 1)) {
                        jmvcore::reject(.fmt(
                            .("Priority Event Type must be 1 or greater (it is {value}). Code 0 is the censored/baseline code, so giving it priority would make censoring outrank every event and remove them all."),
                            value = highest_priority))
                    }
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

                            # A priority code that the recode never produced makes the
                            # hierarchy a no-op -- every branch below falls through to
                            # "keep myoutcome" -- while the diagnostic still reported
                            # "Event hierarchy applied (priority: N)". Say so instead.
                            # The option is a free integer, so this is easy to hit:
                            # overall survival codes only 0/1, and a user who typed 2
                            # thinking of competing risks gets silence.
                            present_codes <- sort(unique(stats::na.omit(mydata[["myoutcome"]])))

                            # THE SAME REFUSAL THE ADMIN-CENSORING PATH MAKES.
                            # The recode below writes literal 0 to every non-priority
                            # row (`TRUE ~ 0`). Under the multistate coding 0 is
                            # "Alive without disease" -- a clinical state, not a
                            # censoring indicator -- so a patient who was Alive WITH
                            # disease is silently rewritten as disease-free, and a
                            # competing event (2) recorded before the priority event
                            # is erased. Measured: rows (AWD, DOD) for one patient at
                            # priority 2 came out 0, 2 -- the disease state gone.
                            # The administrative-censoring block refuses exactly this
                            # a few hundred lines below; the hierarchy had no such
                            # guard, which is the inconsistency this closes.
                            if (identical(self$options$analysistype, "multistate")) {
                                diagnostics$hierarchy <- paste0(
                                    "Event hierarchy NOT applied: the multistate coding has no censored code ",
                                    "(0 means 'alive without disease', a state rather than a censoring indicator), ",
                                    "so collapsing a patient's non-priority rows to 0 would relabel them into the ",
                                    "baseline state rather than censor them. No record was changed.")
                                private$.addHtmlMessage(
                                    "warning",
                                    "Event hierarchy not applied",
                                    paste0(
                                        "The multistate coding has no censored code - 0 means 'alive without ",
                                        "disease', a clinical state - so collapsing repeated records to 0 would ",
                                        "erase real disease states rather than censor them. Nothing was changed. ",
                                        "Reduce the data to one row per patient before this analysis, or use an ",
                                        "analysis type whose 0 code means censored."))
                                applied$hierarchy <- FALSE
                            } else if (!(highest_priority %in% present_codes) &&
                                       length(present_codes) > 0) {
                                diagnostics$hierarchy <- sprintf(
                                    "Event hierarchy NOT applied: the priority event code %s does not occur in the recoded outcome, which contains only %s. No record was changed. Set Priority Event Type to one of the codes actually produced by this analysis type.",
                                    highest_priority, paste(present_codes, collapse = ", "))
                                private$.addHtmlMessage(
                                    "warning",
                                    "Event hierarchy not applied",
                                    sprintf(paste0(
                                        "The priority event code %s does not occur in the recoded outcome ",
                                        "(the codes produced here are %s), so the event hierarchy changed nothing. ",
                                        "Set Priority Event Type to one of those codes."),
                                        highest_priority, paste(present_codes, collapse = ", ")))
                                applied$hierarchy <- FALSE
                            } else

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
                                            # `TRUE ~ 0` here overwrote EVERY other row,
                                            # earlier ones included -- so a competing
                                            # event (code 2) recorded BEFORE the priority
                                            # event was erased to censored, and under a
                                            # competing-risks coding that is a
                                            # substantive statistical error, not a
                                            # bookkeeping one. It also made the notice
                                            # below ("their later rows were censored")
                                            # false. Censor only what actually comes
                                            # after the retained event; leave earlier
                                            # records as they were coded.
                                            .hier_t > .first_pri            ~ 0,
                                            TRUE                            ~ myoutcome
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
                            # Only report success when the priority code actually
                            # exists; otherwise the guard above already explained why
                            # nothing happened, and this would overwrite it.
                            # BRACED deliberately: without the braces this guarded only
                            # the diagnostics assignment, and the two .addHtmlMessage()
                            # calls below still told the user the priority code "has
                            # been written to EVERY one of their rows" on a run where
                            # the hierarchy provably changed nothing.
                            hierarchy_ran <- !identical(self$options$analysistype, "multistate") &&
                                             highest_priority %in% present_codes
                            if (hierarchy_ran) {
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
                            }
                            applied$hierarchy <- hierarchy_ran
                        }
                    }
                }
            }

            # Cache labelled data lookups for efficiency (used multiple times)
            all_labels_cache <- labelled::var_label(mydata)

            # Apply interval censoring if specified
            # A ticked checkbox whose partner variable is missing produced output
            # byte-identical to leaving it unticked, with no notice and no diagnostic
            # row -- the guard sat on the whole block, so nothing ran at all.
            if (self$options$intervalCensoring &&
                (is.null(self$options$intervalStart) || is.null(self$options$intervalEnd))) {
                diagnostics$interval_censoring <- paste0(
                    "Interval censoring NOT applied: both an Interval Start and an Interval End ",
                    "variable are required, and at least one was not selected.")
                private$.addHtmlMessage(
                    "warning",
                    "Interval censoring not applied",
                    paste0(
                        "Interval censoring is switched on but needs BOTH an Interval Start and an ",
                        "Interval End variable; at least one is not selected, so nothing was done ",
                        "and these results are identical to leaving it switched off."))
            }

            if (self$options$intervalCensoring && !is.null(self$options$intervalStart) && !is.null(self$options$intervalEnd)) {
                # Get interval variables from cached labels
                start_var <- names(all_labels_cache)[all_labels_cache == self$options$intervalStart]
                end_var <- names(all_labels_cache)[all_labels_cache == self$options$intervalEnd]

                if (length(start_var) > 0 && length(end_var) > 0) {
                    # Add interval variables to output for use with survival::Surv()
                    mydata[["interval_L"]] <- mydata[[start_var[1]]]
                    mydata[["interval_R"]] <- mydata[[end_var[1]]]

                    # An interval must run forwards. Surv(type="interval2") requires
                    # left <= right; hand it an inverted pair and it errors, or worse
                    # silently produces a nonsense interval. Nothing checked this, so
                    # a swapped pair of columns was reported as "Interval endpoints
                    # prepared" and the Summary claimed interval censoring was in use.
                    # `jmvcore::toNumeric()` alone made this check DEAD CODE for
                    # the columns most likely to be used here. intervalStart and
                    # intervalEnd carry no `permitted:`, so unlike followupTime they
                    # accept a date or a text column -- and toNumeric is a no-op on
                    # both, so `.il`/`.ir` came back all-NA, `.inv` was 0, and a
                    # swapped pair of date columns was reported as "Interval
                    # endpoints prepared" with no warning at all.
                    .l_s <- private$.timeScale(mydata[["interval_L"]])
                    .r_s <- private$.timeScale(mydata[["interval_R"]])
                    .il <- .l_s$v
                    .ir <- .r_s$v
                    .inv <- sum(!is.na(.il) & !is.na(.ir) & .il > .ir)
                    # A date start against a numeric end is not an interval, and
                    # comparing them says nothing -- the same cross-scale problem the
                    # administrative cut-off refuses on.
                    .kind_mismatch <- !identical(.l_s$kind, .r_s$kind) ||
                                      .l_s$kind == "unreadable"
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

                    if (.kind_mismatch) {
                        diagnostics$interval_censoring <- sprintf(
                            paste0("Interval censoring NOT prepared: the interval start is a %s and the interval end is a %s, ",
                                   "so they are not on the same scale and the interval cannot be checked or used. ",
                                   "Supply both endpoints as the same kind of column, in the same units and from the same origin."),
                            .l_s$kind, .r_s$kind)
                        private$.addHtmlMessage(
                            "warning",
                            "Interval endpoints not on the same scale",
                            paste0(diagnostics$interval_censoring,
                                   " These results are identical to leaving interval censoring switched off."))
                        applied$interval <- FALSE
                    } else if (.inv > 0) {
                        diagnostics$interval_censoring <- sprintf(
                            "Interval censoring: %d row(s) have a start later than their end, so the interval runs backwards. Surv(type='interval2') requires start <= end. Check that the two columns are the right way round.",
                            .inv)
                        private$.addHtmlMessage(
                            "warning",
                            "Interval endpoints run backwards",
                            sprintf(paste0(
                                "%d row(s) have an interval start later than their interval end. ",
                                "An interval must run forwards - Surv(type='interval2') requires ",
                                "start <= end - so these rows cannot be used as they stand. Check ",
                                "that the Interval Start and Interval End variables are not swapped."),
                                .inv))
                        applied$interval <- FALSE
                    }
                } else {
                    diagnostics$interval_censoring <- "Interval censoring: variables not found in dataset"
                }
            }

            # Same silent no-op as interval censoring above.
            if (self$options$adminCensoring && is.null(self$options$adminDate)) {
                diagnostics$admin_censoring <- paste0(
                    "Administrative censoring NOT applied: no cut-off date variable was selected.")
                private$.addHtmlMessage(
                    "warning",
                    "Administrative censoring not applied",
                    paste0(
                        "Administrative censoring is switched on but no cut-off date variable is ",
                        "selected, so nothing was truncated and no event status was reset - these ",
                        "results are identical to leaving it switched off."))
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
                        # as.numeric() explicitly, not jmvcore::toNumeric() alone:
                        # toNumeric is a NO-OP on a Date/character column (it only
                        # unwraps a `values` attribute), so `cut` stayed a Date here and
                        # every later arithmetic comparison silently coerced it to a day
                        # count. Coercing up front makes the scale explicit, which is
                        # what the mismatch guard below has to reason about.
                        # COMPARABLE SCALES, DECIDED BY TYPE -- NOT BY MAGNITUDE.
                        # `fu > cut` only means anything if both sides are on the same
                        # scale, and the two ways to get this wrong are both real:
                        # a DURATION against a DATE (a date as a number is a day count
                        # since 1970 -- 18992 for 2021-12-31 -- so "59 months > 18992"
                        # is never TRUE and the cut-off silently never fires), and a
                        # FACTOR column, where as.numeric() returns LEVEL INDICES
                        # (1, 2, 3...) rather than anything temporal, truncating every
                        # follow-up to a tiny integer.
                        #
                        # A magnitude heuristic cannot separate these. It was tried --
                        # "refuse when the cut-off is more than 10x the largest
                        # follow-up" -- and it fails in BOTH directions: follow-up in
                        # DAYS against a date is only a 5x ratio, so a real mismatch
                        # passed; while a surgical registry whose per-patient
                        # administrative window is legitimately in the same units was
                        # refused with a message telling the user to do what they had
                        # already done. Classify the columns instead.
                        fu_s  <- private$.timeScale(mydata[[fu_var[1]]])
                        # A NEGATIVE FOLLOW-UP IS NOT A TIME.
                        # pmin() happily carries -5 through, and this analysis WRITES
                        # THAT COLUMN BACK, so the negative value lands in the
                        # spreadsheet ready to be handed to Surv() -- which rejects
                        # negative times outright. A zero survives Surv() but
                        # contributes no person-time and usually means a missing date
                        # was read as the origin rather than a same-day event.
                        # Nothing looked at this, so both passed silently.
                        .fu_raw <- fu_s$v
                        n_neg  <- sum(!is.na(.fu_raw) & .fu_raw < 0)
                        n_zero <- sum(!is.na(.fu_raw) & .fu_raw == 0)
                        if (n_neg > 0 || n_zero > 0)
                            private$.addHtmlMessage(
                                "warning",
                                "Follow-up times are not all positive",
                                paste0(
                                    if (n_neg > 0) sprintf(
                                        "%d patient(s) have a NEGATIVE follow-up time, which cannot be a duration - survival functions reject it. This usually means the start and end dates are the wrong way round. ",
                                        n_neg) else "",
                                    if (n_zero > 0) sprintf(
                                        "%d patient(s) have a follow-up time of zero, which contributes no person-time; check whether a missing date was read as the origin. ",
                                        n_zero) else "",
                                    "These rows are truncated and written back unchanged in sign - fix them in the data before running a survival model."))
                        cut_s <- private$.timeScale(mydata[["admin_censor_date"]])
                        fu    <- fu_s$v
                        cut   <- cut_s$v
                        keep  <- !is.na(fu) & !is.na(cut)

                        scale_mismatch <- !identical(fu_s$kind, cut_s$kind) ||
                                          fu_s$kind == "unreadable"
                        # Nothing to compare is not success. With no row carrying both
                        # a follow-up and a cut-off, every guard below is vacuous and
                        # pmin() would leave the follow-up untouched while the analysis
                        # reported the cut-off as applied.
                        no_overlap <- sum(keep) == 0

                        # GUARD 2 -- A CENSORED CODE MUST EXIST.
                        # Censoring writes 0, but under the multistate coding 0 is
                        # "Alive without disease", a real clinical state, not censored.
                        # Applying it there RELABELS patients into the baseline state
                        # instead of censoring them, which is worse than not applying it.
                        multistate_coding <- identical(self$options$analysistype, "multistate")

                        if (scale_mismatch || no_overlap) {
                            diagnostics$admin_censoring <- if (no_overlap)
                                paste0("Administrative cut-off NOT applied: no row has both a follow-up time and a cut-off value, so there was nothing to compare.")
                            else if (fu_s$kind == "unreadable" || cut_s$kind == "unreadable")
                                paste0("Administrative cut-off NOT applied: the ",
                                       if (fu_s$kind == "unreadable") "follow-up time" else "cut-off",
                                       " column could not be read as a number or a date.")
                            # "Supply both as dates" was impossible advice: the
                            # Follow-up Time field is `permitted: [numeric]`, so jamovi
                            # will not let a date column be dropped into it at all. The
                            # only route the GUI actually offers is to make the CUT-OFF
                            # a duration on the follow-up's own scale, so say that.
                            else sprintf(
                                paste0("Administrative cut-off NOT applied: the follow-up time is a %s and the cut-off is a %s, so they are not on the same scale and cannot be compared. ",
                                       "The Follow-up Time field accepts numeric columns only, so express the cut-off the same way - as a duration in the same units and from the same origin as the follow-up time (for a date cut-off, that is the number of %s between each patient's start date and the cut-off date)."),
                                fu_s$kind, cut_s$kind,
                                if (identical(fu_s$kind, "number")) "time units" else "days")
                            private$.addHtmlMessage(
                                "warning",
                                "Administrative censoring not applied",
                                paste0(diagnostics$admin_censoring,
                                       " These results are identical to leaving administrative censoring switched off."))
                            applied$admin <- FALSE
                        } else if (multistate_coding) {
                            diagnostics$admin_censoring <- paste0(
                                "Administrative cut-off NOT applied: the multistate coding has no censored code ",
                                "(0 means 'alive without disease', a state rather than a censoring indicator), so censoring ",
                                "at the cut-off would relabel patients into the baseline state instead of censoring them. ",
                                "Apply the cut-off with an analysis type that has a censored code, or truncate follow-up before this analysis.")
                            private$.addHtmlMessage(
                                "warning",
                                "Administrative censoring not applied",
                                paste0(
                                    "The multistate coding has no censored code - 0 means 'alive without ",
                                    "disease', a clinical state rather than a censoring indicator - so ",
                                    "censoring at the cut-off would relabel patients into the baseline ",
                                    "state instead of censoring them. Nothing was changed. Use an analysis ",
                                    "type that has a censored code, or truncate follow-up before this analysis."))
                            applied$admin <- FALSE
                        } else {
                            n_trunc <- sum(keep & fu > cut)
                            n_reset <- sum(keep & fu > cut & !is.na(mydata[["myoutcome"]]) &
                                           mydata[["myoutcome"]] > 0)
                            n_events_before <- sum(!is.na(mydata[["myoutcome"]]) &
                                                   mydata[["myoutcome"]] > 0)

                            # JUDGE BY CONSEQUENCE, BECAUSE UNITS CANNOT BE INFERRED.
                            # The type check above only separates a date from a number.
                            # It cannot help for the combination the GUI actually
                            # delivers: followupTime is `permitted: [numeric]`, so it
                            # is ALWAYS a number, and a numeric cut-off is a number
                            # too. Follow-up in days against a cut-off typed in months
                            # therefore passes every structural check while deleting
                            # every event -- measured: 25 deaths removed, 40 patients
                            # all truncated to 36, and a green "completed successfully"
                            # notice. A date serial imported as a number (44561) is the
                            # mirror case: nothing is truncated and the analysis still
                            # reports the cut-off as applied.
                            #
                            # No heuristic can recover the units, so refuse on the two
                            # outcomes that cannot be a real cut-off:
                            #   * it removes EVERY event -- a cut-off that leaves no
                            #     event is a unit error, not an analysis;
                            #   * it truncates EVERY patient -- the cut-off precedes
                            #     the whole cohort's follow-up.
                            # Anything short of that is applied, but never silently:
                            # the impact is stated in a visible notice, not only in a
                            # Diagnostics table that is off by default.
                            # ROWS THE CUT-OFF COULD NOT REACH ARE NOT "UNAFFECTED".
                            # `keep` drops any row missing a follow-up time or a
                            # cut-off, and those rows keep their full follow-up AND
                            # their event. That is a PARTIAL application of the cut-off:
                            # some patients are censored at it and some are not, which
                            # is exactly the immortal-time bias administrative censoring
                            # exists to remove. Every count below is over `keep` too, so
                            # the notice read "truncated for 3 of 37" and never
                            # mentioned the other 13 -- the user could not tell from any
                            # output that the cut-off had been applied to only part of
                            # the cohort.
                            n_exempt <- sum(!keep)
                            exempt_note <- if (n_exempt > 0) sprintf(
                                paste0(" %d patient(s) were left untouched because they have no follow-up time or no cut-off value; ",
                                       "they keep their full follow-up and their original event status, so the cut-off applies to only part of the cohort. ",
                                       "Supply the missing values, or exclude those rows, before comparing groups."),
                                n_exempt) else ""

                            wipes_all_events <- n_events_before > 0 && n_reset == n_events_before
                            truncates_everyone <- sum(keep) > 0 && n_trunc == sum(keep)
                            # The mirror case: nothing is beyond the cut-off, so it did
                            # nothing. Legitimate when the cut-off genuinely follows the
                            # whole cohort -- but indistinguishable from a date serial
                            # imported as a plain number (44561 against months of
                            # follow-up), and in BOTH cases claiming "applied" is false
                            # and writing a column titled "Follow-up Truncated at
                            # Administrative Cut-off" that holds untruncated values is
                            # worse than false. Report it as a no-op either way.
                            truncates_nobody <- sum(keep) > 0 && n_trunc == 0

                            if (truncates_nobody) {
                                looks_like_serial <- stats::median(cut[keep], na.rm = TRUE) > 10000 &&
                                                     max(fu[keep], na.rm = TRUE) < 1000
                                diagnostics$admin_censoring <- paste0(
                                    "Administrative cut-off had no effect: no patient's follow-up extends beyond it, ",
                                    "so nothing was truncated and no event status was reset.",
                                    if (looks_like_serial)
                                        sprintf(" The cut-off reads %s while follow-up reaches only %s - that is what a date looks like once it has been imported as a plain number, and it is almost certainly not in the same units as the follow-up time.",
                                                base::format(stats::median(cut[keep], na.rm = TRUE)),
                                                base::format(max(fu[keep], na.rm = TRUE)))
                                    else "")
                                private$.addHtmlMessage(
                                    if (looks_like_serial) "strong_warning" else "info",
                                    "Administrative censoring had no effect",
                                    paste0(diagnostics$admin_censoring,
                                           " No truncated follow-up column was written, because it would be identical to your original follow-up."))
                                applied$admin <- FALSE
                            } else if (wipes_all_events || truncates_everyone) {
                                diagnostics$admin_censoring <- sprintf(
                                    paste0("Administrative cut-off NOT applied: at this cut-off %s, which cannot be right. ",
                                           "The follow-up time and the cut-off are almost certainly in different units - ",
                                           "follow-up runs from %s to %s and the cut-off is %s. ",
                                           "Express the cut-off in the same units as the follow-up time, measured from the same origin."),
                                    if (wipes_all_events)
                                        sprintf("every one of the %d events would be reset to censored", n_events_before)
                                    else sprintf("all %d patients would be truncated", n_trunc),
                                    base::format(min(fu[keep], na.rm = TRUE)),
                                    base::format(max(fu[keep], na.rm = TRUE)),
                                    base::format(stats::median(cut[keep], na.rm = TRUE)))
                                private$.addHtmlMessage(
                                    "strong_warning",
                                    "Administrative censoring not applied",
                                    paste0(diagnostics$admin_censoring,
                                           " Nothing was changed; these results are identical to leaving administrative censoring switched off."))
                                applied$admin <- FALSE
                            } else {

                            mydata[["admin_time"]] <- fu
                            mydata[["admin_time"]][keep] <- pmin(fu[keep], cut[keep])
                            # Anyone whose event happened after the cut-off is
                            # censored at the cut-off, not counted as an event.
                            mydata[["myoutcome"]][keep & fu > cut] <- 0

                            # The TRUNCATED TIME has to travel with the censored status.
                            # This analysis exports only the status column, so pairing it
                            # with the user's ORIGINAL follow-up gives a censored patient
                            # their full untruncated time -- person-time inflated and the
                            # event removed, which biases survival upward exactly where
                            # the cut-off was supposed to protect it. Carried out of the
                            # recoder so .run() can hand it to the second output column.
                            private$.adminTime <- mydata[["admin_time"]]

                            diagnostics$admin_censoring <- paste0(sprintf(
                                "Administrative censoring applied at the supplied cut-off: follow-up truncated for %d patient(s); %d event(s) occurring after the cut-off were reset to censored. Use the truncated follow-up time this analysis writes back, not the original follow-up column.",
                                n_trunc, n_reset), exempt_note)
                            # Say it where the user will see it. Deleting events from a
                            # survival dataset must never be reported only in a panel
                            # that defaults to off.
                            private$.addHtmlMessage(
                                "warning",
                                "Administrative censoring applied",
                                paste0(sprintf(paste0(
                                    "Follow-up was truncated for %d of %d patient(s) and %d of %d event(s) ",
                                    "occurring after the cut-off were reset to censored. Check that the cut-off ",
                                    "is in the same units as the follow-up time before using these numbers, and ",
                                    "pair the recoded outcome with the truncated follow-up column this analysis ",
                                    "writes back, not with your original follow-up column."),
                                    n_trunc, sum(keep), n_reset, n_events_before),
                                    exempt_note))
                            applied$admin <- TRUE
                            }
                        }
                    } else {
                        diagnostics$admin_censoring <- paste0(
                            "Administrative cut-off date read, but NOT applied: no Follow-up Time ",
                            "variable was selected, so no follow-up was truncated and no event ",
                            "status was reset. Select a Follow-up Time variable to apply it here.")
                        # A ticked checkbox that does nothing must not be visible only in
                        # a Diagnostics table that is off by default: the output is
                        # byte-identical to leaving the box unticked, so the user has no
                        # way to tell the cut-off was ignored.
                        private$.addHtmlMessage(
                            "warning",
                            "Administrative censoring not applied",
                            paste0(
                                "A cut-off date was supplied but no Follow-up Time variable was selected, ",
                                "so nothing was truncated and no event status was reset - these results are ",
                                "identical to leaving administrative censoring switched off. ",
                                "Select a Follow-up Time variable, measured in the same units as the cut-off."))
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

        # `codes_present` is the set of values the recode ACTUALLY produced. It
        # decides the label width; the options only choose the wording.
        #
        # Was: the branch was chosen from the `multievent` and `analysistype`
        # OPTIONS alone. The Censored/Event/Competing hand-off path fires with
        # multievent = FALSE, so re-running this analysis on the very column it
        # exports fell into the 2-level branch and labelled every competing event
        # "Unknown (2)" -- while the Summary asserted "Alive (other levels): coded
        # as 0". The file already knows to do this: its own comment at the export
        # path says "ask the returned VECTOR, not the options branch". That fix
        # reached `private$.causeFactor` and never reached the display.
        # A TIME COLUMN'S SCALE, DECIDED BY TYPE -- NOT BY MAGNITUDE.
        # Two callers need this: administrative censoring (comparing a follow-up
        # against a cut-off) and the interval-censoring endpoint check. Both were
        # written with `jmvcore::toNumeric()`, which is a NO-OP on a Date, a factor
        # or a character column -- it only unwraps a `values` attribute -- so a date
        # column silently became a day count and a text column silently became all-NA,
        # which turned the interval "runs backwards" check into dead code.
        #
        # `as.numeric()` on a FACTOR is the mirror trap: it returns LEVEL INDICES
        # (1, 2, 3...), not anything temporal.
        #
        # A magnitude heuristic cannot separate these. It was tried -- "refuse when
        # the cut-off is more than 10x the largest follow-up" -- and it fails in BOTH
        # directions: follow-up in DAYS against a date is only a 5x ratio, so a real
        # mismatch passed; while a registry whose administrative window is legitimately
        # in the same units was refused with a message telling the user to do what they
        # had already done. Classify the column instead, and return the kind alongside
        # the values so the caller can refuse a comparison across two different kinds.
        # An unset Level option is NULL; naming it "(not set)" beats printing an
        # empty pair of quotes in a message whose whole job is to say which
        # selection is wrong.
        .lvlOrUnset = function(x) if (is.null(x) || !length(x) || !nzchar(x)) "(not set)" else as.character(x),

        .timeScale = function(x) {
            if (inherits(x, "Date"))
                return(list(v = as.numeric(x), kind = "date"))
            if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
                # as.Date() on a POSIXct defaults to tz = "UTC", NOT the timestamp's
                # own zone, so an evening timestamp west of Greenwich rolls forward a
                # day: 2021-12-31 23:00 New York becomes 2022-01-01. A cut-off off by
                # one day silently moves every patient recorded that evening across it.
                tz <- attr(x, "tzone")
                tz <- if (is.null(tz) || !nzchar(tz[1])) Sys.timezone() else tz[1]
                if (is.na(tz)) tz <- "UTC"
                return(list(v = as.numeric(as.Date(x, tz = tz)), kind = "date"))
            }
            if (is.factor(x)) x <- as.character(x)
            if (is.character(x)) {
                n <- suppressWarnings(as.numeric(x))
                if (any(!is.na(n))) return(list(v = n, kind = "number"))
                # tryCatch, not suppressWarnings: as.Date.character ERRORS
                # ("character string is not in a standard unambiguous format")
                # when no format matches -- it does not return NA. Without this a
                # text column such as "N/A" or "31 Dec 2021" killed the whole
                # analysis, taking the recoded outcome with it, and the
                # "unreadable" branch below was dead code.
                d <- tryCatch(suppressWarnings(as.Date(x)),
                              error = function(e) rep(as.Date(NA), length(x)))
                if (any(!is.na(d))) return(list(v = as.numeric(d), kind = "date"))
                return(list(v = rep(NA_real_, length(x)), kind = "unreadable"))
            }
            list(v = suppressWarnings(as.numeric(jmvcore::toNumeric(x))), kind = "number")
        },

        .getOutcomeLabel = function(value, analysistype, multievent, codes_present = NULL) {
            val_str <- as.character(value)

            # A 2-level branch cannot describe a vector containing 2 or 3, whatever
            # the options say. Widen on the evidence.
            if (!is.null(codes_present)) {
                mx <- suppressWarnings(max(as.numeric(codes_present), na.rm = TRUE))
                if (is.finite(mx) && mx > 1) {
                    multievent <- TRUE
                    if (!analysistype %in% c('multistate', 'compete'))
                        analysistype <- if (mx >= 3) 'multistate' else 'compete'
                }
            }

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
            <div style='background-color: rgba(155, 155, 155, 0.06); padding: 15px; border-radius: 8px; color: inherit;'>
            <h4>Survival Analysis Glossary</h4>
            <dl>
                <dt><b>Overall Survival (OS)</b></dt>
                <dd>Time from diagnosis/treatment to death from any cause. Patients alive at last follow-up are censored.</dd>

                <dt><b>Cause-Specific Survival</b></dt>
                <dd>Time to death from the disease of interest. Deaths from other causes are censored (treated as non-events).</dd>

                <dt><b>Competing Risks</b></dt>
                <dd>Analysis accounting for multiple types of events (e.g., disease death vs. other death). Competing events prevent the event of interest from occurring.</dd>

                <dt><b>Recurrence-Free Survival (RFS)</b></dt>
                <dd>Time to disease recurrence or death. Used for cancers after curative treatment. Note that RFS is sometimes defined as counting only death <i>from disease</i>; this analysis counts death from <b>any</b> cause, because RFS runs on a single event level and the cause of death is not available to it. If you need other-cause deaths treated separately, use Cause-Specific Survival or Competing Risks with Multiple Event Levels.</dd>

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
                jmvcore::reject(.("Data contains no (complete) rows"))

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

                # REQUIRE AT LEAST ONE ASSIGNMENT, NOT ALL FOUR.
                # This used to block the run unless all four slots were filled, which
                # is a UI-slot check rather than a data check: a perfectly ordinary
                # 3-state registry outcome (Alive / Dead of disease / Dead of other
                # causes) has no fourth level to assign, so competing-risks and
                # cause-specific survival could not be run on it at all without
                # inventing a category. The shared coder .defineEventIndicator()
                # (R/survival_utils.R:126-208) already states the real contract --
                # at least one bucket filled, and every OBSERVED level assigned to
                # exactly one bucket -- and its own comment records that an empty
                # category "is perfectly normal and must not error". Defer to it:
                # it also produces the better message, naming the unmapped levels.
                assigned <- list(dod = self$options$dod, dooc = self$options$dooc,
                                 awd = self$options$awd, awod = self$options$awod)
                n_assigned <- sum(vapply(assigned,
                                         function(b) !is.null(b) && length(b) > 0, logical(1)))
                missing_levels <- character(0)
                if (n_assigned == 0)
                    missing_levels <- c("Dead of Disease", "Dead of Other Causes",
                                        "Alive with Disease", "Alive without Disease")

                if (length(missing_levels) > 0) {
                    # Add informative notice about available levels
                    private$.addNotice(jmvcore::NoticeType$INFO,
                        paste0("Outcome variable has ", length(unique_outcomes), " unique values: ",
                               paste(unique_outcomes, collapse = ", ")))

                    # Add strong warning about missing selections
                    private$.addNotice(jmvcore::NoticeType$STRONG_WARNING,
                        paste0("Multiple Event Types is enabled but no outcome level has been assigned to a category. ",
                               "Assign each level of your outcome variable to one of: ",
                               paste(missing_levels, collapse = ", "), ". ",
                               "A category with no patients in this cohort is fine and can be left empty - ",
                               "what matters is that every level present in the data is assigned somewhere."))

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

            # Add validation strong warnings, then warnings
            for (sw_msg in validation_results$strong_warnings) {
                private$.addNotice(jmvcore::NoticeType$STRONG_WARNING, sw_msg)
            }
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

            # THE SUMMARY MUST FOLLOW THE CODING TOO.
            # `codes_present` reached the output table, the plot and the copy-ready
            # text, but not this pane -- which has no `visible:` gate and is therefore
            # the one every user sees. On the hand-off path (.defineEventIndicator
            # returns 0/1/2 for a Censored/Event/Competing column regardless of
            # analysistype) it asserted "Alive (other levels): coded as 0" for a
            # vector containing 2s, and recommended Kaplan-Meier and Cox for a status
            # those functions cannot take.
            #
            # Computed HERE, above the branch, for two reasons: the recommendations
            # block further down reads it on every path (defining it inside one branch
            # made every other path die with "object 'handoff_coded' not found"), and
            # `codes_present` is defined ~100 lines later, so borrowing that instead
            # parses cleanly and then fails at runtime the same way.
            .codes_here <- unique(stats::na.omit(mydata[["myoutcome"]]))
            handoff_coded <- length(.codes_here) > 0 && any(!.codes_here %in% c(0, 1))

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
                if (handoff_coded) {
                    summary_text <- glue::glue(
                        "
                        <br><b>Competing-Risks Coding Detected</b><br>
                        The outcome supplied is already a multi-state coding, so it was recoded to:<br>
                        - Censored: coded as 0<br>
                        - Event of interest ({esc_outcomeLevel}): coded as 1<br>
                        - Competing event: coded as 2<br>
                        <br>
                        <i>This is NOT a two-level survival indicator. Kaplan-Meier and Cox treat any non-zero status as the same event, so use a competing-risks method (cumulative incidence, Fine-Gray) instead.</i>
                        "
                    )
                } else if (analysistype == 'os') {
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
                # The Diagnostics table already says the endpoint columns are not
                # written back, but it is OFF BY DEFAULT. This pane is always visible,
                # so on its own it read as "interval-censored data has been prepared
                # for you" when the only column this analysis exports is the recoded
                # outcome -- and pairing that outcome with an exact follow-up time is
                # precisely the analysis interval censoring exists to avoid.
                summary_text <- paste(summary_text, "<br><b>Interval Censoring:</b> Events are known to occur within time intervals rather than at exact times. Your two interval columns were checked but are <b>not</b> written back - this analysis exports only the recoded outcome - so build the response yourself with Surv(&lt;start&gt;, &lt;end&gt;, type='interval2') using your original interval columns.<br>")
            }

            if (isTRUE(applied$admin)) {
                summary_text <- paste(summary_text, "<br><b>Administrative Censoring:</b> Observations are censored at a specified administrative date.<br>")
            }

            # Add recommendations for appropriate analyses
            summary_text <- paste(summary_text, "<br><b>Recommended Analysis Approaches:</b><br>")

            # Recommend for the coding that was PRODUCED, not the option that was
            # chosen. On the hand-off path a 0/1/2 status came out under
            # analysistype = "os" and this block told the reader to run Kaplan-Meier
            # and Cox -- which treat any non-zero status as the same event, silently
            # merging competing deaths into the event of interest.
            if (handoff_coded) {
                summary_text <- paste(summary_text, "- Cumulative incidence function (competing risks)<br>- Fine-Gray subdistribution hazard model<br>- Do NOT use Kaplan-Meier or Cox on this status: they treat every non-zero code as the same event, merging the competing events into the event of interest<br>")
            } else if (analysistype == 'os') {
                summary_text <- paste(summary_text, "- Kaplan-Meier method for univariate analysis<br>- Cox proportional hazards for multivariable analysis<br>")
            } else if (analysistype == 'cause') {
                summary_text <- paste(summary_text, "- Cause-specific hazard models (standard Cox regression)<br>- Cumulative incidence function with competing risks<br>")
            } else if (analysistype == 'compete') {
                summary_text <- paste(summary_text, "- Fine-Gray subdistribution hazard model<br>- Cumulative incidence function accounting for competing risks<br>")
            } else if (analysistype == 'multistate') {
                summary_text <- paste(summary_text, "- A per-patient state code (0 = disease-free, 1 = disease, 2 = death from disease, 3 = death from other causes)<br>- Note: fitting a multistate or illness-death model requires transition times and a subject identifier in long format, which this analysis does not export<br>")
            } else if (analysistype %in% c('rfs', 'pfs', 'dfs')) {
                summary_text <- paste(summary_text,
                    if (is.null(self$options$recurrence))
                        "- NOTE: no recurrence/progression variable was supplied, so the composite endpoint was NOT built and this status is a death-only indicator. Supply the recurrence variable, or report this as overall survival<br>"
                    else "- Standard survival analysis (Kaplan-Meier, Cox)<br>- Consider competing risks if appropriate<br>")
            } else if (analysistype == 'ttp') {
                summary_text <- paste(summary_text,
                    if (is.null(self$options$recurrence))
                        "- NOTE: no recurrence/progression variable was supplied, so progression could not be identified and this status is a death-only indicator - the opposite of time to progression, which censors death. Supply the recurrence variable<br>"
                    else "- Standard survival analysis with death as censoring<br>- Consider sensitivity analysis treating death as competing risk<br>")
            }

            # Summary now only contains analysis description (validation moved to Notices)
            self$results$summary$setContent(summary_text)

            # Frequency table of recoded outcomes. Computed unconditionally
            # because both the output table and the visualization state consume
            # it; previously it was defined only inside the outputTable block,
            # so enabling the visualization without the table crashed with
            # "object 'outcome_counts' not found".
            outcome_counts <- table(mydata$myoutcome)
            # The codes the recode ACTUALLY produced. Every label and every
            # 'is this multi-state' decision below keys off this, not off the
            # options -- the two disagree whenever the hand-off path fires.
            codes_present <- names(outcome_counts)

            # Add data table if requested
            if (self$options$outputTable) {
                outcome_table <- self$results$outputTable
                # Both of these tables are `rows: 0` and are filled with addRow()
                # in .run(). addRow() does NOT check for a duplicate rowKey, so a
                # re-run that clearWith did not clear appends a second set and the
                # table dies with "duplicate 'row.names' are not allowed". A complete
                # clearWith (now in the .r.yaml) is the primary fix; clearing here
                # is the belt-and-braces half, because the row set depends on the
                # DATA (one row per distinct outcome code), not only on options.
                outcome_table$deleteRows()

                # Add rows for each unique outcome value
                for (i in seq_along(outcome_counts)) {
                    value <- names(outcome_counts)[i]
                    count <- outcome_counts[i]
                    label <- private$.getOutcomeLabel(value, analysistype, self$options$multievent, codes_present)

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
                diagnostics_table$deleteRows()   # see the note on outputTable above

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
                # codes_present travels with the state: the renderer decides its
                # labels from the codes the recode actually produced, exactly as the
                # table and the summary do. Without it the plot kept the old
                # options-driven labelling and disagreed with the table beside it.
                image$setState(list(
                    "table" = outcome_counts,
                    "analysis_type" = analysistype,
                    "multi_event" = self$options$multievent,
                    "codes_present" = codes_present
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
            # The truncated follow-up time, written only when the cut-off actually
            # applied. Gated on isNotFilled() alone, never on self$options$addAdminTime:
            # an Output option is not an argument of the generated R wrapper, so it is
            # permanently FALSE from the R API and gating on it would make the column
            # unreachable headless. jamovi already gates delivery via Output$enabled.
            if (!is.null(private$.adminTime) &&
                self$results$addAdminTime$isNotFilled()) {
                self$results$addAdminTime$setRowNums(df_outcome$row_names)
                self$results$addAdminTime$setValues(as.numeric(private$.adminTime))
            }

            # Same gate as addAdminTime above: isNotFilled() only, never
            # self$options$addOutcome (unreachable from the R wrapper).
            if (self$results$addOutcome$isNotFilled()) {
                # The .r.yaml varTitle is a STATIC string. It used to interpolate
                # `{analysistype}`, which substitutes the List option's raw KEY, so
                # the column landed in the user's spreadsheet called "Recoded Outcome
                # for os Survival Analysis". Name it properly here, where the human
                # label is available.
                self$results$addOutcome$setTitle(sprintf(
                    "Recoded Outcome (%s)",
                    switch(analysistype,
                           os = "overall survival", cause = "cause-specific survival",
                           compete = "competing risks", rfs = "recurrence-free survival",
                           pfs = "progression-free survival", dfs = "disease-free survival",
                           ttp = "time to progression", multistate = "multistate",
                           analysistype)))
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
                # These describe what the coding MEANS, so they must follow what was
                # actually coded, not the analysis type alone. Two ways they diverge,
                # both silent before:
                #  * the composite branches (rfs/pfs/dfs, ttp) are skipped when no
                #    recurrence variable is supplied, leaving a plain death
                #    indicator that this text still called "recurrence/progression
                #    or death";
                #  * the Censored/Event/Competing hand-off path produces a 3-level
                #    coding under analysistype "os", which this text described as a
                #    two-level all-cause outcome and left the competing events out
                #    of the narrative entirely.
                composite_built <- analysistype %in% c("rfs", "pfs", "dfs", "ttp") &&
                                   !is.null(self$options$recurrence)
                multi_coded <- any(!codes_present %in% c("0", "1"))

                # Multistate has no "event" in the two-group sense at all: 0/1/2/3 are
                # four clinical STATES, and counting myoutcome == 1 makes every
                # "alive with disease" patient an event. The chain had no multistate
                # arm, so it fell through to the placeholder "the selected event type"
                # and produced a headline sentence that was simply wrong.
                event_desc <- if (analysistype == "multistate")
                        "transition to the disease state (1); this is a multistate coding with four states, not a single event indicator - the per-state breakdown below is the meaningful summary"
                    else if (analysistype %in% c("rfs", "pfs", "dfs") && !composite_built)
                        "death (no recurrence variable was supplied, so the composite endpoint was not built and this is a death-only indicator)"
                    else if (analysistype == "ttp" && !composite_built)
                        "death (no recurrence variable was supplied, so progression could not be identified and this is a death-only indicator)"
                    else if (analysistype == "os" && multi_coded)
                        "the event of interest (coded 1); this outcome also carries competing states, listed below"
                    else if (analysistype == "os") "death from any cause"
                    else if (analysistype == "cause") "death from the disease of interest"
                    else if (analysistype == "compete") "disease-specific death (competing events coded as 2)"
                    else if (analysistype %in% c("rfs", "pfs", "dfs")) "recurrence/progression or death"
                    else if (analysistype == "ttp") "disease progression only"
                    else "the selected event type"

                # Censor description
                # Was hard-coded to "patients who remain alive or event-free" for every
                # type but TTP. Under cause-specific and competing risks the 0 group is
                # NOT event-free: it contains patients who died of something else, and
                # calling them event-free in a manuscript sentence is a factual error.
                censor_desc <- if (analysistype == "ttp" && composite_built)
                        "patients who died without progression or remain event-free"
                    else if (analysistype == "cause")
                        "patients who were alive at last contact or died of another cause (both censored for this endpoint)"
                    else if (analysistype == "compete")
                        "patients alive at last contact; deaths from other causes are coded 2 as competing events, not censored"
                    else if (analysistype == "multistate")
                        "patients in the baseline state (0), alive without disease"
                    else "patients who remain alive or event-free"

                # Calculate frequencies. The denominator must be ALL non-missing coded
                # records: for competing risks (codes 0/1/2) and multistate (0/1/2/3)
                # the events (==1) plus censored (==0) counts do NOT sum to the total,
                # so using n_events + n_censored silently drops the competing/other-cause
                # states and reports percentages against a wrong total. For binary OS
                # coding this is identical to the previous denominator.
                # Under multistate, code 1 is "alive with disease" -- a state, not an
                # event -- so this count is not an event count and must not be
                # presented as one. The per-state breakdown below carries the real
                # information; the headline numbers are labelled accordingly.
                n_events <- sum(mydata$myoutcome == 1, na.rm = TRUE)
                n_censored <- sum(mydata$myoutcome == 0, na.rm = TRUE)
                count_noun <- if (analysistype == "multistate")
                    "patients in the disease state" else "events"
                total_n <- sum(!is.na(mydata$myoutcome))
                event_pct <- if (total_n > 0) round(n_events / total_n * 100, 1) else 0
                censor_pct <- if (total_n > 0) round(n_censored / total_n * 100, 1) else 0

                # For competing-risks / multistate coding, append an explicit per-state
                # breakdown so the copy-ready text does not imply the non-event group
                # accounts for every remaining patient.
                state_breakdown <- ""
                # Fire on what was CODED, not on the option: a 3-level coding needs its
                # state breakdown however the analysis type is labelled.
                if (analysistype %in% c("compete", "multistate") ||
                    any(!codes_present %in% c("0", "1"))) {
                    state_tab <- table(mydata$myoutcome)
                    state_lines <- vapply(names(state_tab), function(v) {
                        cnt <- as.integer(state_tab[[v]])
                        pct <- if (total_n > 0) round(cnt / total_n * 100, 1) else 0
                        sprintf("%s: %d (%.1f%%)",
                                htmltools::htmlEscape(private$.getOutcomeLabel(v, analysistype, self$options$multievent, codes_present)),
                                cnt, pct)
                    }, character(1))
                    state_breakdown <- paste0(
                        " Full state breakdown (of ", total_n, " coded records): ",
                        paste(state_lines, collapse = "; "), "."
                    )
                }

                natural_summary <- sprintf(
                    "<div style='background-color: rgba(33, 144, 255, 0.11); padding: 15px; border-radius: 8px; margin: 10px 0; color: inherit;'>
                    <b> Copy-Ready Report Text:</b><br><br>
                    The outcome variable '<b>%s</b>' was recoded for <b>%s</b> analysis.
                    Code 1 represents %s.
                    Code 0 represents %s.
                    The recoded variable '<b>myoutcome</b>' contains <b>%d %s (%.1f%%)</b> and <b>%d coded 0 (%.1f%%)</b> out of %d coded records.%s
                    </div>",
                    htmltools::htmlEscape(self$options$outcome),
                    analysis_type_labels[[analysistype]],
                    event_desc,
                    censor_desc,
                    n_events, count_noun, event_pct,
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
                    private$.getOutcomeLabel(val, plotData$analysis_type, plotData$multi_event, plotData$codes_present)
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
