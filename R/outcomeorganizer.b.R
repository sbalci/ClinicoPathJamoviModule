#' @title Enhanced Outcome Organizer for Survival Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#'

outcomeorganizerClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "outcomeorganizerClass",
    inherit = outcomeorganizerBase,
    private = list(
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

        # getData function to properly handle the data labels
        .getData = function() {
            # Get the data
            mydata <- self$data

            # Add row names if missing
            if (is.null(rownames(mydata))) {
                mydata$row_names <- seq_len(nrow(mydata))
            } else {
                mydata$row_names <- rownames(mydata)
            }

            # Get original names
            original_names <- names(mydata)

            # Create labels vector
            labels <- stats::setNames(original_names, original_names)

            # Clean names safely
            mydata_cleaned <- try({
                janitor::clean_names(mydata)
            }, silent = TRUE)

            if (inherits(mydata_cleaned, "try-error")) {
                stop('Error cleaning variable names. Please check column names.')
            }

            # Create corrected labels
            corrected_labels <- stats::setNames(original_names, names(mydata_cleaned))

            # Apply labels
            mydata_labelled <- try({
                labelled::set_variable_labels(.data = mydata_cleaned, .labels = corrected_labels)
            }, silent = TRUE)

            if (inherits(mydata_labelled, "try-error")) {
                stop('Error setting variable labels')
            }

            # Get all labels
            all_labels <- labelled::var_label(mydata_labelled)

            # Get variable names from labels
            outcome_var <- try({
                names(all_labels)[all_labels == self$options$outcome]
            }, silent = TRUE)

            # Get recurrence/progression variable if specified
            recurrence_var <- NULL
            if (!is.null(self$options$recurrence)) {
                recurrence_var <- try({
                    names(all_labels)[all_labels == self$options$recurrence]
                }, silent = TRUE)
            }

            # Get patient ID variable if specified
            id_var <- NULL
            if (!is.null(self$options$patientID)) {
                id_var <- try({
                    names(all_labels)[all_labels == self$options$patientID]
                }, silent = TRUE)
            }

            return(list(
                "mydata_labelled" = mydata_labelled,
                "outcome_var" = outcome_var,
                "recurrence_var" = recurrence_var,
                "id_var" = id_var
            ))
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
                contin <- c("integer", "numeric", "double")

                if (inherits(outcome1, contin)) {
                    # Check if it's binary (0 and 1)
                    unique_vals <- unique(outcome1[!is.na(outcome1)])
                    if (!((length(unique_vals) == 2) && (sum(unique_vals) == 1))) {
                        warning_msg <- 'Continuous outcome variable does not contain only 0s and 1s. This may lead to incorrect analysis.'
                        warning(warning_msg)
                        diagnostics$binary_check <- warning_msg
                    }

                    # Simply copy the outcome as it's already coded
                    mydata[["myoutcome"]] <- mydata[[outcome_var]]

                } else if (inherits(outcome1, c("factor", "character"))) {
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

                # Initialize all as NA
                mydata[["myoutcome"]] <- NA_integer_

                if (analysistype == 'overall') {
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

                # Apply event hierarchy if specified
                if (self$options$useHierarchy) {
                    # If multiple events could be coded for the same patient, apply hierarchy
                    highest_priority <- self$options$eventPriority
                    if (!is.null(id_var) && !is.null(highest_priority)) {
                        # Group by patient ID and apply hierarchy
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

                        diagnostics$hierarchy <- paste("Event hierarchy applied, priority:", highest_priority)
                    }
                }
            }

            # Apply interval censoring if specified
            if (self$options$intervalCensoring && !is.null(self$options$intervalStart) && !is.null(self$options$intervalEnd)) {
                # This would require more complex handling that would depend on the specific survival analysis package
                # For now, just note that interval censoring was requested
                diagnostics$interval_censoring <- "Interval censoring requested but requires specialized analysis"
            }

            # Handle administrative censoring if specified
            if (self$options$adminCensoring && !is.null(self$options$adminDate)) {
                # Mark observations censored after the administrative censoring date
                admin_date_var <- labelled_data[["adminDate"]]
                if (!is.null(admin_date_var)) {
                    admin_date <- mydata[[admin_date_var]]
                    end_date_var <- ifelse(!is.null(labelled_data$fudate), labelled_data$fudate, NULL)

                    if (!is.null(end_date_var)) {
                        end_date <- mydata[[end_date_var]]
                        # If end date is after admin date, censor at admin date
                        admin_censored <- end_date > admin_date
                        mydata[["myoutcome"]][admin_censored] <- 0

                        diagnostics$admin_censoring <- paste(sum(admin_censored), "observations administratively censored")
                    }
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
            # Initial validation
            if (is.null(self$options$outcome)) {
                private$.todo()
                return()
            }

            if (nrow(self$data) == 0)
                stop('Data contains no (complete) rows')

            # Create table if needed
            private$.checkpoint()

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
                if (analysistype == 'overall') {
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

            if (analysistype == 'overall' || analysistype == 'os') {
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

            self$results$summary$setContent(summary_text)

            # Add data table if requested
            if (self$options$outputTable) {
                # Create frequency table of new outcomes
                outcome_counts <- table(mydata$myoutcome)
                outcome_table <- self$results$outputTable

                # Create labels for outcome values
                outcome_labels <- function(value) {
                    if (self$options$multievent && analysistype == 'multistate') {
                        switch(value,
                               "0" = "Disease-free (0)",
                               "1" = "Disease state (1)",
                               "2" = "Death from disease (2)",
                               "3" = "Death from other causes (3)",
                               paste("Unknown (", value, ")", sep=""))
                    } else if (self$options$multievent && analysistype == 'compete') {
                        switch(value,
                               "0" = "Censored (0)",
                               "1" = "Disease event (1)",
                               "2" = "Competing event (2)",
                               paste("Unknown (", value, ")", sep=""))
                    } else {
                        switch(value,
                               "0" = "Censored (0)",
                               "1" = "Event (1)",
                               paste("Unknown (", value, ")", sep=""))
                    }
                }

                # Add rows for each unique outcome value
                for (i in seq_along(outcome_counts)) {
                    value <- names(outcome_counts)[i]
                    count <- outcome_counts[i]
                    label <- outcome_labels(value)

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
        },

        # Plot function for outcome distribution visualization
        .plotOutcome = function(image, ggtheme, theme, ...) {
            if (!self$options$visualization)
                return()

            plotData <- image$state

            if (is.null(plotData))
                return()

            # Create data frame from outcome counts
            plot_df <- data.frame(
                Outcome = names(plotData$table),
                Count = as.numeric(plotData$table)
            )

            # Add proper labels based on analysis type
            if (plotData$multi_event && plotData$analysis_type == 'multistate') {
                plot_df$Label <- c("Disease-free", "Disease state", "Death - disease", "Death - other")[as.numeric(plot_df$Outcome) + 1]
            } else if (plotData$multi_event && plotData$analysis_type == 'compete') {
                plot_df$Label <- c("Censored", "Disease event", "Competing event")[as.numeric(plot_df$Outcome) + 1]
            } else {
                plot_df$Label <- c("Censored", "Event")[as.numeric(plot_df$Outcome) + 1]
            }

            # Create the plot
            plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Label, y = Count, fill = Label)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::geom_text(ggplot2::aes(label = Count), vjust = -0.5) +
                ggplot2::labs(
                    title = "Distribution of Recoded Outcome Values",
                    x = "Outcome Category",
                    y = "Count"
                ) +
                ggtheme +
                ggplot2::theme(legend.position = "none")

            print(plot)
            TRUE
        }
    )
)
