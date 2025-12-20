
#' @title Survival Endpoint Derivation
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import survival
#' @import dplyr
#' @import ggplot2
#' @importFrom survminer ggsurvplot
#' @export


survivalendpointsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "survivalendpointsClass",
    inherit = survivalendpointsBase,
    private = list(

        #---------------------------------------------
        # INITIALIZATION
        #---------------------------------------------

        .init = function() {

            # Initialize instructions
            self$results$instructions$setContent(
                "<html>
                <head>
                <style>
                    .guide {
                        font-family: Arial, sans-serif;
                        max-width: 800px;
                        line-height: 1.6;
                        padding: 15px;
                    }
                    .guide h3 {
                        color: #2c3e50;
                        border-bottom: 2px solid #3498db;
                        padding-bottom: 5px;
                    }
                    .guide .section {
                        margin: 15px 0;
                        padding: 10px;
                        background-color: #f8f9fa;
                        border-left: 4px solid #3498db;
                    }
                    .guide .endpoint {
                        margin: 10px 0;
                        padding: 8px;
                        background-color: #ffffff;
                        border: 1px solid #dee2e6;
                    }
                    .guide .endpoint-name {
                        font-weight: bold;
                        color: #2c3e50;
                    }
                    .guide .formula {
                        font-family: monospace;
                        background-color: #f1f3f5;
                        padding: 2px 5px;
                        border-radius: 3px;
                    }
                </style>
                </head>
                <body>
                <div class='guide'>
                    <h3>Survival Endpoint Derivation Module</h3>

                    <div class='section'>
                        <strong>Purpose:</strong> This module derives standard survival endpoints (PFS, OS, TTP, DOR)
                        from clinical trial timeline data for use in survival analysis.
                    </div>

                    <div class='section'>
                        <strong>Required Variables:</strong>
                        <ul>
                            <li><strong>Patient ID:</strong> Unique patient identifier</li>
                            <li><strong>Treatment Start:</strong> Baseline date or time point</li>
                            <li><strong>Last Follow-up:</strong> Last known contact date/time</li>
                            <li><strong>Events:</strong> Death and/or progression indicators (0=no, 1=yes)</li>
                        </ul>
                    </div>

                    <div class='section'>
                        <strong>Survival Endpoints:</strong>

                        <div class='endpoint'>
                            <span class='endpoint-name'>PFS (Progression-Free Survival):</span><br/>
                            Time from start to progression OR death (whichever occurs first)
                        </div>

                        <div class='endpoint'>
                            <span class='endpoint-name'>OS (Overall Survival):</span><br/>
                            Time from start to death (alive patients are censored)
                        </div>

                        <div class='endpoint'>
                            <span class='endpoint-name'>TTP (Time to Progression):</span><br/>
                            Time from start to progression (death without progression is censored)
                        </div>

                        <div class='endpoint'>
                            <span class='endpoint-name'>DOR (Duration of Response):</span><br/>
                            Time from confirmed response to progression (requires response date)
                        </div>
                    </div>

                    <div class='section'>
                        <strong>Next Steps:</strong>
                        <ol>
                            <li>Select your timeline variables and event indicators</li>
                            <li>Choose which endpoints to calculate</li>
                            <li>Review the derived endpoints table</li>
                            <li>Optionally export to CSV for use in survival analysis modules</li>
                        </ol>
                    </div>
                </div>
                </body>
                </html>"
            )

            # Initialize usage guide
            if (self$options$showUsageGuide) {
                self$results$usageGuide$setContent(
                    private$.generateUsageGuide()
                )
            }

        },

        #---------------------------------------------
        # MAIN RUN FUNCTION
        #---------------------------------------------

        .run = function() {

            # Check for required variables
            if (is.null(self$options$patientId) ||
                is.null(self$options$startDate) ||
                is.null(self$options$lastFollowup)) {

                # Build list of missing variables for single-line notice
                missingVars <- c()
                if (is.null(self$options$patientId)) missingVars <- c(missingVars, 'Patient ID')
                if (is.null(self$options$startDate)) missingVars <- c(missingVars, 'Treatment Start Date')
                if (is.null(self$options$lastFollowup)) missingVars <- c(missingVars, 'Last Follow-up Date')

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'missingRequiredVars',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(
                    sprintf('Required variables missing: %s • Please select all required variables to derive survival endpoints.',
                            paste(missingVars, collapse=', '))
                )
                self$results$insert(1, notice)
                return()
            }

            # Get data
            data <- self$data

            # Check for empty dataset
            if (nrow(data) == 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'emptyDataset',
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent('No data available • Dataset contains no rows • Please load data to derive survival endpoints.')
                self$results$insert(1, notice)
                return()
            }

            # Extract variables (keep patientId as character to preserve alphanumeric IDs)
            patientId <- as.character(data[[self$options$patientId]])
            startDate <- data[[self$options$startDate]]
            lastFollowup <- data[[self$options$lastFollowup]]

            # Check if we have any endpoints selected
            if (!self$options$calculatePFS && !self$options$calculateOS &&
                !self$options$calculateTTP && !self$options$calculateDOR &&
                !self$options$calculateTOT) {

                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'noEndpointsSelected',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent('No survival endpoints selected • Please select at least one endpoint to calculate: PFS (Progression-Free Survival), OS (Overall Survival), TTP (Time to Progression), DOR (Duration of Response), or Time on Treatment.')
                self$results$insert(1, notice)
                return()
            }

            # Convert dates if needed
            if (self$options$inputType == 'dates') {
                startDate <- private$.convertToDate(startDate)
                lastFollowup <- private$.convertToDate(lastFollowup)
            }

            # Extract event dates if provided
            progressionDate <- NULL
            deathDate <- NULL

            if (!is.null(self$options$progressionDate)) {
                progressionDate <- data[[self$options$progressionDate]]
                if (self$options$inputType == 'dates') {
                    progressionDate <- private$.convertToDate(progressionDate)
                }
            }

            if (!is.null(self$options$deathDate)) {
                deathDate <- data[[self$options$deathDate]]
                if (self$options$inputType == 'dates') {
                    deathDate <- private$.convertToDate(deathDate)
                }
            }

            # Extract event indicators (used if dates not provided)
            progressionEvent <- NULL
            deathEvent <- NULL

            if (!is.null(self$options$progressionEvent)) {
                progressionEvent <- jmvcore::toNumeric(data[[self$options$progressionEvent]])
            }

            if (!is.null(self$options$deathEvent)) {
                deathEvent <- jmvcore::toNumeric(data[[self$options$deathEvent]])
            }

            # Create base data frame
            derivedData <- data.frame(
                patientId = as.character(patientId),
                stringsAsFactors = FALSE
            )

            # Calculate each endpoint with CORRECT time-to-event calculations
            if (self$options$calculatePFS) {
                pfs <- private$.calculatePFS(startDate, lastFollowup,
                                            progressionDate, deathDate,
                                            progressionEvent, deathEvent)
                derivedData$pfs_time <- pfs$time
                derivedData$pfs_event <- pfs$event
            }

            if (self$options$calculateOS) {
                os <- private$.calculateOS(startDate, lastFollowup,
                                          deathDate, deathEvent)
                derivedData$os_time <- os$time
                derivedData$os_event <- os$event
            }

            if (self$options$calculateTTP) {
                ttp <- private$.calculateTTP(startDate, lastFollowup,
                                            progressionDate, progressionEvent, deathDate)
                derivedData$ttp_time <- ttp$time
                derivedData$ttp_event <- ttp$event
            }

            if (self$options$calculateDOR && !is.null(self$options$responseDate)) {
                responseDate <- data[[self$options$responseDate]]
                if (self$options$inputType == 'dates') {
                    responseDate <- private$.convertToDate(responseDate)
                }

                dor <- private$.calculateDOR(responseDate, lastFollowup,
                                            progressionDate, progressionEvent)
                derivedData$dor_time <- dor$time
                derivedData$dor_event <- dor$event
            }

            if (self$options$calculateTOT && !is.null(self$options$treatmentEnd)) {
                treatmentEnd <- data[[self$options$treatmentEnd]]
                if (self$options$inputType == 'dates') {
                    treatmentEnd <- private$.convertToDate(treatmentEnd)
                }

                tot <- private$.calculateTOT(startDate, treatmentEnd)
                derivedData$tot_time <- tot$time
            }

            # Store derived data for use in other functions

            # -----------------------------------------------------------
            # QUALITY CHECKS AND WARNINGS - Using Notices
            # -----------------------------------------------------------
            noticePosition <- 500  # Mid-range position for warnings

            # 1. Check for negative times (DATA ERROR - must stop analysis)
            time_cols <- grep("_time", names(derivedData), value = TRUE)
            if (length(time_cols) > 0) {
                total_neg <- 0
                for (col in time_cols) {
                    neg_count <- sum(derivedData[[col]] < 0, na.rm = TRUE)
                    if (neg_count > 0) {
                        total_neg <- total_neg + neg_count
                    }
                }

                if (total_neg > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'negativeTimesDetected',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(
                        sprintf('Data error: %d observations have event dates BEFORE start dates • This indicates incorrect date entry • Please fix your data before proceeding with analysis.', total_neg)
                    )
                    self$results$insert(1, notice)
                    return()  # STOP analysis - data must be corrected
                }
            }

            # 2. Check for Imputed Events (Event = 1 but Date is Missing)
            # PFS / TTP Imputation
            if ((self$options$calculatePFS || self$options$calculateTTP) &&
                !is.null(progressionEvent)) {

                if (is.null(progressionDate)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'progressionDateNotProvided',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent('Progression Date not provided • Progression events without dates were timed to Last Follow-up • Provide progression dates for accurate PFS/TTP.')
                    self$results$insert(noticePosition, notice)
                    noticePosition <- noticePosition + 1
                } else {
                    imputed_prog <- sum(progressionEvent == 1 & is.na(progressionDate), na.rm = TRUE)
                    if (imputed_prog > 0) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'progressionDateMissing',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(
                            sprintf('Progression Date missing for %d patient(s) with progression event • Time imputed using Last Follow-up Date • This may overestimate time-to-event.', imputed_prog)
                        )
                        self$results$insert(noticePosition, notice)
                        noticePosition <- noticePosition + 1
                    }
                }
            }

            # OS Imputation
            if ((self$options$calculateOS || self$options$calculatePFS) &&
                !is.null(deathEvent)) {

                if (is.null(deathDate)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'deathDateNotProvided',
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent('Death Date not provided • Death events without dates were timed to Last Follow-up • Provide death dates for accurate OS/PFS.')
                    self$results$insert(noticePosition, notice)
                    noticePosition <- noticePosition + 1
                } else {
                    imputed_death <- sum(deathEvent == 1 & is.na(deathDate), na.rm = TRUE)
                    if (imputed_death > 0) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = 'deathDateMissing',
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(
                            sprintf('Death Date missing for %d patient(s) with death event • Time imputed using Last Follow-up Date.', imputed_death)
                        )
                        self$results$insert(noticePosition, notice)
                        noticePosition <- noticePosition + 1
                    }
                }
            }
            
            private$.derivedData <- derivedData

            # Populate data info table
            private$.populateDataInfo(derivedData)

            # Populate derived endpoints table
            if (self$options$showDerivedData) {
                private$.populateDerivedTable(derivedData)
            }

            # Populate summary statistics
            if (self$options$showSummaryStats) {
                private$.populateSummaryStats(derivedData)
            }

            # Populate event rates
            if (self$options$showEventRates) {
                private$.populateEventRates(derivedData)
            }

            # Populate milestone analysis
            if (self$options$showMilestones) {
                private$.populateMilestones(derivedData)
            }

            # Export if requested
            if (self$options$exportEndpoints) {
                private$.exportEndpoints(derivedData)
            }

            # Add success notice summarizing derived endpoints
            endpointsList <- c()
            if (self$options$calculatePFS) endpointsList <- c(endpointsList, 'PFS')
            if (self$options$calculateOS) endpointsList <- c(endpointsList, 'OS')
            if (self$options$calculateTTP) endpointsList <- c(endpointsList, 'TTP')
            if (self$options$calculateDOR) endpointsList <- c(endpointsList, 'DOR')
            if (self$options$calculateTOT) endpointsList <- c(endpointsList, 'Time on Treatment')

            if (length(endpointsList) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisComplete',
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(
                    sprintf('Analysis complete: Successfully derived %s for %d patients • Variables created and ready for survival analysis.',
                            paste(endpointsList, collapse=', '), nrow(derivedData))
                )
                self$results$insert(999, notice)
            }

        },

        #---------------------------------------------
        # ENDPOINT CALCULATION FUNCTIONS (CLINICALLY CORRECT)
        #---------------------------------------------

        .calculatePFS = function(startDate, lastFollowup, progressionDate, deathDate,
                                 progressionEvent, deathEvent) {
            # PFS: time to FIRST event (progression OR death, whichever occurs first)
            n <- length(startDate)
            time <- rep(NA_real_, n)
            event <- rep(0, n)

            for (i in 1:n) {
                # Determine event times
                prog_time <- NA
                death_time <- NA

                # Get progression time
                if (!is.null(progressionDate) && !is.na(progressionDate[i])) {
                    prog_time <- private$.calculateTimeDiff(startDate[i], progressionDate[i])
                } else if (!is.null(progressionEvent) && !is.na(progressionEvent[i]) && progressionEvent[i] == 1) {
                    # If only event indicator, assume event at last followup (legacy support)
                    prog_time <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                }

                # Get death time
                if (!is.null(deathDate) && !is.na(deathDate[i])) {
                    death_time <- private$.calculateTimeDiff(startDate[i], deathDate[i])
                } else if (!is.null(deathEvent) && !is.na(deathEvent[i]) && deathEvent[i] == 1) {
                    # If only event indicator, assume event at last followup (legacy support)
                    death_time <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                }

                # Time to last followup (for censoring)
                followup_time <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])

                # PFS = time to FIRST event (progression or death)
                if (!is.na(prog_time) && !is.na(death_time)) {
                    # Both events occurred - take earliest
                    time[i] <- min(prog_time, death_time, na.rm = TRUE)
                    event[i] <- 1
                } else if (!is.na(prog_time)) {
                    # Only progression occurred
                    time[i] <- prog_time
                    event[i] <- 1
                } else if (!is.na(death_time)) {
                    # Only death occurred
                    time[i] <- death_time
                    event[i] <- 1
                } else {
                    # No event - censored at last followup
                    time[i] <- followup_time
                    event[i] <- 0
                }
            }

            list(time = time, event = event)
        },

        .calculateOS = function(startDate, lastFollowup, deathDate, deathEvent) {
            # OS: time to death (alive patients censored at last followup)
            n <- length(startDate)
            time <- rep(NA_real_, n)
            event <- rep(0, n)

            for (i in 1:n) {
                # Check if death occurred
                if (!is.null(deathDate) && !is.na(deathDate[i])) {
                    # Use actual death date
                    time[i] <- private$.calculateTimeDiff(startDate[i], deathDate[i])
                    event[i] <- 1
                } else if (!is.null(deathEvent) && !is.na(deathEvent[i]) && deathEvent[i] == 1) {
                    # Legacy support: event indicator without date
                    time[i] <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                    event[i] <- 1
                } else {
                    # Alive - censored at last followup
                    time[i] <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                    event[i] <- 0
                }
            }

            list(time = time, event = event)
        },

        .calculateTTP = function(startDate, lastFollowup, progressionDate, progressionEvent, deathDate = NULL) {
            # TTP: time to progression (death WITHOUT progression is censored)
            n <- length(startDate)
            time <- rep(NA_real_, n)
            event <- rep(0, n)

            for (i in 1:n) {
                # Check if progression occurred
                if (!is.null(progressionDate) && !is.na(progressionDate[i])) {
                    # Use actual progression date
                    time[i] <- private$.calculateTimeDiff(startDate[i], progressionDate[i])
                    event[i] <- 1
                } else if (!is.null(progressionEvent) && !is.na(progressionEvent[i]) && progressionEvent[i] == 1) {
                    # Legacy support: event indicator without date
                    time[i] <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                    event[i] <- 1
                } else {
                    # No progression - censored at last follow-up (TTP standard)
                    # Per FDA/EMA guidance: death without progression is censored at last follow-up
                    time[i] <- private$.calculateTimeDiff(startDate[i], lastFollowup[i])
                    event[i] <- 0
                }
            }

            list(time = time, event = event)
        },

        .calculateDOR = function(responseDate, lastFollowup, progressionDate, progressionEvent) {
            # DOR: time from confirmed response to progression (for responders only)
            n <- length(responseDate)
            time <- rep(NA_real_, n)
            event <- rep(NA_integer_, n)

            for (i in 1:n) {
                # Only calculate for patients with a response
                if (is.na(responseDate[i])) {
                    time[i] <- NA
                    event[i] <- NA
                    next
                }

                # Check if progression occurred
                if (!is.null(progressionDate) && !is.na(progressionDate[i])) {
                    # Use actual progression date
                    time[i] <- private$.calculateTimeDiff(responseDate[i], progressionDate[i])
                    event[i] <- 1
                } else if (!is.null(progressionEvent) && !is.na(progressionEvent[i]) && progressionEvent[i] == 1) {
                    # Legacy support: event indicator without date
                    time[i] <- private$.calculateTimeDiff(responseDate[i], lastFollowup[i])
                    event[i] <- 1
                } else {
                    # No progression - censored at last followup
                    time[i] <- private$.calculateTimeDiff(responseDate[i], lastFollowup[i])
                    event[i] <- 0
                }
            }

            list(time = time, event = event)
        },

        .calculateTOT = function(startDate, treatmentEnd) {
            # Time on treatment: start to treatment end
            time <- private$.calculateTimeDiff(startDate, treatmentEnd)

            list(time = time)
        },

        .calculateTimeDiff = function(start, end) {
            if (self$options$inputType == 'dates') {
                # Calculate difference in days
                diff_days <- as.numeric(difftime(end, start, units = "days"))

                # Convert to selected unit
                time <- switch(self$options$timeUnit,
                    days = diff_days,
                    weeks = diff_days / 7,
                    months = diff_days / 30.4375,  # Average month length
                    years = diff_days / 365.25
                )
            } else {
                # Numeric input - already in correct units
                time <- end - start
            }

            return(time)
        },

        .convertToDate = function(x) {
            # Try to convert to Date
            if (inherits(x, "Date")) {
                return(x)
            }

            # Try standard formats
            result <- tryCatch({
                as.Date(as.character(x))
            }, error = function(e) {
                # Try numeric (days since epoch)
                tryCatch({
                    as.Date(as.numeric(x), origin = "1970-01-01")
                }, error = function(e2) {
                    # Conversion failed - warn user with Notice
                    na_count <- sum(is.na(x))
                    total_count <- length(x)

                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'dateConversionFailed',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(
                        sprintf('Date conversion failed: Could not convert %d of %d date values to Date format • Ensure dates are in standard format (YYYY-MM-DD or YYYY/MM/DD) or use Numeric input type.', na_count, total_count)
                    )
                    self$results$insert(1, notice)
                    rep(NA, length(x))
                })
            })

            # Check if conversion resulted in many NAs
            if (sum(is.na(result)) > length(result) * 0.5 && sum(!is.na(x)) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'partialDateConversion',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(
                    sprintf('Partial date conversion issues: More than 50%% of date values could not be converted • Converted: %d / %d values • Please check your date format or use Numeric input type.', sum(!is.na(result)), length(result))
                )
                self$results$insert(500, notice)
            }

            return(result)
        },

        #---------------------------------------------
        # TABLE POPULATION FUNCTIONS
        #---------------------------------------------

        .populateDataInfo = function(derivedData) {
            table <- self$results$dataInfo

            n_patients <- nrow(derivedData)

            metrics <- c(
                list(metric = "Total Patients", value = as.character(n_patients))
            )

            if (self$options$calculatePFS) {
                n_pfs <- sum(!is.na(derivedData$pfs_time))
                metrics <- c(metrics, list(
                    list(metric = "Patients with PFS data", value = as.character(n_pfs))
                ))
            }

            if (self$options$calculateOS) {
                n_os <- sum(!is.na(derivedData$os_time))
                metrics <- c(metrics, list(
                    list(metric = "Patients with OS data", value = as.character(n_os))
                ))
            }

            for (i in seq_along(metrics)) {
                table$addRow(rowKey = i, values = metrics[[i]])
            }
        },

        .populateDerivedTable = function(derivedData) {
            table <- self$results$derivedEndpoints

            for (i in 1:nrow(derivedData)) {
                row <- list(patientId = derivedData$patientId[i])

                if (self$options$calculatePFS) {
                    row$pfs_time <- derivedData$pfs_time[i]
                    row$pfs_event <- derivedData$pfs_event[i]
                }

                if (self$options$calculateOS) {
                    row$os_time <- derivedData$os_time[i]
                    row$os_event <- derivedData$os_event[i]
                }

                if (self$options$calculateTTP) {
                    row$ttp_time <- derivedData$ttp_time[i]
                    row$ttp_event <- derivedData$ttp_event[i]
                }

                if (self$options$calculateDOR) {
                    row$dor_time <- derivedData$dor_time[i]
                    row$dor_event <- derivedData$dor_event[i]
                }

                if (self$options$calculateTOT) {
                    row$tot_time <- derivedData$tot_time[i]
                }

                table$addRow(rowKey = i, values = row)
            }
        },

        .populateSummaryStats = function(derivedData) {
            table <- self$results$summaryStats

            endpoints <- list()
            if (self$options$calculatePFS) {
                endpoints[["PFS"]] <- list(
                    time = derivedData$pfs_time,
                    event = derivedData$pfs_event
                )
            }
            if (self$options$calculateOS) {
                endpoints[["OS"]] <- list(
                    time = derivedData$os_time,
                    event = derivedData$os_event
                )
            }
            if (self$options$calculateTTP) {
                endpoints[["TTP"]] <- list(
                    time = derivedData$ttp_time,
                    event = derivedData$ttp_event
                )
            }
            if (self$options$calculateDOR) {
                endpoints[["DOR"]] <- list(
                    time = derivedData$dor_time,
                    event = derivedData$dor_event
                )
            }

            rowNum <- 1
            for (name in names(endpoints)) {
                ep <- endpoints[[name]]

                # Remove missing values
                valid <- !is.na(ep$time) & !is.na(ep$event)
                time <- ep$time[valid]
                event <- ep$event[valid]

                if (length(time) == 0) next

                n <- length(time)
                n_events <- sum(event == 1)
                n_censored <- sum(event == 0)

                # Check for all-censored scenario (no events observed)
                if (n_events == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('allCensored_%s', name),
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(
                        sprintf('%s: All %d observations are censored (no events observed) • Survival analysis cannot be performed without observed events • Check event definitions or extend follow-up time.',
                                name, n)
                    )
                    self$results$insert(100, notice)
                    next  # Skip this endpoint - no events to analyze
                }

                # Event count validation - ensure reliable statistical estimates
                if (n_events < 3) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('insufficientEvents_%s', name),
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(
                        sprintf('%s: Only %d event(s) observed • Minimum 3 events required for reliable survival analysis • Cannot estimate median or confidence intervals with fewer than 3 events.',
                                name, n_events)
                    )
                    self$results$insert(100, notice)
                    next  # Skip this endpoint - cannot analyze reliably
                } else if (n_events < 10) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('fewEvents_%s', name),
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(
                        sprintf('%s: Only %d events observed • Median and confidence intervals are UNRELIABLE with <10 events • Results should be interpreted with extreme caution and confirmed with larger sample.',
                                name, n_events)
                    )
                    self$results$insert(100, notice)
                } else if (n_events < 20) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('limitedEvents_%s', name),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(
                        sprintf('%s: %d events observed • Statistical power is limited with <20 events • Confidence intervals may be wide and median estimate less precise.',
                                name, n_events)
                    )
                    self$results$insert(100, notice)
                }

                # Calculate median and CI using survival package
                surv_obj <- tryCatch({
                    survival::Surv(time, event)
                }, error = function(e) NULL)

                if (!is.null(surv_obj)) {
                    km_fit <- tryCatch({
                        survival::survfit(surv_obj ~ 1)
                    }, error = function(e) NULL)

                    if (!is.null(km_fit)) {
                        median_time <- summary(km_fit)$table["median"]
                        ci <- tryCatch({
                            confint_km <- summary(km_fit)$table[c("0.95LCL", "0.95UCL")]
                            list(lower = confint_km[1], upper = confint_km[2])
                        }, error = function(e) {
                            list(lower = NA, upper = NA)
                        })
                    } else {
                        # Cannot estimate median without KM fit - report as not estimable
                        median_time <- NA
                        ci <- list(lower = NA, upper = NA)
                    }
                } else {
                    # Cannot create survival object - median not estimable
                    median_time <- NA
                    ci <- list(lower = NA, upper = NA)
                }

                table$addRow(rowKey = rowNum, values = list(
                    endpoint = name,
                    n = n,
                    events = n_events,
                    censored = n_censored,
                    median_time = median_time,
                    ci_lower = ci$lower,
                    ci_upper = ci$upper
                ))

                rowNum <- rowNum + 1
            }
        },

        .populateEventRates = function(derivedData) {
            table <- self$results$eventRates

            endpoints <- list()
            if (self$options$calculatePFS) {
                endpoints[["PFS"]] <- list(
                    time = derivedData$pfs_time,
                    event = derivedData$pfs_event
                )
            }
            if (self$options$calculateOS) {
                endpoints[["OS"]] <- list(
                    time = derivedData$os_time,
                    event = derivedData$os_event
                )
            }

            rowNum <- 1
            for (name in names(endpoints)) {
                ep <- endpoints[[name]]

                # Remove missing values
                valid <- !is.na(ep$time) & !is.na(ep$event)
                event <- ep$event[valid]

                if (length(event) == 0) next

                total <- length(event)
                n_events <- sum(event == 1)

                # Validate minimum sample size for meaningful event rate
                if (total < 10) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('eventRateSmallSample_%s', name),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(
                        sprintf('%s event rates: Only %d observations • Event rates and censoring percentages are unreliable with <10 observations • Interpret with caution.',
                                name, total)
                    )
                    self$results$insert(100, notice)
                }

                event_rate <- (n_events / total) * 100
                censoring_rate <- 100 - event_rate

                table$addRow(rowKey = rowNum, values = list(
                    endpoint = name,
                    total = total,
                    events = n_events,
                    event_rate = event_rate,
                    censoring_rate = censoring_rate
                ))

                rowNum <- rowNum + 1
            }
        },

        .populateMilestones = function(derivedData) {
            table <- self$results$milestoneTable

            # Parse milestone times
            milestone_str <- self$options$milestones
            milestones <- tryCatch({
                as.numeric(unlist(strsplit(milestone_str, "[,;\\s]+")))
            }, error = function(e) {
                return(NULL)
            })

            if (is.null(milestones) || length(milestones) == 0) {
                return()
            }

            endpoints <- list()
            if (self$options$calculatePFS) {
                endpoints[["PFS"]] <- list(
                    time = derivedData$pfs_time,
                    event = derivedData$pfs_event
                )
            }
            if (self$options$calculateOS) {
                endpoints[["OS"]] <- list(
                    time = derivedData$os_time,
                    event = derivedData$os_event
                )
            }

            rowNum <- 1
            for (name in names(endpoints)) {
                ep <- endpoints[[name]]

                # Remove missing values
                valid <- !is.na(ep$time) & !is.na(ep$event)
                time <- ep$time[valid]
                event <- ep$event[valid]

                if (length(time) == 0) next

                # Validate event count for reliable milestone estimates
                n_events <- sum(event == 1)
                if (n_events < 3) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = sprintf('milestoneInsufficientEvents_%s', name),
                        type = jmvcore::NoticeType$WARNING
                    )
                    notice$setContent(
                        sprintf('%s milestone analysis: Only %d event(s) observed • Minimum 3 events required for reliable survival probability estimates at specific time points • Skipping milestone analysis for this endpoint.',
                                name, n_events)
                    )
                    self$results$insert(100, notice)
                    next  # Skip milestone calculation for this endpoint
                }

                # Fit KM curve
                surv_obj <- tryCatch({
                    survival::Surv(time, event)
                }, error = function(e) NULL)

                if (is.null(surv_obj)) next

                km_fit <- tryCatch({
                    survival::survfit(surv_obj ~ 1)
                }, error = function(e) NULL)

                if (is.null(km_fit)) next

                # Calculate survival at each milestone
                for (milestone in milestones) {
                    surv_summary <- summary(km_fit, times = milestone, extend = TRUE)

                    table$addRow(rowKey = rowNum, values = list(
                        endpoint = name,
                        milestone = milestone,
                        survival_prob = surv_summary$surv,
                        ci_lower = surv_summary$lower,
                        ci_upper = surv_summary$upper,
                        n_at_risk = surv_summary$n.risk
                    ))

                    rowNum <- rowNum + 1
                }
            }
        },

        #---------------------------------------------
        # KAPLAN-MEIER PLOT
        #---------------------------------------------

        .kmPlot = function(image, ...) {

            if (is.null(private$.derivedData)) {
                return()
            }

            derivedData <- private$.derivedData

            # State management - ensures plot updates when options change
            plotState <- list(
                data = as.data.frame(derivedData),  # Ensure base data.frame for serialization
                calculatePFS = self$options$calculatePFS,
                calculateOS = self$options$calculateOS,
                calculateTTP = self$options$calculateTTP,
                timeUnit = self$options$timeUnit
            )
            image$setState(plotState)

            # Prepare data for plotting
            plot_data <- data.frame()

            if (self$options$calculatePFS && "pfs_time" %in% names(derivedData)) {
                valid <- !is.na(derivedData$pfs_time) & !is.na(derivedData$pfs_event)
                pfs_data <- data.frame(
                    time = derivedData$pfs_time[valid],
                    event = derivedData$pfs_event[valid],
                    endpoint = "PFS"
                )
                plot_data <- rbind(plot_data, pfs_data)
            }

            if (self$options$calculateOS && "os_time" %in% names(derivedData)) {
                valid <- !is.na(derivedData$os_time) & !is.na(derivedData$os_event)
                os_data <- data.frame(
                    time = derivedData$os_time[valid],
                    event = derivedData$os_event[valid],
                    endpoint = "OS"
                )
                plot_data <- rbind(plot_data, os_data)
            }

            if (self$options$calculateTTP && "ttp_time" %in% names(derivedData)) {
                valid <- !is.na(derivedData$ttp_time) & !is.na(derivedData$ttp_event)
                ttp_data <- data.frame(
                    time = derivedData$ttp_time[valid],
                    event = derivedData$ttp_event[valid],
                    endpoint = "TTP"
                )
                plot_data <- rbind(plot_data, ttp_data)
            }

            if (nrow(plot_data) == 0) {
                return()
            }

            # Validate event counts per endpoint before plotting
            endpoint_summary <- table(plot_data$endpoint[plot_data$event == 1])
            insufficient_endpoints <- names(endpoint_summary[endpoint_summary < 3])

            if (length(insufficient_endpoints) > 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'kmPlotInsufficientEvents',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(
                    sprintf('KM plot warning: Endpoint(s) %s have <3 events • Survival curves may be unreliable • Consider using only endpoints with adequate event counts.',
                            paste(insufficient_endpoints, collapse=', '))
                )
                self$results$insert(100, notice)

                # Remove endpoints with <3 events from plot
                plot_data <- plot_data[!plot_data$endpoint %in% insufficient_endpoints, ]

                if (nrow(plot_data) == 0) {
                    return()  # No valid endpoints to plot
                }
            }

            # Fit survival models
            surv_obj <- survival::Surv(plot_data$time, plot_data$event)
            km_fit <- survival::survfit(surv_obj ~ endpoint, data = plot_data)

            # Create plot
            time_unit <- switch(self$options$timeUnit,
                days = "Days",
                weeks = "Weeks",
                months = "Months",
                years = "Years"
            )

            p <- tryCatch({
                survminer::ggsurvplot(
                    km_fit,
                    data = plot_data,
                    palette = c("#E7B800", "#2E9FDF", "#FC4E07"),
                    risk.table = FALSE,
                    conf.int = TRUE,
                    xlab = paste("Time (", time_unit, ")", sep = ""),
                    ylab = "Survival Probability",
                    title = "Kaplan-Meier Survival Curves",
                    legend.title = "Endpoint",
                    legend.labs = levels(factor(plot_data$endpoint)),
                    ggtheme = theme_minimal()
                )
            }, error = function(e) {
                # Fallback to base plot
                plot(km_fit, col = 1:length(unique(plot_data$endpoint)),
                     xlab = paste("Time (", time_unit, ")", sep = ""),
                     ylab = "Survival Probability",
                     main = "Kaplan-Meier Survival Curves")
                legend("topright", legend = levels(factor(plot_data$endpoint)),
                       col = 1:length(unique(plot_data$endpoint)), lty = 1)
                return(NULL)
            })

            if (!is.null(p)) {
                print(p$plot)
            }

            TRUE
        },

        #---------------------------------------------
        # EXPORT AND DOCUMENTATION
        #---------------------------------------------

        .exportEndpoints = function(derivedData) {

            # Create export directory
            export_dir <- file.path(getwd(), "survival_exports")
            if (!dir.exists(export_dir)) {
                dir.create(export_dir, recursive = TRUE)
            }

            # Generate filename with timestamp
            timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
            filename <- file.path(export_dir,
                paste0("derived_endpoints_", timestamp, ".csv"))

            # Write CSV
            tryCatch({
                write.csv(derivedData, filename, row.names = FALSE)

                message <- paste0(
                    "<div style='padding: 15px; background-color: #d4edda; ",
                    "border: 1px solid #c3e6cb; border-radius: 5px;'>",
                    "<strong>Export Successful!</strong><br/>",
                    "File saved to: <code>", filename, "</code><br/><br/>",
                    "You can now import this file into survival analysis modules:<br/>",
                    "<ul>",
                    "<li>Use PFS columns (pfs_time, pfs_event) for PFS analysis</li>",
                    "<li>Use OS columns (os_time, os_event) for OS analysis</li>",
                    "<li>Link by Patient ID for group comparisons</li>",
                    "</ul>",
                    "</div>"
                )

                self$results$exportMessage$setContent(message)
            }, error = function(e) {
                message <- paste0(
                    "<div style='padding: 15px; background-color: #f8d7da; ",
                    "border: 1px solid #f5c6cb; border-radius: 5px;'>",
                    "<strong>Export Failed</strong><br/>",
                    "Error: ", e$message,
                    "</div>"
                )
                self$results$exportMessage$setContent(message)
            })
        },

        .generateUsageGuide = function() {
            paste0(
                "<html>",
                "<head>",
                "<style>",
                ".usage-guide {",
                "    font-family: Arial, sans-serif;",
                "    max-width: 900px;",
                "    line-height: 1.6;",
                "    padding: 20px;",
                "}",
                ".usage-guide h3 {",
                "    color: #2c3e50;",
                "    border-bottom: 2px solid #3498db;",
                "    padding-bottom: 5px;",
                "}",
                ".usage-guide .workflow {",
                "    background-color: #f8f9fa;",
                "    padding: 15px;",
                "    margin: 15px 0;",
                "    border-left: 4px solid #28a745;",
                "}",
                ".usage-guide .step {",
                "    margin: 10px 0;",
                "    padding-left: 20px;",
                "}",
                ".usage-guide .note {",
                "    background-color: #fff3cd;",
                "    border: 1px solid #ffc107;",
                "    padding: 10px;",
                "    margin: 10px 0;",
                "    border-radius: 5px;",
                "}",
                "</style>",
                "</head>",
                "<body>",
                "<div class='usage-guide'>",
                "    <h3>How to Use Derived Endpoints in Survival Analysis</h3>",
                "    ",
                "    <div class='workflow'>",
                "        <strong>Workflow:</strong>",
                "        <div class='step'>",
                "            <strong>1. Export Derived Data:</strong> Check the 'Export Derived Endpoints' option",
                "        </div>",
                "        <div class='step'>",
                "            <strong>2. Import to Survival Module:</strong> Use the exported CSV in modules like:",
                "            <ul>",
                "                <li>Kaplan-Meier Survival Analysis</li>",
                "                <li>Cox Proportional Hazards</li>",
                "                <li>Competing Risks Analysis</li>",
                "            </ul>",
                "        </div>",
                "        <div class='step'>",
                "            <strong>3. Variable Mapping:</strong>",
                "            <ul>",
                "                <li>Time variable: Use pfs_time, os_time, etc.</li>",
                "                <li>Event variable: Use pfs_event, os_event, etc.</li>",
                "                <li>Grouping: Join with baseline characteristics by Patient ID</li>",
                "            </ul>",
                "        </div>",
                "    </div>",
                "    ",
                "    <div class='note'>",
                "        <strong>Note:</strong> The simple KM plot shown here is for verification only. ",
                "        For full survival analysis with group comparisons, log-rank tests, and Cox regression, ",
                "        use the dedicated survival analysis modules.",
                "    </div>",
                "    ",
                "    <h3>Endpoint Definitions</h3>",
                "    <ul>",
                "        <li><strong>PFS:</strong> Progression-Free Survival - time to progression or death</li>",
                "        <li><strong>OS:</strong> Overall Survival - time to death from any cause</li>",
                "        <li><strong>TTP:</strong> Time to Progression - time to disease progression (censors death)</li>",
                "        <li><strong>DOR:</strong> Duration of Response - time from response to progression (responders only)</li>",
                "    </ul>",
                "</div>",
                "</body>",
                "</html>"
            )
        },

        # Store derived data
        .derivedData = NULL
    )
)
