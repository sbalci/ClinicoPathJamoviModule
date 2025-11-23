
# R6 Class Implementation ====
waterfallrecistClass <- if (requireNamespace('jmvcore', quietly = TRUE)) {
    R6::R6Class(
        "waterfallrecistClass",
        inherit = waterfallrecistBase,
        private = list(

            # RECIST v1.1 Constants ====
            RECIST_CR_THRESHOLD = -100,  # Complete response: disappearance of all lesions
            RECIST_PR_THRESHOLD = -30,   # Partial response: ≥30% decrease
            RECIST_PD_THRESHOLD = 20,    # Progressive disease: ≥20% increase (+ 5mm absolute)
            RECIST_PD_ABSOLUTE_MM = 5,   # Absolute increase required for PD (in addition to 20%)
            MIN_TARGET_DIAMETER_NONLYMPH = 10,  # Minimum 10mm for non-lymph node targets
            MIN_TARGET_DIAMETER_LYMPH = 15,     # Minimum 15mm short axis for lymph nodes

            # Initialization ====
            .init = function() {
                # Methodology notice
                method_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'methodology',
                    type = jmvcore::NoticeType$INFO
                )
                method_notice$setContent(paste0('RECIST v1.1 protocol per Eisenhauer et al. (2009) Eur J Cancer 45:228-247. Target lesion limits: <=', self$options$maxTargetLesions, ' total, <=', self$options$maxLesionsPerOrgan, ' per organ. CR/PR confirmation: >=', self$options$confirmationInterval, ' weeks. Any new lesion = PD.'))
                self$results$insert(999, method_notice)

                # Initialize output elements
                if (self$options$showLesionTable)
                    private$.initLesionTable()

                if (self$options$showTargetSumTable)
                    private$.initTargetSumTable()

                if (self$options$showBestResponseTable)
                    private$.initBestResponseTable()

                if (self$options$showRecistComplianceReport)
                    private$.initComplianceReport()

                if (self$options$showWaterfallPlot)
                    private$.initWaterfallPlot()

                if (self$options$showSpiderPlot)
                    private$.initSpiderPlot()
            },

            # Main Execution ====
            .run = function() {
                # Input validation
                validation_result <- private$.validateInput()
                if (!validation_result$valid) {
                    return()  # Errors already posted as notices
                }

                # Step 1 - Validate and prepare lesion-level data
                lesion_data <- private$.prepareLesionData()

                if (is.null(lesion_data) || nrow(lesion_data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noValidData',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(paste0('No valid lesion data after removing rows with missing Patient ID, Lesion ID, or Visit Time. Ensure baseline visit (time=', self$options$baselineTimepoint, ') exists and data is in lesion-level format.'))
                    self$results$insert(1, notice)
                    return()
                }

                # Step 2 - Validate target lesion selection (max 5, max 2 per organ)
                target_validation <- private$.validateTargetLesionSelection(lesion_data)

                # Step 3 - Calculate target lesion sums by visit
                target_sums <- private$.calculateTargetLesionSums(lesion_data)

                # Step 4 - Detect new lesions (any new lesion = PD)
                new_lesions <- private$.detectNewLesions(lesion_data)

                # Step 5 - Assess non-target lesion progression
                nontarget_assessment <- private$.assessNonTargetProgression(lesion_data)

                # Step 6 - Determine overall response per visit
                visit_responses <- private$.determineOverallResponse(target_sums, new_lesions, nontarget_assessment)

                # Step 7 - Apply confirmation rules (CR/PR must be confirmed ≥4 weeks)
                confirmed_responses <- private$.confirmResponses(visit_responses)

                # Step 8 - Calculate Best Overall Response (BOR)
                best_responses <- private$.calculateBestOverallResponse(confirmed_responses)

                # Step 9 - Populate output tables
                if (self$options$showLesionTable)
                    private$.populateLesionTable(lesion_data)

                if (self$options$showTargetSumTable)
                    private$.populateTargetSumTable(target_sums, confirmed_responses)

                if (self$options$showBestResponseTable) {
                    private$.populateBestResponseTable(best_responses)
                    private$.populateRecistSummary(best_responses)
                }

                if (self$options$showRecistComplianceReport)
                    private$.populateComplianceReport(target_validation, new_lesions, confirmed_responses)

                # Step 10 - Generate plots
                if (self$options$showWaterfallPlot)
                    private$.prepareWaterfallPlot(best_responses, target_sums)

                if (self$options$showSpiderPlot)
                    private$.prepareSpiderPlot(target_sums, best_responses)

                # Step 11 - Analysis completion notice
                n_patients <- length(unique(best_responses$patientID))
                n_cr <- sum(best_responses$bestOverallResponse == "CR")
                n_pr <- sum(best_responses$bestOverallResponse == "PR")
                orr_pct <- round((n_cr + n_pr) / n_patients * 100, 1)

                success_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisComplete',
                    type = jmvcore::NoticeType$INFO
                )
                success_notice$setContent(paste0('RECIST v1.1 analysis completed for ', n_patients, ' patients. ORR: ', orr_pct, '% (CR=', n_cr, ', PR=', n_pr, '). Confirmation interval: >=', self$options$confirmationInterval, ' weeks.'))
                self$results$insert(999, success_notice)

                # Step 12 - Add BOR to dataset if requested
                # TODO: Implement data augmentation
                # if (self$options$addBestResponseToData)
                #     private$.addBestResponseColumn(best_responses)
            },

            # Input Validation ====
            .validateInput = function() {
                # Check for required variables
                if (is.null(self$options$patientID) ||
                    length(self$options$patientID) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingPatientID',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Patient ID variable is required. Select a patient identifier variable and re-run.')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                if (is.null(self$options$lesionID) ||
                    length(self$options$lesionID) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingLesionID',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Lesion ID variable is required. Select a lesion identifier variable and re-run.')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                if (is.null(self$options$visitTime) ||
                    length(self$options$visitTime) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingVisitTime',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Visit Time variable is required. Select a time variable (baseline=0 recommended) and re-run.')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                if (is.null(self$options$diameter) ||
                    length(self$options$diameter) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'missingDiameter',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Lesion Diameter variable is required. Select a numeric diameter variable (in mm) and re-run.')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Get data
                if (is.null(self$data) || nrow(self$data) == 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noData',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('No data available. Load a dataset in lesion-level format (one row per lesion per visit).')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Extract variables
                data_df <- self$data

                # Verify visitTime variable exists
                visitTimeVar <- jmvcore::toB64(self$options$visitTime)
                if (!visitTimeVar %in% colnames(data_df)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'visitTimeNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(paste0('Visit Time variable "', self$options$visitTime, '" not found in dataset. Verify variable name and re-run.'))
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Verify baseline timepoint exists
                baseline_present <- any(data_df[[visitTimeVar]] == self$options$baselineTimepoint, na.rm = TRUE)
                if (!baseline_present) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'noBaseline',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(paste0('Baseline timepoint (', self$options$baselineTimepoint, ') not found in Visit Time. Ensure baseline measurements exist or adjust Baseline Timepoint Value.'))
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Check diameter variable exists
                diameterVar <- jmvcore::toB64(self$options$diameter)
                if (!diameterVar %in% colnames(data_df)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'diameterNotFound',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(paste0('Diameter variable "', self$options$diameter, '" not found in dataset. Verify variable name and re-run.'))
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Check diameter values (must be non-negative)
                diameter_values <- data_df[[diameterVar]]
                if (any(diameter_values < 0, na.rm = TRUE)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'negativeDiameters',
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent('Diameter values must be non-negative (>=0 mm). Correct negative values and re-run.')
                    self$results$insert(1, notice)
                    return(list(valid = FALSE, message = ''))
                }

                # Check if lesionType is provided and has valid values
                if (!is.null(self$options$lesionType) && length(self$options$lesionType) > 0) {
                    lesionTypeVar <- jmvcore::toB64(self$options$lesionType)
                    if (lesionTypeVar %in% colnames(data_df)) {
                        lesion_types <- unique(as.character(data_df[[lesionTypeVar]]))
                        lesion_types <- lesion_types[!is.na(lesion_types)]

                        valid_types <- c("Target", "NonTarget", "New", "target", "nontarget", "new")
                        invalid_types <- lesion_types[!lesion_types %in% valid_types]

                        if (length(invalid_types) > 0) {
                            notice <- jmvcore::Notice$new(
                                options = self$options,
                                name = 'invalidLesionTypes',
                                type = jmvcore::NoticeType$WARNING
                            )
                            notice$setContent(paste0('Lesion Type contains invalid values: ', paste(invalid_types, collapse=', '), '. Valid values: Target, NonTarget, New. Invalid entries treated as Target.'))
                            self$results$insert(2, notice)
                        }
                    }
                }

                return(list(valid = TRUE, message = ''))
            },

            # RECIST v1.1 Core Methods ====

            .prepareLesionData = function() {
                # Extract data
                data_df <- self$data

                # Get variable names (encoded)
                patientIDVar <- jmvcore::toB64(self$options$patientID)
                lesionIDVar <- jmvcore::toB64(self$options$lesionID)
                visitTimeVar <- jmvcore::toB64(self$options$visitTime)
                diameterVar <- jmvcore::toB64(self$options$diameter)

                # Build lesion data frame
                lesion_data <- data.frame(
                    patientID = as.character(data_df[[patientIDVar]]),
                    lesionID = as.character(data_df[[lesionIDVar]]),
                    visitTime = as.numeric(data_df[[visitTimeVar]]),
                    diameter = as.numeric(data_df[[diameterVar]]),
                    stringsAsFactors = FALSE
                )

                # Add optional variables if provided
                if (!is.null(self$options$lesionType) && length(self$options$lesionType) > 0) {
                    lesionTypeVar <- jmvcore::toB64(self$options$lesionType)
                    if (lesionTypeVar %in% colnames(data_df)) {
                        lesion_data$lesionType <- as.character(data_df[[lesionTypeVar]])
                        # Standardize case
                        lesion_data$lesionType <- tolower(lesion_data$lesionType)
                        lesion_data$lesionType[lesion_data$lesionType == "target"] <- "Target"
                        lesion_data$lesionType[lesion_data$lesionType == "nontarget"] <- "NonTarget"
                        lesion_data$lesionType[lesion_data$lesionType == "new"] <- "New"
                    } else {
                        lesion_data$lesionType <- "Target"  # Default to Target if not specified
                    }
                } else {
                    lesion_data$lesionType <- "Target"  # Default to Target if not specified
                }

                if (!is.null(self$options$location) && length(self$options$location) > 0) {
                    locationVar <- jmvcore::toB64(self$options$location)
                    if (locationVar %in% colnames(data_df)) {
                        lesion_data$location <- as.character(data_df[[locationVar]])
                    } else {
                        lesion_data$location <- "Unknown"
                    }
                } else {
                    lesion_data$location <- "Unknown"
                }

                if (!is.null(self$options$isNewLesion) && length(self$options$isNewLesion) > 0) {
                    isNewLesionVar <- jmvcore::toB64(self$options$isNewLesion)
                    if (isNewLesionVar %in% colnames(data_df)) {
                        lesion_data$isNewLesion <- as.numeric(data_df[[isNewLesionVar]])
                        # Mark new lesions
                        lesion_data$lesionType[lesion_data$isNewLesion == 1] <- "New"
                    } else {
                        lesion_data$isNewLesion <- 0
                    }
                } else {
                    lesion_data$isNewLesion <- 0
                }

                # Remove rows with missing key values
                lesion_data <- lesion_data[!is.na(lesion_data$patientID) &
                                           !is.na(lesion_data$lesionID) &
                                           !is.na(lesion_data$visitTime), ]

                # Identify baseline measurements
                lesion_data$isBaseline <- lesion_data$visitTime == self$options$baselineTimepoint

                # Sort by patient, lesion, and time
                lesion_data <- lesion_data[order(lesion_data$patientID,
                                                 lesion_data$lesionID,
                                                 lesion_data$visitTime), ]

                # Add row ID for reference
                lesion_data$rowID <- seq_len(nrow(lesion_data))

                return(lesion_data)
            },

            .validateTargetLesionSelection = function(lesion_data) {
                # Filter to baseline target lesions only
                baseline_targets <- lesion_data[lesion_data$isBaseline == TRUE &
                                                lesion_data$lesionType == "Target", ]

                violations <- character(0)
                warnings_list <- list()

                # Check each patient
                patients <- unique(baseline_targets$patientID)

                for (pt in patients) {
                    pt_targets <- baseline_targets[baseline_targets$patientID == pt, ]

                    # Check 1: Max 5 target lesions per patient
                    n_targets <- nrow(pt_targets)
                    if (n_targets > self$options$maxTargetLesions) {
                        violation_msg <- paste0(
                            "Patient ", pt, " has ", n_targets, " target lesions ",
                            "(exceeds RECIST v1.1 limit of ", self$options$maxTargetLesions, ")"
                        )
                        violations <- c(violations, violation_msg)
                    }

                    # Check 2: Max 2 target lesions per organ
                    if ("location" %in% colnames(pt_targets)) {
                        location_counts <- table(pt_targets$location)
                        over_limit <- location_counts > self$options$maxLesionsPerOrgan

                        if (any(over_limit)) {
                            for (loc in names(location_counts[over_limit])) {
                                violation_msg <- paste0(
                                    "Patient ", pt, " has ", location_counts[loc],
                                    " target lesions in ", loc,
                                    " (exceeds RECIST v1.1 limit of ", self$options$maxLesionsPerOrgan, " per organ)"
                                )
                                violations <- c(violations, violation_msg)
                            }
                        }
                    }

                    # Check 3: Minimum size requirements (10mm non-lymph, 15mm lymph)
                    # NOTE: This is a simplified check - in practice, lymph node detection would require
                    # additional metadata. Here we just check for 10mm minimum.
                    small_lesions <- pt_targets[!is.na(pt_targets$diameter) &
                                                 pt_targets$diameter < private$MIN_TARGET_DIAMETER_NONLYMPH, ]

                    if (nrow(small_lesions) > 0) {
                        for (i in seq_len(nrow(small_lesions))) {
                            violation_msg <- paste0(
                                "Patient ", pt, " lesion ", small_lesions$lesionID[i],
                                " has diameter ", round(small_lesions$diameter[i], 1), "mm ",
                                "(below RECIST v1.1 minimum of ", private$MIN_TARGET_DIAMETER_NONLYMPH, "mm)"
                            )
                            violations <- c(violations, violation_msg)
                        }
                    }
                }

                # Post violations as STRONG_WARNING
                if (length(violations) > 0) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'recistViolations',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    # Single-line format (jamovi Notices don't support newlines)
                    notice$setContent(paste0('RECIST v1.1 COMPLIANCE VIOLATIONS: ', paste(violations, collapse=' • '), ' • Results may not be suitable for regulatory submissions.'))
                    self$results$insert(1, notice)
                }

                return(list(
                    valid = length(violations) == 0,
                    violations = violations,
                    target_lesions = baseline_targets$lesionID
                ))
            },

            .calculateTargetLesionSums = function(lesion_data) {
                # Filter to target lesions only
                target_lesions <- lesion_data[lesion_data$lesionType == "Target", ]

                if (nrow(target_lesions) == 0) {
                    return(data.frame(
                        patientID = character(0),
                        visitTime = numeric(0),
                        visitNumber = integer(0),
                        nTargetLesions = integer(0),
                        baseline_sum = numeric(0),
                        current_sum = numeric(0),
                        absolute_change = numeric(0),
                        percent_change = numeric(0),
                        target_response = character(0),
                        stringsAsFactors = FALSE
                    ))
                }

                # Calculate baseline sums per patient
                baseline_sums <- aggregate(
                    diameter ~ patientID,
                    data = target_lesions[target_lesions$isBaseline == TRUE, ],
                    FUN = function(x) sum(x, na.rm = TRUE)
                )
                names(baseline_sums)[2] <- "baseline_sum"

                # Calculate sums for each visit
                visit_sums <- aggregate(
                    diameter ~ patientID + visitTime,
                    data = target_lesions,
                    FUN = function(x) sum(x, na.rm = TRUE)
                )
                names(visit_sums)[3] <- "current_sum"

                # Count lesions per visit
                lesion_counts <- aggregate(
                    diameter ~ patientID + visitTime,
                    data = target_lesions,
                    FUN = length
                )
                names(lesion_counts)[3] <- "nTargetLesions"

                # Merge baseline sums with visit sums
                target_sums <- merge(visit_sums, baseline_sums, by = "patientID")
                target_sums <- merge(target_sums, lesion_counts, by = c("patientID", "visitTime"))

                # Calculate changes
                target_sums$absolute_change <- target_sums$current_sum - target_sums$baseline_sum

                # Percent change calculation (handle zero baseline)
                target_sums$percent_change <- ifelse(
                    target_sums$baseline_sum > 0,
                    (target_sums$current_sum - target_sums$baseline_sum) / target_sums$baseline_sum * 100,
                    NA
                )

                # Determine target lesion response per RECIST v1.1
                target_sums$target_response <- "SD"  # Default: Stable Disease

                # Complete Response: All lesions disappeared (sum = 0)
                target_sums$target_response[target_sums$current_sum == 0] <- "CR"

                # Partial Response: ≥30% decrease from baseline
                target_sums$target_response[!is.na(target_sums$percent_change) &
                                            target_sums$percent_change <= private$RECIST_PR_THRESHOLD] <- "PR"

                # Progressive Disease: ≥20% increase from baseline AND ≥5mm absolute increase
                is_pd <- !is.na(target_sums$percent_change) &
                         target_sums$percent_change >= private$RECIST_PD_THRESHOLD &
                         target_sums$absolute_change >= private$RECIST_PD_ABSOLUTE_MM
                target_sums$target_response[is_pd] <- "PD"

                # Add visit number (sequential visits per patient)
                target_sums <- target_sums[order(target_sums$patientID, target_sums$visitTime), ]
                target_sums$visitNumber <- ave(
                    target_sums$visitTime,
                    target_sums$patientID,
                    FUN = seq_along
                )

                # Reorder columns
                target_sums <- target_sums[, c("patientID", "visitTime", "visitNumber", "nTargetLesions",
                                                "baseline_sum", "current_sum", "absolute_change",
                                                "percent_change", "target_response")]

                return(target_sums)
            },

            .detectNewLesions = function(lesion_data) {
                # Filter to new lesions
                new_lesions <- lesion_data[lesion_data$lesionType == "New" |
                                           (lesion_data$isNewLesion == 1 & !lesion_data$isBaseline), ]

                if (nrow(new_lesions) == 0) {
                    return(data.frame(
                        patientID = character(0),
                        first_new_lesion_visit = numeric(0),
                        new_lesion_location = character(0),
                        new_lesion_ID = character(0),
                        stringsAsFactors = FALSE
                    ))
                }

                # For each patient, find first visit with new lesion
                new_lesion_summary <- aggregate(
                    visitTime ~ patientID,
                    data = new_lesions,
                    FUN = min
                )
                names(new_lesion_summary)[2] <- "first_new_lesion_visit"

                # Add lesion details
                first_new <- merge(new_lesion_summary, new_lesions,
                                   by.x = c("patientID", "first_new_lesion_visit"),
                                   by.y = c("patientID", "visitTime"))

                # Keep first lesion per patient (if multiple new lesions at same visit)
                first_new <- first_new[!duplicated(first_new$patientID), ]

                first_new <- first_new[, c("patientID", "first_new_lesion_visit",
                                           "location", "lesionID")]
                names(first_new)[3:4] <- c("new_lesion_location", "new_lesion_ID")

                return(first_new)
            },

            .assessNonTargetProgression = function(lesion_data) {
                # Filter to non-target lesions
                nontarget_lesions <- lesion_data[lesion_data$lesionType == "NonTarget", ]

                if (nrow(nontarget_lesions) == 0) {
                    # No non-target lesions - return empty data frame
                    # In RECIST, absence of non-target lesions = non-applicable (treated as Non-CR/Non-PD)
                    return(data.frame(
                        patientID = character(0),
                        visitTime = numeric(0),
                        nontarget_status = character(0),
                        stringsAsFactors = FALSE
                    ))
                }

                # Count non-target lesions per patient-visit
                nontarget_counts <- aggregate(
                    lesionID ~ patientID + visitTime,
                    data = nontarget_lesions,
                    FUN = length
                )
                names(nontarget_counts)[3] <- "n_nontarget"

                # Get baseline counts
                baseline_counts <- aggregate(
                    lesionID ~ patientID,
                    data = nontarget_lesions[nontarget_lesions$isBaseline, ],
                    FUN = length
                )
                names(baseline_counts)[2] <- "baseline_n_nontarget"

                # Merge
                nontarget_assessment <- merge(nontarget_counts, baseline_counts, by = "patientID")

                # Determine status
                # CR: All non-target lesions disappeared (count = 0)
                # Non-CR/Non-PD: Some lesions persist but no clear progression
                # PD: Unequivocal progression (e.g., increase in number - simplified here)

                nontarget_assessment$nontarget_status <- "Non-CR/Non-PD"  # Default

                # CR: All disappeared
                nontarget_assessment$nontarget_status[nontarget_assessment$n_nontarget == 0] <- "CR"

                # PD: Significant increase (simplified: ≥2 new non-target lesions)
                # NOTE: In clinical practice, this is a QUALITATIVE assessment by radiologist
                increase <- nontarget_assessment$n_nontarget - nontarget_assessment$baseline_n_nontarget
                nontarget_assessment$nontarget_status[increase >= 2] <- "PD"

                nontarget_assessment <- nontarget_assessment[, c("patientID", "visitTime", "nontarget_status")]

                return(nontarget_assessment)
            },

            .determineOverallResponse = function(target_sums, new_lesions, nontarget_assessment) {
                # Start with target sums as base
                responses <- target_sums[, c("patientID", "visitTime", "visitNumber", "target_response")]

                # Add new lesion status
                if (nrow(new_lesions) > 0) {
                    responses$new_lesion_present <- FALSE
                    for (i in seq_len(nrow(responses))) {
                        pt <- responses$patientID[i]
                        vt <- responses$visitTime[i]

                        # Check if new lesion appeared at or before this visit
                        pt_new <- new_lesions[new_lesions$patientID == pt, ]
                        if (nrow(pt_new) > 0 && pt_new$first_new_lesion_visit[1] <= vt) {
                            responses$new_lesion_present[i] <- TRUE
                        }
                    }
                } else {
                    responses$new_lesion_present <- FALSE
                }

                # Add non-target status
                if (nrow(nontarget_assessment) > 0) {
                    responses <- merge(responses, nontarget_assessment,
                                       by = c("patientID", "visitTime"),
                                       all.x = TRUE)
                    # If no non-target data for a visit, assume Non-CR/Non-PD
                    responses$nontarget_status[is.na(responses$nontarget_status)] <- "Non-CR/Non-PD"
                } else {
                    responses$nontarget_status <- "Non-CR/Non-PD"
                }

                # Apply RECIST v1.1 OVERALL RESPONSE TABLE
                responses$overall_response_unconfirmed <- "SD"  # Default

                # PRIORITY 1: ANY new lesion → PD
                responses$overall_response_unconfirmed[responses$new_lesion_present] <- "PD"

                # PRIORITY 2: Non-target PD → PD
                responses$overall_response_unconfirmed[responses$nontarget_status == "PD"] <- "PD"

                # PRIORITY 3: Target PD → PD
                responses$overall_response_unconfirmed[responses$target_response == "PD"] <- "PD"

                # Now handle non-PD cases
                is_not_pd <- responses$overall_response_unconfirmed != "PD"

                # CR: Target CR + Non-target CR + No new lesions
                is_cr <- is_not_pd &
                         responses$target_response == "CR" &
                         responses$nontarget_status == "CR" &
                         !responses$new_lesion_present
                responses$overall_response_unconfirmed[is_cr] <- "CR"

                # PR: Target CR with Non-CR/Non-PD non-target, OR Target PR with Non-PD non-target
                is_pr <- is_not_pd &
                         ((responses$target_response == "CR" & responses$nontarget_status != "CR") |
                          (responses$target_response == "PR")) &
                         !responses$new_lesion_present
                responses$overall_response_unconfirmed[is_pr] <- "PR"

                # SD: Everything else (target SD with non-PD non-target, no new lesions)
                # Already set as default

                # Sort by patient and visit
                responses <- responses[order(responses$patientID, responses$visitTime), ]

                return(responses)
            },

            .confirmResponses = function(visit_responses) {
                # Add confirmation status column
                visit_responses$response_confirmed <- FALSE

                # SD and PD do not require confirmation
                visit_responses$response_confirmed[visit_responses$overall_response_unconfirmed %in% c("SD", "PD")] <- TRUE

                # For CR and PR, check confirmation
                patients <- unique(visit_responses$patientID)

                for (pt in patients) {
                    pt_data <- visit_responses[visit_responses$patientID == pt, ]
                    pt_data <- pt_data[order(pt_data$visitTime), ]

                    for (i in seq_len(nrow(pt_data))) {
                        current_response <- pt_data$overall_response_unconfirmed[i]

                        # Only check CR and PR
                        if (current_response %in% c("CR", "PR")) {
                            current_time <- pt_data$visitTime[i]

                            # Check if there's a subsequent visit with same response ≥ confirmation_interval later
                            later_visits <- pt_data[pt_data$visitTime > current_time, ]

                            if (nrow(later_visits) > 0) {
                                # Find visits with sufficient time gap
                                confirmed_visits <- later_visits[
                                    (later_visits$visitTime - current_time) >= self$options$confirmationInterval &
                                    later_visits$overall_response_unconfirmed == current_response,
                                ]

                                if (nrow(confirmed_visits) > 0) {
                                    # Response is confirmed
                                    row_idx <- which(visit_responses$patientID == pt &
                                                     visit_responses$visitTime == current_time)
                                    visit_responses$response_confirmed[row_idx] <- TRUE
                                }
                            }
                        }
                    }
                }

                return(visit_responses)
            },

            .calculateBestOverallResponse = function(confirmed_responses) {
                patients <- unique(confirmed_responses$patientID)

                bor_results <- data.frame(
                    patientID = patients,
                    bestOverallResponse = character(length(patients)),
                    borConfirmed = character(length(patients)),
                    borFirstVisit = character(length(patients)),
                    timeToResponse = numeric(length(patients)),
                    durationOfResponse = numeric(length(patients)),
                    progressionOccurred = character(length(patients)),
                    progressionVisit = character(length(patients)),
                    stringsAsFactors = FALSE
                )

                for (i in seq_along(patients)) {
                    pt <- patients[i]
                    pt_data <- confirmed_responses[confirmed_responses$patientID == pt, ]
                    pt_data <- pt_data[order(pt_data$visitTime), ]

                    # Filter to confirmed responses only for BOR determination
                    confirmed_only <- pt_data[pt_data$response_confirmed, ]

                    # Determine BOR using hierarchy: CR > PR > SD > PD
                    # Only CONFIRMED CR/PR count for BOR
                    has_confirmed_cr <- any(confirmed_only$overall_response_unconfirmed == "CR")
                    has_confirmed_pr <- any(confirmed_only$overall_response_unconfirmed == "PR")
                    has_sd <- any(confirmed_only$overall_response_unconfirmed == "SD")
                    has_pd <- any(confirmed_only$overall_response_unconfirmed == "PD")

                    if (has_confirmed_cr) {
                        bor_results$bestOverallResponse[i] <- "CR"
                        bor_results$borConfirmed[i] <- "Yes"
                        first_cr_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
                        bor_results$borFirstVisit[i] <- paste0("Visit ", first_cr_visit)
                        bor_results$timeToResponse[i] <- first_cr_visit - min(pt_data$visitTime)
                    } else if (has_confirmed_pr) {
                        bor_results$bestOverallResponse[i] <- "PR"
                        bor_results$borConfirmed[i] <- "Yes"
                        first_pr_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
                        bor_results$borFirstVisit[i] <- paste0("Visit ", first_pr_visit)
                        bor_results$timeToResponse[i] <- first_pr_visit - min(pt_data$visitTime)
                    } else if (has_sd) {
                        bor_results$bestOverallResponse[i] <- "SD"
                        bor_results$borConfirmed[i] <- "Yes"
                        first_sd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "SD"])
                        bor_results$borFirstVisit[i] <- paste0("Visit ", first_sd_visit)
                        bor_results$timeToResponse[i] <- NA
                    } else if (has_pd) {
                        bor_results$bestOverallResponse[i] <- "PD"
                        bor_results$borConfirmed[i] <- "Yes"
                        first_pd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PD"])
                        bor_results$borFirstVisit[i] <- paste0("Visit ", first_pd_visit)
                        bor_results$timeToResponse[i] <- NA
                    } else {
                        # No confirmed response
                        bor_results$bestOverallResponse[i] <- "Not Evaluable"
                        bor_results$borConfirmed[i] <- "No"
                        bor_results$borFirstVisit[i] <- "N/A"
                        bor_results$timeToResponse[i] <- NA
                    }

                    # Check for progression
                    if (has_pd) {
                        bor_results$progressionOccurred[i] <- "Yes"
                        first_pd_visit <- min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PD"])
                        bor_results$progressionVisit[i] <- paste0("Visit ", first_pd_visit)

                        # Duration of response (if had CR/PR before PD)
                        if (has_confirmed_cr || has_confirmed_pr) {
                            response_visit <- if (has_confirmed_cr) {
                                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
                            } else {
                                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
                            }
                            bor_results$durationOfResponse[i] <- first_pd_visit - response_visit
                        } else {
                            bor_results$durationOfResponse[i] <- NA
                        }
                    } else {
                        bor_results$progressionOccurred[i] <- "No"
                        bor_results$progressionVisit[i] <- "N/A"

                        # Duration = last visit - first response (censored)
                        if (has_confirmed_cr || has_confirmed_pr) {
                            response_visit <- if (has_confirmed_cr) {
                                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "CR"])
                            } else {
                                min(confirmed_only$visitTime[confirmed_only$overall_response_unconfirmed == "PR"])
                            }
                            bor_results$durationOfResponse[i] <- max(pt_data$visitTime) - response_visit
                        } else {
                            bor_results$durationOfResponse[i] <- NA
                        }
                    }
                }

                return(bor_results)
            },

            # Output Population ====

            .initLesionTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },

            .populateLesionTable = function(lesion_data) {
                table <- self$results$lesionTable

                # Calculate baseline diameters for percent change
                baseline_diameters <- lesion_data[lesion_data$isBaseline, c("patientID", "lesionID", "diameter")]
                names(baseline_diameters)[3] <- "baseline_diameter"

                # Merge baseline with all data
                lesion_data_full <- merge(lesion_data, baseline_diameters,
                                          by = c("patientID", "lesionID"),
                                          all.x = TRUE)

                # Calculate changes
                lesion_data_full$changeFromBaseline <- lesion_data_full$diameter - lesion_data_full$baseline_diameter
                lesion_data_full$percentChange <- ifelse(
                    lesion_data_full$baseline_diameter > 0,
                    (lesion_data_full$diameter - lesion_data_full$baseline_diameter) / lesion_data_full$baseline_diameter * 100,
                    NA
                )

                # Add visit numbers
                lesion_data_full <- lesion_data_full[order(lesion_data_full$patientID,
                                                           lesion_data_full$lesionID,
                                                           lesion_data_full$visitTime), ]
                lesion_data_full$visitNumber <- ave(
                    lesion_data_full$visitTime,
                    paste(lesion_data_full$patientID, lesion_data_full$lesionID),
                    FUN = seq_along
                )

                # Populate table
                for (i in seq_len(nrow(lesion_data_full))) {
                    row <- list(
                        patientID = lesion_data_full$patientID[i],
                        lesionID = lesion_data_full$lesionID[i],
                        visitTime = lesion_data_full$visitTime[i],
                        visitNumber = lesion_data_full$visitNumber[i],
                        lesionType = lesion_data_full$lesionType[i],
                        location = lesion_data_full$location[i],
                        diameter = lesion_data_full$diameter[i],
                        changeFromBaseline = lesion_data_full$changeFromBaseline[i],
                        percentChange = lesion_data_full$percentChange[i]
                    )
                    table$addRow(rowKey = i, values = row)
                }
            },

            .initTargetSumTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },

            .populateTargetSumTable = function(target_sums, confirmed_responses) {
                table <- self$results$targetSumTable

                # Merge confirmation status
                target_sums_full <- merge(
                    target_sums,
                    confirmed_responses[, c("patientID", "visitTime", "response_confirmed")],
                    by = c("patientID", "visitTime"),
                    all.x = TRUE
                )

                # Populate table
                for (i in seq_len(nrow(target_sums_full))) {
                    row <- list(
                        patientID = target_sums_full$patientID[i],
                        visitTime = target_sums_full$visitTime[i],
                        visitNumber = target_sums_full$visitNumber[i],
                        nTargetLesions = target_sums_full$nTargetLesions[i],
                        baselineSum = target_sums_full$baseline_sum[i],
                        currentSum = target_sums_full$current_sum[i],
                        absoluteChange = target_sums_full$absolute_change[i],
                        percentChange = target_sums_full$percent_change[i],
                        targetResponse = target_sums_full$target_response[i],
                        responseConfirmed = ifelse(target_sums_full$response_confirmed[i], "Yes", "No")
                    )
                    table$addRow(rowKey = i, values = row)
                }
            },

            .initBestResponseTable = function() {
                # Table is defined in .r.yaml, no initialization needed
            },

            .populateBestResponseTable = function(best_responses) {
                table <- self$results$bestResponseTable

                for (i in seq_len(nrow(best_responses))) {
                    row <- best_responses[i, ]
                    table$addRow(rowKey = i, values = as.list(row))
                }
            },

            .populateRecistSummary = function(best_responses) {
                # Calculate ORR (Objective Response Rate: CR + PR)
                n_total <- nrow(best_responses)
                n_cr <- sum(best_responses$bestOverallResponse == "CR")
                n_pr <- sum(best_responses$bestOverallResponse == "PR")
                n_orr <- n_cr + n_pr
                orr_rate <- if (n_total > 0) n_orr / n_total * 100 else 0

                # 95% CI for binomial proportion (Wilson score interval)
                orr_ci <- if (n_total > 0) {
                    prop_test <- prop.test(n_orr, n_total, correct = FALSE)
                    paste0(round(prop_test$conf.int[1] * 100, 1), "% - ",
                           round(prop_test$conf.int[2] * 100, 1), "%")
                } else {
                    "N/A"
                }

                # Populate ORR table
                orr_table <- self$results$recistSummary$orrConfirmed
                orr_table$addRow(rowKey = 1, values = list(
                    metric = "ORR (CR + PR)",
                    value = paste0(n_orr, "/", n_total, " (", round(orr_rate, 1), "%)"),
                    ci = orr_ci,
                    interpretation = if (orr_rate >= 20) "Promising activity" else "Limited activity"
                ))

                # Calculate DCR (Disease Control Rate: CR + PR + SD)
                n_sd <- sum(best_responses$bestOverallResponse == "SD")
                n_dcr <- n_cr + n_pr + n_sd
                dcr_rate <- if (n_total > 0) n_dcr / n_total * 100 else 0

                dcr_ci <- if (n_total > 0) {
                    prop_test <- prop.test(n_dcr, n_total, correct = FALSE)
                    paste0(round(prop_test$conf.int[1] * 100, 1), "% - ",
                           round(prop_test$conf.int[2] * 100, 1), "%")
                } else {
                    "N/A"
                }

                # Populate DCR table
                dcr_table <- self$results$recistSummary$dcrConfirmed
                dcr_table$addRow(rowKey = 1, values = list(
                    metric = "DCR (CR + PR + SD)",
                    value = paste0(n_dcr, "/", n_total, " (", round(dcr_rate, 1), "%)"),
                    ci = dcr_ci,
                    interpretation = if (dcr_rate >= 50) "Good disease control" else "Poor disease control"
                ))

                # Response distribution
                dist_table <- self$results$recistSummary$responseDistribution
                response_categories <- c("CR", "PR", "SD", "PD", "Not Evaluable")

                for (i in seq_along(response_categories)) {
                    cat <- response_categories[i]
                    n_cat <- sum(best_responses$bestOverallResponse == cat)
                    pct_cat <- if (n_total > 0) n_cat / n_total * 100 else 0

                    dist_table$addRow(rowKey = i, values = list(
                        category = cat,
                        confirmed = n_cat,
                        confirmedPercent = pct_cat,
                        unconfirmed = 0,  # Placeholder
                        unconfirmedPercent = 0
                    ))
                }
            },

            .initComplianceReport = function() {
                # HTML output is defined in .r.yaml
            },

            .populateComplianceReport = function(target_validation, new_lesions, confirmed_responses) {
                html_content <- "<div style='font-family: Arial, sans-serif;'>"

                # Section 1: Target Lesion Selection Compliance
                html_content <- paste0(html_content,
                    "<h3>1. Target Lesion Selection Compliance</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Maximum ", self$options$maxTargetLesions,
                    " target lesions per patient</li>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Maximum ", self$options$maxLesionsPerOrgan,
                    " target lesions per organ</li>",
                    "<li><strong>RECIST v1.1 Rule:</strong> Minimum diameter ≥10mm (non-lymph nodes) or ≥15mm (lymph nodes)</li>"
                )

                if (target_validation$valid) {
                    html_content <- paste0(html_content,
                        "<li style='color: green;'><strong>STATUS: ✓ COMPLIANT</strong> - All patients meet RECIST v1.1 target lesion criteria</li>")
                } else {
                    html_content <- paste0(html_content,
                        "<li style='color: red;'><strong>STATUS: ✗ NON-COMPLIANT</strong> - ",
                        length(target_validation$violations), " violation(s) detected:</li>",
                        "<ul style='color: red;'>")
                    for (violation in target_validation$violations) {
                        html_content <- paste0(html_content, "<li>", violation, "</li>")
                    }
                    html_content <- paste0(html_content, "</ul>")
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 2: New Lesion Detection
                html_content <- paste0(html_content,
                    "<h3>2. New Lesion Detection</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> ANY new lesion automatically indicates Progressive Disease (PD)</li>"
                )

                if (nrow(new_lesions) == 0) {
                    html_content <- paste0(html_content,
                        "<li style='color: green;'><strong>STATUS:</strong> No new lesions detected</li>")
                } else {
                    html_content <- paste0(html_content,
                        "<li style='color: orange;'><strong>STATUS:</strong> ", nrow(new_lesions),
                        " patient(s) with new lesions detected (automatically classified as PD):</li>",
                        "<ul>")
                    for (i in seq_len(nrow(new_lesions))) {
                        html_content <- paste0(html_content,
                            "<li>Patient ", new_lesions$patientID[i],
                            " - New lesion at visit ", new_lesions$first_new_lesion_visit[i],
                            " (Location: ", new_lesions$new_lesion_location[i], ")</li>")
                    }
                    html_content <- paste0(html_content, "</ul>")
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 3: Response Confirmation Summary
                html_content <- paste0(html_content,
                    "<h3>3. Response Confirmation Summary</h3>",
                    "<ul>",
                    "<li><strong>RECIST v1.1 Rule:</strong> CR and PR must be confirmed by repeat assessment ≥",
                    self$options$confirmationInterval, " weeks after initial documentation</li>"
                )

                cr_pr_responses <- confirmed_responses[
                    confirmed_responses$overall_response_unconfirmed %in% c("CR", "PR"), ]

                if (nrow(cr_pr_responses) > 0) {
                    n_confirmed <- sum(cr_pr_responses$response_confirmed)
                    n_unconfirmed <- sum(!cr_pr_responses$response_confirmed)

                    html_content <- paste0(html_content,
                        "<li><strong>Confirmed CR/PR:</strong> ", n_confirmed, " assessment(s)</li>",
                        "<li><strong>Unconfirmed CR/PR:</strong> ", n_unconfirmed,
                        " assessment(s) (awaiting confirmation or lost)</li>")
                } else {
                    html_content <- paste0(html_content,
                        "<li>No CR or PR responses documented</li>")
                }
                html_content <- paste0(html_content, "</ul>")

                # Section 4: Overall Compliance Summary
                html_content <- paste0(html_content,
                    "<h3>4. Overall RECIST v1.1 Compliance Summary</h3>",
                    "<p style='padding: 10px; background-color: #f0f0f0; border-left: 4px solid ",
                    ifelse(target_validation$valid, "green", "orange"), ";'>",
                    "<strong>Compliance Status:</strong> ",
                    ifelse(target_validation$valid,
                           "This analysis meets RECIST v1.1 protocol requirements and may be suitable for regulatory submissions.",
                           "This analysis contains protocol deviations. Review violations above and consider data quality implications."),
                    "</p>",
                    "</div>"
                )

                self$results$complianceReport$setContent(html_content)
            },

            # Plot Generation ====

            .initWaterfallPlot = function() {
                image <- self$results$waterfallPlot
                image$setSize(800, 600)
            },

            .prepareWaterfallPlot = function(best_responses, target_sums) {
                image <- self$results$waterfallPlot

                # Calculate best percent change for each patient (nadir)
                nadir_data <- aggregate(
                    percent_change ~ patientID,
                    data = target_sums,
                    FUN = function(x) x[which.min(abs(x))[1]]  # Closest to 0 or most negative
                )
                names(nadir_data)[2] <- "best_change"

                # Merge with best response
                plot_data <- merge(nadir_data, best_responses, by = "patientID")

                # Sort by best change (most negative to most positive)
                plot_data <- plot_data[order(plot_data$best_change), ]
                plot_data$patient_order <- seq_len(nrow(plot_data))

                # Store for rendering
                state <- list(
                    plot_data = as.data.frame(plot_data),
                    color_scheme = self$options$colorScheme,
                    pr_threshold = private$RECIST_PR_THRESHOLD,
                    pd_threshold = private$RECIST_PD_THRESHOLD
                )

                image$setState(state)
            },

            .waterfallPlot = function(image, ...) {
                if (is.null(image$state))
                    return(FALSE)

                plot_data <- image$state$plot_data
                color_scheme <- image$state$color_scheme
                pr_threshold <- image$state$pr_threshold
                pd_threshold <- image$state$pd_threshold

                # Define colors
                if (color_scheme == "recist") {
                    colors <- c("CR" = "#00A087", "PR" = "#4DBBD5", "SD" = "#F39B7F", "PD" = "#E64B35", "Not Evaluable" = "#8491B4")
                } else if (color_scheme == "colorblind") {
                    colors <- c("CR" = "#009E73", "PR" = "#0072B2", "SD" = "#F0E442", "PD" = "#D55E00", "Not Evaluable" = "#999999")
                } else {  # jamovi
                    colors <- c("CR" = "#3498DB", "PR" = "#2ECC71", "SD" = "#F39C12", "PD" = "#E74C3C", "Not Evaluable" = "#95A5A6")
                }

                # Create plot
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = patient_order, y = best_change, fill = bestOverallResponse)) +
                    ggplot2::geom_col(width = 0.8) +
                    ggplot2::geom_hline(yintercept = pr_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = pd_threshold, linetype = "dashed", color = "darkred", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                    ggplot2::scale_fill_manual(values = colors, name = "Best Overall Response") +
                    ggplot2::labs(
                        title = "RECIST v1.1 Waterfall Plot",
                        subtitle = "Best Confirmed Percent Change from Baseline",
                        x = "Patient (sorted by response)",
                        y = "Best % Change in Target Lesion Sum"
                    ) +
                    ggplot2::theme_minimal(base_size = 12) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank(),
                        panel.grid.major.x = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank()
                    ) +
                    ggplot2::annotate("text", x = nrow(plot_data) * 0.05, y = pr_threshold - 5,
                                      label = "PR (-30%)", hjust = 0, color = "darkgreen", size = 3) +
                    ggplot2::annotate("text", x = nrow(plot_data) * 0.05, y = pd_threshold + 5,
                                      label = "PD (+20%)", hjust = 0, color = "darkred", size = 3)

                print(plot)
                return(TRUE)
            },

            .initSpiderPlot = function() {
                image <- self$results$spiderPlot
                image$setSize(800, 600)
            },

            .prepareSpiderPlot = function(target_sums, best_responses) {
                image <- self$results$spiderPlot

                # Merge best response for coloring
                plot_data <- merge(target_sums, best_responses[, c("patientID", "bestOverallResponse")],
                                   by = "patientID")

                # Store for rendering
                state <- list(
                    plot_data = as.data.frame(plot_data),
                    color_scheme = self$options$colorScheme,
                    pr_threshold = private$RECIST_PR_THRESHOLD,
                    pd_threshold = private$RECIST_PD_THRESHOLD
                )

                image$setState(state)
            },

            .spiderPlot = function(image, ...) {
                if (is.null(image$state))
                    return(FALSE)

                plot_data <- image$state$plot_data
                color_scheme <- image$state$color_scheme
                pr_threshold <- image$state$pr_threshold
                pd_threshold <- image$state$pd_threshold

                # Define colors
                if (color_scheme == "recist") {
                    colors <- c("CR" = "#00A087", "PR" = "#4DBBD5", "SD" = "#F39B7F", "PD" = "#E64B35", "Not Evaluable" = "#8491B4")
                } else if (color_scheme == "colorblind") {
                    colors <- c("CR" = "#009E73", "PR" = "#0072B2", "SD" = "#F0E442", "PD" = "#D55E00", "Not Evaluable" = "#999999")
                } else {  # jamovi
                    colors <- c("CR" = "#3498DB", "PR" = "#2ECC71", "SD" = "#F39C12", "PD" = "#E74C3C", "Not Evaluable" = "#95A5A6")
                }

                # Create plot
                plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = visitTime, y = percent_change,
                                                                 group = patientID, color = bestOverallResponse)) +
                    ggplot2::geom_line(linewidth = 0.8, alpha = 0.7) +
                    ggplot2::geom_point(size = 2, alpha = 0.7) +
                    ggplot2::geom_hline(yintercept = pr_threshold, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = pd_threshold, linetype = "dashed", color = "darkred", linewidth = 0.8) +
                    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
                    ggplot2::scale_color_manual(values = colors, name = "Best Overall Response") +
                    ggplot2::labs(
                        title = "RECIST v1.1 Spider Plot",
                        subtitle = "Target Lesion Sum Trajectories Over Time",
                        x = "Time from Baseline",
                        y = "% Change in Target Lesion Sum"
                    ) +
                    ggplot2::theme_minimal(base_size = 12) +
                    ggplot2::theme(
                        legend.position = "bottom",
                        plot.title = ggplot2::element_text(face = "bold", size = 14),
                        panel.grid.minor = ggplot2::element_blank()
                    ) +
                    ggplot2::annotate("text", x = max(plot_data$visitTime) * 0.95, y = pr_threshold - 5,
                                      label = "PR (-30%)", hjust = 1, color = "darkgreen", size = 3) +
                    ggplot2::annotate("text", x = max(plot_data$visitTime) * 0.95, y = pd_threshold + 5,
                                      label = "PD (+20%)", hjust = 1, color = "darkred", size = 3)

                print(plot)
                return(TRUE)
            },

            # Utility Methods ====

            .addBestResponseColumn = function(best_responses) {
                # TODO: Add BOR as new variable to original dataset
                # NOTE: Merge best_responses back to patient-level
                # New column: "RECIST_BestOverallResponse"
            },

            .escapeVar = function(x) {
                # Escape variable names with spaces/special characters for safe R usage
                if (is.null(x) || length(x) == 0 || x == "") {
                    return(x)
                }
                safe_name <- make.names(x)
                safe_name <- gsub("[^A-Za-z0-9_]+", "_", safe_name)
                return(safe_name)
            }
        )
    )
}
