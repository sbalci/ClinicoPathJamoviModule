#' @title RECIST 1.1 Multi-Lesion Aggregation
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr mutate filter group_by arrange summarise n ungroup lead lag row_number slice
#' @importFrom tidyr pivot_wider
#' @importFrom stats binom.test
#' @export

recistClass <- R6::R6Class(
    "recistClass",
    inherit = recistBase,
    private = list(
        # Data storage
        .rawData = NULL,
        .lesionData = NULL,
        .targetSumData = NULL,
        .responseData = NULL,
        .bestResponseData = NULL,
        .baselineData = NULL,
        .summaryStatus = list(
             warnings = character(),
             exclusions = character(),
             n_patients = 0,
             n_lesions_total = 0,
             n_lesions_tracked = 0
        ),

        # ---- Initialization ----
        .init = function() {

            if (is.null(self$options$patientId) ||
                is.null(self$options$assessmentTime) ||
                is.null(self$options$lesionId) ||
                is.null(self$options$lesionType)) {

                self$results$instructions$setContent(
                    "<h3>Welcome to RECIST 1.1 Multi-Lesion Aggregation</h3>
                    <p>This module automates the aggregation of individual lesion measurements into
                    patient-level response assessments per <b>RECIST 1.1</b> criteria.</p>

                    <p><b>Key Features:</b></p>
                    <ul>
                    <li><b>Automated Target Lesion Selection:</b> Applies max 5 lesions, max 2 per organ rule</li>
                    <li><b>Target Lesion Sum Calculation:</b> Automatic aggregation by assessment</li>
                    <li><b>RECIST Response Classification:</b> CR, PR, SD, PD based on target, non-target, and new lesions</li>
                    <li><b>Customizable Criteria:</b> Configurable non-target status values and SD minimum duration</li>
                    <li><b>Best Overall Response:</b> Automatic BOR calculation with confirmation requirements</li>
                    <li><b>Lesion Trajectories:</b> Individual lesion plots showing tumor dynamics</li>
                    <li><b>Waterfall Plot:</b> Best percent change visualization</li>
                    <li><b>Efficacy Metrics:</b> ORR (CR + PR), DCR (CR + PR + SD)</li>
                    </ul>

                    <p><b>Required Variables:</b></p>
                    <ul>
                    <li><b>Patient ID:</b> Unique patient identifier</li>
                    <li><b>Assessment Time:</b> Time from baseline</li>
                    <li><b>Lesion ID:</b> Unique lesion identifier (e.g., L1, L2, Liver_1)</li>
                    <li><b>Lesion Type:</b> target, non-target, or new</li>
                    <li><b>Lesion Diameter:</b> Diameter in mm (for target lesions)</li>
                    </ul>

                    <p><b>Optional Variables:</b></p>
                    <ul>
                    <li><b>Non-Target Status:</b> present, absent, or unequivocal PD (values customizable)</li>
                    <li><b>Organ:</b> Anatomic site (enforces max 2 per organ)</li>
                    <li><b>Grouping Variable:</b> For stratified analysis</li>
                    </ul>

                    <p><b>Data Format:</b> Long format with one row per lesion per assessment</p>

                    <p><b>RECIST 1.1 Criteria:</b></p>
                    <ul>
                    <li><b>CR:</b> All target lesions disappear</li>
                    <li><b>PR:</b> ≥30% decrease in target lesion sum from baseline</li>
                    <li><b>PD:</b> ≥20% increase in sum from nadir + 5mm absolute</li>
                    <li><b>SD:</b> Neither PR nor PD criteria met (requires min duration, default 6 weeks)</li>
                    <li><b>Confirmation:</b> CR/PR requires confirmation on ≥2 consecutive scans</li>
                    </ul>

                    <p><b>Reference:</b> Eisenhauer EA, et al. New response evaluation criteria in solid tumours:
                    revised RECIST guideline (version 1.1). Eur J Cancer. 2009;45(2):228-247.</p>

                    <p>Select variables to begin analysis.</p>"
                )
                return()
            }

            self$results$lesionTable$setVisible(self$options$showLesionTable)
            self$results$targetSumTable$setVisible(self$options$showTargetSumTable)
            self$results$responseTable$setVisible(self$options$showResponseTable)
            self$results$bestResponseTable$setVisible(self$options$showBestResponse)
            self$results$lesionPlot$setVisible(self$options$showLesionPlot)
            self$results$sumPlot$setVisible(self$options$showSumPlot)
            self$results$waterfallPlot$setVisible(self$options$showWaterfallPlot)
            self$results$stratifiedTable$setVisible(
                self$options$stratifiedAnalysis && !is.null(self$options$groupVar)
            )
        },
        
        # ---- Helper: Add Warning ----
        .addWarning = function(msg) {
             private$.summaryStatus$warnings <- c(private$.summaryStatus$warnings, msg)
        },

        # ---- Main Analysis ----
        .run = function() {

            if (is.null(self$options$patientId) ||
                is.null(self$options$assessmentTime) ||
                is.null(self$options$lesionId) ||
                is.null(self$options$lesionType)) {
                return()
            }

            tryCatch({
                private$.prepareData()
                private$.selectTargetLesions()
                private$.calculateTargetSums()
                private$.classifyResponses()
                private$.calculateBestResponse()

                # Populate results
                private$.populateDataInfo()

                if (self$options$showLesionTable) {
                    private$.populateLesionTable()
                }

                if (self$options$showTargetSumTable) {
                    private$.populateTargetSumTable()
                }

                if (self$options$showResponseTable) {
                    private$.populateResponseTable()
                }

                if (self$options$showBestResponse) {
                    private$.populateBestResponseTable()
                    private$.populateSummaryStats()
                    private$.populateEfficacyMetrics()
                }

                private$.populateClinicalInterpretation()

                if (self$options$showReference) {
                    private$.populateReferenceInfo()
                }
                
                private$.populateRunSummary()

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<div class='error'>",
                           "<h4>Error in Analysis</h4>",
                           "<p>", e$message, "</p>",
                           "<p><b>Common issues:</b></p>",
                           "<ul>",
                           "<li>Ensure data is in long format (one row per lesion per assessment)</li>",
                           "<li>Check lesion type values (should be: target, non-target, new)</li>",
                           "<li>Verify diameter is numeric for target lesions</li>",
                           "<li>Ensure each patient has baseline assessment (earliest time)</li>",
                           "</ul></div>")
                )
                stop(e)
            })
        },

        # ---- Data Preparation ----
        .prepareData = function() {

            mydata <- self$data

            patientId <- self$options$patientId
            assessmentTime <- self$options$assessmentTime
            lesionId <- self$options$lesionId
            lesionType <- self$options$lesionType

            data <- data.frame(
                patientId = mydata[[patientId]],
                assessmentTime = as.numeric(mydata[[assessmentTime]]),
                lesionId = as.character(mydata[[lesionId]]),
                lesionType = tolower(as.character(mydata[[lesionType]])),
                stringsAsFactors = FALSE
            )

            # Add diameter if provided
            if (!is.null(self$options$lesionDiameter)) {
                lesionDiameter <- self$options$lesionDiameter
                data$diameter <- as.numeric(mydata[[lesionDiameter]])
            } else {
                data$diameter <- NA
            }

            # Add organ if provided
            if (!is.null(self$options$organ)) {
                organ <- self$options$organ
                data$organ <- as.character(mydata[[organ]])
            } else {
                data$organ <- "unknown"
            }

            # Add non-target status if provided
            if (!is.null(self$options$nonTargetStatus)) {
                nonTarget <- self$options$nonTargetStatus
                data$nonTargetStatus <- tolower(as.character(mydata[[nonTarget]]))
            } else {
                data$nonTargetStatus <- NA
            }

            # Sort by patient, time, lesion
            data <- data[order(data$patientId, data$assessmentTime, data$lesionId), ]

            private$.rawData <- data
        },

        # ---- Target Lesion Selection ----
        .selectTargetLesions = function() {

            data <- private$.rawData
            
            # Reset status
            private$.summaryStatus$n_patients <- length(unique(data$patientId))
            private$.summaryStatus$n_lesions_total <- length(unique(data$lesionId[data$lesionType == "target"]))

            # Mark target lesions for inclusion
            data$includedInSum <- FALSE
            data$exclusionReason <- NA_character_

            # Get baseline (first assessment per patient)
            baseline <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                filter(row_number() == 1) %>%
                ungroup()

            # For each patient, select largest valid target lesions at baseline
            tracked_lesions_list <- list()

            for (pid in unique(baseline$patientId)) {
                # Get target lesions at baseline
                patient_baseline <- baseline %>%
                    filter(patientId == pid, lesionType == "target") %>%
                    arrange(desc(diameter))
                
                # Check if patient has NO target lesions at baseline
                if (nrow(patient_baseline) == 0) {
                     private$.addWarning(paste0("Patient ", pid, ": No target lesions found at baseline."))
                     next
                }

                selected <- character(0)
                organs_count <- table(character(0))
                
                # Select up to max
                for (i in 1:nrow(patient_baseline)) {
                    lesion <- patient_baseline$lesionId[i]
                    organ <- patient_baseline$organ[i]

                    # CAPS: Max Total
                    if (length(selected) >= self$options$maxTargetLesions) {
                         data$exclusionReason[data$patientId == pid & data$lesionId == lesion] <- "Exceeds max total lesions"
                         next
                    }
                    
                    # CAPS: Max Per Organ
                    current_count <- organs_count[organ]
                    if (is.na(current_count)) current_count <- 0
                    
                    if (current_count >= self$options$maxPerOrgan) {
                         data$exclusionReason[data$patientId == pid & data$lesionId == lesion] <- "Exceeds max per organ"
                         next
                    }

                    selected <- c(selected, lesion)
                    organs_count[organ] <- current_count + 1
                    
                    # Track this lesion ID for this patient FOREVER
                    # (RECIST: "Target lesions should be selected at baseline and followed...")
                }
                
                tracked_lesions_list[[pid]] <- selected
                
                # Mark selected matches in the FULL dataset
                # Only mark if it MATCHES the ID selected at baseline
                data$includedInSum[data$patientId == pid & data$lesionId %in% selected] <- TRUE
                
                # Identify non-selected target lesions (orphans or late appearances)
                # Any target lesion for this patient NOT in 'selected' is excluded
                # Note: This handles "new" lesions appearing as 'target' type incorrectly by user - they should be 'new' type.
                # If they appear later with type='target', they are ignored from sum.
                
                # Log exclusions for baseline items
                excluded_at_baseline <- patient_baseline$lesionId[!patient_baseline$lesionId %in% selected]
                if (length(excluded_at_baseline) > 0) {
                     msg <- paste0("Patient ", pid, ": Lesions excluded at baseline (max limits): ", paste(excluded_at_baseline, collapse=", "))
                     private$.summaryStatus$exclusions <- c(private$.summaryStatus$exclusions, msg)
                }
            }
            
            # Global check for target lesions appearing mid-stream that were NOT at baseline
            # (Valid targets must be present at baseline)
            # We already set includedInSum=TRUE only for those matching baseline IDs.
            # So any target lesion with includedInSum=FALSE is effectively excluded.
            
            private$.summaryStatus$n_lesions_tracked <- sum(data$includedInSum & data$assessmentTime == min(data$assessmentTime)) 
            private$.lesionData <- data
        },

        # ---- Calculate Target Sums ----
        .calculateTargetSums = function() {

            data <- private$.lesionData

            # Calculate target sum per patient per assessment
            targetSums <- data %>%
                filter(includedInSum == TRUE) %>%
                group_by(patientId, assessmentTime) %>%
                summarise(
                    targetSum = if(any(is.na(diameter))) NA_real_ else sum(diameter, na.rm=TRUE),
                    # NE Logic: If any expected target lesion is missing (NA), sum is suspect.
                    # We need to know if ALL tracked lesions are present.
                    # This simple group_by might miss lesions that are completely missing rows for a timepoint.
                    # Strict way: Count tracked lesions per patient.
                    n_present = n(), 
                    .groups = "drop"
                )
            
            # Post-hoc strictness: 
            # We need to look up how many lesions were tracked for each patient at baseline.
            baseline_counts <- data %>%
                 filter(includedInSum == TRUE) %>%
                 group_by(patientId) %>%
                 summarise(n_tracked = length(unique(lesionId)), .groups="drop")
            
            targetSums <- targetSums %>%
                 left_join(baseline_counts, by="patientId") %>%
                 mutate(
                      is_missing_lesions = n_present < n_tracked,
                      targetSum = ifelse(is_missing_lesions, NA_real_, targetSum)
                 )
            
            if (any(targetSums$is_missing_lesions, na.rm=TRUE)) {
                 affected <- unique(targetSums$patientId[which(targetSums$is_missing_lesions)])
                 if (length(affected) > 0)
                    private$.addWarning(paste0("Incomplete target lesion data (NE) for patients: ", paste(head(affected, 5), collapse=", ")))
            }

            # Count non-target lesions
            nonTargetCounts <- data %>%
                filter(lesionType == "non-target") %>%
                group_by(patientId, assessmentTime) %>%
                summarise(numNonTarget = n(), .groups = "drop")

            # Detect new lesions
            newLesionFlags <- data %>%
                filter(lesionType == "new") %>%
                group_by(patientId, assessmentTime) %>%
                summarise(newLesions = "Yes", .groups = "drop")

            # Merge
            targetSums <- targetSums %>%
                left_join(nonTargetCounts, by = c("patientId", "assessmentTime")) %>%
                left_join(newLesionFlags, by = c("patientId", "assessmentTime")) %>%
                mutate(
                    numNonTarget = ifelse(is.na(numNonTarget), 0, numNonTarget),
                    newLesions = ifelse(is.na(newLesions), "No", newLesions)
                )

            # Calculate baseline for each patient
            baseline <- targetSums %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                filter(row_number() == 1) %>%
                select(patientId, baselineSum = targetSum, baselineTime = assessmentTime)

            targetSums <- targetSums %>%
                left_join(baseline, by = "patientId") %>%
                mutate(changeFromBaseline = ((targetSum - baselineSum) / baselineSum) * 100)

            # Calculate nadir if requested
            if (self$options$nadirReference) {
                targetSums <- targetSums %>%
                    group_by(patientId) %>%
                    arrange(assessmentTime) %>%
                    mutate(
                        nadirSum = cummin(targetSum),
                        nadirSum = ifelse(assessmentTime == baselineTime, baselineSum, nadirSum),
                        changeFromNadir = ((targetSum - nadirSum) / nadirSum) * 100,
                        absoluteChange = targetSum - nadirSum
                    ) %>%
                    ungroup()
            }

            private$.targetSumData <- targetSums
            private$.baselineData <- baseline
        },

        # ---- Classify Responses ----
        .classifyResponses = function() {

            data <- private$.targetSumData
            lesionData <- private$.lesionData

            prThreshold <- -self$options$prThreshold
            pdThreshold <- self$options$pdThreshold
            pdAbsolute <- self$options$pdAbsolute
            
            # Parse custom non-target status strings
            nt_cr_strings <- trimws(unlist(strsplit(self$options$nonTargetCR, ",")))
            nt_pd_strings <- trimws(unlist(strsplit(self$options$nonTargetPD, ",")))
            
            if (length(nt_cr_strings) == 0) nt_cr_strings <- c("absent", "disappeared")
            if (length(nt_pd_strings) == 0) nt_pd_strings <- c("pd", "progression", "unequivocal")
            
            nt_cr_strings <- tolower(nt_cr_strings)
            nt_pd_strings <- tolower(nt_pd_strings)

            # Classify target lesion response
            data <- data %>%
                mutate(
                    targetResponse = case_when(
                        is.na(targetSum) ~ "NE",
                        targetSum == 0 ~ "CR",
                        changeFromBaseline <= prThreshold ~ "PR",
                        !is.na(changeFromNadir) & changeFromNadir >= pdThreshold & absoluteChange >= pdAbsolute ~ "PD",
                        is.na(changeFromNadir) & changeFromBaseline >= pdThreshold & (targetSum - baselineSum) >= pdAbsolute ~ "PD",
                        TRUE ~ "SD"
                    )
                )

            # Classify non-target response
            nonTargetResponses <- lesionData %>%
                filter(lesionType == "non-target") %>%
                group_by(patientId, assessmentTime) %>%
                summarise(
                    nonTargetResponse = {
                        statuses <- tolower(nonTargetStatus)
                        if (all(statuses %in% nt_cr_strings)) {
                            "CR"
                        } else if (any(statuses %in% nt_pd_strings)) {
                            "PD"
                        } else {
                            "non-CR/non-PD"
                        }
                    },
                    .groups = "drop"
                )

            data <- data %>%
                left_join(nonTargetResponses, by = c("patientId", "assessmentTime")) %>%
                mutate(
                    nonTargetResponse = ifelse(is.na(nonTargetResponse), "N/A", nonTargetResponse)
                )

             data <- data %>%
                 mutate(
                    overallResponse = case_when(
                        # 1. New Lesions = PD ALWAYS
                        newLesions == "Yes" ~ "PD",

                        # 2. Non-target PD = PD ALWAYS
                        nonTargetResponse == "PD" ~ "PD",
                        
                        # 3. Target PD = PD
                        targetResponse == "PD" ~ "PD",
                        
                        # 4. If Target is NE, and no PD above -> NE
                        targetResponse == "NE" ~ "NE",
                        
                        # 5. Non-target CR logic
                        targetResponse == "CR" & nonTargetResponse %in% c("CR", "N/A") ~ "CR",
                        
                        # 6. PR
                        targetResponse == "PR" & nonTargetResponse %in% c("CR", "non-CR/non-PD", "N/A") ~ "PR",
                        
                        # 7. SD
                        targetResponse %in% c("SD", "PR") & nonTargetResponse == "non-CR/non-PD" ~ "SD",
                        
                        TRUE ~ "SD" # Fallback, usually implies target=SD and NT=missing/ok
                    ),
                    
                    confirmed = "No"
                 )
                 
                 # Apply confirmation logic if required
                 if (self$options$requireConfirmation) {
                      data <- private$.applyConfirmation(data)
                 }
                 
                 private$.responseData <- data
        },

        # ---- Confirmation Logic ----
        .applyConfirmation = function(data) {

            confirmWindow <- self$options$confirmationWindow

            data <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                mutate(
                    nextTime = lead(assessmentTime),
                    nextResponse = lead(overallResponse),
                    timeDiff = nextTime - assessmentTime,

                    confirmed = case_when(
                        overallResponse %in% c("CR", "PR") &
                            !is.na(nextResponse) &
                            nextResponse == overallResponse &
                            timeDiff >= confirmWindow ~ "Yes",

                        overallResponse %in% c("CR", "PR") ~ "Pending",

                        TRUE ~ "-"
                    )
                ) %>%
                ungroup()

            return(data)
        },

        # ---- Best Overall Response ----
        .calculateBestResponse = function() {

            data <- private$.responseData
            sdMinDuration <- self$options$sdMinDuration

            # Response hierarchy: CR > PR > SD > PD
            bestResponse <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                summarise(
                    bestResponse = {
                        # Get baseline time for this patient
                        baseline_t <- min(assessmentTime)
                        
                        # Filter out baseline assessments for response evaluation
                        post_baseline <- data.frame(
                            response = overallResponse[assessmentTime > baseline_t],
                            time = assessmentTime[assessmentTime > baseline_t],
                            confirmed = confirmed[assessmentTime > baseline_t],
                            stringsAsFactors = FALSE
                        )
                        
                        if (nrow(post_baseline) == 0) {
                            "NE"
                        } else {
                            # Check for PD
                            first_pd_idx <- which(post_baseline$response == "PD")[1]
                            
                            # If PD exists, only consider responses BEFORE the first PD
                            # RECIST: Best response is the best response recorded from the start of the treatment until disease progression/recurrence.
                            if (!is.na(first_pd_idx)) {
                                valid_window <- post_baseline[1:(first_pd_idx-1), ]
                                has_pd <- TRUE
                            } else {
                                valid_window <- post_baseline
                                has_pd <- FALSE
                            }
                            
                            # Logic for CR/PR confirmation
                            if (self$options$requireConfirmation) {
                                # CR must be confirmed
                                has_cr <- any(valid_window$response == "CR" & valid_window$confirmed == "Yes")
                                # PR must be confirmed
                                has_pr <- any(valid_window$response == "PR" & valid_window$confirmed == "Yes")
                            } else {
                                has_cr <- any(valid_window$response == "CR")
                                has_pr <- any(valid_window$response == "PR")
                            }
                            
                            # Logic for SD duration
                            # SD must be met at least once at time >= sdMinDuration (from baseline)
                            # We consider all SDs in the valid window (before PD)
                            # Note: We check if *any* assessment in the valid window is SD (or better? No, just SD/PR/CR qualify for stability) 
                            # but specifically looking for SD classification here.
                            # Actually, if a patient has PR, they technically met SD criteria too, but PR trumps SD.
                            # We only care about SD if CR and PR are not achieved.
                            
                            # Find assessments in valid window that are SD (or unconfirmed CR/PR which revert to SD if duration met?)
                            # Standard practice: Unconfirmed PR/CR -> SD if duration met.
                            # Simplification: Check if any "SD", "PR" (unconfirmed), "CR" (unconfirmed) exists >= min duration
                            
                            candidates_for_sd <- valid_window[valid_window$response %in% c("SD", "PR", "CR"), ]
                            has_sd <- any(candidates_for_sd$time - baseline_t >= sdMinDuration)
                            
                            if (has_cr) {
                                "CR"
                            } else if (has_pr) {
                                "PR"
                            } else if (has_sd) {
                                "SD"
                            } else if (has_pd) {
                                "PD"
                            } else {
                                "NE"
                            }
                        }
                    },

                    timeToResponse = {
                        idx <- which(overallResponse %in% c("CR", "PR"))
                        if (length(idx) > 0) assessmentTime[idx[1]] else NA
                    },

                    confirmed = {
                        if (bestResponse %in% c("CR", "PR") && self$options$requireConfirmation) {
                            # If BOR is CR/PR, it must have been confirmed based on logic above
                            "Yes"
                        } else {
                            "-"
                        }
                    },

                    nadirSum = min(targetSum, na.rm=TRUE),
                    bestPercentChange = min(changeFromBaseline, na.rm=TRUE),

                    .groups = "drop"
                )

            private$.bestResponseData <- bestResponse
        },

        # ---- Populate Results ----
        .populateDataInfo = function() {

            table <- self$results$dataInfo
            data <- private$.rawData

            nPatients <- length(unique(data$patientId))
            nLesions <- length(unique(data$lesionId))
            nAssessments <- length(unique(data$assessmentTime))

            targetLesions <- sum(private$.lesionData$includedInSum & private$.lesionData$lesionType == "target")
            targetLesionsUnique <- length(unique(private$.lesionData$lesionId[private$.lesionData$includedInSum == TRUE]))

            rows <- list(
                list(metric = "Total Patients", value = as.character(nPatients)),
                list(metric = "Total Lesions", value = as.character(nLesions)),
                list(metric = "Target Lesions Selected", value = as.character(targetLesionsUnique)),
                list(metric = "Number of Assessments", value = as.character(nAssessments))
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
        },

        .populateLesionTable = function() {

            table <- self$results$lesionTable
            data <- private$.lesionData

            for (i in 1:min(500, nrow(data))) {  # Limit to 500 rows
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    assessmentTime = data$assessmentTime[i],
                    lesionId = data$lesionId[i],
                    lesionType = data$lesionType[i],
                    diameter = if (!is.na(data$diameter[i])) data$diameter[i] else NA,
                    organ = data$organ[i],
                    included = if (data$includedInSum[i]) "Yes" else "No"
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        .populateTargetSumTable = function() {

            table <- self$results$targetSumTable
            data <- private$.targetSumData

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    assessmentTime = data$assessmentTime[i],
                    targetSum = data$targetSum[i],
                    changeFromBaseline = data$changeFromBaseline[i],
                    changeFromNadir = if (!is.na(data$changeFromNadir[i])) data$changeFromNadir[i] else NA,
                    numTargetLesions = as.integer(data$numTargetLesions[i]),
                    numNonTarget = as.integer(data$numNonTarget[i]),
                    newLesions = data$newLesions[i]
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        .populateResponseTable = function() {

            table <- self$results$responseTable
            data <- private$.responseData

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    assessmentTime = data$assessmentTime[i],
                    targetResponse = data$targetResponse[i],
                    nonTargetResponse = data$nonTargetResponse[i],
                    newLesions = data$newLesions[i],
                    overallResponse = data$overallResponse[i],
                    confirmed = data$confirmed[i]
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        .populateBestResponseTable = function() {

            table <- self$results$bestResponseTable
            data <- private$.bestResponseData

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    bestResponse = data$bestResponse[i],
                    timeToResponse = if (!is.na(data$timeToResponse[i])) data$timeToResponse[i] else NA,
                    confirmed = data$confirmed[i],
                    nadirSum = data$nadirSum[i],
                    bestPercentChange = data$bestPercentChange[i]
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        .populateSummaryStats = function() {

            table <- self$results$summaryStats
            data <- private$.bestResponseData

            responseCounts <- table(data$bestResponse)
            total <- nrow(data)

            for (category in names(responseCounts)) {
                n <- responseCounts[category]
                percent <- (n / total) * 100
                ci <- binom.test(n, total)$conf.int

                row <- list(
                    category = category,
                    n = as.integer(n),
                    percent = percent,
                    ci_lower = ci[1] * 100,
                    ci_upper = ci[2] * 100
                )

                table$addRow(rowKey = category, values = row)
            }
        },

        .populateEfficacyMetrics = function() {

            table <- self$results$efficacyMetrics
            data <- private$.bestResponseData

            total <- nrow(data)

            # ORR
            n_orr <- sum(data$bestResponse %in% c("CR", "PR"))
            orr <- (n_orr / total) * 100
            ci_orr <- binom.test(n_orr, total)$conf.int

            table$addRow(rowKey = "ORR", values = list(
                metric = "Objective Response Rate (ORR)",
                value = orr,
                n = as.integer(n_orr),
                total = as.integer(total),
                ci_lower = ci_orr[1] * 100,
                ci_upper = ci_orr[2] * 100
            ))

            # DCR
            n_dcr <- sum(data$bestResponse %in% c("CR", "PR", "SD"))
            dcr <- (n_dcr / total) * 100
            ci_dcr <- binom.test(n_dcr, total)$conf.int

            table$addRow(rowKey = "DCR", values = list(
                metric = "Disease Control Rate (DCR)",
                value = dcr,
                n = as.integer(n_dcr),
                total = as.integer(total),
                ci_lower = ci_dcr[1] * 100,
                ci_upper = ci_dcr[2] * 100
            ))
        },

        .populateClinicalInterpretation = function() {

            data <- private$.bestResponseData

            if (is.null(data) || nrow(data) == 0) return()

            total <- nrow(data)
            n_orr <- sum(data$bestResponse %in% c("CR", "PR"))
            n_dcr <- sum(data$bestResponse %in% c("CR", "PR", "SD"))

            orr <- round((n_orr / total) * 100, 1)
            dcr <- round((n_dcr / total) * 100, 1)

            html <- paste0(
                "<h4>Clinical Interpretation</h4>",
                "<p><b>Efficacy Summary:</b></p>",
                "<ul>",
                "<li><b>Objective Response Rate (ORR):</b> ", orr, "% (", n_orr, "/", total, " patients)</li>",
                "<li><b>Disease Control Rate (DCR):</b> ", dcr, "% (", n_dcr, "/", total, " patients)</li>",
                "</ul>",

                "<p><b>RECIST 1.1 Response Definitions:</b></p>",
                "<ul>",
                "<li><b>CR (Complete Response):</b> Disappearance of all target lesions + all non-target lesions</li>",
                "<li><b>PR (Partial Response):</b> ≥30% decrease in target lesion sum from baseline</li>",
                "<li><b>SD (Stable Disease):</b> Neither PR nor PD criteria met</li>",
                "<li><b>PD (Progressive Disease):</b> ≥20% increase in sum from nadir + 5mm, OR new lesions, OR unequivocal non-target PD</li>",
                "</ul>"
            )

            self$results$clinicalInterpretation$setContent(html)
        },

        .populateReferenceInfo = function() {

            html <- paste0(
                "<h4>RECIST 1.1 Guidelines Reference</h4>",
                "<p><b>Citation:</b> Eisenhauer EA, Therasse P, Bogaerts J, et al. ",
                "New response evaluation criteria in solid tumours: revised RECIST guideline (version 1.1). ",
                "<i>Eur J Cancer</i>. 2009;45(2):228-247.</p>",

                "<p><b>Target Lesion Selection Rules:</b></p>",
                "<ul>",
                "<li>Maximum ", self$options$maxTargetLesions, " target lesions total</li>",
                "<li>Maximum ", self$options$maxPerOrgan, " target lesions per organ</li>",
                "<li>Select largest measurable lesions at baseline</li>",
                "<li>Must be ≥10mm by CT (≥20mm by chest X-ray)</li>",
                "</ul>",

                "<p><b>Response Thresholds:</b></p>",
                "<ul>",
                "<li><b>PR:</b> ", self$options$prThreshold, "% decrease from baseline</li>",
                "<li><b>PD:</b> ", self$options$pdThreshold, "% increase from nadir + ",
                self$options$pdAbsolute, "mm absolute</li>",
                "<li><b>Confirmation:</b> CR/PR requires ≥", self$options$confirmationWindow, " weeks between assessments</li>",
                "</ul>"
            )

            self$results$referenceInfo$setContent(html)
        },

        
        .populateRunSummary = function() {
             
             status <- private$.summaryStatus
             
             html <- paste0(
                  "<div style='font-size: 13px;'>",
                  "<h4>Analysis Summary</h4>",
                  "<ul>",
                  "<li><b>Patients Processed:</b> ", status$n_patients, "</li>",
                  "<li><b>Total Lesions:</b> ", status$n_lesions_total, "</li>",
                  "<li><b>Tracked Target Lesions:</b> ", status$n_lesions_tracked, "</li>",
                  "</ul>"
             )
             
             if (length(status$exclusions) > 0) {
                  html <- paste0(html, "<h5 style='color: orange;'>Excluded Lesions (Rules)</h5><ul>")
                  for(m in status$exclusions) {
                       html <- paste0(html, "<li>", m, "</li>")
                  }
                  html <- paste0(html, "</ul>")
             }
             
             if (length(status$warnings) > 0) {
                  html <- paste0(html, "<h5 style='color: red;'>Warnings</h5><ul>")
                  for(w in status$warnings) {
                       html <- paste0(html, "<li>", w, "</li>")
                  }
                  html <- paste0(html, "</ul>")
             }
             
             html <- paste0(html, "</div>")
             self$results$runSummary$setContent(html)
        },

        # ---- Plotting Functions ----
        .lesionPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.lesionData)) return()

            data <- private$.lesionData %>%
                filter(includedInSum == TRUE, lesionType == "target")

            if (nrow(data) == 0) return()

            p <- ggplot(data, aes(x = assessmentTime, y = diameter, group = lesionId, color = patientId)) +
                geom_line(alpha = 0.6) +
                geom_point(alpha = 0.6, size = 2) +
                labs(
                    title = "Individual Target Lesion Trajectories",
                    x = "Time from Baseline",
                    y = "Lesion Diameter (mm)"
                ) +
                theme_minimal() +
                theme(legend.position = "none")

            print(p)
            TRUE
        },

        .sumPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.targetSumData)) return()

            data <- private$.targetSumData

            p <- ggplot(data, aes(x = assessmentTime, y = targetSum, group = patientId)) +
                geom_line(alpha = 0.6, color = "steelblue") +
                geom_point(alpha = 0.6, size = 2) +
                labs(
                    title = "Target Lesion Sum Over Time",
                    x = "Time from Baseline",
                    y = "Target Lesion Sum (mm)"
                ) +
                theme_minimal()

            print(p)
            TRUE
        },

        .waterfallPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bestResponseData)) return()

            data <- private$.bestResponseData %>%
                arrange(bestPercentChange) %>%
                mutate(patientOrder = row_number())

            colors <- c("CR" = "#00AA00", "PR" = "#66BB66", "SD" = "#FFDD00", "PD" = "#DD0000")

            p <- ggplot(data, aes(x = patientOrder, y = bestPercentChange, fill = bestResponse)) +
                geom_bar(stat = "identity", width = 0.8) +
                geom_hline(yintercept = 0, linetype = "solid", color = "black") +
                geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
                geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
                scale_fill_manual(values = colors, name = "Best Response") +
                labs(
                    title = "Waterfall Plot - Best Percent Change from Baseline",
                    x = "Patients (ordered by response)",
                    y = "Best % Change from Baseline"
                ) +
                theme_minimal() +
                theme(
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.position = "right"
                )

            print(p)
            TRUE
        }
    )
)
