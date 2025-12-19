#' @title iRECIST Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr mutate filter group_by arrange summarise n ungroup lead lag select left_join group_modify transmute bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom stats binom.test
#' @export

irecistClass <- R6::R6Class(
    "irecistClass",
    inherit = irecistBase,
    private = list(
        # Data storage
        .processedData = NULL,
        .responseData = NULL,
        .bestResponseData = NULL,
        .pseudoprogressionData = NULL,
        .baselineValues = NULL,
        .nadirValues = NULL,

        # ---- Initialization ----
        .init = function() {

            # Display instructions if no variables selected
            if (is.null(self$options$patientId) ||
                is.null(self$options$assessmentTime) ||
                is.null(self$options$targetLesionSum)) {

                # Add ERROR notice
                # Add ERROR notice
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".requiredVariables",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("Required variables missing: Please select Patient ID, Assessment Time, Target Lesion Sum, and New Lesions to begin analysis.")
                self$results$insert(1, notice)

                self$results$instructions$setContent(
                    "<h3>Welcome to iRECIST Analysis</h3>
                    <p>This module implements <b>immune-related Response Evaluation Criteria In Solid Tumors</b>
                    (iRECIST) for assessing tumor response in immunotherapy trials.</p>

                    <p><b>Key Features:</b></p>
                    <ul>
                    <li><b>iRECIST Categories:</b> iCR, iPR, iSD, iUPD (unconfirmed progression), iCPD (confirmed progression)</li>
                    <li><b>Pseudoprogression Detection:</b> Automatic flagging of iUPD requiring confirmation</li>
                    <li><b>Confirmation Requirements:</b> Implements 4-12 week confirmation window per Seymour et al. (2017)</li>
                    <li><b>Best Overall Response:</b> Calculates confirmed best response per patient</li>
                    <li><b>Waterfall Plot:</b> Visual representation of best response with iRECIST color coding</li>
                    <li><b>Swimmer Plot:</b> Timeline visualization with iUPD events marked</li>
                    <li><b>Efficacy Metrics:</b> ORR (iCR + iPR), DCR (iCR + iPR + iSD), pseudoprogression rate</li>
                    </ul>

                    <p><b>Required Variables:</b></p>
                    <ul>
                    <li><b>Patient ID:</b> Unique identifier for each patient</li>
                    <li><b>Assessment Time:</b> Time from baseline (weeks or months)</li>
                    <li><b>Target Lesion Sum:</b> Sum of target lesion diameters (mm)</li>
                    <li><b>New Lesions:</b> Binary indicator (0=no, 1=yes)</li>
                    </ul>

                    <p><b>Optional Variables:</b></p>
                    <ul>
                    <li><b>Non-Target Status:</b> Status of non-target lesions (CR, non-CR/non-PD, PD)</li>
                    <li><b>Tumor Burden:</b> Total tumor burden measure</li>
                    <li><b>Grouping Variable:</b> For stratified analysis (e.g., treatment arm)</li>
                    </ul>

                    <p><b>Data Format:</b> Long format with one row per assessment per patient</p>

                    <p><b>Clinical Interpretation:</b></p>
                    <ul>
                    <li><b>iUPD (unconfirmed PD):</b> Initial progression - may be pseudoprogression</li>
                    <li><b>iCPD (confirmed PD):</b> Progression confirmed on next scan ≥4 weeks later</li>
                    <li><b>Pseudoprogression:</b> iUPD followed by iPR, iSD, or iCR</li>
                    </ul>

                    <p><b>Reference:</b> Seymour L, et al. iRECIST: guidelines for response criteria
                    for use in trials testing immunotherapeutics. Lancet Oncol. 2017;18(3):e143-e152.</p>

                    <p>Select variables to begin analysis.</p>"
                )
                return()
            }

            # Initialize tables
            self$results$dataInfo$setVisible(TRUE)
            self$results$responseTable$setVisible(self$options$showResponseTable)
            self$results$bestResponseTable$setVisible(self$options$showBestResponse)
            self$results$waterfallPlot$setVisible(self$options$showWaterfallPlot)
            self$results$swimmerPlot$setVisible(self$options$showSwimmerPlot)
            self$results$spiderPlot$setVisible(self$options$showSpiderPlot)
            self$results$timeToCPDPlot$setVisible(self$options$showTimeToCPD)
            self$results$pseudoprogressionTable$setVisible(
                self$options$trackPseudoprogression && self$options$showPseudoprogressionRate
            )
            self$results$stratifiedTable$setVisible(
                self$options$stratifiedAnalysis && !is.null(self$options$groupVar)
            )
        },

        # ---- Main Analysis ----
        .run = function() {

            # Check if variables are selected
            if (is.null(self$options$patientId) ||
                is.null(self$options$assessmentTime) ||
                is.null(self$options$targetLesionSum)) {
                return()
            }

            # Validate threshold parameters
            if (self$options$prThreshold < 0 || self$options$prThreshold > 100) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".prThresholdError",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("PR threshold must be between 0-100%. Please check iRECIST Criteria Settings.")
                self$results$insert(1, notice)
                return()
            }

            if (self$options$pdThreshold < 0 || self$options$pdThreshold > 100) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".pdThresholdError",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("PD threshold must be between 0-100%. Please check iRECIST Criteria Settings.")
                self$results$insert(1, notice)
                return()
            }

            if (self$options$pdAbsolute < 0) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".pdAbsoluteError",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("PD absolute increase must be non-negative. Please check iRECIST Criteria Settings.")
                self$results$insert(1, notice)
                return()
            }

            if (self$options$confirmationWindow < 0 ||
                self$options$confirmationWindowMax < self$options$confirmationWindow) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".confirmationWindowError",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("Invalid confirmation window: maximum must be greater than minimum. Please check iRECIST Criteria Settings.")
                self$results$insert(1, notice)
                return()
            }

            # Prepare data
            tryCatch({
                private$.prepareData()

                # Check sample size and add warnings
                nPatients <- length(unique(private$.processedData$patientId))
                if (nPatients < 10) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = ".criticalSampleSize",
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent(paste0("Critically small sample size (N=", nPatients,
                                         " patients). iRECIST analysis requires at least 10 patients for meaningful results."))
                    self$results$insert(1, notice)
                } else if (nPatients < 30) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = ".smallSampleSize",
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    notice$setContent(paste0("Small sample size (N=", nPatients,
                                         " patients). Results should be interpreted with caution. Consider this exploratory analysis."))
                    self$results$insert(2, notice)
                }

                # Check minimum assessments per patient
                assessmentsPerPatient <- private$.processedData %>%
                    group_by(patientId) %>%
                    summarise(nAssessments = n(), .groups = "drop")

                if (all(assessmentsPerPatient$nAssessments == 1)) {
                    notice <- jmvcore::Notice$new(
                        options = self$options,
                        name = ".onlyBaseline",
                        type = jmvcore::NoticeType$ERROR
                    )
                    notice$setContent("All patients have only baseline assessment. At least one follow-up assessment is required for response evaluation.")
                    self$results$insert(1, notice)
                    return()
                }

                private$.calculateBaseline()
                private$.classifyResponses()
                private$.calculateBestResponse()

                if (self$options$trackPseudoprogression) {
                    private$.trackPseudoprogression()
                }

                # Populate results
                private$.populateDataInfo()

                if (self$options$showResponseTable) {
                    private$.populateResponseTable()
                }

                if (self$options$showBestResponse) {
                    private$.populateBestResponseTable()
                    private$.populateSummaryStats()
                }

                if (self$options$calculateORR || self$options$calculateDCR) {
                    private$.populateEfficacyMetrics()
                }

                if (self$options$trackPseudoprogression && self$options$showPseudoprogressionRate) {
                    private$.populatePseudoprogressionTable()
                }

                if (self$options$stratifiedAnalysis && !is.null(self$options$groupVar)) {
                    private$.populateStratifiedAnalysis()
                }

                # Clinical interpretation
                private$.populateClinicalInterpretation()

                if (self$options$showReference) {
                    private$.populateReferenceInfo()
                }

                if (self$options$showSummary) {
                    private$.populateExecutiveSummary()
                }

                if (self$options$showGlossary) {
                    private$.populateGlossary()
                }

                if (self$options$showAssumptions) {
                    private$.populateAssumptions()
                }

                # Add clinical warnings for pending confirmations
                if (self$options$trackPseudoprogression && !is.null(private$.responseData)) {
                    pendingIUPD <- sum(private$.responseData$irecistCategory == "iUPD", na.rm = TRUE)
                    if (pendingIUPD > 0) {
                        notice <- jmvcore::Notice$new(
                            options = self$options,
                            name = ".pendingIUPD",
                            type = jmvcore::NoticeType$WARNING
                        )
                        notice$setContent(paste0(pendingIUPD, " patient(s) have pending iUPD (unconfirmed progression) requiring confirmation scan ",
                                             "within ", self$options$confirmationWindow, "-", self$options$confirmationWindowMax, " weeks per iRECIST guidelines."))
                        self$results$insert(998, notice)
                    }
                }

                # Add completion info notice
                nPatients <- length(unique(private$.processedData$patientId))
                nAssessments <- nrow(private$.processedData)
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".analysisComplete",
                    type = jmvcore::NoticeType$INFO
                )
                notice$setContent(paste0("Analysis complete: ", nPatients, " patients, ", nAssessments, " assessments. ",
                                     "Results follow iRECIST guidelines (Seymour et al. 2017)."))
                self$results$insert(999, notice)

            }, error = function(e) {
                # Add ERROR notice
                errorMsg <- paste0("Analysis error: ", e$message,
                                   " Common issues: Data must be in long format (one row per assessment per patient); ",
                                   "assessment times must be numeric; target lesion sum must be numeric and positive; ",
                                   "new lesions variable must be binary (0/1).")
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".analysisError",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent(errorMsg)
                self$results$insert(1, notice)
                stop(e)
            })
        },

        # ---- Data Preparation ----
        .prepareData = function() {

            # Get data
            mydata <- self$data

            # Helper to get correct column name (supports both R and Jamovi)
            getVarName <- function(name) {
                if (is.null(name)) return(NULL)
                if (name %in% names(mydata)) {
                    return(name)
                }
                b64 <- jmvcore::toB64(name)
                if (b64 %in% names(mydata)) {
                    return(b64)
                }
                return(name) # Fallback
            }

            # Extract variables
            patientId <- getVarName(self$options$patientId)
            assessmentTime <- getVarName(self$options$assessmentTime)
            targetLesionSum <- getVarName(self$options$targetLesionSum)
            newLesions <- getVarName(self$options$newLesions)

            # Helper to coerce new lesion indicator to binary (accepts 0/1, TRUE/FALSE, yes/no)
            parseNewLesions <- function(x) {
                if (is.null(x)) return(rep(NA_real_, nrow(mydata)))
                if (is.numeric(x)) return(x)
                x <- tolower(as.character(x))
                ifelse(x %in% c("1", "yes", "y", "true"), 1,
                       ifelse(x %in% c("0", "no", "n", "false"), 0, NA_real_))
            }

            # Create working dataframe
            data <- data.frame(
                patientId = as.character(mydata[[patientId]]),
                assessmentTime = as.numeric(mydata[[assessmentTime]]),
                targetSum = as.numeric(mydata[[targetLesionSum]]),
                newLesions = parseNewLesions(mydata[[newLesions]]),
                stringsAsFactors = FALSE
            )

            # Add non-target status if provided
            if (!is.null(self$options$nonTargetStatus)) {
                nonTarget <- getVarName(self$options$nonTargetStatus)
                data$nonTargetStatus <- as.character(mydata[[nonTarget]])
            } else {
                data$nonTargetStatus <- NA
            }

            # Add grouping variable if provided
            if (!is.null(self$options$groupVar)) {
                groupVar <- getVarName(self$options$groupVar)
                data$group <- as.character(mydata[[groupVar]])
            }

            # Check for negative target lesion sum values
            if (any(data$targetSum < 0, na.rm = TRUE)) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".negativeTargetSum",
                    type = jmvcore::NoticeType$ERROR
                )
                notice$setContent("Target lesion sum cannot be negative. Please check your data for errors.")
                self$results$insert(1, notice)
                stop("Negative target lesion sum detected")
            }

            # Remove rows missing required values (logically missing assessments)
            nBefore <- nrow(data)
            data <- data[complete.cases(data[, c("patientId", "assessmentTime", "targetSum", "newLesions")]), ]
            nAfter <- nrow(data)

            # Notify user if data was removed
            if (nBefore > nAfter) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".assessmentsRemoved",
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent(paste0(nBefore - nAfter, " assessment(s) removed due to missing data. ",
                                     nAfter, " assessments retained for analysis."))
                self$results$insert(4, notice)
            }

            # Sort by patient and time
            data <- data[order(data$patientId, data$assessmentTime), ]

            private$.processedData <- data
        },

        # ---- Baseline Calculation ----
        .calculateBaseline = function() {

            data <- private$.processedData

            # Calculate baseline (first assessment for each patient)
            baseline <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                mutate(isBaseline = row_number() == 1) %>%
                filter(isBaseline) %>%
                ungroup() %>%
                dplyr::select(patientId, baselineSum = targetSum, baselineTime = assessmentTime)

            private$.baselineValues <- baseline

            # Merge baseline back to data
            data <- merge(data, baseline, by = "patientId")
            
            # Identify baseline rows in the full dataset
            data$isBaseline <- data$assessmentTime == data$baselineTime

            # Calculate nadir (lowest value up to current timepoint)
            if (self$options$nadirReference) {
                data <- data %>%
                    group_by(patientId) %>%
                    arrange(assessmentTime) %>%
                    mutate(nadirSum = cummin(targetSum)) %>%
                    ungroup()

                private$.nadirValues <- data %>%
                    dplyr::select(patientId, assessmentTime, nadirSum)
            }

            private$.processedData <- data
        },

        # ---- Response Classification ----
        .classifyResponses = function() {

            data <- private$.processedData

            # Get thresholds
            prThreshold <- self$options$prThreshold / 100
            pdThreshold <- self$options$pdThreshold / 100
            pdAbsolute <- self$options$pdAbsolute

            # Calculate changes from baseline and nadir
            data <- data %>%
                mutate(
                    # Protect against division by zero
                    changeFromBaseline = ifelse(baselineSum == 0,
                                                NA_real_,
                                                ((targetSum - baselineSum) / baselineSum) * 100),
                    changeFromNadir = if (self$options$nadirReference) {
                        ifelse(nadirSum == 0,
                               NA_real_,
                               ((targetSum - nadirSum) / nadirSum) * 100)
                    } else {
                        NA_real_
                    },
                    absoluteChange = if (self$options$nadirReference) {
                        targetSum - nadirSum
                    } else {
                        targetSum - baselineSum
                    }
                )

            # Warn if any baseline/nadir = 0
            if (any(data$baselineSum == 0, na.rm = TRUE) ||
                (self$options$nadirReference && any(data$nadirSum == 0, na.rm = TRUE))) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = ".zeroTumorBurden",
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent("Some patients have zero tumor burden at baseline or nadir. Percentage changes cannot be calculated for these cases.")
                self$results$insert(3, notice)
            }

            # Classify responses
            data <- data %>%
                mutate(
                    # Determine reference for PD (nadir or baseline)
                    refChange = if (self$options$nadirReference) changeFromNadir else changeFromBaseline,

                    # iRECIST base classification (before confirmation)
                    irecistCategory = case_when(
                        # New lesions = iUPD (await confirmation)
                        newLesions == 1 ~ "iUPD",

                        # Non-target progression counts as iUPD
                        !is.na(nonTargetStatus) &
                            tolower(nonTargetStatus) == "pd" ~ "iUPD",

                        # Complete response (all target lesions disappear; allow non-target CR)
                        targetSum == 0 & (is.na(nonTargetStatus) | tolower(nonTargetStatus) %in% c("cr", "complete")) ~ "iCR",

                        # Partial response (user-configured threshold from baseline)
                        changeFromBaseline <= -100 * prThreshold ~ "iPR",

                        # Progressive disease (user-configured % increase + absolute)
                        refChange >= 100 * pdThreshold & absoluteChange >= pdAbsolute ~ "iUPD",

                        # Stable disease (everything else)
                        TRUE ~ "iSD"
                    ),

                    irecistCategoryOrig = irecistCategory,

                    # Initially all responses are unconfirmed
                    confirmed = "No"
                ) %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                mutate(
                    nextTime = lead(assessmentTime),
                    nextCategoryRaw = lead(irecistCategory),
                    timeDiff = nextTime - assessmentTime
                ) %>%
                ungroup()

            # Apply confirmation logic
            if (self$options$requireConfirmation) {
                data <- private$.applyConfirmation(data)
            } else {
                # Keep timing info for pseudoprogression even when confirmation not required
                data$confirmed <- "Not required"
            }

            private$.responseData <- data
            private$.processedData <- data
        },

        # ---- Confirmation Logic ----
        .applyConfirmation = function(data) {

            confirmWindow <- self$options$confirmationWindow
            confirmWindowMax <- self$options$confirmationWindowMax

            # For each patient, check if iUPD is confirmed
            data <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                mutate(
                    # Use pre-computed next assessment info
                    confirmed = case_when(
                        # Confirm progression within window
                        irecistCategory == "iUPD" &
                            !is.na(timeDiff) &
                            timeDiff >= confirmWindow &
                            timeDiff <= confirmWindowMax &
                            nextCategoryRaw == "iUPD" ~ "Yes (iCPD)",

                        # Pseudoprogression: iUPD followed by response/stable
                        irecistCategory == "iUPD" &
                            !is.na(timeDiff) &
                            timeDiff >= confirmWindow &
                            timeDiff <= confirmWindowMax &
                            nextCategoryRaw %in% c("iPR", "iSD", "iCR") ~ "No (Pseudoprogression)",

                        # Awaiting confirmation
                        irecistCategory == "iUPD" ~ "Pending",

                        # Response confirmation: need a follow-up scan in window
                        irecistCategory %in% c("iPR", "iCR") &
                            !is.na(timeDiff) &
                            timeDiff >= confirmWindow &
                            timeDiff <= confirmWindowMax &
                            nextCategoryRaw %in% c("iPR", "iCR", "iSD") ~ "Yes (Confirmed)",

                        irecistCategory %in% c("iPR", "iCR") ~ "Requires 2nd scan",

                        TRUE ~ confirmed
                    ),

                    # Update category based on confirmation
                    irecistCategory = case_when(
                        confirmed == "Yes (iCPD)" ~ "iCPD",
                        confirmed == "No (Pseudoprogression)" ~ paste0(nextCategoryRaw, " (pseudo)"),
                        TRUE ~ irecistCategory
                    )
                ) %>%
                ungroup()

            return(data)
        },

        # ---- Best Overall Response ----
        .calculateBestResponse = function() {

            data <- private$.responseData

            # Calculate best response per patient (stop at first iCPD)
            bestResponse <- lapply(split(data, data$patientId), function(df) {
                df <- df[order(df$assessmentTime), ]
                df <- df[!df$isBaseline, ]

                idx_cpd <- which(df$irecistCategory == "iCPD")
                if (length(idx_cpd) > 0) {
                    df <- df[seq_len(idx_cpd[1]), ]
                }

                responses <- if (self$options$requireConfirmation) {
                    df$irecistCategory[df$confirmed != "Pending"]
                } else {
                    df$irecistCategory
                }

                responses <- gsub(" \\(pseudo\\)", "", responses)

                best <- if (length(responses) == 0) {
                    "Unknown"
                } else if ("iCR" %in% responses) {
                    "iCR"
                } else if ("iPR" %in% responses) {
                    "iPR"
                } else if ("iSD" %in% responses) {
                    "iSD"
                } else if ("iCPD" %in% responses) {
                    "iCPD"
                } else {
                    "iUPD"
                }

                timeToResponse <- {
                    idx <- which(df$irecistCategory %in% c("iCR", "iPR"))
                    if (length(idx) > 0) df$assessmentTime[idx[1]] else NA
                }

                confirmedBest <- if (best %in% c("iCR", "iPR")) {
                    sum_confirmed <- sum(df$confirmed %in% c("Yes", "Yes (Confirmed)"))
                    if (sum_confirmed >= self$options$confirmationScans) "Yes" else "No"
                } else {
                    "-"
                }

                hadPseudo <- any(grepl("Pseudo", df$confirmed) | grepl("\\(pseudo\\)", df$irecistCategory))

                timeToCPD <- {
                    idx <- which(df$irecistCategory == "iCPD")
                    if (length(idx) > 0) df$assessmentTime[idx[1]] else NA
                }

                tibble(
                    patientId = unique(df$patientId),
                    bestResponse = best,
                    timeToResponse = timeToResponse,
                    confirmed = confirmedBest,
                    hadPseudoprogression = hadPseudo,
                    timeToCPD = timeToCPD
                )
            }) %>%
                bind_rows()

            private$.bestResponseData <- bestResponse
        },

        # ---- Pseudoprogression Tracking ----
        .trackPseudoprogression = function() {

            data <- private$.responseData

            # Filter for original iUPD events and track outcomes
            pseudo <- data %>%
                filter(irecistCategoryOrig == "iUPD") %>%
                transmute(
                    patientId = patientId,
                    iupdTime = assessmentTime,
                    confirmationTime = ifelse(confirmed %in% c("Yes (iCPD)", "No (Pseudoprogression)"), nextTime, NA_real_),
                    outcome = confirmed,
                    subsequentResponse = ifelse(!is.na(nextCategoryRaw), nextCategoryRaw, "Pending")
                )

            private$.pseudoprogressionData <- pseudo
        },

        # ---- Populate Data Info ----
        .populateDataInfo = function() {

            table <- self$results$dataInfo
            data <- private$.processedData

            nPatients <- length(unique(data$patientId))
            nAssessments <- nrow(data)
            meanAssessments <- round(nAssessments / nPatients, 1)

            assessmentRange <- range(data$assessmentTime)

            rows <- list(
                list(metric = "Total Patients", value = as.character(nPatients)),
                list(metric = "Total Assessments", value = as.character(nAssessments)),
                list(metric = "Mean Assessments per Patient", value = as.character(meanAssessments)),
                list(metric = "Assessment Time Range",
                     value = paste0(round(assessmentRange[1], 1), " - ", round(assessmentRange[2], 1)))
            )

            for (row in rows) {
                table$addRow(rowKey = row$metric, values = row)
            }
        },

        # ---- Populate Response Table ----
        .populateResponseTable = function() {

            table <- self$results$responseTable
            data <- private$.responseData

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    assessmentTime = data$assessmentTime[i],
                    targetSum = data$targetSum[i],
                    changeFromBaseline = data$changeFromBaseline[i],
                    changeFromNadir = if (!is.na(data$changeFromNadir[i])) data$changeFromNadir[i] else NA,
                    newLesions = if (data$newLesions[i] == 1) "Yes" else "No",
                    irecistCategory = data$irecistCategory[i],
                    confirmed = data$confirmed[i],
                    notes = ""
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        # ---- Populate Best Response Table ----
        .populateBestResponseTable = function() {

            table <- self$results$bestResponseTable
            data <- private$.bestResponseData

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    bestResponse = data$bestResponse[i],
                    timeToResponse = if (!is.na(data$timeToResponse[i])) data$timeToResponse[i] else NA,
                    confirmed = data$confirmed[i],
                    hadPseudoprogression = if (data$hadPseudoprogression[i]) "Yes" else "No",
                    timeToCPD = if (!is.na(data$timeToCPD[i])) data$timeToCPD[i] else NA
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        # ---- Populate Summary Stats ----
        .populateSummaryStats = function() {

            table <- self$results$summaryStats
            data <- private$.bestResponseData

            # Count responses
            responseCounts <- table(data$bestResponse)
            total <- nrow(data)

            for (category in names(responseCounts)) {
                n <- responseCounts[category]
                percent <- (n / total) * 100

                # Calculate binomial CI
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

        # ---- Populate Efficacy Metrics ----
        .populateEfficacyMetrics = function() {

            table <- self$results$efficacyMetrics
            data <- private$.bestResponseData

            total <- nrow(data)

            if (self$options$calculateORR) {
                # Objective Response Rate (iCR + iPR)
                n_orr <- sum(data$bestResponse %in% c("iCR", "iPR"))
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
            }

            if (self$options$calculateDCR) {
                # Disease Control Rate (iCR + iPR + iSD)
                n_dcr <- sum(data$bestResponse %in% c("iCR", "iPR", "iSD"))
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
            }

            if (self$options$showPseudoprogressionRate) {
                # Pseudoprogression Rate
                n_pseudo <- sum(data$hadPseudoprogression)
                pseudo_rate <- (n_pseudo / total) * 100
                ci_pseudo <- binom.test(n_pseudo, total)$conf.int

                table$addRow(rowKey = "Pseudo", values = list(
                    metric = "Pseudoprogression Rate",
                    value = pseudo_rate,
                    n = as.integer(n_pseudo),
                    total = as.integer(total),
                    ci_lower = ci_pseudo[1] * 100,
                    ci_upper = ci_pseudo[2] * 100
                ))
            }
        },

        # ---- Populate Pseudoprogression Table ----
        .populatePseudoprogressionTable = function() {

            table <- self$results$pseudoprogressionTable
            data <- private$.pseudoprogressionData

            if (is.null(data) || nrow(data) == 0) {
                return()
            }

            for (i in 1:nrow(data)) {
                row <- list(
                    patientId = as.character(data$patientId[i]),
                    iupdTime = data$iupdTime[i],
                    confirmationTime = if (!is.na(data$confirmationTime[i])) data$confirmationTime[i] else NA,
                    outcome = data$outcome[i],
                    subsequentResponse = if (!is.na(data$subsequentResponse[i])) data$subsequentResponse[i] else "Pending"
                )

                table$addRow(rowKey = i, values = row)
            }
        },

        # ---- Populate Stratified Analysis ----
        .populateStratifiedAnalysis = function() {

            if (is.null(self$options$groupVar) || !self$options$stratifiedAnalysis) {
                return()
            }

            table <- self$results$stratifiedTable

            groupLookup <- private$.processedData %>%
                group_by(patientId) %>%
                summarise(group = dplyr::first(group), .groups = "drop")

            data <- private$.bestResponseData %>%
                left_join(groupLookup, by = "patientId")

            if (is.null(data) || nrow(data) == 0) {
                return()
            }

            summary <- data %>%
                group_by(group) %>%
                summarise(
                    n = n(),
                    iCR = mean(bestResponse == "iCR") * 100,
                    iPR = mean(bestResponse == "iPR") * 100,
                    iSD = mean(bestResponse == "iSD") * 100,
                    iCPD = mean(bestResponse == "iCPD") * 100,
                    orr = mean(bestResponse %in% c("iCR", "iPR")) * 100,
                    dcr = mean(bestResponse %in% c("iCR", "iPR", "iSD")) * 100,
                    pseudoprogression_rate = mean(hadPseudoprogression) * 100,
                    .groups = "drop"
                )

            for (i in 1:nrow(summary)) {
                table$addRow(rowKey = i, values = list(
                    group = as.character(summary$group[i]),
                    n = as.integer(summary$n[i]),
                    iCR = summary$iCR[i],
                    iPR = summary$iPR[i],
                    iSD = summary$iSD[i],
                    iCPD = summary$iCPD[i],
                    orr = summary$orr[i],
                    dcr = summary$dcr[i],
                    pseudoprogression_rate = summary$pseudoprogression_rate[i]
                ))
            }
        },

        # ---- Populate Clinical Interpretation ----
        .populateClinicalInterpretation = function() {

            data <- private$.bestResponseData

            if (is.null(data) || nrow(data) == 0) {
                return()
            }

            total <- nrow(data)
            n_orr <- sum(data$bestResponse %in% c("iCR", "iPR"))
            n_dcr <- sum(data$bestResponse %in% c("iCR", "iPR", "iSD"))
            n_pseudo <- sum(data$hadPseudoprogression)

            orr <- round((n_orr / total) * 100, 1)
            dcr <- round((n_dcr / total) * 100, 1)
            pseudo_rate <- round((n_pseudo / total) * 100, 1)

            html <- paste0(
                "<h4>Clinical Interpretation</h4>",
                "<p><b>Efficacy Summary:</b></p>",
                "<ul>",
                "<li><b>Objective Response Rate (ORR):</b> ", orr, "% (", n_orr, "/", total, " patients)</li>",
                "<li><b>Disease Control Rate (DCR):</b> ", dcr, "% (", n_dcr, "/", total, " patients)</li>",
                "<li><b>Pseudoprogression Rate:</b> ", pseudo_rate, "% (", n_pseudo, "/", total, " patients)</li>",
                "</ul>",

                "<p><b>iRECIST Categories:</b></p>",
                "<ul>",
                "<li><b>iCR (Immune Complete Response):</b> Disappearance of all target lesions</li>",
                "<li><b>iPR (Immune Partial Response):</b> ≥30% decrease from baseline</li>",
                "<li><b>iSD (Immune Stable Disease):</b> Neither PR nor PD criteria met</li>",
                "<li><b>iUPD (Immune Unconfirmed PD):</b> Initial progression - may be pseudoprogression</li>",
                "<li><b>iCPD (Immune Confirmed PD):</b> Progression confirmed on next scan ≥4 weeks later</li>",
                "</ul>",

                "<p><b>Key Findings:</b></p>",
                "<ul>"
            )

            if (n_pseudo > 0) {
                html <- paste0(html,
                    "<li>Pseudoprogression detected in ", pseudo_rate, "% of patients - ",
                    "initial increase in tumor burden followed by response</li>"
                )
            }

            if (n_orr > 0) {
                html <- paste0(html,
                    "<li>Objective responses (iCR/iPR) observed in ", orr, "% of patients</li>"
                )
            }

            html <- paste0(html, "</ul>")

            self$results$clinicalInterpretation$setContent(html)
        },

        # ---- Populate Reference Info ----
        .populateReferenceInfo = function() {

            html <- paste0(
                "<h4>iRECIST Guidelines Reference</h4>",
                "<p><b>Citation:</b> Seymour L, Bogaerts J, Perrone A, et al. ",
                "iRECIST: guidelines for response criteria for use in trials testing immunotherapeutics. ",
                "<i>Lancet Oncol</i>. 2017;18(3):e143-e152.</p>",

                "<p><b>Key Differences from RECIST 1.1:</b></p>",
                "<ul>",
                "<li><b>Pseudoprogression:</b> Initial increase may be followed by response in immunotherapy</li>",
                "<li><b>Confirmation Requirement:</b> Progression must be confirmed ≥4 weeks later</li>",
                "<li><b>iUPD Category:</b> New category for unconfirmed progression</li>",
                "<li><b>Continue Treatment:</b> Patients with iUPD can continue therapy if clinically stable</li>",
                "</ul>",

                "<p><b>Implementation Notes:</b></p>",
                "<ul>",
                "<li>This analysis implements iRECIST v1.1 criteria</li>",
                "<li>Confirmation window: ", self$options$confirmationWindow, "-",
                self$options$confirmationWindowMax, " weeks</li>",
                "<li>PR threshold: ", self$options$prThreshold, "% decrease</li>",
                "<li>PD threshold: ", self$options$pdThreshold, "% increase + ",
                self$options$pdAbsolute, "mm absolute</li>",
                "</ul>"
            )

            self$results$referenceInfo$setContent(html)
        },

        # ---- Populate Executive Summary ----
        .populateExecutiveSummary = function() {

            data <- private$.bestResponseData

            if (is.null(data) || nrow(data) == 0) {
                return()
            }

            total <- nrow(data)
            n_orr <- sum(data$bestResponse %in% c("iCR", "iPR"))
            n_dcr <- sum(data$bestResponse %in% c("iCR", "iPR", "iSD"))
            n_pseudo <- sum(data$hadPseudoprogression)

            orr <- round((n_orr / total) * 100, 1)
            dcr <- round((n_dcr / total) * 100, 1)
            pseudo_rate <- round((n_pseudo / total) * 100, 1)

            # Calculate confidence intervals
            ci_orr <- binom.test(n_orr, total)$conf.int
            ci_dcr <- binom.test(n_dcr, total)$conf.int

            html <- paste0(
                "<div style='background-color: #f0f8ff; padding: 15px; border-left: 4px solid #4682b4; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>Executive Summary</h4>",
                "<p><b>Study Population:</b> ", total, " patients analyzed using iRECIST criteria (Seymour et al. 2017)</p>",

                "<p><b>Key Efficacy Results:</b></p>",
                "<ul>",
                "<li><b>Objective Response Rate (ORR):</b> ", orr, "% (95% CI: ",
                round(ci_orr[1] * 100, 1), "-", round(ci_orr[2] * 100, 1), "%) — ",
                n_orr, "/", total, " patients achieved iCR or iPR</li>",

                "<li><b>Disease Control Rate (DCR):</b> ", dcr, "% (95% CI: ",
                round(ci_dcr[1] * 100, 1), "-", round(ci_dcr[2] * 100, 1), "%) — ",
                n_dcr, "/", total, " patients achieved iCR, iPR, or iSD</li>",

                "<li><b>Pseudoprogression:</b> ", pseudo_rate, "% (", n_pseudo, "/", total,
                " patients) showed initial progression followed by response, ",
                "highlighting the importance of confirmation scans per iRECIST guidelines</li>",
                "</ul>",

                "<p><b>Clinical Interpretation:</b> ",
                if (orr >= 20) {
                    "The objective response rate suggests clinically meaningful activity."
                } else if (orr >= 10) {
                    "The objective response rate indicates modest activity."
                } else {
                    "The objective response rate is low, though disease control may provide benefit."
                },
                if (pseudo_rate > 0) {
                    paste0(" Pseudoprogression was observed in ", pseudo_rate,
                           "% of patients, emphasizing the value of iRECIST over traditional RECIST 1.1 in immunotherapy trials.")
                } else {
                    ""
                },
                "</p>",
                "</div>"
            )

            self$results$executiveSummary$setContent(html)
        },

        # ---- Populate Glossary ----
        .populateGlossary = function() {

            html <- paste0(
                "<div style='background-color: #fff9e6; padding: 15px; border-left: 4px solid #ffa500; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>iRECIST Glossary</h4>",

                "<dl>",
                "<dt><b>iCR (Immune Complete Response)</b></dt>",
                "<dd>Complete disappearance of all target lesions. Best possible outcome. ",
                "Non-target lesions must also be absent or assessed as complete response.</dd>",

                "<dt><b>iPR (Immune Partial Response)</b></dt>",
                "<dd>At least 30% decrease in sum of target lesion diameters compared to baseline. ",
                "Favorable response indicating meaningful tumor shrinkage.</dd>",

                "<dt><b>iSD (Immune Stable Disease)</b></dt>",
                "<dd>Neither sufficient shrinkage for iPR nor sufficient growth for iUPD/iCPD. ",
                "Tumor burden is controlled but not significantly decreased.</dd>",

                "<dt><b>iUPD (Immune Unconfirmed Progressive Disease)</b></dt>",
                "<dd>Initial evidence of progression requiring confirmation scan ≥4 weeks later. ",
                "May represent true progression OR pseudoprogression (transient increase before response). ",
                "Treatment may continue if patient is clinically stable.</dd>",

                "<dt><b>iCPD (Immune Confirmed Progressive Disease)</b></dt>",
                "<dd>Progression confirmed on follow-up scan performed ≥4 weeks after initial iUPD. ",
                "Represents true disease progression, not pseudoprogression.</dd>",

                "<dt><b>Pseudoprogression</b></dt>",
                "<dd>Paradoxical increase in tumor burden (iUPD) followed by subsequent response or stability. ",
                "Occurs in ~5-10% of immunotherapy patients due to immune cell infiltration. ",
                "Unique to immunotherapy, rare with chemotherapy.</dd>",

                "<dt><b>ORR (Objective Response Rate)</b></dt>",
                "<dd>Percentage of patients achieving iCR or iPR. Primary efficacy endpoint in most oncology trials. ",
                "ORR ≥20% generally considered clinically significant for solid tumors.</dd>",

                "<dt><b>DCR (Disease Control Rate)</b></dt>",
                "<dd>Percentage achieving iCR, iPR, or iSD. Broader measure of clinical benefit. ",
                "Captures patients with stable disease who may benefit from treatment.</dd>",

                "<dt><b>Confirmation Window</b></dt>",
                "<dd>Time interval (typically 4-12 weeks) required between initial iUPD and confirmation scan. ",
                "Allows differentiation of pseudoprogression from true progression.</dd>",
                "</dl>",
                "</div>"
            )

            self$results$glossary$setContent(html)
        },

        # ---- Populate Assumptions ----
        .populateAssumptions = function() {

            html <- paste0(
                "<div style='background-color: #ffe6e6; padding: 15px; border-left: 4px solid #dc143c; margin: 10px 0;'>",
                "<h4 style='margin-top: 0;'>Assumptions & Caveats</h4>",

                "<p><b>Data Requirements:</b></p>",
                "<ul>",
                "<li><b>Format:</b> Longitudinal data in long format (one row per assessment per patient)</li>",
                "<li><b>Minimum Data:</b> Each patient requires at least 1 baseline + 1 follow-up assessment</li>",
                "<li><b>Required Variables:</b> Patient ID, assessment time, target lesion sum (mm), new lesions (0/1)</li>",
                "<li><b>Time Units:</b> Assessment time should be in consistent units (weeks or months from baseline)</li>",
                "</ul>",

                "<p><b>Analysis Assumptions:</b></p>",
                "<ul>",
                "<li><b>Baseline Definition:</b> First recorded assessment for each patient is treated as baseline (time 0)</li>",
                "<li><b>Missing Data:</b> Complete-case analysis — assessments with missing values are excluded</li>",
                "<li><b>Nadir Reference:</b> When enabled, progressive disease is determined relative to nadir (lowest) value, not baseline</li>",
                "<li><b>Confirmation Window:</b> Currently set to ", self$options$confirmationWindow, "-",
                self$options$confirmationWindowMax, " weeks per iRECIST v1.1 guidelines</li>",
                "<li><b>New Lesions:</b> Any new lesion triggers iUPD classification, even if target lesions decrease</li>",
                "</ul>",

                "<p><b>Statistical Methods:</b></p>",
                "<ul>",
                "<li><b>Response Rates:</b> Exact binomial confidence intervals (95% CI)</li>",
                "<li><b>Best Response:</b> Determined by best confirmed response before iCPD or end of follow-up</li>",
                "<li><b>Confirmation Logic:</b> Requires follow-up scan within confirmation window showing same/worse status</li>",
                "</ul>",

                "<p><b>Important Caveats:</b></p>",
                "<ul>",
                "<li><b>iRECIST vs RECIST 1.1:</b> Results may differ from traditional RECIST due to pseudoprogression handling</li>",
                "<li><b>Clinical Context:</b> Response criteria alone do not determine treatment decisions — clinical status matters</li>",
                "<li><b>Informative Censoring:</b> Patients who progress may have shorter follow-up (early discontinuation)</li>",
                "<li><b>Small Samples:</b> Results with N&lt;30 should be considered exploratory</li>",
                "<li><b>Validation:</b> For regulatory submissions, consult protocol-specific iRECIST implementation details</li>",
                "</ul>",
                "</div>"
            )

            self$results$assumptions$setContent(html)
        },

        # ---- Plotting Functions ----
        .waterfallPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bestResponseData)) {
                return()
            }

            # Get baseline and best response data
            data <- private$.processedData
            baseline <- private$.baselineValues

            # Calculate best percent change for each patient
            waterfallData <- data %>%
                left_join(baseline, by = "patientId") %>%
                mutate(percentChange = ((targetSum - baselineSum) / baselineSum) * 100) %>%
                group_by(patientId) %>%
                summarise(bestChange = min(percentChange), .groups = "drop") %>%
                arrange(bestChange)

            # Add best response category
            bestResp <- private$.bestResponseData %>%
                dplyr::select(patientId, bestResponse)

            waterfallData <- waterfallData %>%
                left_join(bestResp, by = "patientId")

            # Add patient ordering
            waterfallData$patientOrder <- 1:nrow(waterfallData)

            # Define colors
            colors <- c(
                "iCR" = "#00AA00",
                "iPR" = "#66BB66",
                "iSD" = "#FFDD00",
                "iUPD" = "#FFA500",
                "iCPD" = "#DD0000"
            )

            # Create plot
            p <- ggplot(waterfallData, aes(x = patientOrder, y = bestChange, fill = bestResponse)) +
                geom_bar(stat = "identity", width = 0.8) +
                geom_hline(yintercept = 0, linetype = "solid", color = "black") +
                geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
                geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
                scale_fill_manual(values = colors, name = "Best Response") +
                labs(
                    title = "Waterfall Plot - Best Response by iRECIST",
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
        },

        .swimmerPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.responseData)) {
                return()
            }

            data <- private$.responseData

            # Get time on study for each patient
            swimData <- data %>%
                group_by(patientId) %>%
                summarise(
                    maxTime = max(assessmentTime),
                    hadIUPD = any(irecistCategory == "iUPD"),
                    iupdTime = ifelse(hadIUPD, min(assessmentTime[irecistCategory == "iUPD"]), NA),
                    .groups = "drop"
                ) %>%
                arrange(desc(maxTime))

            # Add ordering
            swimData$patientOrder <- 1:nrow(swimData)

            # Create plot
            p <- ggplot(swimData, aes(y = factor(patientOrder))) +
                geom_segment(aes(x = 0, xend = maxTime, yend = factor(patientOrder)),
                            size = 3, color = "steelblue") +
                labs(
                    title = "Swimmer Plot - Time on Study",
                    x = "Time from Baseline",
                    y = "Patients"
                ) +
                theme_minimal() +
                theme(
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )

            # Add iUPD markers if tracking pseudoprogression
            if (self$options$trackPseudoprogression) {
                iupdData <- swimData %>% filter(hadIUPD)
                if (nrow(iupdData) > 0) {
                    p <- p + geom_point(data = iupdData,
                                       aes(x = iupdTime, y = factor(patientOrder)),
                                       color = "orange", size = 3, shape = 17)
                }
            }

            print(p)

            TRUE
        },

        .spiderPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.processedData)) {
                return()
            }

            data <- private$.processedData

            # Create spider plot
            p <- ggplot(data, aes(x = assessmentTime, y = changeFromBaseline, group = patientId)) +
                geom_line(alpha = 0.6, color = "steelblue") +
                geom_point(alpha = 0.4, size = 1.5) +
                geom_hline(yintercept = 0, linetype = "solid", color = "black") +
                geom_hline(yintercept = -30, linetype = "dashed", color = "blue", alpha = 0.5) +
                geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.5) +
                labs(
                    title = "Spider Plot - Individual Tumor Trajectories",
                    x = "Time from Baseline",
                    y = "% Change from Baseline"
                ) +
                theme_minimal()

            print(p)

            TRUE
        },

        .timeToCPDPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bestResponseData)) {
                return()
            }

            data <- private$.bestResponseData
            data <- data %>% filter(!is.na(timeToCPD))
            if (nrow(data) == 0) {
                return()
            }

            p <- ggplot(data, aes(x = timeToCPD)) +
                geom_histogram(binwidth = 2, fill = "firebrick", color = "white", alpha = 0.8) +
                labs(
                    title = "Time to Confirmed PD (iCPD)",
                    x = "Time",
                    y = "Patients"
                ) +
                theme_minimal()

            print(p)

            TRUE
        },

        .timelinePlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.responseData)) {
                return()
            }

            data <- private$.responseData

            p <- ggplot(data, aes(x = assessmentTime, y = patientId, color = irecistCategory)) +
                geom_line(aes(group = patientId), alpha = 0.3) +
                geom_point(size = 2) +
                labs(
                    title = "Response Timeline",
                    x = "Time from Baseline",
                    y = "Patient"
                ) +
                theme_minimal()

            print(p)

            TRUE
        }
    )
)
