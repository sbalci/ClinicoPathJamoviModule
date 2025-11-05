#' @title iRECIST Analysis
#' @importFrom R6 R6Class
#' @import jmvcore
#' @import ggplot2
#' @importFrom dplyr mutate filter group_by arrange summarise n ungroup lead lag
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

            # Prepare data
            tryCatch({
                private$.prepareData()
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

            }, error = function(e) {
                self$results$instructions$setContent(
                    paste0("<div class='error'>",
                           "<h4>Error in Analysis</h4>",
                           "<p>", e$message, "</p>",
                           "<p><b>Common issues:</b></p>",
                           "<ul>",
                           "<li>Ensure data is in long format (one row per assessment per patient)</li>",
                           "<li>Check that assessment times are numeric</li>",
                           "<li>Verify target lesion sum is numeric and positive</li>",
                           "<li>Ensure new lesions variable is binary (0/1)</li>",
                           "</ul></div>")
                )
                stop(e)
            })
        },

        # ---- Data Preparation ----
        .prepareData = function() {

            # Get data
            mydata <- self$data

            # Extract variables
            patientId <- jmvcore::toB64(self$options$patientId)
            assessmentTime <- jmvcore::toB64(self$options$assessmentTime)
            targetLesionSum <- jmvcore::toB64(self$options$targetLesionSum)
            newLesions <- jmvcore::toB64(self$options$newLesions)

            # Create working dataframe
            data <- data.frame(
                patientId = mydata[[patientId]],
                assessmentTime = as.numeric(mydata[[assessmentTime]]),
                targetSum = as.numeric(mydata[[targetLesionSum]]),
                newLesions = as.numeric(mydata[[newLesions]]),
                stringsAsFactors = FALSE
            )

            # Add non-target status if provided
            if (!is.null(self$options$nonTargetStatus)) {
                nonTarget <- jmvcore::toB64(self$options$nonTargetStatus)
                data$nonTargetStatus <- as.character(mydata[[nonTarget]])
            } else {
                data$nonTargetStatus <- NA
            }

            # Add grouping variable if provided
            if (!is.null(self$options$groupVar)) {
                groupVar <- jmvcore::toB64(self$options$groupVar)
                data$group <- as.character(mydata[[groupVar]])
            }

            # Remove missing values
            data <- na.omit(data[, c("patientId", "assessmentTime", "targetSum", "newLesions")])

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
                filter(row_number() == 1) %>%
                ungroup() %>%
                select(patientId, baselineSum = targetSum, baselineTime = assessmentTime)

            private$.baselineValues <- baseline

            # Merge baseline back to data
            data <- merge(data, baseline, by = "patientId")

            # Calculate nadir (lowest value up to current timepoint)
            if (self$options$nadirReference) {
                data <- data %>%
                    group_by(patientId) %>%
                    arrange(assessmentTime) %>%
                    mutate(
                        nadirSum = cummin(targetSum),
                        nadirSum = ifelse(assessmentTime == baselineTime, baselineSum, nadirSum)
                    ) %>%
                    ungroup()

                private$.nadirValues <- data %>%
                    select(patientId, assessmentTime, nadirSum)
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
                    changeFromBaseline = ((targetSum - baselineSum) / baselineSum) * 100,
                    changeFromNadir = if (self$options$nadirReference) {
                        ((targetSum - nadirSum) / nadirSum) * 100
                    } else {
                        NA
                    },
                    absoluteChange = if (self$options$nadirReference) {
                        targetSum - nadirSum
                    } else {
                        targetSum - baselineSum
                    }
                )

            # Classify responses
            data <- data %>%
                mutate(
                    # Determine reference for PD (nadir or baseline)
                    refChange = if (self$options$nadirReference) changeFromNadir else changeFromBaseline,

                    # iRECIST classification
                    irecistCategory = case_when(
                        # New lesions = PD (iUPD)
                        newLesions == 1 ~ "iUPD",

                        # Complete response (all target lesions disappear)
                        targetSum == 0 ~ "iCR",

                        # Partial response (≥30% decrease from baseline)
                        changeFromBaseline <= -30 ~ "iPR",

                        # Progressive disease (≥20% increase from nadir + 5mm absolute)
                        refChange >= 20 & absoluteChange >= pdAbsolute ~ "iUPD",

                        # Stable disease (everything else)
                        TRUE ~ "iSD"
                    ),

                    # Initially all responses are unconfirmed
                    confirmed = "No"
                )

            # Apply confirmation logic
            if (self$options$requireConfirmation) {
                data <- private$.applyConfirmation(data)
            }

            private$.responseData <- data
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
                    # Check next assessment
                    nextTime = lead(assessmentTime),
                    nextCategory = lead(irecistCategory),
                    timeDiff = nextTime - assessmentTime,

                    # iUPD confirmed if:
                    # 1. Current assessment is iUPD
                    # 2. Next assessment is 4-12 weeks later
                    # 3. Next assessment shows progression
                    confirmed = case_when(
                        irecistCategory == "iUPD" &
                            !is.na(timeDiff) &
                            timeDiff >= confirmWindow &
                            timeDiff <= confirmWindowMax &
                            nextCategory == "iUPD" ~ "Yes (iCPD)",

                        irecistCategory == "iUPD" &
                            !is.na(timeDiff) &
                            timeDiff >= confirmWindow &
                            timeDiff <= confirmWindowMax &
                            nextCategory %in% c("iPR", "iSD", "iCR") ~ "No (Pseudoprogression)",

                        irecistCategory == "iUPD" ~ "Pending",

                        irecistCategory %in% c("iPR", "iCR") ~ "Requires 2nd scan",

                        TRUE ~ confirmed
                    ),

                    # Update category based on confirmation
                    irecistCategory = case_when(
                        confirmed == "Yes (iCPD)" ~ "iCPD",
                        confirmed == "No (Pseudoprogression)" ~ paste0(nextCategory, " (pseudo)"),
                        TRUE ~ irecistCategory
                    )
                ) %>%
                ungroup()

            return(data)
        },

        # ---- Best Overall Response ----
        .calculateBestResponse = function() {

            data <- private$.responseData

            # Define response hierarchy (best to worst)
            response_order <- c("iCR", "iPR", "iSD", "iUPD", "iCPD")

            # Calculate best response per patient
            bestResponse <- data %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                summarise(
                    bestResponse = {
                        # Get confirmed responses only if required
                        if (self$options$requireConfirmation) {
                            responses <- irecistCategory[confirmed != "Pending"]
                        } else {
                            responses <- irecistCategory
                        }

                        # Remove pseudo labels for comparison
                        responses <- gsub(" \\(pseudo\\)", "", responses)

                        # Find best response
                        if (length(responses) == 0) {
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
                    },

                    timeToResponse = {
                        # Time to first CR or PR
                        idx <- which(irecistCategory %in% c("iCR", "iPR"))
                        if (length(idx) > 0) assessmentTime[idx[1]] else NA
                    },

                    confirmed = {
                        # Check if best response is confirmed
                        if (bestResponse %in% c("iCR", "iPR")) {
                            sum_confirmed <- sum(confirmed == "Requires 2nd scan" | confirmed == "Yes")
                            if (sum_confirmed >= self$options$confirmationScans) "Yes" else "No"
                        } else {
                            "-"
                        }
                    },

                    hadPseudoprogression = any(grepl("pseudo", confirmed)),

                    timeToCPD = {
                        # Time to first confirmed PD
                        idx <- which(irecistCategory == "iCPD")
                        if (length(idx) > 0) assessmentTime[idx[1]] else NA
                    },

                    .groups = "drop"
                )

            private$.bestResponseData <- bestResponse
        },

        # ---- Pseudoprogression Tracking ----
        .trackPseudoprogression = function() {

            data <- private$.responseData

            # Find all iUPD events and their outcomes
            pseudo <- data %>%
                filter(irecistCategory == "iUPD") %>%
                group_by(patientId) %>%
                arrange(assessmentTime) %>%
                summarise(
                    iupdTime = assessmentTime[1],
                    confirmationTime = lead(assessmentTime)[1],
                    outcome = confirmed[1],
                    subsequentResponse = lead(irecistCategory)[1],
                    .groups = "drop"
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

            # This would require group variable - placeholder for now
            return()
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
                select(patientId, bestResponse)

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

            # Placeholder for Kaplan-Meier style plot for time to iCPD
            # Would require survival analysis integration

            return()
        },

        .timelinePlot = function(image, ggtheme, theme, ...) {

            # Placeholder for detailed timeline visualization

            return()
        }
    )
)
