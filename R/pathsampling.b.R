
# This file is a generated template, your changes will not be overwritten

pathsamplingClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pathsamplingClass",
    inherit = pathsamplingBase,
    private = list(
        # HTML Styling Constants (decisionpanel style)
        .styleConstants = list(
            font = "font-family: Arial, sans-serif;",
            lineHeight = "line-height: 1.4;",
            colorPrimary = "color: #333;",
            colorSecondary = "color: #666;",
            bgLight = "background: #f5f5f5;",
            bgLighter = "background: #f9f9f9;",
            bgWhite = "background: white;",
            borderPrimary = "border: 2px solid #333;",
            borderSecondary = "border: 1px solid #ccc;",
            borderLeft = "border-left: 4px solid #333;",
            borderWarning = "border-left: 4px solid #ffc107;",
            fontSize13 = "font-size: 13px;",
            fontSize14 = "font-size: 14px;",
            fontSize15 = "font-size: 15px;",
            fontSize16 = "font-size: 16px;",
            fontSize18 = "font-size: 18px;",
            fontWeight700 = "font-weight: 700;",
            padding10 = "padding: 10px;",
            padding15 = "padding: 15px;",
            padding20 = "padding: 20px;",
            margin10 = "margin: 10px 0;",
            margin15 = "margin: 15px 0;",
            margin20 = "margin: 20px 0;",
            colorSuccess = "color: #155724;",
            colorInfo = "color: #0c5460;"
        ),

        # Helper to build combined styles
        .buildStyle = function(...) {
            paste(..., collapse = " ")
        },

        # Utility function to escape variable names with special characters
        .escapeVar = function(x) {
            if (is.null(x) || length(x) == 0) return(NULL)
            # Convert to valid R names, replacing special chars with underscores
            escaped <- gsub("[^A-Za-z0-9_]+", "_", make.names(x))
            return(escaped)
        },

        .init = function() {

            # Welcome message when no variables selected
            welcome <- self$results$welcome
            welcomeHtml <- "
            <div style='font-family: Arial, sans-serif; max-width: 800px; line-height: 1.4;'>
                <div style='background: #f5f5f5; border: 2px solid #333; padding: 20px; margin-bottom: 20px;'>
                    <h2 style='margin: 0 0 10px 0; font-size: 18px; color: #333;'>Pathology Sampling Adequacy Analysis</h2>
                    <p style='margin: 0; font-size: 14px; color: #666;'>Determine the minimum number of tissue samples required to detect lesions with specified confidence levels</p>
                </div>

                <table style='width: 100%; border-collapse: collapse; margin-bottom: 20px;'>
                    <tr>
                        <td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>
                            <h4 style='margin: 0 0 10px 0; font-size: 15px; color: #333;'>Quick Start Guide</h4>
                            <ol style='margin: 0; padding-left: 20px; font-size: 14px; color: #333;'>
                                <li style='margin-bottom: 8px;'><strong>Select Required Variables:</strong>
                                    <ul style='margin: 5px 0; padding-left: 20px;'>
                                        <li>Total samples taken</li>
                                        <li>Sample number where lesion first detected</li>
                                    </ul>
                                </li>
                                <li style='margin-bottom: 8px;'><strong>Configure Analysis Parameters:</strong>
                                    <ul style='margin: 5px 0; padding-left: 20px;'>
                                        <li>Target confidence (default: 95%)</li>
                                        <li>Maximum samples to evaluate (default: 10)</li>
                                        <li>Bootstrap iterations (default: 10,000)</li>
                                    </ul>
                                </li>
                                <li style='margin-bottom: 8px;'><strong>Optional Analyses:</strong> Enable tumor burden, LN analysis, or hypergeometric models as needed</li>
                            </ol>
                        </td>

                        <td style='width: 50%; border: 1px solid #ccc; padding: 15px; vertical-align: top;'>
                            <h4 style='margin: 0 0 10px 0; font-size: 15px; color: #333;'>What You'll Get</h4>
                            <ul style='margin: 0; padding-left: 20px; font-size: 14px; color: #333;'>
                                <li style='margin-bottom: 8px;'><strong>Binomial probability model</strong> for theoretical detection rates</li>
                                <li style='margin-bottom: 8px;'><strong>Bootstrap confidence intervals</strong> for empirical sensitivity estimates</li>
                                <li style='margin-bottom: 8px;'><strong>Diagnostic yield curves</strong> showing cumulative detection probability</li>
                                <li style='margin-bottom: 8px;'><strong>Clinical recommendations</strong> for minimum samples required</li>
                                <li style='margin-bottom: 8px;'><strong>Specialized analyses</strong> for LN dissection, omentum sampling, and tumor burden</li>
                            </ul>
                        </td>
                    </tr>
                </table>

                <div style='background: #f9f9f9; border: 1px solid #ccc; padding: 15px;'>
                    <h4 style='margin: 0 0 10px 0; font-size: 15px; color: #333;'>Statistical Methods</h4>
                    <p style='margin: 0 0 10px 0; font-size: 14px; color: #333;'>
                        <strong>Core Methods:</strong> Binomial probability models, Bootstrap resampling, Hypergeometric and Beta-Binomial models for finite populations
                    </p>
                    <p style='margin: 0; font-size: 13px; color: #666;'>
                        <strong>Key References:</strong> Skala & Hagemann 2015 (omentum sampling), Tomlinson 2007, Pu 2021, Yoon 2025 (lymph node adequacy), Maglalang & Fadare 2025 (recent omentum literature)
                    </p>
                </div>
            </div>"
            welcome$setContent(welcomeHtml)

            # Initialize with instructions
            instructions <- self$results$instructions

            if (self$options$showGuidedChecklist) {
                # Guided workflow checklist
                html <- sprintf("<div style='%s'>
                    <div style='%s'>
                        <h3 style='%s'>Quick Start Checklist</h3>
                        <ol style='%s'>
                            <li style='margin: 8px 0;'>
                                <strong>Select required variables:</strong>
                                <ul style='%s'>
                                    <li>Total samples taken (e.g., total_blocks, total_LN)</li>
                                    <li>Sample where lesion first detected (e.g., first_positive_block)</li>
                                    <li><em>Tip:</em> First detection = sample number (1, 2, 3...), NA if never detected</li>
                                </ul>
                            </li>
                            <li style='margin: 8px 0;'>
                                <strong>Set analysis parameters:</strong>
                                <ul style='%s'>
                                    <li>Target confidence: 95%% (standard for diagnostic tests)</li>
                                    <li>Maximum samples: 10-20 (adjust based on your protocol)</li>
                                </ul>
                            </li>
                            <li style='margin: 8px 0;'>
                                <strong>Enable optional analyses</strong> (if applicable):
                                <ul style='%s'>
                                    <li>Tumor burden: Requires positive_cassettes variable</li>
                                    <li>LN analysis: Requires total_lymph_nodes + positive_lymph_nodes</li>
                                    <li>Omentum literature: Compares your data to published studies</li>
                                </ul>
                            </li>
                            <li style='margin: 8px 0;'>
                                <strong>Review results:</strong>
                                <ul style='%s'>
                                    <li>Data Summary: Verify case counts and detection rates</li>
                                    <li>Binomial Model: Theoretical minimum samples required</li>
                                    <li>Bootstrap Validation: Empirical sensitivity with confidence intervals</li>
                                    <li>Clinical Recommendations: Evidence-based sampling protocol</li>
                                </ul>
                            </li>
                        </ol>
                    </div>

                    <div style='%s'>
                        <h4 style='%s'>Common Use Cases</h4>
                        <ul style='%s'>
                            <li><strong>Omentum sampling:</strong> Total blocks submitted, first block with tumor</li>
                            <li><strong>Lymph node dissection:</strong> Total LN examined, first metastatic LN found</li>
                            <li><strong>Serial sections:</strong> Total section levels, first level showing margin involvement</li>
                        </ul>
                    </div>
                </div>",
                private$.styleConstants$font,
                private$.buildStyle(
                    private$.styleConstants$bgLight,
                    private$.styleConstants$borderLeft,
                    private$.styleConstants$padding15,
                    private$.styleConstants$margin10
                ),
                private$.buildStyle(
                    private$.styleConstants$colorPrimary,
                    private$.styleConstants$fontSize16,
                    "margin: 0 0 10px 0;"
                ),
                private$.buildStyle(
                    "margin: 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary
                ),
                private$.buildStyle(
                    "margin: 5px 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorSecondary
                ),
                private$.buildStyle(
                    "margin: 5px 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorSecondary
                ),
                private$.buildStyle(
                    "margin: 5px 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorSecondary
                ),
                private$.buildStyle(
                    "margin: 5px 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorSecondary
                ),
                private$.buildStyle(
                    "background: #fff3cd;",
                    private$.styleConstants$borderSecondary,
                    private$.styleConstants$padding10,
                    private$.styleConstants$margin10
                ),
                private$.buildStyle(
                    "color: #856404;",
                    private$.styleConstants$fontSize15,
                    "margin: 0 0 8px 0;"
                ),
                private$.buildStyle(
                    "margin: 0;",
                    "padding-left: 20px;",
                    "color: #856404;",
                    private$.styleConstants$fontSize14
                ))
            } else {
                # Concise instructions
                html <- sprintf("<div style='%s'>
                    <h3 style='%s %s'>Pathology Sampling Adequacy Analysis</h3>
                    <p style='%s %s'>This analysis determines the minimum number of tissue samples
                    (blocks, cassettes, sections, or lymph nodes) required to detect lesions with a
                    specified confidence level.</p>

                    <p style='%s %s'><b>Required Variables:</b></p>
                    <ul style='%s %s'>
                        <li><b>Total samples taken:</b> Total number submitted per case</li>
                        <li><b>First detection:</b> Sample number where lesion first observed (NA if never detected)</li>
                    </ul>

                    <p style='%s %s'><b>Statistical Methods:</b> Binomial probability models,
                    Bootstrap resampling, Hypergeometric and Beta-Binomial models for finite populations</p>
                </div>",
                private$.styleConstants$font,
                private$.styleConstants$colorPrimary, private$.styleConstants$fontSize16,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary)
            }

            instructions$setContent(html)

        },

        .run = function() {

            # Get required variables
            totalSamples <- self$options$totalSamples
            firstDetection <- self$options$firstDetection

            # Get optional enhanced variables
            positiveCount <- self$options$positiveCount
            positiveSamplesList <- self$options$positiveSamplesList
            sampleType <- self$options$sampleType

            # Validate required variables
            if (is.null(totalSamples) || is.null(firstDetection)) {
                # Variables not selected - show instructions only
                return()
            }

            # Get options and validate
            targetConf <- self$options$targetConfidence
            maxSamp <- self$options$maxSamples
            nBoot <- self$options$bootstrapIterations

            # Prepare recommendation collector to summarize minimum samples for target confidence
            recommendations <- list()
            addRecommendation <- function(method, probVec, priority, description, detail = NULL, ci = NULL) {
                # Extend recommendation list with standardized fields and target-driven summary
                if (length(probVec) == 0 || all(is.na(probVec))) {
                    status <- "No valid probabilities available"
                    rec <- list(
                        method = method,
                        description = description,
                        minSamples = NA_integer_,
                        achievedProb = NA_real_,
                        bestProb = NA_real_,
                        bestN = NA_integer_,
                        status = status,
                        detail = ifelse(is.null(detail) || detail == "", "", detail),
                        priority = priority,
                        ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                        ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                    )
                    recommendations <<- c(recommendations, list(rec))
                    return(invisible())
                }

                validIdx <- which(!is.na(probVec))
                if (length(validIdx) == 0) {
                    status <- "No valid probabilities available"
                    rec <- list(
                        method = method,
                        description = description,
                        minSamples = NA_integer_,
                        achievedProb = NA_real_,
                        bestProb = NA_real_,
                        bestN = NA_integer_,
                        status = status,
                        detail = ifelse(is.null(detail) || detail == "", "", detail),
                        priority = priority,
                        ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                        ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                    )
                    recommendations <<- c(recommendations, list(rec))
                    return(invisible())
                }

                targetCandidates <- validIdx[probVec[validIdx] >= targetConf]
                targetIdx <- if (length(targetCandidates) > 0) targetCandidates[1] else NA_integer_

                bestInValid <- which.max(probVec[validIdx])
                bestIdx <- validIdx[bestInValid]
                bestProb <- probVec[bestIdx]

                if (!is.na(targetIdx)) {
                    minSamples <- targetIdx
                    achievedProb <- probVec[targetIdx]
                    status <- sprintf("Meets target at %d samples (%.1f%%)",
                        minSamples, achievedProb * 100)
                } else {
                    minSamples <- NA_integer_
                    achievedProb <- bestProb
                    status <- ifelse(length(validIdx) > 0,
                        sprintf("Target not reached; best %.1f%% at %d samples",
                            bestProb * 100, bestIdx),
                        "Target not reached")
                }

                detailParts <- character()
                if (!is.null(detail) && nzchar(detail)) {
                    detailParts <- c(detailParts, detail)
                }
                if (!is.null(ci) && length(ci) == 2 && all(is.finite(ci))) {
                    detailParts <- c(detailParts,
                        sprintf("95%% CI %.1f%%-%.1f%%", ci[1] * 100, ci[2] * 100))
                }
                detailText <- paste(detailParts, collapse = "; ")

                rec <- list(
                    method = method,
                    description = description,
                    minSamples = minSamples,
                    achievedProb = achievedProb,
                    bestProb = bestProb,
                    bestN = bestIdx,
                    status = status,
                    detail = detailText,
                    priority = priority,
                    ciLower = ifelse(is.null(ci), NA_real_, ci[1]),
                    ciUpper = ifelse(is.null(ci), NA_real_, ci[2])
                )

                recommendations <<- c(recommendations, list(rec))
            }

            # Storage for bootstrap-derived summary (used later for clinical summary narrative)
            bootstrapTargetIdx <- NA_integer_
            bootstrapMeansVec <- NULL
            bootstrapCILowerVec <- NULL
            bootstrapCIUpperVec <- NULL

            # Validate target confidence range
            if (targetConf <= 0 || targetConf >= 1) {
                dataInfo <- self$results$dataInfo
                dataInfo$addRow(rowKey="error_conf", values=list(
                    measure = "ERROR",
                    value = "Target confidence must be between 0 and 1"
                ))
                return()
            }

            # Warning for extreme confidence levels
            if (targetConf > 0.99) {
                interpretText <- self$results$interpretText
                warningHtml <- sprintf("<div style='%s %s %s %s'>
                    <p style='margin: 0; %s'><strong>WARNING: Extreme Confidence Level</strong></p>
                    <p style='margin: 5px 0 0 0; %s'>
                        Target confidence of %.1f%% is extremely high and may require impractical sample sizes.
                        Standard diagnostic test sensitivity uses 95%%. Consider using 0.90-0.95 for clinical applicability.
                    </p>
                </div>",
                private$.styleConstants$font, private$.styleConstants$bgLight,
                private$.styleConstants$borderLeft, private$.styleConstants$padding10,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary,
                targetConf * 100)
                interpretText$setContent(warningHtml)
            }

            # Warning for insufficient bootstrap iterations
            if (self$options$showBootstrap && nBoot < 1000) {
                interpretText <- self$results$interpretText
                warningHtml <- sprintf("<div style='%s %s %s %s'>
                    <p style='margin: 0; %s'><strong>WARNING: Low Bootstrap Iterations</strong></p>
                    <p style='margin: 5px 0 0 0; %s'>
                        Only %d bootstrap iterations specified. Confidence intervals may be unstable.
                        Recommended: ≥1,000 iterations for preliminary analysis, ≥10,000 for publication.
                    </p>
                </div>",
                private$.styleConstants$font, private$.styleConstants$bgLight,
                private$.styleConstants$borderLeft, private$.styleConstants$padding10,
                private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary,
                nBoot)
                interpretText$setContent(warningHtml)
            }

            # Escape variable names for safe lookup
            totalSamplesEsc <- private$.escapeVar(totalSamples)
            firstDetectionEsc <- private$.escapeVar(firstDetection)

            # Get data with escaped names
            data <- self$data
            totalSamplesData <- jmvcore::toNumeric(data[[totalSamplesEsc]])
            firstDetectionData <- jmvcore::toNumeric(data[[firstDetectionEsc]])

            # Get optional enhanced data
            positiveCountData <- NULL
            positiveSamplesListData <- NULL
            sampleTypeData <- NULL

            if (!is.null(positiveCount)) {
                positiveCountEsc <- private$.escapeVar(positiveCount)
                positiveCountData <- jmvcore::toNumeric(data[[positiveCountEsc]])
            }

            if (!is.null(positiveSamplesList)) {
                positiveSamplesListEsc <- private$.escapeVar(positiveSamplesList)
                positiveSamplesListData <- data[[positiveSamplesListEsc]]
            }

            if (!is.null(sampleType)) {
                sampleTypeEsc <- private$.escapeVar(sampleType)
                sampleTypeData <- data[[sampleTypeEsc]]
            }

            # Preserve raw inputs for reporting before exclusions
            rawTotalSamplesData <- totalSamplesData
            rawFirstDetectionData <- firstDetectionData
            totalCasesInput <- length(rawTotalSamplesData)

            # Handle labelled data (convert factors to numeric)
            if (is.factor(totalSamplesData) || !is.null(attr(totalSamplesData, 'labels'))) {
                totalSamplesData <- as.numeric(as.character(totalSamplesData))
            }
            if (is.factor(firstDetectionData) || !is.null(attr(firstDetectionData, 'labels'))) {
                firstDetectionData <- as.numeric(as.character(firstDetectionData))
            }

            # Identify valid cases (allow missing first detection for non-detected lesions)
            validCases <- !is.na(totalSamplesData)

            # Track data quality notes before filtering
            dataWarnings <- character()
            nExcludedMissingTotal <- 0
            if (any(!validCases)) {
                nExcludedMissingTotal <- sum(!validCases)
                dataWarnings <- c(dataWarnings,
                    sprintf("%d cases removed due to missing total samples", nExcludedMissingTotal))
            }

            # Filter to valid cases
            totalSamplesData <- totalSamplesData[validCases]
            firstDetectionData <- firstDetectionData[validCases]

            # Filter optional data
            if (!is.null(positiveCountData)) {
                positiveCountData <- positiveCountData[validCases]
            }
            if (!is.null(positiveSamplesListData)) {
                positiveSamplesListData <- positiveSamplesListData[validCases]
            }
            if (!is.null(sampleTypeData)) {
                sampleTypeData <- sampleTypeData[validCases]
            }

            # ===== Edge Case Validation =====

            # Error 1: No valid cases after removing missing total samples
            if (length(totalSamplesData) == 0) {
                dataInfo <- self$results$dataInfo
                dataInfo$addRow(rowKey="error_no_cases", values=list(
                    measure = "ERROR",
                    value = "No valid cases found. All cases have missing total samples."
                ))

                interpretText <- self$results$interpretText
                errorHtml <- sprintf("<div style='%s %s %s %s'>
                    <p style='margin: 0; %s'><strong>⚠️ ERROR: No Valid Data</strong></p>
                    <p style='margin: 10px 0 0 0; %s'>
                        All %d cases in the dataset have missing values for total samples.
                    </p>
                    <p style='margin: 10px 0 0 0; %s'><strong>Required Actions:</strong></p>
                    <ul style='margin: 5px 0 0 0; padding-left: 20px; %s'>
                        <li>Verify that the correct variable is selected for 'Total Samples'</li>
                        <li>Check that the variable contains numeric data</li>
                        <li>Ensure at least some cases have non-missing values</li>
                    </ul>
                </div>",
                private$.styleConstants$font, private$.styleConstants$bgLight,
                private$.styleConstants$borderWarning, private$.styleConstants$padding15,
                private$.styleConstants$fontSize15,
                private$.styleConstants$fontSize14, totalCasesInput,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14)
                interpretText$setContent(errorHtml)
                return()
            }

            # Check for invalid data: first detection > total samples
            invalidCases <- !is.na(firstDetectionData) & (firstDetectionData > totalSamplesData)
            nExcludedInvalidDetection <- 0
            if (any(invalidCases)) {
                nExcludedInvalidDetection <- sum(invalidCases)
                totalSamplesData <- totalSamplesData[!invalidCases]
                firstDetectionData <- firstDetectionData[!invalidCases]

                # Filter optional data for invalid cases too
                if (!is.null(positiveCountData)) {
                    positiveCountData <- positiveCountData[!invalidCases]
                }
                if (!is.null(positiveSamplesListData)) {
                    positiveSamplesListData <- positiveSamplesListData[!invalidCases]
                }
                if (!is.null(sampleTypeData)) {
                    sampleTypeData <- sampleTypeData[!invalidCases]
                }

                # Error 2: All remaining cases invalid after filtering
                if (length(firstDetectionData) == 0) {
                    dataInfo <- self$results$dataInfo
                    dataInfo$addRow(rowKey="error_invalid_all", values=list(
                        measure = "ERROR",
                        value = "No valid cases remaining after removing data errors"
                    ))

                    interpretText <- self$results$interpretText
                    errorHtml <- sprintf("<div style='%s %s %s %s'>
                        <p style='margin: 0; %s'><strong>⚠️ ERROR: Data Quality Issues</strong></p>
                        <p style='margin: 10px 0 0 0; %s'>
                            All %d remaining cases have invalid data (first detection > total samples).
                        </p>
                        <p style='margin: 10px 0 0 0; %s'><strong>Common Causes:</strong></p>
                        <ul style='margin: 5px 0 0 0; padding-left: 20px; %s'>
                            <li>First detection sample number exceeds total samples examined</li>
                            <li>Incorrect variable mapping (check that variables are assigned correctly)</li>
                            <li>Data entry errors in source data</li>
                        </ul>
                        <p style='margin: 10px 0 0 0; %s'><strong>Recommendation:</strong> Review source data for consistency.</p>
                    </div>",
                    private$.styleConstants$font, private$.styleConstants$bgLight,
                    private$.styleConstants$borderWarning, private$.styleConstants$padding15,
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$fontSize14, nExcludedInvalidDetection,
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$fontSize14)
                    interpretText$setContent(errorHtml)
                    return()
                }

                dataWarnings <- c(dataWarnings,
                    sprintf("%d cases removed due to first detection exceeding total samples", nExcludedInvalidDetection))
            }

            # Treat non-positive detection indices as censored (no lesion detected)
            invalidNonPositive <- !is.na(firstDetectionData) & firstDetectionData < 1
            if (any(invalidNonPositive)) {
                dataWarnings <- c(dataWarnings,
                    sprintf("%d cases recorded first detection < 1; treated as no lesion detected", sum(invalidNonPositive)))
                firstDetectionData[invalidNonPositive] <- NA
            }

            # Recalculate detected cases after cleaning
            detectedCases <- !is.na(firstDetectionData)
            nDetected <- sum(detectedCases)

            # Error 3: Zero positive cases - cannot perform analysis
            if (nDetected == 0) {
                dataInfo <- self$results$dataInfo
                nCases <- length(totalSamplesData)

                dataInfo$addRow(rowKey="total_cases", values=list(
                    measure = "Total cases analyzed",
                    value = as.character(nCases)
                ))

                dataInfo$addRow(rowKey="positive_cases", values=list(
                    measure = "Cases with lesion detected",
                    value = "0"
                ))

                dataInfo$addRow(rowKey="error_no_positive", values=list(
                    measure = "ERROR",
                    value = "Cannot estimate detection probability without positive cases"
                ))

                interpretText <- self$results$interpretText
                errorHtml <- sprintf("<div style='%s %s %s %s'>
                    <p style='margin: 0; %s'><strong>⚠️ ERROR: No Positive Cases</strong></p>
                    <p style='margin: 10px 0 0 0; %s'>
                        Analysis requires at least one case with detected lesions.
                        Current dataset has %d cases, but zero cases show lesion detection.
                    </p>
                    <p style='margin: 10px 0 0 0; %s'><strong>This could mean:</strong></p>
                    <ul style='margin: 5px 0 0 0; padding-left: 20px; %s'>
                        <li>All values in 'First Detection' variable are missing (NA)</li>
                        <li>Incorrect variable selected for 'First Detection'</li>
                        <li>Dataset truly contains no positive findings</li>
                    </ul>
                    <p style='margin: 10px 0 0 0; %s'><strong>Recommendation:</strong> Verify variable selection and data entry.</p>
                    <p style='margin: 10px 0 0 0; %s'><strong>Note:</strong> This module estimates sampling adequacy from observed detection patterns.
                    If no lesions are detected, estimation is not possible.</p>
                </div>",
                private$.styleConstants$font, private$.styleConstants$bgLight,
                private$.styleConstants$borderWarning, private$.styleConstants$padding15,
                private$.styleConstants$fontSize15,
                private$.styleConstants$fontSize14, nCases,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14)
                interpretText$setContent(errorHtml)
                return()
            }

            # Warning 1: Small sample size (detected cases only)
            if (nDetected < 10) {
                interpretText <- self$results$interpretText
                warningHtml <- sprintf("<div style='%s %s %s %s'>
                    <p style='margin: 0; %s'><strong>⚠️ WARNING: Small Sample Size</strong></p>
                    <p style='margin: 10px 0 0 0; %s'>
                        Only %d cases with detected lesions. Results may be unreliable.
                    </p>
                    <p style='margin: 10px 0 0 0; %s'><strong>Recommendations:</strong></p>
                    <ul style='margin: 5px 0 0 0; padding-left: 20px; %s'>
                        <li>Collect more cases for robust estimates (recommended: n ≥ 30 for bootstrap analysis)</li>
                        <li>Interpret confidence intervals with caution</li>
                        <li>Consider using 'Auto' estimation method which adapts to sample size</li>
                    </ul>
                </div>",
                private$.styleConstants$font, private$.styleConstants$bgLight,
                private$.styleConstants$borderWarning, private$.styleConstants$padding15,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14, nDetected,
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontSize14)
                interpretText$setContent(warningHtml)
            }

            # Set seed if requested
            if (self$options$setSeed) {
                set.seed(self$options$seedValue)
            }

            # === Key Results Summary ===
            keyResults <- self$results$keyResults
            keyResultsHtml <- "<div style='padding: 15px; background: #e9f5ff; border: 2px solid #b3d7ff; margin-bottom: 20px;'>
                                <h3 style='margin: 0 0 10px 0; font-size: 16px; color: #0056b3;'>Key Results</h3>
                                <p style='margin: 0; font-size: 14px; color: #0056b3;'>This section will be populated with the key findings from the analysis.</p>
                             </div>"
            keyResults$setContent(keyResultsHtml)

            # === Data Summary ===
            dataInfo <- self$results$dataInfo

            nCases <- length(totalSamplesData)
            nNoDetection <- nCases - nDetected

            totalSubmitted <- sum(totalSamplesData, na.rm = TRUE)  # Total samples submitted (includes unexamined)
            examinedDetected <- sum(firstDetectionData[detectedCases], na.rm = TRUE)
            examinedNondetected <- sum(totalSamplesData[!detectedCases], na.rm = TRUE)
            totalExamined <- examinedDetected + examinedNondetected  # Samples actually examined across all cases
            meanSamplesPerCase <- mean(totalSamplesData, na.rm = TRUE)
            medianFirst <- if (nDetected > 0) median(firstDetectionData[detectedCases], na.rm = TRUE) else NA

            dataInfo <- self$results$dataInfo

            totalSubmittedRaw <- sum(rawTotalSamplesData, na.rm = TRUE)

            dataInfo$addRow(rowKey="total_cases", values=list(
                measure = "Total cases supplied",
                value = as.character(totalCasesInput)
            ))

            dataInfo$addRow(rowKey="cases_analyzed", values=list(
                measure = "Cases analyzed",
                value = as.character(nCases)
            ))

            if (nExcludedMissingTotal > 0) {
                dataInfo$addRow(rowKey="excluded_missing", values=list(
                    measure = "Excluded: missing total samples",
                    value = as.character(nExcludedMissingTotal)
                ))
            }

            if (nExcludedInvalidDetection > 0) {
                dataInfo$addRow(rowKey="excluded_invalid", values=list(
                    measure = "Excluded: first detection > total",
                    value = as.character(nExcludedInvalidDetection)
                ))
            }

            dataInfo$addRow(rowKey="total_input", values=list(
                measure = "Total samples (input)",
                value = sprintf("%d (recorded)", totalSubmittedRaw)
            ))

            dataInfo$addRow(rowKey="total_analyzed", values=list(
                measure = "Total samples analyzed",
                value = sprintf("%d (up to first detection)", totalExamined)
            ))

            dataInfo$addRow(rowKey="mean_samples", values=list(
                measure = "Mean samples per analyzed case",
                value = sprintf("%.2f", meanSamplesPerCase)
            ))

            dataInfo$addRow(rowKey="median_first", values=list(
                measure = "Median first detection",
                value = if (!is.na(medianFirst)) sprintf("%.0f", medianFirst) else "No lesions detected"
            ))

            dataInfo$addRow(rowKey="no_detection", values=list(
                measure = "Cases without detected lesion",
                value = sprintf("%d", nNoDetection)
            ))

            if (length(dataWarnings) > 0) {
                dataInfo$addRow(rowKey="data_notes", values=list(
                    measure = "Data notes",
                    value = paste(dataWarnings, collapse = "; ")
                ))
            }

            # === Binomial Model ===
            pEstimate <- NA_real_
            estimationMethod <- "Not calculated"

            if (self$options$showBinomialModel) {

                # Determine which estimation method to use
                methodChoice <- self$options$estimationMethod

                # Method 1: Empirical Proportion (preferred if positiveCount available)
                if ((methodChoice == "auto" || methodChoice == "empirical") &&
                    !is.null(positiveCountData) && nDetected > 0) {

                    # Use empirical proportion: sum of positive samples / sum of total samples (positive cases only)
                    positive_idx <- !is.na(firstDetectionData)
                    total_samples_positive <- sum(totalSamplesData[positive_idx], na.rm = TRUE)
                    total_positive_samples <- sum(positiveCountData[positive_idx], na.rm = TRUE)

                    if (total_samples_positive > 0) {
                        pEstimate <- total_positive_samples / total_samples_positive
                        estimationMethod <- "Empirical Proportion (uses all positive samples)"
                    }
                }

                # Method 2: Geometric MLE (fallback or explicit choice)
                if ((is.na(pEstimate) || methodChoice == "geometric") && nDetected > 0) {

                    # Use geometric MLE: q = 1 / mean(first detection position)
                    positive_first <- firstDetectionData[!is.na(firstDetectionData)]

                    if (length(positive_first) > 0) {
                        mean_first_detection <- mean(positive_first, na.rm = TRUE)
                        if (mean_first_detection > 0) {
                            pEstimate <- 1 / mean_first_detection
                            estimationMethod <- "Geometric MLE (first detection only)"
                        }
                    }
                }

                # Guard against degenerate estimates for downstream calculations
                pForCalc <- pEstimate
                if (is.na(pForCalc)) {
                    pForCalc <- NA_real_
                } else if (pForCalc <= 0) {
                    pForCalc <- 0
                } else if (pForCalc >= 1) {
                    pForCalc <- 1 - 1e-12
                }

                binomialText <- self$results$binomialText

                # Context-specific warning for inappropriate use of binomial model
                analysisContext <- self$options$analysisContext
                binomialWarning <- ""

                if (analysisContext == "tumor") {
                    binomialWarning <- sprintf("<div style='background: #ffebee; border: 1px solid #ef5350; padding: 12px; margin: 10px 0; border-radius: 4px;'>
                        <p style='%s margin: 0;'><b>⚠️ WARNING: Binomial Model May Underestimate Sensitivity for Tumor Sampling</b></p>
                        <p style='%s margin: 8px 0 0 0;'>
                            Sequential tumor samples (blocks) are <b>not independent</b> - they are serial sections through the same lesion
                            with spatial clustering. The binomial model assumes each sample is an independent event, which leads to
                            <b>systematic underestimation</b> of cumulative detection sensitivity.
                        </p>
                        <p style='%s margin: 8px 0 0 0;'>
                            <b>Recommendation:</b> Use <b>Empirical Cumulative Detection</b> (non-parametric) with <b>Bootstrap CIs</b>
                            instead. These methods correctly handle spatial dependence and are the gold standard for tumor block sampling adequacy.
                        </p>
                        <p style='%s margin: 8px 0 0 0;'>
                            <em>Reference: See vignette 'Independent vs Dependent Sampling' for detailed explanation</em>
                        </p>
                    </div>",
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize12, private$.styleConstants$colorSecondary))
                } else if (analysisContext == "margin") {
                    binomialWarning <- sprintf("<div style='background: #fff3e0; border: 1px solid #ff9800; padding: 12px; margin: 10px 0; border-radius: 4px;'>
                        <p style='%s margin: 0;'><b>⚠️ Caution: Margin samples may show spatial clustering</b></p>
                        <p style='%s margin: 8px 0 0 0;'>
                            Consider using empirical methods if margin positivity is geographically clustered.
                        </p>
                    </div>",
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary))
                }

                # Calculate description based on data available
                if (!is.null(positiveCountData) && estimationMethod == "Empirical Proportion (uses all positive samples)") {
                    positive_idx <- !is.na(firstDetectionData)
                    total_samples_positive <- sum(totalSamplesData[positive_idx], na.rm = TRUE)
                    total_positive_samples <- sum(positiveCountData[positive_idx], na.rm = TRUE)
                    data_desc <- sprintf("Based on %d positive cases with %d positive samples out of %d total samples examined.",
                                        nDetected, total_positive_samples, total_samples_positive)
                } else {
                    positive_first <- firstDetectionData[!is.na(firstDetectionData)]
                    mean_first <- if (length(positive_first) > 0) mean(positive_first, na.rm = TRUE) else NA
                    data_desc <- sprintf("Based on %d positive cases with mean first detection at sample %.2f.",
                                        nDetected, ifelse(is.na(mean_first), 0, mean_first))
                }

                html <- sprintf("<div style='%s'>
                    %s
                    <div style='%s'>
                        <h4 style='%s'>Binomial Probability Model</h4>
                        <p style='%s'>
                            Estimated per-sample detection probability: <b style='%s'>q = %s</b>
                        </p>
                        <p style='%s'>
                            <b>Estimation Method:</b> %s
                        </p>
                        <p style='%s'>
                            %s
                        </p>
                        <p style='%s'>
                            <b>Formula:</b> P(detect ≥ 1 in n samples) = 1 - (1-q)<sup>n</sup>
                        </p>
                    </div>
                </div>",
                private$.styleConstants$font,
                binomialWarning,  # Insert context-specific warning
                private$.buildStyle(
                    private$.styleConstants$bgLight,
                    private$.styleConstants$borderLeft,
                    private$.styleConstants$padding15,
                    private$.styleConstants$margin10
                ),
                private$.buildStyle(
                    private$.styleConstants$colorPrimary,
                    private$.styleConstants$fontSize15,
                    "margin: 0 0 10px 0;"
                ),
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                ),
                private$.styleConstants$colorPrimary,
                if (!is.na(pEstimate)) sprintf("%.4f", pEstimate) else "NA",
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                ),
                estimationMethod,
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorSecondary,
                    "margin: 0 0 10px 0;"
                ),
                data_desc,
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0;"
                ))
                binomialText$setContent(html)

                # Calculate detection probabilities for different sample sizes
                binomialTable <- self$results$binomialTable

                binomProbVec <- rep(NA_real_, maxSamp)
                prevProb <- 0
                for (i in 1:maxSamp) {
                    if (is.na(pForCalc)) {
                        cumProb <- NA
                        marginal <- NA
                    } else {
                        cumProb <- 1 - (1 - pForCalc)^i
                        marginal <- cumProb - prevProb
                    }

                    binomialTable$addRow(rowKey=paste0("n_", i), values=list(
                        nSamples = i,
                        cumProb = cumProb,
                        marginalGain = marginal
                    ))
                    if (!is.na(cumProb)) {
                        prevProb <- cumProb
                        binomProbVec[i] <- cumProb
                    }
                }

                # Minimum samples for different confidence levels
                recommendTable <- self$results$recommendTable

                confLevels <- c(0.80, 0.90, 0.95, 0.99)
                for (i in seq_along(confLevels)) {
                    conf <- confLevels[i]
                    if (is.na(pForCalc) || pForCalc <= 0) {
                        nMin <- NA
                    } else if (pForCalc >= 1 - 1e-12) {
                        nMin <- 1
                    } else {
                        nMin <- ceiling(log(1 - conf) / log(1 - pForCalc))
                    }

                    recommendTable$addRow(rowKey=paste0("conf_", conf), values=list(
                        confidence = conf,
                        minSamples = nMin
                    ))
                }

                addRecommendation(
                    method = "Binomial",
                    probVec = binomProbVec,
                    priority = 4,
                    description = "Independent detection probability model",
                    detail = if (!is.na(pEstimate)) sprintf("p = %.4f", pEstimate) else "Per-sample probability unavailable"
                )
            }

            # === Probability Explanation ===
            probabilityExplanation <- self$results$probabilityExplanation

            # Calculate prevalence and example probabilities
            prevalence <- nDetected / nCases

            # Use pEstimate if available, otherwise use empirical estimate
            qForExamples <- if (!is.na(pEstimate) && pEstimate > 0) {
                pEstimate
            } else if (nDetected > 0) {
                # Fallback: use geometric MLE
                positive_first <- firstDetectionData[!is.na(firstDetectionData)]
                if (length(positive_first) > 0) {
                    1 / mean(positive_first, na.rm = TRUE)
                } else {
                    0.40  # Reasonable default for demonstration
                }
            } else {
                0.40  # Reasonable default for demonstration
            }

            # Calculate conditional probabilities (sensitivity)
            conditional_3 <- 1 - (1 - qForExamples)^3
            conditional_5 <- 1 - (1 - qForExamples)^5
            conditional_10 <- 1 - (1 - qForExamples)^10

            # Calculate population-level probabilities
            population_3 <- prevalence * conditional_3
            population_5 <- prevalence * conditional_5
            population_10 <- prevalence * conditional_10

            html <- sprintf("<div style='%s'>
                <h4 style='%s'>📊 Two Ways to Measure Detection Performance</h4>

                <p style='%s'>
                    This analysis reports probabilities in two ways, depending on the clinical question:
                </p>

                <div style='%s'>
                    <h5 style='%s'>
                        1️⃣ Conditional Detection (Sensitivity) - \"If metastasis is present\"
                    </h5>
                    <p style='%s'>
                        <strong>Clinical question:</strong> If metastasis is truly present, how many blocks do I need to detect it?
                    </p>
                    <p style='%s'>
                        <strong>Best used for:</strong> Setting minimum blocks per tumour-positive case, validating adequacy targets, or counselling surgeons on block counts required to avoid false reassurance.
                    </p>
                    <p style='%s'>
                        <strong>Formula:</strong> P(detect | metastasis present) = 1 - (1-q)<sup>n</sup>
                    </p>
                    <p style='%s'>
                        <strong>In Your Data:</strong> Among %d cases <em>with</em> detected metastasis (q = %.3f):
                    </p>
                    <ul style='%s'>
                        <li>With 3 samples: Detects %.1f%% of positive cases</li>
                        <li>With 5 samples: Detects %.1f%% of positive cases</li>
                        <li>With 10 samples: Detects %.1f%% of positive cases</li>
                    </ul>
                    <p style='%s'>
                        <strong style='%s'>Clinical Use:</strong>
                        <em>\"How many samples do I need to confidently rule out metastasis?\"</em>
                        This is the probability shown in the <strong>Diagnostic Yield Curve</strong>.
                    </p>
                </div>

                <div style='%s'>
                    <h5 style='%s'>
                        2️⃣ Population-Level Detection - \"Overall detection rate\"
                    </h5>
                    <p style='%s'>
                        <strong>Clinical question:</strong> Across all specimens submitted (positive + negative), how often do we detect tumour with n blocks?
                    </p>
                    <p style='%s'>
                        <strong>Best used for:</strong> Monitoring service-level performance, comparing surgeons/protocols, or highlighting when low prevalence—not sampling—limits detection.
                    </p>
                    <p style='%s'>
                        <strong>Formula:</strong> P(detect overall) = Prevalence × Sensitivity = π × [1 - (1-q)<sup>n</sup>]
                    </p>
                    <p style='%s'>
                        <strong>In Your Data:</strong> Observed prevalence = %.1f%% (%d/%d cases had metastasis):
                    </p>
                    <ul style='%s'>
                        <li>With 3 samples: Detects metastasis in %.1f%% of all specimens</li>
                        <li>With 5 samples: Detects metastasis in %.1f%% of all specimens</li>
                        <li>With 10 samples: Detects metastasis in %.1f%% of all specimens</li>
                    </ul>
                    <p style='%s'>
                        <strong style='%s'>Clinical Use:</strong>
                        <em>\"What percentage of incoming specimens will test positive?\"</em>
                        This is useful for workload planning and quality metrics.
                    </p>
                </div>

                <div style='%s'>
                    <p style='%s'>
                        <strong>⚠️ Important:</strong> These are fundamentally different quantities!
                    </p>
                    <p style='%s'>
                        • <strong>Conditional (sensitivity)</strong> assumes metastasis is present
                    </p>
                    <p style='%s'>
                        • <strong>Population-level</strong> includes cases without metastasis
                    </p>
                    <p style='%s'>
                        The ratio between them equals the prevalence (%.1f%% in your data).
                        This module focuses on <strong>conditional probability (sensitivity)</strong>
                        because that's what determines sampling adequacy.
                    </p>
                </div>
            </div>",
            private$.styleConstants$font,
            private$.buildStyle(
                private$.styleConstants$bgLighter,
                private$.styleConstants$borderSecondary,
                private$.styleConstants$padding20
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                "margin: 0 0 15px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$bgWhite,
                private$.styleConstants$borderLeft,
                private$.styleConstants$padding15,
                private$.styleConstants$margin15
            ),
            private$.buildStyle(
                private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize15,
                "margin: 0 0 10px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            nDetected, qForExamples,
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0; padding-left: 25px;"
            ),
            conditional_3 * 100, conditional_5 * 100, conditional_10 * 100,
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                "margin: 0;"
            ),
            private$.styleConstants$colorSuccess,
            private$.buildStyle(
                private$.styleConstants$bgWhite,
                private$.styleConstants$borderLeft,
                private$.styleConstants$padding15,
                private$.styleConstants$margin15
            ),
            private$.buildStyle(
                private$.styleConstants$colorPrimary,
                private$.styleConstants$fontSize15,
                "margin: 0 0 10px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            prevalence * 100, nDetected, nCases,
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0; padding-left: 25px;"
            ),
            population_3 * 100, population_5 * 100, population_10 * 100,
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                "margin: 0;"
            ),
            private$.styleConstants$colorInfo,
            private$.buildStyle(
                private$.styleConstants$bgLight,
                private$.styleConstants$borderWarning,
                private$.styleConstants$padding15,
                private$.styleConstants$margin15
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$fontWeight700,
                "margin: 0 0 10px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0 0 8px 0;"
            ),
            private$.buildStyle(
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorSecondary,
                "margin: 0;"
            ),
            prevalence * 100)

            probabilityExplanation$setContent(html)

            # === Bootstrap Analysis ===
            if (self$options$showBootstrap) {

                # Validation: Check if we have enough positive cases for bootstrap
                if (nDetected < 3) {
                    bootstrapText <- self$results$bootstrapText
                    errorHtml <- sprintf("<div style='%s %s %s %s'>
                        <p style='margin: 0; %s'><strong>⚠️ ERROR: Insufficient Data for Bootstrap</strong></p>
                        <p style='margin: 10px 0 0 0; %s'>
                            Bootstrap analysis requires at least 3 positive cases.
                            Current dataset has only %d positive case%s.
                        </p>
                        <p style='margin: 10px 0 0 0; %s'><strong>Recommendation:</strong>
                            Disable bootstrap analysis or collect more positive cases.</p>
                    </div>",
                    private$.styleConstants$font, private$.styleConstants$bgLight,
                    private$.styleConstants$borderWarning, private$.styleConstants$padding15,
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$fontSize14, nDetected, if (nDetected == 1) "" else "s",
                    private$.styleConstants$fontSize14)
                    bootstrapText$setContent(errorHtml)
                } else {

                bootstrapText <- self$results$bootstrapText
                html <- sprintf("<div style='%s'>
                    <div style='%s'>
                        <h4 style='%s'>Bootstrap Resampling Analysis</h4>
                        <p style='%s'>
                            Empirical sensitivity estimates based on <b style='%s'>%d bootstrap iterations</b>.
                        </p>
                        <p style='%s'>
                            This method resamples cases with replacement to estimate sensitivity and
                            confidence intervals without parametric assumptions.
                        </p>
                        <p style='%s'>
                            <b>Reference:</b> Skala SL, Hagemann IS. <em>Int J Gynecol Pathol.</em> 2015;34(4):374-378.
                        </p>
                    </div>
                </div>",
                private$.styleConstants$font,
                private$.buildStyle(
                    private$.styleConstants$bgLight,
                    private$.styleConstants$borderLeft,
                    private$.styleConstants$padding15,
                    private$.styleConstants$margin10
                ),
                private$.buildStyle(
                    private$.styleConstants$colorPrimary,
                    private$.styleConstants$fontSize15,
                    "margin: 0 0 10px 0;"
                ),
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                ),
                private$.styleConstants$colorPrimary,
                nBoot,
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                ),
                private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0;"
                ))
                bootstrapText$setContent(html)

                # Perform bootstrap
                bootstrapResults <- matrix(0, nrow = nBoot, ncol = maxSamp)

                bootstrapMeans <- rep(NA_real_, maxSamp)
                bootstrapCILower <- rep(NA_real_, maxSamp)
                bootstrapCIUpper <- rep(NA_real_, maxSamp)

                # Add checkpoint calls for progress (only for large iterations)
                if (nBoot >= 5000) {
                    checkpointInterval <- floor(nBoot / 10)  # Update every 10%
                } else {
                    checkpointInterval <- nBoot + 1  # Don't checkpoint for small iterations
                }

                # Get positive cases only for bootstrap (conditional probability)
                positiveCases <- !is.na(firstDetectionData)
                positiveIndices <- which(positiveCases)
                nPositiveCases <- length(positiveIndices)

                for (iter in 1:nBoot) {
                    # Progress checkpoint every 10%
                    if (iter %% checkpointInterval == 0) {
                        private$.checkpoint()
                    }

                    # Resample from POSITIVE cases only with replacement
                    sampledPositiveIndices <- sample(positiveIndices, nPositiveCases, replace = TRUE)
                    sampledFirst <- firstDetectionData[sampledPositiveIndices]

                    # Calculate detection rate for each sample count (conditional)
                    for (j in 1:maxSamp) {
                        detected <- sum(!is.na(sampledFirst) & sampledFirst <= j)
                        bootstrapResults[iter, j] <- detected / nPositiveCases
                    }
                }

                # Store for plotting
                private$.bootstrapResults <- bootstrapResults

                # Populate bootstrap table
                bootstrapTable <- self$results$bootstrapTable

                for (i in 1:maxSamp) {
                    meanSens <- mean(bootstrapResults[, i])
                    ciLower <- quantile(bootstrapResults[, i], 0.025)
                    ciUpper <- quantile(bootstrapResults[, i], 0.975)

                    bootstrapMeans[i] <- meanSens
                    bootstrapCILower[i] <- ciLower
                    bootstrapCIUpper[i] <- ciUpper

                    bootstrapTable$addRow(rowKey=paste0("n_", i), values=list(
                        nSamples = i,
                        meanSens = meanSens,
                        ciLower = ciLower,
                        ciUpper = ciUpper
                    ))
                }

                bootstrapMeansVec <- bootstrapMeans
                bootstrapCILowerVec <- bootstrapCILower
                bootstrapCIUpperVec <- bootstrapCIUpper

                targetCandidates <- which(!is.na(bootstrapMeans) & bootstrapMeans >= targetConf)
                bootstrapTargetIdx <- if (length(targetCandidates) > 0) targetCandidates[1] else NA_integer_

                ciTarget <- if (!is.na(bootstrapTargetIdx)) c(bootstrapCILowerVec[bootstrapTargetIdx], bootstrapCIUpperVec[bootstrapTargetIdx]) else NULL

                addRecommendation(
                    method = "Bootstrap",
                    probVec = bootstrapMeans,
                    priority = 1,
                    description = "Empirical resampling of cases",
                    detail = sprintf("%d iterations", nBoot),
                    ci = ciTarget
                )
                }  # End else block for bootstrap validation
            }

            # Store data for plotting
            private$.totalSamplesData <- totalSamplesData
            private$.firstDetectionData <- firstDetectionData
            private$.pEstimate <- pEstimate
            private$.maxSamp <- maxSamp

            # Calculate observed conditional detection probability (sensitivity)
            # Only among positive cases, not population-level
            nPositiveCases <- sum(!is.na(firstDetectionData))

            observedProbVec <- sapply(1:maxSamp, function(n) {
                if (nPositiveCases == 0) return(0)
                # Count positive cases detected by sample n / total positive cases
                sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
            })
            addRecommendation(
                method = "Empirical",
                probVec = observedProbVec,
                priority = 5,
                description = "Observed cumulative detection in dataset",
                detail = sprintf("Conditional probability among %d positive cases", nPositiveCases)
            )

            obsPercents <- observedProbVec * 100
            obs1 <- if (length(obsPercents) >= 1) obsPercents[1] else NA_real_
            obs2 <- if (length(obsPercents) >= 2) obsPercents[2] else NA_real_
            obs3 <- if (length(obsPercents) >= 3) obsPercents[3] else NA_real_
            obs4 <- if (length(obsPercents) >= 4) obsPercents[4] else NA_real_
            obsIndices <- seq_len(min(4, length(obsPercents)))
            obsListHtml <- ""
            if (length(obsIndices) > 0) {
                obsListHtml <- paste(
                    sprintf("<li>%d sample%s: %.1f%%%%</li>",
                        obsIndices,
                        ifelse(obsIndices == 1, "", "s"),
                        obsPercents[obsIndices]),
                    collapse = "")
            }

            # ===== Tumor Burden Analysis =====
            # Modern implementation: Analyzes extent of tumor involvement using
            # sample positivity ratio (SPR) and distribution patterns

            if (self$options$showTumorBurden && !is.null(self$options$positiveCassettes)) {
                private$.checkpoint()

                # Get positive samples count data
                positiveCassettesVar <- self$options$positiveCassettes
                positiveCassettesEsc <- private$.escapeVar(positiveCassettesVar)
                positiveCassettesData <- jmvcore::toNumeric(data[[positiveCassettesEsc]])

                # Handle factor/labelled data
                if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, 'labels'))) {
                    positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                }

                # Filter to valid analyzed cases
                positiveCassettesData <- positiveCassettesData[validCases]
                if (!is.null(invalidCases)) {
                    positiveCassettesData <- positiveCassettesData[!invalidCases]
                }

                # Store for other analyses
                private$.positiveCassettesData <- positiveCassettesData

                # === Explanatory Text ===
                tumorBurdenText <- self$results$tumorBurdenText

                html <- sprintf("<div style='%s'>
                    <h4 style='%s'>Tumor Burden Analysis</h4>
                    <p style='%s'>
                        Analyzes the <strong>sample positivity ratio (SPR)</strong>: the proportion of samples
                        containing tumor out of all samples examined per case.
                    </p>
                    <p style='%s'>
                        SPR provides insights into:
                    </p>
                    <ul style='%s'>
                        <li>Extent of tumor involvement beyond first detection</li>
                        <li>Tumor distribution patterns (focal vs diffuse)</li>
                        <li>Relationship between sampling intensity and detection completeness</li>
                    </ul>
                    <p style='%s'>
                        <strong>Note:</strong> SPR is calculated only for cases with detected tumor.
                        Higher SPR may indicate more extensive disease or better sampling.
                    </p>
                </div>",
                private$.buildStyle(private$.styleConstants$font),
                private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                tumorBurdenText$setContent(html)

                # === Calculate SPR Statistics ===
                tumorBurdenInfo <- self$results$tumorBurdenInfo

                # Filter to positive cases only
                positive_cases_idx <- !is.na(firstDetectionData)
                spr_values <- positiveCassettesData[positive_cases_idx] / totalSamplesData[positive_cases_idx]

                # Summary statistics
                n_positive_for_burden <- sum(positive_cases_idx)
                mean_spr <- mean(spr_values, na.rm = TRUE)
                median_spr <- median(spr_values, na.rm = TRUE)
                sd_spr <- sd(spr_values, na.rm = TRUE)
                min_spr <- min(spr_values, na.rm = TRUE)
                max_spr <- max(spr_values, na.rm = TRUE)

                # Overall totals
                total_positive_samples <- sum(positiveCassettesData[positive_cases_idx], na.rm = TRUE)
                total_samples_positive_cases <- sum(totalSamplesData[positive_cases_idx], na.rm = TRUE)
                overall_spr <- total_positive_samples / total_samples_positive_cases

                # Populate statistics table
                tumorBurdenInfo$addRow(rowKey="n_cases", values=list(
                    measure = "Cases analyzed (with tumor)",
                    value = sprintf("%d", n_positive_for_burden)
                ))

                tumorBurdenInfo$addRow(rowKey="mean_spr", values=list(
                    measure = "Mean SPR",
                    value = sprintf("%.3f (SD: %.3f)", mean_spr, sd_spr)
                ))

                tumorBurdenInfo$addRow(rowKey="median_spr", values=list(
                    measure = "Median SPR",
                    value = sprintf("%.3f", median_spr)
                ))

                tumorBurdenInfo$addRow(rowKey="range_spr", values=list(
                    measure = "SPR range",
                    value = sprintf("%.3f - %.3f", min_spr, max_spr)
                ))

                tumorBurdenInfo$addRow(rowKey="overall_spr", values=list(
                    measure = "Overall SPR (pooled)",
                    value = sprintf("%.3f (%d / %d samples)", overall_spr, total_positive_samples, total_samples_positive_cases)
                ))

                # === Tumor Distribution Pattern Classification ===
                cassetteDistribution <- self$results$cassetteDistribution

                # Classify based on number of positive samples
                # Focal: 1 positive, Limited: 2-3 positive, Moderate: 4-6 positive, Extensive: 7+ positive
                n_focal <- sum(positiveCassettesData[positive_cases_idx] == 1, na.rm = TRUE)
                n_limited <- sum(positiveCassettesData[positive_cases_idx] >= 2 &
                                 positiveCassettesData[positive_cases_idx] <= 3, na.rm = TRUE)
                n_moderate <- sum(positiveCassettesData[positive_cases_idx] >= 4 &
                                  positiveCassettesData[positive_cases_idx] <= 6, na.rm = TRUE)
                n_extensive <- sum(positiveCassettesData[positive_cases_idx] >= 7, na.rm = TRUE)

                # Add to table
                if (n_focal > 0) {
                    cassetteDistribution$addRow(rowKey="focal", values=list(
                        pattern = "Focal (1 positive sample)",
                        count = n_focal,
                        percent = n_focal / n_positive_for_burden
                    ))
                }

                if (n_limited > 0) {
                    cassetteDistribution$addRow(rowKey="limited", values=list(
                        pattern = "Limited (2-3 positive)",
                        count = n_limited,
                        percent = n_limited / n_positive_for_burden
                    ))
                }

                if (n_moderate > 0) {
                    cassetteDistribution$addRow(rowKey="moderate", values=list(
                        pattern = "Moderate (4-6 positive)",
                        count = n_moderate,
                        percent = n_moderate / n_positive_for_burden
                    ))
                }

                if (n_extensive > 0) {
                    cassetteDistribution$addRow(rowKey="extensive", values=list(
                        pattern = "Extensive (7+ positive)",
                        count = n_extensive,
                        percent = n_extensive / n_positive_for_burden
                    ))
                }

                # === Additional Insight ===
                # Calculate correlation between total samples and positive samples
                if (n_positive_for_burden >= 3) {
                    cor_result <- tryCatch({
                        cor.test(totalSamplesData[positive_cases_idx],
                                positiveCassettesData[positive_cases_idx],
                                method = "spearman")
                    }, error = function(e) NULL)

                    if (!is.null(cor_result) && !is.na(cor_result$estimate)) {
                        tumorBurdenInfo$addRow(rowKey="correlation", values=list(
                            measure = "Correlation (samples examined vs positive)",
                            value = sprintf("ρ = %.3f (p %s %.3f)",
                                          cor_result$estimate,
                                          if (cor_result$p.value < 0.001) "<" else "=",
                                          if (cor_result$p.value < 0.001) 0.001 else cor_result$p.value)
                        ))
                    }
                }
            }

            # ===== Stage Migration Analysis =====
            # Analyzes whether examining fewer samples leads to understaging

            if (self$options$showStageMigration && !is.null(self$options$positiveCassettes)) {
                private$.checkpoint()

                # Get positive samples data (reuse if already loaded from tumor burden)
                if (exists("positiveCassettesData", inherits = FALSE)) {
                    # Already loaded in tumor burden section
                    positiveCassettesData <- private$.positiveCassettesData
                } else {
                    # Load it now
                    positiveCassettesVar <- self$options$positiveCassettes
                    positiveCassettesEsc <- private$.escapeVar(positiveCassettesVar)
                    positiveCassettesData <- jmvcore::toNumeric(data[[positiveCassettesEsc]])

                    if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, 'labels'))) {
                        positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                    }

                    positiveCassettesData <- positiveCassettesData[validCases]
                    if (!is.null(invalidCases)) {
                        positiveCassettesData <- positiveCassettesData[!invalidCases]
                    }
                }

                # === Explanatory Text ===
                stageMigrationText <- self$results$stageMigrationText

                html <- sprintf("<div style='%s'>
                    <h4 style='%s'>Stage Migration Analysis</h4>
                    <p style='%s'>
                        Examines whether examining fewer samples leads to <strong>understaging</strong>
                        (missing tumor present in later samples).
                    </p>
                    <p style='%s'>
                        Compares detection rates between cases with fewer vs more samples examined.
                        A significant difference suggests inadequate sampling may lead to false negatives.
                    </p>
                    <p style='%s'>
                        <strong>Reference:</strong> Habib et al. (2024), Goess et al. (2024) - lymph node adequacy methods.
                    </p>
                </div>",
                private$.buildStyle(private$.styleConstants$font),
                private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                stageMigrationText$setContent(html)

                # Analyze detection rates by cassette groups
                stageMigrationTable <- self$results$stageMigrationTable

                # Define groups based on data quartiles or standard thresholds
                # Using standard thresholds: <median, >=median
                medianCassettes <- median(totalSamplesData)

                # Group 1: Below median
                group1 <- totalSamplesData < medianCassettes
                nGroup1 <- sum(group1)
                nPosGroup1 <- sum(positiveCassettesData[group1] > 0)
                rateGroup1 <- if(nGroup1 > 0) nPosGroup1 / nGroup1 else 0

                # Group 2: At or above median
                group2 <- totalSamplesData >= medianCassettes
                nGroup2 <- sum(group2)
                nPosGroup2 <- sum(positiveCassettesData[group2] > 0)
                rateGroup2 <- if(nGroup2 > 0) nPosGroup2 / nGroup2 else 0

                stageMigrationTable$addRow(rowKey="group_1", values=list(
                    cassettes = sprintf("<%d", medianCassettes),
                    nCases = nGroup1,
                    nPositive = nPosGroup1,
                    positivityRate = rateGroup1
                ))
                stageMigrationTable$addRow(rowKey="group_2", values=list(
                    cassettes = sprintf("≥%d", medianCassettes),
                    nCases = nGroup2,
                    nPositive = nPosGroup2,
                    positivityRate = rateGroup2
                ))
                stageMigrationTable$addRow(rowKey="group_3", values=list(
                    cassettes = "Absolute difference",
                    nCases = NA,
                    nPositive = NA,
                    positivityRate = abs(rateGroup2 - rateGroup1)
                ))
            }

            # === Correlation Analysis ===
            if (self$options$showCorrelation && !is.null(positiveCassettes)) {

                positiveCassettesEsc <- private$.escapeVar(positiveCassettes)
                positiveCassettesData <- jmvcore::toNumeric(data[[positiveCassettesEsc]])

                if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, 'labels'))) {
                    positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                }

                positiveCassettesData <- positiveCassettesData[validCases]
                if (!is.null(invalidCases)) {
                    positiveCassettesData <- positiveCassettesData[!invalidCases]
                }

                correlationText <- self$results$correlationText
                html <- "<h4>Examined vs Positive Correlation</h4>
                <p>Relationship between total cassettes examined and number of positive cassettes.</p>
                <p>Positive correlation suggests more extensive sampling yields more detection.</p>"
                correlationText$setContent(html)

                # Calculate correlation
                corTest <- cor.test(totalSamplesData, positiveCassettesData, method = "spearman")

                correlationStats <- self$results$correlationStats
                correlationStats$addRow(rowKey="r_value", values=list(
                    statistic = "Spearman's rho",
                    value = sprintf("%.3f", corTest$estimate)
                ))
                correlationStats$addRow(rowKey="p_value", values=list(
                    statistic = "p-value",
                    value = sprintf("%.4f", corTest$p.value)
                ))
                correlationStats$addRow(rowKey="n_cases", values=list(
                    statistic = "Interpretation",
                    value = if(corTest$p.value < 0.05) {
                        if(corTest$estimate > 0) "Significant positive correlation" else "Significant negative correlation"
                    } else "No significant correlation"
                ))
            }

            # === Distribution Pattern Analysis (Single vs Summed) ===
            maxPositiveSingle <- self$options$maxPositiveSingle

            if (self$options$showDistributionPattern && !is.null(positiveCassettes) && !is.null(maxPositiveSingle)) {

                # Get both variables
                positiveCassettesEsc <- private$.escapeVar(positiveCassettes)
                maxPositiveSingleEsc <- private$.escapeVar(maxPositiveSingle)

                positiveCassettesData <- jmvcore::toNumeric(data[[positiveCassettesEsc]])
                maxPositiveSingleData <- jmvcore::toNumeric(data[[maxPositiveSingleEsc]])

                # Handle labelled data
                if (is.factor(positiveCassettesData) || !is.null(attr(positiveCassettesData, 'labels'))) {
                    positiveCassettesData <- as.numeric(as.character(positiveCassettesData))
                }
                if (is.factor(maxPositiveSingleData) || !is.null(attr(maxPositiveSingleData, 'labels'))) {
                    maxPositiveSingleData <- as.numeric(as.character(maxPositiveSingleData))
                }

                # Filter to valid cases
                positiveCassettesData <- positiveCassettesData[validCases]
                maxPositiveSingleData <- maxPositiveSingleData[validCases]
                if (!is.null(invalidCases)) {
                    positiveCassettesData <- positiveCassettesData[!invalidCases]
                    maxPositiveSingleData <- maxPositiveSingleData[!invalidCases]
                }

                # Get threshold
                threshold <- self$options$distributionThreshold

                # Distribution Pattern Text
                distributionPatternText <- self$results$distributionPatternText
                html <- sprintf("<h4>Distribution Pattern Analysis (Single vs Summed)</h4>
                <p>Based on Ates et al. (2025), cases reaching ≥%d foci on a <b>single cassette</b>
                may have worse prognosis than those reaching ≥%d only when <b>summing across cassettes</b>.</p>
                <p>This analysis classifies cases by how they meet the threshold for substantial involvement.</p>
                <p><b>Reference:</b> Ates D, et al. Lymphovascular Space Invasion in Endometrial Cancer.
                <i>Mod Pathol.</i> 2025;38:100885.</p>", threshold, threshold)
                distributionPatternText$setContent(html)

                # Classify cases
                # Focal: total < threshold
                # Substantial-single: max on single cassette >= threshold
                # Substantial-summed: total >= threshold BUT max < threshold

                focal <- positiveCassettesData < threshold
                substantialSingle <- maxPositiveSingleData >= threshold
                substantialSummed <- (positiveCassettesData >= threshold) & (maxPositiveSingleData < threshold)

                nFocal <- sum(focal, na.rm = TRUE)
                nSubstantialSingle <- sum(substantialSingle, na.rm = TRUE)
                nSubstantialSummed <- sum(substantialSummed, na.rm = TRUE)

                # Distribution Pattern Table
                distributionPatternTable <- self$results$distributionPatternTable

                distributionPatternTable$addRow(rowKey="predominant_single", values=list(
                    pattern = sprintf("Focal (<%d total)", threshold),
                    count = nFocal,
                    percent = nFocal / nCases
                ))
                distributionPatternTable$addRow(rowKey="summed_effect", values=list(
                    pattern = sprintf("Substantial on single cassette (≥%d on ≥1 cassette)", threshold),
                    count = nSubstantialSingle,
                    percent = nSubstantialSingle / nCases
                ))
                distributionPatternTable$addRow(rowKey="diffuse", values=list(
                    pattern = sprintf("Substantial only when summed (≥%d total, <%d max)", threshold, threshold),
                    count = nSubstantialSummed,
                    percent = nSubstantialSummed / nCases
                ))

                # Comparison statistics
                distributionComparisonTable <- self$results$distributionComparisonTable

                # Among substantial cases (total >= threshold)
                substantialCases <- positiveCassettesData >= threshold
                nSubstantial <- sum(substantialCases, na.rm = TRUE)

                if (nSubstantial > 0) {
                    pctSingleAmongSubstantial <- sum(substantialSingle, na.rm = TRUE) / nSubstantial * 100
                    pctSummedAmongSubstantial <- sum(substantialSummed, na.rm = TRUE) / nSubstantial * 100

                    distributionComparisonTable$addRow(rowKey="mean_single", values=list(
                        measure = sprintf("Cases with ≥%d foci (substantial)", threshold),
                        value = sprintf("%d (%.1f%%)", nSubstantial, nSubstantial/nCases*100)
                    ))
                    distributionComparisonTable$addRow(rowKey="max_single", values=list(
                        measure = sprintf("  - Met on single cassette"),
                        value = sprintf("%d (%.1f%% of substantial)", nSubstantialSingle, pctSingleAmongSubstantial)
                    ))
                    distributionComparisonTable$addRow(rowKey="mean_summed", values=list(
                        measure = sprintf("  - Met only by summing"),
                        value = sprintf("%d (%.1f%% of substantial)", nSubstantialSummed, pctSummedAmongSubstantial)
                    ))

                    # Calculate mean max on single cassette for each group
                    meanMaxSingle <- mean(maxPositiveSingleData[substantialSingle], na.rm = TRUE)
                    meanMaxSummed <- mean(maxPositiveSingleData[substantialSummed], na.rm = TRUE)

                    distributionComparisonTable$addRow(rowKey="predominance_ratio", values=list(
                        measure = "Mean max foci per cassette (single group)",
                        value = sprintf("%.1f", meanMaxSingle)
                    ))
                    distributionComparisonTable$addRow(rowKey="detection_yield", values=list(
                        measure = "Mean max foci per cassette (summed group)",
                        value = sprintf("%.1f", meanMaxSummed)
                    ))

                    # Clinical interpretation
                    distributionComparisonTable$addRow(rowKey="clinical_relevance", values=list(
                        measure = "Clinical significance",
                        value = "Cases with ≥5 on single cassette had worse survival (Ates 2025, p=.023)"
                    ))
                } else {
                    distributionComparisonTable$addRow(rowKey="mean_single", values=list(
                        measure = "No substantial cases",
                        value = sprintf("No cases with ≥%d foci", threshold)
                    ))
                }

                # Store for potential future use
                private$.maxPositiveSingleData <- maxPositiveSingleData
            }

            # ===== PHASE 2: Empirical Cumulative Detection =====
            if (self$options$showEmpiricalCumulative && nDetected > 0) {
                private$.checkpoint()

                empiricalCumulativeText <- self$results$empiricalCumulativeText
                empiricalCumulativeTable <- self$results$empiricalCumulativeTable

                # Calculate empirical cumulative detection with bootstrap CIs
                boot_results <- private$.bootstrapEmpiricalCumulative(
                    firstDetectionData, totalSamplesData, maxSamp, nBoot
                )

                if (!is.null(boot_results)) {
                    # Context-specific explanatory text
                    analysisContext <- self$options$analysisContext

                    contextNote <- ""
                    if (analysisContext == "tumor") {
                        contextNote <- sprintf("<div style='background: #fff3cd; border: 1px solid #ffc107; padding: 12px; margin: 10px 0; border-radius: 4px;'>
                            <p style='%s margin: 0;'><b>⚠️ Note for Tumor Sampling:</b> Sequential tumor samples (blocks) are not independent -
                            they represent serial sections through the same lesion. Spatial clustering of features like venous invasion (VI) or
                            perineural invasion (PNI) is expected. The <b>empirical method is recommended</b> over parametric models (binomial/geometric)
                            which assume independence. This non-parametric approach accurately reflects real-world detection patterns without
                            distributional assumptions.</p>
                            <p style='%s margin: 5px 0 0 0;'><em>Reference: Duan et al. 2023 - Histopathology (tissue sampling impact on VI detection)</em></p>
                        </div>",
                        private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary),
                        private$.buildStyle(private$.styleConstants$fontSize12, private$.styleConstants$colorSecondary))
                    } else if (analysisContext == "margin") {
                        contextNote <- sprintf("<div style='background: #d1ecf1; border: 1px solid #0c5460; padding: 12px; margin: 10px 0; border-radius: 4px;'>
                            <p style='%s margin: 0;'><b>ℹ️ Note for Margin Sampling:</b> Margin samples may show spatial clustering -
                            positive margins are often geographically close. Empirical approach recommended for accurate sensitivity estimates.</p>
                        </div>",
                        private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorPrimary))
                    }

                    # Populate text
                    html <- sprintf("<div style='%s'>
                        <h4 style='%s'>Empirical Cumulative Detection Analysis</h4>
                        <p style='%s'>Non-parametric estimation of detection probability based on actual observed data.
                        Does not assume geometric distribution - uses bootstrap resampling for confidence intervals.</p>
                        <p style='%s'><b>Based on:</b> %d positive cases with first detection positions ranging from %.0f to %.0f.</p>
                        %s
                    </div>",
                    private$.buildStyle(private$.styleConstants$font),
                    private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                    nDetected,
                    min(firstDetectionData, na.rm=TRUE),
                    max(firstDetectionData, na.rm=TRUE),
                    contextNote)

                    empiricalCumulativeText$setContent(html)

                    # Populate table
                    prev_cum <- 0
                    for (n in 1:nrow(boot_results)) {
                        incremental <- boot_results$mean[n] - prev_cum
                        empiricalCumulativeTable$addRow(rowKey=paste0("n_", n), values=list(
                            nSamples = n,
                            cumDetection = boot_results$mean[n],
                            ciLower = boot_results$lower[n],
                            ciUpper = boot_results$upper[n],
                            incrementalYield = incremental
                        ))
                        prev_cum <- boot_results$mean[n]
                    }
                }
            }

            # ===== PHASE 2: Incremental Yield Analysis =====
            if (self$options$showIncrementalYield && nDetected > 0) {
                private$.checkpoint()

                incrementalYieldText <- self$results$incrementalYieldText
                incrementalYieldTable <- self$results$incrementalYieldTable

                # Calculate incremental yield
                positive_idx <- !is.na(firstDetectionData)
                positive_first <- firstDetectionData[positive_idx]

                incremental_data <- data.frame()
                for (n in 1:(maxSamp-1)) {
                    from_n <- sum(positive_first <= n, na.rm=TRUE) / length(positive_first)
                    to_n <- sum(positive_first <= (n+1), na.rm=TRUE) / length(positive_first)
                    incremental <- to_n - from_n
                    cases_per_100 <- incremental * 100

                    # Cost-benefit rating
                    if (incremental >= 0.10) {
                        rating <- "High value"
                    } else if (incremental >= 0.05) {
                        rating <- "Moderate value"
                    } else if (incremental >= 0.02) {
                        rating <- "Diminishing returns"
                    } else {
                        rating <- "Low yield"
                    }

                    incrementalYieldTable$addRow(rowKey=paste0("n_", n), values=list(
                        fromSamples = n,
                        toSamples = n+1,
                        incrementalDetection = incremental,
                        casesDetected = cases_per_100,
                        costBenefit = rating
                    ))
                }

                html <- sprintf("<div style='%s'>
                    <h4 style='%s'>Incremental Diagnostic Yield Analysis</h4>
                    <p style='%s'>Marginal benefit of examining each additional sample. Helps identify the optimal stopping point where yield diminishes.</p>
                    <p style='%s'><b>Interpretation:</b> High value (≥10%%), Moderate (5-10%%), Diminishing (<5%%), Low (<2%%).</p>
                </div>",
                private$.buildStyle(private$.styleConstants$font),
                private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary))

                incrementalYieldText$setContent(html)
            }

            # ===== PHASE 3: Sample Type Stratification =====
            if (self$options$showStratifiedAnalysis && !is.null(sampleTypeData) && nDetected > 0) {
                private$.checkpoint()

                stratifiedText <- self$results$stratifiedText
                prevalenceTable <- self$results$prevalenceTable
                stratifiedDetectionTable <- self$results$stratifiedDetectionTable

                # Get unique sample types
                unique_types <- unique(sampleTypeData[!is.na(sampleTypeData)])

                if (length(unique_types) > 0) {
                    html <- sprintf("<div style='%s'>
                        <h4 style='%s'>Stratified Analysis by Sample Type</h4>
                        <p style='%s'>Separate analysis for each sample type reveals differences in:</p>
                        <ul style='%s'>
                            <li><b>Prevalence:</b> Proportion of cases with positive findings</li>
                            <li><b>Per-sample probability (q):</b> Detection probability given positivity</li>
                            <li><b>Conditional detection:</b> P(detect | present) - diagnostic sensitivity</li>
                            <li><b>Population detection:</b> P(detect overall) = prevalence × sensitivity</li>
                        </ul>
                    </div>",
                    private$.buildStyle(private$.styleConstants$font),
                    private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary))

                    stratifiedText$setContent(html)

                    # Calculate for each type
                    type_row <- 1
                    for (type in unique_types) {
                        type_idx <- sampleTypeData == type & !is.na(sampleTypeData)
                        type_positive_idx <- type_idx & !is.na(firstDetectionData)

                        n_type_total <- sum(type_idx, na.rm=TRUE)
                        n_type_positive <- sum(type_positive_idx, na.rm=TRUE)
                        prevalence_type <- n_type_positive / n_type_total

                        if (n_type_positive > 0) {
                            # Calculate q for this type
                            if (!is.null(positiveCountData)) {
                                q_type <- sum(positiveCountData[type_positive_idx], na.rm=TRUE) /
                                         sum(totalSamplesData[type_positive_idx], na.rm=TRUE)
                            } else {
                                q_type <- 1 / mean(firstDetectionData[type_positive_idx], na.rm=TRUE)
                            }

                            # Prevalence table
                            prevalenceTable$addRow(rowKey=paste0("type_", type_row), values=list(
                                sampleType = as.character(type),
                                totalCases = n_type_total,
                                positiveCases = n_type_positive,
                                prevalence = prevalence_type,
                                qEstimate = q_type
                            ))

                            # Detection probability table (for key thresholds)
                            for (n in c(3, 5, 7, 10)) {
                                if (n <= maxSamp) {
                                    conditional_det <- 1 - (1 - q_type)^n
                                    population_det <- prevalence_type * conditional_det

                                    stratifiedDetectionTable$addRow(rowKey=paste0(type, "_", n), values=list(
                                        sampleType = as.character(type),
                                        nSamples = n,
                                        conditionalDetection = conditional_det,
                                        populationDetection = population_det
                                    ))
                                }
                            }

                            type_row <- type_row + 1
                        }
                    }
                }
            }

            # ===== PHASE 3: Population-Level Detection Rates =====
            if (self$options$showPopulationDetection && nDetected > 0) {
                private$.checkpoint()

                populationDetectionText <- self$results$populationDetectionText
                populationDetectionTable <- self$results$populationDetectionTable

                # Calculate overall prevalence
                prevalence <- nDetected / length(firstDetectionData)

                html <- sprintf("<div style='%s'>
                    <h4 style='%s'>Population-Level vs Conditional Detection</h4>
                    <p style='%s'>Distinguishes between:</p>
                    <ul style='%s'>
                        <li><b>Conditional (Sensitivity):</b> P(detect | lesion present) - assumes disease exists</li>
                        <li><b>Population (Overall):</b> P(detect in general) = prevalence × sensitivity</li>
                    </ul>
                    <p style='%s'><b>Observed prevalence:</b> %.1f%% (%d/%d cases positive)</p>
                    <p style='%s'><em>Note:</em> Prevalence reflects this specific dataset and may not generalize.</p>
                </div>",
                private$.buildStyle(private$.styleConstants$font),
                private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                100*prevalence, nDetected, length(firstDetectionData),
                private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                populationDetectionText$setContent(html)

                # Populate table
                for (n in 1:maxSamp) {
                    if (!is.na(pForCalc) && pForCalc > 0) {
                        conditional <- 1 - (1 - pForCalc)^n
                        population <- prevalence * conditional

                        populationDetectionTable$addRow(rowKey=paste0("n_", n), values=list(
                            nSamples = n,
                            prevalence = prevalence,
                            conditional = conditional,
                            population = population
                        ))
                    }
                }
            }

            # ===== PHASE 4: Spatial Clustering Analysis =====
            if (self$options$showSpatialClustering && !is.null(positiveSamplesListData) && nDetected > 0) {
                private$.checkpoint()

                spatialClusteringText <- self$results$spatialClusteringText
                clusteringTable <- self$results$clusteringTable

                # Parse sample lists and calculate clustering
                clustering_indices <- numeric(length(positiveSamplesListData))
                clustering_indices[] <- NA

                positive_idx <- !is.na(firstDetectionData)
                for (i in which(positive_idx)) {
                    samples <- private$.parseSampleList(positiveSamplesListData[i])
                    if (length(samples) > 1) {
                        clustering_indices[i] <- private$.calculateClusteringIndex(
                            samples, totalSamplesData[i]
                        )
                    }
                }

                # Categorize patterns
                clustered_idx <- clustering_indices < 0.7 & !is.na(clustering_indices)
                random_idx <- clustering_indices >= 0.7 & clustering_indices <= 1.3 & !is.na(clustering_indices)
                dispersed_idx <- clustering_indices > 1.3 & !is.na(clustering_indices)

                n_clustered <- sum(clustered_idx, na.rm=TRUE)
                n_random <- sum(random_idx, na.rm=TRUE)
                n_dispersed <- sum(dispersed_idx, na.rm=TRUE)
                n_total <- sum(!is.na(clustering_indices))

                if (n_total > 0) {
                    html <- sprintf("<div style='%s'>
                        <h4 style='%s'>Spatial Clustering Analysis</h4>
                        <p style='%s'>Analyzes how positive samples are distributed spatially:</p>
                        <ul style='%s'>
                            <li><b>Clustered (index < 0.7):</b> Positive samples grouped together (focal/contiguous)</li>
                            <li><b>Random (0.7-1.3):</b> Positive samples evenly distributed</li>
                            <li><b>Dispersed (> 1.3):</b> Positive samples widely separated (multifocal)</li>
                        </ul>
                        <p style='%s'><b>Clinical significance:</b> Clustered patterns may allow more targeted sampling;
                        dispersed patterns require broader sampling strategy.</p>
                    </div>",
                    private$.buildStyle(private$.styleConstants$font),
                    private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorSecondary),
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                    spatialClusteringText$setContent(html)

                    # Populate table
                    clusteringTable$addRow(rowKey="clustered", values=list(
                        pattern = "Clustered (focal)",
                        count = n_clustered,
                        percent = n_clustered / n_total,
                        meanClusterIndex = if (n_clustered > 0) mean(clustering_indices[clustered_idx], na.rm=TRUE) else NA
                    ))
                    clusteringTable$addRow(rowKey="random", values=list(
                        pattern = "Random",
                        count = n_random,
                        percent = n_random / n_total,
                        meanClusterIndex = if (n_random > 0) mean(clustering_indices[random_idx], na.rm=TRUE) else NA
                    ))
                    clusteringTable$addRow(rowKey="dispersed", values=list(
                        pattern = "Dispersed (multifocal)",
                        count = n_dispersed,
                        percent = n_dispersed / n_total,
                        meanClusterIndex = if (n_dispersed > 0) mean(clustering_indices[dispersed_idx], na.rm=TRUE) else NA
                    ))
                }
            }

            # ===== PHASE 4: Multifocal Detection Analysis =====
            if (self$options$showMultifocalAnalysis && !is.null(positiveSamplesListData) && nDetected > 0) {
                private$.checkpoint()

                multifocalText <- self$results$multifocalText
                multifocalTable <- self$results$multifocalTable

                # Parse sample lists and estimate foci
                foci_counts <- integer(length(positiveSamplesListData))
                foci_counts[] <- NA

                positive_idx <- !is.na(firstDetectionData)
                for (i in which(positive_idx)) {
                    samples <- private$.parseSampleList(positiveSamplesListData[i])
                    if (length(samples) > 0) {
                        foci_counts[i] <- private$.estimateFociCount(samples)
                    }
                }

                # Categorize by foci count
                unifocal_idx <- foci_counts == 1 & !is.na(foci_counts)
                bifocal_idx <- foci_counts == 2 & !is.na(foci_counts)
                multifocal_idx <- foci_counts >= 3 & !is.na(foci_counts)

                n_unifocal <- sum(unifocal_idx, na.rm=TRUE)
                n_bifocal <- sum(bifocal_idx, na.rm=TRUE)
                n_multifocal <- sum(multifocal_idx, na.rm=TRUE)
                n_total <- sum(!is.na(foci_counts))

                if (n_total > 0) {
                    html <- sprintf("<div style='%s'>
                        <h4 style='%s'>Multifocal Detection Analysis</h4>
                        <p style='%s'>Estimates number of separate foci based on spatial distribution of positive samples.
                        Gaps > 2 samples suggest separate foci.</p>
                        <p style='%s'><b>Clinical note:</b> Multifocal involvement may indicate more advanced disease
                        and can affect staging/treatment decisions.</p>
                    </div>",
                    private$.buildStyle(private$.styleConstants$font),
                    private$.buildStyle(private$.styleConstants$fontSize15, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary),
                    private$.buildStyle(private$.styleConstants$fontSize13, private$.styleConstants$colorSecondary))

                    multifocalText$setContent(html)

                    # Populate table
                    if (n_unifocal > 0) {
                        multifocalTable$addRow(rowKey="single", values=list(
                            fociCount = "Unifocal (1 focus)",
                            cases = n_unifocal,
                            percent = n_unifocal / n_total,
                            meanFirstDetection = mean(firstDetectionData[unifocal_idx], na.rm=TRUE)
                        ))
                    }
                    if (n_bifocal > 0) {
                        multifocalTable$addRow(rowKey="low_multi", values=list(
                            fociCount = "Bifocal (2 foci)",
                            cases = n_bifocal,
                            percent = n_bifocal / n_total,
                            meanFirstDetection = mean(firstDetectionData[bifocal_idx], na.rm=TRUE)
                        ))
                    }
                    if (n_multifocal > 0) {
                        multifocalTable$addRow(rowKey="high_multi", values=list(
                            fociCount = "Multifocal (3+ foci)",
                            cases = n_multifocal,
                            percent = n_multifocal / n_total,
                            meanFirstDetection = mean(firstDetectionData[multifocal_idx], na.rm=TRUE)
                        ))
                    }
                }
            }

            # === Hypergeometric Model Analysis ===
            if (self$options$showHypergeometric && !is.null(self$options$totalPopulation) && !is.null(self$options$successStates)) {

                # Get hypergeometric parameters
                totalPopulationEsc <- private$.escapeVar(self$options$totalPopulation)
                successStatesEsc <- private$.escapeVar(self$options$successStates)

                totalPopulationData <- jmvcore::toNumeric(data[[totalPopulationEsc]])
                successStatesData <- jmvcore::toNumeric(data[[successStatesEsc]])

                # Handle labelled data
                if (is.factor(totalPopulationData) || !is.null(attr(totalPopulationData, 'labels'))) {
                    totalPopulationData <- as.numeric(as.character(totalPopulationData))
                }
                if (is.factor(successStatesData) || !is.null(attr(successStatesData, 'labels'))) {
                    successStatesData <- as.numeric(as.character(successStatesData))
                }

                # Filter to valid cases
                totalPopulationData <- totalPopulationData[validCases]
                successStatesData <- successStatesData[validCases]
                if (!is.null(invalidCases)) {
                    totalPopulationData <- totalPopulationData[!invalidCases]
                    successStatesData <- successStatesData[!invalidCases]
                }

                target <- self$options$targetDetections        # Minimum detections desired
                if (is.null(target) || is.na(target) || target < 1) {
                    target <- 1
                }

                # Identify cases with complete hypergeometric inputs
                hyperValid <- !is.na(totalPopulationData) & !is.na(successStatesData)
                if (sum(hyperValid) == 0) {
                    hypergeometricText <- self$results$hypergeometricText
                    hypergeometricTable <- self$results$hypergeometricTable
                    hyperRecommendTable <- self$results$hyperRecommendTable
                    hypergeometricText$setContent("<p>No valid cases with total population and success counts were found for the hypergeometric model.</p>")
                    hypergeometricTable$clearRows()
                    hyperRecommendTable$clearRows()
                    return()
                }

                N_values <- totalPopulationData[hyperValid]
                K_values <- successStatesData[hyperValid]

                hyperNotes <- character()

                # Enforce integer counts (hypergeometric requires discrete totals)
                if (any(abs(N_values - round(N_values)) > 1e-6, na.rm = TRUE)) {
                    hyperNotes <- c(hyperNotes, "Total population counts rounded to nearest integer")
                }
                if (any(abs(K_values - round(K_values)) > 1e-6, na.rm = TRUE)) {
                    hyperNotes <- c(hyperNotes, "Positive counts rounded to nearest integer")
                }

                N_int <- round(N_values)
                K_int <- round(K_values)

                # Remove impossible cases
                invalidHyper <- (N_int <= 0) | (K_int < 0) | (K_int > N_int)
                if (any(invalidHyper, na.rm = TRUE)) {
                    removed <- sum(invalidHyper, na.rm = TRUE)
                    hyperNotes <- c(hyperNotes, sprintf("%d cases removed (invalid population/success counts)", removed))
                    N_int <- N_int[!invalidHyper]
                    K_int <- K_int[!invalidHyper]
                }

                if (length(N_int) == 0) {
                    hypergeometricText <- self$results$hypergeometricText
                    hypergeometricTable <- self$results$hypergeometricTable
                    hyperRecommendTable <- self$results$hyperRecommendTable
                    hypergeometricText$setContent("<p>All cases were removed because population/success counts were invalid for the hypergeometric model.</p>")
                    hypergeometricTable$clearRows()
                    hyperRecommendTable$clearRows()
                    return()
                }

                nHyperCases <- length(N_int)
                medianN <- stats::median(N_int)
                medianK <- stats::median(K_int)

                hypergeometricText <- self$results$hypergeometricText
                notesHtml <- if (length(hyperNotes) > 0) {
                    sprintf("<p><b>Data notes:</b> %s.</p>", paste(hyperNotes, collapse = "; "))
                } else ""

                html <- sprintf("<h4>Hypergeometric Probability Model</h4>
                <p>Finite-population sampling without replacement (e.g., lymph node dissections) evaluated on %d cases.</p>
                <p><b>Typical case:</b> median total nodes = %.0f, median positive nodes = %.0f.</p>
                %s
                <p><b>Model:</b> For each case i with population N<sub>i</sub> and positives K<sub>i</sub>, the probability of detecting ≥%d positives after n draws is averaged across cases:</p>
                <p style='margin-left: 15px;'>P(detect ≥ %d) = mean<sub>i</sub>[1 - Σ<sub>x=0</sub><sup>%d-1</sup> dhyper(x, K<sub>i</sub>, N<sub>i</sub>-K<sub>i</sub>, min(n, N<sub>i</sub>))]</p>
                <p><b>Reference:</b> Orange-peeling LN dissection study (2025) - Hypergeometric adequacy thresholds.</p>",
                nHyperCases, medianN, medianK, notesHtml, target, target, max(target - 1, 0))
                hypergeometricText$setContent(html)

                # Pre-compute cumulative probabilities across cases
                hypergeometricTable <- self$results$hypergeometricTable
                aggregatedProb <- rep(NA_real_, maxSamp)

                prevProb <- NA_real_
                for (n in 1:maxSamp) {
                    draws <- pmin(n, N_int)

                    caseProb <- vapply(seq_along(N_int), function(idx) {
                        Ni <- N_int[idx]
                        Ki <- K_int[idx]
                        draw <- draws[idx]

                        if (Ki <= 0 || target > draw) {
                            return(0)
                        }

                        failures <- Ni - Ki
                        if (target > 1) {
                            probLess <- stats::phyper(target - 1, Ki, failures, draw)
                        } else {
                            probLess <- stats::dhyper(0, Ki, failures, draw)
                        }

                        1 - probLess
                    }, numeric(1))

                    if (all(is.na(caseProb))) {
                        cumProb <- NA_real_
                    } else {
                        cumProb <- mean(caseProb, na.rm = TRUE)
                    }

                    aggregatedProb[n] <- cumProb

                    marginal <- if (!is.na(cumProb) && !is.na(prevProb)) cumProb - prevProb else if (!is.na(cumProb) && is.na(prevProb)) cumProb else NA

                    hypergeometricTable$addRow(rowKey=paste0("n_", n), values=list(
                        nSamples = n,
                        cumProb = cumProb,
                        marginalGain = marginal
                    ))

                    if (!is.na(cumProb)) prevProb <- cumProb
                }

                hyperDetail <- if (length(hyperNotes) > 0) paste(hyperNotes, collapse = "; ") else ""

                addRecommendation(
                    method = "Hypergeometric",
                    probVec = aggregatedProb,
                    priority = 3,
                    description = "Finite population model (sampling without replacement)",
                    detail = hyperDetail
                )

                # Minimum samples for target confidence levels
                hyperRecommendTable <- self$results$hyperRecommendTable

                confLevels <- c(0.80, 0.90, 0.95, 0.99)
                for (i in seq_along(confLevels)) {
                    conf <- confLevels[i]

                    minSamples <- NA
                    expectedYield <- NA

                    idx <- which(!is.na(aggregatedProb) & aggregatedProb >= conf)[1]
                    if (!is.na(idx)) {
                        minSamples <- idx

                        # Expected detections based on case-level probabilities
                        caseExpected <- vapply(seq_along(N_int), function(idxCase) {
                            Ni <- N_int[idxCase]
                            Ki <- K_int[idxCase]
                            draw <- min(minSamples, Ni)

                            if (Ki <= 0 || draw <= 0) {
                                return(0)
                            }

                            # Expected detections under hypergeometric sampling with replacement adjustment
                            expected <- draw * Ki / Ni

                            # To respect user target, cap expected at target if target specified
                            if (!is.null(target) && !is.na(target)) {
                                expected <- min(expected, target)
                            }

                            expected
                        }, numeric(1))

                        expectedYield <- mean(caseExpected, na.rm = TRUE)
                    }

                    hyperRecommendTable$addRow(rowKey=paste0("conf_", i), values=list(
                        confidence = conf,
                        minSamples = minSamples,
                        expectedYield = expectedYield
                    ))
                }
            }

            # === Beta-Binomial Model Analysis ===
            if (self$options$showBetaBinomial && !is.null(self$options$totalPopulation) && !is.null(self$options$successStates)) {

                # Get data
                totalPopulationEsc <- private$.escapeVar(self$options$totalPopulation)
                successStatesEsc <- private$.escapeVar(self$options$successStates)

                totalPopulationData <- jmvcore::toNumeric(data[[totalPopulationEsc]])
                successStatesData <- jmvcore::toNumeric(data[[successStatesEsc]])

                # Filter to valid cases
                totalPopulationData <- totalPopulationData[validCases]
                successStatesData <- successStatesData[validCases]
                if (!is.null(invalidCases)) {
                    totalPopulationData <- totalPopulationData[!invalidCases]
                    successStatesData <- successStatesData[!invalidCases]
                }

                # Estimate alpha and beta with safeguards against invalid variance patterns
                p <- successStatesData / totalPopulationData
                p <- p[is.finite(p)]

                mu <- mean(p, na.rm = TRUE)
                var <- var(p, na.rm = TRUE)

                alpha <- NA_real_
                beta <- NA_real_
                betaBinomNotes <- character()

                if (length(p) < 2 || is.na(mu) || is.na(var)) {
                    betaBinomNotes <- c(betaBinomNotes, "Insufficient variability; defaulting to Beta(1,1)")
                    alpha <- 1
                    beta <- 1
                } else {
                    theoreticalMaxVar <- mu * (1 - mu)

                    if (var <= 0 || var >= theoreticalMaxVar) {
                        # Overdispersion beyond beta-binomial support or zero variance
                        epsilon <- 1e-6
                        if (var <= 0) {
                            betaBinomNotes <- c(betaBinomNotes, "Zero variance in detection rates; defaulting to Beta(1,1)")
                            alpha <- 1
                            beta <- 1
                        } else {
                            betaBinomNotes <- c(betaBinomNotes,
                                "Observed variance exceeds binomial limit; applying ridge adjustment")
                            varAdj <- min(var, theoreticalMaxVar - epsilon)
                            term <- (mu * (1 - mu) / varAdj) - 1
                            alpha <- max(mu * term, epsilon)
                            beta <- max((1 - mu) * term, epsilon)
                        }
                    } else {
                        term <- (mu * (1 - mu) / var) - 1
                        alpha <- mu * term
                        beta <- (1 - mu) * term
                    }
                }

                if (!is.finite(alpha) || !is.finite(beta) || alpha <= 0 || beta <= 0) {
                    betaBinomNotes <- c(betaBinomNotes, "Fallback to Beta(1,1) due to invalid parameter estimates")
                    alpha <- 1
                    beta <- 1
                }

                # Beta-Binomial Text
                betaBinomialText <- self$results$betaBinomialText
                extraText <- if (length(betaBinomNotes) > 0) {
                    sprintf("<p><b>Estimation notes:</b> %s.</p>", paste(betaBinomNotes, collapse = "; "))
                } else ""

                html <- sprintf("<h4>Beta-Binomial Probability Model</h4>
                <p>For <b>finite population sampling with overdispersion</b> (e.g., lymph node dissection where positivity varies between cases).</p>
                <p>This model is more robust than the hypergeometric model when there is case-by-case variability in the number of positive items.</p>
                <p><b>Model parameters (estimated from data):</b></p>
                <ul>
                    <li>Alpha (α): %.3f</li>
                    <li>Beta (β): %.3f</li>
                </ul>
                %s
                <p><b>Reference:</b> Zhou J, et al. Beta-binomial model for lymph node yield. <i>Front Oncol.</i> 2022;12:872527.</p>",
                alpha, beta, extraText)
                betaBinomialText$setContent(html)

                # Calculate beta-binomial probabilities
                betaBinomialTable <- self$results$betaBinomialTable

                betaCumProb <- rep(NA_real_, maxSamp)
                prevProb <- 0
                for (n in 1:maxSamp) {
                    # P(detect ≥ 1) using beta-binomial distribution
                    # We need a function for the beta-binomial PMF
                    dbetabinom_pmf <- function(k, n, alpha, beta) {
                        exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
                    }

                    # P(X < 1) = P(X=0)
                    prob_zero <- dbetabinom_pmf(0, n, alpha, beta)
                    cumProb <- 1 - prob_zero

                    marginal <- cumProb - prevProb

                    betaBinomialTable$addRow(rowKey=paste0("n_", n), values=list(
                        nSamples = n,
                        cumProb = cumProb,
                        marginalGain = marginal
                    ))

                    prevProb <- cumProb
                    betaCumProb[n] <- cumProb
                }

                # Minimum samples for target confidence levels
                betaBinomialRecommendTable <- self$results$betaBinomialRecommendTable

                addRecommendation(
                    method = "Beta-binomial",
                    probVec = betaCumProb,
                    priority = 2,
                    description = "Overdispersed finite population model",
                    detail = if (length(betaBinomNotes) > 0) paste(betaBinomNotes, collapse = "; ") else ""
                )

                maxFeasible <- floor(max(totalPopulationData, na.rm = TRUE))
                maxFeasible <- min(maxFeasible, maxSamp)

                confLevels <- c(0.80, 0.90, 0.95, 0.99)
                for (i in seq_along(confLevels)) {
                    conf <- confLevels[i]

                    minSamples <- NA
                    expectedYield <- NA

                    if (maxFeasible > 0) {
                        for (n in 1:maxFeasible) {
                            dbetabinom_pmf <- function(k, n, alpha, beta) {
                                exp(lchoose(n, k) + lbeta(k + alpha, n - k + beta) - lbeta(alpha, beta))
                            }
                            prob_zero <- dbetabinom_pmf(0, n, alpha, beta)
                            cumProb <- 1 - prob_zero

                            if (cumProb >= conf) {
                                minSamples <- n
                                expectedYield <- n * alpha / (alpha + beta)
                                if (!is.null(target) && !is.na(target)) {
                                    expectedYield <- min(expectedYield, target)
                                }
                                break
                            }
                        }
                    }

                    betaBinomialRecommendTable$addRow(rowKey=paste0("conf_", i), values=list(
                        confidence = conf,
                        minSamples = minSamples,
                        expectedYield = expectedYield
                    ))
                }
            }

            # === Lymph Node Ratio and Staging Analysis ===
            if (self$options$showLNAnalysis && !is.null(self$options$totalLymphNodes) && !is.null(self$options$positiveLymphNodes)) {

                # Get LN variables
                totalLymphNodesEsc <- private$.escapeVar(self$options$totalLymphNodes)
                positiveLymphNodesEsc <- private$.escapeVar(self$options$positiveLymphNodes)

                totalELN <- jmvcore::toNumeric(data[[totalLymphNodesEsc]])
                positiveLN <- jmvcore::toNumeric(data[[positiveLymphNodesEsc]])

                # Handle labelled data
                if (is.factor(totalELN) || !is.null(attr(totalELN, 'labels'))) {
                    totalELN <- as.numeric(as.character(totalELN))
                }
                if (is.factor(positiveLN) || !is.null(attr(positiveLN, 'labels'))) {
                    positiveLN <- as.numeric(as.character(positiveLN))
                }

                # Filter to valid cases
                totalELN <- totalELN[validCases]
                positiveLN <- positiveLN[validCases]
                if (!is.null(invalidCases)) {
                    totalELN <- totalELN[!invalidCases]
                    positiveLN <- positiveLN[!invalidCases]
                }

                # LN Analysis Text
                lnAnalysisText <- self$results$lnAnalysisText
                html <- "<h4>Lymph Node Ratio and Staging Analysis</h4>
                <p>Based on validated lymph node adequacy studies in pancreatic adenocarcinoma:</p>
                <ul>
                    <li><b>Tomlinson et al. 2007 (UCLA/SEER, n=1,150 pN0):</b> minELN=15
                        <br>• 8-month survival advantage (27 vs 19 mo, P<0.001), HR=0.63
                        <br>• 90% of pN1a disease detected with ≤15 nodes</li>
                    <li><b>Pu et al. 2021 (Johns Hopkins, n=1,837):</b> minELN=12
                        <br>• Binomial: P=1-(1-p)^n ≥0.95 → n=11.6 → 12
                        <br>• LNR classification (X-tile: 0.1, 0.3), LNR superior when ELN 12-28</li>
                    <li><b>Yoon et al. 2025 (Korean dual-cohort, n=1,252):</b> minELN=12-16
                        <br>• False N0 modeling: 16 LN → 18.9% false N0 (exploration)
                        <br>• Independent validation: 12 LN → 19.5% false N0 (validation)
                        <br>• Survival benefit up to 21 LNs in N0 patients</li>
                </ul>
                <p><b>False N0:</b> Probability of node-positive patient misclassified as N0 due to inadequate sampling</p>
                <p><b>LNR:</b> Metastatic lymph nodes / Total examined lymph nodes</p>
                <p><b>Consensus:</b> 12-16 lymph nodes optimal (12=minimum, 16=quality target)</p>
                <p><b>References:</b>
                <br>• Tomlinson JS, et al. <i>Arch Surg.</i> 2007;142(8):767-774.
                <br>• Pu N, et al. <i>J Natl Compr Canc Netw.</i> 2021;19(9):1029-1036.
                <br>• Yoon SJ, et al. <i>Ann Surg Oncol.</i> 2025. doi:10.1245/s10434-025-18029-7</p>"
                lnAnalysisText$setContent(html)

                # Calculate LNR
                LNR <- positiveLN / totalELN

                # Get user-specified thresholds
                threshold1 <- self$options$lnrThreshold1
                threshold2 <- self$options$lnrThreshold2

                # LNR Classification (Pu2021 thresholds: 0.1 and 0.3)
                lnrStage <- character(length(LNR))
                lnrRange <- character(length(LNR))
                lnrStage[LNR == 0] <- "LNR0"
                lnrRange[LNR == 0] <- "0.000"
                lnrStage[LNR > 0 & LNR <= threshold1] <- "LNR1"
                lnrRange[LNR > 0 & LNR <= threshold1] <- sprintf("0.001-%.3f", threshold1)
                lnrStage[LNR > threshold1 & LNR <= threshold2] <- "LNR2"
                lnrRange[LNR > threshold1 & LNR <= threshold2] <- sprintf("%.3f-%.3f", threshold1, threshold2)
                lnrStage[LNR > threshold2] <- "LNR3"
                lnrRange[LNR > threshold2] <- sprintf("%.3f-1.000", threshold2)

                # Populate LNR Classification Table
                lnrClassification <- self$results$lnrClassification

                lnrGroups <- c("LNR0 (node-negative)", "LNR1 (low burden)", "LNR2 (moderate burden)", "LNR3 (high burden)")
                lnrRanges <- c("0.000", sprintf("0.001-%.3f", threshold1),
                              sprintf("%.3f-%.3f", threshold1, threshold2),
                              sprintf("%.3f-1.000", threshold2))

                for (i in 1:4) {
                    cases_in_group <- sum(lnrStage == c("LNR0", "LNR1", "LNR2", "LNR3")[i], na.rm = TRUE)
                    median_eln <- median(totalELN[lnrStage == c("LNR0", "LNR1", "LNR2", "LNR3")[i]], na.rm = TRUE)

                    lnrClassification$addRow(rowKey=paste0("lnr_", i), values=list(
                        lnrStage = lnrGroups[i],
                        lnrRange = lnrRanges[i],
                        cases = cases_in_group,
                        percent = cases_in_group / nCases,
                        medianELN = if (!is.na(median_eln)) median_eln else NA
                    ))
                }

                # AJCC N Stage Classification
                nStage <- character(length(positiveLN))
                nStage[positiveLN == 0] <- "N0"
                nStage[positiveLN >= 1 & positiveLN <= 3] <- "N1"
                nStage[positiveLN >= 4] <- "N2"

                # Populate AJCC N Stage Table
                ajccNStage <- self$results$ajccNStage

                nStageGroups <- c("N0", "N1", "N2")
                nStageCriteria <- c("0 positive LN", "1-3 positive LN", "≥4 positive LN")

                for (i in 1:3) {
                    cases_in_stage <- sum(nStage == nStageGroups[i], na.rm = TRUE)
                    median_eln_stage <- median(totalELN[nStage == nStageGroups[i]], na.rm = TRUE)

                    ajccNStage$addRow(rowKey=paste0("stage_", i), values=list(
                        nStage = nStageGroups[i],
                        criteria = nStageCriteria[i],
                        cases = cases_in_stage,
                        percent = cases_in_stage / nCases,
                        medianELN = if (!is.na(median_eln_stage)) median_eln_stage else NA
                    ))
                }

                # Adequacy Assessment by ELN Thresholds (Tomlinson2007 + Pu2021)
                adequacyByELN <- self$results$adequacyByELN

                # Define ELN groups (Tomlinson 2007, Pu 2021, Yoon 2025)
                eln_group <- character(length(totalELN))
                eln_group[totalELN < 9] <- "<9 ELN"
                eln_group[totalELN >= 9 & totalELN < 12] <- "9-11 ELN"
                eln_group[totalELN >= 12 & totalELN < 16] <- "12-15 ELN"
                eln_group[totalELN >= 16 & totalELN < 22] <- "16-21 ELN"
                eln_group[totalELN >= 22] <- "≥22 ELN"

                elnGroups <- c("<9 ELN", "9-11 ELN", "12-15 ELN", "16-21 ELN", "≥22 ELN")
                comments <- c(
                    "Inadequate (false N0 >30%)",
                    "Marginal (false N0 20-30%)",
                    "Adequate - Minimum (Pu2021/Yoon2025: 12 LN, false N0 ~20%)",
                    "Adequate - Quality (Tomlinson2007/Yoon2025: 16 LN, false N0 13-19%)",
                    "Excellent (false N0 <13%, diminishing returns)"
                )

                for (i in 1:5) {
                    cases_in_eln <- sum(eln_group == elnGroups[i], na.rm = TRUE)
                    npositive_in_eln <- sum(positiveLN[eln_group == elnGroups[i]] > 0, na.rm = TRUE)

                    adequacyByELN$addRow(rowKey=paste0("eln_", i), values=list(
                        elnGroup = elnGroups[i],
                        cases = cases_in_eln,
                        percent = cases_in_eln / nCases,
                        nPositive = npositive_in_eln,
                        comment = comments[i]
                    ))
                }
            }

            # === Effect Size Measures ===
            if (self$options$showEffectSizes && !is.null(self$options$totalLymphNodes)) {

                # Get total ELN data
                totalLymphNodesEsc <- private$.escapeVar(self$options$totalLymphNodes)
                totalELN <- jmvcore::toNumeric(data[[totalLymphNodesEsc]])

                # Handle labelled data
                if (is.factor(totalELN) || !is.null(attr(totalELN, 'labels'))) {
                    totalELN <- as.numeric(as.character(totalELN))
                }

                # Filter to valid cases
                totalELN <- totalELN[validCases]
                if (!is.null(invalidCases)) {
                    totalELN <- totalELN[!invalidCases]
                }

                # Effect Size Text
                effectSizesText <- self$results$effectSizesText
                html <- "<h4>Effect Size Measures</h4>
                <p>Non-parametric effect size measures for adequacy comparisons.</p>
                <p><b>Measures:</b></p>
                <ul>
                    <li><b>Cliff's Delta:</b> Non-parametric effect size for ordinal data (range: -1 to +1)</li>
                    <li><b>Hodges-Lehmann Estimator:</b> Robust median difference between groups</li>
                    <li><b>Odds Ratio (OR):</b> Odds of achieving adequacy (≥12 LN)</li>
                    <li><b>Relative Risk (RR):</b> Relative probability of adequacy</li>
                    <li><b>Risk Difference (RD):</b> Absolute improvement in adequacy rate</li>
                </ul>
                <p><b>Interpretation Guidelines:</b></p>
                <ul>
                    <li>Cliff's Delta: |d| < 0.147 (negligible), 0.147-0.330 (small), 0.330-0.474 (medium), > 0.474 (large)</li>
                    <li>OR: 1.0 (no effect), 1.5-3.0 (small-moderate), 3.0-9.0 (moderate-large), > 9.0 (large)</li>
                </ul>"
                effectSizesText$setContent(html)

                # Define adequacy groups (e.g., <12 vs ≥12 LN)
                adequate <- totalELN >= 12
                inadequate <- totalELN < 12

                # Cliff's Delta function
                cliff_delta <- function(x, y) {
                    concordant <- sum(outer(x, y, ">"))
                    discordant <- sum(outer(x, y, "<"))
                    delta <- (concordant - discordant) / (length(x) * length(y))
                    return(delta)
                }

                # Hodges-Lehmann estimator function
                hodges_lehmann <- function(x, y) {
                    diffs <- outer(x, y, "-")
                    median(diffs)
                }

                # Calculate effect sizes
                effectSizesTable <- self$results$effectSizesTable

                if (sum(adequate) > 0 && sum(inadequate) > 0) {
                    # Cliff's Delta
                    delta <- cliff_delta(totalELN[adequate], totalELN[inadequate])
                    delta_interp <- if (abs(delta) < 0.147) "Negligible" else if (abs(delta) < 0.330) "Small" else if (abs(delta) < 0.474) "Medium" else "Large"

                    effectSizesTable$addRow(rowKey="cliff_delta", values=list(
                        measure = "Cliff's Delta",
                        value = sprintf("%.3f", delta),
                        interpretation = delta_interp
                    ))

                    # Hodges-Lehmann Estimator
                    hl_est <- hodges_lehmann(totalELN[adequate], totalELN[inadequate])
                    effectSizesTable$addRow(rowKey="hodges_lehmann", values=list(
                        measure = "Hodges-Lehmann Estimator",
                        value = sprintf("%.2f LN", hl_est),
                        interpretation = "Median ELN difference"
                    ))

                    # Odds Ratio (OR) for achieving adequacy
                    # Define outcome: achieved ≥12 LN
                    # Could compare by different grouping variables if available
                    # For now, calculate overall adequacy rate
                    adequacy_rate <- sum(adequate) / nCases
                    inadequacy_rate <- sum(inadequate) / nCases

                    if (adequacy_rate > 0 && inadequacy_rate > 0) {
                        odds_adequate <- adequacy_rate / (1 - adequacy_rate)
                        odds_inadequate <- inadequacy_rate / (1 - inadequacy_rate)
                        OR <- odds_adequate / odds_inadequate
                        OR_interp <- if (OR < 1.5) "Negligible" else if (OR < 3.0) "Small-moderate" else if (OR < 9.0) "Moderate-large" else "Large"

                        effectSizesTable$addRow(rowKey="odds_ratio", values=list(
                            measure = "Odds Ratio (OR)",
                            value = sprintf("%.2f", OR),
                            interpretation = OR_interp
                        ))

                        # Relative Risk (RR)
                        RR <- adequacy_rate / inadequacy_rate
                        effectSizesTable$addRow(rowKey="relative_risk", values=list(
                            measure = "Relative Risk (RR)",
                            value = sprintf("%.2f", RR),
                            interpretation = sprintf("%.0f%% relative improvement", (RR - 1) * 100)
                        ))

                        # Risk Difference (RD)
                        RD <- adequacy_rate - inadequacy_rate
                        effectSizesTable$addRow(rowKey="risk_diff", values=list(
                            measure = "Risk Difference (RD)",
                            value = sprintf("%.3f", RD),
                            interpretation = sprintf("%.1f%% absolute improvement", RD * 100)
                        ))
                    } else {
                        effectSizesTable$addRow(rowKey="or_rr_rd_na", values=list(
                            measure = "OR/RR/RD",
                            value = "N/A",
                            interpretation = "Insufficient variability in adequacy"
                        ))
                    }

                    # Mean and median ELN comparison
                    mean_adequate <- mean(totalELN[adequate], na.rm = TRUE)
                    mean_inadequate <- mean(totalELN[inadequate], na.rm = TRUE)
                    mean_diff <- mean_adequate - mean_inadequate

                    effectSizesTable$addRow(rowKey="mean_eln", values=list(
                        measure = "Mean ELN Difference",
                        value = sprintf("%.2f - %.2f = %.2f", mean_adequate, mean_inadequate, mean_diff),
                        interpretation = "Adequate vs Inadequate group"
                    ))

                    median_adequate <- median(totalELN[adequate], na.rm = TRUE)
                    median_inadequate <- median(totalELN[inadequate], na.rm = TRUE)
                    median_diff <- median_adequate - median_inadequate

                    effectSizesTable$addRow(rowKey="median_eln", values=list(
                        measure = "Median ELN Difference",
                        value = sprintf("%.1f - %.1f = %.1f", median_adequate, median_inadequate, median_diff),
                        interpretation = "Adequate vs Inadequate group"
                    ))

                } else {
                    effectSizesTable$addRow(rowKey="insufficient", values=list(
                        measure = "Effect Size Calculation",
                        value = "N/A",
                        interpretation = "Insufficient cases in one or both adequacy groups"
                    ))
                }
            }

            # === Clinical Recommendations ===
            recommendationTable <- NULL
            primaryRecommendation <- NULL
            fallbackRecommendation <- NULL

            if (length(recommendations) > 0) {
                recommendationTable <- do.call(rbind, lapply(recommendations, function(rec) {
                    data.frame(
                        method = rec$method,
                        description = rec$description,
                        minSamples = ifelse(is.null(rec$minSamples), NA, rec$minSamples),
                        achievedProb = rec$achievedProb,
                        bestProb = rec$bestProb,
                        bestN = rec$bestN,
                        status = rec$status,
                        detail = ifelse(is.null(rec$detail), "", rec$detail),
                        priority = rec$priority,
                        ciLower = rec$ciLower,
                        ciUpper = rec$ciUpper,
                        stringsAsFactors = FALSE
                    )
                }))

                recommendationTable$priority <- as.numeric(recommendationTable$priority)
                recommendationTable$minSamples <- as.numeric(recommendationTable$minSamples)
                recommendationTable$achievedProb <- as.numeric(recommendationTable$achievedProb)
                recommendationTable$bestProb <- as.numeric(recommendationTable$bestProb)
                recommendationTable$bestN <- as.numeric(recommendationTable$bestN)
                recommendationTable$ciLower <- as.numeric(recommendationTable$ciLower)
                recommendationTable$ciUpper <- as.numeric(recommendationTable$ciUpper)

                recommendationTable <- recommendationTable[order(recommendationTable$priority, recommendationTable$method), , drop = FALSE]

                validPrimary <- recommendationTable[!is.na(recommendationTable$minSamples), , drop = FALSE]
                if (nrow(validPrimary) > 0) {
                    ordPrimary <- order(validPrimary$priority, validPrimary$minSamples)
                    primaryRecommendation <- validPrimary[ordPrimary[1], , drop = FALSE]
                }

                fallbackCandidates <- recommendationTable[!is.na(recommendationTable$bestProb), , drop = FALSE]
                if (nrow(fallbackCandidates) > 0) {
                    ordFallback <- order(-fallbackCandidates$bestProb, fallbackCandidates$priority, fallbackCandidates$method)
                    fallbackRecommendation <- fallbackCandidates[ordFallback[1], , drop = FALSE]
                }
            }

            recommendText <- self$results$recommendText

            # Get analysis context for tailored recommendations
            analysisContext <- self$options$analysisContext

            if (!is.null(primaryRecommendation) && nrow(primaryRecommendation) == 1) {
                rec <- primaryRecommendation
                detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""

                # Context-specific recommendation text
                contextHeader <- switch(analysisContext,
                    "tumor" = "Tumor Sampling Recommendations",
                    "lymphnode" = "Lymph Node Dissection Recommendations",
                    "omentum" = "Omentum Sampling Recommendations",
                    "margin" = "Margin Sampling Recommendations",
                    "Clinical Recommendations"  # default for general
                )

                contextExample <- switch(analysisContext,
                    "tumor" = sprintf("<p style='font-size: 13px; color: #666; font-style: italic; margin: 10px 0;'>
                        Example: To achieve 95%% sensitivity for detecting venous invasion (VI), examine at least %d tumor samples.
                        This recommendation is based on empirical detection patterns from similar cases.</p>", rec$minSamples),
                    "lymphnode" = sprintf("<p style='font-size: 13px; color: #666; font-style: italic; margin: 10px 0;'>
                        Example: For adequate lymph node dissection with 95%% confidence, examine at least %d lymph nodes.</p>", rec$minSamples),
                    ""  # default: no example
                )

                html <- sprintf("<h4>%s</h4>
                <p><b>Recommended minimum samples for %.0f%% sensitivity:</b> %d (based on %s model)%s.</p>
                <p>This plan achieves an estimated sensitivity of <b>%.1f%%</b> using the %s.</p>
                %s",
                    contextHeader,
                    targetConf * 100,
                    rec$minSamples,
                    rec$method,
                    detailSuffix,
                    rec$achievedProb * 100,
                    rec$description,
                    contextExample)
                if (nzchar(obsListHtml)) {
                    html <- paste0(html,
                        "<p><b>Observed cumulative detection:</b></p><ul>",
                        obsListHtml,
                        "</ul>")
                }
            } else if (!is.null(fallbackRecommendation) && nrow(fallbackRecommendation) == 1) {
                rec <- fallbackRecommendation
                detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                bestProb <- rec$bestProb * 100
                bestSamples <- rec$bestN
                html <- sprintf("<h4>Clinical Recommendations</h4>
                <p>Target sensitivity of %.0f%% could not be achieved within the evaluated range.</p>
                <p><b>Best observed performance:</b> %s model%s reached %.1f%% at %d samples.</p>",
                    targetConf * 100,
                    rec$method,
                    detailSuffix,
                    bestProb,
                    bestSamples)
                if (nzchar(obsListHtml)) {
                    html <- paste0(html,
                        "<p><b>Observed cumulative detection:</b></p><ul>",
                        obsListHtml,
                        "</ul>")
                }
            } else {
                html <- "<p>No sufficient information to produce clinical recommendations.</p>"
            }

            recommendText$setContent(html)

            keyResults <- self$results$keyResults

            targetLine <- sprintf("<p style='%s %s'><b>Target confidence:</b> %.0f%%%%</p>",
                private$.styleConstants$fontSize14,
                private$.styleConstants$colorPrimary,
                targetConf * 100)

            if (!is.null(primaryRecommendation) && nrow(primaryRecommendation) == 1) {
                rec <- primaryRecommendation
                detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                primaryLine <- sprintf("<p style='%s %s'><b>Recommended minimum samples:</b> %d (%s) achieving %.1f%%%% sensitivity%s.</p>",
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary,
                    rec$minSamples,
                    rec$method,
                    rec$achievedProb * 100,
                    detailSuffix)
            } else if (!is.null(fallbackRecommendation) && nrow(fallbackRecommendation) == 1) {
                rec <- fallbackRecommendation
                detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                primaryLine <- sprintf("<p style='%s %s'><b>Target not reached:</b> Best performance is %.1f%%%% at %d samples using %s%s.</p>",
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary,
                    rec$bestProb * 100,
                    rec$bestN,
                    rec$method,
                    detailSuffix)
            } else {
                primaryLine <- sprintf("<p style='%s %s'>No valid models were available to evaluate sampling adequacy.</p>",
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary)
            }

            methodListHtml <- ""
            if (!is.null(recommendationTable) && nrow(recommendationTable) > 0) {
                methodItems <- character(nrow(recommendationTable))
                for (i in seq_len(nrow(recommendationTable))) {
                    rec <- recommendationTable[i, ]
                    detailSuffix <- if (!is.na(rec$detail) && nzchar(rec$detail)) sprintf(" (%s)", rec$detail) else ""
                    methodItems[i] <- sprintf("<li><b>%s:</b> %s%s</li>",
                        rec$method,
                        rec$status,
                        detailSuffix)
                }
                methodListHtml <- paste(methodItems, collapse = "")
            }

            comparisonSection <- if (nzchar(methodListHtml)) {
                sprintf("<p style='%s %s'><b>Model comparison:</b></p><ul style='%s %s'>%s</ul>",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    methodListHtml)
            } else ""

            keyResultsHtml <- sprintf("<div style='%s %s %s %s %s'>%s%s%s</div>",
                private$.styleConstants$font,
                private$.styleConstants$bgLight,
                private$.styleConstants$borderPrimary,
                private$.styleConstants$padding15,
                private$.styleConstants$margin10,
                targetLine,
                primaryLine,
                comparisonSection)

            keyResults$setContent(keyResultsHtml)

            # === Clinical Summary (Plain Language) ===
            if (self$options$showClinicalSummary && self$options$showBootstrap) {
                clinicalSummary <- self$results$clinicalSummary

                if (is.null(bootstrapMeansVec)) {
                    html <- sprintf("<div style='%s %s %s %s'>
                        <h3 style='%s %s margin: 0 0 15px 0;'>Clinical Summary</h3>
                        <p style='%s margin: 0;'>
                            Unable to generate summary: Bootstrap analysis did not run.
                        </p>
                    </div>",
                    private$.styleConstants$font, private$.styleConstants$bgLight,
                    private$.styleConstants$borderPrimary, private$.styleConstants$padding15,
                    private$.styleConstants$colorPrimary, private$.styleConstants$fontSize16,
                    private$.styleConstants$fontSize14, private$.styleConstants$colorPrimary)

                    clinicalSummary$setContent(html)
                } else if (is.na(bootstrapTargetIdx) || length(bootstrapTargetIdx) == 0) {
                    html <- sprintf("<div style='%s %s %s %s'>
                        <h3 style='%s %s margin: 0 0 15px 0;'>Clinical Summary</h3>
                        <p style='%s margin: 0;'>
                            Target confidence (%.0f%%) not achievable with evaluated sample sizes (maximum: %d).
                            Consider increasing 'Maximum samples to evaluate' parameter.
                        </p>
                    </div>",
                    private$.styleConstants$font, private$.styleConstants$bgLight,
                    private$.styleConstants$borderPrimary, private$.styleConstants$padding15,
                    private$.styleConstants$colorPrimary, private$.styleConstants$fontSize16,
                    private$.styleConstants$fontSize14,
                    targetConf * 100, maxSamp)

                    clinicalSummary$setContent(html)
                } else {
                    # Get key metrics (safe now)
                    ciLower <- bootstrapCILowerVec[bootstrapTargetIdx]
                    ciUpper <- bootstrapCIUpperVec[bootstrapTargetIdx]
                    meanAtTarget <- bootstrapMeansVec[bootstrapTargetIdx]

                    perSampleText <- if (!is.na(pEstimate)) sprintf("%.1f%%", pEstimate * 100) else "Not estimated"
                    obs3Text <- if (!is.na(obs3)) sprintf("%.1f%%", obs3) else "Not available"

                outerStyle <- private$.buildStyle(
                    private$.styleConstants$font,
                    private$.styleConstants$bgLighter,
                    private$.styleConstants$borderPrimary,
                    private$.styleConstants$padding20,
                    private$.styleConstants$margin20
                )
                headerStyle <- private$.buildStyle(
                    private$.styleConstants$colorPrimary,
                    private$.styleConstants$fontSize18,
                    "margin: 0 0 15px 0;"
                )
                overviewDivStyle <- private$.buildStyle(
                    private$.styleConstants$bgWhite,
                    private$.styleConstants$borderSecondary,
                    private$.styleConstants$padding15,
                    private$.styleConstants$margin10
                )
                overviewHeadingStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                )
                overviewBodyStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 5px 0;"
                )
                overviewBodyText <- sprintf(
                    "Pathology sampling adequacy analysis of <strong>%d cases</strong> to determine the minimum number of samples required to reliably detect lesions.",
                    nCases)

                keyDivStyle <- private$.buildStyle(
                    private$.styleConstants$bgWhite,
                    private$.styleConstants$borderSecondary,
                    private$.styleConstants$padding15,
                    private$.styleConstants$margin10
                )
                keyHeadingStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary,
                    "margin: 0 0 10px 0;"
                )
                keyListStyle <- private$.buildStyle(
                    "margin: 0;",
                    "padding-left: 20px;",
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary
                )
                bootstrapIterationsText <- sprintf("%.0fk", nBoot / 1000)
                bootstrapSensitivityText <- sprintf("%.1f%%", meanAtTarget * 100)
                bootstrapCIText <- sprintf("%.1f%%-%.1f%%", ciLower * 100, ciUpper * 100)
                keyFindingsItems <- paste0(
                    sprintf("<li style='margin: 5px 0;'>Detection probability per sample: <strong>%s</strong></li>", perSampleText),
                    sprintf("<li style='margin: 5px 0;'>Recommended samples for %.0f%%%% sensitivity: <strong>%d samples</strong></li>", targetConf * 100, bootstrapTargetIdx),
                    sprintf("<li style='margin: 5px 0;'>Bootstrap validation (%s iterations): <strong>%s</strong> sensitivity (95%%%% CI: %s)</li>", bootstrapIterationsText, bootstrapSensitivityText, bootstrapCIText),
                    sprintf("<li style='margin: 5px 0;'>First 3 samples detected: <strong>%s</strong> of all cases</li>", obs3Text)
                )

                reportDivStyle <- private$.buildStyle(
                    private$.styleConstants$bgLight,
                    private$.styleConstants$borderLeft,
                    private$.styleConstants$padding10,
                    private$.styleConstants$margin10
                )
                reportHeadingStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize13,
                    private$.styleConstants$colorSecondary,
                    "margin: 0 0 8px 0;"
                )
                reportInnerStyle <- private$.buildStyle(
                    private$.styleConstants$bgWhite,
                    private$.styleConstants$borderSecondary,
                    private$.styleConstants$padding10,
                    private$.styleConstants$margin10
                )
                reportParagraphStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize14,
                    private$.styleConstants$colorPrimary,
                    "margin: 0; font-style: italic;"
                )
                reportParagraph <- sprintf(
                    "\"Sampling adequacy analysis of %d cases showed a per-sample detection probability of %s. To achieve %.0f%%%% sensitivity, a minimum of %d samples is recommended based on binomial probability modeling and bootstrap validation (%d iterations, 95%%%% CI: %.1f%%%%-%.1f%%%%). Observed data showed %s of lesions detected within first 3 samples.\"",
                    nCases,
                    perSampleText,
                    targetConf * 100,
                    bootstrapTargetIdx,
                    nBoot,
                    ciLower * 100,
                    ciUpper * 100,
                    obs3Text)

                recommendDivStyle <- private$.buildStyle(
                    private$.styleConstants$bgLight,
                    private$.styleConstants$borderPrimary,
                    private$.styleConstants$padding15
                )
                recommendParagraphStyle <- private$.buildStyle(
                    private$.styleConstants$fontSize15,
                    private$.styleConstants$colorPrimary,
                    "margin: 0;"
                )

                html <- paste0(
                    "<div style='", outerStyle, "'>",
                    "<h3 style='", headerStyle, "'>Clinical Summary</h3>",
                    "<div style='", overviewDivStyle, "'>",
                    "<p style='", overviewHeadingStyle, "'><strong>Analysis Overview:</strong></p>",
                    "<p style='", overviewBodyStyle, "'>", overviewBodyText, "</p>",
                    "</div>",
                    "<div style='", keyDivStyle, "'>",
                    "<p style='", keyHeadingStyle, "'><strong>Key Findings:</strong></p>",
                    "<ul style='", keyListStyle, "'>", keyFindingsItems, "</ul>",
                    "</div>",
                    "<div style='", reportDivStyle, "'>",
                    "<p style='", reportHeadingStyle, "'><em>Copy-ready text for reports:</em></p>",
                    "<div style='", reportInnerStyle, "'>",
                    "<p style='", reportParagraphStyle, "'>", reportParagraph, "</p>",
                    "</div>",
                    "</div>",
                    "<div style='", recommendDivStyle, "'>",
                    "<p style='", recommendParagraphStyle, "'><strong>Clinical Recommendation:</strong> Submit a minimum of <strong>",
                    bootstrapTargetIdx, " samples</strong> to ensure adequate diagnostic sensitivity in routine practice.</p>",
                    "</div>",
                    "</div>")

                clinicalSummary$setContent(html)
                }
            }

            # === Statistical Interpretation ===
            interpretText <- self$results$interpretText

            html <- "<h4>Statistical Interpretation</h4>
            <p>This analysis addresses the question: <i>How many tissue samples are necessary
            to reliably detect a lesion?</i></p>

            <p><b>Key Concepts:</b></p>
            <ul>
                <li><b>Sensitivity:</b> Probability of detecting the lesion if it is present</li>
                <li><b>Cumulative detection probability:</b> Increases with each additional sample</li>
                <li><b>Diminishing returns:</b> At some point, additional samples provide minimal gain</li>
                <li><b>Target confidence:</b> User-selected detection probability used to determine minimum sampling</li>
            </ul>

            <p><b>Assumptions and Limitations:</b></p>
            <ul>
                <li><b>Complete cohorts:</b> Record every case in the dataset, including those with no detected lesion. Use <code>NA</code> for the first-detection column when a lesion is never observed so the model can treat the case as a confirmed negative.</li>
                <li><b>Binomial model:</b> Assumes the per-sample detection probability is constant across all cases and samples. Heterogeneous disease burden can violate this assumption.</li>
                <li><b>Hypergeometric model:</b> Requires per-case totals and positives. It still simplifies within-case variability; the beta-binomial model relaxes this by allowing overdispersion.</li>
                <li><b>Residual false negatives:</b> Even when all submitted samples are examined, lesions may still be missed. The reported sensitivities therefore represent an upper bound conditional on the submitted material.</li>
                <li><b>Bootstrap CIs:</b> Assume the analyzed sample is representative of the broader population of interest.</li>
            </ul>"

            interpretText$setContent(html)

            # === Omentum-Specific Analysis ===
            if (self$options$showOmentumAnalysis) {
                private$.populateOmentumAnalysis()
            }

            # === References ===
            referencesText <- self$results$referencesText

            html <- "<h4>Statistical Methods & References</h4>
            <p><b>Methods:</b></p>
            <ul>
                <li>Binomial probability distribution for detection modeling</li>
                <li>Hypergeometric probability distribution for finite population sampling</li>
                <li>Beta-Binomial probability distribution for sampling with overdispersion</li>
                <li>Bootstrap resampling with replacement (10,000 iterations default)</li>
                <li>95% confidence intervals using percentile method</li>
                <li>Lymph node ratio (LNR) classification with X-tile optimized cutpoints</li>
                <li>Non-parametric effect size measures (Cliff's delta, Hodges-Lehmann)</li>
            </ul>

            <p><b>Key References:</b></p>
            <ul>
                <li><b>Lymph Node Adequacy & LNR Classification:</b>
                    <ul>
                        <li>Tomlinson JS, et al. Accuracy of Staging Node-Negative Pancreas Cancer.
                            <i>Arch Surg.</i> 2007;142(8):767-774.
                            <br><i>minELN=15, 8-month survival (SEER n=1,150, HR=0.63)</i></li>
                        <li>Pu N, et al. An Artificial Neural Network Improves Prediction of Observed Survival
                            in Patients with Pancreatic Cancer. <i>J Natl Compr Canc Netw.</i> 2021;19(9):1029-1036.
                            <br><i>minELN=12, binomial model, LNR thresholds 0.1 & 0.3 (n=1,837)</i></li>
                        <li>Yoon SJ, et al. Optimal Number of Lymph Nodes Retrieved to Lower False N0 Risk.
                            <i>Ann Surg Oncol.</i> 2025. doi:10.1245/s10434-025-18029-7
                            <br><i>minELN=12-16, false N0 modeling, Korean dual-cohort (n=1,252)</i></li>
                    </ul>
                </li>
                <li><b>Sampling Adequacy Methods:</b>
                    <ul>
                        <li>Skala SL, Hagemann IS. Pathologic Sampling of the Omentum.
                            <i>Int J Gynecol Pathol.</i> 2015;34(4):374-378.</li>
                        <li>Gönen M, et al. Nodal Staging Score for Colon Cancer.
                            <i>J Clin Oncol.</i> 2009;27(36):6166-6171.</li>
                        <li>Zhou J, et al. Beta-binomial model for lymph node yield.
                            <i>Front Oncol.</i> 2022;12:872527.</li>
                        <li>Buderer NM. Statistical methodology for diagnostic test sample size.
                            <i>Stat Med.</i> 1996;15(6):649-652.</li>
                    </ul>
                </li>
                <li><b>Distribution Pattern Analysis:</b>
                    <ul>
                        <li>Ates D, et al. Lymphovascular Space Invasion in Endometrial Cancer.
                            <i>Mod Pathol.</i> 2025;38:100885.
                            <br><i>Single vs summed cassette distribution patterns</i></li>
                    </ul>
                </li>
            </ul>"

            referencesText$setContent(html)

        },

        .detectionCurve = function(image, ggtheme, theme, ...) {

            if (is.null(private$.firstDetectionData))
                return()

            firstDetectionData <- private$.firstDetectionData
            pEstimate <- private$.pEstimate
            maxSamp <- private$.maxSamp
            nCases <- length(firstDetectionData)
            nPositiveCases <- sum(!is.na(firstDetectionData))

            # Calculate observed conditional probability (sensitivity)
            # Only among positive cases, not population-level
            nSamples <- 1:maxSamp
            observedProb <- sapply(nSamples, function(n) {
                if (nPositiveCases == 0) return(0)
                sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
            })
            if (is.na(pEstimate)) {
                predictedProb <- rep(NA_real_, length(nSamples))
            } else {
                predictedProb <- 1 - (1 - pEstimate)^nSamples
            }

            # Create data frame for plotting
            plotData <- data.frame(
                nSamples = rep(nSamples, 2),
                Probability = c(observedProb, predictedProb),
                Type = rep(c("Observed", "Binomial Model"), each = maxSamp)
            )

            # Create plot
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = nSamples, y = Probability,
                                                         color = Type, linetype = Type)) +
                ggplot2::geom_line(size = 1.2) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_hline(yintercept = self$options$targetConfidence,
                                   linetype = "dashed", color = "red", alpha = 0.5) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::scale_x_continuous(breaks = nSamples) +
                ggplot2::labs(
                    title = "Diagnostic Yield Curve",
                    subtitle = sprintf("Target sensitivity: %.0f%% (red line)",
                                      self$options$targetConfidence * 100),
                    x = "Number of Samples",
                    y = "Cumulative Detection Probability",
                    color = "Method",
                    linetype = "Method"
                ) +
                ggtheme +
                ggplot2::theme(
                    legend.position = "bottom",
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 11)
                )

            print(p)
            TRUE
        },

        .sensitivityPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.bootstrapResults))
                return()

            bootstrapResults <- private$.bootstrapResults
            maxSamp <- private$.maxSamp

            # Calculate statistics
            nSamples <- 1:maxSamp
            meanSens <- colMeans(bootstrapResults)
            ciLower <- apply(bootstrapResults, 2, quantile, 0.025)
            ciUpper <- apply(bootstrapResults, 2, quantile, 0.975)

            plotData <- data.frame(
                nSamples = nSamples,
                meanSens = meanSens,
                ciLower = ciLower,
                ciUpper = ciUpper
            )

            # Create plot
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = nSamples, y = meanSens)) +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = ciLower, ymax = ciUpper),
                                    alpha = 0.3, fill = "steelblue") +
                ggplot2::geom_line(color = "steelblue", size = 1.2) +
                ggplot2::geom_point(color = "steelblue", size = 3) +
                ggplot2::geom_hline(yintercept = self$options$targetConfidence,
                                   linetype = "dashed", color = "red", alpha = 0.5) +
                ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
                ggplot2::scale_x_continuous(breaks = nSamples) +
                ggplot2::labs(
                    title = "Bootstrap Sensitivity Estimates with 95% Confidence Intervals",
                    subtitle = sprintf("Based on %d bootstrap iterations",
                                      self$options$bootstrapIterations),
                    x = "Number of Samples",
                    y = "Sensitivity (Detection Probability)"
                ) +
                ggtheme +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 11)
                )

            print(p)
            TRUE
        },

        .empiricalCumulativePlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.firstDetectionData))
                return()

            firstDetectionData <- private$.firstDetectionData
            pEstimate <- private$.pEstimate
            maxSamp <- private$.maxSamp
            nPositiveCases <- sum(!is.na(firstDetectionData))

            if (nPositiveCases == 0)
                return()

            # Calculate empirical cumulative detection (conditional - positive cases only)
            nSamples <- 1:maxSamp
            empirical_prob <- sapply(nSamples, function(n) {
                sum(!is.na(firstDetectionData) & firstDetectionData <= n) / nPositiveCases
            })

            # Calculate binomial model for comparison
            if (!is.na(pEstimate) && pEstimate > 0) {
                model_prob <- 1 - (1 - pEstimate)^nSamples
            } else {
                model_prob <- rep(NA_real_, length(nSamples))
            }

            # Create data frame for plotting
            plot_data <- data.frame(
                samples = rep(nSamples, 2),
                probability = c(empirical_prob, model_prob),
                method = rep(c("Empirical (Observed)", "Binomial Model"), each = length(nSamples))
            )

            # Remove NAs for model if pEstimate not available
            plot_data <- plot_data[!is.na(plot_data$probability), ]

            # Create plot
            p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = samples, y = probability,
                                                          color = method, linetype = method)) +
                ggplot2::geom_line(size = 1.2) +
                ggplot2::geom_point(size = 3) +
                ggplot2::geom_hline(yintercept = self$options$targetConfidence,
                                   linetype = "dashed", color = "red", alpha = 0.5) +
                ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                           limits = c(0, 1),
                                           breaks = seq(0, 1, 0.2)) +
                ggplot2::scale_x_continuous(breaks = nSamples) +
                ggplot2::scale_color_manual(values = c("Empirical (Observed)" = "darkorange",
                                                       "Binomial Model" = "steelblue")) +
                ggplot2::scale_linetype_manual(values = c("Empirical (Observed)" = "solid",
                                                          "Binomial Model" = "dashed")) +
                ggplot2::labs(
                    title = "Empirical Cumulative Detection vs Binomial Model",
                    subtitle = sprintf("Conditional probability (sensitivity) | %d positive cases | q = %.3f",
                                      nPositiveCases,
                                      ifelse(is.na(pEstimate), 0, pEstimate)),
                    x = "Number of Samples Examined",
                    y = "Cumulative Detection Probability",
                    color = "Method",
                    linetype = "Method"
                ) +
                ggtheme +
                ggplot2::theme(
                    legend.position = "bottom",
                    plot.title = ggplot2::element_text(face = "bold", size = 14),
                    plot.subtitle = ggplot2::element_text(size = 11)
                )

            print(p)
            TRUE
        },

        .correlationPlot = function(image, ggtheme, theme, ...) {

            if (is.null(private$.totalSamplesData) || is.null(private$.positiveCassettesData))
                return()

            totalSamplesData <- private$.totalSamplesData
            positiveCassettesData <- private$.positiveCassettesData

            # Create data frame
            plotData <- data.frame(
                total = totalSamplesData,
                positive = positiveCassettesData
            )

            # Calculate correlation
            corTest <- cor.test(totalSamplesData, positiveCassettesData, method = "spearman")

            # Create scatter plot with regression line
            p <- ggplot2::ggplot(plotData, ggplot2::aes(x = total, y = positive)) +
                ggplot2::geom_point(alpha = 0.6, size = 3, color = "steelblue") +
                ggplot2::geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "pink", alpha = 0.2) +
                ggplot2::labs(
                    title = "Correlation: Total Cassettes Examined vs Positive Cassettes",
                    subtitle = sprintf("Spearman's rho = %.3f, p = %.4f",
                                      corTest$estimate, corTest$p.value),
                    x = "Total Cassettes Examined",
                    y = "Number of Positive Cassettes"
                ) +
                ggtheme +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 11)
                )

            print(p)
            TRUE
        },

        .populateOmentumAnalysis = function() {
            omentumText <- self$results$omentumText

            html <- "<h4>Omentum Sampling Adequacy - Literature Comparison</h4>

            <p><b>Recent Evidence:</b></p>
            <ul>
                <li><b>Maglalang & Fadare 2025 (UCSD, n=1,055):</b>
                    <br>• Grossly normal: 8.0% MPR, 1-2 blocks showed similar MPR to 5-6 blocks (P>.50)
                    <br>• Post-neoadjuvant: 19.0% MPR (5× higher, P=.03)
                    <br>• <b>Critical limitation:</b> No gold standard comparison, false negative rate unknown</li>
                <li><b>ISGyP Guidelines (2019):</b> ≥4 blocks for grossly normal omentum</li>
                <li><b>Skala & Hagemann 2015 (n=44, simulation):</b>
                    <br>• 5 blocks → 82% sensitivity
                    <br>• 10 blocks → 95% sensitivity</li>
            </ul>

            <p><b>Risk-Stratified Recommendations:</b></p>
            <table style='border-collapse: collapse; width: 100%;'>
                <tr style='background: #f0f0f0;'>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Scenario</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Maglalang MPR</th>
                    <th style='border: 1px solid #ddd; padding: 8px;'>Recommended Blocks</th>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Grossly normal, no NACT</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>8.0%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>≥4 (guideline)</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Grossly normal, post-NACT</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>19.0%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>≥6 (high risk)</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Multifocal/diffuse abnormal</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>92.1%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>2-3 (high pre-test probability)</td>
                </tr>
                <tr>
                    <td style='border: 1px solid #ddd; padding: 8px;'>Focal abnormal</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>66.4%</td>
                    <td style='border: 1px solid #ddd; padding: 8px;'>≥4 (heterogeneous)</td>
                </tr>
            </table>

            <p><i><b>Note:</b> Maglalang's retrospective design cannot assess false negative rates.
            Prospective validation with complete sampling gold standard is needed before reducing
            sampling intensity below guideline recommendations.</i></p>

            <p><b>References:</b></p>
            <ul style='font-size: 12px;'>
                <li>Maglalang NA, Fadare O. <i>Am J Clin Pathol.</i> 2025. doi:10.1093/ajcp/aqaf082</li>
                <li>Malpica A, et al. <i>Int J Gynecol Pathol.</i> 2019;38:S9-S24 (ISGyP guideline)</li>
                <li>Skala SL, Hagemann IS. <i>Int J Gynecol Pathol.</i> 2015;34:281-287</li>
            </ul>"

            omentumText$setContent(html)
        },

        # ===== Helper Functions for Enhanced Analyses =====

        # Parse comma-separated sample list
        .parseSampleList = function(sampleListString) {
            if (is.na(sampleListString) || sampleListString == "" ||
                is.null(sampleListString) || length(sampleListString) == 0) {
                return(integer(0))
            }

            # Handle both string and numeric input
            str_val <- as.character(sampleListString)
            if (str_val == "") return(integer(0))

            # Split and convert to integer
            samples <- tryCatch({
                as.integer(strsplit(str_val, ",")[[1]])
            }, error = function(e) {
                integer(0)
            })

            # Remove NAs and return
            samples <- samples[!is.na(samples)]
            return(samples)
        },

        # Calculate clustering index for a list of positive samples
        .calculateClusteringIndex = function(positiveSamples, totalSamples) {
            if (length(positiveSamples) <= 1) return(NA_real_)

            sorted_samples <- sort(positiveSamples)
            distances <- diff(sorted_samples)
            mean_distance <- mean(distances)

            expected_distance <- totalSamples / length(positiveSamples)

            if (expected_distance == 0) return(NA_real_)

            clustering_index <- mean_distance / expected_distance
            return(clustering_index)
        },

        # Estimate number of foci from sample positions
        .estimateFociCount = function(positiveSamples) {
            if (length(positiveSamples) <= 1) return(1)

            sorted_samples <- sort(positiveSamples)
            distances <- diff(sorted_samples)

            # Count gaps > 2 as separate foci
            gaps <- sum(distances > 2)
            foci <- gaps + 1

            return(foci)
        },

        # Bootstrap empirical cumulative detection
        .bootstrapEmpiricalCumulative = function(firstDetectionData, totalSamplesData,
                                                 maxN, nBoot = 10000) {

            positive_idx <- !is.na(firstDetectionData)
            positive_first <- firstDetectionData[positive_idx]
            positive_total <- totalSamplesData[positive_idx]

            if (length(positive_first) == 0) {
                return(NULL)
            }

            n_cases <- length(positive_first)
            detection_matrix <- matrix(NA_real_, nrow = nBoot, ncol = maxN)

            for (b in 1:nBoot) {
                boot_idx <- sample(n_cases, replace = TRUE)
                boot_first <- positive_first[boot_idx]

                for (n in 1:maxN) {
                    detected <- sum(boot_first <= n, na.rm = TRUE)
                    detection_matrix[b, n] <- detected / n_cases
                }
            }

            # Calculate percentiles
            ci_lower <- apply(detection_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
            ci_upper <- apply(detection_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
            ci_mean <- apply(detection_matrix, 2, mean, na.rm = TRUE)

            return(data.frame(
                n = 1:maxN,
                mean = ci_mean,
                lower = ci_lower,
                upper = ci_upper
            ))
        },

        # Private storage for plotting data
        .totalSamplesData = NULL,
        .firstDetectionData = NULL,
        .pEstimate = NULL,
        .maxSamp = NULL,
        .bootstrapResults = NULL,
        .positiveCassettesData = NULL,
        .maxPositiveSingleData = NULL
    )
)
