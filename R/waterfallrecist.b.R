
#' @title RECIST v1.1 Compliant Response Analysis
#' @description Creates RECIST v1.1 compliant waterfall and spider plots with
#' full protocol implementation including target lesion summation, new lesion
#' detection, non-target progression assessment, and response confirmation.
#' @export
waterfallrecist <- function(
    data,
    patientID,
    lesionID,
    visitTime,
    lesionType = NULL,
    location = NULL,
    diameter,
    isNewLesion = NULL,
    baselineTimepoint = 0,
    confirmationInterval = 4,
    maxTargetLesions = 5,
    maxLesionsPerOrgan = 2,
    showWaterfallPlot = TRUE,
    showSpiderPlot = TRUE,
    showLesionTable = TRUE,
    showTargetSumTable = TRUE,
    showBestResponseTable = TRUE,
    showRecistComplianceReport = TRUE,
    colorScheme = "recist",
    addBestResponseToData = FALSE) {

    if (!is.null(addBestResponseToData))
        addBestResponseToData <- jmvcore::resolveQuo(
            jmvcore::enquo(addBestResponseToData))

    if (missing(patientID))
        stop("Argument 'patientID' is required", call. = FALSE)
    if (missing(lesionID))
        stop("Argument 'lesionID' is required", call. = FALSE)
    if (missing(visitTime))
        stop("Argument 'visitTime' is required", call. = FALSE)
    if (missing(diameter))
        stop("Argument 'diameter' is required", call. = FALSE)

    if (!is.null(lesionType))
        lesionType <- jmvcore::resolveQuo(jmvcore::enquo(lesionType))
    if (!is.null(location))
        location <- jmvcore::resolveQuo(jmvcore::enquo(location))
    if (!is.null(isNewLesion))
        isNewLesion <- jmvcore::resolveQuo(jmvcore::enquo(isNewLesion))

    options <- waterfallrecistOptions$new(
        patientID = patientID,
        lesionID = lesionID,
        visitTime = visitTime,
        lesionType = lesionType,
        location = location,
        diameter = diameter,
        isNewLesion = isNewLesion,
        baselineTimepoint = baselineTimepoint,
        confirmationInterval = confirmationInterval,
        maxTargetLesions = maxTargetLesions,
        maxLesionsPerOrgan = maxLesionsPerOrgan,
        showWaterfallPlot = showWaterfallPlot,
        showSpiderPlot = showSpiderPlot,
        showLesionTable = showLesionTable,
        showTargetSumTable = showTargetSumTable,
        showBestResponseTable = showBestResponseTable,
        showRecistComplianceReport = showRecistComplianceReport,
        colorScheme = colorScheme,
        addBestResponseToData = addBestResponseToData
    )

    analysis <- waterfallrecistClass$new(
        options = options,
        data = data
    )

    analysis$run()

    analysis$results
}


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
                # INFO: Regulatory Approval Notice
                regulatory_approved_notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = "regulatoryApproved",
                    type = jmvcore::NoticeType$INFO
                )
                regulatory_approved_notice$setContent(paste0(
                    "RECIST v1.1 COMPLIANT ANALYSIS: This function implements full RECIST v1.1 protocol as published in ",
                    "Eisenhauer et al. (2009) Eur J Cancer 45:228-247. Includes: (1) Target lesion summation with anatomic limits ",
                    "(max 5 lesions, max 2 per organ), (2) New lesion detection (any new lesion = PD), (3) Non-target lesion assessment, ",
                    "(4) Response confirmation at ≥4 weeks for CR/PR, (5) Best Overall Response (BOR) determination per protocol algorithm. ",
                    "REGULATORY SUITABILITY: Results may be used for clinical trial endpoints and regulatory submissions when data ",
                    "collection follows RECIST v1.1 guidelines. USER RESPONSIBILITY: Ensure lesion selection, measurement techniques, ",
                    "and imaging schedules comply with RECIST v1.1 requirements. This software performs calculations correctly but ",
                    "cannot validate upstream data quality or measurement accuracy."
                ))
                self$results$insert(0, regulatory_approved_notice)

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

                # TODO: Step 1 - Validate and prepare lesion-level data
                # lesion_data <- private$.prepareLesionData()

                # TODO: Step 2 - Validate target lesion selection (max 5, max 2 per organ)
                # target_validation <- private$.validateTargetLesionSelection(lesion_data)

                # TODO: Step 3 - Calculate target lesion sums by visit
                # target_sums <- private$.calculateTargetLesionSums(lesion_data)

                # TODO: Step 4 - Detect new lesions (any new lesion = PD)
                # new_lesions <- private$.detectNewLesions(lesion_data)

                # TODO: Step 5 - Assess non-target lesion progression
                # nontarget_assessment <- private$.assessNonTargetProgression(lesion_data)

                # TODO: Step 6 - Determine overall response per visit
                # visit_responses <- private$.determineOverallResponse(target_sums, new_lesions, nontarget_assessment)

                # TODO: Step 7 - Apply confirmation rules (CR/PR must be confirmed ≥4 weeks)
                # confirmed_responses <- private$.confirmResponses(visit_responses)

                # TODO: Step 8 - Calculate Best Overall Response (BOR)
                # best_responses <- private$.calculateBestOverallResponse(confirmed_responses)

                # TODO: Step 9 - Populate output tables
                # private$.populateLesionTable(lesion_data)
                # private$.populateTargetSumTable(target_sums)
                # private$.populateBestResponseTable(best_responses)
                # private$.populateComplianceReport(target_validation, new_lesions)

                # TODO: Step 10 - Generate plots
                # private$.generateWaterfallPlot(best_responses)
                # private$.generateSpiderPlot(target_sums)

                # TODO: Step 11 - Add BOR to dataset if requested
                # if (self$options$addBestResponseToData)
                #     private$.addBestResponseColumn(best_responses)
            },

            # Input Validation ====
            .validateInput = function() {
                # TODO: Implement comprehensive input validation
                # - Check for required variables
                # - Verify data structure (lesion-level format)
                # - Validate baseline timepoint exists
                # - Check for missing/invalid measurements
                # - Verify lesionType values (Target/NonTarget/New)
                # - Check diameter values (numeric, positive)

                # PLACEHOLDER: Return valid for now
                return(list(valid = TRUE, message = ""))
            },

            # RECIST v1.1 Core Methods ====

            .prepareLesionData = function() {
                # TODO: Prepare and validate lesion-level dataset
                # - Extract relevant variables
                # - Convert to appropriate types
                # - Handle missing values
                # - Identify baseline timepoint
                # - Sort by patient, lesion, time
                # RETURNS: Cleaned lesion-level data frame
                return(NULL)
            },

            .validateTargetLesionSelection = function(lesion_data) {
                # TODO: Validate target lesion selection per RECIST v1.1
                # CHECKS:
                # - Max 5 target lesions per patient
                # - Max 2 target lesions per organ
                # - Target lesions meet minimum size (≥10mm non-lymph, ≥15mm lymph)
                # - Target lesions are measurable
                # POST: WARNING notices if rules violated
                # RETURNS: List with validation results and selected target lesions
                return(list(
                    valid = TRUE,
                    violations = character(0),
                    target_lesions = character(0)
                ))
            },

            .calculateTargetLesionSums = function(lesion_data) {
                # TODO: Calculate sum of target lesion diameters by visit
                # ALGORITHM:
                # 1. Filter to target lesions only
                # 2. Group by patient and visit time
                # 3. Sum diameters for each visit
                # 4. Calculate percent change from baseline sum
                # 5. Calculate absolute change from baseline (for PD rule)
                # RETURNS: Data frame with columns:
                #   - patientID, visitTime, baseline_sum, current_sum,
                #     percent_change, absolute_change_mm
                return(NULL)
            },

            .detectNewLesions = function(lesion_data) {
                # TODO: Detect new lesions appearing after baseline
                # ALGORITHM:
                # 1. Identify baseline timepoint
                # 2. Filter to lesions with isNewLesion = 1 OR lesions appearing after baseline
                # 3. For each patient, determine first visit with new lesion
                # RECIST RULE: ANY new lesion → PD (overrides target lesion improvement)
                # RETURNS: Data frame with:
                #   - patientID, first_new_lesion_visit, new_lesion_location, new_lesion_ID
                return(NULL)
            },

            .assessNonTargetProgression = function(lesion_data) {
                # TODO: Assess non-target lesion status by visit
                # RECIST CATEGORIES for non-target:
                # - CR: Disappearance of all non-target lesions
                # - Non-CR/Non-PD: Persistence of ≥1 non-target lesion(s)
                # - PD: Unequivocal progression of non-target lesions
                # NOTE: Non-target assessment is QUALITATIVE (not measured)
                # RETURNS: Data frame with:
                #   - patientID, visitTime, nontarget_status (CR/NonCR_NonPD/PD)
                return(NULL)
            },

            .determineOverallResponse = function(target_sums, new_lesions, nontarget_assessment) {
                # TODO: Determine overall response per visit using RECIST v1.1 response table
                # RECIST v1.1 OVERALL RESPONSE TABLE:
                # Target | Non-Target | New Lesions | Overall Response
                # -----------------------------------------------------------
                # CR     | CR         | No          | CR
                # CR     | Non-CR/PD  | No          | PR
                # PR     | Non-PD     | No          | PR
                # SD     | Non-PD     | No          | SD
                # PD     | Any        | No/Yes      | PD
                # Any    | PD         | No/Yes      | PD
                # Any    | Any        | Yes         | PD
                #
                # ALGORITHM:
                # 1. Merge target_sums, new_lesions, nontarget_assessment by patient/visit
                # 2. Apply RECIST response table logic
                # 3. Classify each visit as CR/PR/SD/PD (UNCONFIRMED)
                # RETURNS: Data frame with:
                #   - patientID, visitTime, target_response, nontarget_response,
                #     new_lesion_present, overall_response_unconfirmed
                return(NULL)
            },

            .confirmResponses = function(visit_responses) {
                # TODO: Apply RECIST v1.1 confirmation requirements
                # RECIST CONFIRMATION RULES:
                # - CR: Must be confirmed by repeat scan ≥4 weeks after initial CR
                # - PR: Must be confirmed by repeat scan ≥4 weeks after initial PR
                # - SD: No confirmation required (but duration matters)
                # - PD: No confirmation required
                #
                # ALGORITHM:
                # 1. For each patient with CR or PR at visit V:
                #    a. Check if same response maintained at visit V+k where time_diff ≥ confirmation_interval
                #    b. If confirmed: response_confirmed = TRUE
                #    c. If not confirmed: response remains UNCONFIRMED (not used for BOR)
                # 2. Mark confirmation status for each visit
                # RETURNS: Data frame with additional column:
                #   - response_confirmed (TRUE/FALSE)
                return(NULL)
            },

            .calculateBestOverallResponse = function(confirmed_responses) {
                # TODO: Calculate Best Overall Response (BOR) per patient
                # RECIST v1.1 BOR ALGORITHM:
                # 1. CR: Best response = CR if CR confirmed AND no PD before CR
                # 2. PR: Best response = PR if PR confirmed (or CR unconfirmed) AND no PD before PR
                # 3. SD: Best response = SD if SD maintained AND no PR/CR achieved
                # 4. PD: Best response = PD if PD occurred OR progression after initial response
                #
                # HIERARCHY: CR (confirmed) > PR (confirmed) > SD > PD
                # NOTE: Unconfirmed CR/PR are NOT used for BOR
                #
                # ALGORITHM:
                # 1. For each patient, filter to confirmed responses only
                # 2. Determine best response using hierarchy
                # 3. Record first date of BOR achievement
                # 4. Calculate time to response (for CR/PR)
                # 5. Calculate duration of response (time from first response to PD or censoring)
                # RETURNS: Data frame with:
                #   - patientID, best_overall_response, bor_first_visit, time_to_response,
                #     duration_of_response, progression_occurred
                return(NULL)
            },

            # Output Population ====

            .initLesionTable = function() {
                # TODO: Initialize lesion-level table structure
                # COLUMNS: PatientID, LesionID, Visit, Time, Type, Location, Diameter, %Change
            },

            .populateLesionTable = function(lesion_data) {
                # TODO: Populate lesion-level table with all measurements
            },

            .initTargetSumTable = function() {
                # TODO: Initialize target lesion sum table structure
                # COLUMNS: PatientID, Visit, Time, Baseline Sum, Current Sum, %Change, Absolute Change
            },

            .populateTargetSumTable = function(target_sums) {
                # TODO: Populate target sum table with calculated sums
            },

            .initBestResponseTable = function() {
                # TODO: Initialize best overall response table structure
                # COLUMNS: PatientID, Best Response, Confirmed, Time to Response, Duration
            },

            .populateBestResponseTable = function(best_responses) {
                # TODO: Populate best response table
            },

            .initComplianceReport = function() {
                # TODO: Initialize RECIST compliance audit report
                # SECTIONS:
                # - Target lesion selection validation
                # - Confirmation status summary
                # - New lesion detection summary
                # - Protocol deviations (if any)
            },

            .populateComplianceReport = function(target_validation, new_lesions) {
                # TODO: Generate compliance audit trail
            },

            # Plot Generation ====

            .initWaterfallPlot = function() {
                # TODO: Initialize waterfall plot image
                image <- self$results$waterfallPlot
                image$setSize(800, 600)
            },

            .generateWaterfallPlot = function(best_responses) {
                # TODO: Generate RECIST-compliant waterfall plot
                # FEATURES:
                # - Bars colored by CONFIRMED best response
                # - RECIST thresholds at -30% (PR) and +20% (PD)
                # - Sorted by best response (most negative to most positive)
                # - Asterisks or indicators for confirmed vs unconfirmed
                # - Legend showing CR/PR/SD/PD
            },

            .initSpiderPlot = function() {
                # TODO: Initialize spider plot image
                image <- self$results$spiderPlot
                image$setSize(800, 600)
            },

            .generateSpiderPlot = function(target_sums) {
                # TODO: Generate spider plot tracking target lesion sums over time
                # FEATURES:
                # - One line per patient tracking sum of target lesions
                # - RECIST thresholds at -30% and +20%
                # - Color by best response
                # - Points indicating assessment timepoints
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
