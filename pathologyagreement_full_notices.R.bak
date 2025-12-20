# COMPLETE .run() METHOD WITH NOTICES
# Replace lines 83-160 in R/pathologyagreement.b.R

.run = function() {
    # Track notice insertion position
    notice_position <- 1

    # ========================================================================
    # CRITICAL ERRORS (Position 1-5)
    # ========================================================================

    # ERROR: Missing required variables
    if (is.null(self$options$dep1) || is.null(self$options$dep2)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingVariables',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Method 1 and Method 2 variables are required. Please select both variables to begin analysis.')
        self$results$insert(notice_position, notice)
        return()
    }

    data <- self$data

    # ERROR: Empty dataset
    if (nrow(data) == 0) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'emptyDataset',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Dataset is empty. Please provide data with observations.')
        self$results$insert(notice_position, notice)
        return()
    }

    # ERROR: Check package availability EARLY
    if (!requireNamespace('psych', quietly = TRUE)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'psychPackageMissing',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Required R package "psych" not installed. Install via: install.packages("psych")')
        self$results$insert(notice_position, notice)
        return()
    }

    if (!requireNamespace('epiR', quietly = TRUE)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'epiRPackageMissing',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Required R package "epiR" not installed. Install via: install.packages("epiR")')
        self$results$insert(notice_position, notice)
        return()
    }

    # Get variables with safe conversion (handles factors/labelled data)
    method1 <- jmvcore::toNumeric(data[[self$options$dep1]])
    method2 <- jmvcore::toNumeric(data[[self$options$dep2]])

    # Handle missing values based on option
    n_removed <- 0
    if (self$options$missing_data == "listwise") {
        complete_cases <- complete.cases(method1, method2)
        n_total <- length(method1)
        method1 <- method1[complete_cases]
        method2 <- method2[complete_cases]
        n_removed <- n_total - length(method1)
    }

    # ERROR: Insufficient data after cleaning
    if (length(method1) < 3) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'insufficientData',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent(sprintf('Insufficient complete observations (n=%d). At least 3 paired observations required for agreement analysis.', length(method1)))
        self$results$insert(notice_position, notice)
        return()
    }

    # ========================================================================
    # STRONG WARNINGS (Position 6-20)
    # ========================================================================

    notice_position <- 6  # Start STRONG_WARNING section

    # Enhanced validation based on clinical preset
    n <- length(method1)

    # STRONG_WARNING: Very small sample (clinical threshold)
    if (n < 10) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'verySmallSample',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        notice$setContent(sprintf('Very small sample (n=%d). Results may be unreliable. Minimum n=30 recommended for pathology validation studies.', n))
        self$results$insert(notice_position, notice)
        notice_position <- notice_position + 1
    }

    # ========================================================================
    # WARNINGS (Position 21-100)
    # ========================================================================

    notice_position <- 21  # Start WARNING section

    # WARNING: Sample size below recommended minimum
    if (n >= 10 && n < private$.CLINICAL_CONSTANTS$MIN_SAMPLE_PATHOLOGY) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'sampleSizeWarning',
            type = jmvcore::NoticeType$WARNING
        )
        notice$setContent(sprintf('Sample size (n=%d) below recommended minimum (n=30) for pathology validation studies. Consider increasing sample size for robust estimates.', n))
        self$results$insert(notice_position, notice)
        notice_position <- notice_position + 1
    }

    # WARNING: Biomarker range validation
    if (self$options$clinical_preset == "biomarker_platforms") {
        range_check1 <- any(method1 < private$.CLINICAL_CONSTANTS$BIOMARKER_MIN_RANGE |
                           method1 > private$.CLINICAL_CONSTANTS$BIOMARKER_MAX_RANGE, na.rm = TRUE)
        range_check2 <- any(method2 < private$.CLINICAL_CONSTANTS$BIOMARKER_MIN_RANGE |
                           method2 > private$.CLINICAL_CONSTANTS$BIOMARKER_MAX_RANGE, na.rm = TRUE)

        if (range_check1 || range_check2) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'biomarkerRangeWarning',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent('Some biomarker values outside typical 0-100% range. Please verify data scaling (e.g., ensure percentages not decimals).')
            self$results$insert(notice_position, notice)
            notice_position <- notice_position + 1
        }
    }

    # WARNING: Bootstrap recommendation for high-stakes studies
    if (self$options$bootstrap_n < private$.CLINICAL_CONSTANTS$BOOTSTRAP_RECOMMENDED &&
        self$options$clinical_preset %in% c("multisite_validation", "ai_pathologist")) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'bootstrapRecommendation',
            type = jmvcore::NoticeType$WARNING
        )
        notice$setContent(sprintf('Bootstrap replicates (n=%d) below FDA-recommended threshold (n=2000) for high-stakes validation studies. Consider increasing for regulatory submissions.', self$options$bootstrap_n))
        self$results$insert(notice_position, notice)
        notice_position <- notice_position + 1
    }

    # ========================================================================
    # PERFORM ANALYSIS (notice positions reserved for dynamic warnings)
    # ========================================================================

    private$.performAgreementAnalysis(method1, method2, notice_position)
    private$.performCorrelationAnalysis(method1, method2, notice_position)
    private$.generatePlots(method1, method2)

    # Perform multi-method analysis if additional methods provided
    if (!is.null(self$options$additional_methods) && length(self$options$additional_methods) > 0) {
        if (length(self$options$additional_methods) >= 1) {
            all_methods <- private$.extractAllMethods(data)
            if (ncol(all_methods) >= 3) {
                private$.performMultiMethodAnalysis(all_methods)
            }
        }
    }

    private$.generateInterpretation(method1, method2)

    # Generate copy-ready report, glossary, and ICC guide (if interpretation enabled)
    if (self$options$show_interpretation) {
        private$.generateReportSentences(method1, method2)
        private$.generateStatisticalGlossary()
        private$.generateICCSelectionGuide()
    }

    # ========================================================================
    # INFO NOTICES (Position 999)
    # ========================================================================

    # INFO: Missing data summary (if any removed)
    if (n_removed > 0) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingDataInfo',
            type = jmvcore::NoticeType$INFO
        )
        notice$setContent(sprintf('Listwise deletion removed %d cases (%.1f%%) with missing values. Analysis based on %d complete observations.',
                                  n_removed, 100*n_removed/(length(method1)+n_removed), length(method1)))
        self$results$insert(999, notice)
    }

    # INFO: Analysis completion summary
    n_methods <- if (!is.null(self$options$additional_methods)) length(self$options$additional_methods) + 2 else 2
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'analysisComplete',
        type = jmvcore::NoticeType$INFO
    )
    notice$setContent(sprintf('Agreement analysis completed: %d methods compared, %d complete observations analyzed.', n_methods, n))
    self$results$insert(999, notice)
}


# ========================================================================
# UPDATED .performAgreementAnalysis with STRONG_WARNING for negative ICC
# ========================================================================

.performAgreementAnalysis = function(method1, method2, notice_position = 6) {
    # ... existing code for ICC calculation (lines 189-220) ...

    # STRONG_WARNING: Negative ICC
    if (!is.na(icc_value) && icc_value < 0) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'negativeICC',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        notice$setContent(sprintf('Negative ICC (%.3f) indicates severe reliability issues, often due to model assumption errors or greater within-subject than between-subject variance.', icc_value))
        self$results$insert(notice_position, notice)
    }

    # ... rest of agreement analysis code (lines 221-305) ...
}


# ========================================================================
# UPDATED .performCorrelationAnalysis with normality warning
# ========================================================================

.performCorrelationAnalysis = function(method1, method2, notice_position = 21) {
    # ... existing code ...

    # WARNING: Normality violation for Pearson
    if (method_option %in% c("both", "pearson")) {
        if (length(method1) >= 3 && length(method1) <= 5000) {
             sw1 <- shapiro.test(method1)
             sw2 <- shapiro.test(method2)
             if (sw1$p.value < 0.05 || sw2$p.value < 0.05) {
                notice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'normalityViolation',
                    type = jmvcore::NoticeType$WARNING
                )
                notice$setContent('Normality assumption violated (Shapiro-Wilk p<0.05). Spearman rank correlation recommended over Pearson for non-normal data.')
                self$results$insert(notice_position, notice)
             }
        }

        # ... rest of Pearson correlation code ...
    }

    # ... rest of correlation analysis ...
}


# ========================================================================
# DELETE these private functions (no longer needed):
# ========================================================================

# DELETE .messages = NULL
# DELETE .accumulateMessage = function(msg) { ... }
# DELETE .resetMessages = function() { ... }
