# NOTICES IMPLEMENTATION PATCH FOR pathologyagreement.b.R
# Apply these changes to migrate from legacy HTML warnings to jmvcore::Notice

# ============================================================================
# PART 1: Replace silent returns with ERROR Notices
# ============================================================================

# LOCATION: Line 86-90 in .run()
# OLD CODE:
#   if (is.null(self$options$dep1) || is.null(self$options$dep2))
#       return()
#   data <- self$data
#   if (nrow(data) == 0) return()

# NEW CODE:
.run = function() {
    private$.resetMessages()

    # ERROR: Missing required variables
    if (is.null(self$options$dep1) || is.null(self$options$dep2)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingVariables',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent('Method 1 and Method 2 variables are required. Please select both variables to begin analysis.')
        self$results$insert(1, notice)
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
        self$results$insert(1, notice)
        return()
    }

    # ... continue with rest of .run()
}

# ============================================================================
# PART 2: Replace insufficient data HTML error with ERROR Notice
# ============================================================================

# LOCATION: Line 112-118 in .run()
# OLD CODE:
#   if (length(method1) < 3) {
#       self$results$interpretation$setContent(
#           "<p style='color: red;'><strong>Error:</strong> Insufficient data..."
#       )
#       return()
#   }

# NEW CODE:
if (length(method1) < 3) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'insufficientData',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent(sprintf('Insufficient complete observations (n=%d). At least 3 paired observations required for agreement analysis.', length(method1)))
    self$results$insert(1, notice)
    return()
}

# ============================================================================
# PART 3: Convert accumulated messages to prioritized Notices
# ============================================================================

# LOCATION: Replace .accumulateMessage calls throughout with Notice insertions

# EXAMPLE 1: Sample size warning (Line 725)
# OLD:
#   private$.accumulateMessage(paste0("Sample size (n=", n, ") is below recommended minimum..."))

# NEW:
if (n < private$.CLINICAL_CONSTANTS$MIN_SAMPLE_PATHOLOGY) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'sampleSizeWarning',
        type = jmvcore::NoticeType$WARNING
    )
    notice$setContent(sprintf('Sample size (n=%d) below recommended minimum (n=30) for pathology validation studies. Consider increasing sample size for robust estimates.', n))
    self$results$insert(10, notice)  # Mid position for warnings
}

# EXAMPLE 2: Normality violation (Line 344)
# OLD:
#   private$.accumulateMessage("Pearson correlation assumes normality, but data appears non-normal...")

# NEW:
if (sw1$p.value < 0.05 || sw2$p.value < 0.05) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'normalityViolation',
        type = jmvcore::NoticeType$WARNING
    )
    notice$setContent('Normality assumption violated (Shapiro-Wilk p<0.05). Spearman rank correlation recommended over Pearson.')
    self$results$insert(10, notice)
}

# EXAMPLE 3: Negative ICC (Line 218)
# OLD:
#   private$.accumulateMessage(paste0("Negative ICC (", round(icc_value, 3), ") detected..."))

# NEW:
if (!is.na(icc_value) && icc_value < 0) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'negativeICC',
        type = jmvcore::NoticeType$STRONG_WARNING
    )
    notice$setContent(sprintf('Negative ICC (%.3f) indicates severe reliability issues, often due to model assumption errors or greater within-subject than between-subject variance.', icc_value))
    self$results$insert(2, notice)  # After errors, before warnings
}

# EXAMPLE 4: Missing data info (Line 103)
# OLD:
#   private$.accumulateMessage(paste0("Listwise deletion: ", n_removed, " cases removed..."))

# NEW:
if (n_removed > 0) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'missingDataInfo',
        type = jmvcore::NoticeType$INFO
    )
    notice$setContent(sprintf('Listwise deletion removed %d cases (%.1f%%) with missing values. Analysis based on %d complete observations.',
                              n_removed, 100*n_removed/(length(method1)+n_removed), length(method1)))
    self$results$insert(999, notice)  # Bottom position for info
}

# EXAMPLE 5: Package missing ERROR (Line 189, 223, 638)
# OLD:
#   if (requireNamespace('psych', quietly = TRUE)) { ... } else { icc_value <- NA }

# NEW:
if (!requireNamespace('psych', quietly = TRUE)) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'psychPackageMissing',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent('Required package "psych" not installed. Install via: install.packages("psych")')
    self$results$insert(1, notice)
    return()
}

if (!requireNamespace('epiR', quietly = TRUE)) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'epiRPackageMissing',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent('Required package "epiR" not installed. Install via: install.packages("epiR")')
    self$results$insert(1, notice)
    return()
}

# ============================================================================
# PART 4: Remove legacy HTML warnings infrastructure
# ============================================================================

# DELETE these elements from private list:
# - .messages = NULL
# - .accumulateMessage = function(msg) { ... }
# - .resetMessages = function() { ... }

# DELETE from .run() (Lines 148-160):
# if (!is.null(private$.messages) && length(private$.messages) > 0) {
#      self$results$warnings$setContent(...)
# } else {
#     self$results$warnings$setVisible(FALSE)
# }

# ============================================================================
# PART 5: Keep HTML warnings output for backward compatibility (optional)
# ============================================================================

# OPTION A: Remove `warnings` from .r.yaml entirely (clean break)
# OPTION B: Keep it but use for aggregated summary only:

summary_notice <- sprintf('Analysis completed: %d methods, %d observations, %d notices generated.',
                          n_methods, nrow(data), notice_count)
self$results$warnings$setContent(paste0("<div class='alert alert-info'>", summary_notice, "</div>"))

# ============================================================================
# SUMMARY OF NOTICE POSITIONING STRATEGY
# ============================================================================

# Position 1-5:     ERROR notices (missing vars, empty data, package errors)
# Position 6-20:    STRONG_WARNING notices (negative ICC, severe violations)
# Position 21-100:  WARNING notices (sample size, normality, biomarker range)
# Position 101-998: Reserved for future use
# Position 999:     INFO notices (missing data summary, analysis complete)

# ============================================================================
# END OF PATCH
# ============================================================================
