# CODE REVIEW: `statsplot2`

**Date:** 2025-11-15
**Reviewer:** Claude Code (Automated Review)
**Function:** Automatic Plot Selection Based on Variable Types
**Version:** 0.0.32

---

## EXECUTIVE SUMMARY

**Overall Quality**: ⭐⭐⭐⭐ (4/5)
**Maintainability**: HIGH
**Performance**: GOOD
**User Experience**: GOOD (Clinical: NEEDS_WORK)

### Quick Stats
- **Lines of Code**: 1,143 (.b.R)
- **Functions**: 15 private methods
- **Complexity**: Medium-High
- **Test Coverage**: ❌ NO TESTS FOUND
- **Documentation**: Good (descriptive .a.yaml, auto-generated .Rd)

---

## STRENGTHS

### 1. **Excellent Architecture & Design** ⭐⭐⭐⭐⭐

**R6 Class Structure** (lines 8-1142):
```r
statsplot2Class <- if (requireNamespace('jmvcore'))
    R6::R6Class(
        "statsplot2Class",
        inherit = statsplot2Base,
        private = list(...)
    )
```
- Clean inheritance from jamovi base class
- Well-organized private methods with clear responsibilities
- Proper separation of concerns (detection, validation, plotting, fallback)

**Method Organization**:
- `.detectAnalysisType()` - Type detection with caching (lines 130-194)
- `.validatePlotData()` - Centralized validation (lines 29-127)
- `.prepareDataForPlot()` - Data preprocessing (lines 693-726)
- `.generatePlot()` - Plot dispatcher (lines 729-765)
- Specialized plot methods for each combination (lines 768-943)

### 2. **Robust Error Handling & Validation** ⭐⭐⭐⭐⭐

**Multi-Layer Validation** (lines 29-127):
```r
.validatePlotData = function(prepared_data, plot_type) {
    # Basic data validation
    if (nrow(data) < 2) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'insufficientData',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent(glue::glue(
            "Insufficient data for {plot_type}.\n",
            "• Variables: {y_var} by {x_var}\n",
            "• Found: {nrow(data)} observation(s)\n",
            "• Required: ≥2 observations\n",
            "• Check your data filtering."
        ))
        self$results$insert(1, notice)
        return(FALSE)
    }
    // ... more validations
}
```

**Contextual Error Messages**:
- Variable names included in every error
- Expected vs. actual values clearly stated
- Actionable suggestions provided
- Hierarchical notice system (ERROR → STRONG_WARNING → WARNING → INFO)

**Examples of Excellence**:
- Lines 41-49: Insufficient data with specific counts
- Lines 64-71: Missing values detection per variable
- Lines 98-105: Factor level validation with data preview
- Lines 470-478: Empty dataset with variable selection context

### 3. **Smart Caching & Performance** ⭐⭐⭐⭐

**Analysis Type Caching** (lines 14, 130-194):
```r
# Cache for analysis results to avoid redundant calculations
.cached_analysis = NULL,

.detectAnalysisType = function(force_refresh = FALSE) {
    # Return cached result if available and no refresh requested
    if (!is.null(private$.cached_analysis) && !force_refresh) {
        return(private$.cached_analysis)
    }
    // ... detection logic
    private$.cached_analysis <- analysis_info
    return(analysis_info)
}
```

**Large Dataset Sampling** (lines 704-709):
```r
if (self$options$sampleLarge && original_nrow > 10000) {
    set.seed(42)  # For reproducible sampling
    sample_size <- 5000
    mydata <- mydata[sample(nrow(mydata), sample_size), ]
    message(glue::glue("Large dataset detected ({original_nrow:,} rows).
            Sampled {sample_size:,} rows..."))
}
```

**Checkpoint Integration**:
- Lines 775, 794, 813, 835, 856, 902, 998, 1087, 1127: Strategic checkpoints before expensive operations
- Proper `flush = FALSE` for data aggregation (line 902)

### 4. **Comprehensive Assumption Checking** ⭐⭐⭐⭐⭐

**Statistical Assumption Validation** (lines 297-424):

**Sample Size Warnings** (lines 304-318):
```r
total_n <- sum(!is.na(dep_data) & !is.na(group_data))
if (total_n < 30) {
    notice$setContent(glue::glue(
        "Small sample size detected (n={total_n}).\n",
        "• Nonparametric approaches recommended for n<30\n",
        "• Consider robust statistical methods\n",
        "• Results may have reduced statistical power"
    ))
}
```

**Outlier Detection** (lines 322-342):
```r
Q1 <- quantile(dep_data, 0.25, na.rm = TRUE)
Q3 <- quantile(dep_data, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
extreme_outliers <- sum(dep_data < (Q1 - 3.5 * IQR_val) |
                        dep_data > (Q3 + 3.5 * IQR_val), na.rm = TRUE)
if (extreme_outliers > 0) {
    notice$setContent(glue::glue(
        "Extreme outliers detected in {analysis_info$dep_var}.\n",
        "• Found: {extreme_outliers} extreme outlier(s) (>3.5 IQR)\n",
        "• Consider robust statistical approach (distribution='r')\n"
    ))
}
```

**Group Balance Checks** (lines 362-398):
- Unbalanced design detection (>4:1 ratio)
- Very small group warnings (<5 per group)
- Complete pair validation for repeated measures

### 5. **Graceful Degradation with Fallback** ⭐⭐⭐⭐⭐

**Fallback Plot System** (lines 609-690):
```r
.plotFallback = function(prepared_data, analysis_info) {
    # Determine the most appropriate basic plot
    if (analysis_info$dep_type == "continuous" &&
        analysis_info$group_type == "continuous") {
        plot <- ggplot2::ggplot(...) +
            ggplot2::geom_point(alpha = 0.6) +
            ggplot2::geom_smooth(method = "lm", se = TRUE) +
            ggplot2::labs(
                title = paste("Basic Scatter Plot:", y_var, "vs", x_var),
                subtitle = "Generated using basic ggplot2 (variable type checks bypassed)"
            )
    }
    // ... 5 different fallback plot types
}
```

**Error Recovery** (lines 740-763):
```r
plot <- tryCatch({
    switch(analysis_info$plot_type,
        "independent_factor_continuous" = private$.plotBetweenStats(prepared_data),
        // ... other types
        NULL  # Will trigger fallback
    )
}, error = function(e) {
    message("Specialized plot function failed: ", conditionMessage(e))
    return(NULL)
})

# If specialized plot failed or returned NULL, use fallback
if (is.null(plot)) {
    message("Using fallback ggplot2 visualization")
    plot <- private$.plotFallback(prepared_data, analysis_info)
}
```

### 6. **Dynamic Plot Sizing** ⭐⭐⭐⭐

**Intelligent Size Adjustment** (lines 16-21, 1093-1124):
```r
.PLOT_DIMENSIONS = list(
    default = list(width = 800, height = 600),
    grouped_native = list(width_per_level = 400, height_per_level = 300,
                         max_width = 1600, max_height = 1200),
    grouped_manual = list(width = 1200, height_per_row = 450, max_height = 1400)
),

// In .plot():
if (!is.null(prepared_data$grvar)) {
    num_levels <- length(unique(grvar_col))
    uses_native_grouped <- (analysis_info$plot_type == "independent_factor_continuous") ||
                          (analysis_info$plot_type == "independent_continuous_continuous")

    if (uses_native_grouped) {
        new_width <- max(dims$default$width, min(num_levels * dims$grouped_native$width_per_level,
                                                  dims$grouped_native$max_width))
    } else {
        rows <- ceiling(num_levels / 2)
        new_width <- dims$grouped_manual$width
        new_height <- min(rows * dims$grouped_manual$height_per_row, dims$grouped_manual$max_height)
    }
    image$setSize(new_width, new_height)
}
```

### 7. **Good Documentation Practices** ⭐⭐⭐⭐

**Clear Variable Descriptions** (.a.yaml lines 48-71):
```yaml
- name: dep
  title: 'Outcome - Dependent Variable (y-axis)'
  type: Variable
  description:
      ui: Select the main outcome you want to analyze (e.g., tumor size,
          survival time, biomarker level, treatment response)
      R: >-
        The dependent variable (y-axis, 1st measurement). Can be continuous
        or categorical.
```

**Informative Welcome Message** (lines 441-454):
```r
todo <- glue::glue("
    <br>Welcome to ClinicoPath
    <br><br>
    This tool will help you generate plots based on variable types.
    <br><br>
    This function uses ggstatsplot and ggalluvial packages.
    Please cite jamovi and the packages as given below.
")
```

---

## CRITICAL ISSUES

### 1. **❌ NO TEST COVERAGE** - SEVERITY: HIGH

**Issue**: Zero unit tests found for statsplot2.

**Impact**:
- No automated regression testing
- Refactoring risk is high
- Edge cases may be untested
- Cannot verify assumption checking logic

**Location**: `tests/testthat/` directory

**Recommended Tests**:
```r
# tests/testthat/test-statsplot2.R
test_that("statsplot2 detects variable types correctly", {
    data <- data.frame(
        continuous = rnorm(100),
        factor = factor(rep(c("A", "B"), 50)),
        integer = 1:100
    )

    result <- ClinicoPath::statsplot2(
        data = data,
        dep = "continuous",
        group = "factor",
        direction = "independent"
    )

    expect_s3_class(result, "statsplot2Results")
})

test_that("statsplot2 handles small samples with warnings", {
    small_data <- data.frame(
        y = rnorm(15),
        x = factor(rep(c("A", "B", "C"), 5))
    )

    expect_warning({
        result <- ClinicoPath::statsplot2(
            data = small_data,
            dep = "y",
            group = "x"
        )
    }, regexp = "small sample", ignore.case = TRUE)
})

test_that("statsplot2 samples large datasets correctly", {
    large_data <- data.frame(
        y = rnorm(15000),
        x = factor(sample(c("A", "B"), 15000, replace = TRUE))
    )

    result <- ClinicoPath::statsplot2(
        data = large_data,
        dep = "y",
        group = "x",
        sampleLarge = TRUE
    )

    # Should have sampled to 5000 rows
    expect_true(TRUE)  # Verify through logs
})

test_that("statsplot2 validates data correctly", {
    # Empty data
    expect_error({
        ClinicoPath::statsplot2(
            data = data.frame(),
            dep = "y",
            group = "x"
        )
    })

    # Missing variables
    expect_error({
        ClinicoPath::statsplot2(
            data = mtcars,
            dep = "nonexistent",
            group = "cyl"
        )
    })
})

test_that("statsplot2 handles all plot type combinations", {
    # Factor vs Continuous (independent)
    # Continuous vs Continuous (independent)
    # Factor vs Factor (independent)
    # Factor vs Continuous (repeated)
    # Factor vs Factor (repeated) - alluvial
    # Test each combination
})

test_that("statsplot2 caching works correctly", {
    # Test cache invalidation
    # Test cache reuse
})
```

### 2. **⚠️ Missing Clinical Interpretation Features** - SEVERITY: MEDIUM

**Issue**: While the function generates **technical** explanations (lines 196-254, 257-294), it lacks:
- ✅ Clinical interpretation exists (lines 257-294) - **GOOD**
- ❌ No user-controlled visibility toggles for explanations
- ❌ No glossary of statistical terms
- ❌ No copy-ready report sentences
- ❌ No guided workflow/wizard mode
- ❌ No example interpretations in actual output
- ❌ No TR/EN language support

**Current Implementation** (lines 257-294):
```r
.generateClinicalInterpretation = function(analysis_info) {
    interpretation <- switch(analysis_info$plot_type,
        "independent_factor_continuous" = glue::glue(
            "Clinical Interpretation: This violin plot compares the
            distribution of {analysis_info$dep_var} between different
            {analysis_info$group_var} groups. Look for differences in
            medians (center lines) and spread (violin width)..."
        ),
        // ... more interpretations
    )
    return(paste0(interpretation, assumption_notes))
}
```

**Problem**: This is **ALWAYS** shown in `ExplanationMessage` (.r.yaml line 21), not user-controlled.

**Missing UI Controls** (.u.yaml should have):
```yaml
- type: CollapseBox
  label: Output Options
  collapsed: true
  children:
    - type: CheckBox
      name: showSummary
      label: "Show Summary (natural‑language)"
    - type: CheckBox
      name: showExplanations
      label: "Show Explanations (educational notes)"
    - type: CheckBox
      name: showGlossary
      label: "Show Statistical Glossary"
    - type: CheckBox
      name: showHowTo
      label: "Show Usage Guide"
```

**Missing .r.yaml Outputs**:
```yaml
items:
  - name: summary
    title: "Plain-Language Summary"
    type: Html
    visible: (showSummary)

  - name: glossary
    title: "Statistical Terms"
    type: Html
    visible: (showGlossary)

  - name: howto
    title: "How to Use This Analysis"
    type: Html
    visible: (showHowTo)

  - name: reportSentence
    title: "Copy-Ready Report"
    type: Preformatted
    visible: true
```

**Missing .b.R Logic**:
```r
.run = function() {
    // ... existing code ...

    # Only show summaries when user enables them
    if (isTRUE(self$options$showSummary)) {
        summary_text <- private$.generatePlainLanguageSummary(analysis_info)
        self$results$summary$setVisible(TRUE)
        self$results$summary$setContent(summary_text)
    } else {
        self$results$summary$setVisible(FALSE)
    }

    if (isTRUE(self$options$showGlossary)) {
        glossary_html <- private$.generateGlossary(analysis_info)
        self$results$glossary$setVisible(TRUE)
        self$results$glossary$setContent(glossary_html)
    } else {
        self$results$glossary$setVisible(FALSE)
    }

    # Always generate but keep concise
    report_sentence <- private$.generateReportSentence(analysis_info, stats_results)
    self$results$reportSentence$setContent(report_sentence)
}
```

### 3. **⚠️ Incomplete Package Dependency Handling** - SEVERITY: MEDIUM

**Issue** (lines 533-549):
```r
.init = function() {
    required_packages <- c("ggstatsplot", "ggalluvial", "dplyr",
                          "easyalluvial", "patchwork", "cowplot")
    missing_packages <- required_packages[!sapply(required_packages,
                        function(pkg) requireNamespace(pkg, quietly = TRUE))]
    if (length(missing_packages) > 0) {
        notice <- jmvcore::Notice$new(...)
        notice$setContent(glue::glue(
            "Optional packages missing for full functionality.\n",
            "• Missing: {paste(missing_packages, collapse = ', ')}\n",
            "• Install with: install.packages(c({paste(shQuote(missing_packages),
                                                      collapse = ', ')}))\n"
        ))
    }
}
```

**Problems**:
1. **Packages listed as "optional" but are actually required for core features**:
   - `ggstatsplot` - REQUIRED for all statistical plots (lines 777, 796, 815, 840, 858)
   - `ggalluvial` - REQUIRED for alluvial plots (line 888)
   - `dplyr` - REQUIRED for alluvial data prep (line 891)

2. **Missing graceful degradation**:
   - If `ggstatsplot` is missing, all plot types fail (no pure fallback)
   - Should use `.plotFallback()` when packages missing

3. **Warning is too gentle**:
   - Should be `ERROR` for truly required packages
   - Should be `WARNING` for truly optional ones (patchwork, cowplot, easyalluvial)

**Recommended Fix**:
```r
.init = function() {
    # REQUIRED packages (core functionality)
    required_core <- c("ggstatsplot", "ggplot2", "ggalluvial")
    missing_core <- required_core[!sapply(required_core,
                    function(pkg) requireNamespace(pkg, quietly = TRUE))]

    if (length(missing_core) > 0) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingCorePackages',
            type = jmvcore::NoticeType$ERROR
        )
        notice$setContent(glue::glue(
            "REQUIRED packages missing. Function cannot work without these.\n",
            "• Missing: {paste(missing_core, collapse = ', ')}\n",
            "• Install with: install.packages(c({paste(shQuote(missing_core),
                                                      collapse = ', ')}))\n",
            "• These are essential dependencies, not optional"
        ))
        self$results$insert(1, notice)
        return()  # Stop initialization
    }

    # OPTIONAL packages (enhanced functionality)
    optional_pkgs <- c("dplyr", "easyalluvial", "patchwork", "cowplot")
    missing_optional <- optional_pkgs[!sapply(optional_pkgs,
                        function(pkg) requireNamespace(pkg, quietly = TRUE))]

    if (length(missing_optional) > 0) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingOptionalPackages',
            type = jmvcore::NoticeType$WARNING
        )
        notice$setContent(glue::glue(
            "Optional packages missing. Some features will use fallbacks.\n",
            "• Missing: {paste(missing_optional, collapse = ', ')}\n",
            "• Affected features:\n",
            "  - dplyr: Alluvial plots will use base R aggregation\n",
            "  - patchwork/cowplot: Grouped plots will show only first group\n",
            "  - easyalluvial: Alluvial style 't2' will fallback to 't1'\n",
            "• Core functionality still works"
        ))
        self$results$insert(1, notice)
    }
}
```

### 4. **⚠️ Repeated Measures Validation Incomplete** - SEVERITY: MEDIUM

**Issue** (lines 400-421):
```r
# Repeated measures specific checks
if (analysis_info$direction == "repeated") {
    # Check for complete pairs
    if (is.factor(group_data) && length(levels(group_data)) == 2) {
        complete_pairs <- sum(complete.cases(dep_data, group_data))
        // ... warning about incomplete pairs
    }
}
```

**Problems**:
1. **Only checks for 2-level factors** - what about 3+ timepoints?
2. **Doesn't verify actual pairing structure** - needs subject ID
3. **No check for independence violation** - user might select wrong design
4. **No guidance on how to structure repeated measures data**

**Recommended Enhancement**:
```r
# Repeated measures specific checks
if (analysis_info$direction == "repeated") {
    # 1. Check for appropriate data structure
    if (is.factor(group_data)) {
        n_levels <- length(levels(group_data))
        n_obs <- nrow(data)

        # For true repeated measures, each subject should have n_levels observations
        # Expected structure: long format with implicit subject ordering
        # Or: wide format not supported - should guide user

        if (n_obs %% n_levels != 0) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'repeatedMeasuresStructure',
                type = jmvcore::NoticeType$STRONG_WARNING
            )
            notice$setContent(glue::glue(
                "Repeated measures data structure may be incorrect.\n",
                "• Factor '{analysis_info$group_var}' has {n_levels} levels\n",
                "• Dataset has {n_obs} observations\n",
                "• Expected: Number of observations divisible by {n_levels}\n",
                "• For repeated measures, ensure:\n",
                "  - Data is in long format\n",
                "  - Each subject has exactly {n_levels} rows\n",
                "  - Rows are ordered by subject\n",
                "• If groups are independent, change 'Study Design' to 'Independent'"
            ))
            notices <- append(notices, list(notice))
        }

        # 2. Check for complete cases within assumed groups
        expected_group_size <- n_obs / n_levels
        if (n_levels == 2) {
            complete_pairs <- sum(complete.cases(dep_data, group_data))
            // ... existing check
        } else if (n_levels > 2) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'multipleTimepoints',
                type = jmvcore::NoticeType$INFO
            )
            notice$setContent(glue::glue(
                "Multiple time points detected ({n_levels} levels).\n",
                "• Repeated measures ANOVA will be used\n",
                "• Ensure sphericity assumption is met\n",
                "• Consider Greenhouse-Geisser correction if violated"
            ))
            notices <- append(notices, list(notice))
        }
    }

    # 3. Misuse detection: continuous grouping variable in repeated measures
    if (analysis_info$group_type == "continuous") {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'repeatedMeasuresContinuous',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        notice$setContent(glue::glue(
            "Continuous grouping variable in repeated measures design.\n",
            "• Variable '{analysis_info$group_var}' is continuous\n",
            "• Repeated measures typically require categorical time/condition variables\n",
            "• Consider:\n",
            "  - Converting to factor (cut into time bins)\n",
            "  - Using 'Independent' design if observations aren't paired\n",
            "  - Using correlation/regression if both variables are continuous"
        ))
        notices <- append(notices, list(notice))
    }
}
```

---

## IMPROVEMENT OPPORTUNITIES

### 1. **Magic Numbers Should Be Constants** - CODE QUALITY

**Issue**: Multiple hardcoded thresholds scattered in code.

**Examples**:
- Line 305: `if (total_n < 30)`
- Line 327: `extreme_outliers <- sum(dep_data < (Q1 - 3.5 * IQR_val)...)`
- Line 345: `if (total_n < 100)`
- Line 366: `if (max_group / min_group > 4)`
- Line 383: `if (min_group < 5)`
- Line 704: `if (self$options$sampleLarge && original_nrow > 10000)`
- Line 706: `sample_size <- 5000`

**Recommended Refactoring**:
```r
# Add to top of class
.THRESHOLDS = list(
    sample_size = list(
        small_n = 30,
        normality_check_n = 100,
        large_dataset_threshold = 10000,
        large_dataset_sample_size = 5000
    ),
    outliers = list(
        iqr_multiplier = 3.5  # Beyond Q1 - 3.5*IQR or Q3 + 3.5*IQR
    ),
    groups = list(
        balance_ratio_warning = 4,   # Warn if largest/smallest > 4
        minimum_group_size = 5        # Warn if any group < 5
    )
),

# Then use throughout:
if (total_n < private$.THRESHOLDS$sample_size$small_n) {
    # ... warning
}

if (extreme_outliers > 0) {
    iqr_mult <- private$.THRESHOLDS$outliers$iqr_multiplier
    notice$setContent(glue::glue(
        "Extreme outliers detected in {analysis_info$dep_var}.\n",
        "• Found: {extreme_outliers} extreme outlier(s) (>{iqr_mult} IQR)\n"
    ))
}
```

**Benefits**:
- Single source of truth for thresholds
- Easy to tune for clinical context
- Self-documenting code
- Facilitates A/B testing of thresholds

### 2. **Extract Message Generation to Templates** - MAINTAINABILITY

**Issue**: Error/warning messages are embedded in code, making them:
- Hard to maintain
- Difficult to translate (i18n)
- Not reusable

**Current** (lines 41-49):
```r
notice$setContent(glue::glue(
    "Insufficient data for {plot_type}.\n",
    "• Variables: {y_var} by {x_var}\n",
    "• Found: {nrow(data)} observation(s)\n",
    "• Required: ≥2 observations\n",
    "• Check your data filtering."
))
```

**Recommended**:
```r
# Add message templates
.MESSAGES = list(
    errors = list(
        insufficient_data = "Insufficient data for {plot_type}.\n• Variables: {y_var} by {x_var}\n• Found: {n_obs} observation(s)\n• Required: ≥{required_obs} observations\n• Check your data filtering.",
        empty_dataset = "No data available for analysis.\n• Variables selected: dependent='{dep_name}', grouping='{group_name}'\n• Check data loading and variable selection\n• Verify dataset is not empty",
        // ... more templates
    ),
    warnings = list(
        small_sample = "Small sample size detected (n={total_n}).\n• Nonparametric approaches recommended for n<{threshold}\n• Consider robust statistical methods\n• Results may have reduced statistical power",
        // ... more
    ),
    info = list(
        analysis_complete = "Analysis completed successfully.\n• Plot type: {plot_type}\n• Observations: {n_used:,} of {n_total:,}\n• Statistical approach: {distribution}\n• Study design: {direction}",
        // ... more
    )
),

# Helper function
.formatMessage = function(template_key, ..., type = "errors") {
    template <- private$.MESSAGES[[type]][[template_key]]
    if (is.null(template)) {
        stop(glue::glue("Message template '{template_key}' not found in type '{type}'"))
    }
    return(glue::glue(template, ...))
},

# Usage
notice$setContent(private$.formatMessage(
    "insufficient_data",
    plot_type = plot_type,
    y_var = y_var,
    x_var = x_var,
    n_obs = nrow(data),
    required_obs = 2
))
```

**Benefits for i18n**:
```r
# Future enhancement: language support
.MESSAGES_TR = list(  # Turkish translations
    errors = list(
        insufficient_data = "Yetersiz veri: {plot_type}.\n• Değişkenler: {y_var} ve {x_var}\n• Bulunan: {n_obs} gözlem\n• Gerekli: ≥{required_obs} gözlem\n• Veri filtrelemenizi kontrol edin.",
        // ...
    )
),

.formatMessage = function(template_key, ..., type = "errors", lang = "EN") {
    message_set <- if (lang == "TR") private$.MESSAGES_TR else private$.MESSAGES
    template <- message_set[[type]][[template_key]]
    return(glue::glue(template, ...))
}
```

### 3. **Improve Grouped Plot Error Handling** - ROBUSTNESS

**Issue** (lines 984-990):
```r
empty_levels <- sapply(grvar_levels, function(level) {
    sum(grvar_col == level, na.rm = TRUE) == 0
})
if (any(empty_levels)) {
    warning(glue::glue("Some levels of '{prepared_data$grvar}' have no data..."))
    grvar_levels <- grvar_levels[!empty_levels]
}
```

**Problems**:
1. `warning()` is printed to console, not shown in UI
2. Should use `Notice` system for consistency
3. Should track which levels were skipped
4. Doesn't handle case where ALL levels are empty

**Recommended**:
```r
# Check for empty levels after filtering
grvar_col <- prepared_data$data[[prepared_data$grvar]]
grvar_levels <- unique(grvar_col[!is.na(grvar_col)])

level_counts <- sapply(grvar_levels, function(level) {
    sum(grvar_col == level, na.rm = TRUE)
})

empty_levels <- level_counts == 0
if (any(empty_levels)) {
    skipped_levels <- grvar_levels[empty_levels]

    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'emptyGroupLevels',
        type = jmvcore::NoticeType$WARNING
    )
    notice$setContent(glue::glue(
        "Some '{prepared_data$grvar}' levels have no data after filtering.\n",
        "• Skipped levels: {paste(skipped_levels, collapse = ', ')}\n",
        "• Remaining levels: {paste(grvar_levels[!empty_levels], collapse = ', ')}\n",
        "• Check data filtering or variable selection"
    ))
    self$results$insert(2, notice)

    grvar_levels <- grvar_levels[!empty_levels]
}

# Handle case where ALL levels are empty
if (length(grvar_levels) == 0) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'allGroupLevelsEmpty',
        type = jmvcore::NoticeType$ERROR
    )
    notice$setContent(glue::glue(
        "All levels of grouping variable '{prepared_data$grvar}' are empty.\n",
        "• Check: Missing values, data filtering, variable type\n",
        "• Cannot create grouped plots without data"
    ))
    self$results$insert(1, notice)
    return(NULL)
}
```

### 4. **Add Progress Indicators for Large Operations** - UX

**Issue**: Long-running operations provide no feedback to user.

**Current**: Silent checkpoint calls don't inform user of progress.

**Recommended Enhancement**:
```r
.plotGrouped = function(analysis_info, prepared_data) {
    grvar_levels <- unique(prepared_data$data[[prepared_data$grvar]])
    n_levels <- length(grvar_levels)

    # Inform user about multi-plot generation
    if (n_levels > 3) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'generatingGroupedPlots',
            type = jmvcore::NoticeType$INFO
        )
        notice$setContent(glue::glue(
            "Generating {n_levels} grouped plots...\n",
            "• This may take a moment for large datasets\n",
            "• Each plot is created separately and combined\n",
            "• Progress: Processing groups..."
        ))
        self$results$insert(999, notice)
    }

    # ... plot generation loop

    # Update completion notice
    if (n_levels > 3 && length(plot_list) > 0) {
        complete_notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'groupedPlotsComplete',
            type = jmvcore::NoticeType$INFO
        )
        complete_notice$setContent(glue::glue(
            "Grouped plots completed.\n",
            "• Generated: {length(plot_list)} plots\n",
            "• Layout: {ceiling(length(plot_list)/2)} rows × 2 columns\n",
            "• Zoom plot to see details"
        ))
        self$results$insert(999, complete_notice)
    }

    // ... return plot
}
```

### 5. **Performance Optimization for Assumption Checks** - PERFORMANCE

**Issue**: Assumption checks run on full dataset even for large samples.

**Current** (lines 304, 323):
```r
total_n <- sum(!is.na(dep_data) & !is.na(group_data))
// ... many statistical calculations on full data
Q1 <- quantile(dep_data, 0.25, na.rm = TRUE)
Q3 <- quantile(dep_data, 0.75, na.rm = TRUE)
```

**Optimization for Large Datasets**:
```r
.checkAssumptions = function(analysis_info, data) {
    dep_data <- data[[analysis_info$dep_var]]
    group_data <- data[[analysis_info$group_var]]

    # For very large datasets, sample for assumption checks
    # Full data stats aren't necessary for detecting issues
    n_total <- length(dep_data)
    if (n_total > 50000) {
        # Sample 10,000 for assumption validation
        sample_idx <- sample(n_total, min(10000, n_total))
        dep_data_check <- dep_data[sample_idx]
        group_data_check <- group_data[sample_idx]

        sampled_notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'assumptionCheckSampled',
            type = jmvcore::NoticeType$INFO
        )
        sampled_notice$setContent(glue::glue(
            "Assumption checks performed on random sample.\n",
            "• Full dataset: {n_total:,} observations\n",
            "• Sample for checks: {length(dep_data_check):,} observations\n",
            "• This improves performance without affecting validity"
        ))
        notices <- append(notices, list(sampled_notice))
    } else {
        dep_data_check <- dep_data
        group_data_check <- group_data
    }

    # Run checks on potentially sampled data
    total_n <- sum(!is.na(dep_data_check) & !is.na(group_data_check))
    // ... rest of checks use dep_data_check, group_data_check
}
```

---

## ENHANCEMENT SUGGESTIONS

### 1. **Add Clinician-Friendly Features** - UX ENHANCEMENT

#### A. Plain-Language Summary (TOGGLE-CONTROLLED)

**Add to .a.yaml**:
```yaml
- name: showSummary
  title: Show Plain-Language Summary
  type: Bool
  default: false
  description:
      ui: Display analysis summary in plain language suitable for reports
      R: If TRUE, shows a non-technical summary of the analysis and results
```

**Add to .r.yaml**:
```yaml
- name: summary
  title: "Plain-Language Summary"
  type: Html
  visible: (showSummary)
  clearWith:
    - dep
    - group
    - direction
    - distribution
```

**Add to .b.R**:
```r
.generatePlainLanguageSummary = function(analysis_info, stats_results = NULL) {
    # Only generate when user enables this option
    if (!isTRUE(self$options$showSummary)) {
        self$results$summary$setVisible(FALSE)
        return()
    }

    plot_name <- switch(analysis_info$plot_type,
        "independent_factor_continuous" = "violin plot comparing groups",
        "independent_continuous_continuous" = "scatter plot showing association",
        "independent_factor_factor" = "bar chart comparing categories",
        "repeated_factor_continuous" = "paired comparison plot",
        "repeated_factor_factor" = "flow diagram showing changes",
        "basic visualization"
    )

    approach_name <- switch(analysis_info$distribution,
        "p" = "parametric tests (assumes normal distribution)",
        "np" = "nonparametric tests (no distribution assumptions)",
        "r" = "robust methods (resistant to outliers)",
        "bf" = "Bayesian analysis (strength of evidence)",
        "standard statistical tests"
    )

    summary_html <- glue::glue("
        <div style='padding: 15px; background: #e8f5e9; border-left: 4px solid #4caf50;'>
        <h4 style='margin-top: 0; color: #2e7d32;'>Plain-Language Summary</h4>
        <p><strong>Analysis:</strong> We created a {plot_name} to examine the relationship
        between <strong>{analysis_info$dep_var}</strong> (outcome) and
        <strong>{analysis_info$group_var}</strong> (groups/predictor).</p>

        <p><strong>Method:</strong> {ifelse(analysis_info$direction == 'independent',
            'Independent groups were compared',
            'Repeated measurements from the same subjects were compared')}
        using {approach_name}.</p>

        <p><strong>Sample:</strong> {nrow(self$data):,} observations were analyzed.</p>

        <p><strong>Interpretation:</strong> {private$.generateClinicalInterpretation(analysis_info)}</p>
        </div>
    ")

    self$results$summary$setVisible(TRUE)
    self$results$summary$setContent(summary_html)
},

# Call in .run()
.run = function() {
    // ... existing code ...

    # Generate summaries only if enabled
    private$.generatePlainLanguageSummary(analysis_info)
}
```

#### B. Statistical Glossary (TOGGLE-CONTROLLED)

**Add to .a.yaml**:
```yaml
- name: showGlossary
  title: Show Statistical Glossary
  type: Bool
  default: false
  description:
      ui: Display definitions of statistical terms used in this analysis
      R: If TRUE, shows a glossary of statistical terms and assumptions
```

**Add to .r.yaml**:
```yaml
- name: glossary
  title: "Statistical Terms Guide"
  type: Html
  visible: (showGlossary)
```

**Add to .b.R**:
```r
.generateGlossary = function(analysis_info) {
    if (!isTRUE(self$options$showGlossary)) {
        self$results$glossary$setVisible(FALSE)
        return()
    }

    # Generate contextual glossary based on analysis type
    terms <- list()

    # Always include basics
    terms$mean <- "Average value of all observations"
    terms$median <- "Middle value when data is sorted (50th percentile)"
    terms$sd <- "Standard Deviation: How spread out the data is around the mean"

    # Add terms based on plot type
    if (analysis_info$plot_type == "independent_factor_continuous") {
        terms$violin_plot <- "Shows distribution shape, median, and quartiles for each group"
        terms$quartiles <- "Q1 (25th percentile), Q2 (median), Q3 (75th percentile)"
        if (analysis_info$distribution == "p") {
            terms$anova <- "Analysis of Variance: Tests if group means differ significantly"
            terms$post_hoc <- "Pairwise comparisons to find which specific groups differ"
        }
    }

    if (analysis_info$distribution == "p") {
        terms$parametric <- "Assumes data follows normal (bell-curve) distribution"
        terms$p_value <- "Probability that results occurred by chance. <0.05 typically considered significant"
        terms$confidence_interval <- "Range where we're 95% confident the true value lies"
    } else if (analysis_info$distribution == "np") {
        terms$nonparametric <- "No assumption about data distribution. Uses ranks instead of raw values"
        terms$mann_whitney <- "Nonparametric test comparing two independent groups"
        terms$wilcoxon <- "Nonparametric test for paired/repeated measures"
    }

    glossary_html <- "<div style='padding: 15px; background: #fff3e0; border-left: 4px solid #ff9800;'>\n"
    glossary_html <- paste0(glossary_html, "<h4 style='margin-top: 0; color: #e65100;'>Statistical Terms</h4>\n")
    glossary_html <- paste0(glossary_html, "<dl>\n")

    for (term in names(terms)) {
        glossary_html <- paste0(glossary_html,
            "<dt><strong>", tools::toTitleCase(gsub("_", " ", term)), "</strong></dt>\n",
            "<dd>", terms[[term]], "</dd>\n")
    }

    glossary_html <- paste0(glossary_html, "</dl></div>")

    self$results$glossary$setVisible(TRUE)
    self$results$glossary$setContent(glossary_html)
}
```

#### C. Copy-Ready Report Sentence

**Add to .r.yaml**:
```yaml
- name: reportSentence
  title: "Report Sentence (Copy-Ready)"
  type: Preformatted
  visible: true
```

**Add to .b.R**:
```r
.generateReportSentence = function(analysis_info) {
    design_text <- ifelse(analysis_info$direction == "independent",
        "between independent groups",
        "within subjects (repeated measures)")

    method_text <- switch(analysis_info$distribution,
        "p" = "parametric statistical tests",
        "np" = "nonparametric statistical tests",
        "r" = "robust statistical methods",
        "bf" = "Bayesian analysis",
        "standard statistical methods"
    )

    plot_text <- switch(analysis_info$plot_type,
        "independent_factor_continuous" = "violin plots with statistical comparisons",
        "independent_continuous_continuous" = "scatter plot with correlation analysis",
        "independent_factor_factor" = "bar chart with chi-square analysis",
        "repeated_factor_continuous" = "paired violin plots with paired t-test",
        "repeated_factor_factor" = "alluvial diagram showing transitions",
        "graphical visualization"
    )

    sentence <- glue::glue(
        "We examined the relationship between {analysis_info$dep_var} and ",
        "{analysis_info$group_var} using {plot_text}. The comparison was made ",
        "{design_text} using {method_text}. ",
        "Data from {nrow(self$data):,} observations were analyzed."
    )

    # Add conditional sentence about grouping variable
    if (!is.null(analysis_info$grvar)) {
        sentence <- paste0(sentence, glue::glue(
            " Analyses were stratified by {analysis_info$grvar}."
        ))
    }

    self$results$reportSentence$setContent(as.character(sentence))
}
```

### 2. **Add Misuse Detection Warnings** - QUALITY

**Scenarios to Detect**:

1. **Independent design selected but data structure suggests pairing**
2. **Repeated measures selected but groups appear independent**
3. **Too many factor levels for practical comparison**
4. **Continuous variable treated as categorical**

**Implementation**:
```r
.detectMisuse = function(analysis_info, data) {
    misuse_notices <- list()

    # 1. Check for suspiciously perfect balance (suggests repeated measures)
    if (analysis_info$direction == "independent" &&
        is.factor(data[[analysis_info$group_var]])) {

        group_counts <- table(data[[analysis_info$group_var]])
        if (length(unique(group_counts)) == 1) {
            # Perfect balance - might be repeated measures
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'possibleRepeatedMeasures',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent(glue::glue(
                "Perfectly balanced groups detected.\n",
                "• All groups have exactly {group_counts[1]} observations\n",
                "• This pattern is common in repeated measures data\n",
                "• If same subjects measured multiple times, change 'Study Design' to 'Repeated Measures'\n",
                "• If groups are truly independent, ignore this warning"
            ))
            misuse_notices <- append(misuse_notices, list(notice))
        }
    }

    # 2. Check for too many factor levels
    if (is.factor(data[[analysis_info$group_var]])) {
        n_levels <- length(levels(data[[analysis_info$group_var]]))
        if (n_levels > 10) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'tooManyLevels',
                type = jmvcore::NoticeType$STRONG_WARNING
            )
            notice$setContent(glue::glue(
                "Many factor levels detected in '{analysis_info$group_var}'.\n",
                "• Number of levels: {n_levels}\n",
                "• Plots with >10 groups are difficult to interpret\n",
                "• Consider:\n",
                "  - Combining related categories\n",
                "  - Treating as continuous if levels are ordered\n",
                "  - Using different grouping variable"
            ))
            misuse_notices <- append(misuse_notices, list(notice))
        }
    }

    # 3. Check for continuous variable with few unique values (should be factor?)
    if (analysis_info$group_type == "continuous") {
        unique_vals <- length(unique(data[[analysis_info$group_var]]))
        if (unique_vals <= 5) {
            notice <- jmvcore::Notice$new(
                options = self$options,
                name = 'lowCardinality',
                type = jmvcore::NoticeType$WARNING
            )
            notice$setContent(glue::glue(
                "Continuous variable has few unique values.\n",
                "• Variable: '{analysis_info$group_var}'\n",
                "• Unique values: {unique_vals}\n",
                "• Values: {paste(sort(unique(data[[analysis_info$group_var]])), collapse = ', ')}\n",
                "• Consider converting to factor if these represent categories\n",
                "• Otherwise, correlation/regression may be more appropriate"
            ))
            misuse_notices <- append(misuse_notices, list(notice))
        }
    }

    return(misuse_notices)
}

# Call in .run()
.run = function() {
    // ... after assumption checks

    # Check for common misuse patterns
    misuse_notices <- private$.detectMisuse(analysis_info, self$data)
    if (length(misuse_notices) > 0) {
        for (notice in misuse_notices) {
            self$results$insert(2, notice)
        }
    }
}
```

### 3. **Add How-To Guide** - DOCUMENTATION

**Add to .a.yaml**:
```yaml
- name: showHowTo
  title: Show Usage Guide
  type: Bool
  default: false
```

**Add to .r.yaml**:
```yaml
- name: howto
  title: "How to Use This Analysis"
  type: Html
  visible: (showHowTo)
```

**Add to .b.R**:
```r
.generateHowToGuide = function() {
    if (!isTRUE(self$options$showHowTo)) {
        self$results$howto$setVisible(FALSE)
        return()
    }

    guide_html <- "
    <div style='padding: 15px; background: #e3f2fd; border-left: 4px solid #2196f3;'>
    <h4 style='margin-top: 0; color: #1565c0;'>Quick Start Guide</h4>

    <h5>Step 1: Select Variables</h5>
    <ul>
        <li><strong>Dependent Variable (y-axis):</strong> The outcome you're measuring
            <br><em>Examples: tumor size, survival time, biomarker level, treatment response</em></li>
        <li><strong>Grouping Variable (x-axis):</strong> The groups you're comparing or predictor
            <br><em>Examples: treatment vs control, tumor grades (G1/G2/G3), age</em></li>
        <li><strong>Split By (optional):</strong> Create separate plots for subgroups
            <br><em>Examples: gender, hospital site, disease stage</em></li>
    </ul>

    <h5>Step 2: Choose Study Design</h5>
    <ul>
        <li><strong>Independent:</strong> Comparing different subjects/samples
            <br><em>Example: Comparing tumor sizes between male and female patients</em></li>
        <li><strong>Repeated Measures:</strong> Same subjects measured multiple times
            <br><em>Example: Tumor size before and after treatment in same patients</em></li>
    </ul>

    <h5>Step 3: Select Statistical Approach</h5>
    <ul>
        <li><strong>Parametric:</strong> Use for normally distributed continuous data
            <br><em>When: Bell-shaped distribution, no major outliers</em></li>
        <li><strong>Nonparametric:</strong> Use for skewed data or ordinal scales
            <br><em>When: Highly skewed data, ranked data, small samples</em></li>
        <li><strong>Robust:</strong> Resistant to outliers
            <br><em>When: Continuous data with extreme values</em></li>
        <li><strong>Bayesian:</strong> Quantifies strength of evidence
            <br><em>When: Want to know how much evidence supports hypothesis</em></li>
    </ul>

    <h5>Step 4: Interpret Results</h5>
    <p>Look for:</p>
    <ul>
        <li>Differences in central tendency (means/medians)</li>
        <li>Overlap in distributions (violin plots)</li>
        <li>Statistical significance (p-values, if shown)</li>
        <li>Clinical significance (effect size, practical importance)</li>
    </ul>

    <p><strong>Tip:</strong> Enable 'Show Plain-Language Summary' and 'Show Statistical Glossary'
    for additional help interpreting your results.</p>
    </div>
    "

    self$results$howto$setVisible(TRUE)
    self$results$howto$setContent(guide_html)
}
```

---

## ACTION ITEMS

### High Priority (Must Do)
- [ ] **Create comprehensive test suite** (test-statsplot2.R)
  - Variable type detection tests
  - Validation error handling tests
  - All plot type combination tests
  - Assumption checking tests
  - Caching behavior tests
  - Large dataset sampling tests

- [ ] **Add user-controlled visibility toggles** for explanatory content
  - Add `showSummary`, `showGlossary`, `showHowTo` options to .a.yaml
  - Add corresponding Html items to .r.yaml with `visible:` rules
  - Implement `.generatePlainLanguageSummary()`, `.generateGlossary()`, `.generateHowToGuide()`
  - Ensure content only renders when user enables it (CRITICAL for UX)

- [ ] **Fix package dependency handling**
  - Separate REQUIRED vs OPTIONAL packages
  - Use ERROR notices for missing required packages
  - Implement graceful degradation when optional packages missing
  - Test with minimal package set

- [ ] **Improve repeated measures validation**
  - Add data structure checks (balanced observations)
  - Detect independence violations
  - Guide users on proper data format
  - Warn about continuous variables in RM design

### Medium Priority (Should Do)
- [ ] **Extract magic numbers to constants** (.THRESHOLDS)
- [ ] **Create message templates** (.MESSAGES, .MESSAGES_TR for i18n)
- [ ] **Enhance grouped plot error handling** (use Notice system)
- [ ] **Add misuse detection warnings**
- [ ] **Implement copy-ready report sentence generation**
- [ ] **Add progress indicators** for multi-level grouped plots

### Low Priority (Nice to Have)
- [ ] **Performance optimization**: Sample for assumption checks on huge datasets (>50k)
- [ ] **Add language support** (TR/EN) using message templates
- [ ] **Create guided wizard mode** (step-by-step UI flow)
- [ ] **Add clinical presets** (common analysis configurations)
- [ ] **Implement color-blind safe palette defaults**

---

## SPECIFIC RECOMMENDATIONS

### Architecture Improvement: Modular Message System

**Current Problem**: Messages embedded throughout code
**Recommendation**: Central message registry with i18n support

```r
# Add to class definition
.MESSAGE_REGISTRY = list(
    EN = list(
        errors = list(
            insufficient_data = list(
                template = "Insufficient data for {plot_type}.\n• Variables: {y_var} by {x_var}\n• Found: {n_obs} observation(s)\n• Required: ≥{required_obs} observations\n• Check your data filtering.",
                params = c("plot_type", "y_var", "x_var", "n_obs", "required_obs")
            ),
            // ... more error messages
        ),
        warnings = list(
            small_sample = list(
                template = "Small sample size detected (n={total_n}).\n• Nonparametric approaches recommended for n<{threshold}\n• Consider robust statistical methods\n• Results may have reduced statistical power",
                params = c("total_n", "threshold")
            ),
            // ... more warnings
        )
    ),
    TR = list(  # Turkish translations
        errors = list(
            insufficient_data = list(
                template = "Yetersiz veri: {plot_type}.\n• Değişkenler: {y_var} ve {x_var}\n• Bulunan: {n_obs} gözlem\n• Gerekli: ≥{required_obs} gözlem",
                params = c("plot_type", "y_var", "x_var", "n_obs", "required_obs")
            )
        )
    }
),

# Message generation helper
.getMessage = function(key, type = "errors", lang = "EN", ...) {
    msg_data <- private$.MESSAGE_REGISTRY[[lang]][[type]][[key]]
    if (is.null(msg_data)) {
        stop(glue::glue("Message '{key}' not found in {type}/{lang}"))
    }

    # Validate all required params are provided
    provided_params <- names(list(...))
    missing_params <- setdiff(msg_data$params, provided_params)
    if (length(missing_params) > 0) {
        stop(glue::glue("Missing required params for message '{key}': {paste(missing_params, collapse=', ')}"))
    }

    return(glue::glue(msg_data$template, ...))
}
```

### Performance: Lazy Evaluation of Expensive Checks

```r
.checkAssumptions = function(analysis_info, data) {
    notices <- list()

    # Quick checks first (cheap operations)
    dep_data <- data[[analysis_info$dep_var]]
    group_data <- data[[analysis_info$group_var]]
    total_n <- sum(!is.na(dep_data) & !is.na(group_data))

    # Sample size check (free)
    if (total_n < private$.THRESHOLDS$sample_size$small_n) {
        notices <- append(notices, list(private$.createSmallSampleNotice(total_n)))
    }

    # Only run expensive checks if parametric approach is selected
    if (analysis_info$distribution == "p" && analysis_info$dep_type == "continuous") {
        # Checkpoint before expensive outlier detection
        private$.checkpoint(flush = FALSE)

        # Outlier detection (requires quantile calculations)
        if (length(dep_data) > 4) {
            outlier_notice <- private$.checkOutliers(dep_data, analysis_info$dep_var)
            if (!is.null(outlier_notice)) {
                notices <- append(notices, list(outlier_notice))
            }
        }
    }

    # Group-specific checks (conditional on data type)
    if (analysis_info$direction == "independent" && is.factor(group_data)) {
        balance_notices <- private$.checkGroupBalance(group_data, analysis_info$group_var)
        notices <- append(notices, balance_notices)
    }

    return(notices)
}
```

### Error Handling: Better Fallback Chain

```r
.generatePlot = function(analysis_info, prepared_data) {
    # Try native grouped function first (if applicable)
    if (!is.null(prepared_data$grvar)) {
        plot <- tryCatch({
            private$.plotGrouped(analysis_info, prepared_data)
        }, error = function(e) {
            message("Grouped plot failed: ", conditionMessage(e))
            NULL
        })
        if (!is.null(plot)) return(plot)
    }

    # Try specialized ggstatsplot function
    plot <- tryCatch({
        private$.plotSpecialized(analysis_info, prepared_data)
    }, error = function(e) {
        message("Specialized plot failed: ", conditionMessage(e))
        NULL
    })
    if (!is.null(plot)) return(plot)

    # Try basic ggplot2 fallback
    plot <- tryCatch({
        private$.plotFallback(prepared_data, analysis_info)
    }, error = function(e) {
        message("Fallback plot failed: ", conditionMessage(e))
        NULL
    })
    if (!is.null(plot)) return(plot)

    # Last resort: ultra-basic plot
    plot <- private$.plotUltraBasic(prepared_data)
    return(plot)
}

.plotUltraBasic = function(prepared_data) {
    # Absolute last resort - just show the data exists
    data <- prepared_data$data
    x_var <- prepared_data$group
    y_var <- prepared_data$dep

    plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_var, y = y_var)) +
        ggplot2::geom_point(alpha = 0.3, color = "steelblue") +
        ggplot2::labs(
            title = paste("Basic Data View:", y_var, "vs", x_var),
            subtitle = "All specialized plot functions failed. Contact support if this persists.",
            x = x_var,
            y = y_var
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(color = "red", size = 10))

    return(plot)
}
```

---

## CLINICIAN-FRIENDLY IMPROVEMENTS CHECKLIST

| Feature | Status | Priority | Notes |
|---------|--------|----------|-------|
| ✅ Plain-language labels/tooltips | PARTIAL | HIGH | .a.yaml has good `ui:` descriptions, expand |
| ❌ Micro-explanations per option | NO | HIGH | Add hover tooltips with examples |
| ✅ Clinical interpretation present | YES | ✅ DONE | Lines 257-294, excellent |
| ❌ User-controlled visibility | NO | **CRITICAL** | Must add toggles for summary/glossary/howto |
| ❌ Glossary entries present | NO | HIGH | Implement `.generateGlossary()` |
| ❌ Guided flow (wizard) | NO | LOW | Future enhancement |
| ✅ Misuse warnings/guards | PARTIAL | MEDIUM | Good assumption checks, add design misuse |
| ❌ Example interpretations in outputs | NO | MEDIUM | Add to summary when enabled |
| ❌ Report sentence templates | NO | HIGH | Implement `.generateReportSentence()` |
| ⚠️ Sensible defaults | MIXED | MEDIUM | Good defaults, document "Recommended" |
| ❌ Accessibility (CB-safe, font) | NO | MEDIUM | Need palette configuration |
| ❌ i18n (TR/EN) coverage | NO | LOW | Message template system enables this |
| ❌ Natural-language summary | NO | HIGH | Implement with toggle control |
| ⚠️ About/How-to section | PARTIAL | HIGH | Welcome message exists, expand to full guide |
| ⚠️ Caveats & assumptions | PARTIAL | MEDIUM | Assumption checks good, add caveats panel |
| ❌ Guidance links/examples | NO | LOW | Add to documentation |

### Critical Gap: USER-CONTROLLED VISIBILITY
**The function already generates clinical interpretations but shows them ALWAYS.** This violates the prompt requirement:

> Natural‑language summaries and educational/explanatory outputs must render **only when** the corresponding UI options are enabled by the user (see the `.u.yaml` checkboxes below). Keep these sections hidden by default unless selected.

**Fix Required**:
1. Add `.a.yaml` options: `showSummary`, `showGlossary`, `showHowTo` (all `default: false`)
2. Add `.r.yaml` items with `visible: (optionName)` rules
3. Modify `.b.R` to check `if (isTRUE(self$options$showSummary))` before populating
4. Move current always-shown interpretation to ExplanationMessage (technical) vs Summary (plain-language, toggle-controlled)

---

## SUMMARY SCORECARD

| Category | Score | Rationale |
|----------|-------|-----------|
| **Code Quality** | 4/5 | Excellent architecture, clear separation, good patterns. -1 for magic numbers |
| **Error Handling** | 5/5 | Outstanding multi-layer validation, contextual messages, graceful degradation |
| **Performance** | 4/5 | Good caching, smart sampling. Could optimize assumption checks for huge datasets |
| **Maintainability** | 4/5 | Well-organized, clear methods. -1 for embedded messages (i18n issue) |
| **Documentation** | 4/5 | Good inline docs, excellent .a.yaml descriptions. Missing comprehensive tests |
| **User Experience** | 3/5 | Good technical UX. Missing toggle-controlled summaries, glossary, wizard |
| **Clinical Friendliness** | 2/5 | Has interpretations but lacks toggles, glossary, report sentences, i18n |
| **Test Coverage** | 0/5 | **NO TESTS FOUND** - critical gap |
| **Accessibility** | 2/5 | Basic support. Missing CB-safe palettes, font options, full i18n |

**Overall: 4 Stars (⭐⭐⭐⭐)**
**With Improvements: Could be 5 Stars (⭐⭐⭐⭐⭐)**

---

## FINAL RECOMMENDATIONS

### Must Implement (For 5-Star Rating):
1. **Add comprehensive test suite** - Highest priority for maintainability
2. **Implement toggle-controlled explanatory content** - Critical UX requirement from prompt
3. **Fix package dependency validation** - Prevents runtime errors
4. **Add glossary and plain-language summary** - Core clinical-friendliness feature

### Should Implement (For Production Readiness):
5. **Message template system** - Enables i18n and easier maintenance
6. **Extract magic numbers to constants** - Improves code quality
7. **Enhanced repeated measures validation** - Prevents common user errors
8. **Copy-ready report sentence** - High-value clinical feature

### Nice to Have (Future Enhancements):
9. **Guided wizard mode** - Improved first-time user experience
10. **TR/EN language support** - Broader accessibility
11. **Color-blind safe palettes** - Better accessibility
12. **Performance optimizations** - For very large datasets (>100k rows)

**Status**: Excellent foundation, ready for production **after** implementing critical toggles for educational content and adding test coverage.
