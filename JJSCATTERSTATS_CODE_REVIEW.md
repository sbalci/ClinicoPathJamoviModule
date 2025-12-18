# jjscatterstats Function - Comprehensive Code Review

**Date**: 2025-12-16
**Reviewer**: Claude Code (Sonnet 4.5)
**Function**: jjscatterstats (Scatter Plot with Correlation Analysis)
**Status**: ⚠️ **NOT READY FOR RELEASE** - Critical bugs and missing features

---

## Overall Quality Ratings

| Aspect | Rating | Notes |
|--------|--------|-------|
| **Overall Quality** | ⭐⭐ (2/5) | Critical bugs prevent release |
| **Code Maintainability** | ⭐⭐⭐ (3/5) | Well-structured but has dead code |
| **Performance** | ⭐⭐⭐ (3/5) | Adequate but inefficient in places |
| **User Experience** | ⭐⭐ (2/5) | Missing clinician-friendly features |
| **Statistical Correctness** | ⭐⭐⭐⭐ (4/5) | Core statistics correct, implementation issues |
| **Clinical Readiness** | ⭐ (1/5) | Lacks validation, guidance, safety features |

**RELEASE STATUS**: ❌ **NOT READY** - Contains showstopper bugs and critical gaps

---

## Executive Summary

The `jjscatterstats` function provides scatter plot visualization with correlation analysis using ggstatsplot and ggpubr packages. While the core statistical methods are sound (delegating to well-tested packages), the implementation contains **critical bugs** that will cause runtime crashes, lacks proper error handling using jamovi's Notices API, and is missing essential clinician-friendly features.

**Critical Issues Requiring Immediate Fix**:
1. 🔴 **SHOWSTOPPER BUG**: Dead code references removed `warnings` output (will crash application)
2. 🔴 **NO VALIDATION**: Zero data quality checks (sample size, missing data, outliers)
3. 🔴 **NO NOTICES**: Uses legacy `stop()` instead of jamovi Notices API
4. 🟡 **POOR UX**: Missing interpretation guidance, report summaries, educational content

---

## CRITICAL ISSUES (Must Fix Before Release)

### 🔴 Issue 1: SHOWSTOPPER - Dead Code References Removed Output

**Severity**: CRITICAL (Application Crash)
**Location**: [R/jjscatterstats.b.R:521-531, 551-552](R/jjscatterstats.b.R#L521-L552)

**Problem**:
The enhanced plot method (`.plot3`) contains code that writes warnings to `self$results$warnings`, but this output was **removed** in recent fixes (see JJSCATTERSTATS_FIXES_APPLIED.md). This creates a runtime error when users select robust or Bayesian correlation with enhanced plot aesthetics.

**Affected Code**:
```r
# Lines 521-531 - WILL CRASH
if (!is.null(warning_msg)) {
    current_warnings <- self$results$warnings$state  # ❌ warnings doesn't exist
    if (is.null(current_warnings)) {
        current_warnings <- ""
    }
    new_warning <- paste0(
        current_warnings,
        "<p style='color:#856404;'>", warning_msg, "</p>"
    )
    self$results$warnings$setContent(new_warning)      # ❌ CRASH
    self$results$warnings$setVisible(TRUE)             # ❌ CRASH
}

# Lines 551-552 - WILL CRASH
warning_msg <- paste0("⚠️ Correlation calculation failed: ", e$message)
self$results$warnings$setContent(warning_msg)          # ❌ CRASH
self$results$warnings$setVisible(TRUE)                 # ❌ CRASH
```

**User Impact**:
When a user:
1. Selects robust or Bayesian correlation
2. Adds color/size/shape/alpha/label variables (triggers plot3)
3. Runs analysis

Result: **Application crashes** with error about undefined results object.

**Fix Required**:
Replace with Notices API:
```r
# CORRECT Implementation
if (!is.null(warning_msg)) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$WARNING,
        message = warning_msg
    )
    self$results$insert(100, notice)  # Position 100 for warnings
}
```

---

### 🔴 Issue 2: No Data Validation

**Severity**: CRITICAL (Clinical Safety)
**Location**: [R/jjscatterstats.b.R:54-88](R/jjscatterstats.b.R#L54-L88)

**Problem**:
Function performs NO validation of data quality:
- ❌ No minimum sample size check (correlation unreliable with n < 20)
- ❌ No missing data assessment
- ❌ No outlier detection
- ❌ No check for constant variables (r undefined)
- ❌ No range/distribution checks

**Current Validation**:
```r
# Line 83 - ONLY validation
if (nrow(self$data) == 0)
    stop('Data contains no (complete) rows')  # ❌ Legacy stop()
```

**Clinical Risk**:
Clinicians may:
- Trust correlations calculated from n=5 patients (statistically meaningless)
- Miss that 80% of data is missing
- Get distorted results from extreme outliers
- Interpret undefined results from constant biomarkers

**Required Validation**:

```r
# Sample size validation
n <- nrow(plotData)
if (n < 3) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$ERROR,
        message = "Insufficient data: Correlation requires at least 3 observations. Current sample size: n = {n}"
    )
    self$results$insert(1, notice)
    return()
}

if (n < 20) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$STRONG_WARNING,
        message = "Small sample size (n = {n}): Correlation estimates may be unstable. Results should be interpreted with caution. Recommend n ≥ 20 for reliable correlation analysis."
    )
    self$results$insert(50, notice)
}

# Missing data check
complete_cases <- sum(complete.cases(plotData[, c(dep, group)]))
missing_pct <- round(100 * (n - complete_cases) / n, 1)
if (missing_pct > 20) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$WARNING,
        message = "High proportion of missing data ({missing_pct}%). Correlation calculated on {complete_cases} complete observations. Consider investigating missing data patterns before interpretation."
    )
    self$results$insert(100, notice)
}

# Constant variable check
if (sd(plotData[[dep]], na.rm = TRUE) == 0 || sd(plotData[[group]], na.rm = TRUE) == 0) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$ERROR,
        message = "Cannot compute correlation: One or both variables have zero variance (constant values). Correlation is undefined for constant variables."
    )
    self$results$insert(1, notice)
    return()
}

# Outlier detection (IQR method)
detect_outliers <- function(x) {
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    sum(x < (q1 - 3 * iqr) | x > (q3 + 3 * iqr), na.rm = TRUE)
}

outliers_x <- detect_outliers(plotData[[dep]])
outliers_y <- detect_outliers(plotData[[group]])
total_outliers <- outliers_x + outliers_y

if (total_outliers > 0) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$WARNING,
        message = "Extreme outliers detected ({total_outliers} observations beyond 3×IQR). Outliers can strongly influence correlation coefficients. Consider: (1) Visual inspection of scatter plot, (2) Robust correlation methods, or (3) Outlier removal if biologically implausible."
    )
    self$results$insert(100, notice)
}
```

---

### 🔴 Issue 3: No Notices API Implementation

**Severity**: CRITICAL (Architecture)
**Location**: Throughout [R/jjscatterstats.b.R](R/jjscatterstats.b.R)

**Problem**:
Function uses **ZERO** jamovi Notices API calls:
- Uses legacy `stop()` on line 83
- No ERROR notices for validation failures
- No WARNING notices for data quality issues
- No INFO notices for completion or guidance

**Impact**:
- Poor user experience (hard crashes vs. friendly notices)
- Inconsistent with other jamovi modules
- Missing opportunities for user education

**Reference**: See detailed Notices audit in previous `/check-function-base` output.

---

### 🟡 Issue 4: Clinical Preset Option Mutation

**Severity**: MODERATE (User Confusion)
**Location**: [R/jjscatterstats.b.R:90-152](R/jjscatterstats.b.R#L90-L152)

**Problem**:
Clinical presets directly mutate `self$options` values:

```r
# Lines 112-114 - Direct mutation
self$options$typestatistics <- "nonparametric"
self$options$addGGPubrPlot <- TRUE
self$options$ggpubrPalette <- "jco"
```

**Issues**:
1. **User Confusion**: "I didn't select nonparametric, why did it change?"
2. **Unconventional**: Most jamovi modules don't mutate options after initialization
3. **UI Sync**: jamovi UI may not reflect the mutated values
4. **Reproducibility**: Saved analysis files may not capture preset application

**Better Approach**:
```r
# Option 1: Use preset-specific defaults in .a.yaml
# Option 2: Create computed options based on preset
# Option 3: Show INFO notice explaining preset application

.applyClinicalPreset = function() {
    preset <- self$options$clinicalPreset
    if (preset == "custom") return()

    # Build preset configuration
    preset_config <- private$.getPresetConfig(preset)

    # Show what preset WOULD apply (don't mutate)
    notice_msg <- sprintf(
        "Clinical Preset: %s • This preset recommends: %s • Manually adjust options below or select Custom preset to disable recommendations.",
        preset_config$name,
        paste(preset_config$recommendations, collapse = " • ")
    )

    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$INFO,
        message = notice_msg
    )
    self$results$insert(900, notice)
}
```

---

## STRENGTHS

### ✅ 1. Sound Statistical Foundation

**What's Good**:
- Delegates core statistics to **ggstatsplot** (well-tested, peer-reviewed package)
- Correct correlation methods: Pearson, Spearman, robust (WRS2), Bayesian (BayesFactor)
- Proper confidence interval handling via ggstatsplot
- Correct smooth line methods: lm, loess, gam

**Evidence**:
```r
# Lines 201-222 - Proper argument passing to ggstatsplot
.args <- list(
    data = plotData,
    x = self$options$dep,
    y = self$options$group,
    type = self$options$typestatistics,  # Correctly maps to ggstatsplot types
    conf.level = self$options$conflevel,
    bf.message = self$options$bfmessage,
    k = self$options$k  # Decimal rounding
)
```

**Statistical Accuracy**: ⭐⭐⭐⭐⭐ (5/5) for core ggstatsplot integration

---

### ✅ 2. Variable Name Safety (Recent Addition)

**What's Good**:
- Implements `.escapeVar()` helper for variables with special characters
- Correctly applied to NSE contexts with `rlang::sym()`

**Evidence**:
```r
# Lines 13-20 - Helper method
.escapeVar = function(varname) {
    if (is.null(varname) || varname == "") return(varname)
    if (grepl("[^A-Za-z0-9_.]", varname)) {
        return(paste0("`", varname, "`"))
    }
    return(varname)
}

# Lines 407-420 - Applied to aesthetics
point_aes$colour <- rlang::sym(private$.escapeVar(self$options$colorvar))
```

**Robustness**: Handles clinical variable names like "Ki-67 (%)", "CD3+ Count", "Tumor Size (mm)"

---

### ✅ 3. Comprehensive Visualization Options

**What's Good**:
- Three plot types: basic (ggstatsplot), grouped (by factor), enhanced (multi-aesthetic)
- ggpubr integration for publication-ready plots
- Multiple aesthetic mappings: color, size, shape, alpha, labels
- Marginal distributions, rug plots, smooth lines
- Customizable themes, dimensions, colors

**Flexibility**: Covers diverse clinical use cases from exploratory to publication

---

### ✅ 4. Well-Organized Code Structure

**What's Good**:
- Clear separation of plot methods (`.plot`, `.plot2`, `.plot3`, `.plotGGPubr`, `.plotGGPubr2`)
- Modular helper methods (`.applyClinicalPreset`, `.generateExplanations`, `.escapeVar`)
- R6 class structure follows jamovi conventions
- Consistent variable naming

**Maintainability**: Easy to locate and understand different plot implementations

---

## IMPROVEMENT OPPORTUNITIES

### 🟡 1. Duplicate Code in Grouped Plot

**Issue**: `.plot2` constructs the same `rlang::expr` twice based on marginal option
**Location**: [R/jjscatterstats.b.R:276-336](R/jjscatterstats.b.R#L276-L336)

**Current Code** (61 lines duplicated):
```r
# Lines 278-302 - First construction
plot_call <- rlang::expr(
    ggstatsplot::grouped_ggscatterstats(...)
)

# Lines 306-332 - Duplicate construction with marginal additions
if (self$options$marginal) {
    plot_call <- rlang::expr(
        ggstatsplot::grouped_ggscatterstats(...)  # Same call rebuilt
    )
}
```

**Improvement**:
```r
# Build base arguments list
base_args <- list(
    data = plotData,
    x = rlang::sym(self$options$dep),
    y = rlang::sym(self$options$group),
    grouping.var = rlang::sym(self$options$grvar),
    type = self$options$typestatistics,
    # ... all common args
)

# Add marginal-specific args conditionally
if (self$options$marginal) {
    base_args$xfill <- self$options$xsidefill
    base_args$yfill <- self$options$ysidefill
}

# Single call construction
plot_call <- rlang::expr(
    ggstatsplot::grouped_ggscatterstats(!!!base_args)
)
plot <- eval(plot_call)
```

**Impact**: Reduces code from 61 lines to ~25 lines, improves maintainability

---

### 🟡 2. Magic Strings and Hard-Coded Values

**Issue**: Preset names, method names, and defaults hard-coded throughout
**Location**: Multiple locations

**Examples**:
```r
# Line 92 - Magic string
if (preset == "custom") {  # ❌ Hard-coded

# Line 99 - Magic string
if (preset == "biomarker_correlation") {  # ❌ Hard-coded

# Lines 435-441 - Repeated switch logic
smooth_method <- switch(
    self$options$smoothMethod,
    "lm" = "lm",      # ❌ Redundant
    "loess" = "loess",
    "gam" = "gam",
    "lm"
)
```

**Improvement**:
```r
# Define constants at top of private methods
PRESET_CUSTOM <- "custom"
PRESET_BIOMARKER <- "biomarker_correlation"
PRESET_TREATMENT <- "treatment_response_analysis"
PRESET_PUBLICATION <- "publication_ready"

DEFAULT_SAMPLE_SIZE_WARNING <- 20
DEFAULT_SAMPLE_SIZE_MINIMUM <- 3
DEFAULT_OUTLIER_IQR_MULTIPLIER <- 3

# Use constants
if (preset == PRESET_CUSTOM) {
    return()
}

if (preset == PRESET_BIOMARKER) {
    # Apply preset...
}
```

---

### 🟡 3. Limited Code Comments

**Issue**: Complex logic lacks explanatory comments
**Location**: Throughout, especially `.plot3`

**Examples Needing Comments**:
```r
# Line 395-398 - Why use .data[[]] here?
aes_mapping <- ggplot2::aes(
    x = .data[[self$options$dep]],
    y = .data[[self$options$group]]
)

# Lines 467-553 - Long tryCatch needs overview comment
tryCatch({
    # What does this block do? Why fallbacks? When does it execute?
    test_type <- self$options$typestatistics
    # ... 80 lines of logic
})
```

**Improvement**:
```r
# Enhanced scatter plot uses direct data column access (.data[[]])
# for base aesthetics to avoid NSE issues. Aesthetic mappings (color,
# size, shape, alpha) use rlang::sym() with variable name escaping.
aes_mapping <- ggplot2::aes(
    x = .data[[self$options$dep]],
    y = .data[[self$options$group]]
)

# Calculate correlation for enhanced plot annotation
# Note: ggstatsplot handles correlations for plot1/plot2, but plot3
# is custom ggplot2 implementation requiring manual calculation.
# Robust and Bayesian methods are not fully implemented here - we
# fall back to classical methods with user notification.
tryCatch({
    # Map jamovi test type to cor.test method
    # ...
})
```

---

## ENHANCEMENT SUGGESTIONS

### 💡 1. Add Report Summary (Like jjridges)

**Clinician Need**: Copy-paste ready text for pathology reports

**Implementation**:
```r
# Add to .r.yaml
- name: reportSummary
  title: Report Summary (Copy-Ready)
  type: Html
  visible: true

# Add to .b.R
.generateReportSummary = function() {
    dep <- self$options$dep
    group <- self$options$group
    method <- switch(
        self$options$typestatistics,
        "parametric" = "Pearson",
        "nonparametric" = "Spearman",
        "robust" = "Robust (percentage bend)",
        "bayes" = "Bayesian",
        "Pearson"
    )

    # Get correlation from ggstatsplot results (would need to capture)
    # For now, recalculate
    cor_method <- if (self$options$typestatistics == "parametric") "pearson" else "spearman"
    cor_result <- cor.test(
        self$data[[dep]],
        self$data[[group]],
        method = cor_method
    )

    r <- cor_result$estimate
    p <- cor_result$p.value
    n <- sum(complete.cases(self$data[, c(dep, group)]))

    # Interpret correlation strength
    strength <- if (abs(r) < 0.3) "weak" else if (abs(r) < 0.7) "moderate" else "strong"
    direction <- if (r > 0) "positive" else "negative"

    # Generate copy-ready text
    html <- paste0(
        "<div style='background:#f5f5f5; border:1px solid #ddd; padding:15px; font-family:monospace;'>",
        "<h4 style='margin-top:0;'>📋 Report Summary</h4>",
        "<p style='margin:5px 0;'><strong>Analysis:</strong> Correlation between ", dep, " and ", group, "</p>",
        "<p style='margin:5px 0;'><strong>Method:</strong> ", method, " correlation</p>",
        "<p style='margin:5px 0;'><strong>Sample Size:</strong> n = ", n, " complete observations</p>",
        "<p style='margin:5px 0;'><strong>Correlation Coefficient:</strong> r = ", sprintf("%.3f", r), "</p>",
        "<p style='margin:5px 0;'><strong>Statistical Significance:</strong> p ",
        ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)), "</p>",
        "<p style='margin:5px 0;'><strong>Interpretation:</strong> ",
        ifelse(p < 0.05,
            paste0("A ", strength, " ", direction, " correlation was observed between ", dep, " and ", group,
                   " (", method, " r = ", sprintf("%.3f", r), ", p ",
                   ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)), ")."),
            paste0("No statistically significant correlation was observed between ", dep, " and ", group,
                   " (", method, " r = ", sprintf("%.3f", r), ", p = ", sprintf("%.3f", p), ").")
        ),
        "</p>",
        "</div>"
    )

    return(html)
}
```

---

### 💡 2. Add About Panel (Educational Content)

**Clinician Need**: Understanding scatter plots and correlation

**Implementation**:
```r
# Add to .a.yaml
- name: showAboutPanel
  title: Show About Panel
  type: Bool
  default: false

# Add to .r.yaml
- name: aboutPanel
  title: About Scatter Plots & Correlation
  type: Html
  visible: false

# Add to .b.R
.generateAboutPanel = function() {
    html <- paste0(
        "<div style='background:#e8f5e9; border-left:4px solid #4caf50; padding:15px;'>",
        "<h3 style='color:#2e7d32; margin-top:0;'>📚 About Scatter Plots & Correlation</h3>",

        "<h4>What is a Scatter Plot?</h4>",
        "<p>A scatter plot visualizes the relationship between two continuous variables. ",
        "Each point represents one observation (e.g., one patient). The position shows the ",
        "values of both variables for that observation.</p>",

        "<h4>What is Correlation?</h4>",
        "<p>Correlation measures the strength and direction of the linear relationship between ",
        "two variables. The correlation coefficient (r) ranges from -1 to +1:</p>",
        "<ul>",
        "<li><strong>r = +1</strong>: Perfect positive correlation (as one increases, other increases)</li>",
        "<li><strong>r = 0</strong>: No linear correlation</li>",
        "<li><strong>r = -1</strong>: Perfect negative correlation (as one increases, other decreases)</li>",
        "</ul>",

        "<h4>Interpreting Correlation Strength</h4>",
        "<ul>",
        "<li><strong>|r| < 0.3</strong>: Weak correlation</li>",
        "<li><strong>0.3 ≤ |r| < 0.7</strong>: Moderate correlation</li>",
        "<li><strong>|r| ≥ 0.7</strong>: Strong correlation</li>",
        "</ul>",

        "<h4>Clinical Example</h4>",
        "<p><em>Question:</em> Is tumor size related to Ki-67 proliferation index?</p>",
        "<p><em>Analysis:</em> Scatter plot of tumor size (mm) vs. Ki-67 (%). ",
        "Spearman r = 0.62, p < 0.001</p>",
        "<p><em>Interpretation:</em> There is a moderate positive correlation between tumor size ",
        "and Ki-67 index. Larger tumors tend to have higher proliferation rates, though the relationship ",
        "is not perfect (many exceptions exist).</p>",

        "<h4>Important Caveats</h4>",
        "<ul>",
        "<li><strong>Correlation ≠ Causation:</strong> Correlation shows association, not cause-and-effect</li>",
        "<li><strong>Outliers:</strong> Single extreme values can strongly influence correlation</li>",
        "<li><strong>Linearity:</strong> Correlation measures linear relationships only</li>",
        "<li><strong>Sample Size:</strong> Small samples (n < 20) produce unstable estimates</li>",
        "</ul>",

        "<h4>Choosing the Right Method</h4>",
        "<ul>",
        "<li><strong>Parametric (Pearson):</strong> For normally distributed data without outliers</li>",
        "<li><strong>Nonparametric (Spearman):</strong> For skewed data, ranks, or ordinal scales</li>",
        "<li><strong>Robust:</strong> When outliers are present but you want to analyze all data</li>",
        "<li><strong>Bayesian:</strong> For quantifying evidence strength and uncertainty</li>",
        "</ul>",

        "</div>"
    )
    return(html)
}
```

---

### 💡 3. Add Assumptions Panel

**Clinician Need**: Understanding when results are valid

**Implementation**:
```r
# Add to .a.yaml
- name: showAssumptions
  title: Show Statistical Assumptions
  type: Bool
  default: false

# Add to .r.yaml
- name: assumptionsPanel
  title: Statistical Assumptions & Caveats
  type: Html
  visible: false

# Add to .b.R
.generateAssumptionsPanel = function() {
    test_type <- self$options$typestatistics

    html <- paste0(
        "<div style='background:#fff3cd; border-left:4px solid #ffc107; padding:15px;'>",
        "<h3 style='color:#856404; margin-top:0;'>⚠️ Statistical Assumptions</h3>"
    )

    if (test_type == "parametric") {
        html <- paste0(html,
            "<h4>Pearson Correlation Assumptions</h4>",
            "<ol>",
            "<li><strong>Linearity:</strong> Relationship between variables must be linear. ",
            "Check scatter plot for curved patterns.</li>",
            "<li><strong>Normality:</strong> Both variables should be approximately normally distributed. ",
            "Important for small samples (n < 30).</li>",
            "<li><strong>Homoscedasticity:</strong> Variance should be similar across the range. ",
            "Check scatter plot for funnel shapes.</li>",
            "<li><strong>No Outliers:</strong> Extreme values can dominate the correlation. ",
            "Inspect plot for unusual points.</li>",
            "<li><strong>Independence:</strong> Observations must be independent (e.g., not repeated measures).</li>",
            "</ol>",
            "<p><strong>⚠️ If assumptions violated:</strong> Consider Spearman (nonparametric) or Robust methods.</p>"
        )
    } else if (test_type == "nonparametric") {
        html <- paste0(html,
            "<h4>Spearman Correlation Assumptions</h4>",
            "<ol>",
            "<li><strong>Monotonic Relationship:</strong> As one variable increases, the other tends to ",
            "increase (or decrease) consistently.</li>",
            "<li><strong>Ordinal or Continuous:</strong> Variables can be ranked meaningfully.</li>",
            "<li><strong>Independence:</strong> Observations must be independent.</li>",
            "</ol>",
            "<p><strong>✅ Advantages:</strong> Robust to outliers, works with skewed data, ",
            "no normality assumption.</p>",
            "<p><strong>⚠️ Limitation:</strong> May have slightly less power than Pearson when assumptions are met.</p>"
        )
    } else if (test_type == "robust") {
        html <- paste0(html,
            "<h4>Robust Correlation (Percentage Bend)</h4>",
            "<p>Uses a robust estimator that downweights outliers while retaining information.</p>",
            "<p><strong>✅ When to Use:</strong></p>",
            "<ul>",
            "<li>Outliers are present but you want parametric-style analysis</li>",
            "<li>Data is approximately linear but has extreme values</li>",
            "<li>You want estimates less influenced by single observations</li>",
            "</ul>",
            "<p><strong>⚠️ Assumptions:</strong> Still assumes approximately linear relationship. ",
            "Not appropriate for severely non-monotonic patterns.</p>"
        )
    } else if (test_type == "bayes" || test_type == "bayesian") {
        html <- paste0(html,
            "<h4>Bayesian Correlation</h4>",
            "<p>Provides Bayes Factor (BF) quantifying evidence for correlation vs. no correlation.</p>",
            "<p><strong>Interpreting Bayes Factor (BF₁₀):</strong></p>",
            "<ul>",
            "<li><strong>BF > 10:</strong> Strong evidence for correlation</li>",
            "<li><strong>BF 3-10:</strong> Moderate evidence for correlation</li>",
            "<li><strong>BF 1-3:</strong> Anecdotal evidence for correlation</li>",
            "<li><strong>BF ≈ 1:</strong> No evidence either way</li>",
            "<li><strong>BF < 1:</strong> Evidence for no correlation (BF₀₁ = 1/BF₁₀)</li>",
            "</ul>",
            "<p><strong>✅ Advantages:</strong> Quantifies evidence strength, handles small samples well, ",
            "incorporates prior knowledge.</p>"
        )
    }

    html <- paste0(html,
        "<h4>General Caveats for Clinical Interpretation</h4>",
        "<ul>",
        "<li><strong>Sample Size:</strong> Correlations from n < 20 are unstable. Confidence intervals will be wide.</li>",
        "<li><strong>Missing Data:</strong> Check percentage of complete observations. High missingness may bias results.</li>",
        "<li><strong>Subgroup Heterogeneity:</strong> Overall correlation may hide different patterns in subgroups.</li>",
        "<li><strong>Measurement Error:</strong> Unreliable measurements attenuate (weaken) observed correlations.</li>",
        "<li><strong>Restricted Range:</strong> Analyzing only high or low values reduces apparent correlation.</li>",
        "</ul>",
        "</div>"
    )

    return(html)
}
```

---

### 💡 4. Add Completion Notice with Interpretation

**Clinician Need**: Confirmation that analysis completed + next steps

**Implementation**:
```r
# Add at end of .run()
.run = function() {
    # ... existing code ...

    private$.applyClinicalPreset()
    private$.generateExplanations()

    # Add completion notice
    private$.addCompletionNotice()
}

.addCompletionNotice = function() {
    dep <- self$options$dep
    group <- self$options$group

    notice_msg <- sprintf(
        "Scatter plot analysis completed: %s vs. %s • Review plots above for visual patterns • Check statistical results in subtitle • For detailed interpretation, enable 'Show About Panel' or 'Show Statistical Assumptions' in Clinical Presets & Options",
        dep, group
    )

    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$INFO,
        message = notice_msg
    )
    self$results$insert(999, notice)
}
```

---

## SPECIFIC RECOMMENDATIONS

### 1. Architecture & Design

**Recommendation**: Refactor preset system to avoid option mutation

**Action**:
- Remove direct `self$options` mutations in `.applyClinicalPreset()`
- Use INFO notices to communicate preset recommendations
- Let user manually select recommended settings
- OR: Implement preset defaults in .a.yaml conditional logic

**Benefit**: Clearer user understanding, better reproducibility, conventional jamovi pattern

---

### 2. Mathematical & Statistical Correctness

**Recommendation**: Fix correlation method handling in enhanced plot

**Action**:
1. Remove dead code referencing `self$results$warnings` (lines 521-531, 551-552)
2. Implement Notices API for method fallback warnings
3. Consider removing correlation annotation from plot3 entirely (ggstatsplot already handles it in plot1/plot2)

**Code Fix**:
```r
# OPTION A: Remove correlation from plot3 (RECOMMENDED)
# Delete lines 467-553 entirely
# Rationale: ggstatsplot plots already show correlation correctly
# Enhanced plot is for exploring multi-variate relationships with aesthetics

# OPTION B: Fix with Notices API
if (!is.null(warning_msg)) {
    notice <- jmvcore::Notice$new(
        type = jmvcore::NoticeType$WARNING,
        message = warning_msg
    )
    self$results$insert(100, notice)
}
```

---

### 3. Clinical Readiness

**Recommendation**: Implement comprehensive validation framework

**Priority Actions**:
1. ✅ **Sample size validation** (minimum n=3, warning n<20)
2. ✅ **Missing data assessment** (warn if >20% missing)
3. ✅ **Outlier detection** (IQR method, warn if present)
4. ✅ **Constant variable check** (r undefined if SD=0)
5. ✅ **Replace stop() with ERROR Notice**

**Code Template** (see Issue 2 above for full implementation)

---

### 4. Clinician-Friendly Enhancements

**High-Priority Additions**:

| Feature | Priority | Effort | Impact |
|---------|----------|--------|--------|
| Report Summary | HIGH | Medium | High (copy-paste for reports) |
| About Panel | HIGH | Low | High (education) |
| Assumptions Panel | HIGH | Low | High (validity guidance) |
| Completion Notice | MEDIUM | Low | Medium (confirmation) |
| Example Interpretations | MEDIUM | Medium | High (learning) |
| Guided Mode | LOW | High | Medium (complexity) |

**Implementation Order**:
1. Fix critical bugs (warnings references)
2. Add data validation
3. Implement Notices API
4. Add Report Summary
5. Add About Panel
6. Add Assumptions Panel
7. Add Completion Notice

---

## ACTION ITEMS CHECKLIST

### 🔴 URGENT (Must Fix Before ANY Release)

- [ ] **SHOWSTOPPER**: Remove/fix lines 521-531 and 551-552 (warnings references)
  - [ ] Either remove correlation from plot3 entirely
  - [ ] OR replace with Notices API (see code above)
- [ ] **SAFETY**: Replace `stop()` on line 83 with ERROR Notice
- [ ] **VALIDATION**: Implement minimum sample size check (n ≥ 3)
- [ ] **VALIDATION**: Implement constant variable check (SD > 0)
- [ ] **TEST**: Verify no crashes when selecting robust/bayes with enhanced plot

### 🟡 HIGH Priority (Before Production Release)

- [ ] **VALIDATION**: Add sample size warning (n < 20)
- [ ] **VALIDATION**: Add missing data assessment (>20%)
- [ ] **VALIDATION**: Add outlier detection (3×IQR)
- [ ] **UX**: Implement Report Summary (copy-ready text)
- [ ] **UX**: Implement About Panel (educational content)
- [ ] **UX**: Implement Assumptions Panel (validity guidance)
- [ ] **UX**: Add completion INFO notice
- [ ] **REFACTOR**: Fix duplicate code in `.plot2` (lines 276-336)
- [ ] **REFACTOR**: Replace preset option mutation with clearer approach

### 🟢 MEDIUM Priority (Quality Improvements)

- [ ] **CODE QUALITY**: Extract magic strings to constants
- [ ] **CODE QUALITY**: Add explanatory comments to complex logic
- [ ] **PERFORMANCE**: Cache correlation results between plots
- [ ] **UX**: Add interpretation examples to About Panel
- [ ] **UX**: Add sample size to plot subtitles
- [ ] **TEST**: Test with variables containing special characters
- [ ] **TEST**: Test with n=2, n=10, n=100 datasets
- [ ] **TEST**: Test with 50% missing data
- [ ] **TEST**: Test with extreme outliers

### 🔵 LOW Priority (Nice to Have)

- [ ] **UX**: Implement guided mode for test selection
- [ ] **UX**: Add interactive tooltips in jamovi UI
- [ ] **FEATURE**: Add sample size power analysis
- [ ] **FEATURE**: Add distribution normality tests
- [ ] **DOCS**: Add vignette with clinical examples
- [ ] **I18N**: Prepare for internationalization

---

## TESTING RECOMMENDATIONS

### Unit Tests Needed

```r
# Test data validation
test_that("Sample size validation works", {
    expect_error(jjscatterstats(data.frame(x=1:2, y=1:2), dep="x", group="y"))
})

test_that("Constant variable detection works", {
    expect_error(jjscatterstats(data.frame(x=rep(5,10), y=1:10), dep="x", group="y"))
})

# Test variable name escaping
test_that("Special characters in variable names work", {
    df <- data.frame(
        `Ki-67 (%)` = rnorm(50),
        `Tumor Size (mm)` = rnorm(50),
        check.names = FALSE
    )
    expect_no_error(jjscatterstats(df, dep="Ki-67 (%)", group="Tumor Size (mm)"))
})

# Test preset application
test_that("Clinical presets apply correctly", {
    result <- jjscatterstats(mtcars, dep="mpg", group="hp", clinicalPreset="biomarker_correlation")
    # Verify typestatistics set to nonparametric
    # Verify INFO notice generated
})
```

### Manual Testing Protocol

1. **Basic Functionality** (2 min)
   - [ ] mtcars: mpg vs. hp → Plot renders, correlation shown
   - [ ] iris: Sepal.Length vs. Petal.Length → Works with standard data

2. **Edge Cases** (5 min)
   - [ ] n=2 → ERROR notice shown, no plot
   - [ ] n=10 → WARNING notice about small sample
   - [ ] 80% missing data → WARNING notice
   - [ ] Constant variable → ERROR notice
   - [ ] Extreme outliers → WARNING notice

3. **Variable Name Safety** (3 min)
   - [ ] Variable: "Ki-67 (%)" in colorvar → No error
   - [ ] Variable: "CD3+ Count" in sizevar → No error
   - [ ] Variable: "Tumor Size (mm)" in labelvar → No error

4. **Enhanced Plot + Robust/Bayes** (2 min)
   - [ ] Select robust + add colorvar → No crash, appropriate notice
   - [ ] Select bayes + add sizevar → No crash, appropriate notice

5. **Clinical Presets** (2 min)
   - [ ] Biomarker Correlation → INFO notice, settings recommended
   - [ ] Publication Ready → INFO notice, theme applied
   - [ ] Treatment Response → INFO notice, robust selected

6. **Clinician-Friendly Features** (3 min)
   - [ ] Report Summary shows, copy-pasteable
   - [ ] About Panel displays when checked
   - [ ] Assumptions Panel shows appropriate content for test type
   - [ ] Completion notice appears at bottom

**Total Testing Time**: ~17 minutes

---

## CONCLUSION

### Current State

**jjscatterstats** has a solid statistical foundation (delegating to ggstatsplot) and recently added variable name safety, but contains **critical bugs** that prevent release:

1. 🔴 **Showstopper bug**: References deleted `warnings` output (will crash)
2. 🔴 **No validation**: Accepts n=1, constant variables, corrupted data
3. 🔴 **No Notices API**: Uses legacy error handling
4. 🟡 **Poor UX**: Missing clinician-friendly features

### Effort Estimate

| Task Category | Effort | Timeline |
|---------------|--------|----------|
| Fix critical bugs | 1-2 hours | Immediate |
| Add data validation | 2-3 hours | Same day |
| Implement Notices API | 1-2 hours | Same day |
| Add Report Summary | 2-3 hours | Next day |
| Add About Panel | 1-2 hours | Next day |
| Add Assumptions Panel | 2-3 hours | Next day |
| Testing & documentation | 3-4 hours | Next day |
| **TOTAL** | **12-19 hours** | **2-3 days** |

### Release Recommendation

**Status**: ❌ **NOT READY FOR RELEASE**

**Minimum Requirements for Release**:
1. ✅ Fix showstopper bug (remove warnings references)
2. ✅ Add minimum data validation (n ≥ 3, SD > 0)
3. ✅ Replace stop() with ERROR Notice
4. ✅ Test basic functionality without crashes

**Recommended for Quality Release**:
1. All minimum requirements above
2. ✅ Comprehensive data validation (sample size, missing data, outliers)
3. ✅ Full Notices API implementation
4. ✅ Report Summary for clinicians
5. ✅ About Panel for education
6. ✅ Assumptions Panel for guidance

**Next Steps**:
1. Create feature branch: `fix/jjscatterstats-critical-bugs`
2. Fix showstopper (lines 521-531, 551-552)
3. Implement data validation
4. Add Notices API
5. Add clinician-friendly enhancements
6. Test thoroughly
7. Document changes
8. Review and merge

---

**Prepared by**: Claude Code (Sonnet 4.5)
**Review Date**: 2025-12-16
**Status**: Comprehensive code review completed
**Recommended Action**: Fix critical bugs before implementing enhancements

---

## Comparison with jjridges

For reference, `jjridges` (recently enhanced) now includes:
- ✅ Report Summary (copy-ready)
- ✅ About Panel (educational)
- ✅ Assumptions Panel (guidance)
- ✅ Full Notices API implementation
- ✅ Data validation

**jjscatterstats should match this quality standard.**
