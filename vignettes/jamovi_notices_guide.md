# The Complete Guide to User Notifications in jamovi Development

This is the comprehensive guide to implementing user notices and alerts in jamovi module development. Notices are essential for communicating important information, warnings, and errors directly within analysis results. This guide covers everything from basic notice implementation to advanced notification strategies for clinical applications.

## Table of Contents

1. [Introduction: Understanding jamovi Notices](#1-introduction-understanding-jamovi-notices)
2. [Notice Types and When to Use Them](#2-notice-types-and-when-to-use-them)
3. [Basic Notice Implementation](#3-basic-notice-implementation)
4. [Notice Content Best Practices](#4-notice-content-best-practices)
5. [Advanced Notice Patterns](#5-advanced-notice-patterns)
6. [Clinical and Research Notice Strategies](#6-clinical-and-research-notice-strategies)
7. [Notice Positioning and Management](#7-notice-positioning-and-management)
8. [Error Communication Patterns](#8-error-communication-patterns)
9. [Integration with jamovi Module Architecture](#9-integration-with-jamovi-module-architecture)
10. [Complete Examples](#10-complete-examples)
11. [Real-World Patterns from jamovi Core](#11-real-world-patterns-from-jamovi-core)
12. [References and Additional Resources](#12-references-and-additional-resources)

---

## 1. Introduction: Understanding jamovi Notices

### What are jamovi Notices?

Notices are user-facing messages displayed within jamovi analysis results using the `jmvcore::Notice` class. They provide a standardized way to communicate:

- **Errors** - Critical issues preventing analysis
- **Warnings** - Potential problems with data or analysis
- **Information** - Helpful context and guidance
- **Alerts** - Important methodological considerations

### The jamovi Notice Architecture

Notices are integrated into the jamovi results framework:

```
.b.R implementation → jmvcore::Notice → Results object → User interface
```

### Why Use Notices?

#### User Experience Benefits
- **Immediate feedback** - Users see issues in context
- **Actionable guidance** - Clear instructions for resolution
- **Progressive disclosure** - Information when needed
- **Professional presentation** - Consistent, branded communication

#### Development Benefits
- **Standardized messaging** - Consistent look and behavior
- **Integrated with results** - No separate error handling needed
- **Type-safe communication** - Predefined notice types
- **Flexible positioning** - Control placement in results

### Notice vs Other Communication Methods

**Use Notices for:**
- Analysis-specific warnings
- Data validation messages
- Methodological considerations
- User guidance

**Do NOT use Notices for:**
- R console debugging (use `cat()` or `message()`)
- Logging (use proper logging frameworks)
- Progress indicators (use different mechanisms)

### Quick Reference: Common Patterns

**Basic Notice Creation:**
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'noticeName',
    type = jmvcore::NoticeType$WARNING
)
notice$setContent('Your message here')
self$results$insert(1, notice)
```

**With Internationalization:**
```r
notice$setContent(
    jmvcore::format(.("Found {} issues in {}"), n_issues, var_name)
)
```

**Accumulating Warnings:**
```r
warnings <- ""
if (issue1) warnings <- paste(warnings, "Issue 1", sep = "\n")
if (issue2) warnings <- paste(warnings, "Issue 2", sep = "\n")
if (warnings != "") {
    notice <- jmvcore::Notice$new(self$options, name = 'warnings',
                                  type = jmvcore::NoticeType$WARNING,
                                  content = warnings)
    self$results$insert(1, notice)
}
```

**Helper Function Pattern:**
```r
setAnalysisNotice <- function(self, message, name, type) {
    notice <- jmvcore::Notice$new(options = self$options, name = name, type = type)
    notice$setContent(message)
    self$results$insert(1, notice)
}
```

---

## 2. Notice Types and When to Use Them

jamovi provides four distinct notice types through `jmvcore::NoticeType`:

### ERROR - Critical Issues

**Type**: `jmvcore::NoticeType$ERROR`

**When to Use**:
- Analysis cannot proceed
- Required data is missing or invalid
- Critical assumptions violated
- Fatal calculation errors

**Visual Presentation**: Red background, error icon

**Example Scenarios**:
```r
# Insufficient data
"Survival analysis requires at least 10 events. Current dataset has only 5 events."

# Invalid variable type
"Outcome variable must be numeric. Selected variable 'treatment' is categorical."

# Missing required input
"Cox regression requires both time and event variables to be specified."
```

### STRONG_WARNING - Serious Concerns

**Type**: `jmvcore::NoticeType$STRONG_WARNING`

**When to Use**:
- Analysis proceeds but results may be unreliable
- Important assumptions questionable
- Data quality issues detected
- Results should be interpreted with caution

**Visual Presentation**: Orange/amber background, warning icon

**Example Scenarios**:
```r
# Sample size concerns
"Small sample size (n=15) may produce unreliable estimates. Consider cautious interpretation."

# Assumption violations
"Proportional hazards assumption violated (p < 0.05). Consider stratified or time-varying models."

# Data quality issues
"25% of observations have missing outcome data. Complete case analysis may introduce bias."
```

### WARNING - Minor Concerns

**Type**: `jmvcore::NoticeType$WARNING`

**When to Use**:
- Analysis valid but user should be aware
- Optional enhancements available
- Minor data issues
- Methodological notes

**Visual Presentation**: Yellow background, warning icon

**Example Scenarios**:
```r
# Methodological notes
"Median survival not reached for control group. Restricted mean survival time used instead."

# Optional improvements
"Consider including age as a covariate to adjust for potential confounding."

# Minor data notes
"3 tied event times detected. Efron approximation used for handling ties."
```

### INFO - Helpful Information

**Type**: `jmvcore::NoticeType$INFO`

**When to Use**:
- Clarifying methodology
- Explaining analysis choices
- Educational content
- Positive confirmation

**Visual Presentation**: Blue background, info icon

**Example Scenarios**:
```r
# Methodology explanation
"Confidence intervals calculated using bootstrap method with 1000 resamples."

# Analysis confirmation
"Analysis successfully completed using 247 observations (98% of dataset)."

# Educational guidance
"AUC > 0.8 indicates good discriminative ability for this diagnostic test."
```

---

## 3. Basic Notice Implementation

### Core Notice Syntax

The basic structure for creating any notice (from jamovi/jmv official implementation):

```r
# Create notice object
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'uniqueNoticeName',
    type = jmvcore::NoticeType$INFO
)

# Set content
notice$setContent('Your message to the user')

# Add to results (position 1 = top of results)
self$results$insert(1, notice)
```

### Real-World Example from jamovi/jmv

From the official jamovi `conttables.b.R` implementation:

```r
# From jamovi/jmv/R/conttables.b.R
weightsNotice <- jmvcore::Notice$new(
    self$options,
    name = '.weights',
    type = jmvcore::NoticeType$WARNING
)

# Set the content with informative message
weightsNotice$setContent(
    jmvcore::format(.("The data is weighted by the variable {}"),
                    self$options$counts)
)

# Insert at top of results
self$results$insert(1, weightsNotice)
```

**Key points from this example:**
- Notice name starts with `.` to indicate internal/system notice
- Uses `jmvcore::format()` with `..()` for internationalization
- Inserts dynamic content (variable name) into the message

### Step-by-Step Implementation

#### Step 1: Create Notice Object

```r
# In your .b.R file, within the .run() function
dataValidationNotice <- jmvcore::Notice$new(
    options = self$options,
    name = 'dataValidation',  # Must be unique within results
    type = jmvcore::NoticeType$WARNING
)
```

**Key Parameters**:
- `options` - Always pass `self$options` (required)
- `name` - Unique identifier (no spaces, use camelCase)
- `type` - One of four NoticeType values

#### Step 2: Set Notice Content

```r
# Simple text message
dataValidationNotice$setContent('Sample size is below recommended minimum.')

# Formatted message with variables
missing_count <- sum(is.na(data$outcome))
dataValidationNotice$setContent(
    sprintf('Analysis excludes %d observations with missing outcome data.', missing_count)
)

# Multi-line message
message <- paste(
    'Small sample size detected (n = 23).',
    'Results should be interpreted cautiously.',
    sep = '\n'
)
dataValidationNotice$setContent(message)
```

#### Step 3: Add Notice to Results

**Method 1: Insert into main results**

```r
# Insert at top of results (position 1)
self$results$insert(1, dataValidationNotice)

# Insert at bottom (use high position number)
self$results$insert(999, dataValidationNotice)

# Insert before specific table
# (Use position just before that table)
self$results$insert(2, dataValidationNotice)
```

**Method 2: Attach to specific result element (from descriptives.b.R)**

```r
# From jamovi/jmv/R/descriptives.b.R
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'warningMessage',
    type = jmvcore::NoticeType$WARNING
)
notice$setContent('Plots are not yet supported for weighted descriptives')

# Attach notice to plots header
plots$setHeader(notice)
```

**Method 3: Insert into result group (from linreg.b.R)**

```r
# From jamovi/jmv/R/linreg.b.R
mahalNote <- jmvcore::Notice$new(
    options = self$options,
    name = 'warningMessage',
    type = jmvcore::NoticeType$WARNING
)

mahalNote$setContent(
    .("Mahalanobis distance can only be calculated for models with two or more covariates")
)

# Insert into a specific result group
group$insert(1, mahalNote)
```

### Complete Basic Example

```r
# In your .b.R file's .run() function
.run = function() {

    # Validate sample size
    n_obs <- nrow(self$data)

    if (n_obs < 30) {
        # Create warning notice
        sampleSizeNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'sampleSizeWarning',
            type = jmvcore::NoticeType$WARNING
        )

        # Set content with specific information
        sampleSizeNotice$setContent(
            sprintf('Sample size (n=%d) is below recommended minimum of 30. Statistical power may be limited.', n_obs)
        )

        # Add to top of results
        self$results$insert(1, sampleSizeNotice)
    }

    # Continue with analysis...
}
```

---

## 4. Notice Content Best Practices

### Writing Effective Notice Messages

#### 1. Be Specific and Actionable

**Poor**:
```r
notice$setContent('Error in data')
```

**Better**:
```r
notice$setContent('Outcome variable contains non-numeric values. Please select a numeric variable.')
```

**Best**:
```r
n_invalid <- sum(!is.numeric(data[[outcome_var]]))
notice$setContent(
    sprintf('Outcome variable "%s" contains %d non-numeric values. Analysis requires a continuous numeric variable. Please verify variable type or select different variable.',
            outcome_var, n_invalid)
)
```

#### 2. Provide Context and Consequences

**Poor**:
```r
notice$setContent('Missing data detected')
```

**Better**:
```r
missing_pct <- sum(is.na(data$outcome)) / nrow(data) * 100
notice$setContent(
    sprintf('%.1f%% of observations have missing outcome data. Complete case analysis used.', missing_pct)
)
```

**Best**:
```r
missing_count <- sum(is.na(data$outcome))
missing_pct <- missing_count / nrow(data) * 100
complete_n <- nrow(data) - missing_count

notice$setContent(
    sprintf('Missing Data: %d observations (%.1f%%) excluded due to missing outcome values. Analysis based on %d complete cases. Consider multiple imputation if missing data is substantial.',
            missing_count, missing_pct, complete_n)
)
```

#### 3. Avoid Technical Jargon

**Poor** (too technical):
```r
notice$setContent('Eigenvalue decomposition failed in SVD computation')
```

**Better** (user-friendly):
```r
notice$setContent('Statistical computation encountered numerical instability. Data may have high multicollinearity or extreme values.')
```

**Best** (explains and guides):
```r
notice$setContent('Analysis cannot proceed due to computational issues, likely caused by:\n• High correlation between predictor variables\n• Extreme outliers in the data\n• Near-zero variance in variables\n\nSuggestion: Review data for outliers and consider standardizing variables.')
```

#### 4. Use Proper Grammar and Formatting

```r
# Good formatting
notice$setContent(
    paste(
        'Sample Size Consideration:',
        '',
        'Current sample (n=45) provides limited statistical power.',
        'Recommendations:',
        '• Results should be considered preliminary',
        '• Confidence intervals may be wide',
        '• Consider collecting additional data',
        sep = '\n'
    )
)
```

#### 5. Include Numeric Details

```r
# Vague
'Some observations removed'

# Precise
sprintf('%d of %d observations (%.1f%%) removed due to missing data',
        n_removed, n_total, (n_removed/n_total)*100)

# Complete context
sprintf(
    'Data Exclusions:\n• Total observations: %d\n• Missing outcome: %d\n• Missing predictors: %d\n• Analysis sample: %d (%.1f%%)',
    n_total, n_missing_outcome, n_missing_predictors, n_complete, (n_complete/n_total)*100
)
```

### Formatting Guidelines

#### Line Breaks and Structure

```r
# Single-line notice
notice$setContent('Analysis completed successfully.')

# Multi-line with '\n'
notice$setContent('Warning: Small sample size.\nResults should be interpreted cautiously.')

# Structured with paste()
message <- paste(
    'Assumption Violation Detected:',
    '',
    'The proportional hazards assumption is violated for:',
    '• Age (p = 0.023)',
    '• Treatment (p = 0.041)',
    '',
    'Consider stratified analysis or time-varying covariates.',
    sep = '\n'
)
notice$setContent(message)
```

#### Bullet Points and Lists

```r
# Using bullet characters
recommendations <- paste(
    'Recommendations for this analysis:',
    '• Increase sample size if possible',
    '• Consider bootstrapping for confidence intervals',
    '• Report effect sizes in addition to p-values',
    '• Document all exclusion criteria',
    sep = '\n'
)

# Using numbered lists
steps <- paste(
    'To improve analysis:',
    '1. Check for data entry errors',
    '2. Verify variable distributions',
    '3. Consider transformation if needed',
    '4. Review outliers and influential points',
    sep = '\n'
)
```

#### Avoid HTML in Notices

**Important**: According to jamovi best practices and official documentation, notices should avoid HTML formatting.

**From dev.jamovi.org:** "Avoid using HTML in the content of your notices."

**Don't do this**:
```r
# Avoid HTML tags
notice$setContent('<b>Error:</b> Invalid data detected')
notice$setContent('<ul><li>Item 1</li><li>Item 2</li></ul>')
```

**Do this instead**:
```r
# Use plain text with proper formatting
notice$setContent('Error: Invalid data detected')
notice$setContent('Important points:\n• Item 1\n• Item 2')
```

### Internationalization Support

For modules that support multiple languages, use the `..()` function and `jmvcore::format()`:

```r
# From jamovi/jmv examples

# Simple internationalized message
notice$setContent(.("The data is weighted"))

# With dynamic content using format
notice$setContent(
    jmvcore::format(.("The data is weighted by the variable {}"),
                    self$options$counts)
)

# Multiple placeholders
notice$setContent(
    jmvcore::format(.("Found {} observations with {} events"),
                    n_total, n_events)
)

# Complex message with paste
message <- paste(
    .("Model fit issue detected!"),
    .("It appears that your model has a singularity problem."),
    .("This usually means you have too many variables for your sample size."),
    sep = "\n"
)
notice$setContent(message)
```

**Benefits:**
- Messages automatically translated based on user locale
- Maintains consistency with jamovi interface language
- Supports parameterized messages with dynamic content

### Alternative Constructor Parameters

The Notice constructor accepts parameters in different orders:

```r
# Full parameter names (most explicit)
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'myNotice',
    type = jmvcore::NoticeType$WARNING
)

# Positional parameters (from conttables.b.R)
notice <- jmvcore::Notice$new(
    self$options,           # options (first parameter)
    name = 'myNotice',
    type = jmvcore::NoticeType$WARNING
)

# With content in constructor (from principal.b.R)
notice <- jmvcore::Notice$new(
    self$options,
    type = jmvcore::NoticeType$WARNING,
    name = '.weights',
    content = warningMsg    # Set content directly
)
```

**Recommendation:** Use the explicit named parameter style for clarity, unless content is known at creation time.

---

## 5. Advanced Notice Patterns

### Helper Function Pattern (from jamovi/jmv)

The jamovi core team uses helper functions to standardize notice creation. This pattern is recommended for consistency:

```r
# From jamovi/jmv/R/utils.R
setAnalysisNotice <- function(self, message, name, type) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = name,
        type = type
    )
    notice$setContent(message)
    self$results$insert(1, notice)
}

# Usage in your analysis
.run = function() {
    if (some_error_condition) {
        setAnalysisNotice(
            self,
            message = "Your error message here",
            name = "errorNotice",
            type = jmvcore::NoticeType$ERROR
        )
        return()
    }
}
```

### Specific Warning Functions (from utilsanova.R)

Create specific helper functions for common warnings:

```r
# From jamovi/jmv/R/utilsanova.R
setSingularityWarning <- function(self) {
    message <- paste(
        .("Model fit issue detected!"),
        .("It appears that your model has a 'singularity' problem. This usually means...")
    )

    setAnalysisNotice(
        self,
        message = message,
        name = "refLevelWarning",
        type = jmvcore::NoticeType$STRONG_WARNING
    )
}

# Usage
.run = function() {
    if (is_singular_design) {
        setSingularityWarning(self)
        return()
    }
}
```

**Benefits of Helper Functions:**
- Consistent notice creation across your module
- Easier to update notice behavior module-wide
- More readable code in `.run()` function
- Centralized message management
- Easier internationalization

### Conditional Notice Creation (from principal.b.R)

Always check conditions before creating notices to avoid empty or unnecessary messages:

```r
# From vijPlots/R/principal.b.R
warningMsg <- ""

# Accumulate warnings during analysis
if (has_correlation_issue) {
    warningMsg <- paste(warningMsg,
        "The correlation matrix is not positive definite. Computations may not be accurate.",
        sep = "\n")
}

if (rotation_failed) {
    warningMsg <- paste(warningMsg,
        "Rotation method failed. Using unrotated solution.",
        sep = "\n")
}

# Only create notice if there are warnings
if (warningMsg != "") {
    weightsNotice <- jmvcore::Notice$new(
        self$options,
        type = jmvcore::NoticeType$WARNING,
        name = '.weights',
        content = warningMsg
    )
    self$results$insert(1, weightsNotice)
}
```

**Key Pattern:**
- Build up warning messages as you encounter issues
- Check if message is non-empty before creating notice
- This avoids creating empty notices that confuse users

### Conditional Notices

#### Based on Data Characteristics

```r
# Sample size-dependent warnings
.run = function() {

    n_total <- nrow(self$data)
    n_events <- sum(self$data[[self$options$event]], na.rm = TRUE)

    # Multiple threshold checks
    if (n_events < 10) {
        # Critical error - cannot proceed
        errorNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'insufficientEvents',
            type = jmvcore::NoticeType$ERROR
        )
        errorNotice$setContent(
            sprintf('Insufficient events for survival analysis. Found %d events, minimum 10 required.', n_events)
        )
        self$results$insert(1, errorNotice)
        return()  # Stop analysis

    } else if (n_events < 20) {
        # Strong warning - proceed with caution
        warningNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'limitedEvents',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        warningNotice$setContent(
            sprintf('Limited events (n=%d) may produce unstable estimates. Results should be interpreted very cautiously.', n_events)
        )
        self$results$insert(1, warningNotice)

    } else if (n_events < 50) {
        # Mild warning - inform user
        infoNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'moderateEvents',
            type = jmvcore::NoticeType$WARNING
        )
        infoNotice$setContent(
            sprintf('Moderate number of events (n=%d). Confidence intervals may be wide.', n_events)
        )
        self$results$insert(1, infoNotice)
    }

    # Continue with analysis...
}
```

#### Based on User Options

```r
# Warn about option combinations
.run = function() {

    # Check for problematic option combinations
    if (self$options$adjustForAge && is.null(self$options$ageVariable)) {

        optionNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'missingAgeVariable',
            type = jmvcore::NoticeType$ERROR
        )
        optionNotice$setContent(
            'Age adjustment requested but no age variable specified. Please select age variable or disable age adjustment.'
        )
        self$results$insert(1, optionNotice)
        return()
    }

    # Inform about automatic adjustments
    if (self$options$automaticAdjustment) {

        autoNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'automaticAdjustments',
            type = jmvcore::NoticeType$INFO
        )

        adjustments_made <- c()
        if (detected_outliers) adjustments_made <- c(adjustments_made, 'outlier exclusion')
        if (applied_transformation) adjustments_made <- c(adjustments_made, 'log transformation')

        autoNotice$setContent(
            sprintf('Automatic adjustments applied: %s', paste(adjustments_made, collapse = ', '))
        )
        self$results$insert(1, autoNotice)
    }
}
```

#### Based on Statistical Results

```r
# Post-analysis warnings
.run = function() {

    # Perform analysis
    cox_model <- coxph(formula, data = self$data)
    ph_test <- cox.zph(cox_model)

    # Check assumptions
    if (any(ph_test$table[, "p"] < 0.05)) {

        violatedVars <- rownames(ph_test$table)[ph_test$table[, "p"] < 0.05]

        assumptionNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'assumptionViolation',
            type = jmvcore::NoticeType$STRONG_WARNING
        )

        message <- paste(
            'Proportional Hazards Assumption Violated:',
            '',
            sprintf('The following variables violate the proportional hazards assumption (p < 0.05):'),
            paste('•', violatedVars, collapse = '\n'),
            '',
            'Consider:',
            '• Stratified analysis',
            '• Time-varying covariates',
            '• Alternative models',
            sep = '\n'
        )

        assumptionNotice$setContent(message)
        self$results$insert(1, assumptionNotice)
    }
}
```

### Progressive Notices

Show different notices based on analysis progress:

```r
.run = function() {

    # Stage 1: Data validation
    validation_result <- validate_data(self$data, self$options)

    if (!validation_result$valid) {
        validationNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'validationFailed',
            type = jmvcore::NoticeType$ERROR
        )
        validationNotice$setContent(
            paste('Data Validation Failed:', validation_result$message, sep = '\n')
        )
        self$results$insert(1, validationNotice)
        return()
    }

    # Stage 2: Preliminary checks
    if (validation_result$warnings) {
        warningNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'dataWarnings',
            type = jmvcore::NoticeType$WARNING
        )
        warningNotice$setContent(validation_result$warning_message)
        self$results$insert(1, warningNotice)
    }

    # Stage 3: Analysis
    tryCatch({
        result <- perform_analysis(self$data, self$options)

        # Stage 4: Post-analysis notices
        if (result$convergence_issues) {
            convergenceNotice <- jmvcore::Notice$new(
                options = self$options,
                name = 'convergenceIssue',
                type = jmvcore::NoticeType$STRONG_WARNING
            )
            convergenceNotice$setContent(
                'Model convergence was slow. Results may be unreliable. Consider simpler model.'
            )
            self$results$insert(1, convergenceNotice)
        }

        # Success notice (optional)
        successNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'analysisComplete',
            type = jmvcore::NoticeType$INFO
        )
        successNotice$setContent(
            sprintf('Analysis completed successfully using %d observations.', nrow(self$data))
        )
        self$results$insert(999, successNotice)

    }, error = function(e) {
        errorNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'analysisFailed',
            type = jmvcore::NoticeType$ERROR
        )
        errorNotice$setContent(
            sprintf('Analysis failed: %s', e$message)
        )
        self$results$insert(1, errorNotice)
    })
}
```

### Dynamic Notice Content

```r
# Generate notice content based on multiple factors
generate_sample_size_notice <- function(n_total, n_complete, n_events, analysis_type) {

    # Determine severity
    if (n_complete < 10) {
        notice_type <- jmvcore::NoticeType$ERROR
        severity <- 'Critical'
    } else if (n_complete < 30) {
        notice_type <- jmvcore::NoticeType$STRONG_WARNING
        severity <- 'Serious'
    } else if (n_complete < 50) {
        notice_type <- jmvcore::NoticeType$WARNING
        severity <- 'Moderate'
    } else {
        return(NULL)  # No notice needed
    }

    # Build message components
    message_parts <- c(
        sprintf('%s Sample Size Issue:', severity),
        '',
        sprintf('• Total observations: %d', n_total),
        sprintf('• Complete cases: %d (%.1f%%)', n_complete, (n_complete/n_total)*100)
    )

    # Add analysis-specific information
    if (analysis_type == 'survival') {
        message_parts <- c(
            message_parts,
            sprintf('• Events: %d', n_events),
            sprintf('• Events per variable ratio: %.1f', n_events / n_predictors)
        )
    }

    # Add recommendations
    message_parts <- c(
        message_parts,
        '',
        'Implications:',
        if (severity == 'Critical') {
            '• Analysis cannot produce reliable results'
        } else if (severity == 'Serious') {
            '• Results should be considered very preliminary'
        } else {
            '• Confidence intervals may be wide'
        },
        '• Statistical power is limited',
        '• Consider collecting more data if possible'
    )

    return(list(
        type = notice_type,
        content = paste(message_parts, collapse = '\n')
    ))
}

# Use in analysis
.run = function() {
    notice_info <- generate_sample_size_notice(
        n_total = nrow(self$data),
        n_complete = sum(complete.cases(self$data)),
        n_events = sum(self$data$event),
        analysis_type = 'survival'
    )

    if (!is.null(notice_info)) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'sampleSizeNotice',
            type = notice_info$type
        )
        notice$setContent(notice_info$content)
        self$results$insert(1, notice)
    }
}
```

---

## 6. Clinical and Research Notice Strategies

### Clinical Safety Notices

```r
# Emphasize clinical implications
.run = function() {

    # Check for critical clinical thresholds
    auc_value <- calculate_auc(self$data)

    if (auc_value < 0.5) {
        # Worse than random
        clinicalNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'poorDiscrimination',
            type = jmvcore::NoticeType$ERROR
        )
        clinicalNotice$setContent(
            sprintf(
                paste(
                    'CRITICAL: Diagnostic Test Performance Below Chance Level',
                    '',
                    'AUC = %.3f (below 0.5)',
                    '',
                    'This indicates the test performs worse than random chance.',
                    'DO NOT use this model for clinical decision-making.',
                    '',
                    'Possible causes:',
                    '• Incorrect outcome coding (consider reversing)',
                    '• Fundamental model misspecification',
                    '• Inappropriate predictor selection',
                    sep = '\n'
                ),
                auc_value
            )
        )
        self$results$insert(1, clinicalNotice)
        return()

    } else if (auc_value < 0.7) {
        # Poor discrimination
        clinicalNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'limitedDiscrimination',
            type = jmvcore::NoticeType$STRONG_WARNING
        )
        clinicalNotice$setContent(
            sprintf(
                paste(
                    'Clinical Performance Warning',
                    '',
                    'AUC = %.3f indicates limited discriminative ability.',
                    '',
                    'Clinical Implications:',
                    '• Model has poor ability to distinguish outcomes',
                    '• High rates of misclassification expected',
                    '• Consider additional predictors or alternative approaches',
                    '',
                    'This model should NOT be used for clinical decision-making without further validation.',
                    sep = '\n'
                ),
                auc_value
            )
        )
        self$results$insert(1, clinicalNotice)
    }
}
```

### Research Integrity Notices

```r
# Transparent reporting of analysis decisions
.run = function() {

    # Document exclusions
    n_original <- nrow(self$data_original)
    n_final <- nrow(self$data)

    exclusions <- list()
    if (n_missing > 0) exclusions$missing <- n_missing
    if (n_outliers > 0) exclusions$outliers <- n_outliers
    if (n_duplicate > 0) exclusions$duplicates <- n_duplicate

    if (length(exclusions) > 0) {
        transparencyNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'dataExclusions',
            type = jmvcore::NoticeType$INFO
        )

        message_parts <- c(
            'Data Exclusions and Sample Composition:',
            '',
            sprintf('Original dataset: %d observations', n_original),
            sprintf('Final analysis sample: %d observations (%.1f%%)', n_final, (n_final/n_original)*100),
            ''
        )

        if (!is.null(exclusions$missing)) {
            message_parts <- c(message_parts,
                sprintf('• Missing data: %d excluded (%.1f%%)',
                        exclusions$missing, (exclusions$missing/n_original)*100))
        }
        if (!is.null(exclusions$outliers)) {
            message_parts <- c(message_parts,
                sprintf('• Outliers: %d excluded (%.1f%%)',
                        exclusions$outliers, (exclusions$outliers/n_original)*100))
        }
        if (!is.null(exclusions$duplicates)) {
            message_parts <- c(message_parts,
                sprintf('• Duplicates: %d excluded (%.1f%%)',
                        exclusions$duplicates, (exclusions$duplicates/n_original)*100))
        }

        message_parts <- c(message_parts,
            '',
            'Note: All exclusions should be documented in study protocol and publications.'
        )

        transparencyNotice$setContent(paste(message_parts, collapse = '\n'))
        self$results$insert(1, transparencyNotice)
    }
}
```

### Statistical Best Practice Notices

```r
# Guide users toward best practices
.run = function() {

    # Check if user performed multiple testing without adjustment
    if (self$options$multipleComparisons && self$options$adjustment == 'none') {

        bestPracticeNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'multipleTestingWarning',
            type = jmvcore::NoticeType$STRONG_WARNING
        )

        n_comparisons <- calculate_n_comparisons(self$options)

        bestPracticeNotice$setContent(
            sprintf(
                paste(
                    'Multiple Testing Without Adjustment',
                    '',
                    'You are performing %d comparisons without p-value adjustment.',
                    '',
                    'Risk: Inflated Type I error rate (false positives)',
                    'Expected false positives at α=0.05: %.1f',
                    '',
                    'Recommended Actions:',
                    '• Apply Bonferroni correction (most conservative)',
                    '• Use Holm or FDR methods (less conservative)',
                    '• Pre-specify primary comparison',
                    '• Report both adjusted and unadjusted values',
                    '',
                    'Statistical reporting guidelines recommend adjustment for multiple comparisons.',
                    sep = '\n'
                ),
                n_comparisons,
                n_comparisons * 0.05
            )
        )

        self$results$insert(1, bestPracticeNotice)
    }
}
```

### Methodological Guidance Notices

```r
# Educate users about methodology
.run = function() {

    # For complex analyses, explain what was done
    if (self$options$analysisMethod == 'bootstrap') {

        methodNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'bootstrapMethodology',
            type = jmvcore::NoticeType$INFO
        )

        methodNotice$setContent(
            sprintf(
                paste(
                    'Bootstrap Methodology Details:',
                    '',
                    'This analysis used bootstrap resampling to estimate confidence intervals:',
                    '',
                    'Method: %s',
                    'Resamples: %d',
                    'Confidence level: %.0f%%',
                    '',
                    'Interpretation:',
                    '• Bootstrap CIs are robust to non-normality',
                    '• More reliable for small samples than asymptotic CIs',
                    '• Wider intervals indicate greater uncertainty',
                    '',
                    'Reference: Efron & Tibshirani (1993). An Introduction to the Bootstrap.',
                    sep = '\n'
                ),
                self$options$bootstrapMethod,
                self$options$nBootstrap,
                self$options$confLevel * 100
            )
        )

        self$results$insert(2, methodNotice)
    }
}
```

---

## 7. Notice Positioning and Management

### Understanding Result Positions

Results in jamovi are ordered by position index:

```
Position 1:   First item (top)
Position 2:   Second item
Position 3:   Third item
...
Position 999: Last item (bottom)
```

### Strategic Notice Placement

#### Top Notices - Critical Information First

```r
# Put errors and critical warnings at the very top
.run = function() {

    if (critical_error) {
        errorNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'criticalError',
            type = jmvcore::NoticeType$ERROR
        )
        errorNotice$setContent('Critical error message')
        self$results$insert(1, errorNotice)  # Position 1 = top
        return()  # Stop processing
    }
}
```

#### Mid-Position Notices - Context Before Results

```r
# Place explanatory notices just before related tables
.run = function() {

    # First, create and insert the table (position 3)
    # Then, add contextual notice just before it

    contextNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'tableContext',
        type = jmvcore::NoticeType$INFO
    )
    contextNotice$setContent('The following table shows...')
    self$results$insert(2, contextNotice)  # Just before table at position 3
}
```

#### Bottom Notices - Summary Information

```r
# Place summary or success notices at bottom
.run = function() {

    # After all analysis complete
    summaryNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'analysisSummary',
        type = jmvcore::NoticeType$INFO
    )

    summaryNotice$setContent(
        sprintf(
            'Analysis Summary: %d observations analyzed, %d statistical tests performed, computation time: %.2f seconds',
            n_obs, n_tests, elapsed_time
        )
    )

    self$results$insert(999, summaryNotice)  # Bottom position
}
```

### Managing Multiple Notices

#### Priority-Based Insertion

```r
# Insert notices in order of importance
.run = function() {

    notices <- list()

    # Collect all notices
    if (has_error) {
        notices$error <- create_error_notice()
    }
    if (has_strong_warning) {
        notices$strong_warning <- create_strong_warning()
    }
    if (has_warning) {
        notices$warning <- create_warning()
    }
    if (has_info) {
        notices$info <- create_info_notice()
    }

    # Insert in priority order (error first, info last)
    position <- 1
    if (!is.null(notices$error)) {
        self$results$insert(position, notices$error)
        position <- position + 1
    }
    if (!is.null(notices$strong_warning)) {
        self$results$insert(position, notices$strong_warning)
        position <- position + 1
    }
    if (!is.null(notices$warning)) {
        self$results$insert(position, notices$warning)
        position <- position + 1
    }
    if (!is.null(notices$info)) {
        self$results$insert(position, notices$info)
        position <- position + 1
    }
}
```

#### Conditional Notice Removal

```r
# Remove notice if condition resolved
.run = function() {

    # Previously, a warning notice may have been added
    # Check if condition still exists

    if (!condition_exists) {
        # Notice is no longer needed
        # jamovi automatically handles notice lifecycle
        # Don't add the notice this time
    } else {
        # Condition still exists, add notice
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'conditionalWarning',
            type = jmvcore::NoticeType$WARNING
        )
        notice$setContent('Condition still exists')
        self$results$insert(1, notice)
    }
}
```

### Notice Lifecycle

Notices are managed automatically by jamovi:

```r
# Notices are recreated each time .run() executes
.run = function() {

    # Step 1: jamovi clears previous results (including notices)

    # Step 2: Your code runs and creates new notices
    if (should_show_notice) {
        notice <- jmvcore::Notice$new(
            options = self$options,
            name = 'myNotice',
            type = jmvcore::NoticeType$INFO
        )
        notice$setContent('Current message')
        self$results$insert(1, notice)
    }

    # Step 3: User sees updated notices
}

# No need to manually remove or update notices
# Just decide whether to create them each time
```

---

## 8. Error Communication Patterns

### Graceful Error Handling

#### Comprehensive Error Catching

```r
.run = function() {

    # Wrap entire analysis in error handling
    tryCatch({

        # Attempt analysis
        result <- perform_complex_analysis(self$data, self$options)

        # Populate results
        populate_tables(result)

    }, error = function(e) {

        # Create detailed error notice
        errorNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'analysisError',
            type = jmvcore::NoticeType$ERROR
        )

        # Provide helpful error message
        error_message <- paste(
            'Analysis Error:',
            '',
            sprintf('The analysis encountered an error: %s', e$message),
            '',
            'Common causes:',
            '• Insufficient data for selected analysis',
            '• Invalid variable types',
            '• Numerical instability in calculations',
            '',
            'Suggested actions:',
            '• Review data quality and variable selection',
            '• Try simpler model specification',
            '• Check for extreme outliers',
            '• Ensure adequate sample size',
            sep = '\n'
        )

        errorNotice$setContent(error_message)
        self$results$insert(1, errorNotice)

        # Optionally log technical details for debugging
        cat(sprintf('Technical error details: %s\n', e$message))
        cat(sprintf('Call stack: %s\n', paste(sys.calls(), collapse = ' -> ')))
    })
}
```

#### Specific Error Conditions

```r
# Handle specific errors with targeted messages
perform_analysis_safely <- function(data, options) {

    # Check 1: Data structure
    if (is.null(data) || nrow(data) == 0) {
        stop_with_notice(
            'No data available for analysis. Please provide dataset.',
            type = jmvcore::NoticeType$ERROR
        )
    }

    # Check 2: Required variables
    required_vars <- c(options$outcome, options$time, options$event)
    missing_vars <- required_vars[!required_vars %in% names(data)]

    if (length(missing_vars) > 0) {
        stop_with_notice(
            sprintf('Required variables not found: %s', paste(missing_vars, collapse = ', ')),
            type = jmvcore::NoticeType$ERROR
        )
    }

    # Check 3: Variable types
    if (!is.numeric(data[[options$time]])) {
        stop_with_notice(
            sprintf('Time variable "%s" must be numeric. Current type: %s',
                    options$time, class(data[[options$time]])),
            type = jmvcore::NoticeType$ERROR
        )
    }

    # Check 4: Sufficient events
    n_events <- sum(data[[options$event]], na.rm = TRUE)
    if (n_events < 10) {
        stop_with_notice(
            sprintf('Insufficient events for analysis. Found %d events, need at least 10.', n_events),
            type = jmvcore::NoticeType$ERROR
        )
    }

    # If all checks pass, proceed
    return(perform_core_analysis(data, options))
}
```

### Warning Escalation

```r
# Escalate warnings based on severity
check_assumptions <- function(model, data) {

    warnings <- list()

    # Check assumption 1
    if (!check_linearity(model)) {
        warnings$linearity <- list(
            severity = 'warning',
            message = 'Linearity assumption questionable for some predictors'
        )
    }

    # Check assumption 2
    if (!check_homoscedasticity(model)) {
        warnings$variance <- list(
            severity = 'strong_warning',
            message = 'Heteroscedasticity detected. Consider robust standard errors.'
        )
    }

    # Check assumption 3
    if (!check_independence(data)) {
        warnings$independence <- list(
            severity = 'error',
            message = 'Independence assumption violated. Clustered data detected.'
        )
    }

    return(warnings)
}

.run = function() {

    model <- fit_model(self$data)
    warnings <- check_assumptions(model, self$data)

    # Create notices for each warning
    position <- 1
    for (assumption in names(warnings)) {
        warning_info <- warnings[[assumption]]

        notice_type <- switch(warning_info$severity,
            'error' = jmvcore::NoticeType$ERROR,
            'strong_warning' = jmvcore::NoticeType$STRONG_WARNING,
            'warning' = jmvcore::NoticeType$WARNING,
            jmvcore::NoticeType$INFO
        )

        notice <- jmvcore::Notice$new(
            options = self$options,
            name = paste0('assumption_', assumption),
            type = notice_type
        )

        notice$setContent(warning_info$message)
        self$results$insert(position, notice)
        position <- position + 1

        # Stop if error-level assumption violation
        if (warning_info$severity == 'error') {
            return()
        }
    }
}
```

---

## 9. Integration with jamovi Module Architecture

### Notice Integration with .r.yaml

Notices are dynamically created in .b.R and don't require .r.yaml definition. However, you can add placeholder notices in .r.yaml for consistent result structure:

```yaml
# In your .r.yaml file
items:
  # Note: Notices created in .b.R do not need to be defined here
  # This is just for documentation purposes

  - name: dataValidationTable
    title: Data Validation
    type: Table
    rows: 0
    columns:
      # ... table definition

  # Notices will be inserted dynamically by .b.R code
  # Position control via insert() method
```

### Notice Integration with .init()

```r
# In your .b.R file

classificationClass <- R6::R6Class(
    "classificationClass",
    inherit = classificationBase,
    private = list(

        .init = function() {
            # Initialize can prepare notice infrastructure
            # But notices are typically created in .run()

            # No initialization needed for notices
            # They are created fresh each .run() cycle
        },

        .run = function() {

            # Validate early, notice early
            if (!self$validate_inputs()) {
                # Create validation notice
                validationNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'validationError',
                    type = jmvcore::NoticeType$ERROR
                )
                validationNotice$setContent(self$validation_message)
                self$results$insert(1, validationNotice)
                return()
            }

            # Continue with analysis...
        },

        validate_inputs = function() {
            # Validation logic
            # Returns TRUE/FALSE
            # Sets self$validation_message if FALSE
        }
    )
)
```

### Notice Integration with Options

```r
# Make notices responsive to user options

.run = function() {

    # Option 1: User controls notice visibility
    if (self$options$showDetailedWarnings) {
        # Show all warnings
        show_all_warnings(self)
    } else {
        # Show only critical notices
        show_critical_notices_only(self)
    }

    # Option 2: User controls verbosity level
    verbosity <- self$options$noticeVerbosity  # 'minimal', 'standard', 'detailed'

    if (verbosity == 'detailed') {
        # Show comprehensive information
        add_detailed_methodology_notice(self)
        add_assumption_check_notices(self)
        add_interpretation_guidance_notices(self)
    } else if (verbosity == 'standard') {
        # Show important warnings only
        add_critical_warnings(self)
    } else {
        # Minimal: errors only
        add_errors_only(self)
    }
}
```

### Notice Integration with State Management

```r
# Track notice state across .run() calls if needed

.run = function() {

    # Get previous state (if any)
    previous_state <- self$results$state

    # Check if data has changed since last run
    current_data_hash <- digest::digest(self$data)

    if (!is.null(previous_state) &&
        previous_state$data_hash == current_data_hash &&
        previous_state$had_warning) {

        # Data unchanged, warning persists
        persistentNotice <- jmvcore::Notice$new(
            options = self$options,
            name = 'persistentWarning',
            type = jmvcore::NoticeType$WARNING
        )
        persistentNotice$setContent(
            'Warning: This issue persists from previous analysis. Data or options need adjustment.'
        )
        self$results$insert(1, persistentNotice)
    }

    # Update state for next run
    new_state <- list(
        data_hash = current_data_hash,
        had_warning = check_for_warnings(self)
    )
    self$results$setState(new_state)
}
```

---

## 10. Complete Examples

### Example 1: Survival Analysis with Comprehensive Notices

```r
# Complete survival analysis implementation with notices

survivalAnalysisClass <- R6::R6Class(
    "survivalAnalysisClass",
    inherit = survivalAnalysisBase,
    private = list(

        .run = function() {

            # Early return if no variables selected
            if (is.null(self$options$time) || is.null(self$options$event)) {
                inputNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'needVariables',
                    type = jmvcore::NoticeType$INFO
                )
                inputNotice$setContent(
                    'Please select time and event variables to begin survival analysis.'
                )
                self$results$insert(1, inputNotice)
                return()
            }

            # Get data
            data <- self$data
            time_var <- self$options$time
            event_var <- self$options$event
            group_var <- self$options$group

            # Validate data
            validation <- self$validate_survival_data(data, time_var, event_var, group_var)

            if (!validation$valid) {
                # Show error notice
                errorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'validationError',
                    type = jmvcore::NoticeType$ERROR
                )
                errorNotice$setContent(validation$message)
                self$results$insert(1, errorNotice)
                return()
            }

            # Show warnings if any
            if (!is.null(validation$warnings)) {
                for (i in seq_along(validation$warnings)) {
                    warningNotice <- jmvcore::Notice$new(
                        options = self$options,
                        name = paste0('warning_', i),
                        type = jmvcore::NoticeType$WARNING
                    )
                    warningNotice$setContent(validation$warnings[i])
                    self$results$insert(i, warningNotice)
                }
            }

            # Perform survival analysis
            tryCatch({

                # Create survival formula
                if (is.null(group_var)) {
                    formula <- as.formula(sprintf('Surv(%s, %s) ~ 1', time_var, event_var))
                } else {
                    formula <- as.formula(sprintf('Surv(%s, %s) ~ %s', time_var, event_var, group_var))
                }

                # Fit Kaplan-Meier
                km_fit <- survfit(formula, data = data)

                # Check for issues
                n_events <- sum(data[[event_var]], na.rm = TRUE)

                if (n_events < 20) {
                    eventNotice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'limitedEvents',
                        type = jmvcore::NoticeType$STRONG_WARNING
                    )
                    eventNotice$setContent(
                        sprintf(
                            paste(
                                'Limited Event Count',
                                '',
                                'This analysis includes only %d events, which may result in:',
                                '• Wide confidence intervals',
                                '• Unstable estimates',
                                '• Limited statistical power',
                                '',
                                'Interpretation should be cautious. Consider:',
                                '• Combining groups if appropriate',
                                '• Using exact methods',
                                '• Collecting additional data',
                                sep = '\n'
                            ),
                            n_events
                        )
                    )
                    self$results$insert(1, eventNotice)
                }

                # Populate results tables
                self$populate_survival_table(km_fit)

                # Add methodology notice
                methodNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'methodology',
                    type = jmvcore::NoticeType$INFO
                )
                methodNotice$setContent(
                    paste(
                        'Analysis Methodology:',
                        '',
                        '• Survival estimates: Kaplan-Meier method',
                        '• Confidence intervals: log-log transformation',
                        '• Confidence level: 95%',
                        '• Censoring: Right-censored data',
                        '',
                        sprintf('Analysis included %d observations with %d events (%.1f%% event rate).',
                                nrow(data), n_events, (n_events/nrow(data))*100),
                        sep = '\n'
                    )
                )
                self$results$insert(999, methodNotice)

            }, error = function(e) {

                # Analysis failed
                analysisErrorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisError',
                    type = jmvcore::NoticeType$ERROR
                )

                # Provide helpful error message
                error_msg <- paste(
                    'Survival Analysis Error:',
                    '',
                    sprintf('Analysis failed with error: %s', e$message),
                    '',
                    'Possible causes:',
                    '• All observations censored (no events)',
                    '• Negative or zero survival times',
                    '• Invalid group variable',
                    '',
                    'Please check your data and variable selections.',
                    sep = '\n'
                )

                analysisErrorNotice$setContent(error_msg)
                self$results$insert(1, analysisErrorNotice)
            })
        },

        validate_survival_data = function(data, time_var, event_var, group_var) {

            result <- list(valid = TRUE, warnings = c())

            # Check time variable
            if (!is.numeric(data[[time_var]])) {
                result$valid <- FALSE
                result$message <- sprintf('Time variable "%s" must be numeric.', time_var)
                return(result)
            }

            # Check for non-positive times
            if (any(data[[time_var]] <= 0, na.rm = TRUE)) {
                result$warnings <- c(result$warnings,
                    'Some survival times are zero or negative. These will be excluded.')
            }

            # Check event variable
            if (!all(unique(data[[event_var]]) %in% c(0, 1, NA))) {
                result$valid <- FALSE
                result$message <- sprintf('Event variable "%s" must be binary (0/1).', event_var)
                return(result)
            }

            # Check for missing data
            missing_time <- sum(is.na(data[[time_var]]))
            missing_event <- sum(is.na(data[[event_var]]))

            if (missing_time > 0 || missing_event > 0) {
                total_missing <- length(unique(c(
                    which(is.na(data[[time_var]])),
                    which(is.na(data[[event_var]]))
                )))

                result$warnings <- c(result$warnings,
                    sprintf('%d observations excluded due to missing time or event data.', total_missing))
            }

            # Check minimum events
            n_events <- sum(data[[event_var]], na.rm = TRUE)
            if (n_events < 10) {
                result$valid <- FALSE
                result$message <- sprintf(
                    'Insufficient events for survival analysis. Found %d events, minimum 10 required.',
                    n_events
                )
                return(result)
            }

            return(result)
        }
    )
)
```

### Example 2: Diagnostic Test Evaluation with Progressive Notices

```r
# Diagnostic accuracy analysis with comprehensive user guidance

diagnosticTestClass <- R6::R6Class(
    "diagnosticTestClass",
    inherit = diagnosticTestBase,
    private = list(

        .run = function() {

            # Check for variable selection
            if (is.null(self$options$testResult) || is.null(self$options$goldStandard)) {
                welcomeNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'welcome',
                    type = jmvcore::NoticeType$INFO
                )
                welcomeNotice$setContent(
                    paste(
                        'Diagnostic Test Evaluation',
                        '',
                        'This analysis calculates diagnostic accuracy metrics including:',
                        '• Sensitivity and Specificity',
                        '• Positive and Negative Predictive Values',
                        '• Likelihood Ratios',
                        '• ROC curve and AUC',
                        '',
                        'To begin, select:',
                        '1. Test Result variable (binary or continuous)',
                        '2. Gold Standard reference variable (binary)',
                        sep = '\n'
                    )
                )
                self$results$insert(1, welcomeNotice)
                return()
            }

            # Get data
            data <- self$data
            test_var <- self$options$testResult
            gold_var <- self$options$goldStandard

            # Stage 1: Data validation
            validation <- self$validate_diagnostic_data(data, test_var, gold_var)

            if (!validation$valid) {
                errorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'validationError',
                    type = jmvcore::NoticeType$ERROR
                )
                errorNotice$setContent(validation$message)
                self$results$insert(1, errorNotice)
                return()
            }

            # Stage 2: Check data quality
            prevalence <- mean(data[[gold_var]], na.rm = TRUE)

            # Warn about extreme prevalence
            if (prevalence < 0.05 || prevalence > 0.95) {
                prevalenceNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'extremePrevalence',
                    type = jmvcore::NoticeType$STRONG_WARNING
                )
                prevalenceNotice$setContent(
                    sprintf(
                        paste(
                            'Extreme Disease Prevalence',
                            '',
                            'Sample prevalence: %.1f%%',
                            '',
                            'Very low or high prevalence affects:',
                            '• Predictive value estimates (PPV and NPV)',
                            '• Clinical generalizability',
                            '• Statistical precision',
                            '',
                            'Considerations:',
                            '• Ensure sample is representative of target population',
                            '• Report prevalence explicitly',
                            '• Consider prevalence-adjusted metrics',
                            '• Interpret predictive values cautiously',
                            sep = '\n'
                        ),
                        prevalence * 100
                    )
                )
                self$results$insert(1, prevalenceNotice)
            }

            # Stage 3: Perform analysis
            tryCatch({

                # Calculate diagnostic metrics
                metrics <- calculate_diagnostic_metrics(
                    data[[test_var]],
                    data[[gold_var]]
                )

                # Populate results
                self$populate_metrics_table(metrics)

                # Stage 4: Clinical interpretation
                interpretation <- interpret_diagnostic_performance(metrics)

                if (interpretation$clinical_concern) {
                    clinicalNotice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'clinicalConcern',
                        type = jmvcore::NoticeType$WARNING
                    )
                    clinicalNotice$setContent(interpretation$message)
                    self$results$insert(2, clinicalNotice)
                }

                # Stage 5: Success summary
                summaryNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'analysisSummary',
                    type = jmvcore::NoticeType$INFO
                )
                summaryNotice$setContent(
                    sprintf(
                        paste(
                            'Analysis Complete',
                            '',
                            'Diagnostic metrics calculated for:',
                            '• Sample size: %d',
                            '• Disease positive: %d (%.1f%%)',
                            '• Disease negative: %d (%.1f%%)',
                            '',
                            'AUC: %.3f (%s discrimination)',
                            '',
                            'Note: Predictive values (PPV/NPV) are specific to this prevalence.',
                            sep = '\n'
                        ),
                        nrow(data),
                        sum(data[[gold_var]]), prevalence * 100,
                        sum(!data[[gold_var]]), (1-prevalence) * 100,
                        metrics$auc,
                        ifelse(metrics$auc > 0.8, 'excellent',
                               ifelse(metrics$auc > 0.7, 'acceptable', 'poor'))
                    )
                )
                self$results$insert(999, summaryNotice)

            }, error = function(e) {
                errorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'calculationError',
                    type = jmvcore::NoticeType$ERROR
                )
                errorNotice$setContent(
                    sprintf('Diagnostic calculation failed: %s', e$message)
                )
                self$results$insert(1, errorNotice)
            })
        },

        validate_diagnostic_data = function(data, test_var, gold_var) {

            result <- list(valid = TRUE)

            # Check test variable exists
            if (!test_var %in% names(data)) {
                result$valid <- FALSE
                result$message <- sprintf('Test variable "%s" not found in data.', test_var)
                return(result)
            }

            # Check gold standard variable
            if (!gold_var %in% names(data)) {
                result$valid <- FALSE
                result$message <- sprintf('Gold standard variable "%s" not found in data.', gold_var)
                return(result)
            }

            # Check gold standard is binary
            unique_values <- unique(data[[gold_var]][!is.na(data[[gold_var]])])
            if (length(unique_values) != 2) {
                result$valid <- FALSE
                result$message <- sprintf(
                    'Gold standard variable must be binary. Found %d unique values.',
                    length(unique_values)
                )
                return(result)
            }

            # Check minimum sample size per group
            n_positive <- sum(data[[gold_var]], na.rm = TRUE)
            n_negative <- sum(!data[[gold_var]], na.rm = TRUE)

            if (n_positive < 10 || n_negative < 10) {
                result$valid <- FALSE
                result$message <- sprintf(
                    paste(
                        'Insufficient sample size for diagnostic analysis.',
                        'Minimum 10 observations per group required.',
                        'Current: %d positive, %d negative.',
                        sep = '\n'
                    ),
                    n_positive, n_negative
                )
                return(result)
            }

            return(result)
        },

        interpret_diagnostic_performance = function(metrics) {

            result <- list(clinical_concern = FALSE)

            # Check for poor discriminative ability
            if (metrics$auc < 0.7) {
                result$clinical_concern <- TRUE
                result$message <- sprintf(
                    paste(
                        'Limited Diagnostic Performance',
                        '',
                        'AUC = %.3f indicates limited discriminative ability.',
                        '',
                        'Clinical Implications:',
                        '• Sensitivity: %.1f%% (%.1f%% of cases detected)',
                        '• Specificity: %.1f%% (%.1f%% false positive rate)',
                        '',
                        'This test may not be suitable for clinical decision-making.',
                        'Consider alternative tests or combining with additional markers.',
                        sep = '\n'
                    ),
                    metrics$auc,
                    metrics$sensitivity * 100, metrics$sensitivity * 100,
                    metrics$specificity * 100, (1-metrics$specificity) * 100
                )
            }

            # Check for imbalanced sensitivity/specificity
            if (abs(metrics$sensitivity - metrics$specificity) > 0.3) {
                result$clinical_concern <- TRUE
                result$message <- sprintf(
                    paste(
                        'Imbalanced Test Performance',
                        '',
                        'Large difference between sensitivity (%.1f%%) and specificity (%.1f%%).',
                        '',
                        'Interpretation:',
                        if (metrics$sensitivity > metrics$specificity) {
                            '• Good at detecting disease (high sensitivity)'
                        } else {
                            '• Good at ruling out disease (high specificity)'
                        },
                        if (metrics$sensitivity > metrics$specificity) {
                            '• May generate many false positives'
                        } else {
                            '• May miss many true cases'
                        },
                        '',
                        'Consider the clinical context and costs of false positives vs false negatives.',
                        sep = '\n'
                    ),
                    metrics$sensitivity * 100,
                    metrics$specificity * 100
                )
            }

            return(result)
        }
    )
)
```

### Example 3: Sample Size Planning with Educational Notices

```r
# Sample size calculator with extensive user guidance

sampleSizeClass <- R6::R6Class(
    "sampleSizeClass",
    inherit = sampleSizeBase,
    private = list(

        .run = function() {

            # Educational introduction
            if (is.null(self$options$effectSize)) {
                educationalNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'education',
                    type = jmvcore::NoticeType$INFO
                )
                educationalNotice$setContent(
                    paste(
                        'Sample Size Planning for Clinical Studies',
                        '',
                        'This tool calculates the required sample size based on:',
                        '',
                        'Key Parameters:',
                        '• Effect Size: The clinically meaningful difference to detect',
                        '• Power: Probability of detecting a true effect (typically 80-90%)',
                        '• Alpha: Type I error rate (typically 5%)',
                        '• Study Design: One-sample, two-sample, or paired',
                        '',
                        'Getting Started:',
                        '1. Specify expected effect size (from pilot data or literature)',
                        '2. Set desired power (recommend 80% minimum)',
                        '3. Choose appropriate alpha level',
                        '4. Account for expected dropout/attrition',
                        '',
                        'Best Practice: Calculate sample size before data collection!',
                        sep = '\n'
                    )
                )
                self$results$insert(1, educationalNotice)
                return()
            }

            # Get parameters
            effect_size <- self$options$effectSize
            power <- self$options$power
            alpha <- self$options$alpha
            design <- self$options$studyDesign

            # Validate parameters
            if (effect_size <= 0) {
                errorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'invalidEffect',
                    type = jmvcore::NoticeType$ERROR
                )
                errorNotice$setContent(
                    'Effect size must be greater than 0. Please specify a positive effect size.'
                )
                self$results$insert(1, errorNotice)
                return()
            }

            # Warn about small effect sizes
            if (effect_size < 0.3) {
                smallEffectNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'smallEffect',
                    type = jmvcore::NoticeType$WARNING
                )
                smallEffectNotice$setContent(
                    sprintf(
                        paste(
                            'Small Effect Size Specified',
                            '',
                            'Effect size = %.3f (considered small by Cohen\'s conventions)',
                            '',
                            'Implications:',
                            '• Large sample size will be required',
                            '• Consider feasibility of recruitment',
                            '• Ensure effect is clinically meaningful',
                            '• May need multi-center study',
                            '',
                            'Question to consider: Is this effect size clinically important?',
                            sep = '\n'
                        ),
                        effect_size
                    )
                )
                self$results$insert(1, smallEffectNotice)
            }

            # Calculate sample size
            tryCatch({

                n_required <- calculate_sample_size(
                    effect = effect_size,
                    power = power,
                    alpha = alpha,
                    design = design
                )

                # Adjust for attrition
                if (self$options$accountAttrition) {
                    attrition_rate <- self$options$attritionRate
                    n_adjusted <- ceiling(n_required / (1 - attrition_rate))

                    attritionNotice <- jmvcore::Notice$new(
                        options = self$options,
                        name = 'attritionAdjustment',
                        type = jmvcore::NoticeType$INFO
                    )
                    attritionNotice$setContent(
                        sprintf(
                            paste(
                                'Attrition Adjustment Applied',
                                '',
                                'Base sample size: %d',
                                'Expected attrition: %.0f%%',
                                'Adjusted sample size: %d',
                                '',
                                'Recommendation: Recruit %d participants to ensure %d complete cases.',
                                sep = '\n'
                            ),
                            n_required,
                            attrition_rate * 100,
                            n_adjusted,
                            n_adjusted, n_required
                        )
                    )
                    self$results$insert(2, attritionNotice)

                    n_final <- n_adjusted
                } else {
                    n_final <- n_required
                }

                # Populate results
                self$populate_sample_size_table(n_final, effect_size, power, alpha)

                # Provide context
                feasibilityNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'feasibility',
                    type = if (n_final > 500) jmvcore::NoticeType$WARNING else jmvcore::NoticeType$INFO
                )

                if (n_final > 500) {
                    feasibilityNotice$setContent(
                        sprintf(
                            paste(
                                'Large Sample Required: n = %d',
                                '',
                                'This sample size may present recruitment challenges.',
                                '',
                                'Options to consider:',
                                '• Multi-center collaboration',
                                '• Longer recruitment period',
                                '• Accepting lower power (e.g., 70-75%%)',
                                '• Using more efficient design (paired, crossover)',
                                '• Interim analysis with adaptive design',
                                '',
                                'Ensure adequate resources before initiating study.',
                                sep = '\n'
                            ),
                            n_final
                        )
                    )
                } else {
                    feasibilityNotice$setContent(
                        sprintf(
                            paste(
                                'Sample Size Calculation Complete',
                                '',
                                'Required sample size: n = %d per group',
                                '',
                                'This calculation assumes:',
                                '• Effect size: %.3f (%s)',
                                '• Power: %.0f%%',
                                '• Alpha (two-sided): %.0f%%',
                                '• Equal group sizes',
                                '',
                                'Next Steps:',
                                '• Pre-register study design',
                                '• Prepare for ethics approval',
                                '• Develop recruitment strategy',
                                '• Plan interim analyses if appropriate',
                                sep = '\n'
                            ),
                            n_final,
                            effect_size,
                            interpret_effect_size(effect_size),
                            power * 100,
                            alpha * 100
                        )
                    )
                }

                self$results$insert(999, feasibilityNotice)

            }, error = function(e) {
                calcErrorNotice <- jmvcore::Notice$new(
                    options = self$options,
                    name = 'calculationError',
                    type = jmvcore::NoticeType$ERROR
                )
                calcErrorNotice$setContent(
                    sprintf('Sample size calculation failed: %s\nPlease check your parameters.', e$message)
                )
                self$results$insert(1, calcErrorNotice)
            })
        }
    )
)
```

---

## 11. Real-World Patterns from jamovi Core

This section summarizes key patterns discovered from examining the official jamovi/jmv codebase.

### Pattern 1: Helper Function Architecture

**Source:** `jamovi/jmv/R/utils.R`

Create centralized helper functions for notice management:

```r
# utils.R
setAnalysisNotice <- function(self, message, name, type) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = name,
        type = type
    )
    notice$setContent(message)
    self$results$insert(1, notice)
}

# Specific warning helpers
setSingularityWarning <- function(self) {
    message <- .("Model has singularity problem...")
    setAnalysisNotice(self, message, "singularityWarning",
                     jmvcore::NoticeType$STRONG_WARNING)
}
```

### Pattern 2: Multiple Insertion Methods

Different situations require different insertion approaches:

```r
# 1. Main results insertion (most common)
self$results$insert(1, notice)

# 2. Attach to specific element header (descriptives.b.R)
plots$setHeader(notice)

# 3. Insert into result group (linreg.b.R)
group$insert(1, notice)
```

**Use Cases:**
- **Main results:** General errors, warnings, or information
- **Element header:** Warnings specific to plots, tables, or other result items
- **Result group:** Notices that apply to a subset of results

### Pattern 3: Accumulating Warnings

**Source:** `vijPlots/R/principal.b.R`

Build warning messages as issues are discovered:

```r
warningMsg <- ""

# Check multiple conditions
if (condition1) {
    warningMsg <- paste(warningMsg, "Issue 1 detected", sep = "\n")
}

if (condition2) {
    warningMsg <- paste(warningMsg, "Issue 2 detected", sep = "\n")
}

if (condition3) {
    warningMsg <- paste(warningMsg, "Issue 3 detected", sep = "\n")
}

# Create single notice with all warnings
if (warningMsg != "") {
    notice <- jmvcore::Notice$new(
        self$options,
        type = jmvcore::NoticeType$WARNING,
        name = 'accumulatedWarnings',
        content = warningMsg
    )
    self$results$insert(1, notice)
}
```

**Benefits:**
- Single notice instead of multiple cluttered notices
- User sees all issues at once
- Cleaner results presentation

### Pattern 4: Internationalization with jmvcore::format

**Source:** `jamovi/jmv/R/conttables.b.R`

Use format placeholders for dynamic content:

```r
# Template with placeholder {}
notice$setContent(
    jmvcore::format(
        .("The data is weighted by the variable {}"),
        self$options$counts
    )
)

# Multiple placeholders
notice$setContent(
    jmvcore::format(
        .("Analysis used {} observations with {} events ({} censored)"),
        n_total, n_events, n_censored
    )
)
```

### Pattern 5: Notice Naming Conventions

From examining jamovi/jmv codebase:

```r
# System/internal notices: prefix with '.'
name = '.weights'
name = '.variableType'

# User-facing notices: descriptive camelCase
name = 'warningMessage'
name = 'validationError'
name = 'assumptionViolation'

# Specific warnings: combine type + context
name = 'refLevelWarning'
name = 'singularityWarning'
name = 'sampleSizeWarning'
```

### Pattern 6: Variable Type Checking

**Source:** `jamovi/jmv/R/descriptives.b.R`

Check variable types and provide helpful guidance:

```r
# Check if variables can be treated as numeric
nonNumericVars <- c()

for (var in selected_vars) {
    if (!is.numeric(data[[var]])) {
        nonNumericVars <- c(nonNumericVars, var)
    }
}

if (length(nonNumericVars) > 0) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'variableTypeWarning',
        type = jmvcore::NoticeType$WARNING
    )

    notice$setContent(
        jmvcore::format(
            .("The variable(s) {} cannot be treated as numeric. Plots that expect numeric data will not be created."),
            paste(nonNumericVars, collapse = ", ")
        )
    )

    plots$setHeader(notice)
}
```

### Pattern 7: Conditional Method Availability

**Source:** `jamovi/jmv/R/linreg.b.R`

Inform users when features require specific conditions:

```r
# Check prerequisites for advanced features
if (self$options$calculateMahalanobis && n_covariates < 2) {
    notice <- jmvcore::Notice$new(
        options = self$options,
        name = 'warningMessage',
        type = jmvcore::NoticeType$WARNING
    )

    notice$setContent(
        .("Mahalanobis distance can only be calculated for models with two or more covariates")
    )

    group$insert(1, notice)
    return()  # Skip this calculation
}
```

### Key Takeaways from Real-World Code

1. **Consistency:** Use helper functions like `setAnalysisNotice()` for uniform behavior
2. **Clarity:** Use `jmvcore::format()` with `..()` for internationalization
3. **Efficiency:** Accumulate multiple warnings into single notices
4. **Placement:** Choose insertion method based on notice scope (main results, element header, or group)
5. **Naming:** Follow conventions (`.name` for internal, `descriptiveName` for user-facing)
6. **Content:** Set content directly in constructor when known, or use `setContent()` for dynamic messages
7. **HTML:** Never use HTML formatting in notices (official recommendation)
8. **Positioning:** Always use `insert(1, notice)` for top placement

---

## Conclusion

Effective use of notices enhances the user experience of jamovi modules by:

### Essential Principles

1. **Clear Communication** - Use plain language and specific details
2. **Appropriate Severity** - Match notice type to issue importance
3. **Actionable Guidance** - Tell users what to do next
4. **Context Awareness** - Adapt messages to analysis stage and results
5. **Educational Value** - Teach users about methodology and interpretation

### Notice Type Selection Guide

| Situation | Notice Type | When to Use |
|-----------|-------------|-------------|
| Analysis cannot proceed | ERROR | Missing data, invalid inputs, fatal errors |
| Results unreliable | STRONG_WARNING | Assumption violations, quality issues |
| User should be aware | WARNING | Minor issues, methodological notes |
| Helpful information | INFO | Methodology, confirmations, guidance |

### Best Practices Summary

- **Be specific** - Include numbers and details
- **Be helpful** - Provide next steps and recommendations
- **Be clear** - Avoid jargon, use plain language
- **Be consistent** - Similar issues should have similar messages
- **Be appropriate** - Don't over-warn or under-warn

### Integration with Clinical Modules

For clinical and pathology modules:
- Emphasize clinical implications
- Reference established guidelines
- Explain statistical concepts in clinical terms
- Warn about inappropriate clinical use
- Document methodological decisions transparently

This comprehensive guide provides the foundation for implementing professional, user-friendly notices in jamovi modules. Well-crafted notices transform statistical software into an educational tool that guides users toward appropriate analysis and valid interpretation of results.

---

## 12. References and Additional Resources

### Official Documentation

**jamovi Developer Hub - Notices API**
- URL: https://dev.jamovi.org/api_notices.html
- Official API documentation for the notices system
- Core reference for all notice implementations

### Source Code References

This guide incorporates real-world patterns from the following repositories:

**jamovi/jmv (Official jamovi Analyses)**
- Repository: https://github.com/jamovi/jmv
- Key files examined:
  - `R/conttables.b.R` - Weighted data notices
  - `R/descriptives.b.R` - Variable type warnings and plot limitations
  - `R/linreg.b.R` - Prerequisite checking for advanced features
  - `R/utils.R` - Helper functions for notice management
  - `R/utilsanova.R` - Singularity warnings and model fit issues

**vijPlots (Community Module)**
- Repository: https://github.com/vjalby/vijPlots
- Key files examined:
  - `R/principal.b.R` - Accumulating warning pattern
  - `R/corresp.b.R` - Similar warning accumulation approach

### Key Patterns Documented

1. **Helper Function Architecture** - Centralized notice creation via `setAnalysisNotice()`
2. **Multiple Insertion Methods** - `insert()`, `setHeader()`, and group insertion
3. **Accumulating Warnings** - Building composite warning messages
4. **Internationalization** - Using `..()` and `jmvcore::format()`
5. **Naming Conventions** - System notices (`.name`) vs user-facing notices
6. **Conditional Creation** - Check before creating to avoid empty notices
7. **Variable Type Checking** - Informing users about incompatible data types

### Best Practice Summary from Real-World Code

**From jamovi/jmv codebase analysis:**

✅ **DO:**
- Use helper functions for consistency
- Implement internationalization with `..()` and `jmvcore::format()`
- Accumulate multiple warnings into single notices
- Choose appropriate insertion method (results, header, or group)
- Follow naming conventions (`.internal` or `descriptiveName`)
- Always avoid HTML in notice content
- Position critical notices at top with `insert(1, notice)`

❌ **DON'T:**
- Create empty notices (check conditions first)
- Use HTML formatting in content
- Create multiple notices for related issues
- Hard-code messages without internationalization support

### Implementation Checklist

When implementing notices in your jamovi module:

- [ ] Create helper functions in a utils file (e.g., `setAnalysisNotice()`)
- [ ] Use appropriate notice type (ERROR, STRONG_WARNING, WARNING, INFO)
- [ ] Implement internationalization with `..()` for all messages
- [ ] Check conditions before creating notices
- [ ] Accumulate related warnings into single notices
- [ ] Use plain text formatting (no HTML)
- [ ] Choose correct insertion method for your use case
- [ ] Follow naming conventions for notice identifiers
- [ ] Provide actionable guidance in notice content
- [ ] Test notices with different data scenarios

### Contributing

This guide is part of the ClinicoPath jamovi module project. Contributions and improvements are welcome. If you discover additional patterns or best practices from other jamovi modules, please consider contributing them to this guide.

---

**Document Version:** 2.0
**Last Updated:** Based on jamovi/jmv commit 8aa8270 and official API documentation
**Author:** ClinicoPath Development Team
**License:** Follow jamovi module licensing guidelines
