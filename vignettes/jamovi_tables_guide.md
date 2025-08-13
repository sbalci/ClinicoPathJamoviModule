# The Complete Guide to Creating and Managing Tables in jamovi Development

This is the definitive, comprehensive guide to creating, defining, and managing tables in jamovi module development. Tables are fundamental for presenting statistical results in a professional, structured format. This guide covers everything from basic table definitions to advanced population techniques and clinical applications.

## Table of Contents

1. [Introduction: The jamovi Table Architecture](#1-introduction-the-jamovi-table-architecture)
2. [Table Definition in .r.yaml Files](#2-table-definition-in-ryaml-files)
3. [Table Population in .b.R Files](#3-table-population-in-br-files)
4. [Column Types and Formatting](#4-column-types-and-formatting)
5. [Advanced Table Features](#5-advanced-table-features)
6. [Table State Management and Performance](#6-table-state-management-and-performance)
7. [HTML Tables and Custom Content](#7-html-tables-and-custom-content)
8. [Clinical and Research Table Patterns](#8-clinical-and-research-table-patterns)
9. [Dynamic Tables and Conditional Display](#9-dynamic-tables-and-conditional-display)
10. [Best Practices and Design Principles](#10-best-practices-and-design-principles)
11. [Complete Examples](#11-complete-examples)

---

## 1. Introduction: The jamovi Table Architecture

### The Two-File Table System

Creating tables in jamovi follows a coordinated two-file architecture:

1. **`.r.yaml` file** - **Table Structure Definition**
   - Defines table schema, columns, and metadata
   - Acts as the blueprint for table creation
   - Specifies data types, formatting, and visibility conditions

2. **`.b.R` file** - **Table Population and Logic**
   - Contains R code to populate tables with data
   - Implements statistical calculations and data processing
   - Manages table state and dynamic updates

### Table Integration with jamovi Module Components

Tables integrate seamlessly with other module components:

- **`.a.yaml`** - Options control table content and visibility
- **`.u.yaml`** - UI elements trigger table updates
- **`.r.yaml`** - Tables work alongside plots and other results

### Table Compilation Process

```
.r.yaml definition → jamovi compiler → Table object → .b.R population → User interface
```

---

## 2. Table Definition in .r.yaml Files

### Basic Table Structure

Every table in jamovi is defined as a result item of type `Table`:

```yaml
- name: myAnalysisTable
  title: Statistical Analysis Results
  type: Table
  rows: 0
  columns:
    - name: variable
      title: "Variable"
      type: text
    - name: estimate
      title: "Estimate"
      type: number
      format: zto
    - name: pvalue
      title: "p-value"
      type: number
      format: zto,pvalue
  clearWith:
    - analysisMethod
    - selectedVariables
  visible: true
```

### Core Table Properties

#### `name` (Required)
- **Type**: String
- **Purpose**: Unique identifier for the table
- **Usage**: Referenced in `.b.R` file as `self$results$tableName`
- **Rules**: Must be valid R identifier (no spaces, numbers, special characters)

```yaml
name: medianSurvivalTable    # Good
name: median-survival-table  # Bad - contains hyphens
name: 1stTable              # Bad - starts with number
```

#### `title` (Required)
- **Type**: String
- **Purpose**: Display title shown above the table
- **Features**: Supports dynamic content using `${}` syntax
- **Examples**:

```yaml
title: "Summary Statistics"                           # Static title
title: "Survival Analysis Results"                    # Static descriptive title
title: "`Median Survival - ${explanatory}`"          # Dynamic with variable
title: "`Cox Regression for ${outcome} vs ${covariates}`"  # Multiple variables
```

#### `type` (Required)
- **Value**: Must be `Table`
- **Purpose**: Identifies this result item as a table

#### `rows` (Required)
- **Type**: Integer
- **Purpose**: Initial number of rows in table
- **Common Practice**: Use `rows: 0` for dynamic tables where row count depends on data
- **Fixed Tables**: Use specific number for tables with predetermined structure

```yaml
rows: 0        # Dynamic - row count determined by data
rows: 5        # Fixed - exactly 5 rows always
rows: 1        # Single-row summary table
```

### Column Definitions

#### Basic Column Properties

Each column is defined as a dictionary with required and optional properties:

```yaml
columns:
  - name: columnName      # Required - unique identifier
    title: "Display Name" # Required - header text
    type: text           # Required - data type
    format: zto          # Optional - number formatting
    superTitle: "Group"  # Optional - spanning header
    visible: true        # Optional - conditional visibility
```

#### Column Data Types

##### `text` Type
For categorical data, labels, and string content:

```yaml
- name: variable_name
  title: "Variable"
  type: text
```

**Use Cases**:
- Variable names and labels
- Factor levels and categories
- Text descriptions and notes
- Formatted statistical results as strings

##### `integer` Type
For whole numbers and count data:

```yaml
- name: sample_size
  title: "N"
  type: integer
```

**Use Cases**:
- Sample sizes and counts
- Number of events or observations
- Degrees of freedom
- Time periods (years, months)

##### `number` Type
For continuous numerical data:

```yaml
- name: mean_value
  title: "Mean"
  type: number
  format: zto
```

**Use Cases**:
- Statistical estimates and coefficients
- p-values and confidence intervals
- Percentages and proportions
- Measurement values

### Column Formatting Options

#### Number Format Patterns

##### `zto` - Standard Decimal Format
```yaml
format: zto        # 0.1234 → 0.123
format: zto        # 0.0001 → < 0.001
```

##### `pc` - Percentage Format
```yaml
format: pc         # 0.1234 → 12.3%
format: pc         # 0.0567 → 5.7%
```

##### `pvalue` - P-value Format
```yaml
format: zto,pvalue # 0.0001 → < 0.001
format: zto,pvalue # 0.045 → 0.045
```

##### Custom Decimal Places
```yaml
format: dp:2       # 1.23456 → 1.23
format: dp:4       # 1.23456 → 1.2346
```

#### Advanced Formatting Examples

```yaml
# Confidence interval columns with consistent formatting
- name: ci_lower
  title: "Lower"
  superTitle: "95% CI"
  type: number
  format: zto
- name: ci_upper
  title: "Upper"
  superTitle: "95% CI"
  type: number
  format: zto

# Percentage with specific precision
- name: survival_rate
  title: "5-Year Survival"
  type: number
  format: pc

# P-values with proper formatting
- name: log_rank_p
  title: "Log-Rank p"
  type: number
  format: zto,pvalue
```

### Column Grouping with superTitle

Create spanning headers that group related columns:

```yaml
columns:
  - name: variable
    title: "Variable"
    type: text
  
  # Confidence interval group
  - name: estimate
    title: "Estimate"
    type: number
    format: zto
  - name: ci_lower
    title: "Lower"
    superTitle: "95% Confidence Interval"
    type: number
    format: zto
  - name: ci_upper
    title: "Upper"
    superTitle: "95% Confidence Interval"
    type: number
    format: zto
  
  # Statistical test group
  - name: statistic
    title: "Statistic"
    superTitle: "Test Results"
    type: number
    format: zto
  - name: p_value
    title: "p-value"
    superTitle: "Test Results"
    type: number
    format: zto,pvalue
```

### Table Metadata and State Management

#### `clearWith` Property
Specifies which options trigger table clearing and recalculation:

```yaml
clearWith:
  - outcome          # Clear when outcome variable changes
  - explanatory      # Clear when explanatory variable changes
  - analysisMethod   # Clear when analysis method changes
  - includeCI        # Clear when confidence interval option changes
```

#### `visible` Property
Controls when tables are displayed:

```yaml
visible: true                    # Always visible
visible: false                   # Never visible (for debugging)
visible: (performAnalysis)       # Conditional on boolean option
visible: (method:survival)       # Conditional on specific list value
visible: (outcome && covariates) # Multiple conditions
```

#### `refs` Property
Links to external packages or references:

```yaml
refs:
  - survival      # Links to survival package documentation
  - survminer     # Links to survminer package
  - finalfit      # Links to finalfit package
```

---

## 3. Table Population in .b.R Files

### Accessing Tables

Tables defined in `.r.yaml` are accessed in `.b.R` files through the results object:

```r
# Access a table by its name
medianTable <- self$results$medianTable
survivalTable <- self$results$survivalTable
coxTable <- self$results$coxTable
```

### Table Population Methods

#### Method 1: `addRow()` - Row-by-Row Addition

Most common method for populating tables with data:

```r
# Basic addRow usage
medianTable <- self$results$medianTable

# Add single row with named list
medianTable$addRow(
    rowKey = 1, 
    values = list(
        factor = "Treatment A",
        records = 45,
        events = 12,
        median = 24.5,
        ci_lower = 18.2,
        ci_upper = 30.8
    )
)

# Add rows from data frame
results_df <- data.frame(
    factor = c("Treatment A", "Treatment B", "Control"),
    records = c(45, 52, 48),
    events = c(12, 18, 25),
    median = c(24.5, 18.3, 12.1),
    ci_lower = c(18.2, 14.1, 8.9),
    ci_upper = c(30.8, 22.5, 15.3)
)

# Loop through data frame
for (i in seq_len(nrow(results_df))) {
    medianTable$addRow(
        rowKey = i, 
        values = as.list(results_df[i, ])
    )
}
```

#### Method 2: `setRow()` - Direct Row Setting

Set entire row at specific position:

```r
# Set row at specific position
coxTable$setRow(
    rowNo = 1,
    values = list(
        variable = "Age",
        hazard_ratio = 1.05,
        ci_lower = 1.01,
        ci_upper = 1.09,
        p_value = 0.023
    )
)

# Set row with row key
coxTable$setRow(
    rowKey = "age_variable",
    values = list(
        variable = "Age",
        hazard_ratio = 1.05,
        ci_lower = 1.01,
        ci_upper = 1.09,
        p_value = 0.023
    )
)
```

#### Method 3: `setCell()` - Individual Cell Updates

Update specific cells within existing rows:

```r
# Set individual cell values
diagnosticTable$setCell(
    rowKey = 1,
    col = "sensitivity",
    value = 0.856
)

diagnosticTable$setCell(
    rowKey = 1,
    col = "specificity",
    value = 0.924
)

# Update multiple cells for the same row
row_key <- "roc_analysis"
diagnosticTable$setCell(rowKey = row_key, col = "auc", value = 0.89)
diagnosticTable$setCell(rowKey = row_key, col = "ci_lower", value = 0.81)
diagnosticTable$setCell(rowKey = row_key, col = "ci_upper", value = 0.97)
```

### Advanced Population Patterns

#### Populating from Statistical Results

```r
# Example: Cox regression results
library(survival)

# Perform analysis
cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
cox_summary <- summary(cox_model)

# Extract results
coefficients <- cox_summary$coefficients
conf_int <- cox_summary$conf.int

# Populate table
coxTable <- self$results$coxTable

for (i in seq_len(nrow(coefficients))) {
    variable_name <- rownames(coefficients)[i]
    
    coxTable$addRow(
        rowKey = i,
        values = list(
            variable = variable_name,
            coefficient = coefficients[i, "coef"],
            hazard_ratio = coefficients[i, "exp(coef)"],
            se = coefficients[i, "se(coef)"],
            z_score = coefficients[i, "z"],
            p_value = coefficients[i, "Pr(>|z|)"],
            ci_lower = conf_int[i, "lower .95"],
            ci_upper = conf_int[i, "upper .95"]
        )
    )
}
```

#### Conditional Table Population

```r
# Only populate if specific options are selected
if (self$options$performSurvivalAnalysis) {
    
    # Perform survival analysis
    surv_fit <- survfit(Surv(time, status) ~ group, data = mydata)
    surv_summary <- summary(surv_fit, times = c(12, 36, 60))
    
    # Populate survival table
    survTable <- self$results$survivalTable
    
    # Check if results exist
    if (!is.null(surv_summary$strata)) {
        for (i in seq_along(surv_summary$time)) {
            survTable$addRow(
                rowKey = i,
                values = list(
                    group = surv_summary$strata[i],
                    time = surv_summary$time[i],
                    n_risk = surv_summary$n.risk[i],
                    n_event = surv_summary$n.event[i],
                    survival = surv_summary$surv[i],
                    ci_lower = surv_summary$lower[i],
                    ci_upper = surv_summary$upper[i]
                )
            )
        }
    }
}
```

#### Error Handling in Table Population

```r
# Robust table population with error handling
tryCatch({
    
    # Attempt statistical analysis
    model_results <- performComplexAnalysis(data)
    
    # Clear any existing rows
    analysisTable$deleteRows()
    
    # Populate with new results
    if (!is.null(model_results) && nrow(model_results) > 0) {
        for (i in seq_len(nrow(model_results))) {
            analysisTable$addRow(
                rowKey = i,
                values = list(
                    parameter = model_results$parameter[i],
                    estimate = model_results$estimate[i],
                    std_error = model_results$se[i],
                    p_value = model_results$p_value[i]
                )
            )
        }
    } else {
        # Add placeholder row if no results
        analysisTable$addRow(
            rowKey = 1,
            values = list(
                parameter = "No results",
                estimate = NA,
                std_error = NA,
                p_value = NA
            )
        )
    }
    
}, error = function(e) {
    # Handle errors gracefully
    analysisTable$addRow(
        rowKey = 1,
        values = list(
            parameter = paste("Error:", e$message),
            estimate = NA,
            std_error = NA,
            p_value = NA
        )
    )
})
```

---

## 4. Column Types and Formatting

### Text Columns

#### Basic Text Display
```yaml
- name: variable_label
  title: "Variable"
  type: text
```

#### Formatted Text Content
Text columns can contain formatted statistical results:

```r
# Example: Formatted hazard ratios
hr_text <- sprintf("%.2f (%.2f-%.2f)", 
                   hazard_ratio, 
                   ci_lower, 
                   ci_upper)

coxTable$addRow(
    rowKey = i,
    values = list(
        variable = variable_name,
        hazard_ratio_formatted = hr_text,
        p_value = p_val
    )
)
```

### Numeric Columns

#### Integer Columns for Counts
```yaml
- name: sample_size
  title: "N"
  type: integer

- name: events
  title: "Events"
  type: integer

- name: degrees_freedom
  title: "df"
  type: integer
```

#### Number Columns for Continuous Data
```yaml
- name: mean_value
  title: "Mean"
  type: number
  format: dp:2

- name: standard_error
  title: "SE"
  type: number
  format: zto

- name: confidence_level
  title: "CI Level"
  type: number
  format: pc
```

### Specialized Formatting

#### P-value Formatting
```yaml
# P-values with proper scientific notation
- name: p_value
  title: "p-value"
  type: number
  format: zto,pvalue
```

Results in display:
- `0.045` → `0.045`
- `0.0001` → `< 0.001`
- `0.99` → `0.990`

#### Percentage Formatting
```yaml
# Percentages with automatic % symbol
- name: survival_rate
  title: "5-Year Survival"
  type: number
  format: pc
```

Results in display:
- `0.856` → `85.6%`
- `0.12` → `12.0%`
- `1.0` → `100.0%`

#### Custom Precision
```yaml
# Fixed decimal places
- name: coefficient
  title: "β"
  type: number
  format: dp:4    # 4 decimal places

- name: odds_ratio
  title: "OR"
  type: number
  format: dp:2    # 2 decimal places
```

### Complex Column Patterns

#### Confidence Interval Pairs
```yaml
# Paired columns for confidence intervals
- name: estimate
  title: "Estimate"
  type: number
  format: zto

- name: ci_lower
  title: "Lower"
  superTitle: "95% Confidence Interval"
  type: number
  format: zto

- name: ci_upper
  title: "Upper"
  superTitle: "95% Confidence Interval"
  type: number
  format: zto
```

#### Statistical Test Results
```yaml
# Complete statistical test reporting
- name: test_statistic
  title: "Statistic"
  superTitle: "Test Results"
  type: number
  format: zto

- name: degrees_freedom
  title: "df"
  superTitle: "Test Results"
  type: integer

- name: p_value
  title: "p-value"
  superTitle: "Test Results"
  type: number
  format: zto,pvalue

- name: effect_size
  title: "Effect Size"
  superTitle: "Test Results"
  type: number
  format: zto
```

---

## 5. Advanced Table Features

### Table Footnotes and Annotations

#### Adding Footnotes
```r
# Add footnote to explain methods or assumptions
pairwiseTable$setNote(
    key = "adjustment_method",
    note = paste0("p-value adjustment method: ", self$options$padjustmethod)
)

# Add footnote about missing data
analysisTable$setNote(
    key = "missing_data",
    note = sprintf("Analysis excludes %d observations with missing data", 
                   n_missing)
)

# Add footnote about statistical methods
survivalTable$setNote(
    key = "methods",
    note = "Survival estimates calculated using Kaplan-Meier method"
)
```

#### Multiple Footnotes
```r
# Multiple footnotes for complex tables
table <- self$results$complexAnalysisTable

table$setNote("method", "Analysis performed using Cox proportional hazards model")
table$setNote("assumptions", "Proportional hazards assumption tested and satisfied")
table$setNote("missing", sprintf("Complete case analysis: %d/%d observations used", 
                                n_complete, n_total))
table$setNote("ci", "Confidence intervals calculated using profile likelihood")
```

### Dynamic Column Addition

#### Adding Columns Programmatically
```r
# Add column based on user options
if (self$options$showConfidenceIntervals) {
    analysisTable$addColumn(
        name = "ci_lower",
        title = "Lower CI",
        type = "number",
        format = "zto"
    )
    
    analysisTable$addColumn(
        name = "ci_upper", 
        title = "Upper CI",
        type = "number",
        format = "zto"
    )
}

# Add method-specific columns
if (self$options$analysisMethod == "bootstrap") {
    analysisTable$addColumn(
        name = "bootstrap_se",
        title = "Bootstrap SE",
        type = "number",
        format = "zto"
    )
}
```

### Table Sorting and Organization

#### Row Keys for Organization
```r
# Use meaningful row keys for organization
diagnosticTable <- self$results$diagnosticTable

diagnosticTable$addRow(
    rowKey = "sensitivity",
    values = list(
        metric = "Sensitivity",
        value = sens_value,
        ci_lower = sens_ci_lower,
        ci_upper = sens_ci_upper
    )
)

diagnosticTable$addRow(
    rowKey = "specificity",
    values = list(
        metric = "Specificity", 
        value = spec_value,
        ci_lower = spec_ci_lower,
        ci_upper = spec_ci_upper
    )
)

diagnosticTable$addRow(
    rowKey = "ppv",
    values = list(
        metric = "Positive Predictive Value",
        value = ppv_value,
        ci_lower = ppv_ci_lower,
        ci_upper = ppv_ci_upper
    )
)
```

### Row and Cell Styling

#### Conditional Cell Highlighting
```r
# Highlight significant p-values
for (i in seq_len(nrow(results))) {
    p_val <- results$p_value[i]
    
    # Add row
    table$addRow(
        rowKey = i,
        values = list(
            variable = results$variable[i],
            estimate = results$estimate[i],
            p_value = p_val
        )
    )
    
    # Highlight significant results
    if (p_val < 0.05) {
        table$addFormat(
            rowKey = i,
            col = "p_value",
            format = "bold"
        )
    }
}
```

### Table State Management

#### Clearing and Resetting Tables
```r
# Clear all existing rows before repopulating
analysisTable$deleteRows()

# Clear specific row
analysisTable$deleteRow(rowKey = "old_result")

# Reset table state
analysisTable$setState(NULL)
```

#### Preserving Table State
```r
# Save table state for reuse
current_state <- list(
    data = processed_data,
    results = analysis_results,
    options = current_options
)

analysisTable$setState(current_state)
```

---

## 6. Table State Management and Performance

### State Management with `clearWith`

The `clearWith` property in `.r.yaml` files controls when tables are automatically cleared:

```yaml
- name: survivalAnalysisTable
  title: "Survival Analysis Results"
  type: Table
  rows: 0
  clearWith:
    - outcome           # Clear when outcome variable changes
    - explanatory       # Clear when grouping variable changes
    - analysisMethod    # Clear when method changes
    - timeVariable      # Clear when time variable changes
    - includeCI         # Clear when CI option changes
  columns:
    # ... column definitions
```

### Performance Optimization

#### Efficient Table Population
```r
# Inefficient: Multiple individual operations
# for (i in 1:1000) {
#     table$addRow(rowKey = i, values = slow_calculation(i))
# }

# Efficient: Batch processing
results_list <- lapply(1:1000, function(i) {
    # Perform all calculations first
    slow_calculation(i)
})

# Then populate table in batch
for (i in seq_along(results_list)) {
    table$addRow(rowKey = i, values = results_list[[i]])
}
```

#### Memory Management
```r
# Clear large intermediate objects
tryCatch({
    # Perform analysis
    large_dataset <- processLargeData(self$data)
    results <- performAnalysis(large_dataset)
    
    # Populate table
    populateTable(self$results$analysisTable, results)
    
    # Clean up memory
    rm(large_dataset)
    gc()
    
}, error = function(e) {
    # Handle errors
    self$results$analysisTable$setNote("error", paste("Analysis failed:", e$message))
})
```

### Conditional Table Display

#### Complex Visibility Conditions
```yaml
# Multiple conditions for table visibility
- name: advancedResultsTable
  title: "Advanced Analysis Results"
  type: Table
  visible: (performAdvanced && (method:cox || method:parametric) && covariates)
  rows: 0
  columns:
    # ... column definitions
```

#### Dynamic Table Management
```r
# Show/hide tables based on results
if (analysis_successful && has_results) {
    self$results$mainTable$setVisible(TRUE)
    self$results$errorTable$setVisible(FALSE)
} else {
    self$results$mainTable$setVisible(FALSE)
    self$results$errorTable$setVisible(TRUE)
    
    # Populate error information
    self$results$errorTable$addRow(
        rowKey = 1,
        values = list(
            error_type = "Analysis Failed",
            description = error_message,
            suggestion = "Check data format and variable selection"
        )
    )
}
```

---

## 7. HTML Tables and Custom Content

### HTML Result Items

For complex formatting beyond standard tables, use `Html` result type:

```yaml
- name: customFormattedTable
  title: "Custom Formatted Results"
  type: Html
  visible: true
```

### Integration with R Table Packages

#### Using `gtsummary`
```r
# Create publication-ready table with gtsummary
library(gtsummary)

# Create summary table
summary_table <- self$data %>%
    select(age, sex, outcome, treatment) %>%
    tbl_summary(
        by = treatment,
        statistic = list(
            all_continuous() ~ "{mean} ({sd})",
            all_categorical() ~ "{n} ({p}%)"
        ),
        missing = "no"
    ) %>%
    add_p() %>%
    add_overall() %>%
    bold_p() %>%
    modify_header(label = "**Variable**") %>%
    modify_caption("**Table 1. Patient Characteristics**")

# Convert to HTML
html_content <- summary_table %>%
    as_kable_extra(format = "html") %>%
    kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover"),
        full_width = FALSE,
        position = "center"
    )

# Set HTML content
self$results$customFormattedTable$setContent(html_content)
```

#### Using `gt` Package
```r
# Create table with gt package
library(gt)

# Prepare data
results_df <- data.frame(
    Treatment = c("A", "B", "C"),
    N = c(45, 52, 48),
    `Mean (SD)` = c("12.3 (2.1)", "14.7 (3.2)", "11.9 (2.8)"),
    `Median [IQR]` = c("12.1 [10.8-13.9]", "14.2 [12.1-16.8]", "11.7 [9.8-13.2]"),
    `p-value` = c("", "0.023", "0.567")
)

# Create gt table
gt_table <- results_df %>%
    gt() %>%
    tab_header(
        title = "Treatment Comparison Results",
        subtitle = "Statistical Summary by Treatment Group"
    ) %>%
    tab_source_note("Statistical tests: ANOVA for continuous variables") %>%
    cols_align(align = "center", columns = everything()) %>%
    cols_align(align = "left", columns = Treatment) %>%
    tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_column_labels()
    ) %>%
    tab_style(
        style = list(cell_fill(color = "#f0f0f0")),
        locations = cells_body(rows = `p-value` < 0.05)
    )

# Convert to HTML
html_content <- as_raw_html(gt_table)
self$results$customFormattedTable$setContent(html_content)
```

### Custom HTML Generation

#### Manual HTML Creation
```r
# Create custom HTML table
create_custom_html_table <- function(data, title) {
    
    # Start HTML structure
    html <- paste0(
        '<div class="custom-table-container">',
        '<h3 class="table-title">', title, '</h3>',
        '<table class="custom-results-table">'
    )
    
    # Add header
    html <- paste0(html, '<thead><tr>')
    for (col_name in names(data)) {
        html <- paste0(html, '<th>', col_name, '</th>')
    }
    html <- paste0(html, '</tr></thead>')
    
    # Add body
    html <- paste0(html, '<tbody>')
    for (i in seq_len(nrow(data))) {
        html <- paste0(html, '<tr>')
        for (j in seq_len(ncol(data))) {
            cell_value <- data[i, j]
            # Add conditional formatting
            if (is.numeric(cell_value) && cell_value < 0.05) {
                html <- paste0(html, '<td class="significant">', 
                              format(cell_value, digits = 3), '</td>')
            } else {
                html <- paste0(html, '<td>', cell_value, '</td>')
            }
        }
        html <- paste0(html, '</tr>')
    }
    html <- paste0(html, '</tbody></table></div>')
    
    # Add CSS styling
    css <- '
    <style>
    .custom-table-container { margin: 20px 0; }
    .table-title { color: #333; font-weight: bold; margin-bottom: 10px; }
    .custom-results-table { 
        width: 100%; 
        border-collapse: collapse; 
        font-family: Arial, sans-serif;
    }
    .custom-results-table th, .custom-results-table td { 
        border: 1px solid #ddd; 
        padding: 8px 12px; 
        text-align: left; 
    }
    .custom-results-table th { 
        background-color: #f2f2f2; 
        font-weight: bold; 
    }
    .custom-results-table .significant { 
        background-color: #fff3cd; 
        font-weight: bold; 
    }
    </style>'
    
    return(paste0(css, html))
}

# Use custom HTML table
analysis_results <- data.frame(
    Variable = c("Age", "Sex", "Treatment"),
    Coefficient = c(0.123, -0.456, 0.789),
    `P-value` = c(0.023, 0.001, 0.234)
)

html_table <- create_custom_html_table(analysis_results, "Regression Results")
self$results$customFormattedTable$setContent(html_table)
```

---

## 8. Clinical and Research Table Patterns

### Survival Analysis Tables

#### Kaplan-Meier Results Table
```yaml
# Median survival table definition
- name: medianSurvivalTable
  title: "`Median Survival by ${explanatory}`"
  type: Table
  rows: 0
  columns:
    - name: group
      title: "Group"
      type: text
    - name: n_total
      title: "N"
      type: integer
    - name: n_events
      title: "Events"
      type: integer
    - name: median_survival
      title: "Median"
      superTitle: "Survival Time"
      type: number
      format: zto
    - name: ci_lower
      title: "Lower"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: ci_upper
      title: "Upper"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: restricted_mean
      title: "RMST"
      type: number
      format: zto
```

#### Cox Regression Results Table
```yaml
# Cox regression results
- name: coxRegressionTable
  title: "Cox Proportional Hazards Analysis"
  type: Table
  rows: 0
  columns:
    - name: variable
      title: "Variable"
      type: text
    - name: level
      title: "Level"
      type: text
    - name: hazard_ratio
      title: "HR"
      superTitle: "Hazard Ratio"
      type: number
      format: zto
    - name: ci_lower
      title: "Lower"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: ci_upper
      title: "Upper"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: p_value
      title: "p-value"
      type: number
      format: zto,pvalue
    - name: significance
      title: "Sig."
      type: text
```

### Diagnostic Accuracy Tables

#### ROC Analysis Results
```yaml
- name: rocAnalysisTable
  title: "ROC Curve Analysis"
  type: Table
  rows: 0
  columns:
    - name: cutoff
      title: "Cutoff"
      type: number
      format: zto
    - name: sensitivity
      title: "Sensitivity"
      type: number
      format: pc
    - name: specificity
      title: "Specificity"
      type: number
      format: pc
    - name: ppv
      title: "PPV"
      superTitle: "Predictive Values"
      type: number
      format: pc
    - name: npv
      title: "NPV"
      superTitle: "Predictive Values"
      type: number
      format: pc
    - name: auc
      title: "AUC"
      type: number
      format: zto
    - name: auc_ci_lower
      title: "Lower"
      superTitle: "AUC 95% CI"
      type: number
      format: zto
    - name: auc_ci_upper
      title: "Upper"
      superTitle: "AUC 95% CI"
      type: number
      format: zto
```

### Clinical Trial Tables

#### Baseline Characteristics Table
```yaml
- name: baselineCharacteristics
  title: "Baseline Patient Characteristics"
  type: Table
  rows: 0
  columns:
    - name: characteristic
      title: "Characteristic"
      type: text
    - name: overall
      title: "Overall\n(N=${total_n})"
      type: text
    - name: treatment_a
      title: "Treatment A\n(N=${treatment_a_n})"
      type: text
    - name: treatment_b
      title: "Treatment B\n(N=${treatment_b_n})"
      type: text
    - name: p_value
      title: "p-value"
      type: number
      format: zto,pvalue
    - name: test_statistic
      title: "Test"
      type: text
```

#### Adverse Events Table
```yaml
- name: adverseEventsTable
  title: "Adverse Events Summary"
  type: Table
  rows: 0
  columns:
    - name: system_organ_class
      title: "System Organ Class"
      type: text
    - name: preferred_term
      title: "Preferred Term"
      type: text
    - name: grade
      title: "Grade"
      type: text
    - name: treatment_n
      title: "n"
      superTitle: "Treatment (N=${treatment_n})"
      type: integer
    - name: treatment_percent
      title: "%"
      superTitle: "Treatment (N=${treatment_n})"
      type: number
      format: pc
    - name: control_n
      title: "n"
      superTitle: "Control (N=${control_n})"
      type: integer
    - name: control_percent
      title: "%"
      superTitle: "Control (N=${control_n})"
      type: number
      format: pc
    - name: risk_difference
      title: "Risk Diff"
      type: number
      format: zto
    - name: p_value
      title: "p-value"
      type: number
      format: zto,pvalue
```

### Population Tables in Clinical Context

#### Survival Analysis Population
```r
# Population of median survival table
populate_median_survival_table <- function(surv_fit, table) {
    
    # Extract median survival times
    surv_summary <- summary(surv_fit)
    median_data <- surv_summary$table
    
    # Extract group information
    if (is.null(names(surv_fit$strata))) {
        # Single group analysis
        table$addRow(
            rowKey = 1,
            values = list(
                group = "Overall",
                n_total = median_data["records"],
                n_events = median_data["events"],
                median_survival = median_data["median"],
                ci_lower = median_data["0.95LCL"],
                ci_upper = median_data["0.95UCL"],
                restricted_mean = median_data["rmean"]
            )
        )
    } else {
        # Multi-group analysis
        group_names <- names(surv_fit$strata)
        
        for (i in seq_along(group_names)) {
            group_data <- median_data[i, ]
            
            table$addRow(
                rowKey = i,
                values = list(
                    group = gsub(".*=", "", group_names[i]),
                    n_total = group_data["records"],
                    n_events = group_data["events"],
                    median_survival = group_data["median"],
                    ci_lower = group_data["0.95LCL"],
                    ci_upper = group_data["0.95UCL"],
                    restricted_mean = group_data["rmean"]
                )
            )
        }
    }
    
    # Add footnotes
    table$setNote("method", "Median survival calculated using Kaplan-Meier estimator")
    table$setNote("ci", "95% confidence intervals calculated using log-log transformation")
}
```

#### ROC Analysis Population
```r
# Population of ROC analysis table
populate_roc_table <- function(roc_obj, optimal_cutoffs, table) {
    
    # Calculate metrics for each cutoff
    for (i in seq_along(optimal_cutoffs)) {
        cutoff <- optimal_cutoffs[i]
        
        # Find closest threshold in ROC object
        threshold_idx <- which.min(abs(roc_obj$thresholds - cutoff))
        
        # Extract metrics
        sens <- roc_obj$sensitivities[threshold_idx]
        spec <- roc_obj$specificities[threshold_idx]
        
        # Calculate predictive values (assuming prevalence)
        prevalence <- sum(roc_obj$response == levels(roc_obj$response)[2]) / length(roc_obj$response)
        ppv <- (sens * prevalence) / (sens * prevalence + (1 - spec) * (1 - prevalence))
        npv <- (spec * (1 - prevalence)) / ((1 - sens) * prevalence + spec * (1 - prevalence))
        
        table$addRow(
            rowKey = i,
            values = list(
                cutoff = cutoff,
                sensitivity = sens,
                specificity = spec,
                ppv = ppv,
                npv = npv,
                auc = as.numeric(roc_obj$auc),
                auc_ci_lower = as.numeric(roc_obj$ci)[1],
                auc_ci_upper = as.numeric(roc_obj$ci)[3]
            )
        )
    }
    
    # Add footnotes
    table$setNote("auc", sprintf("AUC: %.3f (95%% CI: %.3f-%.3f)", 
                                 as.numeric(roc_obj$auc),
                                 as.numeric(roc_obj$ci)[1],
                                 as.numeric(roc_obj$ci)[3]))
    table$setNote("prevalence", sprintf("Disease prevalence in sample: %.1f%%", 
                                        prevalence * 100))
}
```

---

## 9. Dynamic Tables and Conditional Display

### Conditional Table Visibility

#### Method-Based Conditional Display
```yaml
# Different tables for different analysis methods
- name: parametricSurvivalTable
  title: "Parametric Survival Analysis"
  type: Table
  visible: (analysisMethod:parametric)
  rows: 0
  columns:
    - name: distribution
      title: "Distribution"
      type: text
    - name: aic
      title: "AIC"
      type: number
      format: zto
    - name: bic
      title: "BIC"
      type: number
      format: zto

- name: semiparametricTable
  title: "Cox Proportional Hazards"
  type: Table
  visible: (analysisMethod:cox)
  rows: 0
  columns:
    - name: variable
      title: "Variable"
      type: text
    - name: hazard_ratio
      title: "HR"
      type: number
      format: zto
```

#### Complex Conditional Logic
```yaml
# Table visible only with specific combinations
- name: advancedAnalysisTable
  title: "Advanced Analysis Results"
  type: Table
  visible: (performAdvanced && covariates && (analysisMethod:cox || analysisMethod:parametric))
  rows: 0
  columns:
    # ... column definitions
```

### Dynamic Column Management

#### Adding Columns Based on Options
```r
# Add columns dynamically based on user selections
initialize_analysis_table <- function(self) {
    
    table <- self$results$analysisTable
    
    # Always include basic columns
    if (!table$hasColumn("variable")) {
        table$addColumn(name = "variable", title = "Variable", type = "text")
        table$addColumn(name = "estimate", title = "Estimate", type = "number", format = "zto")
    }
    
    # Add confidence intervals if requested
    if (self$options$includeConfidenceIntervals) {
        if (!table$hasColumn("ci_lower")) {
            table$addColumn(name = "ci_lower", title = "Lower", 
                          superTitle = "95% CI", type = "number", format = "zto")
            table$addColumn(name = "ci_upper", title = "Upper",
                          superTitle = "95% CI", type = "number", format = "zto")
        }
    }
    
    # Add p-values if statistical testing is enabled
    if (self$options$performTests) {
        if (!table$hasColumn("p_value")) {
            table$addColumn(name = "p_value", title = "p-value", 
                          type = "number", format = "zto,pvalue")
        }
    }
    
    # Add method-specific columns
    if (self$options$analysisMethod == "bootstrap") {
        if (!table$hasColumn("bootstrap_se")) {
            table$addColumn(name = "bootstrap_se", title = "Bootstrap SE",
                          type = "number", format = "zto")
        }
    }
}
```

### Progressive Table Building

#### Multi-Stage Table Population
```r
# Build tables progressively based on analysis stages
progressive_analysis <- function(self) {
    
    # Stage 1: Basic descriptives
    populate_descriptive_statistics(self)
    
    # Stage 2: Primary analysis (if data adequate)
    if (check_sample_size(self$data)) {
        populate_primary_analysis(self)
        
        # Stage 3: Advanced analysis (if options selected)
        if (self$options$performAdvanced) {
            populate_advanced_analysis(self)
            
            # Stage 4: Sensitivity analysis (if requested)
            if (self$options$performSensitivity) {
                populate_sensitivity_analysis(self)
            }
        }
    } else {
        # Add warning about insufficient data
        self$results$primaryAnalysisTable$addRow(
            rowKey = 1,
            values = list(
                message = "Insufficient data for primary analysis",
                required_n = "Minimum 30 observations required",
                current_n = nrow(self$data)
            )
        )
    }
}
```

### Responsive Table Content

#### Content Adaptation Based on Data
```r
# Adapt table content based on data characteristics
adaptive_table_population <- function(self, results) {
    
    table <- self$results$adaptiveTable
    
    # Check data structure
    n_groups <- length(unique(results$group))
    has_missing <- any(is.na(results$outcome))
    n_events <- sum(results$event, na.rm = TRUE)
    
    # Populate based on data characteristics
    if (n_groups == 1) {
        # Single group analysis
        populate_single_group_table(table, results)
        table$setNote("design", "Single-arm analysis")
        
    } else if (n_groups == 2) {
        # Two-group comparison
        populate_two_group_comparison(table, results)
        table$setNote("design", "Two-group comparison")
        
        # Add effect size if appropriate
        if (n_events >= 20) {
            add_effect_size_columns(table, results)
        }
        
    } else {
        # Multi-group analysis
        populate_multi_group_analysis(table, results)
        table$setNote("design", sprintf("Multi-group analysis (%d groups)", n_groups))
        
        # Add post-hoc comparisons if warranted
        if (self$options$performPostHoc) {
            populate_posthoc_table(self$results$postHocTable, results)
        }
    }
    
    # Handle missing data
    if (has_missing) {
        missing_count <- sum(is.na(results$outcome))
        table$setNote("missing", 
                      sprintf("Analysis excludes %d observations with missing outcome data", 
                              missing_count))
    }
}
```

---

## 10. Best Practices and Design Principles

### User Experience Design

#### 1. Clear and Descriptive Headers
```yaml
# Good: Descriptive and informative
- name: hazard_ratio
  title: "Hazard Ratio"
  type: number
  format: zto

- name: ci_lower
  title: "Lower"
  superTitle: "95% Confidence Interval"
  type: number
  format: zto

# Better: Even more specific
- name: adjusted_hazard_ratio
  title: "Adjusted HR"
  superTitle: "Multivariable Analysis"
  type: number
  format: zto
```

#### 2. Logical Column Organization
```yaml
# Organize columns in logical reading order
columns:
  # 1. Identification
  - name: variable
    title: "Variable"
    type: text
    
  - name: level
    title: "Level"
    type: text
  
  # 2. Sample size information
  - name: n_total
    title: "N"
    type: integer
    
  - name: n_events
    title: "Events"
    type: integer
  
  # 3. Main results
  - name: estimate
    title: "Estimate"
    type: number
    format: zto
  
  # 4. Uncertainty measures
  - name: ci_lower
    title: "Lower"
    superTitle: "95% CI"
    type: number
    format: zto
    
  - name: ci_upper
    title: "Upper"
    superTitle: "95% CI"
    type: number
    format: zto
  
  # 5. Statistical significance
  - name: p_value
    title: "p-value"
    type: number
    format: zto,pvalue
```

#### 3. Consistent Formatting Standards
```yaml
# Establish consistent formatting rules
formatting_standards:
  p_values: "zto,pvalue"      # Always use scientific notation cutoff
  percentages: "pc"           # Always show as percentages
  estimates: "zto"            # Standard decimal places
  confidence_intervals: "zto" # Match estimate formatting
  integers: "integer"         # No decimal places for counts
```

### Technical Best Practices

#### 4. Robust Error Handling
```r
# Comprehensive error handling for table population
safe_table_population <- function(self) {
    
    tryCatch({
        
        # Validate data before analysis
        if (is.null(self$data) || nrow(self$data) == 0) {
            add_error_message(self$results$analysisTable, "No data available")
            return()
        }
        
        # Check required variables
        required_vars <- c("outcome", "time", "group")
        missing_vars <- required_vars[!required_vars %in% names(self$data)]
        
        if (length(missing_vars) > 0) {
            add_error_message(self$results$analysisTable, 
                             paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
            return()
        }
        
        # Perform analysis with error checking
        results <- perform_analysis_safely(self$data, self$options)
        
        if (is.null(results)) {
            add_error_message(self$results$analysisTable, "Analysis failed to produce results")
            return()
        }
        
        # Populate table
        populate_table_safely(self$results$analysisTable, results)
        
    }, error = function(e) {
        # Log error for debugging
        cat("Table population error:", e$message, "\n")
        
        # Provide user-friendly error message
        add_error_message(self$results$analysisTable, 
                         "Analysis encountered an error. Please check your data and variable selections.")
    })
}

add_error_message <- function(table, message) {
    table$deleteRows()  # Clear any existing content
    table$addRow(
        rowKey = 1,
        values = list(
            error = message,
            suggestion = "Please review your data and analysis options"
        )
    )
}
```

#### 5. Performance Optimization
```r
# Optimize table population for large datasets
optimize_table_population <- function(self, results) {
    
    table <- self$results$largeAnalysisTable
    
    # Pre-allocate and batch process
    n_results <- nrow(results)
    
    # Process in chunks for very large results
    chunk_size <- 100
    
    for (chunk_start in seq(1, n_results, chunk_size)) {
        chunk_end <- min(chunk_start + chunk_size - 1, n_results)
        chunk_data <- results[chunk_start:chunk_end, ]
        
        # Process chunk
        for (i in seq_len(nrow(chunk_data))) {
            row_idx <- chunk_start + i - 1
            
            table$addRow(
                rowKey = row_idx,
                values = list(
                    variable = chunk_data$variable[i],
                    estimate = chunk_data$estimate[i],
                    p_value = chunk_data$p_value[i]
                )
            )
        }
        
        # Force garbage collection periodically
        if (chunk_start %% 500 == 1) {
            gc()
        }
    }
}
```

#### 6. Data Validation and Quality Checks
```r
# Validate data before table population
validate_analysis_data <- function(data, options) {
    
    validation_results <- list(
        valid = TRUE,
        warnings = character(),
        errors = character()
    )
    
    # Check sample size
    if (nrow(data) < 10) {
        validation_results$warnings <- c(validation_results$warnings,
                                       "Small sample size may affect reliability of results")
    }
    
    # Check for missing data
    outcome_var <- options$outcome
    if (sum(is.na(data[[outcome_var]])) > nrow(data) * 0.5) {
        validation_results$errors <- c(validation_results$errors,
                                     "More than 50% of outcome data is missing")
        validation_results$valid <- FALSE
    }
    
    # Check for sufficient events (survival analysis)
    if (options$analysisType == "survival") {
        n_events <- sum(data[[options$event]], na.rm = TRUE)
        if (n_events < 10) {
            validation_results$errors <- c(validation_results$errors,
                                         "Insufficient events for survival analysis (minimum 10 required)")
            validation_results$valid <- FALSE
        }
    }
    
    return(validation_results)
}
```

### Clinical Research Standards

#### 7. Statistical Reporting Standards
```r
# Follow clinical research reporting standards
format_clinical_results <- function(estimate, ci_lower, ci_upper, p_value) {
    
    # Format estimate with confidence interval
    if (!is.na(estimate) && !is.na(ci_lower) && !is.na(ci_upper)) {
        estimate_text <- sprintf("%.2f (%.2f to %.2f)", estimate, ci_lower, ci_upper)
    } else {
        estimate_text <- ifelse(is.na(estimate), "NR", sprintf("%.2f", estimate))
    }
    
    # Format p-value according to statistical guidelines
    if (!is.na(p_value)) {
        if (p_value < 0.001) {
            p_text <- "< 0.001"
        } else if (p_value >= 0.001 && p_value < 0.01) {
            p_text <- sprintf("%.3f", p_value)
        } else {
            p_text <- sprintf("%.2f", p_value)
        }
    } else {
        p_text <- "NR"
    }
    
    return(list(estimate = estimate_text, p_value = p_text))
}
```

#### 8. Comprehensive Documentation
```r
# Add comprehensive footnotes for clinical tables
add_clinical_footnotes <- function(table, analysis_type, options) {
    
    # Method footnote
    if (analysis_type == "survival") {
        table$setNote("method", 
                     "Survival analysis performed using Kaplan-Meier method and Cox proportional hazards regression")
    } else if (analysis_type == "diagnostic") {
        table$setNote("method",
                     "Diagnostic accuracy calculated with exact binomial confidence intervals")
    }
    
    # Sample size footnote
    table$setNote("sample", 
                 sprintf("Analysis includes %d participants with complete data", 
                         options$analysis_n))
    
    # Statistical significance footnote
    table$setNote("significance", 
                 sprintf("Statistical significance defined as p < %.2f (two-sided)", 
                         options$alpha_level))
    
    # Missing data footnote
    if (options$missing_n > 0) {
        table$setNote("missing",
                     sprintf("%d participants excluded due to missing data", 
                             options$missing_n))
    }
    
    # Software footnote
    table$setNote("software",
                 "Analysis conducted using R statistical software with jamovi interface")
}
```

---

## 11. Complete Examples

### Example 1: Basic Clinical Characteristics Table

#### .r.yaml Definition
```yaml
- name: clinicalCharacteristicsTable
  title: "Patient Clinical Characteristics"
  type: Table
  rows: 0
  columns:
    - name: characteristic
      title: "Characteristic"
      type: text
    - name: overall
      title: "Overall\n(N = ${total_n})"
      type: text
    - name: group_a
      title: "Group A\n(N = ${group_a_n})"
      type: text
    - name: group_b
      title: "Group B\n(N = ${group_b_n})"
      type: text
    - name: p_value
      title: "p-value"
      type: number
      format: zto,pvalue
    - name: test_used
      title: "Test"
      type: text
  clearWith:
    - outcome
    - grouping
    - includeTests
  visible: true
```

#### .b.R Implementation
```r
# Populate clinical characteristics table
populate_clinical_characteristics <- function(self) {
    
    table <- self$results$clinicalCharacteristicsTable
    data <- self$data
    
    # Define characteristics to analyze
    characteristics <- c("age", "sex", "bmi", "smoking_status", "comorbidities")
    
    for (char in characteristics) {
        if (char %in% names(data)) {
            
            if (is.numeric(data[[char]])) {
                # Continuous variable
                result <- analyze_continuous_variable(data, char, self$options$grouping)
                
                table$addRow(
                    rowKey = char,
                    values = list(
                        characteristic = get_variable_label(char),
                        overall = sprintf("%.1f (%.1f)", result$overall_mean, result$overall_sd),
                        group_a = sprintf("%.1f (%.1f)", result$group_a_mean, result$group_a_sd),
                        group_b = sprintf("%.1f (%.1f)", result$group_b_mean, result$group_b_sd),
                        p_value = result$p_value,
                        test_used = "t-test"
                    )
                )
                
            } else {
                # Categorical variable
                result <- analyze_categorical_variable(data, char, self$options$grouping)
                
                # Add main category row
                table$addRow(
                    rowKey = paste0(char, "_main"),
                    values = list(
                        characteristic = get_variable_label(char),
                        overall = "",
                        group_a = "",
                        group_b = "",
                        p_value = result$p_value,
                        test_used = "Chi-square"
                    )
                )
                
                # Add subcategory rows
                for (level in names(result$levels)) {
                    level_result <- result$levels[[level]]
                    
                    table$addRow(
                        rowKey = paste0(char, "_", level),
                        values = list(
                            characteristic = paste0("  ", level),
                            overall = sprintf("%d (%.1f%%)", level_result$overall_n, level_result$overall_pct),
                            group_a = sprintf("%d (%.1f%%)", level_result$group_a_n, level_result$group_a_pct),
                            group_b = sprintf("%d (%.1f%%)", level_result$group_b_n, level_result$group_b_pct),
                            p_value = NA,
                            test_used = ""
                        )
                    )
                }
            }
        }
    }
    
    # Add footnotes
    table$setNote("continuous", "Continuous variables presented as mean (standard deviation)")
    table$setNote("categorical", "Categorical variables presented as n (%)")
    table$setNote("tests", "p-values from t-tests for continuous and chi-square tests for categorical variables")
}
```

### Example 2: Comprehensive Survival Analysis Table

#### .r.yaml Definition
```yaml
- name: comprehensiveSurvivalTable
  title: "`Survival Analysis Results - ${outcome}`"
  type: Table
  rows: 0
  columns:
    - name: analysis_type
      title: "Analysis"
      type: text
    - name: group
      title: "Group"
      type: text
    - name: n_total
      title: "N"
      superTitle: "Sample Size"
      type: integer
    - name: n_events
      title: "Events"
      superTitle: "Sample Size"
      type: integer
    - name: person_time
      title: "Person-Time"
      type: number
      format: zto
    - name: median_survival
      title: "Median"
      superTitle: "Survival Time"
      type: number
      format: zto
    - name: survival_ci_lower
      title: "Lower"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: survival_ci_upper
      title: "Upper"
      superTitle: "95% CI"
      type: number
      format: zto
    - name: hazard_ratio
      title: "HR"
      superTitle: "Hazard Ratio"
      type: number
      format: zto
    - name: hr_ci_lower
      title: "Lower"
      superTitle: "HR 95% CI"
      type: number
      format: zto
    - name: hr_ci_upper
      title: "Upper"
      superTitle: "HR 95% CI"
      type: number
      format: zto
    - name: log_rank_p
      title: "Log-rank p"
      type: number
      format: zto,pvalue
    - name: cox_p
      title: "Cox p"
      type: number
      format: zto,pvalue
  clearWith:
    - outcome
    - timevar
    - grouping
    - covariates
  visible: true
```

#### .b.R Implementation
```r
# Comprehensive survival analysis table population
populate_comprehensive_survival_table <- function(self) {
    
    table <- self$results$comprehensiveSurvivalTable
    data <- self$data
    
    # Prepare survival formula
    time_var <- self$options$timevar
    event_var <- self$options$outcome
    group_var <- self$options$grouping
    
    surv_formula <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var))
    
    tryCatch({
        
        # Kaplan-Meier analysis
        km_fit <- survfit(surv_formula, data = data)
        km_summary <- summary(km_fit)
        
        # Log-rank test
        logrank_test <- survdiff(surv_formula, data = data)
        logrank_p <- 1 - pchisq(logrank_test$chisq, length(logrank_test$n) - 1)
        
        # Cox regression
        cox_fit <- coxph(surv_formula, data = data)
        cox_summary <- summary(cox_fit)
        
        # Extract group information
        groups <- names(km_fit$strata)
        if (is.null(groups)) {
            groups <- "Overall"
        } else {
            groups <- gsub(".*=", "", groups)
        }
        
        # Populate table for each group
        for (i in seq_along(groups)) {
            
            # Extract Kaplan-Meier results
            if (length(groups) == 1) {
                km_data <- km_summary$table
                median_surv <- km_data["median"]
                median_ci_lower <- km_data["0.95LCL"]
                median_ci_upper <- km_data["0.95UCL"]
                n_total <- km_data["records"]
                n_events <- km_data["events"]
                
            } else {
                km_data <- km_summary$table[i, ]
                median_surv <- km_data["median"]
                median_ci_lower <- km_data["0.95LCL"]
                median_ci_upper <- km_data["0.95UCL"]
                n_total <- km_data["records"]
                n_events <- km_data["events"]
            }
            
            # Extract Cox regression results
            if (i == 1) {
                # Reference group
                hazard_ratio <- 1.0
                hr_ci_lower <- NA
                hr_ci_upper <- NA
                cox_p <- NA
            } else {
                # Comparison groups
                cox_coef_idx <- i - 1
                hazard_ratio <- cox_summary$coefficients[cox_coef_idx, "exp(coef)"]
                hr_ci_lower <- cox_summary$conf.int[cox_coef_idx, "lower .95"]
                hr_ci_upper <- cox_summary$conf.int[cox_coef_idx, "upper .95"]
                cox_p <- cox_summary$coefficients[cox_coef_idx, "Pr(>|z|)"]
            }
            
            # Calculate person-time
            person_time <- sum(data[[time_var]][data[[group_var]] == groups[i]], na.rm = TRUE)
            
            # Add row to table
            table$addRow(
                rowKey = i,
                values = list(
                    analysis_type = ifelse(i == 1, "Kaplan-Meier", ""),
                    group = groups[i],
                    n_total = n_total,
                    n_events = n_events,
                    person_time = person_time,
                    median_survival = median_surv,
                    survival_ci_lower = median_ci_lower,
                    survival_ci_upper = median_ci_upper,
                    hazard_ratio = hazard_ratio,
                    hr_ci_lower = hr_ci_lower,
                    hr_ci_upper = hr_ci_upper,
                    log_rank_p = ifelse(i == 1, logrank_p, NA),
                    cox_p = cox_p
                )
            )
        }
        
        # Add comprehensive footnotes
        add_survival_footnotes(table, km_fit, cox_fit, logrank_test)
        
    }, error = function(e) {
        # Handle errors gracefully
        table$addRow(
            rowKey = 1,
            values = list(
                analysis_type = "Error",
                group = paste("Analysis failed:", e$message),
                n_total = NA,
                n_events = NA,
                person_time = NA,
                median_survival = NA,
                survival_ci_lower = NA,
                survival_ci_upper = NA,
                hazard_ratio = NA,
                hr_ci_lower = NA,
                hr_ci_upper = NA,
                log_rank_p = NA,
                cox_p = NA
            )
        )
    })
}

# Helper function for footnotes
add_survival_footnotes <- function(table, km_fit, cox_fit, logrank_test) {
    
    table$setNote("km_method", 
                 "Median survival times calculated using Kaplan-Meier method")
    table$setNote("km_ci", 
                 "95% confidence intervals for survival times calculated using log-log transformation")
    table$setNote("cox_method", 
                 "Hazard ratios calculated using Cox proportional hazards regression")
    table$setNote("cox_assumptions", 
                 "Cox model assumes proportional hazards over time")
    table$setNote("reference", 
                 sprintf("Reference group: %s", names(km_fit$strata)[1]))
    table$setNote("logrank", 
                 sprintf("Log-rank test statistic: %.2f (df = %d)", 
                         logrank_test$chisq, length(logrank_test$n) - 1))
    table$setNote("person_time", 
                 "Person-time calculated as sum of individual follow-up times")
}
```

### Example 3: Diagnostic Accuracy Analysis Table

#### .r.yaml Definition
```yaml
- name: diagnosticAccuracyTable
  title: "Diagnostic Test Performance"
  type: Table
  rows: 0
  columns:
    - name: metric
      title: "Metric"
      type: text
    - name: estimate
      title: "Estimate"
      type: number
      format: pc
    - name: ci_lower
      title: "Lower"
      superTitle: "95% Confidence Interval"
      type: number
      format: pc
    - name: ci_upper
      title: "Upper"  
      superTitle: "95% Confidence Interval"
      type: number
      format: pc
    - name: interpretation
      title: "Clinical Interpretation"
      type: text
  clearWith:
    - testResult
    - goldStandard
    - prevalenceCorrection
  visible: true
```

#### .b.R Implementation
```r
# Populate diagnostic accuracy table
populate_diagnostic_accuracy_table <- function(self) {
    
    table <- self$results$diagnosticAccuracyTable
    data <- self$data
    
    test_var <- self$options$testResult
    gold_var <- self$options$goldStandard
    
    tryCatch({
        
        # Create confusion matrix
        confusion_matrix <- table(data[[test_var]], data[[gold_var]])
        
        # Extract values (assuming 2x2 table)
        if (all(dim(confusion_matrix) == c(2, 2))) {
            
            # Standard 2x2 confusion matrix
            # Rows: Test result (Negative, Positive)
            # Cols: True status (Negative, Positive)
            tn <- confusion_matrix[1, 1]  # True negatives
            fp <- confusion_matrix[2, 1]  # False positives
            fn <- confusion_matrix[1, 2]  # False negatives
            tp <- confusion_matrix[2, 2]  # True positives
            
            # Calculate diagnostic metrics with confidence intervals
            metrics <- calculate_diagnostic_metrics(tp, fp, fn, tn)
            
            # Populate table
            table$addRow(
                rowKey = "sensitivity",
                values = list(
                    metric = "Sensitivity",
                    estimate = metrics$sensitivity$estimate,
                    ci_lower = metrics$sensitivity$ci_lower,
                    ci_upper = metrics$sensitivity$ci_upper,
                    interpretation = interpret_sensitivity(metrics$sensitivity$estimate)
                )
            )
            
            table$addRow(
                rowKey = "specificity",
                values = list(
                    metric = "Specificity",
                    estimate = metrics$specificity$estimate,
                    ci_lower = metrics$specificity$ci_lower,
                    ci_upper = metrics$specificity$ci_upper,
                    interpretation = interpret_specificity(metrics$specificity$estimate)
                )
            )
            
            table$addRow(
                rowKey = "ppv",
                values = list(
                    metric = "Positive Predictive Value",
                    estimate = metrics$ppv$estimate,
                    ci_lower = metrics$ppv$ci_lower,
                    ci_upper = metrics$ppv$ci_upper,
                    interpretation = interpret_ppv(metrics$ppv$estimate)
                )
            )
            
            table$addRow(
                rowKey = "npv",
                values = list(
                    metric = "Negative Predictive Value",
                    estimate = metrics$npv$estimate,
                    ci_lower = metrics$npv$ci_lower,
                    ci_upper = metrics$npv$ci_upper,
                    interpretation = interpret_npv(metrics$npv$estimate)
                )
            )
            
            table$addRow(
                rowKey = "accuracy",
                values = list(
                    metric = "Overall Accuracy",
                    estimate = metrics$accuracy$estimate,
                    ci_lower = metrics$accuracy$ci_lower,
                    ci_upper = metrics$accuracy$ci_upper,
                    interpretation = interpret_accuracy(metrics$accuracy$estimate)
                )
            )
            
            # Add likelihood ratios
            table$addRow(
                rowKey = "lr_positive",
                values = list(
                    metric = "Positive Likelihood Ratio",
                    estimate = metrics$lr_positive$estimate,
                    ci_lower = metrics$lr_positive$ci_lower,
                    ci_upper = metrics$lr_positive$ci_upper,
                    interpretation = interpret_lr_positive(metrics$lr_positive$estimate)
                )
            )
            
            table$addRow(
                rowKey = "lr_negative",
                values = list(
                    metric = "Negative Likelihood Ratio",
                    estimate = metrics$lr_negative$estimate,
                    ci_lower = metrics$lr_negative$ci_lower,
                    ci_upper = metrics$lr_negative$ci_upper,
                    interpretation = interpret_lr_negative(metrics$lr_negative$estimate)
                )
            )
            
            # Add diagnostic footnotes
            add_diagnostic_footnotes(table, confusion_matrix, metrics)
            
        } else {
            # Handle non-2x2 tables
            table$addRow(
                rowKey = 1,
                values = list(
                    metric = "Error",
                    estimate = NA,
                    ci_lower = NA,
                    ci_upper = NA,
                    interpretation = "Analysis requires binary test and reference standard"
                )
            )
        }
        
    }, error = function(e) {
        table$addRow(
            rowKey = 1,
            values = list(
                metric = "Error",
                estimate = NA,
                ci_lower = NA,
                ci_upper = NA,
                interpretation = paste("Analysis failed:", e$message)
            )
        )
    })
}

# Helper functions for diagnostic metrics
calculate_diagnostic_metrics <- function(tp, fp, fn, tn) {
    
    # Calculate estimates
    sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    ppv <- tp / (tp + fp)
    npv <- tn / (tn + fn)
    accuracy <- (tp + tn) / (tp + fp + fn + tn)
    lr_positive <- sensitivity / (1 - specificity)
    lr_negative <- (1 - sensitivity) / specificity
    
    # Calculate confidence intervals using exact binomial method
    sens_ci <- binom.test(tp, tp + fn)$conf.int
    spec_ci <- binom.test(tn, tn + fp)$conf.int
    ppv_ci <- binom.test(tp, tp + fp)$conf.int
    npv_ci <- binom.test(tn, tn + fn)$conf.int
    acc_ci <- binom.test(tp + tn, tp + fp + fn + tn)$conf.int
    
    # Return structured results
    return(list(
        sensitivity = list(estimate = sensitivity, ci_lower = sens_ci[1], ci_upper = sens_ci[2]),
        specificity = list(estimate = specificity, ci_lower = spec_ci[1], ci_upper = spec_ci[2]),
        ppv = list(estimate = ppv, ci_lower = ppv_ci[1], ci_upper = ppv_ci[2]),
        npv = list(estimate = npv, ci_lower = npv_ci[1], ci_upper = npv_ci[2]),
        accuracy = list(estimate = accuracy, ci_lower = acc_ci[1], ci_upper = acc_ci[2]),
        lr_positive = list(estimate = lr_positive, ci_lower = NA, ci_upper = NA),
        lr_negative = list(estimate = lr_negative, ci_lower = NA, ci_upper = NA)
    ))
}

# Clinical interpretation functions
interpret_sensitivity <- function(value) {
    if (value >= 0.95) return("Excellent - very few false negatives")
    if (value >= 0.85) return("Good - acceptable false negative rate")
    if (value >= 0.70) return("Moderate - consider impact of missed cases")
    return("Poor - high false negative rate")
}

interpret_specificity <- function(value) {
    if (value >= 0.95) return("Excellent - very few false positives")
    if (value >= 0.85) return("Good - acceptable false positive rate")
    if (value >= 0.70) return("Moderate - consider impact of false alarms")
    return("Poor - high false positive rate")
}

add_diagnostic_footnotes <- function(table, confusion_matrix, metrics) {
    
    table$setNote("method", "95% confidence intervals calculated using exact binomial method")
    table$setNote("confusion_matrix", 
                 sprintf("2×2 contingency table: TP=%d, FP=%d, FN=%d, TN=%d",
                         confusion_matrix[2,2], confusion_matrix[2,1], 
                         confusion_matrix[1,2], confusion_matrix[1,1]))
    table$setNote("prevalence", 
                 sprintf("Disease prevalence in sample: %.1f%%",
                         sum(confusion_matrix[,2]) / sum(confusion_matrix) * 100))
    table$setNote("interpretation", 
                 "Clinical interpretation based on established diagnostic accuracy thresholds")
}
```

---

## Conclusion

This comprehensive guide provides everything needed to create professional, sophisticated tables in jamovi modules. Key principles for success:

### Essential Guidelines

1. **User-Centered Design** - Structure tables to match clinical and research workflows
2. **Consistent Standards** - Apply formatting and organization patterns consistently
3. **Robust Implementation** - Include error handling and validation at every step
4. **Clear Communication** - Use descriptive headers, logical organization, and helpful footnotes
5. **Performance Optimization** - Implement efficient population methods for large datasets

### Advanced Capabilities

- **Dynamic Tables** - Adapt content based on user selections and data characteristics
- **HTML Integration** - Leverage R packages for publication-ready formatting
- **Clinical Standards** - Follow established reporting guidelines for medical research
- **Error Resilience** - Gracefully handle edge cases and data quality issues

### Next Steps

After mastering table creation, explore:
- Integration with plot generation for comprehensive results
- Advanced conditional logic for sophisticated user interfaces
- Custom HTML styling for branded presentation
- Performance optimization for large-scale analyses

This guide establishes the foundation for creating professional, reliable tables that effectively communicate statistical results in clinical and research contexts. The patterns and practices outlined here scale from simple descriptive tables to complex multi-dimensional analyses, ensuring your jamovi modules provide users with clear, actionable insights.