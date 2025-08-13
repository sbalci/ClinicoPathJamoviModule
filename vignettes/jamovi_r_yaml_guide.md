# The Complete Guide to Writing `.r.yaml` Files for jamovi Development

This is the definitive, comprehensive guide to writing `.r.yaml` files for developing jamovi modules. These files define the structure and format of all output elements that your analysis will produce, from tables and plots to HTML content and data exports.

## Table of Contents

1. [Introduction: The Role of `.r.yaml`](#1-introduction-the-role-of-ryaml)
2. [File Structure and Core Components](#2-file-structure-and-core-components)
3. [Essential Top-Level Properties](#3-essential-top-level-properties)
4. [The `items` Section: Complete Reference](#4-the-items-section-complete-reference)
5. [Result Element Types: Detailed Coverage](#5-result-element-types-detailed-coverage)
6. [Table Design and Column Specifications](#6-table-design-and-column-specifications)
7. [Advanced Features and Patterns](#7-advanced-features-and-patterns)
8. [Clinical and Scientific Output Examples](#8-clinical-and-scientific-output-examples)
9. [Best Practices and Design Guidelines](#9-best-practices-and-design-guidelines)
10. [Integration with `.b.R` Implementation](#10-integration-with-br-implementation)
11. [Complete Examples](#11-complete-examples)

---

## 1. Introduction: The Role of `.r.yaml`

The `.r.yaml` file serves as the **results definition** for a jamovi analysis, acting as a blueprint that specifies:

- **Output Structure**: What types of results will be displayed (tables, plots, HTML content)
- **Table Schemas**: Column definitions, data types, and formatting for all tables
- **Plot Specifications**: Dimensions, rendering functions, and display conditions for graphics
- **Content Organization**: How results are grouped and presented to users
- **Dynamic Elements**: Conditional visibility and context-sensitive titles

### The jamovi Module Architecture Integration

The `.r.yaml` file works in concert with:

- **`.a.yaml`** - Defines input parameters and UI elements
- **`.u.yaml`** - Defines visual layout and interface design  
- **`.b.R`** - Contains the R implementation that populates the results structure

**Compilation Process**: The `.r.yaml` file is compiled into `.h.R` header files that provide base classes with predefined result structures for your R implementation.

---

## 2. File Structure and Core Components

Every `.r.yaml` file follows a consistent hierarchical structure:

```yaml
---
name: analysisname
title: Human Readable Results Title
jrs: '1.1'

items:
    - name: result1
      title: First Result
      type: ResultType
      # ... result-specific properties
      
    - name: result2
      title: Second Result  
      type: AnotherType
      # ... more properties

refs:
    - reference1
    - reference2

...
```

---

## 3. Essential Top-Level Properties

### Core Identification Properties

#### `name` (Required)
- **Type**: String
- **Purpose**: Must exactly match the corresponding `.a.yaml`, `.u.yaml`, and `.b.R` files
- **Rules**: Single word, no spaces, valid R identifier
- **Example**: `name: advancedsurvival`

#### `title` (Required)
- **Type**: String (can include dynamic variables)
- **Purpose**: Primary title displayed at the top of results
- **Dynamic Syntax**: Use `${variableName}` to reference analysis options
- **Examples**:
  ```yaml
  title: Advanced Survival Analysis
  title: '`Survival Analysis - ${explanatory}`'
  title: '`Cross Table - ${group} by ${outcome}`'
  ```

#### `jrs` (Required)
- **Type**: String (quoted)
- **Purpose**: jamovi Results Specification version
- **Current Standard**: `'1.1'`
- **Example**: `jrs: '1.1'`

### Documentation Properties

#### `refs` (Optional)
- **Type**: Array of strings
- **Purpose**: References to R packages, methodologies, or citations
- **Usage**: Appears in jamovi output for proper attribution
- **Examples**:
  ```yaml
  refs:
      - survival
      - survminer
      - ClinicoPathJamoviModule
  ```

---

## 4. The `items` Section: Complete Reference

The `items` section defines every element that will appear in the analysis results. Each item is a YAML object with specific properties.

### Universal Item Properties

These properties can be used with any result element type:

#### `name` (Required)
- **Type**: String
- **Purpose**: Unique identifier used in R code to access the result element
- **Rules**: Valid R variable name, unique within the analysis
- **Example**: `name: medianSurvivalTable`

#### `title` (Required)
- **Type**: String (supports dynamic variables)
- **Purpose**: Display title shown to users
- **Examples**:
  ```yaml
  title: Summary Statistics
  title: '`Median Survival Table: Levels for ${explanatory}`'
  title: Cox Regression Results
  ```

#### `type` (Required)
- **Type**: String
- **Purpose**: Specifies the result element type
- **Common Types**: `Table`, `Image`, `Html`, `Preformatted`, `Output`, `Group`

#### `visible` (Optional)
- **Type**: Boolean or condition string
- **Purpose**: Controls when the element appears in output
- **Examples**:
  ```yaml
  visible: true                    # Always visible
  visible: false                   # Never visible (hidden)
  visible: (showTable)             # Visible when showTable option is true
  visible: (method:cox_regression) # Visible when method equals "cox_regression"
  visible: (addPValue)             # Visible when addPValue option is checked
  ```

#### `clearWith` (Recommended)
- **Type**: Array of option names
- **Purpose**: Clears element content when specified options change
- **Critical**: Ensures results stay synchronized with analysis parameters
- **Examples**:
  ```yaml
  clearWith:
      - outcome
      - covariates
      - method
  ```

#### `description` (Optional)
- **Type**: String
- **Purpose**: Additional information or help text
- **Example**: `description: Kaplan-Meier survival curves with risk tables`

---

## 5. Result Element Types: Detailed Coverage

### Core Output Types

#### `Html`
Renders rich HTML content with full formatting capability.

```yaml
- name: summaryText
  title: Analysis Summary
  type: Html
  visible: (showSummary)
  clearWith:
      - method
      - variables
```

**Use Cases**:
- Instructions and help text
- Formatted analysis summaries
- Complex narrative results
- Custom formatted statistical output

**R Implementation Access**:
```r
# In your .b.R file
self$results$summaryText$setContent(html_content)
```

#### `Preformatted`
Displays plain text with preserved formatting and monospace font.

```yaml
- name: codeOutput
  title: R Code
  type: Preformatted
  visible: (showCode)
  clearWith:
      - allOptions
```

**Use Cases**:
- R code display
- Console output
- Fixed-width formatted text
- Algorithm descriptions

#### `Table`
The most complex and powerful result type for tabular data.

```yaml
- name: coefficientsTable
  title: Regression Coefficients
  type: Table
  rows: 0
  clearWith:
      - covariates
      - method
  columns:
      - name: term
        title: Term
        type: text
      - name: estimate
        title: Estimate
        type: number
        format: zto3
      - name: pvalue
        title: p
        type: number
        format: pvalue
```

**Key Properties**:
- `rows`: Number of rows (`0` = dynamic based on data)
- `columns`: Array of column definitions (detailed in Section 6)

#### `Image`
For plots, graphs, and other visual outputs.

```yaml
- name: survivalPlot
  title: '`Survival Curves - ${grouping}`'
  type: Image
  width: 700
  height: 500
  renderFun: .plotSurvival
  visible: (showPlot)
  requiresData: true
  clearWith:
      - outcome
      - grouping
      - timeVar
```

**Properties**:
- `width`: Plot width in pixels
- `height`: Plot height in pixels  
- `renderFun`: R function name that generates the plot
- `requiresData`: Whether plot needs data to render

#### `Output`
Creates new variables in the original dataset.

```yaml
- name: riskScore
  title: Calculated Risk Score
  type: Output
  varTitle: '`Risk Score - ${model}`'
  varDescription: Calculated risk score from logistic regression model
  measureType: continuous
  clearWith:
      - predictors
      - model
```

**Properties**:
- `varTitle`: Name for the new variable
- `varDescription`: Description of the variable
- `measureType`: Data type (`continuous`, `nominal`, `ordinal`)

#### `Group`
Organizes multiple result elements into collapsible sections.

```yaml
- name: diagnosticTests
  title: Model Diagnostics
  type: Group
  visible: (performDiagnostics)
  items:
      - name: residualPlots
        title: Residual Plots
        type: Image
        # ... plot properties
        
      - name: goodnessOfFit
        title: Goodness of Fit
        type: Table
        # ... table properties
```

---

## 6. Table Design and Column Specifications

Tables are the most complex result elements, requiring detailed column specifications.

### Column Definition Properties

#### Basic Column Properties

```yaml
columns:
    - name: variableName        # Required: Column identifier
      title: Display Name       # Required: Header text
      type: text                # Required: Data type
      format: none              # Optional: Formatting
      superTitle: Group Header  # Optional: Spanning header
      visible: true            # Optional: Visibility condition
```

#### Column Data Types

**`text`** - String data
```yaml
- name: term
  title: Variable
  type: text
```

**`number`** - Numeric data with formatting options
```yaml
- name: estimate
  title: Estimate
  type: number
  format: zto3          # 3 decimal places
```

**`integer`** - Whole numbers
```yaml
- name: n
  title: N
  type: integer
```

### Formatting Options

#### Numeric Formats

```yaml
# Common numeric formats
format: zto3        # 0.000 (3 decimal places)
format: zto2        # 0.00 (2 decimal places)  
format: sf3         # Scientific notation with 3 significant figures
format: pc          # Percentage (e.g., 45.2%)
format: pvalue      # P-value formatting (< 0.001, etc.)
format: currency    # Currency formatting ($1,234.56)
```

#### Advanced Formatting Examples

```yaml
# Confidence intervals
- name: ci_lower
  title: Lower
  superTitle: 95% Confidence Interval
  type: number
  format: zto3

- name: ci_upper
  title: Upper  
  superTitle: 95% Confidence Interval
  type: number
  format: zto3

# Statistical results
- name: statistic
  title: χ²
  type: number
  format: sf2

- name: pvalue
  title: p
  type: number
  format: pvalue
```

### Complex Table Examples

#### Survival Analysis Table
```yaml
- name: medianTable
  title: '`Median Survival: ${grouping}`'
  type: Table
  rows: 0
  clearWith:
      - grouping
      - timeVar
      - eventVar
  columns:
      - name: group
        title: Group
        type: text
      - name: n
        title: N
        type: integer
      - name: events
        title: Events
        type: integer
      - name: median
        title: Median Survival
        type: number
        format: zto1
      - name: lcl
        title: Lower
        superTitle: 95% CI
        type: number
        format: zto1
      - name: ucl
        title: Upper
        superTitle: 95% CI
        type: number
        format: zto1
```

#### Regression Results Table
```yaml
- name: coefficientsTable
  title: Regression Coefficients
  type: Table
  rows: 0
  clearWith:
      - covariates
      - method
  columns:
      - name: term
        title: Term
        type: text
      - name: estimate
        title: Estimate
        type: number
        format: zto3
      - name: se
        title: SE
        type: number
        format: zto3
      - name: statistic
        title: z
        type: number
        format: zto2
      - name: pvalue
        title: p
        type: number
        format: pvalue
      - name: ci_lower
        title: Lower
        superTitle: 95% CI
        type: number
        format: zto3
      - name: ci_upper
        title: Upper
        superTitle: 95% CI
        type: number
        format: zto3
```

---

## 7. Advanced Features and Patterns

### Conditional Visibility Patterns

#### Simple Boolean Conditions
```yaml
visible: (showTable)              # When option is true
visible: (!hideResults)           # When option is false (negation)
```

#### Value-Based Conditions
```yaml
visible: (method:cox_regression)  # When method equals specific value
visible: (analysisType:advanced)  # When analysisType equals "advanced"
```

#### Multiple Conditions
```yaml
visible: (showTable && hasData)   # Both conditions must be true
visible: (method:cox || method:parametric)  # Either condition true
```

### Dynamic Titles and Content

#### Variable Substitution
```yaml
title: '`Analysis Results - ${outcome} by ${grouping}`'
title: '`${method} - ${n} subjects`'
title: '`Survival Analysis: ${eventVar} over ${timeVar}`'
```

#### Contextual Information
```yaml
title: '`Median Survival Table: Levels for ${explanatory}`'
varTitle: '`Predicted Risk - ${model} Model`'
varDescription: '`Risk prediction using ${predictors}`'
```

### Result Organization Patterns

#### Sequential Results Flow
```yaml
items:
    # Instructions/Overview
    - name: overview
      title: Analysis Overview
      type: Html
      visible: (showInstructions)
      
    # Main Results
    - name: primaryTable
      title: Primary Results
      type: Table
      
    # Supporting Tables  
    - name: detailsTable
      title: Detailed Results
      type: Table
      visible: (showDetails)
      
    # Visualizations
    - name: mainPlot
      title: Results Plot
      type: Image
      visible: (showPlot)
```

#### Grouped Organization
```yaml
items:
    - name: basicResults
      title: Basic Analysis
      type: Group
      items:
          - name: summaryTable
            title: Summary Statistics
            type: Table
            
    - name: advancedResults  
      title: Advanced Analysis
      type: Group
      visible: (performAdvanced)
      items:
          - name: detailedTable
            title: Detailed Results
            type: Table
```

### Content Synchronization

#### Comprehensive `clearWith` Usage
```yaml
clearWith:
    # Core variables
    - outcome
    - predictors
    - grouping
    
    # Analysis options
    - method
    - performBootstrap
    - confidenceLevel
    
    # Display options
    - showDetails
    - includeConfidenceIntervals
```

---

## 8. Clinical and Scientific Output Examples

### Survival Analysis Results

```yaml
---
name: advancedsurvival
title: '`Advanced Survival Analysis - ${outcome}`'
jrs: '1.1'

items:
    # Analysis Overview
    - name: analysisOverview
      title: Analysis Summary
      type: Html
      visible: (showSummary)
      
    # Survival Summary Statistics
    - name: survivalSummary
      title: '`Survival Summary - ${grouping}`'
      type: Table
      rows: 0
      clearWith:
          - outcome
          - eventVar
          - timeVar
          - grouping
      columns:
          - name: group
            title: Group
            type: text
          - name: n
            title: N
            type: integer
          - name: events
            title: Events
            type: integer
          - name: median
            title: Median
            type: number
            format: zto1
          - name: ci_lower
            title: Lower
            superTitle: 95% CI
            type: number
            format: zto1
          - name: ci_upper
            title: Upper
            superTitle: 95% CI
            type: number
            format: zto1
            
    # Kaplan-Meier Plot
    - name: survivalPlot
      title: '`Survival Curves - ${grouping}`'
      type: Image
      width: 700
      height: 500
      renderFun: .plotKM
      visible: (showPlot)
      requiresData: true
      clearWith:
          - outcome
          - grouping
          - timeVar
          
    # Log-rank Test Results
    - name: logRankTest
      title: Log-Rank Test
      type: Table
      visible: (performLogRank)
      rows: 1
      clearWith:
          - grouping
          - outcome
      columns:
          - name: statistic
            title: χ²
            type: number
            format: zto2
          - name: df
            title: df
            type: integer
          - name: pvalue
            title: p
            type: number
            format: pvalue
```

### Diagnostic Test Analysis

```yaml
---
name: diagnosticaccuracy
title: Diagnostic Test Accuracy Analysis
jrs: '1.1'

items:
    # 2x2 Contingency Table
    - name: contingencyTable
      title: Contingency Table
      type: Table
      rows: 2
      clearWith:
          - testResult
          - goldStandard
      columns:
          - name: test_result
            title: Test Result
            type: text
          - name: disease_pos
            title: Disease+
            type: integer
          - name: disease_neg
            title: Disease-
            type: integer
          - name: total
            title: Total
            type: integer
            
    # Diagnostic Accuracy Metrics
    - name: accuracyMetrics
      title: Diagnostic Accuracy Metrics
      type: Table
      rows: 0
      clearWith:
          - testResult
          - goldStandard
      columns:
          - name: metric
            title: Metric
            type: text
          - name: estimate
            title: Estimate
            type: number
            format: zto3
          - name: ci_lower
            title: Lower
            superTitle: 95% CI
            type: number
            format: zto3
          - name: ci_upper
            title: Upper
            superTitle: 95% CI
            type: number
            format: zto3
            
    # ROC Curve
    - name: rocCurve
      title: ROC Curve
      type: Image
      width: 600
      height: 500
      renderFun: .plotROC
      visible: (showROC)
      requiresData: true
```

### Cox Regression Analysis

```yaml
---
name: coxregression
title: Cox Proportional Hazards Regression
jrs: '1.1'

items:
    # Model Summary
    - name: modelSummary
      title: Model Summary
      type: Table
      rows: 1
      clearWith:
          - covariates
          - outcome
      columns:
          - name: n
            title: N
            type: integer
          - name: events
            title: Events
            type: integer
          - name: concordance
            title: Concordance
            type: number
            format: zto3
          - name: logLikelihood
            title: Log Likelihood
            type: number
            format: sf3
          - name: aic
            title: AIC
            type: number
            format: zto1
            
    # Coefficients Table
    - name: coefficientsTable
      title: Regression Coefficients
      type: Table
      rows: 0
      clearWith:
          - covariates
          - outcome
      columns:
          - name: term
            title: Term
            type: text
          - name: coef
            title: Coefficient
            type: number
            format: zto3
          - name: se
            title: SE
            type: number
            format: zto3
          - name: z
            title: z
            type: number
            format: zto2
          - name: pvalue
            title: p
            type: number
            format: pvalue
            
    # Hazard Ratios Table  
    - name: hazardRatios
      title: Hazard Ratios
      type: Table
      visible: (showHazardRatios)
      rows: 0
      clearWith:
          - covariates
          - outcome
      columns:
          - name: term
            title: Term
            type: text
          - name: hr
            title: Hazard Ratio
            type: number
            format: zto3
          - name: hr_lower
            title: Lower
            superTitle: 95% CI
            type: number
            format: zto3
          - name: hr_upper
            title: Upper
            superTitle: 95% CI
            type: number
            format: zto3
```

---

## 9. Best Practices and Design Guidelines

### Organization and Structure

#### 1. Logical Result Flow
Organize results from general to specific:

```yaml
items:
    # 1. Overview/Instructions (if needed)
    - name: instructions
      title: Analysis Guide
      type: Html
      visible: (showInstructions)
      
    # 2. Primary Results
    - name: mainResults
      title: Primary Results
      type: Table
      
    # 3. Supporting Statistics
    - name: supportingStats
      title: Supporting Statistics  
      type: Table
      visible: (showDetails)
      
    # 4. Visualizations
    - name: primaryPlot
      title: Results Visualization
      type: Image
      visible: (showPlots)
      
    # 5. Advanced/Optional Results
    - name: advancedResults
      title: Advanced Analysis
      type: Group
      visible: (performAdvanced)
```

#### 2. Clear Naming Conventions
Use descriptive, consistent names:

```yaml
# Good naming patterns
- name: baselineCharacteristics  # Descriptive purpose
- name: survivalAnalysisResults   # Clear content type
- name: diagnosticAccuracyMetrics # Specific analysis type

# Avoid generic names
- name: table1                    # Not descriptive
- name: results                   # Too generic
- name: output                    # Unclear purpose
```

#### 3. Comprehensive Documentation
Include helpful titles and descriptions:

```yaml
- name: complexAnalysisTable
  title: '`Multivariate Analysis - ${method}`'
  type: Table
  description: >
    Multivariate analysis results showing adjusted associations
    between predictors and outcomes, controlling for confounders.
  visible: (performMultivariate)
  clearWith:
      - predictors
      - outcome
      - adjustmentVariables
      - method
```

### User Experience Design

#### 4. Progressive Disclosure
Show basic results by default, advanced results conditionally:

```yaml
# Always shown - essential results
- name: primaryResults
  title: Primary Analysis Results
  type: Table
  
# Conditionally shown - detailed results  
- name: detailedResults
  title: Detailed Results
  type: Table
  visible: (showDetails)
  
# Expert-level results
- name: diagnosticResults
  title: Model Diagnostics
  type: Group
  visible: (performDiagnostics)
```

#### 5. Context-Sensitive Titles
Use dynamic titles that provide context:

```yaml
# Dynamic context
title: '`Cox Regression - ${outcome} over ${timeVar}`'
title: '`Survival Analysis: ${n} subjects, ${events} events`'
title: '`Diagnostic Accuracy - ${testName} vs ${goldStandard}`'

# Group-specific context
title: '`Results by ${groupingVariable}`'
title: '`Stratified Analysis - ${stratifyBy}`'
```

#### 6. Appropriate Precision
Choose formatting that matches clinical/scientific context:

```yaml
# P-values - use pvalue format
- name: pvalue
  title: p
  type: number
  format: pvalue         # Handles < 0.001 automatically

# Proportions - use appropriate decimals
- name: sensitivity
  title: Sensitivity
  type: number
  format: zto3          # 0.000 for precision

# Counts - use integers
- name: sample_size
  title: N
  type: integer         # No decimal places

# Clinical measurements - match precision
- name: survival_time
  title: Median Survival
  type: number
  format: zto1          # 0.0 months/years
```

### Technical Best Practices

#### 7. Robust Content Synchronization
Always use comprehensive `clearWith`:

```yaml
clearWith:
    # Include ALL options that affect this result
    - primaryVariable
    - adjustmentVariables  
    - analysisMethod
    - confidenceLevel
    - bootstrapOptions
    - displayOptions
```

#### 8. Conditional Logic Strategy
Use clear, readable conditions:

```yaml
# Simple boolean
visible: (showAdvanced)

# Value comparison  
visible: (method:multivariate)

# Multiple conditions
visible: (showPlots && hasGrouping)

# Complex logic
visible: (analysisType:survival && (showKM || showCox))
```

#### 9. Performance Considerations
Structure for efficient rendering:

```yaml
# Group expensive computations
- name: computationallyIntensive
  title: Advanced Analysis
  type: Group
  visible: (performAdvanced)    # Only compute when needed
  items:
      # Multiple related results that share computations
      - name: bootstrapResults
      - name: permutationTests
      - name: crossValidation
```

---

## 10. Integration with `.b.R` Implementation

### Accessing Result Elements in R Code

Your `.b.R` file interacts with result elements through the `self$results` object:

#### Table Population
```r
# In your .b.R file .run() function
populateTable <- function() {
    # Get the table object
    table <- self$results$coefficientsTable
    
    # Set data
    table$setRow(rowNo = 1, values = list(
        term = "Age",
        estimate = 0.045,
        se = 0.012,
        pvalue = 0.0003
    ))
}
```

#### Plot Generation
```r
# Render function referenced in renderFun
.plotSurvival <- function(image, ggtheme, theme, ...) {
    # Create plot
    plot <- ggplot(data) + 
        geom_step(aes(x = time, y = surv)) +
        theme_minimal()
    
    # Return the plot object
    return(plot)
}
```

#### HTML Content
```r
# Set HTML content
self$results$summaryText$setContent(
    "<h3>Analysis Summary</h3>
     <p>This analysis included <strong>500</strong> subjects...</p>"
)
```

### Error Handling and Validation

#### Conditional Result Population
```r
.run <- function() {
    # Only populate if conditions are met
    if (self$options$showTable && !is.null(data)) {
        self$populateTable()
    }
    
    # Handle missing data gracefully
    if (nrow(data) == 0) {
        self$results$errorMessage$setVisible(TRUE)
        return()
    }
}
```

---

## 11. Complete Examples

### Example 1: Simple T-Test Results

```yaml
---
name: simplettest
title: Two-Sample t-Test Results
jrs: '1.1'

items:
    # Test Results Table
    - name: ttestResults
      title: t-Test Results
      type: Table
      rows: 1
      clearWith:
          - outcome
          - grouping
          - equalVariances
      columns:
          - name: test_type
            title: Test Type
            type: text
          - name: statistic
            title: t
            type: number
            format: zto3
          - name: df
            title: df
            type: number
            format: zto1
          - name: pvalue
            title: p
            type: number
            format: pvalue
          - name: mean_diff
            title: Mean Difference
            type: number
            format: zto3
          - name: ci_lower
            title: Lower
            superTitle: 95% CI
            type: number
            format: zto3
          - name: ci_upper
            title: Upper
            superTitle: 95% CI
            type: number
            format: zto3
            
    # Descriptive Statistics
    - name: descriptives
      title: Descriptive Statistics
      type: Table
      visible: (showDescriptives)
      rows: 0
      clearWith:
          - outcome
          - grouping
      columns:
          - name: group
            title: Group
            type: text
          - name: n
            title: N
            type: integer
          - name: mean
            title: Mean
            type: number
            format: zto2
          - name: sd
            title: SD
            type: number
            format: zto2
          - name: se
            title: SE
            type: number
            format: zto2

refs:
    - ClinicoPathJamoviModule
...
```

### Example 2: Advanced Survival Analysis

```yaml
---
name: comprehensivesurvival
title: '`Comprehensive Survival Analysis - ${outcome}`'
jrs: '1.1'

items:
    # Analysis Overview
    - name: analysisOverview
      title: Analysis Overview
      type: Html
      visible: (showOverview)
      clearWith:
          - outcome
          - timeVar
          - eventVar
          
    # Study Population Summary
    - name: populationSummary
      title: Study Population
      type: Table
      rows: 1
      clearWith:
          - outcome
          - timeVar
          - eventVar
          - grouping
      columns:
          - name: total_n
            title: Total N
            type: integer
          - name: events
            title: Events
            type: integer
          - name: censored
            title: Censored
            type: integer
          - name: median_followup
            title: Median Follow-up
            type: number
            format: zto1
          - name: max_followup
            title: Max Follow-up
            type: number
            format: zto1
            
    # Kaplan-Meier Results
    - name: kmResults
      title: Kaplan-Meier Analysis
      type: Group
      visible: (performKM)
      items:
          # Survival Summary by Group
          - name: kmSummary
            title: '`Survival Summary by ${grouping}`'
            type: Table
            rows: 0
            clearWith:
                - outcome
                - grouping
                - timeVar
                - eventVar
            columns:
                - name: group
                  title: '`${grouping}`'
                  type: text
                - name: n
                  title: N
                  type: integer
                - name: events
                  title: Events
                  type: integer
                - name: median
                  title: Median
                  type: number
                  format: zto1
                - name: ci_lower
                  title: Lower
                  superTitle: 95% CI
                  type: number
                  format: zto1
                - name: ci_upper
                  title: Upper
                  superTitle: 95% CI
                  type: number
                  format: zto1
                - name: rmst
                  title: RMST
                  type: number
                  format: zto1
                  
          # Log-rank Test
          - name: logRankTest
            title: Log-rank Test
            type: Table
            visible: (performLogRank)
            rows: 1
            clearWith:
                - grouping
                - outcome
            columns:
                - name: statistic
                  title: χ²
                  type: number
                  format: zto2
                - name: df
                  title: df
                  type: integer
                - name: pvalue
                  title: p
                  type: number
                  format: pvalue
                  
          # Survival Plot
          - name: kmPlot
            title: '`Kaplan-Meier Curves - ${grouping}`'
            type: Image
            width: 700
            height: 500
            renderFun: .plotKM
            visible: (showPlot)
            requiresData: true
            clearWith:
                - outcome
                - grouping
                - timeVar
                - eventVar
                
    # Cox Regression Results
    - name: coxResults
      title: Cox Proportional Hazards
      type: Group
      visible: (performCox)
      items:
          # Model Summary
          - name: coxModelSummary
            title: Model Summary
            type: Table
            rows: 1
            clearWith:
                - covariates
                - outcome
            columns:
                - name: n
                  title: N
                  type: integer
                - name: events
                  title: Events
                  type: integer
                - name: concordance
                  title: Concordance
                  type: number
                  format: zto3
                - name: rsquared
                  title: R²
                  type: number
                  format: zto3
                - name: aic
                  title: AIC
                  type: number
                  format: zto1
                  
          # Coefficients
          - name: coxCoefficients
            title: Regression Coefficients
            type: Table
            rows: 0
            clearWith:
                - covariates
                - outcome
            columns:
                - name: term
                  title: Variable
                  type: text
                - name: coef
                  title: β
                  type: number
                  format: zto3
                - name: se
                  title: SE(β)
                  type: number
                  format: zto3
                - name: z
                  title: z
                  type: number
                  format: zto2
                - name: pvalue
                  title: p
                  type: number
                  format: pvalue
                  
          # Hazard Ratios
          - name: hazardRatios
            title: Hazard Ratios
            type: Table
            rows: 0
            clearWith:
                - covariates
                - outcome
                - confidenceLevel
            columns:
                - name: term
                  title: Variable
                  type: text
                - name: hr
                  title: Hazard Ratio
                  type: number
                  format: zto3
                - name: hr_lower
                  title: Lower
                  superTitle: '`${confidenceLevel*100}% CI`'
                  type: number
                  format: zto3
                - name: hr_upper
                  title: Upper
                  superTitle: '`${confidenceLevel*100}% CI`'
                  type: number
                  format: zto3
                  
    # Model Diagnostics
    - name: diagnostics
      title: Model Diagnostics
      type: Group
      visible: (performDiagnostics)
      items:
          # Proportional Hazards Test
          - name: phTest
            title: Proportional Hazards Test
            type: Table
            rows: 0
            clearWith:
                - covariates
                - outcome
            columns:
                - name: variable
                  title: Variable
                  type: text
                - name: rho
                  title: ρ
                  type: number
                  format: zto3
                - name: chisq
                  title: χ²
                  type: number
                  format: zto2
                - name: pvalue
                  title: p
                  type: number
                  format: pvalue
                  
          # Diagnostic Plots
          - name: diagnosticPlots
            title: Diagnostic Plots
            type: Image
            width: 800
            height: 600
            renderFun: .plotDiagnostics
            visible: (showDiagnosticPlots)
            requiresData: true

refs:
    - survival
    - survminer
    - ClinicoPathJamoviModule
    
...
```

### Example 3: Clinical Decision Analysis

```yaml
---
name: clinicaldecision
title: Clinical Decision Analysis
jrs: '1.1'

items:
    # Decision Tree Visualization
    - name: treeVisualization
      title: Decision Tree
      type: Image
      width: 800
      height: 600
      renderFun: .plotDecisionTree
      clearWith:
          - decisions
          - probabilities
          - costs
          - utilities
          
    # Expected Value Analysis
    - name: expectedValues
      title: Expected Values
      type: Table
      rows: 0
      clearWith:
          - decisions
          - probabilities
          - costs
          - utilities
      columns:
          - name: strategy
            title: Strategy
            type: text
          - name: expected_cost
            title: Expected Cost
            type: number
            format: currency
          - name: expected_utility
            title: Expected Utility
            type: number
            format: zto3
          - name: net_benefit
            title: Net Benefit
            type: number
            format: currency
          - name: rank
            title: Rank
            type: integer
            
    # Cost-Effectiveness Analysis
    - name: costEffectiveness
      title: Cost-Effectiveness Analysis
      type: Table
      visible: (performCEA)
      rows: 0
      clearWith:
          - decisions
          - costs
          - utilities
          - willingnessToPay
      columns:
          - name: strategy
            title: Strategy
            type: text
          - name: incremental_cost
            title: Incremental Cost
            type: number
            format: currency
          - name: incremental_utility
            title: Incremental Utility
            type: number
            format: zto3
          - name: icer
            title: ICER
            type: number
            format: currency
          - name: dominated
            title: Dominated
            type: text
            
    # Sensitivity Analysis Results
    - name: sensitivityResults
      title: Sensitivity Analysis
      type: Group
      visible: (performSensitivity)
      items:
          - name: tornadoDiagram
            title: Tornado Diagram
            type: Image
            width: 600
            height: 400
            renderFun: .plotTornado
            
          - name: sensitivityTable
            title: One-Way Sensitivity
            type: Table
            rows: 0
            columns:
                - name: parameter
                  title: Parameter
                  type: text
                - name: base_value
                  title: Base Value
                  type: number
                  format: zto3
                - name: low_value
                  title: Low Value
                  type: number
                  format: zto3
                - name: high_value
                  title: High Value
                  type: number
                  format: zto3
                - name: range
                  title: NMB Range
                  type: number
                  format: currency

refs:
    - DecisionAnalysis
    - ClinicoPathJamoviModule
    
...
```

---

## Conclusion

This comprehensive guide provides everything needed to create professional, well-structured `.r.yaml` files for jamovi development. Key principles:

1. **Clear Structure**: Organize results logically from general to specific
2. **Dynamic Content**: Use variable substitution for context-sensitive titles
3. **Robust Synchronization**: Always use `clearWith` to maintain result integrity
4. **User-Focused Design**: Show essential results by default, advanced results conditionally
5. **Clinical Relevance**: Format numbers appropriately for clinical/scientific context

### Additional Resources

- [Official jamovi Results Definition Documentation](https://dev.jamovi.org/api_results-definition.html)
- [jamovi Results Elements Documentation](https://dev.jamovi.org/api_results-elements.html)
- [ClinicoPath Module Examples](https://github.com/sbalci/ClinicoPathJamoviModule/tree/master/jamovi)

### Next Steps

After mastering `.r.yaml` files, explore:
- `.u.yaml` files for interface layout design
- `.b.R` files for R implementation that populates results
- Integration patterns between all jamovi module components

This guide establishes the foundation for creating publication-quality analysis results that serve researchers and clinicians effectively.