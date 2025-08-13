# The Complete Guide to Writing `.a.yaml` Files for jamovi Development

This is the definitive, comprehensive guide to writing `.a.yaml` files for developing jamovi modules. These files are the backbone of a jamovi analysis, defining the user interface parameters, data requirements, and analysis options for your R functions.

## Table of Contents

1. [Introduction: The Role of `.a.yaml`](#1-introduction-the-role-of-ayaml)
2. [File Structure and Basic Components](#2-file-structure-and-basic-components)
3. [Essential Top-Level Properties](#3-essential-top-level-properties)
4. [The `options` Section: Complete Reference](#4-the-options-section-complete-reference)
5. [Option Types: Detailed Coverage](#5-option-types-detailed-coverage)
6. [Advanced Features and Patterns](#6-advanced-features-and-patterns)
7. [Clinical and Scientific Use Cases](#7-clinical-and-scientific-use-cases)
8. [Best Practices and Design Patterns](#8-best-practices-and-design-patterns)
9. [Debugging and Troubleshooting](#9-debugging-and-troubleshooting)
10. [Complete Examples](#10-complete-examples)

---

## 1. Introduction: The Role of `.a.yaml`

The `.a.yaml` file is a YAML configuration file that specifies the **analysis definition** for a jamovi module. It serves as the bridge between the user interface in jamovi and your R implementation, defining:

- **User Interface Elements**: All the controls, dropdowns, checkboxes, and input fields users will interact with
- **Data Requirements**: What types of variables can be selected and how they're validated
- **Parameter Specifications**: Default values, ranges, and constraints for analysis options
- **Menu Organization**: Where your analysis appears in jamovi's menu system

### The jamovi Module Architecture

Each analysis in a jamovi module consists of four coordinated files:

- **`.a.yaml`** - Analysis definition (parameters and UI elements)
- **`.r.yaml`** - Results definition (output structure)
- **`.u.yaml`** - User interface layout (visual arrangement)
- **`.b.R`** - Backend implementation (R code)

The `.a.yaml` file is compiled into a `.h.R` header file that provides the base class for your analysis.

---

## 2. File Structure and Basic Components

Every `.a.yaml` file follows a consistent structure:

```yaml
---
name: analysisname
title: Human Readable Analysis Title
menuGroup: MainMenuGroup
menuSubgroup: SubMenuGroup  
menuSubtitle: Optional Subtitle
version: '0.0.3'
jas: '1.2'

description:
    main: |
        Description for users in jamovi
    R:
        dontrun: true
        usage: |
            # R usage examples

options:
    - name: data
      type: Data
    
    - name: option1
      title: Option Title
      type: OptionType
      # ... option-specific properties
    
    # ... more options
...
```

---

## 3. Essential Top-Level Properties

### Core Identification Properties

#### `name` (Required)
- **Type**: String (single word, no spaces)
- **Purpose**: Unique identifier for the analysis within the module
- **Rules**: Must match the corresponding `.r.yaml`, `.u.yaml`, and `.b.R` filenames
- **Example**: `name: advancedsurvival`

#### `title` (Required)
- **Type**: String
- **Purpose**: Human-readable title displayed in jamovi menus and results
- **Guidelines**: Should be descriptive but concise (≤50 characters)
- **Example**: `title: Advanced Survival Analysis with Cox Regression`

### Menu Organization Properties

#### `menuGroup` (Required)
- **Type**: String
- **Purpose**: Main menu category where analysis appears
- **Common Values**: 
  - `SurvivalD` - Survival analysis
  - `ExplorationD` - Exploratory data analysis  
  - `RegressionD` - Regression methods
  - `DiagnosticsD` - Diagnostic tests
  - `meddecideD` - Medical decision analysis
- **Example**: `menuGroup: SurvivalD`

#### `menuSubgroup` (Optional)
- **Type**: String
- **Purpose**: Subcategory within the main menu group
- **Example**: `menuSubgroup: Advanced Methods`

#### `menuSubtitle` (Optional) 
- **Type**: String
- **Purpose**: Additional descriptive text in menu
- **Example**: `menuSubtitle: Cox Regression with Extensions`

### Version and Specification Properties

#### `version` (Required)
- **Type**: String (quoted)
- **Purpose**: Analysis version for tracking changes
- **Format**: Semantic versioning (major.minor.patch)
- **Example**: `version: '0.0.3'`

#### `jas` (Required)
- **Type**: String (quoted)
- **Purpose**: jamovi Analysis Specification version
- **Current Standard**: `'1.2'`
- **Example**: `jas: '1.2'`

### Documentation Properties

#### `description` (Recommended)
- **Type**: Object with `main` and `R` properties
- **Purpose**: Provides detailed documentation for users and developers

```yaml
description:
    main: |
        Comprehensive description of what this analysis does,
        its intended use cases, and key features. This text
        appears in jamovi help and documentation.
    R:
        dontrun: true
        usage: |
            # Example R code showing how to use this analysis
            result <- myanalysis(
                data = mydata,
                outcome = "survival_time",
                event = "event_occurred"
            )
```

---

## 4. The `options` Section: Complete Reference

The `options` section is the heart of the `.a.yaml` file. It defines every user interface element and analysis parameter.

### Basic Structure

```yaml
options:
    - name: data          # Data option (always required)
      type: Data
      
    - name: parameter1    # First analysis parameter
      title: User Label
      type: OptionType
      # ... type-specific properties
      
    - name: parameter2    # Second analysis parameter  
      title: Another Label
      type: AnotherType
      # ... more properties
```

### Universal Option Properties

These properties can be used with any option type:

#### `name` (Required)
- **Type**: String
- **Purpose**: Internal identifier used in R code to access the option value
- **Rules**: Must be valid R variable name (no spaces, starts with letter)
- **Example**: `name: survivalTime`

#### `title` (Recommended)
- **Type**: String  
- **Purpose**: Label displayed to users in the jamovi interface
- **Guidelines**: Should be clear and descriptive
- **Example**: `title: Survival Time Variable`

#### `description` (Optional)
- **Type**: String or Object
- **Purpose**: Provides help text and documentation

```yaml
description:
    ui: >
        Help text shown in jamovi interface tooltips.
    R: >
        Technical description for R developers.
    jamovi: >
        Additional jamovi-specific guidance.
```

#### `default` (Optional)
- **Type**: Varies by option type
- **Purpose**: Default value when analysis is first loaded
- **Example**: `default: true` (for Bool), `default: 0.05` (for Number)

---

## 5. Option Types: Detailed Coverage

### Core Data Types

#### `Data`
The dataset option - required for every analysis.

```yaml
- name: data
  type: Data
  description:
      R: The data as a data frame.
```

**Properties**: 
- No additional properties needed
- Always required as first option
- Provides access to the dataset in R code

#### `Variable`
Single variable selection from the dataset.

```yaml
- name: outcomeVar
  title: Outcome Variable
  type: Variable
  suggested: [continuous]
  permitted: [numeric]
  default: NULL
  description:
      ui: Select the primary outcome variable for analysis.
      R: Variable containing outcome measurements.
```

**Properties**:
- `suggested: [continuous, ordinal, nominal]` - Variable types to highlight
- `permitted: [numeric, factor]` - Variable types allowed
- `default: NULL` - Use NULL for optional variables

#### `Variables`
Multiple variable selection from the dataset.

```yaml
- name: covariates
  title: Covariate Variables
  type: Variables
  suggested: [continuous, ordinal, nominal]
  permitted: [numeric, factor]
  description:
      ui: Select variables to include as covariates.
      R: Vector of covariate variable names.
```

**Properties**:
- Same `suggested` and `permitted` as Variable
- Returns vector of variable names in R
- Users can select multiple variables

#### `Level`
Specific level selection from a factor variable.

```yaml
- name: eventLevel
  title: Event Level
  type: Level
  variable: (event)  # References another Variable option
  description:
      ui: Select which level indicates event occurrence.
      R: Character string of the selected level.
```

**Properties**:
- `variable: (variableName)` - Must reference another Variable option
- Dynamically populated based on selected variable's levels

### Numeric Input Types

#### `Number`
Numeric input with decimal precision.

```yaml
- name: significanceLevel
  title: Significance Level
  type: Number
  default: 0.05
  min: 0.001
  max: 0.10
  description:
      ui: Alpha level for statistical tests (0.001 to 0.10).
      R: Numeric value for significance threshold.
```

**Properties**:
- `min: value` - Minimum allowed value
- `max: value` - Maximum allowed value  
- `default: value` - Default numeric value

#### `Integer`
Whole number input only.

```yaml
- name: bootstrapReps
  title: Bootstrap Repetitions
  type: Integer
  default: 1000
  min: 100
  max: 10000
  description:
      ui: Number of bootstrap replications (100-10000).
      R: Integer number of bootstrap samples.
```

**Properties**:
- Same as Number but restricts to integers
- Useful for counts, repetitions, sample sizes

### Text Input Types

#### `String`
Short text input field.

```yaml
- name: analysisTitle
  title: Analysis Title
  type: String
  default: ""
  description:
      ui: Custom title for analysis output.
      R: Character string for plot/table titles.
```

**Properties**:
- `default: ""` - Usually empty string
- Best for short labels, titles, custom text

### Boolean Types

#### `Bool`
Checkbox for true/false options.

```yaml
- name: includeConfidenceIntervals
  title: Include 95% Confidence Intervals
  type: Bool
  default: true
  description:
      ui: Calculate and display confidence intervals.
      R: Logical value controlling CI calculation.
```

**Properties**:
- `default: true` or `default: false`
- Creates checkbox in interface
- Returns logical value in R

### Selection Types

#### `List`
Single selection from predefined options.

```yaml
- name: survivalMethod
  title: Survival Analysis Method
  type: List
  options:
    - name: kaplan_meier
      title: Kaplan-Meier Estimator
    - name: cox_ph
      title: Cox Proportional Hazards
    - name: parametric
      title: Parametric Survival Models
    - name: competing_risks
      title: Competing Risks Analysis
  default: kaplan_meier
  description:
      ui: Choose the primary survival analysis method.
      R: Character string indicating selected method.
```

**Properties**:
- `options:` - List of available choices
  - `name:` - Internal value passed to R
  - `title:` - Display text shown to user
- `default:` - Must match one of the option names

#### `NMXList`
Multiple selection list (Non-Mutual eXclusive).

```yaml
- name: summaryStatistics
  title: Summary Statistics to Include
  type: NMXList
  options:
    - name: mean_sd
      title: Mean (SD)
    - name: median_iqr  
      title: Median (IQR)
    - name: min_max
      title: Min, Max
    - name: n_missing
      title: N, Missing
  default: [mean_sd, median_iqr]
  description:
      ui: Select which summary statistics to calculate.
      R: Character vector of selected statistic types.
```

**Properties**:
- Same structure as List but allows multiple selections
- `default:` - Array of selected option names
- Returns character vector in R

### Output Types

#### `Output`
Creates new columns in the dataset.

```yaml
- name: calculatedRisk
  title: Add Risk Score to Data
  type: Output
  description:
      ui: Create new variable with calculated risk scores.
      R: Column name for output variable.
```

**Properties**:
- No additional properties needed
- User specifies name for new variable
- Analysis can write results to this column

---

## 6. Advanced Features and Patterns

### Conditional Logic and Dependencies

Options can be made conditional based on other option values (implemented in `.u.yaml`):

```yaml
# Base option
- name: performBootstrap
  title: Bootstrap Validation
  type: Bool
  default: false

# Dependent option (visibility controlled in .u.yaml)  
- name: bootstrapReps
  title: Bootstrap Repetitions
  type: Integer
  default: 1000
  min: 100
  max: 5000
```

### Complex Data Structures

#### Hierarchical Options
For complex analyses with multiple levels:

```yaml
- name: analysisType
  title: Analysis Complexity
  type: List
  options:
    - name: basic
      title: Basic Analysis
    - name: standard
      title: Standard Analysis
    - name: comprehensive
      title: Comprehensive Analysis
  default: standard

- name: advancedOptions
  title: Advanced Statistical Methods
  type: NMXList
  options:
    - name: bootstrap_validation
      title: Bootstrap Validation
    - name: cross_validation
      title: Cross-Validation  
    - name: sensitivity_analysis
      title: Sensitivity Analysis
    - name: multiple_imputation
      title: Multiple Imputation
  default: []
```

### Professional Configuration Patterns

#### Time-Based Options
```yaml
- name: timePoints
  title: Analysis Time Points (months)
  type: String
  default: "12, 24, 36, 60"
  description:
      ui: Comma-separated time points for survival analysis.
      R: Parsed as numeric vector of time points.

- name: followUpPeriod
  title: Maximum Follow-up (months)
  type: Number
  default: 120
  min: 12
  max: 240
```

#### Clinical Decision Parameters
```yaml
- name: riskThresholds
  title: Clinical Risk Thresholds
  type: String
  default: "0.1, 0.25, 0.5, 0.75"
  description:
      ui: Comma-separated risk thresholds for clinical decisions.
      
- name: costPerQALY
  title: Cost per QALY Threshold
  type: Number
  default: 50000
  min: 10000
  max: 200000
```

#### Advanced Statistical Controls
```yaml
- name: multipleTestingCorrection
  title: Multiple Testing Correction
  type: List
  options:
    - name: none
      title: No Correction
    - name: bonferroni
      title: Bonferroni
    - name: holm
      title: Holm-Bonferroni
    - name: benjamini_hochberg
      title: Benjamini-Hochberg (FDR)
    - name: benjamini_yekutieli
      title: Benjamini-Yekutieli
  default: benjamini_hochberg
```

### Validation and Constraints

#### Range Validation
```yaml
- name: confidenceLevel
  title: Confidence Level
  type: Number
  default: 0.95
  min: 0.80
  max: 0.99
  description:
      ui: Confidence level for intervals (80% to 99%).
```

#### Multi-Variable Dependencies
```yaml
- name: primaryEndpoint
  title: Primary Endpoint
  type: Variable
  suggested: [continuous]
  permitted: [numeric]

- name: eventIndicator
  title: Event Indicator  
  type: Variable
  suggested: [ordinal, nominal]
  permitted: [factor, numeric]

- name: eventLevel
  title: Event Level
  type: Level
  variable: (eventIndicator)
```

---

## 7. Clinical and Scientific Use Cases

### Survival Analysis Configuration

```yaml
---
name: advancedsurvival
title: Advanced Survival Analysis
menuGroup: SurvivalD
menuSubgroup: Extended Methods
version: '0.0.3'
jas: '1.2'

options:
    - name: data
      type: Data
      
    # Core Variables
    - name: timeVar
      title: Time to Event
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      description:
          ui: Select time-to-event or censoring variable.
          
    - name: eventVar
      title: Event Indicator
      type: Variable  
      suggested: [ordinal, nominal]
      permitted: [factor, numeric]
      
    - name: eventLevel
      title: Event Level
      type: Level
      variable: (eventVar)
      
    # Analysis Methods
    - name: survivalMethods
      title: Analysis Methods
      type: NMXList
      options:
        - name: kaplan_meier
          title: Kaplan-Meier Curves
        - name: cox_regression
          title: Cox Proportional Hazards
        - name: competing_risks
          title: Competing Risks Analysis
        - name: frailty_models
          title: Frailty Models
      default: [kaplan_meier, cox_regression]
      
    # Advanced Options
    - name: performBootstrap
      title: Bootstrap Validation
      type: Bool
      default: false
      
    - name: bootstrapReps
      title: Bootstrap Repetitions
      type: Integer
      default: 1000
      min: 100
      max: 5000
```

### Clinical Trial Design

```yaml
---
name: clinicaltrialpoweranalysis
title: Clinical Trial Power Analysis
menuGroup: SampleSizeD
menuSubgroup: Trial Design
version: '0.0.3'
jas: '1.2'

options:
    - name: data
      type: Data
      
    - name: studyDesign
      title: Study Design
      type: List
      options:
        - name: two_sample_ttest
          title: Two-Sample t-test
        - name: chi_square
          title: Chi-Square Test
        - name: survival_logrank
          title: Survival (Log-Rank)
        - name: correlation
          title: Correlation Analysis
      default: two_sample_ttest
      
    - name: powerLevel
      title: Desired Power
      type: Number
      default: 0.80
      min: 0.50
      max: 0.99
      
    - name: significanceLevel
      title: Type I Error Rate (α)
      type: Number
      default: 0.05
      min: 0.01
      max: 0.10
      
    - name: effectSize
      title: Expected Effect Size
      type: Number
      default: 0.5
      min: 0.1
      max: 3.0
      
    - name: allocationRatio
      title: Allocation Ratio (Control:Treatment)
      type: List
      options:
        - name: "1:1"
          title: "1:1 (Equal)"
        - name: "2:1"
          title: "2:1"
        - name: "3:1"
          title: "3:1"
      default: "1:1"
```

### Diagnostic Test Analysis

```yaml
---
name: diagnostictestanalysis
title: Diagnostic Test Accuracy Analysis
menuGroup: DiagnosticsD  
menuSubgroup: Test Performance
version: '0.0.3'
jas: '1.2'

options:
    - name: data
      type: Data
      
    - name: testResult
      title: Test Result
      type: Variable
      suggested: [ordinal, nominal]
      permitted: [factor, numeric]
      
    - name: goldStandard
      title: Gold Standard (Reference)
      type: Variable
      suggested: [ordinal, nominal]
      permitted: [factor]
      
    - name: positiveLevel
      title: Positive Result Level
      type: Level
      variable: (testResult)
      
    - name: diseaseLevel
      title: Disease Present Level  
      type: Level
      variable: (goldStandard)
      
    - name: analysisMetrics
      title: Diagnostic Metrics
      type: NMXList
      options:
        - name: sensitivity_specificity
          title: Sensitivity & Specificity
        - name: predictive_values
          title: Predictive Values (PPV/NPV)
        - name: likelihood_ratios
          title: Likelihood Ratios
        - name: diagnostic_odds_ratio
          title: Diagnostic Odds Ratio
        - name: auc_roc
          title: AUC-ROC Analysis
      default: [sensitivity_specificity, predictive_values]
      
    - name: confidenceLevel
      title: Confidence Level
      type: Number
      default: 0.95
      min: 0.90
      max: 0.99
```

---

## 8. Best Practices and Design Patterns

### Organization and Structure

#### 1. Logical Grouping
Group related options together and order them logically:

```yaml
options:
    # Always start with data
    - name: data
      type: Data
      
    # Core variables next
    - name: outcome
      title: Outcome Variable
      type: Variable
      
    - name: predictors
      title: Predictor Variables
      type: Variables
      
    # Analysis options
    - name: method
      title: Analysis Method
      type: List
      
    # Advanced options last
    - name: performBootstrap
      title: Bootstrap Validation
      type: Bool
```

#### 2. Clear Naming Conventions
- Use descriptive, consistent names
- Follow camelCase convention
- Make purpose obvious from the name

```yaml
# Good naming
- name: survivalTime
- name: censoringIndicator  
- name: includeConfidenceIntervals
- name: bootstrapRepetitions

# Poor naming
- name: var1
- name: opt
- name: flag
- name: n
```

#### 3. Comprehensive Descriptions
Provide helpful descriptions for complex options:

```yaml
- name: multipleTestingCorrection
  title: Multiple Testing Correction
  type: List
  options:
    - name: benjamini_hochberg
      title: Benjamini-Hochberg (FDR)
  default: benjamini_hochberg
  description:
      ui: >
        Controls false discovery rate when testing multiple hypotheses.
        Benjamini-Hochberg is recommended for most applications.
      R: >
        Method for p-value adjustment. Returns character string
        compatible with p.adjust() function.
```

### User Experience Design

#### 4. Sensible Defaults
Choose defaults that work for most common use cases:

```yaml
# Statistical defaults
- name: confidenceLevel
  type: Number
  default: 0.95  # Standard 95% CI

- name: significanceLevel
  type: Number
  default: 0.05  # Standard α = 0.05

# Analysis defaults
- name: includeConfidenceIntervals
  type: Bool
  default: true  # Usually want CIs

- name: showSummaryTable
  type: Bool  
  default: true  # Always show key results
```

#### 5. Appropriate Constraints
Set reasonable min/max values:

```yaml
# Reasonable bounds
- name: bootstrapReps
  type: Integer
  default: 1000
  min: 100     # Minimum for stability
  max: 10000   # Maximum for reasonable computation time

- name: confidenceLevel
  type: Number
  default: 0.95
  min: 0.80    # Lower bound for practical use
  max: 0.99    # Upper bound for practical use
```

#### 6. Progressive Disclosure
Structure options from basic to advanced:

```yaml
# Basic options first
- name: outcome
  title: Outcome Variable
  type: Variable
  
- name: method
  title: Analysis Method
  type: List
  
# Advanced options later (can be hidden in UI)
- name: performSensitivityAnalysis
  title: Sensitivity Analysis
  type: Bool
  default: false
  
- name: customBootstrapMethod
  title: Custom Bootstrap Method
  type: List
```

### Technical Best Practices

#### 7. Validation Strategy
Include options that validate analysis assumptions:

```yaml
- name: checkAssumptions
  title: Check Model Assumptions
  type: Bool
  default: true
  
- name: residualAnalysis
  title: Residual Analysis
  type: Bool
  default: false
  
- name: goodnessOfFit
  title: Goodness-of-Fit Tests
  type: Bool
  default: false
```

#### 8. Flexible Configuration
Allow customization while maintaining simplicity:

```yaml
- name: analysisComplexity
  title: Analysis Level
  type: List
  options:
    - name: basic
      title: Basic Analysis
    - name: standard  
      title: Standard Analysis
    - name: comprehensive
      title: Comprehensive Analysis
    - name: custom
      title: Custom Configuration
  default: standard
```

#### 9. Clinical Relevance
Include clinically meaningful parameters:

```yaml
- name: clinicalSignificanceThreshold
  title: Minimum Clinically Important Difference
  type: Number
  default: 0.5
  description:
      ui: >
        Smallest difference considered clinically meaningful.
        Used for effect size interpretation and power analysis.
```

---

## 9. Debugging and Troubleshooting

### Common Issues and Solutions

#### Issue 1: Analysis Not Appearing in Menu
**Symptoms**: Analysis missing from jamovi interface

**Causes & Solutions**:
```yaml
# Problem: Missing required properties
name: myanalysis    # ✓ Required
title: My Analysis  # ✓ Required  
menuGroup: SurvivalD # ✓ Required
version: '0.0.3'    # ✓ Required
jas: '1.2'          # ✓ Required

# Problem: Invalid characters in name
name: my-analysis   # ✗ Hyphens not allowed
name: myanalysis    # ✓ Correct

# Problem: Inconsistent filenames
# Files must all use same name:
# myanalysis.a.yaml ✓
# myanalysis.r.yaml ✓
# myanalysis.u.yaml ✓
# myanalysis.b.R    ✓
```

#### Issue 2: Options Not Working
**Symptoms**: Options don't appear or behave incorrectly

**Debugging Checklist**:
```yaml
# Check required properties
- name: option1      # ✓ Required for all options
  type: List         # ✓ Required for all options
  title: My Option   # ✓ Recommended for UI

# Check option-specific requirements  
- name: myList
  type: List
  options:           # ✓ Required for List type
    - name: value1   # ✓ Required for each option
      title: Label1  # ✓ Required for each option
  default: value1    # ✓ Must match an option name

# Check variable references
- name: eventLevel
  type: Level
  variable: (eventVar)  # ✓ Must reference another Variable option
```

#### Issue 3: Default Values Not Working
**Common Problems**:
```yaml
# Problem: Wrong data type
- name: significance
  type: Number
  default: "0.05"    # ✗ String instead of number
  default: 0.05      # ✓ Correct

# Problem: Invalid default for List
- name: method
  type: List
  options:
    - name: option_a
      title: Option A
  default: option_b  # ✗ Not in options list
  default: option_a  # ✓ Correct

# Problem: Invalid default for NMXList
- name: statistics
  type: NMXList
  options:
    - name: mean
      title: Mean
    - name: median
      title: Median
  default: mean        # ✗ Should be array
  default: [mean]      # ✓ Correct
```

### Validation Tools

#### YAML Syntax Validation
Use online YAML validators or:
```bash
# Command line validation
python -c "import yaml; yaml.safe_load(open('analysis.a.yaml'))"
```

#### jamovi Module Testing
```r
# In R, test module loading
devtools::install()
library(YourModule)

# Check if analysis loads
jmv::YourModule::youranalysis(data = data.frame())
```

---

## 10. Complete Examples

### Example 1: Simple Analysis
Basic two-sample t-test with essential options:

```yaml
---
name: simplettest
title: Two-Sample t-Test
menuGroup: T-Tests
version: '0.0.1'
jas: '1.2'

description:
    main: |
        Performs two-sample t-test comparing means between groups
        with options for equal or unequal variances.

options:
    - name: data
      type: Data

    - name: outcome
      title: Dependent Variable
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      description:
          ui: Select the continuous outcome variable to compare.

    - name: grouping
      title: Grouping Variable
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      description:
          ui: Select the grouping variable (should have exactly 2 levels).

    - name: equalVariances
      title: Assume Equal Variances
      type: Bool
      default: true
      description:
          ui: Check if group variances can be assumed equal (Student's t-test vs Welch's t-test).

    - name: confidenceLevel
      title: Confidence Level
      type: Number
      default: 0.95
      min: 0.80
      max: 0.99

    - name: alternative
      title: Alternative Hypothesis
      type: List
      options:
        - name: two.sided
          title: Two-sided (≠)
        - name: greater
          title: Greater (>)
        - name: less
          title: Less (<)
      default: two.sided
...
```

### Example 2: Intermediate Analysis
Survival analysis with multiple options:

```yaml
---
name: kaplanmeier
title: Kaplan-Meier Survival Analysis
menuGroup: SurvivalD
menuSubgroup: Non-Parametric
menuSubtitle: Survival Curves with Log-Rank Test
version: '0.0.3'
jas: '1.2'

description:
    main: |
        Non-parametric survival analysis using Kaplan-Meier estimation
        with optional group comparisons using log-rank test.
    R:
        dontrun: true
        usage: |
            kaplanmeier(
                data = survival_data,
                time = "months_to_event", 
                event = "death_occurred",
                grouping = "treatment_group",
                alpha = 0.05
            )

options:
    - name: data
      type: Data

    # Core Variables
    - name: time
      title: Survival Time
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      description:
          ui: Time to event or censoring (must be positive).
          R: Numeric variable with survival times.

    - name: event
      title: Event Indicator
      type: Variable
      suggested: [ordinal, nominal] 
      permitted: [factor, numeric]
      description:
          ui: Variable indicating whether event occurred (1/TRUE) or was censored (0/FALSE).

    - name: eventLevel
      title: Event Level
      type: Level
      variable: (event)
      description:
          ui: Level that indicates event occurrence.

    - name: grouping
      title: Grouping Variable
      type: Variable
      suggested: [nominal, ordinal]
      permitted: [factor]
      default: NULL
      description:
          ui: Optional grouping variable for comparing survival curves.

    # Analysis Options
    - name: confidenceLevel
      title: Confidence Level
      type: Number
      default: 0.95
      min: 0.80
      max: 0.99
      description:
          ui: Confidence level for survival curve confidence bands.

    - name: performLogRank
      title: Log-Rank Test
      type: Bool
      default: true
      description:
          ui: Perform log-rank test when grouping variable is specified.

    - name: riskTable
      title: Show Risk Table
      type: Bool
      default: true
      description:
          ui: Display number at risk below survival curves.

    # Advanced Options
    - name: censoringMarks
      title: Show Censoring Marks
      type: Bool
      default: true
      description:
          ui: Mark censored observations on survival curves.

    - name: survivalTable
      title: Life Table
      type: Bool
      default: false
      description:
          ui: Generate detailed life table with survival estimates.

    - name: medianSurvival
      title: Median Survival Times
      type: Bool
      default: true
      description:
          ui: Calculate median survival time for each group.

    - name: plotTimeRange
      title: Plot Time Range
      type: String
      default: ""
      description:
          ui: >
            Maximum time for plots (leave blank for automatic).
            Example: "60" for 60-month follow-up.
...
```

### Example 3: Complex Analysis
Advanced analysis with multiple subsections:

```yaml
---
name: advancedcoxregression
title: Advanced Cox Proportional Hazards Regression
menuGroup: SurvivalD
menuSubgroup: Regression Models
menuSubtitle: Extended Cox Models with Diagnostics
version: '0.0.3'
jas: '1.2'

description:
    main: |
        Advanced Cox proportional hazards regression with model diagnostics,
        variable selection, and extended features including stratification,
        time-varying coefficients, and frailty models.
    R:
        dontrun: true
        usage: |
            # Basic Cox regression
            result <- advancedcoxregression(
                data = cancer_data,
                time = "survival_months",
                event = "death", 
                covariates = c("age", "stage", "treatment"),
                performDiagnostics = TRUE
            )

options:
    - name: data
      type: Data

    # ===== CORE VARIABLES =====
    - name: time
      title: Survival Time
      type: Variable
      suggested: [continuous]
      permitted: [numeric]
      description:
          ui: Time to event or censoring in consistent units.

    - name: event
      title: Event Indicator
      type: Variable
      suggested: [ordinal, nominal]
      permitted: [factor, numeric]

    - name: eventLevel
      title: Event Level
      type: Level
      variable: (event)

    - name: covariates
      title: Covariates
      type: Variables
      suggested: [continuous, ordinal, nominal]
      permitted: [numeric, factor]
      description:
          ui: Variables to include in Cox regression model.

    # ===== MODEL SPECIFICATION =====
    - name: modelType
      title: Model Type
      type: List
      options:
        - name: standard
          title: Standard Cox Model
        - name: stratified
          title: Stratified Cox Model
        - name: frailty
          title: Frailty Model (Random Effects)
        - name: extended
          title: Extended Cox Model
      default: standard
      description:
          ui: Type of Cox model to fit.

    - name: stratificationVar
      title: Stratification Variable
      type: Variable
      suggested: [nominal, ordinal]
      permitted: [factor]
      default: NULL
      description:
          ui: Variable for stratified Cox model (different baseline hazards per stratum).

    - name: frailtyVar
      title: Frailty Variable
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
      description:
          ui: Clustering variable for frailty model (e.g., hospital, family).

    # ===== VARIABLE SELECTION =====
    - name: performSelection
      title: Variable Selection
      type: Bool
      default: false

    - name: selectionMethod
      title: Selection Method
      type: List
      options:
        - name: forward
          title: Forward Selection
        - name: backward
          title: Backward Elimination
        - name: bidirectional
          title: Bidirectional (Forward/Backward)
        - name: lasso
          title: LASSO Regularization
      default: backward

    - name: selectionCriteria
      title: Selection Criteria
      type: List
      options:
        - name: aic
          title: Akaike Information Criterion (AIC)
        - name: bic
          title: Bayesian Information Criterion (BIC)
        - name: pvalue
          title: P-value Based
      default: aic

    - name: pEntry
      title: Entry P-value
      type: Number
      default: 0.05
      min: 0.01
      max: 0.20
      description:
          ui: P-value threshold for variable entry (forward/bidirectional selection).

    - name: pRemoval
      title: Removal P-value
      type: Number
      default: 0.10
      min: 0.01
      max: 0.50
      description:
          ui: P-value threshold for variable removal (backward/bidirectional selection).

    # ===== MODEL DIAGNOSTICS =====
    - name: performDiagnostics
      title: Model Diagnostics
      type: Bool
      default: true

    - name: diagnosticTests
      title: Diagnostic Tests
      type: NMXList
      options:
        - name: proportional_hazards
          title: Proportional Hazards Assumption
        - name: influential_observations
          title: Influential Observations
        - name: nonlinearity
          title: Non-linearity Assessment
        - name: interactions
          title: Interaction Effects
      default: [proportional_hazards, influential_observations]

    - name: residualPlots
      title: Residual Plots
      type: NMXList
      options:
        - name: schoenfeld
          title: Schoenfeld Residuals
        - name: martingale
          title: Martingale Residuals
        - name: deviance
          title: Deviance Residuals
        - name: dfbeta
          title: DfBeta Residuals
      default: [schoenfeld, martingale]

    # ===== ADVANCED FEATURES =====
    - name: timeVaryingEffects
      title: Time-Varying Coefficients
      type: Bool
      default: false
      description:
          ui: Allow coefficients to change over time.

    - name: timeVaryingVars
      title: Time-Varying Variables
      type: Variables
      suggested: [continuous, ordinal, nominal]
      permitted: [numeric, factor]
      description:
          ui: Variables with potentially time-varying effects.

    - name: robustVariance
      title: Robust Variance Estimation
      type: Bool
      default: false
      description:
          ui: Use robust sandwich variance estimator.

    - name: clusterVar
      title: Cluster Variable
      type: Variable
      suggested: [nominal]
      permitted: [factor]
      default: NULL
      description:
          ui: Variable defining clusters for robust variance estimation.

    # ===== OUTPUT OPTIONS =====
    - name: outputOptions
      title: Output Components
      type: NMXList
      options:
        - name: coefficients_table
          title: Coefficients Table
        - name: hazard_ratios
          title: Hazard Ratios with CI
        - name: model_summary
          title: Model Summary Statistics
        - name: concordance_index
          title: Concordance Index (C-statistic)
        - name: likelihood_ratio_test
          title: Likelihood Ratio Test
      default: [coefficients_table, hazard_ratios, model_summary]

    - name: confidenceLevel
      title: Confidence Level
      type: Number
      default: 0.95
      min: 0.80
      max: 0.99

    - name: showCorrelations
      title: Covariate Correlations
      type: Bool
      default: false
      description:
          ui: Display correlation matrix of model covariates.

    # ===== PRESENTATION =====
    - name: roundingDigits
      title: Rounding Digits
      type: Integer
      default: 3
      min: 1
      max: 6
      description:
          ui: Number of decimal places for results.

    - name: formatPValues
      title: P-value Formatting
      type: List
      options:
        - name: exact
          title: Exact Values
        - name: scientific
          title: Scientific Notation  
        - name: threshold
          title: Threshold (< 0.001)
      default: threshold
...
```

---

## Conclusion

This comprehensive guide covers all aspects of writing `.a.yaml` files for jamovi development. Key takeaways:

1. **Start Simple**: Begin with basic options and add complexity gradually
2. **User-Focused Design**: Always consider the end user's workflow and expertise level
3. **Consistent Patterns**: Follow established conventions for naming, organization, and defaults
4. **Comprehensive Documentation**: Provide clear descriptions for all options
5. **Clinical Relevance**: Include parameters that matter for real-world analysis

### Additional Resources

- [Official jamovi Developer Documentation](https://dev.jamovi.org/api_analysis-definition.html)
- [jamovi Module Examples](https://github.com/jamovi/jmv/tree/master/jamovi) 
- [ClinicoPath Module Examples](https://github.com/sbalci/ClinicoPathJamoviModule/tree/master/jamovi)

### Next Steps

After mastering `.a.yaml` files, explore:
- `.r.yaml` files for results definition
- `.u.yaml` files for interface layout
- `.b.R` files for R implementation  
- Integration patterns between all four components

This guide provides the foundation for creating professional, user-friendly jamovi analyses that serve the clinical and research community effectively.