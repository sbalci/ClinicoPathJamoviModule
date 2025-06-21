# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ClinicoPath is a comprehensive jamovi module for clinicopathological research analysis. It provides statistical analysis tools specifically designed for pathology and clinical research, including survival analysis, decision analysis, descriptive statistics, and specialized plots. The project serves as an umbrella package that coordinates multiple sub-modules distributed across separate repositories.

## Core Architecture

### Jamovi Module Structure

This is a jamovi R module with a specific 4-file architecture pattern that must be followed for all analyses:

- **`.b.R` files**: Backend implementation classes (e.g., `crosstable.b.R`, `survival.b.R`, `decisiongraph.b.R`)
- **`.a.yaml` files**: Analysis definitions with options/parameters (e.g., `crosstable.a.yaml`)
- **`.u.yaml` files**: User interface definitions (e.g., `crosstable.u.yaml`)
- **`.r.yaml` files**: Results/output definitions (e.g., `crosstable.r.yaml`)
- **`.h.R` files**: Auto-generated header files (compiled from .yaml files)

### Key Backend Pattern

All analysis classes inherit from auto-generated base classes and use R6 class system:

```r
crosstableClass <- R6::R6Class(
    "crosstableClass", 
    inherit = crosstableBase,  # Auto-generated from .yaml files
    private = list(
        .init = function() { ... },
        .run = function() { ... }
    )
)
```

### Main Functional Areas

1. **ClinicoPath Descriptives**: Summary statistics, Table One, cross tables, data checking
2. **ClinicoPath Survival**: Survival analysis, Cox regression, Kaplan-Meier, competing risks
3. **meddecide**: Medical decision analysis, ROC curves, sensitivity/specificity, diagnostic tests, decision trees with Markov chain modeling
4. **JJStatsPlot**: Statistical plots using ggstatsplot wrappers
5. **Patient Follow-Up Plots**: Swimmer plots, waterfall plots, treatment response visualization

### Advanced Decision Analysis Architecture

The `decisiongraph` module supports both traditional decision trees and advanced Markov chain models:

**Decision Trees**: One-time decisions with immediate outcomes

- Acute medical conditions (surgery vs. conservative treatment)
- Emergency decisions with clear cost/utility trade-offs
- Point-in-time cost-effectiveness analysis

**Markov Chain Models**: Long-term disease progression modeling

- Chronic disease management with multiple health states
- Transition probability matrices for state changes over time
- Cohort trace analysis with discounted cost-effectiveness calculations
- Multi-cycle analysis for lifetime economic evaluations

The module automatically handles different analysis types based on `treeType` option (`simple`, `costeffectiveness`, `markov`).

## Development Commands

### Testing

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-roc.R')"
```

### Building and Checking

```bash
# Check package
Rscript -e "devtools::check()"

# Build package
Rscript -e "devtools::build()"

# Install development version
Rscript -e "devtools::install()"
```

### Jamovi Module Development

```bash
# Build jamovi module (.jmo file)
# This requires jamovi development tools
Rscript -e "jmvtools::prepare()"
Rscript -e "devtools::document()"
Rscript -e "jmvtools::install()"

# Install module in jamovi for testing
# Copy .jmo file to jamovi modules directory
```

**IMPORTANT**:

- Prepare a dataset and vignette for each function.
  - Make sure the data is comprehensive enough to test all arguments.
  - Vignette should be explanatory for the clinicians.
- After adding a new function, always ensure `jmvtools::prepare()` and `devtools::document()` run without errors to verify module compilation success
- Before closing any GitHub issue, always ensure `jmvtools::prepare()` and `devtools::document()` run without errors to verify module compilation success.

## Jamovi R6 Module Development Patterns

This section documents comprehensive patterns for implementing jamovi modules using R6 classes based on analysis of 100+ existing modules in this project.

### Core R6 Class Structure

All jamovi modules follow this base pattern:

```r
moduleNameClass <- if (requireNamespace("jmvcore")) R6::R6Class("moduleNameClass",
    inherit = moduleNameBase,  # Auto-generated from .yaml files
    private = list(
    
        .init = function() {
            # Initialization code - runs once when module loads
        },
    
        .run = function() {
            # Main analysis code - runs when options change
        },
    
        .plot = function(image, ggtheme, theme, ...) {
            # Plot generation code - runs for Image results
        },
    
        # Additional private helper functions
        .helper_function = function() {
            # Custom helper methods
        }
    )
)
```

### The .init() Function

The `.init()` function runs once when the module is first loaded and is used for:

#### 1. UI State Management and Conditional Display

```r
.init = function() {
    # Control visibility of result components
    if (self$options$show_advanced) {
        self$results$advanced_results$setVisible(TRUE)
    } else {
        self$results$advanced_results$setVisible(FALSE)
    }
  
    # Complex conditional logic
    explanatory_len <- length(self$options$explanatory)
    contexpl_len <- length(self$options$contexpl)
  
    if (explanatory_len > 0 || contexpl_len > 0) {
        self$results$plot8$setSize((explanatory_len + contexpl_len) * 400,
                                   (explanatory_len + contexpl_len) * 300)
    } else {
        self$results$plot8$setVisible(FALSE)
    }
}
```

#### 2. Dynamic Plot Sizing Based on Variables

```r
.init = function() {
    # Adjust plot dimensions based on number of selected variables
    deplen <- length(self$options$dep)
    self$results$plot$setSize(650, deplen * 450)
  
    # Multi-level sizing
    if (!is.null(self$options$grvar)) {
        mydata <- self$data
        grvar <- self$options$grvar
        num_levels <- nlevels(as.factor(mydata[[grvar]]))
        self$results$plot2$setSize(num_levels * 650, deplen * 450)
    }
}
```

#### 3. Table Structure Pre-population

```r
.init = function() {
    # Pre-populate table rows for known structure
    cTable <- self$results$cTable
  
    cTable$addRow(rowKey = "Test Positive",
                  values = list(newtest = "Test Positive"))
    cTable$addRow(rowKey = "Test Negative", 
                  values = list(newtest = "Test Negative"))
    cTable$addRow(rowKey = "Total", values = list(newtest = "Total"))
}
```

#### 4. Instructions and Welcome Messages

```r
.init = function() {
    instructions_html <- paste(
        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h4>Module Instructions:</h4>",
        "<ul>",
        "<li><strong>Step 1:</strong> Select required variables</li>",
        "<li><strong>Step 2:</strong> Configure analysis options</li>",
        "<li><strong>Step 3:</strong> Review results</li>",
        "</ul>",
        "</div>"
    )
    self$results$instructions$setContent(instructions_html)
}
```

### The .run() Function

The `.run()` function contains the main analysis logic and runs whenever user options change:

#### 1. Basic Structure Pattern

```r
.run = function() {
    # 1. Early validation and welcome messages
    if (is.null(self$options$required_var)) {
        welcome_msg <- "<div>Welcome message with instructions...</div>"
        self$results$todo$setContent(welcome_msg)
        return()
    } else {
        self$results$todo$setContent("")
    }
  
    # 2. Data validation
    if (nrow(self$data) == 0) {
        stop("Data contains no (complete) rows")
    }
  
    # 3. Package requirements check
    required_packages <- c("package1", "package2")
    for (pkg in required_packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            error_msg <- paste0("Package ", pkg, " is required")
            self$results$error$setContent(error_msg)
            return()
        }
    }
  
    # 4. Get data and options
    dataset <- self$data
    variables <- self$options$variables
  
    # 5. Perform analysis
    results <- analyze_data(dataset, variables)
  
    # 6. Generate outputs
    if (self$options$show_table) {
        table_html <- generate_table(results)
        self$results$table$setContent(table_html)
    }
  
    # 7. Prepare plot data
    if (self$options$show_plot) {
        plot_data <- prepare_plot_data(results)
        self$results$plot$setState(plot_data)
    }
}
```

#### 2. Complex Conditional Logic

```r
.run = function() {
    # Multi-level validation
    subcondition1a <- !is.null(self$options$outcome)
    subcondition1b1 <- self$options$multievent
    subcondition1b2 <- !is.null(self$options$dod)
    subcondition2a <- !is.null(self$options$elapsedtime)
  
    condition1 <- subcondition1a && !subcondition1b1 || 
                  subcondition1b1 && subcondition1b2
    condition2 <- subcondition2a && self$options$time_calculated
  
    if (!(condition1 && condition2)) {
        self$results$error$setContent("Please check required options")
        return()
    }
  
    # Conditional analysis paths
    if (self$options$analysis_type == "overall") {
        results <- overall_analysis(dataset)
    } else if (self$options$analysis_type == "stratified") {
        results <- stratified_analysis(dataset)
    }
}
```

### The .plot() Function

Plot functions generate visualizations for Image result types:

#### 1. Basic Plot Structure

```r
.plot = function(image, ggtheme, theme, ...) {
    # 1. Check if plot should be generated
    if (!self$options$show_plot)
        return()
  
    # 2. Retrieve plot data from state
    results <- image$state
    if (is.null(results)) {
        return()
    }
  
    # 3. Error checking
    if (nrow(results$data) == 0)
        stop('Data contains no (complete) rows')
  
    # 4. Create plot
    plot <- ggplot2::ggplot(results$data, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        ggplot2::labs(
            title = self$options$plot_title,
            x = self$options$x_label,
            y = self$options$y_label
        )
  
    # 5. Apply theme
    plot <- plot + ggtheme
  
    # 6. Print and return
    print(plot)
    TRUE
}
```

#### 2. Multiple Plot Functions

```r
# Primary plot
.plot = function(image, ggtheme, theme, ...) {
    # Main visualization
}

# Secondary plot
.plot2 = function(image2, ggtheme, theme, ...) {
    # Alternative or grouped visualization
}

# Specialized plots
.plot3 = function(image3, ggtheme, theme, ...) {
    # Additional visualization type
}
```

#### 3. State Management for Complex Data

```r
# In .run() - prepare plot data
plotData <- list(
    "cleanData" = processed_data,
    "variables" = selected_vars,
    "options" = analysis_options,
    "results" = analysis_results
)
image <- self$results$plot
image$setState(plotData)

# In .plot() - use stored data
.plot = function(image, ggtheme, theme, ...) {
    plot_data <- image$state
    if (is.null(plot_data)) return()
  
    # Access stored components
    data <- plot_data$cleanData
    vars <- plot_data$variables
    opts <- plot_data$options
  
    # Generate plot using stored data
    plot <- create_visualization(data, vars, opts)
    plot <- plot + ggtheme
    print(plot)
    TRUE
}
```

### self$options Usage Patterns

Access user-selected variables and settings through `self$options`:

#### 1. Variable Access

```r
# Single variables
outcome_var <- self$options$outcome           # String - variable name
group_var <- self$options$group              # String - grouping variable

# Multiple variables (lists)
dependent_vars <- self$options$dep           # List - multiple variables
explanatory_vars <- self$options$explanatory # List - explanatory variables
```

#### 2. Option Types

```r
# Boolean options
exclude_missing <- self$options$excl         # Boolean - TRUE/FALSE
show_confidence <- self$options$ci           # Boolean - confidence intervals
calculate_power <- self$options$power        # Boolean - power analysis

# String/categorical options
table_style <- self$options$style            # String - "style1", "style2"
analysis_method <- self$options$method       # String - analysis method
output_format <- self$options$format         # String - output format

# Numeric options
confidence_level <- self$options$conf_level  # Numeric - 0.95, 0.99
sample_size <- self$options$n                # Numeric - sample size
alpha_level <- self$options$alpha            # Numeric - significance level
```

#### 3. Complex Conditional Logic

```r
# Multiple option validation
if (self$options$analysis_type == "survival" && 
    !is.null(self$options$time_var) && 
    !is.null(self$options$event_var)) {
  
    # Conditional processing based on date type
    if (self$options$time_format == "ymd") {
        data$time <- lubridate::ymd(data[[self$options$time_var]])
    } else if (self$options$time_format == "mdy") {
        data$time <- lubridate::mdy(data[[self$options$time_var]])
    }
  
    # Analysis type conditional logic
    if (self$options$analysis_type == 'overall') {
        results <- overall_survival_analysis(data)
    } else if (self$options$analysis_type == 'cause_specific') {
        results <- cause_specific_analysis(data)
    }
}
```

### self$results Usage Patterns

Control output display and content through `self$results`:

#### 1. Content Setting Methods

```r
# HTML content
self$results$instructions$setContent(html_content)
self$results$summary$setContent(text_summary)

# Table content
self$results$main_table$setContent(formatted_table)
self$results$statistics$setContent(stats_table)

# Plot state (for images)
self$results$plot$setState(plot_data)
self$results$plot2$setState(secondary_plot_data)
```

#### 2. Visibility Control

```r
# Conditional visibility
if (self$options$show_advanced) {
    self$results$advanced_results$setVisible(TRUE)
} else {
    self$results$advanced_results$setVisible(FALSE)
}

# Hide elements when conditions not met
if (analysis_failed) {
    self$results$main_table$setVisible(FALSE)
    self$results$plot$setVisible(FALSE)
    return()
} else {
    self$results$todo$setVisible(FALSE)
}
```

#### 3. Dynamic Sizing

```r
# Plot sizing based on data
num_variables <- length(self$options$variables)
self$results$plot$setSize(650, num_variables * 450)

# Factor-level based sizing
if (!is.null(self$options$group_var)) {
    num_levels <- nlevels(as.factor(data[[self$options$group_var]]))
    self$results$plot2$setSize(num_levels * 300, 600)
}
```

#### 4. Table Management

```r
# Manual table building
results_table <- self$results$results_table
results_table$addRow(rowKey = "row1", values = list(
    variable = "Variable 1",
    n = 100,
    mean = 25.5,
    sd = 4.2
))

# Dynamic table population
for (i in seq_along(results_data)) {
    results_table$addRow(rowKey = i, values = c(results_data[i,]))
}

# Single row updates
results_table$setRow(rowKey = "summary", values = list(
    total_n = total_sample,
    overall_mean = grand_mean,
    p_value = significance_test$p.value
))
```

#### 5. Advanced Content Management

```r
# Conditional content with rich HTML
if (is.null(self$options$required_var)) {
    welcome_html <- "
    <div style='background-color: #e8f5e8; padding: 20px; border-radius: 8px;'>
        <h3>Welcome to Analysis Module</h3>
        <p><strong>Instructions:</strong></p>
        <ul>
            <li>Select required variables</li>
            <li>Configure analysis options</li>
            <li>Review results and interpretations</li>
        </ul>
    </div>"
    self$results$instructions$setContent(welcome_html)
    return()
} else {
    self$results$instructions$setContent("")
}

# Multiple output formats based on user choice
if (self$options$output_style == "detailed") {
    detailed_table <- generate_detailed_output(results)
    self$results$detailed_output$setContent(detailed_table)
} else if (self$options$output_style == "summary") {
    summary_table <- generate_summary_output(results)
    self$results$summary_output$setContent(summary_table)
}
```

### private$ Functions and Data Storage

Use private functions for helper methods and internal data storage:

#### 1. Helper Functions

```r
private = list(
    .run = function() {
        # Main run function
        processed_data <- private$.process_data(self$data)
        results <- private$.perform_analysis(processed_data)
        private$.generate_outputs(results)
    },
  
    .process_data = function(raw_data) {
        # Data cleaning and preparation
        clean_data <- raw_data %>%
            janitor::clean_names() %>%
            dplyr::filter(!is.na(outcome)) %>%
            dplyr::mutate(processed_var = transform_variable(var))
        return(clean_data)
    },
  
    .perform_analysis = function(data) {
        # Core analysis logic
        if (self$options$method == "parametric") {
            results <- parametric_analysis(data)
        } else {
            results <- nonparametric_analysis(data)
        }
        return(results)
    },
  
    .generate_outputs = function(results) {
        # Output generation
        if (self$options$show_table) {
            table_html <- private$.create_table(results)
            self$results$main_table$setContent(table_html)
        }
    
        if (self$options$show_plot) {
            plot_data <- private$.prepare_plot_data(results)
            self$results$plot$setState(plot_data)
        }
    }
)
```

#### 2. Internal Data Storage

```r
private = list(
    # Internal data fields
    .processed_data = NULL,
    .analysis_results = NULL,
    .plot_cache = NULL,
  
    .init = function() {
        # Initialize internal storage
        private$.processed_data <- NULL
        private$.analysis_results <- NULL
        private$.plot_cache <- NULL
    },
  
    .run = function() {
        # Store processed data for reuse
        private$.processed_data <- process_input_data(self$data)
        private$.analysis_results <- run_analysis(private$.processed_data)
    
        # Generate outputs using stored data
        private$.update_results()
    },
  
    .update_results = function() {
        # Use stored data to update results
        results <- private$.analysis_results
        if (!is.null(results)) {
            self$results$summary$setContent(format_summary(results))
        }
    }
)
```

### Complete Module Example Pattern

```r
#' @title Module Title
#' @description Module description
#' @importFrom R6 R6Class
#' @import jmvcore
#' @importFrom package_name function_name

moduleClass <- if (requireNamespace("jmvcore")) R6::R6Class("moduleClass",
    inherit = moduleBase,
    private = list(
    
        # Internal data storage
        .results_cache = NULL,
        .plot_data = NULL,
    
        .init = function() {
            # UI setup and sizing
            if (self$options$show_advanced) {
                self$results$advanced$setVisible(TRUE)
            }
        
            # Dynamic plot sizing
            n_vars <- length(self$options$variables)
            self$results$plot$setSize(600, n_vars * 200)
        },
    
        .run = function() {
            # 1. Validation
            if (is.null(self$options$variables)) {
                self$results$instructions$setContent(welcome_message)
                return()
            }
        
            # 2. Data processing
            data <- private$.process_data(self$data)
        
            # 3. Analysis
            results <- private$.analyze(data)
        
            # 4. Output generation
            private$.generate_outputs(results)
        },
    
        .plot = function(image, ggtheme, theme, ...) {
            if (!self$options$show_plot) return()
        
            plot_data <- image$state
            if (is.null(plot_data)) return()
        
            plot <- create_plot(plot_data)
            plot <- plot + ggtheme
            print(plot)
            TRUE
        },
    
        # Helper functions
        .process_data = function(raw_data) {
            # Data cleaning logic
            return(processed_data)
        },
    
        .analyze = function(data) {
            # Analysis logic
            return(analysis_results)
        },
    
        .generate_outputs = function(results) {
            # Output generation logic
            if (self$options$show_table) {
                self$results$table$setContent(format_table(results))
            }
            if (self$options$show_plot) {
                self$results$plot$setState(results$plot_data)
            }
        }
    )
)
```

These patterns provide a comprehensive framework for implementing robust, user-friendly jamovi modules with proper error handling, dynamic UI updates, and flexible output generation.

## Jamovi Module File Structure

This section provides comprehensive documentation for the 4-file jamovi module architecture based on analysis of 100+ modules in this project.

### Analysis Definition Files (.a.yaml)

Analysis definition files specify module metadata, options, and parameters.

#### Required Header Structure

```yaml
---
name: modulename                  # Internal module name (no spaces)
title: "Display Title"            # User-visible title
menuGroup: GroupName              # Main menu category
menuSubgroup: SubgroupName        # Submenu category  
menuSubtitle: "Optional Subtitle" # Additional subtitle
version: '0.0.3'                  # Module version
jas: '1.2'                        # jamovi Analysis Specification version
```

#### Menu Groups (menuGroup)

- `Exploration` - Descriptive analysis and EDA
- `Survival` - Survival analysis modules
- `meddecide` - Medical decision analysis
- `JJStatsPlot` - Statistical plotting
- `ExplorationD` - Draft exploration modules
- `SurvivalD` - Draft survival modules
- `meddecideD` - Draft decision modules
- `JJStatsPlotD` - Draft plotting modules

#### Description Block (Optional)

```yaml
description:
  main: |
    Multi-line description of module functionality.
    Supports Markdown formatting.
  R:
    dontrun: true|false           # Whether R examples should be run
    usage: |
      # R code examples
      # Shows typical usage patterns
```

#### Options Structure

All modules must include a data option and analysis-specific options:

```yaml
options:
  - name: data
    type: Data
    description:
      R: >
        The data as a data frame.
      jamovi: >
        The dataset for analysis.
```

#### Option Types and Properties

**Variable Types:**

```yaml
- name: variable_name
  title: "Display Name"
  type: Variable                  # Single variable
  # OR
  type: Variables                 # Multiple variables
  suggested: [continuous, nominal, ordinal]
  permitted: [numeric, factor, id]
  description:
    R: >
      R documentation text.
    ui: >
      UI tooltip text.
    jamovi: >
      Jamovi-specific description.
```

**Variable Suggestions:**

- `continuous` - Numeric continuous variables
- `nominal` - Categorical variables
- `ordinal` - Ordered categorical variables

**Variable Permissions:**

- `numeric` - Numeric data types
- `factor` - Factor/categorical data types
- `id` - ID variables for longitudinal connections

**List/Dropdown Options:**

```yaml
- name: option_name
  title: "Display Name"
  type: List
  options:
    - title: "Display Option 1"
      name: value1
    - title: "Display Option 2" 
      name: value2
  default: value1
```

**Boolean Options:**

```yaml
- name: checkbox_name
  title: "Display Name"
  type: Bool
  default: true|false
```

**Numeric Options:**

```yaml
- name: numeric_name
  title: "Display Name"
  type: Number
  min: 0.1
  max: 5.0
  default: 1.5
```

**String Options:**

```yaml
- name: text_name
  title: "Display Name"
  type: String
  default: "Default text"
```

**Level Selection (for factors):**

```yaml
- name: level_name
  title: "Select Level"
  type: Level
  variable: (parent_variable)     # References another variable option
```

### User Interface Files (.u.yaml)

UI files define the visual layout and controls for the analysis interface.

#### Required Header Structure

```yaml
title: "Module Display Title"
name: modulename                  # Must match .a.yaml name
jus: '3.0'                       # jamovi UI Specification version
stage: 0                         # Development stage (0=stable)
compilerMode: tame|aggressive  # UI compilation mode
```

#### UI Component Types

**VariableSupplier - Variable Selection:**

```yaml
children:
  - type: VariableSupplier
    persistentItems: false
    stretchFactor: 1
    children:
      - type: TargetLayoutBox
        label: "Variable Label"
        children:
          - type: VariablesListBox
            name: variable_option_name    # Must match .a.yaml option
            maxItemCount: 1|unlimited     # 1 for single, omit for multiple
            isTarget: true
            itemDropBehaviour: overwrite  # Optional
```

**Layout Containers:**

```yaml
# Basic layout box with margin
- type: LayoutBox
  margin: large|normal|small
  children:
    # Child components here

# Collapsible section
- type: CollapseBox
  label: "Section Title"
  collapsed: true|false
  children:
    # Child components here

# Group box with border
- type: GroupBox
  title: "Group Title"
  children:
    # Child components here
```

**Form Controls:**

```yaml
# Dropdown/ComboBox
- type: ComboBox
  name: option_name              # Must match .a.yaml option
  label: "Display Label"        # Optional, overrides .a.yaml title
  enable: (condition)            # Conditional enabling

# Checkbox
- type: CheckBox
  name: option_name
  label: "Display Label"
  enable: (condition)

# Text input
- type: TextBox
  name: option_name
  format: string|number          # Data type validation
  label: "Display Label"
  enable: (condition)

# Radio buttons (for List options)
- type: RadioButton
  name: option_name
  optionName: parent_option
  optionPart: "value"
  label: "Radio Label"
  enable: (condition)

# Level selector (for factor variables)
- type: LevelSelector
  name: level_option_name
  enable: (parent_variable)      # Enable when parent variable selected
```

**Labels and Structure:**

```yaml
# Static text label
- type: Label
  label: "Section Label"
  fitToGrid: true|false
  children:
    # Child components here
```

#### Conditional UI Control with `enable`

The `enable` property allows UI components to be conditionally enabled/disabled based on other option values:

**Basic Enable Conditions:**

```yaml
# Enable when option is true/selected
enable: (option_name)

# Enable when option has specific value  
enable: (option_name:value)

# Enable when option is false/not selected
enable: (!option_name)

# Enable when variable is selected
enable: (variable_name)

# Multiple conditions with logical operators
enable: (option1 && option2)          # Both must be true
enable: (option1 || option2)          # Either can be true
enable: (option1 && !option2)         # First true, second false
enable: (!option1 && !option2)        # Both false
```

**Common Enable Patterns:**

```yaml
# Level selectors enabled when parent variable selected
- type: LevelSelector
  name: outcomeLevel
  enable: (outcome && !multievent)

# Date inputs enabled when date calculation checkbox checked
- type: VariablesListBox
  name: dxdate
  enable: (tint)

# Options enabled based on mutually exclusive choices
- type: CheckBox
  name: confidence_intervals
  enable: (!prior_probability)

- type: TextBox
  name: prior_prob_value
  enable: (!confidence_intervals && prior_probability)
```

#### UI Layout Best Practices

1. Group related options in `CollapseBox` or `GroupBox` containers
2. Use `LayoutBox` with appropriate margins for spacing
3. Place primary variables in main `VariableSupplier`
4. Use descriptive labels for all `TargetLayoutBox` components
5. Maintain consistent spacing with `margin: large`
6. Use `enable` conditions to create logical UI flow and prevent invalid combinations
7. Always enable `LevelSelector` components conditionally based on their parent variable

### Results Definition Files (.r.yaml)

Results files define the output structure and types for analysis modules.

#### Required Header Structure

```yaml
---
name: modulename                  # Must match .a.yaml name
title: "Results Title"
jrs: '1.1'                       # jamovi Results Specification version
```

#### Result Item Types

**HTML Output:**

```yaml
items:
  - name: result_name
    title: "Display Title"
    type: Html
    visible: (show_option)        # Conditional visibility
    clearWith:                    # Clear when these options change
      - option1
      - option2
    refs: package_reference       # Package citations
```

**Image/Plot Output:**

```yaml
  - name: plot_name
    title: "Plot Title"
    type: Image
    width: 800                    # Pixel width
    height: 600                   # Pixel height
    renderFun: .plot             # Backend render function
    clearWith:
      - plotting_options
```

**Preformatted Text:**

```yaml
  - name: text_output
    title: "Text Results"
    type: Preformatted
    visible: (show_text)
    clearWith:
      - text_options
```

**Table Output:**

```yaml
  - name: table_name
    title: "Table Title"
    type: Table
    rows: (dynamic_rows)
    clearWith:
      - table_options
    columns:
      - name: column1
        title: "Column 1"
        type: text|number
```

#### Visibility Conditions

Results can be conditionally displayed based on option values:

```yaml
visible: (option_name)              # Show when option is true
visible: (option_name:value)        # Show when option equals value  
visible: "(!option_name)"           # Show when option is false
```

#### Dynamic Titles

Results can use dynamic titles that include option values:

```yaml
title: '`Analysis Results - ${variable_name}`'  # Includes variable name
```

#### Package References

```yaml
refs:
  - package_name1
  - package_name2
```

### Backend Implementation (.b.R) Integration

The YAML files generate header files (.h.R) that define R6 classes with:

1. **Options Classes** - Generated from .a.yaml option definitions
2. **Results Classes** - Generated from .r.yaml item definitions
3. **Base Classes** - Combined options + results for inheritance

Backend .b.R files inherit from these auto-generated base classes:

```r
moduleClass <- R6::R6Class(
    "moduleClass",
    inherit = moduleBase,           # Auto-generated from YAML files
    private = list(
        .init = function() {
            # Initialization code
        },
        .run = function() {
            # Main analysis logic
        },
        .plot = function(image, ggtheme, theme, ...) {
            # Plot rendering (if module has Image results)
        }
    )
)
```

### Module Registration

All modules must be registered in `jamovi/0000.yaml`:

```yaml
analyses:
  - title: "Module Display Title"
    name: modulename
    ns: ClinicoPath
    menuGroup: GroupName
    menuSubgroup: SubgroupName  
    menuTitle: "Menu Display Title"
    menuSubtitle: "Optional Menu Subtitle"
    description: >
      Brief description for module library.
    ribbon: analyses
```

### Compilation and Validation

1. **jmvtools::prepare()** - Compiles YAML files to .h.R headers
2. **devtools::document()** - Generates R documentation
3. Both must complete without errors before module deployment

### Common Patterns and Anti-Patterns

**✅ Good Practices:**

- Use consistent naming conventions across .a.yaml/.u.yaml/.r.yaml
- Group related UI components in logical containers
- Provide clear, descriptive titles and labels
- Include comprehensive option descriptions
- Use appropriate variable suggestions and permissions
- Implement conditional result visibility
- Clear results when relevant options change

**❌ Anti-Patterns:**

- Mismatched option names between YAML files
- Missing required header fields
- Invalid variable permission types
- Overly complex UI layouts without logical grouping
- Missing clearWith specifications for dynamic results
- Inconsistent option types between .a.yaml and .u.yaml

### Adding a new function

```r
Rscript -e "jmvtools::addAnalysis(name = '<newfunctionname>')"
# This will create the necessary .b.R, .a.yaml, .u.yaml, and .r.yaml files
# and update the module structure accordingly.
# Then update these files.
```

### .u.yaml File Structure

The `.u.yaml` file defines the user interface for the analysis module. It includes:

- **title**: The name of the analysis module
- **description**: A brief description of the analysis
- **options**: List of user-configurable options (e.g., variables, statistical tests)
- **results**: Output definitions, including tables and plots

`compilerMode:` is either `aggressive`or `tame`. If aggressive it generates automatically from the `.a.yaml` file, if tame it uses specific changes and organisation defined by the user in the `.u.yaml` file.

### .a.yaml File Structure

The `.a.yaml` file defines the analysis parameters and options. It includes:

- **name**: Unique identifier for the analysis
- **title**: Display name in the jamovi interface
- **description**: Detailed description of the analysis
- **options**: List of parameters that users can configure (e.g., variables, statistical methods)

# Key Dependencies and Patterns

### Core Dependencies

- **jmvcore**: Jamovi module framework
- **R6**: Class system for all analysis classes
- **magrittr**: Pipe operators
- **finalfit**: Survival analysis and medical tables
- **ggstatsplot**: Statistical plotting
- **gtsummary**: Summary tables
- **survival/survminer**: Survival analysis

### Data Processing Pattern

Most modules follow this pattern:

1. Clean variable names with `janitor::clean_names()`
2. Set variable labels with `labelled::set_variable_labels()`
3. Build formulas dynamically based on user inputs
4. Apply appropriate statistical tests
5. Format results for display

### Output Types

- **HTML tables**: Primary output format for most analyses
- **Plots**: ggplot2-based visualizations
- **Text summaries**: Natural language interpretations
- **Export options**: CSV downloads for some modules

## Module Categories by Menu Structure

### Exploration Menu

- **ClinicoPath Descriptives**: `tableone`, `summarydata`, `reportcat`, `benford`
- **ClinicoPath Comparisons**: `crosstable`, `chisqposttest`
- **ClinicoPath Descriptive Plots**: `agepyramid`, `alluvial`, `vartree`, `venn`
- **Patient Follow-Up Plots**: `waterfall`, `swimmerplot`

### Survival Menu

- **ClinicoPath Survival**: `survival`, `survivalcont`, `multisurvival`, `oddsratio`, `singlearm`

### meddecide Menu

- **Agreement**: `agreement`, `icccoeff`
- **Decision**: `decision`, `decisioncalculator`, `decisiongraph`, `nogoldstandard`, `decisioncompare`
- **ROC**: `psychopdaroc`, `roc`
- **Power Analysis**: `kappasizeci`, `kappasizefixedn`, `kappasizepower`

### JJStatsPlot Menu

- **Categorical vs Continuous**: `jjbetweenstats`, `jjwithinstats`, `jjdotplotstats`
- **Categorical vs Categorical**: `jjbarstats`, `jjpiestats`
- **Continuous vs Continuous**: `jjcorrmat`, `jjscatterstats`

## Common Development Tasks

### Adding New Analysis Module

1. Create 4 jamovi files: `.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R`
2. Add entry to `jamovi/0000.yaml`
3. Implement R6 class inheriting from auto-generated base
4. Add to appropriate menu group/subgroup

### Testing Strategy

- Unit tests in `tests/testthat/`
- Test data in `data/` directory (many .csv, .omv, .rda files)
- Example analyses in jamovi format (.omv files)

### Documentation

- Function documentation in .R files using roxygen2
- Vignettes in `vignettes/` directory using R Markdown/Quarto
- Package website built with pkgdown

## File Structure Notes

- `R/`: Backend R code (.b.R analysis classes, utility functions)
- `jamovi/`: Module definitions (.yaml files)
- `data/`: Example datasets in multiple formats
- `man/`: Auto-generated documentation
- `inst/extdata/`: Additional example data files
- `vignettes/`: Documentation and tutorials

## Module Update Workflow

### Automated Module Distribution

This project uses `_updateModules.R` script to synchronize code across 4 separate repositories:

- **Main repository**: ClinicoPathJamoviModule (umbrella package)
- **jjstatsplot**: Statistical plotting functions using ggstatsplot
- **meddecide**: Medical decision analysis tools (includes decision trees, ROC analysis)
- **jsurvival**: Survival analysis functions
- **ClinicoPathDescriptives**: Descriptive statistics and summary tables

The script automatically:

1. Updates version numbers across all DESCRIPTION and YAML files
2. Copies module files (`.b.R`, `.a.yaml`, `.u.yaml`, `.r.yaml`) to appropriate repos
3. Updates documentation and installs modules
4. Commits changes with consistent commit messages

### Version Management

- Version format: `X.Y.Z.BUILD` (e.g., 0.0.3.24)
- Date format: `YYYY-MM-DD`
- Versions are updated centrally and distributed to all sub-modules
- Each module maintains 3-part semantic version in `.a.yaml` files

### Module Distribution Strategy

**WIP vs Release Modes:**

- WIP mode: Limited module sets for testing
- Release mode: Full module distribution
- Controlled by `WIP` flag in update script

### Test Data Generation

- Test data generation scripts in `data-raw/` directory (e.g., `decisiontree_testdata.R`)
- CSV versions automatically created in `inst/extdata/`
- Comprehensive datasets covering all module functionalities
- Edge case testing data for robust validation
- Specialized datasets for decision analysis:
  - Medical treatment comparisons (acute conditions)
  - Pharmaceutical cost-effectiveness studies
  - Disease progression modeling (chronic conditions)
  - Screening program evaluations

## Testing and Quality Assurance

### Test Structure

- Unit tests in `tests/testthat/` directory
- Test data provided in multiple formats (`.csv`, `.omv`, `.rda`)
- Example analyses embedded in jamovi files (`.omv`)
- Key test files: `test-decision.R`, `test-roc.R`, `test-decisiongraph.R`
- Manual testing scripts for complex modules (decision trees, Markov models)
- Performance validation for computationally intensive analyses

### Data Management

- Example datasets in `data/` directory with multiple format support
- Comprehensive test data generation scripts in `data-raw/`
- Each sub-module receives relevant subset of example data
- Extensive test datasets for decision tree analysis:
  - `basic_decision_data`: Surgery vs medical treatment comparisons
  - `markov_decision_data`: Multi-state disease progression models
  - `pharma_decision_data`: Pharmaceutical cost-effectiveness studies
  - `screening_decision_data`: Cancer screening program analysis
  - `appendicitis_decision_tree.csv`: Acute treatment decision example
  - `heart_disease_markov.csv`: Chronic disease Markov model example
  - `minimal_test_data`: Basic functionality testing
  - `edge_case_data`: Error handling and boundary conditions

## Project Distribution Strategy

### Multi-Repository Architecture

The main ClinicoPathJamoviModule repository serves as an umbrella package that coordinates four specialized sub-modules:

1. **ClinicoPathDescriptives**: Basic statistical summaries and descriptive analysis
2. **jsurvival**: Survival analysis (Kaplan-Meier, Cox regression, competing risks)
3. **meddecide**: Medical decision analysis (ROC curves, decision trees, diagnostic tests)
4. **jjstatsplot**: Statistical plotting using ggstatsplot framework

### Installation Methods

- **jamovi Library**: Direct installation through jamovi's module library
- **Sideload**: Manual `.jmo` file installation for development versions
- **R Package**: Direct installation via `devtools::install_github()`
- **Development**: Local builds using `_updateModules.R` script

### Version Synchronization

All sub-modules maintain synchronized version numbers (currently 0.0.3.24) through the automated update script, ensuring compatibility across the ecosystem.

## Special Development Considerations

### Decision Analysis Module Development

When working on decision tree or Markov chain analyses:

1. **Dual Architecture Support**: The `decisiongraph` module handles both decision trees and Markov models based on the `treeType` option
2. **Mathematical Validation**: Transition matrices must be stochastic (rows sum to 1.0)
3. **Performance Considerations**: Markov models with large state spaces or long time horizons require optimization
4. **Extensive Test Data**: Use specialized test datasets that cover realistic medical scenarios

### Module Synchronization Workflow

Use `_updateModules.R` script for coordinated updates across repositories:

```bash
# Update version and synchronize all modules
Rscript _updateModules.R
```

This script:

- Updates version numbers across all DESCRIPTION and YAML files
- Distributes module files to appropriate sub-repositories
- Handles WIP vs release mode configurations
- Maintains version consistency across the ecosystem

### Example Analysis Development

The `examples/` directory contains comprehensive demonstrations:

- `decision_tree_examples.R`: Complete comparison of decision trees vs Markov chains
- `analysis_interpretation_guide.md`: Clinical interpretation guide
- `jamovi_workflow_example.R`: Step-by-step jamovi usage instructions

### Testing Complex Modules

For computationally intensive modules (like Markov models):

1. Use manual testing scripts when unit tests are insufficient
2. Validate mathematical properties (e.g., matrix properties, discounting)
3. Test with multiple realistic datasets
4. Performance profiling for large models

# important-instruction-reminders

Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
