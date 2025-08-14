# A Comprehensive Guide to Writing `.b.R` Files for jamovi Development

This document provides an exhaustive guide to writing `.b.R` files for jamovi module development. The `.b.R` file is the computational core of your analysis, containing all the R code that performs calculations, data processing, and result generation.

## Table of Contents

1. [Introduction: Architecture and Role](#1-introduction-architecture-and-role)
2. [R6 Class Structure and Inheritance](#2-r6-class-structure-and-inheritance)
3. [Core Lifecycle Functions](#3-core-lifecycle-functions)
4. [Data Access and Management](#4-data-access-and-management)
5. [Options and Configuration](#5-options-and-configuration)
6. [Results Population Patterns](#6-results-population-patterns)
7. [Helper Functions and Code Organization](#7-helper-functions-and-code-organization)
8. [Error Handling and Validation](#8-error-handling-and-validation)
9. [State Management](#9-state-management)
10. [Complete Implementation Examples](#10-complete-implementation-examples)
11. [Advanced Patterns](#11-advanced-patterns)
12. [Performance Optimization](#12-performance-optimization)
13. [Clinical Applications](#13-clinical-applications)
14. [Testing and Debugging](#14-testing-and-debugging)
15. [Best Practices](#15-best-practices)
16. [Troubleshooting Guide](#16-troubleshooting-guide)

## 1. Introduction: Architecture and Role

### The Central Role of `.b.R` Files

The `.b.R` file serves as the computational engine of your jamovi analysis. It bridges the gap between user interface definitions (`.a.yaml`, `.u.yaml`) and result presentations (`.r.yaml`) by:

- **Processing user input**: Converting UI selections into analytical parameters
- **Managing data flow**: Accessing, cleaning, and transforming user data
- **Performing computations**: Executing statistical analyses and calculations
- **Generating results**: Populating tables, plots, and other output objects
- **Handling state**: Managing analysis state across user interactions

### jamovi Analysis Workflow

```mermaid
graph TD
    A[User Interface] -->|Options| B[.b.R Class]
    C[User Data] -->|self$data| B
    B -->|.init()| D[Initialize Analysis]
    B -->|.run()| E[Process & Analyze]
    E -->|Results| F[Tables]
    E -->|State| G[Plots]
    E -->|Content| H[HTML/Text]
    F --> I[jamovi Results]
    G --> I
    H --> I
```

### Analysis Lifecycle

1. **Instantiation**: jamovi creates an instance of your analysis class
2. **Initialization**: `.init()` function sets up the analysis environment
3. **Execution**: `.run()` function processes data and generates results
4. **Rendering**: Plot functions render visualizations based on state
5. **Updates**: Process repeats when user changes options or data

## 2. R6 Class Structure and Inheritance

### Basic Class Definition

Every `.b.R` file defines an R6 class that inherits from an auto-generated base class:

```R
# Standard structure with namespace checking
analysisClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "analysisClass",
        inherit = analysisBase,  # Auto-generated from YAML files
        private = list(
            # All analysis logic goes here
            .init = function() { ... },
            .run = function() { ... },
            # Helper functions
            .dataProcessing = function() { ... },
            .statisticalAnalysis = function() { ... },
            # Plot functions
            .plot = function(image, ggtheme, theme, ...) { ... }
        )
    )
```

### Inheritance Hierarchy

```
R6::R6Class
    └── analysisBase (auto-generated)
        ├── Core jamovi functionality
        ├── Results object access
        ├── Options management
        └── Data handling
            └── analysisClass (your implementation)
                ├── Custom analysis logic
                ├── Statistical computations
                └── Result generation
```

### Class Components

#### Essential Elements

- **Class Name**: Must match your analysis name + "Class"
- **Inheritance**: Always inherits from the auto-generated base class
- **Private List**: Contains all your analysis implementation
- **Namespace Check**: Ensures jmvcore availability

#### Advanced Class Features

```R
# Advanced class with comprehensive structure
survivalClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "survivalClass",
        inherit = survivalBase,
        private = list(
            # Configuration
            .config = list(
                version = "1.0.0",
                supported_methods = c("kaplan-meier", "cox", "competing-risks"),
                default_options = list(
                    time_unit = "months",
                    confidence_level = 0.95
                )
            ),
            
            # Cache for expensive computations
            .cache = list(
                survival_fit = NULL,
                cox_model = NULL,
                last_analysis_hash = NULL
            ),
            
            # Core lifecycle functions
            .init = function() { ... },
            .run = function() { ... },
            
            # Data processing pipeline
            .validateData = function() { ... },
            .cleanData = function() { ... },
            .transformData = function() { ... },
            
            # Analysis functions
            .performSurvivalAnalysis = function() { ... },
            .calculateStatistics = function() { ... },
            .generateSummaries = function() { ... },
            
            # Result population
            .populateTables = function() { ... },
            .setPlotStates = function() { ... },
            .generateReports = function() { ... },
            
            # Plot functions
            .plot = function(image, ggtheme, theme, ...) { ... },
            .plotROC = function(image, ggtheme, theme, ...) { ... },
            
            # Utility functions
            .formatResults = function() { ... },
            .generateExplanations = function() { ... },
            .logAnalysisStep = function(step, details) { ... }
        )
    )
```

## 3. Core Lifecycle Functions

### The `.init()` Function

Called once when the analysis is first instantiated. Used for:

- Setting initial UI state
- Configuring result object visibility
- Initializing analysis parameters
- Setting up caching mechanisms

#### Basic `.init()` Implementation

```R
.init = function() {
    # Set conditional visibility
    if (self$options$showAdvanced) {
        self$results$advancedTable$setVisible(TRUE)
        self$results$diagnosticPlots$setVisible(TRUE)
    } else {
        self$results$advancedTable$setVisible(FALSE)
        self$results$diagnosticPlots$setVisible(FALSE)
    }
    
    # Initialize plot arrays if needed
    if (self$options$multipleGroups) {
        # Set up array plots for each group
        groups <- self$options$groupingVariable
        if (!is.null(groups)) {
            self$results$groupPlots$create(keys = groups)
        }
    }
}
```

#### Advanced `.init()` Implementation

```R
.init = function() {
    # Version compatibility checking
    if (!private$.checkCompatibility()) {
        self$results$instructions$setContent(
            "<p><b>Warning:</b> This analysis requires updated packages.</p>"
        )
    }
    
    # Dynamic result structure setup
    private$.setupResultStructure()
    
    # Initialize analysis cache
    private$.cache$last_options_hash <- NULL
    private$.cache$computed_results <- list()
    
    # Set up conditional visibility based on analysis type
    analysis_type <- self$options$analysisType
    
    switch(analysis_type,
        "survival" = {
            self$results$survivalTable$setVisible(TRUE)
            self$results$coxTable$setVisible(self$options$includeCox)
            self$results$competingRisks$setVisible(FALSE)
        },
        "competing_risks" = {
            self$results$survivalTable$setVisible(FALSE)
            self$results$coxTable$setVisible(FALSE)
            self$results$competingRisks$setVisible(TRUE)
        },
        "landmark" = {
            self$results$landmarkTable$setVisible(TRUE)
            self$results$conditionalSurvival$setVisible(TRUE)
        }
    )
    
    # Initialize explanatory content
    if (self$options$showExplanations) {
        private$.initializeExplanations()
    }
}
```

### The `.run()` Function

The main analysis orchestrator, called every time user options change:

#### Standard `.run()` Workflow

```R
.run = function() {
    # 1. Early validation
    if (!private$.validateInputs()) {
        private$.displayTodoMessage()
        return()
    }
    
    # 2. Check if re-computation is needed
    if (private$.needsRecomputation()) {
        # 3. Data preparation
        processed_data <- private$.prepareData()
        if (is.null(processed_data)) {
            return()
        }
        
        # 4. Statistical analysis
        analysis_results <- private$.performAnalysis(processed_data)
        if (is.null(analysis_results)) {
            return()
        }
        
        # 5. Cache results
        private$.cacheResults(analysis_results)
    } else {
        # Use cached results
        analysis_results <- private$.getCachedResults()
    }
    
    # 6. Populate outputs
    private$.populateAllResults(analysis_results)
    
    # 7. Set plot states
    private$.setAllPlotStates(analysis_results)
    
    # 8. Generate explanations if requested
    if (self$options$showExplanations) {
        private$.generateExplanations(analysis_results)
    }
}
```

#### Clinical Example: Survival Analysis `.run()`

```R
.run = function() {
    # Validate essential inputs
    if (is.null(self$options$outcome) || 
        is.null(self$options$elapsedtime) ||
        is.null(self$options$explanatory)) {
        private$.displayInstructions()
        return()
    }
    
    # Check data quality
    data_quality <- private$.assessDataQuality()
    if (!data_quality$acceptable) {
        private$.displayDataWarnings(data_quality$issues)
        return()
    }
    
    # Prepare survival data
    survival_data <- private$.prepareSurvivalData()
    if (is.null(survival_data)) {
        return()
    }
    
    # Perform analyses based on options
    results <- list()
    
    # Median survival analysis
    if (self$options$calculateMedian) {
        results$median <- private$.calculateMedianSurvival(survival_data)
        private$.populateMedianTable(results$median)
    }
    
    # Cox regression analysis
    if (self$options$performCox) {
        results$cox <- private$.performCoxRegression(survival_data)
        private$.populateCoxTable(results$cox)
        
        # Proportional hazards testing
        if (self$options$testProportionalHazards) {
            results$ph_test <- private$.testProportionalHazards(results$cox)
            private$.populatePhTable(results$ph_test)
        }
    }
    
    # Survival probability tables
    if (self$options$calculateSurvProb) {
        time_points <- private$.parseTimePoints(self$options$timePoints)
        results$surv_prob <- private$.calculateSurvivalProbabilities(
            survival_data, time_points
        )
        private$.populateSurvivalTable(results$surv_prob)
    }
    
    # Pairwise comparisons
    if (self$options$pairwiseComparisons && 
        length(unique(survival_data$group)) > 2) {
        results$pairwise <- private$.performPairwiseComparisons(survival_data)
        private$.populatePairwiseTable(results$pairwise)
    }
    
    # Set plot states for all requested visualizations
    plot_data <- private$.preparePlotData(survival_data, results)
    
    if (self$options$survivalCurve) {
        self$results$survivalPlot$setState(plot_data$survival)
    }
    
    if (self$options$cumulativeHazard) {
        self$results$hazardPlot$setState(plot_data$hazard)
    }
    
    if (self$options$rocCurve && !is.null(results$cox)) {
        roc_data <- private$.prepareROCData(results$cox)
        self$results$rocPlot$setState(roc_data)
    }
}
```

## 4. Data Access and Management

### Accessing User Data

The primary data interface is `self$data`, which provides access to the user's dataset:

```R
# Basic data access
user_data <- self$data

# Get specific columns based on user options
time_column <- self$data[[self$options$timeVariable]]
status_column <- self$data[[self$options$statusVariable]]
group_column <- self$data[[self$options$groupVariable]]
```

### Data Validation and Quality Assessment

```R
.validateData = function() {
    data <- self$data
    issues <- list()
    
    # Check for required variables
    required_vars <- c(self$options$outcome, self$options$predictor)
    missing_vars <- setdiff(required_vars, names(data))
    
    if (length(missing_vars) > 0) {
        issues$missing_variables <- missing_vars
    }
    
    # Check data types
    if (!is.null(self$options$timeVariable)) {
        time_var <- self$options$timeVariable
        if (!is.numeric(data[[time_var]])) {
            issues$time_not_numeric <- time_var
        }
        
        # Check for negative times
        if (any(data[[time_var]] < 0, na.rm = TRUE)) {
            issues$negative_times <- TRUE
        }
    }
    
    # Check for sufficient data
    if (nrow(data) < 10) {
        issues$insufficient_data <- nrow(data)
    }
    
    # Check for excessive missing data
    missing_proportion <- sapply(data[required_vars], function(x) {
        sum(is.na(x)) / length(x)
    })
    
    if (any(missing_proportion > 0.5)) {
        issues$excessive_missing <- missing_proportion[missing_proportion > 0.5]
    }
    
    return(list(
        valid = length(issues) == 0,
        issues = issues
    ))
}
```

### Data Cleaning and Preprocessing

```R
.cleanData = function() {
    # Get raw data
    raw_data <- self$data
    
    # Create working copy
    clean_data <- raw_data
    
    # Clean variable names for R compatibility
    original_names <- names(clean_data)
    clean_data <- clean_data %>% janitor::clean_names()
    
    # Maintain variable labels for reference
    name_mapping <- setNames(original_names, names(clean_data))
    clean_data <- labelled::set_variable_labels(
        .data = clean_data,
        .labels = name_mapping
    )
    
    # Map user-selected variables to clean names
    variable_map <- private$.createVariableMapping(clean_data, original_names)
    
    # Handle missing values based on user preferences
    if (self$options$excludeMissing) {
        required_vars <- c(
            variable_map$time,
            variable_map$status,
            variable_map$group
        )
        
        clean_data <- clean_data %>%
            dplyr::filter(
                dplyr::if_all(dplyr::all_of(required_vars), ~ !is.na(.x))
            )
    }
    
    # Convert data types
    if (!is.null(variable_map$time)) {
        clean_data[[variable_map$time]] <- as.numeric(clean_data[[variable_map$time]])
    }
    
    if (!is.null(variable_map$status)) {
        clean_data[[variable_map$status]] <- as.factor(clean_data[[variable_map$status]])
    }
    
    # Handle date calculations if needed
    if (self$options$calculateFromDates) {
        clean_data <- private$.calculateTimeFromDates(clean_data, variable_map)
    }
    
    # Apply filters if specified
    if (!is.null(self$options$filterCondition)) {
        clean_data <- private$.applyDataFilters(clean_data)
    }
    
    return(list(
        data = clean_data,
        variables = variable_map,
        n_original = nrow(raw_data),
        n_final = nrow(clean_data)
    ))
}
```

### Advanced Data Processing

```R
.processComplexData = function() {
    cleaned_data <- private$.cleanData()
    
    if (is.null(cleaned_data)) {
        return(NULL)
    }
    
    data <- cleaned_data$data
    variables <- cleaned_data$variables
    
    # Create analysis-specific variables
    
    # 1. Time-to-event processing
    if (self$options$analysisType == "survival") {
        # Handle different time units
        time_multiplier <- switch(self$options$timeUnit,
            "days" = 1,
            "weeks" = 7,
            "months" = 30.44,  # Average month length
            "years" = 365.25   # Account for leap years
        )
        
        data$analysis_time <- data[[variables$time]] / time_multiplier
        
        # Create survival object
        if (!is.null(variables$status)) {
            status_var <- data[[variables$status]]
            event_level <- self$options$eventLevel
            
            # Binary status indicator
            data$event <- as.numeric(status_var == event_level)
            
            # Create Surv object
            data$surv_object <- survival::Surv(
                time = data$analysis_time,
                event = data$event
            )
        }
    }
    
    # 2. Grouping variable processing
    if (!is.null(variables$group)) {
        group_var <- data[[variables$group]]
        
        # Handle different group types
        if (is.numeric(group_var)) {
            # Convert to factor if numeric
            if (self$options$autoFactorizeNumeric) {
                unique_vals <- sort(unique(group_var[!is.na(group_var)]))
                if (length(unique_vals) <= 10) {
                    data[[variables$group]] <- factor(group_var)
                }
            }
        } else if (is.character(group_var)) {
            data[[variables$group]] <- factor(group_var)
        }
        
        # Order factor levels if specified
        if (!is.null(self$options$groupOrder)) {
            desired_order <- strsplit(self$options$groupOrder, ",")[[1]]
            desired_order <- trimws(desired_order)
            
            current_levels <- levels(data[[variables$group]])
            matching_levels <- intersect(desired_order, current_levels)
            
            if (length(matching_levels) > 0) {
                data[[variables$group]] <- factor(
                    data[[variables$group]],
                    levels = matching_levels
                )
            }
        }
    }
    
    # 3. Stratification variables
    if (self$options$useStratification && !is.null(self$options$stratificationVar)) {
        strata_var <- self$options$stratificationVar
        if (strata_var %in% names(data)) {
            data$strata <- factor(data[[strata_var]])
        }
    }
    
    # 4. Additional covariates processing
    if (!is.null(self$options$covariates)) {
        covariate_vars <- strsplit(self$options$covariates, ",")[[1]]
        covariate_vars <- trimws(covariate_vars)
        
        for (cov in covariate_vars) {
            if (cov %in% names(data)) {
                # Auto-detect and convert data types
                if (is.character(data[[cov]])) {
                    data[[cov]] <- factor(data[[cov]])
                } else if (is.logical(data[[cov]])) {
                    data[[cov]] <- factor(data[[cov]], levels = c(FALSE, TRUE))
                }
            }
        }
    }
    
    return(list(
        data = data,
        variables = variables,
        summary = list(
            n_observations = nrow(data),
            n_events = sum(data$event, na.rm = TRUE),
            n_censored = sum(1 - data$event, na.rm = TRUE),
            n_groups = length(unique(data[[variables$group]])),
            median_followup = median(data$analysis_time, na.rm = TRUE)
        )
    ))
}
```

## 5. Options and Configuration

### Accessing User Options

Options defined in `.a.yaml` are accessible through `self$options`:

```R
# Basic option access
confidence_level <- self$options$confidenceLevel
analysis_method <- self$options$method
show_plots <- self$options$displayPlots

# Handle optional options with defaults
time_unit <- self$options$timeUnit %||% "months"
color_scheme <- self$options$colorScheme %||% "default"
```

### Option Validation and Processing

```R
.validateOptions = function() {
    options <- self$options
    issues <- list()
    
    # Required options check
    required_options <- c("outcome", "timeVariable")
    missing_options <- sapply(required_options, function(opt) {
        is.null(options[[opt]]) || options[[opt]] == ""
    })
    
    if (any(missing_options)) {
        issues$missing_required <- names(missing_options)[missing_options]
    }
    
    # Numeric range validation
    if (!is.null(options$confidenceLevel)) {
        if (options$confidenceLevel <= 0 || options$confidenceLevel >= 1) {
            issues$invalid_confidence <- options$confidenceLevel
        }
    }
    
    # Logical dependencies
    if (options$performCox && is.null(options$explanatory)) {
        issues$cox_missing_explanatory <- TRUE
    }
    
    if (options$pairwiseComparisons && is.null(options$groupVariable)) {
        issues$pairwise_missing_group <- TRUE
    }
    
    # Custom validation based on analysis type
    if (options$analysisType == "competing_risks") {
        required_competing <- c("causeSpecificEvent", "competingEvent")
        missing_competing <- sapply(required_competing, function(opt) {
            is.null(options[[opt]]) || options[[opt]] == ""
        })
        
        if (any(missing_competing)) {
            issues$competing_risks_incomplete <- names(missing_competing)[missing_competing]
        }
    }
    
    return(list(
        valid = length(issues) == 0,
        issues = issues
    ))
}
```

### Dynamic Option Processing

```R
.processOptions = function() {
    # Create processed options object
    processed <- list()
    
    # Time-related processing
    processed$time_conversion <- switch(self$options$timeUnit,
        "days" = 1,
        "weeks" = 7,
        "months" = 30.44,
        "years" = 365.25,
        1  # Default to days
    )
    
    # Parse time points for survival probability estimation
    if (!is.null(self$options$timePoints)) {
        time_string <- gsub("\\s+", "", self$options$timePoints)
        processed$time_points <- as.numeric(strsplit(time_string, ",")[[1]])
        processed$time_points <- processed$time_points[!is.na(processed$time_points)]
    }
    
    # Process grouping and stratification
    processed$use_grouping <- !is.null(self$options$explanatory) && 
                             self$options$explanatory != ""
    
    processed$use_stratification <- self$options$stratifiedAnalysis && 
                                   !is.null(self$options$stratificationVariable)
    
    # Analysis method configuration
    processed$methods <- list()
    
    if (self$options$calculateMedian) {
        processed$methods$median <- list(
            confidence_level = self$options$confidenceLevel,
            method = self$options$medianMethod %||% "kaplan-meier"
        )
    }
    
    if (self$options$performCox) {
        processed$methods$cox <- list(
            method = ifelse(processed$use_stratification, "stratified", "standard"),
            robust = self$options$robustVariance,
            test_ph = self$options$testProportionalHazards
        )
    }
    
    # Plot configuration
    processed$plots <- list(
        theme = self$options$plotTheme %||% "jamovi",
        color_scheme = self$options$colorScheme %||% "default",
        show_confidence = self$options$showConfidenceIntervals,
        show_risk_table = self$options$showRiskTable,
        custom_time_range = !is.null(self$options$plotTimeRange)
    )
    
    if (processed$plots$custom_time_range) {
        time_range <- strsplit(self$options$plotTimeRange, ",")[[1]]
        processed$plots$time_limits <- as.numeric(trimws(time_range))
    }
    
    return(processed)
}
```

## 6. Results Population Patterns

### Table Population

Populating tables defined in `.r.yaml`:

```R
# Basic table population
.populateBasicTable = function(results_data) {
    table <- self$results$summaryTable
    
    for (i in seq_len(nrow(results_data))) {
        row_data <- results_data[i, ]
        
        table$addRow(
            rowKey = i,
            values = list(
                variable = row_data$variable,
                estimate = row_data$estimate,
                ci_lower = row_data$ci_lower,
                ci_upper = row_data$ci_upper,
                p_value = row_data$p_value
            )
        )
    }
}

# Advanced table population with formatting
.populateAdvancedTable = function(analysis_results) {
    table <- self$results$detailedTable
    
    # Clear existing rows
    table$deleteRows()
    
    for (group in names(analysis_results$by_group)) {
        group_data <- analysis_results$by_group[[group]]
        
        # Add group header
        table$addRow(
            rowKey = paste0("header_", group),
            values = list(
                parameter = group,
                estimate = "",
                se = "",
                ci = "",
                p_value = ""
            )
        )
        
        # Add parameter rows for this group
        for (param in names(group_data$parameters)) {
            param_data <- group_data$parameters[[param]]
            
            # Format confidence interval
            ci_text <- sprintf("(%.3f, %.3f)", 
                              param_data$ci_lower, 
                              param_data$ci_upper)
            
            # Format p-value
            p_text <- if (param_data$p_value < 0.001) {
                "< 0.001"
            } else {
                sprintf("%.3f", param_data$p_value)
            }
            
            table$addRow(
                rowKey = paste0(group, "_", param),
                values = list(
                    parameter = paste0("  ", param),  # Indent sub-items
                    estimate = sprintf("%.3f", param_data$estimate),
                    se = sprintf("%.3f", param_data$se),
                    ci = ci_text,
                    p_value = p_text
                )
            )
        }
    }
}
```

### HTML Content Population

```R
.populateHtmlResults = function(analysis_results) {
    # Summary interpretation
    summary_html <- private$.generateSummaryHtml(analysis_results)
    self$results$interpretation$setContent(summary_html)
    
    # Method description
    method_html <- private$.generateMethodHtml()
    self$results$methodDescription$setContent(method_html)
    
    # Warnings and notes
    if (length(analysis_results$warnings) > 0) {
        warning_html <- private$.generateWarningHtml(analysis_results$warnings)
        self$results$warnings$setContent(warning_html)
    }
}

.generateSummaryHtml = function(results) {
    html_parts <- c()
    
    # Header
    html_parts <- c(html_parts, 
        "<div class='analysis-summary'>",
        "<h4>Analysis Summary</h4>"
    )
    
    # Sample size information
    html_parts <- c(html_parts,
        sprintf("<p><strong>Sample Size:</strong> %d observations</p>", 
                results$n_total),
        sprintf("<p><strong>Events:</strong> %d (%.1f%%)</p>", 
                results$n_events, 
                100 * results$n_events / results$n_total)
    )
    
    # Key findings
    if (!is.null(results$median_survival)) {
        html_parts <- c(html_parts,
            sprintf("<p><strong>Median Survival:</strong> %.1f %s (95%% CI: %.1f-%.1f)</p>",
                    results$median_survival$estimate,
                    self$options$timeUnit,
                    results$median_survival$ci_lower,
                    results$median_survival$ci_upper)
        )
    }
    
    # Statistical significance
    if (!is.null(results$overall_test)) {
        significance <- if (results$overall_test$p_value < 0.05) {
            "statistically significant"
        } else {
            "not statistically significant"
        }
        
        html_parts <- c(html_parts,
            sprintf("<p><strong>Overall Test:</strong> %s (p = %.3f)</p>",
                    significance,
                    results$overall_test$p_value)
        )
    }
    
    html_parts <- c(html_parts, "</div>")
    
    return(paste(html_parts, collapse = "\n"))
}
```

### Plot State Management

```R
.setPlotStates = function(analysis_results) {
    # Survival curve plot
    if (self$options$showSurvivalCurve) {
        survival_state <- list(
            data = analysis_results$survival_data,
            fit = analysis_results$survival_fit,
            options = list(
                show_ci = self$options$showConfidenceIntervals,
                show_risk_table = self$options$showRiskTable,
                time_unit = self$options$timeUnit,
                color_scheme = self$options$colorScheme
            )
        )
        
        self$results$survivalPlot$setState(survival_state)
    }
    
    # Forest plot for Cox model
    if (self$options$showForestPlot && !is.null(analysis_results$cox_model)) {
        forest_state <- list(
            model = analysis_results$cox_model,
            data = analysis_results$cox_data,
            options = list(
                log_scale = self$options$useLogScale,
                show_reference = self$options$showReference,
                sort_by = self$options$forestSortBy
            )
        )
        
        self$results$forestPlot$setState(forest_state)
    }
    
    # Diagnostic plots
    if (self$options$showDiagnostics && !is.null(analysis_results$diagnostics)) {
        diagnostic_state <- list(
            residuals = analysis_results$diagnostics$residuals,
            influence = analysis_results$diagnostics$influence,
            proportional_hazards = analysis_results$diagnostics$ph_test
        )
        
        self$results$diagnosticPlots$setState(diagnostic_state)
    }
}
```

## 7. Helper Functions and Code Organization

### Modular Function Structure

Organize complex analyses into focused helper functions:

```R
private = list(
    # Data processing pipeline
    .validateInputs = function() { ... },
    .cleanData = function() { ... },
    .prepareAnalysisData = function() { ... },
    
    # Statistical analysis modules
    .performDescriptiveAnalysis = function() { ... },
    .performInferentialAnalysis = function() { ... },
    .calculateDiagnostics = function() { ... },
    
    # Result formatting
    .formatStatisticalResults = function() { ... },
    .generateSummaryStatistics = function() { ... },
    .createResultTables = function() { ... },
    
    # Visualization preparation
    .preparePlotData = function() { ... },
    .configureVisualizationOptions = function() { ... },
    
    # Utility functions
    .formatPValue = function(p) { ... },
    .formatConfidenceInterval = function(lower, upper) { ... },
    .generateTimestamp = function() { ... }
)
```

### Survival Analysis Helper Functions Example

```R
# Data preparation helpers
.prepareSurvivalData = function() {
    cleaned <- private$.cleanData()
    if (is.null(cleaned)) return(NULL)
    
    data <- cleaned$data
    variables <- cleaned$variables
    
    # Create survival object
    if (self$options$useTimeInterval) {
        # Interval-censored data
        surv_obj <- survival::Surv(
            time = data[[variables$start_time]],
            time2 = data[[variables$end_time]],
            event = data[[variables$status]],
            type = "interval"
        )
    } else {
        # Right-censored data
        surv_obj <- survival::Surv(
            time = data[[variables$time]],
            event = data[[variables$status]]
        )
    }
    
    # Add survival object to data
    data$surv_object <- surv_obj
    
    # Create analysis formula
    formula_terms <- c("surv_object")
    
    if (!is.null(variables$group)) {
        formula_terms <- c(formula_terms, variables$group)
    }
    
    if (self$options$includeStrata && !is.null(variables$strata)) {
        formula_terms <- c(formula_terms, 
                          paste0("strata(", variables$strata, ")"))
    }
    
    analysis_formula <- as.formula(
        paste(formula_terms[1], "~", paste(formula_terms[-1], collapse = " + "))
    )
    
    return(list(
        data = data,
        variables = variables,
        formula = analysis_formula,
        n_total = nrow(data),
        n_events = sum(data[[variables$status]]),
        n_censored = sum(1 - data[[variables$status]])
    ))
}

# Statistical analysis helpers
.calculateMedianSurvival = function(survival_data) {
    fit <- survival::survfit(
        formula = survival_data$formula,
        data = survival_data$data,
        conf.int = self$options$confidenceLevel
    )
    
    # Extract median survival times
    median_results <- summary(fit)$table
    
    if (is.matrix(median_results)) {
        # Multiple groups
        results <- data.frame(
            group = rownames(median_results),
            median = median_results[, "median"],
            ci_lower = median_results[, sprintf("%.3gLCL", self$options$confidenceLevel)],
            ci_upper = median_results[, sprintf("%.3gUCL", self$options$confidenceLevel)],
            stringsAsFactors = FALSE
        )
    } else {
        # Single group
        results <- data.frame(
            group = "Overall",
            median = median_results["median"],
            ci_lower = median_results[sprintf("%.3gLCL", self$options$confidenceLevel)],
            ci_upper = median_results[sprintf("%.3gUCL", self$options$confidenceLevel)],
            stringsAsFactors = FALSE
        )
    }
    
    return(list(
        results = results,
        fit_object = fit,
        method = "Kaplan-Meier"
    ))
}

.performCoxRegression = function(survival_data) {
    # Fit Cox proportional hazards model
    cox_fit <- survival::coxph(
        formula = survival_data$formula,
        data = survival_data$data,
        robust = self$options$robustVariance
    )
    
    # Extract results
    cox_summary <- summary(cox_fit)
    
    # Format results table
    results_table <- data.frame(
        variable = rownames(cox_summary$coefficients),
        hazard_ratio = exp(cox_summary$coefficients[, "coef"]),
        ci_lower = exp(cox_summary$conf.int[, "lower .95"]),
        ci_upper = exp(cox_summary$conf.int[, "upper .95"]),
        p_value = cox_summary$coefficients[, "Pr(>|z|)"]
    )
    
    # Overall model test
    overall_test <- list(
        likelihood_ratio = cox_summary$logtest["test"],
        df = cox_summary$logtest["df"],
        p_value = cox_summary$logtest["pvalue"]
    )
    
    return(list(
        model = cox_fit,
        summary = cox_summary,
        results_table = results_table,
        overall_test = overall_test,
        concordance = cox_summary$concordance
    ))
}

# Diagnostic helpers
.testProportionalHazards = function(cox_results) {
    if (is.null(cox_results$model)) {
        return(NULL)
    }
    
    # Schoenfeld residuals test
    ph_test <- survival::cox.zph(cox_results$model)
    
    # Format results
    ph_results <- data.frame(
        variable = rownames(ph_test$table),
        chi_square = ph_test$table[, "chisq"],
        df = ph_test$table[, "df"],
        p_value = ph_test$table[, "p"]
    )
    
    # Global test
    global_test <- list(
        chi_square = ph_test$table["GLOBAL", "chisq"],
        df = ph_test$table["GLOBAL", "df"],
        p_value = ph_test$table["GLOBAL", "p"]
    )
    
    return(list(
        test_object = ph_test,
        results_table = ph_results,
        global_test = global_test,
        assumption_violated = global_test$p_value < 0.05
    ))
}
```

## 8. Error Handling and Validation

### Comprehensive Error Handling Framework

```R
.safeExecute = function(operation, error_context = "analysis") {
    tryCatch({
        operation()
    }, error = function(e) {
        error_msg <- paste(
            "Error in", error_context, ":", 
            e$message
        )
        
        # Log error for debugging
        private$.logError(error_msg, e)
        
        # Display user-friendly message
        private$.displayErrorMessage(error_context, e)
        
        return(NULL)
    }, warning = function(w) {
        warning_msg <- paste(
            "Warning in", error_context, ":",
            w$message
        )
        
        # Log warning
        private$.logWarning(warning_msg, w)
        
        # Continue execution but note warning
        invokeRestart("muffleWarning")
    })
}

.validateInputsComprehensive = function() {
    validation_results <- list(
        valid = TRUE,
        errors = list(),
        warnings = list()
    )
    
    # Check required options
    required_options <- private$.getRequiredOptions()
    
    for (option in required_options) {
        if (is.null(self$options[[option]]) || self$options[[option]] == "") {
            validation_results$errors[[length(validation_results$errors) + 1]] <- 
                list(
                    type = "missing_option",
                    option = option,
                    message = paste("Required option", option, "is missing")
                )
            validation_results$valid <- FALSE
        }
    }
    
    # Check data availability and quality
    if (is.null(self$data) || nrow(self$data) == 0) {
        validation_results$errors[[length(validation_results$errors) + 1]] <- 
            list(
                type = "no_data",
                message = "No data available for analysis"
            )
        validation_results$valid <- FALSE
    } else {
        # Check minimum sample size
        min_n <- private$.getMinimumSampleSize()
        if (nrow(self$data) < min_n) {
            validation_results$warnings[[length(validation_results$warnings) + 1]] <- 
                list(
                    type = "small_sample",
                    current_n = nrow(self$data),
                    minimum_n = min_n,
                    message = paste("Sample size (", nrow(self$data), 
                                  ") is below recommended minimum (", min_n, ")")
                )
        }
        
        # Validate specific variables
        variable_validation <- private$.validateVariables()
        if (!variable_validation$valid) {
            validation_results$errors <- c(
                validation_results$errors,
                variable_validation$errors
            )
            validation_results$valid <- FALSE
        }
        
        validation_results$warnings <- c(
            validation_results$warnings,
            variable_validation$warnings
        )
    }
    
    # Analysis-specific validation
    analysis_validation <- private$.validateAnalysisSpecific()
    if (!analysis_validation$valid) {
        validation_results$errors <- c(
            validation_results$errors,
            analysis_validation$errors
        )
        validation_results$valid <- FALSE
    }
    
    return(validation_results)
}
```

### Error Messaging and User Feedback

```R
.displayErrorMessage = function(context, error) {
    # Generate user-friendly error message
    user_message <- private$.translateErrorMessage(error)
    
    # Display in appropriate results object
    if (!is.null(self$results$instructions)) {
        error_html <- sprintf(
            "<div class='error-message'>" +
            "<h4>Analysis Error</h4>" +
            "<p><strong>Context:</strong> %s</p>" +
            "<p><strong>Issue:</strong> %s</p>" +
            "<p><strong>Suggestion:</strong> %s</p>" +
            "</div>",
            context,
            user_message$description,
            user_message$suggestion
        )
        
        self$results$instructions$setContent(error_html)
    }
}

.translateErrorMessage = function(error) {
    # Common error patterns and user-friendly translations
    error_msg <- error$message
    
    if (grepl("object.*not found", error_msg)) {
        return(list(
            description = "A required variable was not found in the data",
            suggestion = "Please check that all selected variables exist in your dataset"
        ))
    }
    
    if (grepl("subscript out of bounds", error_msg)) {
        return(list(
            description = "There was an issue accessing data elements",
            suggestion = "This may be due to missing data or incorrect variable selection"
        ))
    }
    
    if (grepl("non-numeric argument", error_msg)) {
        return(list(
            description = "A numeric variable contains non-numeric values",
            suggestion = "Please ensure all numeric variables contain only numbers"
        ))
    }
    
    if (grepl("convergence", error_msg)) {
        return(list(
            description = "The statistical model failed to converge",
            suggestion = "Try simplifying the model or checking for data issues"
        ))
    }
    
    # Default message for unrecognized errors
    return(list(
        description = "An unexpected error occurred during analysis",
        suggestion = "Please check your data and options, or contact support"
    ))
}
```

## 9. State Management

### Analysis State Caching

```R
# Private cache management
private = list(
    .cache = list(
        last_options_hash = NULL,
        last_data_hash = NULL,
        cached_results = NULL,
        cache_timestamp = NULL
    ),
    
    .needsRecomputation = function() {
        # Calculate current state hash
        current_options_hash <- private$.hashOptions()
        current_data_hash <- private$.hashData()
        
        # Check if anything changed
        options_changed <- is.null(private$.cache$last_options_hash) ||
                          private$.cache$last_options_hash != current_options_hash
        
        data_changed <- is.null(private$.cache$last_data_hash) ||
                       private$.cache$last_data_hash != current_data_hash
        
        # Check cache age (invalidate after 1 hour)
        cache_expired <- is.null(private$.cache$cache_timestamp) ||
                        difftime(Sys.time(), private$.cache$cache_timestamp, 
                                units = "hours") > 1
        
        return(options_changed || data_changed || cache_expired)
    },
    
    .hashOptions = function() {
        # Create hash of relevant options
        relevant_options <- self$options[private$.getCacheableOptions()]
        digest::digest(relevant_options, algo = "md5")
    },
    
    .hashData = function() {
        # Create hash of data (subset for efficiency)
        if (is.null(self$data) || nrow(self$data) == 0) {
            return("no_data")
        }
        
        # Hash first few rows, last few rows, and summary statistics
        sample_rows <- min(100, nrow(self$data))
        data_sample <- rbind(
            head(self$data, sample_rows/2),
            tail(self$data, sample_rows/2)
        )
        
        digest::digest(data_sample, algo = "md5")
    },
    
    .cacheResults = function(results) {
        private$.cache$cached_results <- results
        private$.cache$last_options_hash <- private$.hashOptions()
        private$.cache$last_data_hash <- private$.hashData()
        private$.cache$cache_timestamp <- Sys.time()
    },
    
    .getCachedResults = function() {
        private$.cache$cached_results
    }
)
```

### Plot State Management

```R
.managePlotStates = function(analysis_results) {
    # Prepare base plot data
    base_plot_data <- list(
        data = analysis_results$processed_data,
        options = list(
            time_unit = self$options$timeUnit,
            confidence_level = self$options$confidenceLevel,
            color_scheme = self$options$colorScheme,
            theme = self$options$plotTheme
        )
    )
    
    # Survival plot state
    if (self$options$showSurvivalPlot) {
        survival_state <- base_plot_data
        survival_state$analysis_type <- "survival"
        survival_state$survival_fit <- analysis_results$survival_fit
        survival_state$risk_table <- self$options$showRiskTable
        
        self$results$survivalPlot$setState(survival_state)
    }
    
    # Hazard plot state
    if (self$options$showHazardPlot) {
        hazard_state <- base_plot_data
        hazard_state$analysis_type <- "hazard"
        hazard_state$cumulative <- self$options$cumulativeHazard
        
        self$results$hazardPlot$setState(hazard_state)
    }
    
    # Forest plot state (for Cox models)
    if (self$options$showForestPlot && !is.null(analysis_results$cox_results)) {
        forest_state <- list(
            model_results = analysis_results$cox_results,
            options = list(
                log_scale = self$options$forestLogScale,
                show_reference = self$options$showReferenceLevel,
                sort_by = self$options$forestSortBy,
                confidence_level = self$options$confidenceLevel
            )
        )
        
        self$results$forestPlot$setState(forest_state)
    }
    
    # Diagnostic plots state
    if (self$options$showDiagnostics && !is.null(analysis_results$diagnostics)) {
        diagnostic_state <- list(
            diagnostics = analysis_results$diagnostics,
            model = analysis_results$cox_results$model,
            options = list(
                residual_type = self$options$residualType,
                influence_measures = self$options$showInfluence
            )
        )
        
        self$results$diagnosticPlots$setState(diagnostic_state)
    }
}
```

## 10. Complete Implementation Examples

### Example 1: Basic Statistical Analysis (.b.R)

```R
#' @title Basic Statistical Analysis Implementation
#' @description Demonstrates core .b.R patterns for simple statistical analysis

basicAnalysisClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "basicAnalysisClass",
        inherit = basicAnalysisBase,
        private = list(
            
            .init = function() {
                # Set initial visibility based on options
                if (self$options$advancedOptions) {
                    self$results$advancedTable$setVisible(TRUE)
                    self$results$diagnosticPlot$setVisible(TRUE)
                } else {
                    self$results$advancedTable$setVisible(FALSE)
                    self$results$diagnosticPlot$setVisible(FALSE)
                }
                
                # Initialize instructions
                private$.updateInstructions()
            },
            
            .run = function() {
                # Input validation
                validation <- private$.validateInputs()
                if (!validation$valid) {
                    private$.displayValidationErrors(validation$errors)
                    return()
                }
                
                # Data preparation
                processed_data <- private$.prepareData()
                if (is.null(processed_data)) {
                    return()
                }
                
                # Perform analysis
                results <- private$.performAnalysis(processed_data)
                if (is.null(results)) {
                    return()
                }
                
                # Populate results
                private$.populateResults(results)
                
                # Set plot states
                private$.setPlotStates(results)
            },
            
            .validateInputs = function() {
                errors <- list()
                
                # Check required variables
                if (is.null(self$options$dependent) || self$options$dependent == "") {
                    errors <- c(errors, "Dependent variable is required")
                }
                
                if (is.null(self$options$independent) || self$options$independent == "") {
                    errors <- c(errors, "Independent variable is required")
                }
                
                # Check data availability
                if (is.null(self$data) || nrow(self$data) == 0) {
                    errors <- c(errors, "No data available")
                }
                
                return(list(
                    valid = length(errors) == 0,
                    errors = errors
                ))
            },
            
            .prepareData = function() {
                tryCatch({
                    data <- self$data
                    
                    # Extract variables
                    dep_var <- self$options$dependent
                    indep_var <- self$options$independent
                    
                    # Check variable existence
                    if (!dep_var %in% names(data)) {
                        stop(paste("Dependent variable", dep_var, "not found"))
                    }
                    
                    if (!indep_var %in% names(data)) {
                        stop(paste("Independent variable", indep_var, "not found"))
                    }
                    
                    # Clean data
                    clean_data <- data[, c(dep_var, indep_var)]
                    clean_data <- na.omit(clean_data)
                    
                    if (nrow(clean_data) == 0) {
                        stop("No complete cases available after removing missing values")
                    }
                    
                    return(list(
                        data = clean_data,
                        dependent = dep_var,
                        independent = indep_var,
                        n_original = nrow(self$data),
                        n_final = nrow(clean_data)
                    ))
                    
                }, error = function(e) {
                    private$.displayError("Data preparation", e$message)
                    return(NULL)
                })
            },
            
            .performAnalysis = function(processed_data) {
                tryCatch({
                    data <- processed_data$data
                    dep_var <- processed_data$dependent
                    indep_var <- processed_data$independent
                    
                    # Determine analysis type based on variable types
                    dep_type <- class(data[[dep_var]])[1]
                    indep_type <- class(data[[indep_var]])[1]
                    
                    if (dep_type %in% c("numeric", "integer") && 
                        indep_type %in% c("factor", "character")) {
                        # ANOVA or t-test
                        results <- private$.performGroupComparison(data, dep_var, indep_var)
                        
                    } else if (dep_type %in% c("numeric", "integer") && 
                               indep_type %in% c("numeric", "integer")) {
                        # Correlation/regression
                        results <- private$.performCorrelation(data, dep_var, indep_var)
                        
                    } else if (dep_type %in% c("factor", "character") && 
                               indep_type %in% c("factor", "character")) {
                        # Chi-square test
                        results <- private$.performChiSquare(data, dep_var, indep_var)
                        
                    } else {
                        stop("Unsupported variable type combination")
                    }
                    
                    return(results)
                    
                }, error = function(e) {
                    private$.displayError("Statistical analysis", e$message)
                    return(NULL)
                })
            },
            
            .performGroupComparison = function(data, dep_var, indep_var) {
                # Convert independent variable to factor
                data[[indep_var]] <- as.factor(data[[indep_var]])
                
                # Determine test type
                n_groups <- length(levels(data[[indep_var]]))
                
                if (n_groups == 2) {
                    # t-test
                    test_result <- t.test(
                        formula = as.formula(paste(dep_var, "~", indep_var)),
                        data = data,
                        var.equal = self$options$assumeEqualVariances
                    )
                    
                    return(list(
                        type = "t-test",
                        statistic = test_result$statistic,
                        p_value = test_result$p.value,
                        df = test_result$parameter,
                        confidence_interval = test_result$conf.int,
                        means = test_result$estimate
                    ))
                    
                } else {
                    # ANOVA
                    anova_result <- aov(
                        formula = as.formula(paste(dep_var, "~", indep_var)),
                        data = data
                    )
                    
                    anova_summary <- summary(anova_result)
                    
                    return(list(
                        type = "ANOVA",
                        f_statistic = anova_summary[[1]][1, "F value"],
                        p_value = anova_summary[[1]][1, "Pr(>F)"],
                        df_between = anova_summary[[1]][1, "Df"],
                        df_within = anova_summary[[1]][2, "Df"],
                        sum_squares = anova_summary[[1]][, "Sum Sq"]
                    ))
                }
            },
            
            .populateResults = function(results) {
                # Main results table
                main_table <- self$results$mainTable
                
                if (results$type == "t-test") {
                    main_table$addRow(
                        rowKey = 1,
                        values = list(
                            test = "Independent samples t-test",
                            statistic = sprintf("t(%.0f) = %.3f", 
                                              results$df, results$statistic),
                            p_value = results$p_value,
                            effect_size = private$.calculateCohenD(results)
                        )
                    )
                    
                } else if (results$type == "ANOVA") {
                    main_table$addRow(
                        rowKey = 1,
                        values = list(
                            test = "One-way ANOVA",
                            statistic = sprintf("F(%.0f, %.0f) = %.3f",
                                              results$df_between,
                                              results$df_within,
                                              results$f_statistic),
                            p_value = results$p_value,
                            effect_size = private$.calculateEtaSquared(results)
                        )
                    )
                }
                
                # Summary statistics
                if (self$options$descriptiveStatistics) {
                    private$.populateDescriptives()
                }
            },
            
            .setPlotStates = function(results) {
                if (self$options$showPlot) {
                    plot_data <- list(
                        data = self$data,
                        dependent = self$options$dependent,
                        independent = self$options$independent,
                        analysis_results = results
                    )
                    
                    self$results$mainPlot$setState(plot_data)
                }
            },
            
            # Plot functions
            .plot = function(image, ggtheme, theme, ...) {
                state <- image$state
                
                if (is.null(state)) return(FALSE)
                
                data <- state$data
                dep_var <- state$dependent
                indep_var <- state$independent
                
                # Create appropriate plot based on variable types
                if (is.numeric(data[[dep_var]]) && is.factor(data[[indep_var]])) {
                    # Box plot for group comparison
                    plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = indep_var, y = dep_var)) +
                        ggplot2::geom_boxplot() +
                        ggplot2::geom_jitter(width = 0.2, alpha = 0.5) +
                        ggplot2::labs(
                            x = indep_var,
                            y = dep_var,
                            title = paste("Distribution of", dep_var, "by", indep_var)
                        ) +
                        ggtheme
                        
                } else {
                    # Scatter plot for continuous variables
                    plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = indep_var, y = dep_var)) +
                        ggplot2::geom_point(alpha = 0.6) +
                        ggplot2::geom_smooth(method = "lm", se = TRUE) +
                        ggplot2::labs(
                            x = indep_var,
                            y = dep_var,
                            title = paste("Relationship between", indep_var, "and", dep_var)
                        ) +
                        ggtheme
                }
                
                print(plot)
                return(TRUE)
            }
        )
    )
```

### Example 2: Advanced Survival Analysis (.b.R)

```R
#' @title Advanced Survival Analysis Implementation
#' @description Comprehensive survival analysis with multiple methods

advancedSurvivalClass <- if (requireNamespace('jmvcore', quietly = TRUE))
    R6::R6Class(
        "advancedSurvivalClass",
        inherit = advancedSurvivalBase,
        private = list(
            
            # Configuration and cache
            .config = list(
                supported_distributions = c("exponential", "weibull", "lognormal"),
                min_events_per_group = 5,
                max_computation_time = 30  # seconds
            ),
            
            .cache = list(
                survival_fit = NULL,
                cox_model = NULL,
                last_analysis_hash = NULL
            ),
            
            .init = function() {
                # Advanced initialization
                private$.initializeAdvancedOptions()
                private$.setupConditionalVisibility()
                private$.initializeExplanations()
            },
            
            .run = function() {
                # Comprehensive analysis workflow
                
                # 1. Validation and preparation
                if (!private$.validateAdvancedInputs()) {
                    return()
                }
                
                survival_data <- private$.prepareAdvancedSurvivalData()
                if (is.null(survival_data)) {
                    return()
                }
                
                # 2. Cache management
                analysis_hash <- private$.calculateAnalysisHash(survival_data)
                if (private$.cache$last_analysis_hash == analysis_hash) {
                    # Use cached results
                    private$.populateFromCache()
                    return()
                }
                
                # 3. Progressive analysis based on complexity
                results <- list()
                
                # Basic survival analysis
                results$basic <- private$.performBasicSurvival(survival_data)
                
                # Advanced modeling if requested
                if (self$options$advancedModeling) {
                    results$advanced <- private$.performAdvancedModeling(survival_data)
                }
                
                # Comparative analysis
                if (self$options$compareModels) {
                    results$comparison <- private$.compareModels(
                        survival_data, results$basic, results$advanced
                    )
                }
                
                # Bootstrap analysis for robust inference
                if (self$options$bootstrapInference) {
                    results$bootstrap <- private$.performBootstrapAnalysis(
                        survival_data, n_bootstrap = self$options$nBootstrap
                    )
                }
                
                # 4. Cache results
                private$.cacheResults(results, analysis_hash)
                
                # 5. Populate all outputs
                private$.populateAllResults(results)
                private$.setAdvancedPlotStates(results)
            },
            
            .performAdvancedModeling = function(survival_data) {
                results <- list()
                
                # Parametric survival models
                if (self$options$parametricModels) {
                    results$parametric <- private$.fitParametricModels(survival_data)
                }
                
                # Flexible parametric models
                if (self$options$flexibleParametric) {
                    results$flexible <- private$.fitFlexibleParametric(survival_data)
                }
                
                # Machine learning approaches
                if (self$options$machineLearning) {
                    results$ml <- private$.fitMLModels(survival_data)
                }
                
                # Time-varying effects
                if (self$options$timeVaryingEffects) {
                    results$time_varying <- private$.analyzeTimeVaryingEffects(survival_data)
                }
                
                return(results)
            },
            
            .fitParametricModels = function(survival_data) {
                models <- list()
                
                # Fit multiple distributions
                distributions <- self$options$selectedDistributions
                
                for (dist in distributions) {
                    tryCatch({
                        if (requireNamespace("flexsurv", quietly = TRUE)) {
                            model <- flexsurv::flexsurvreg(
                                formula = survival_data$formula,
                                data = survival_data$data,
                                dist = dist
                            )
                            
                            models[[dist]] <- list(
                                model = model,
                                aic = model$AIC,
                                loglik = model$loglik,
                                parameters = model$coefficients
                            )
                        }
                    }, error = function(e) {
                        warning(paste("Failed to fit", dist, "model:", e$message))
                    })
                }
                
                # Model selection
                if (length(models) > 1) {
                    aic_values <- sapply(models, function(m) m$aic)
                    best_model <- names(aic_values)[which.min(aic_values)]
                    
                    models$best_model <- best_model
                    models$model_comparison <- data.frame(
                        distribution = names(aic_values),
                        aic = aic_values,
                        delta_aic = aic_values - min(aic_values)
                    )
                }
                
                return(models)
            },
            
            .performBootstrapAnalysis = function(survival_data, n_bootstrap = 1000) {
                # Bootstrap confidence intervals for survival estimates
                bootstrap_results <- list()
                
                # Setup parallel processing if available
                if (requireNamespace("parallel", quietly = TRUE) && self$options$useParallel) {
                    n_cores <- min(parallel::detectCores() - 1, 4)
                    
                    bootstrap_estimates <- parallel::mclapply(
                        seq_len(n_bootstrap),
                        function(i) {
                            # Bootstrap sample
                            boot_indices <- sample(nrow(survival_data$data), replace = TRUE)
                            boot_data <- survival_data$data[boot_indices, ]
                            
                            # Fit model to bootstrap sample
                            tryCatch({
                                boot_fit <- survival::survfit(
                                    formula = survival_data$formula,
                                    data = boot_data
                                )
                                
                                # Extract key estimates
                                summary_at_times <- summary(boot_fit, 
                                                           times = self$options$evaluationTimes)
                                
                                return(list(
                                    median = summary(boot_fit)$table["median"],
                                    survival_at_times = summary_at_times$surv
                                ))
                            }, error = function(e) {
                                return(NULL)
                            })
                        },
                        mc.cores = n_cores
                    )
                } else {
                    # Sequential bootstrap
                    bootstrap_estimates <- lapply(seq_len(n_bootstrap), function(i) {
                        # ... bootstrap logic ...
                    })
                }
                
                # Process bootstrap results
                valid_estimates <- bootstrap_estimates[!sapply(bootstrap_estimates, is.null)]
                
                if (length(valid_estimates) > 0) {
                    # Calculate bootstrap confidence intervals
                    bootstrap_results$median_ci <- private$.calculateBootstrapCI(
                        sapply(valid_estimates, function(x) x$median),
                        confidence_level = self$options$confidenceLevel
                    )
                    
                    bootstrap_results$survival_ci <- private$.calculateBootstrapCI(
                        t(sapply(valid_estimates, function(x) x$survival_at_times)),
                        confidence_level = self$options$confidenceLevel
                    )
                }
                
                return(bootstrap_results)
            },
            
            # Advanced plotting
            .plotAdvancedSurvival = function(image, ggtheme, theme, ...) {
                state <- image$state
                
                if (is.null(state)) return(FALSE)
                
                # Multi-panel survival visualization
                if (state$plot_type == "comprehensive") {
                    plot <- private$.createComprehensiveSurvivalPlot(state, ggtheme)
                } else if (state$plot_type == "model_comparison") {
                    plot <- private$.createModelComparisonPlot(state, ggtheme)
                } else if (state$plot_type == "time_varying") {
                    plot <- private$.createTimeVaryingEffectsPlot(state, ggtheme)
                }
                
                print(plot)
                return(TRUE)
            }
        )
    )
```

## 11. Advanced Patterns

### Analysis Pipeline Architecture

```R
# Sophisticated analysis pipeline
.runAdvancedPipeline = function() {
    # Create analysis context
    context <- private$.createAnalysisContext()
    
    # Progressive complexity handling
    pipeline_steps <- list(
        "validation" = function(ctx) private$.validateContext(ctx),
        "preprocessing" = function(ctx) private$.preprocessData(ctx),
        "descriptive" = function(ctx) private$.computeDescriptives(ctx),
        "inferential" = function(ctx) private$.performInference(ctx),
        "postprocessing" = function(ctx) private$.postprocessResults(ctx),
        "visualization" = function(ctx) private$.prepareVisualizations(ctx)
    )
    
    # Execute pipeline with error recovery
    for (step_name in names(pipeline_steps)) {
        result <- private$.executeStep(
            step = pipeline_steps[[step_name]],
            context = context,
            step_name = step_name
        )
        
        if (!result$success) {
            private$.handleStepFailure(step_name, result$error)
            return()
        }
        
        context <- result$context
    }
    
    # Finalize results
    private$.finalizeAnalysis(context)
}
```

### Dynamic Result Structure Management

```R
# Adaptive result structure based on analysis complexity
.setupDynamicResults = function() {
    # Determine required result objects based on options
    required_objects <- private$.analyzeRequiredObjects()
    
    # Create result structure dynamically
    for (obj_config in required_objects) {
        if (obj_config$type == "array") {
            # Create array results for multiple groups/conditions
            keys <- private$.determineArrayKeys(obj_config)
            self$results[[obj_config$name]]$create(keys = keys)
        } else if (obj_config$conditional) {
            # Set visibility based on conditions
            visibility <- private$.evaluateVisibilityCondition(obj_config$condition)
            self$results[[obj_config$name]]$setVisible(visibility)
        }
    }
}
```

## 12. Performance Optimization

### Efficient Data Processing

```R
# Optimized data processing for large datasets
.processLargeDataset = function() {
    data_size <- nrow(self$data)
    
    if (data_size > 100000) {
        # Use data.table for efficiency
        if (requireNamespace("data.table", quietly = TRUE)) {
            dt <- data.table::setDT(self$data)
            processed <- private$.processWithDataTable(dt)
        } else {
            # Chunked processing
            processed <- private$.processInChunks()
        }
    } else {
        # Standard dplyr processing
        processed <- private$.processWithDplyr()
    }
    
    return(processed)
}
```

## 13. Clinical Applications

### Clinical Trial Analysis Patterns

```R
# Comprehensive clinical trial analysis
.runClinicalTrialAnalysis = function() {
    # Validate clinical trial structure
    trial_validation <- private$.validateTrialStructure()
    if (!trial_validation$valid) {
        private$.displayTrialErrors(trial_validation$issues)
        return()
    }
    
    # Baseline characteristics
    baseline_results <- private$.analyzeBaselineCharacteristics()
    private$.populateBaselineTable(baseline_results)
    
    # Primary endpoint analysis
    primary_results <- private$.analyzePrimaryEndpoint()
    private$.populatePrimaryResults(primary_results)
}
```

## 14. Testing and Debugging

### Unit Testing Framework

```R
# Testing framework for .b.R functions
.createTestSuite = function() {
    test_suite <- list(
        test_data_validation = function() {
            # Test with valid data
            valid_data <- data.frame(
                time = c(10, 20, 30, 40),
                status = c(1, 0, 1, 1),
                group = factor(c("A", "B", "A", "B"))
            )
            
            self$data <- valid_data
            validation <- private$.validateInputs()
            stopifnot(validation$valid)
        }
    )
    
    return(test_suite)
}
```

## 15. Best Practices

### Code Quality Standards

1. **Function Length**: Keep functions under 50 lines when possible
2. **Error Handling**: Always implement comprehensive error checking
3. **Documentation**: Document all public and complex private functions
4. **Performance**: Monitor execution time and memory usage
5. **Testing**: Implement unit tests for critical functions

### Naming Conventions

- Use descriptive function names: `.calculateSurvivalStatistics()` not `.calcStats()`
- Prefix private functions with dot: `.helperFunction()`
- Use camelCase for variables: `analysisResults` not `analysis_results`
- Use meaningful parameter names in options access: `self$options$confidenceLevel`

## 16. Troubleshooting Guide

### Common Issues and Solutions

#### Issue: Analysis Not Running

**Symptoms:**
- `.run()` function not called
- No results appearing
- jamovi interface shows "Analysis not ready"

**Solutions:**
1. Check `.a.yaml` option definitions match `.b.R` usage
2. Verify required options have values
3. Ensure data is properly loaded
4. Check for errors in `.init()` function

#### Issue: Results Not Populating

**Symptoms:**
- Analysis runs but tables/plots remain empty
- No error messages
- `.run()` completes successfully

**Solutions:**
1. Verify column names match `.r.yaml` definitions
2. Check data types match expected formats
3. Ensure `addRow()` is called with correct parameters
4. Verify result object references are correct

#### Issue: Memory Problems

**Symptoms:**
- Analysis becomes slow with large datasets
- R session crashes
- Out of memory errors

**Solutions:**
```R
# Implement chunked processing for large datasets
.processInChunks = function(chunk_size = 10000) {
    data <- self$data
    n_rows <- nrow(data)
    chunks <- split(seq_len(n_rows), ceiling(seq_len(n_rows) / chunk_size))
    
    results <- list()
    for (i in seq_along(chunks)) {
        chunk_data <- data[chunks[[i]], ]
        results[[i]] <- private$.processChunk(chunk_data)
    }
    
    return(private$.combineChunkResults(results))
}
```

This comprehensive guide provides the foundation for implementing sophisticated, reliable, and maintainable `.b.R` files in jamovi modules. The patterns and examples demonstrate best practices for clinical research applications while ensuring robust error handling and optimal performance.
