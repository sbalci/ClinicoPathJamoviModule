# A Comprehensive Guide to Creating and Populating Plots in jamovi

This document provides a comprehensive guide to creating and populating plots in jamovi module development. Plots are a crucial component of data analysis, providing visual insights that complement statistical results. This guide covers the complete workflow from defining plots in YAML files to implementing sophisticated visualization functions.

## Table of Contents

1. [Introduction: Architecture and Workflow](#1-introduction-architecture-and-workflow)
2. [Plot Definition in .r.yaml Files](#2-plot-definition-in-ryaml-files)
3. [Data Flow and State Management](#3-data-flow-and-state-management)
4. [Plot Function Implementation](#4-plot-function-implementation)
5. [Complete Examples](#5-complete-examples)
6. [Advanced Plot Types](#6-advanced-plot-types)
7. [Theme Integration and Styling](#7-theme-integration-and-styling)
8. [Error Handling and Validation](#8-error-handling-and-validation)
9. [Performance Optimization](#9-performance-optimization)
10. [Clinical Applications](#10-clinical-applications)
11. [Best Practices](#11-best-practices)
12. [Troubleshooting Guide](#12-troubleshooting-guide)

## 1. Introduction: Architecture and Workflow

Creating plots in jamovi follows a structured approach that separates concerns between definition, data preparation, and rendering:

### The Three-Stage Process

1. **Definition Stage (`.r.yaml`)**: Define plot structure, dimensions, and metadata
2. **Preparation Stage (`.b.R` - `.run()`)**: Process data and set plot state
3. **Rendering Stage (`.b.R` - `.plotFunction()`)**: Generate the actual visualization

### Core Components

```mermaid
graph TD
    A[.r.yaml Definition] --> B[.run() Data Preparation]
    B --> C[image$setState()]
    C --> D[.plotFunction() Rendering]
    D --> E[ggplot2 + ggtheme]
    E --> F[print() Output]
```

### jamovi Plot Architecture

The jamovi plot system is built around these fundamental concepts:

- **Image Objects**: Container objects that hold plot definitions and state
- **State Management**: Data persistence between analysis runs and plot rendering
- **Theme Integration**: Consistent styling across the jamovi interface
- **Dynamic Rendering**: Plots update automatically when options or data change

## 2. Plot Definition in .r.yaml Files

### Basic Plot Structure

Every plot in jamovi is defined as an `Image` type item in the `.r.yaml` file:

```yaml
- name: plotName
  title: Plot Title
  type: Image
  width: 600
  height: 450
  renderFun: .plotFunction
  visible: (condition)
  requiresData: true
```

### Essential Properties

#### Core Properties

| Property | Type | Description | Required |
|----------|------|-------------|----------|
| `name` | string | Unique identifier for the plot | Yes |
| `title` | string | Display title (supports dynamic variables) | Yes |
| `type` | string | Must be "Image" for plots | Yes |
| `renderFun` | string | Name of the rendering function in .b.R | Yes |

#### Display Properties

| Property | Type | Description | Default |
|----------|------|-------------|----------|
| `width` | integer | Plot width in pixels | 400 |
| `height` | integer | Plot height in pixels | 300 |
| `visible` | boolean/condition | Visibility condition | true |
| `requiresData` | boolean | Whether plot needs data | true |

### Dynamic Titles with Variables

Use `${variableName}` syntax for dynamic titles:

```yaml
- name: survivalPlot
  title: '`Survival Analysis - ${explanatory} by ${outcome}`'
  type: Image
  renderFun: .plotSurvival
```

### Visibility Conditions

Control plot visibility based on user options:

```yaml
# Simple boolean condition
visible: (showPlots)

# Complex logical conditions
visible: (plotType == 'survival' && !isEmpty(explanatory))

# Multiple conditions
visible: (enablePlots && hasData && !errorState)
```

### Clear Conditions

Specify when plots should be refreshed:

```yaml
clearWith:
  - explanatory    # Refresh when explanatory variable changes
  - outcome        # Refresh when outcome variable changes
  - plotOptions    # Refresh when plot options change
  - data          # Refresh when data changes
```

### Array Plots for Multiple Visualizations

For analyses with multiple similar plots:

```yaml
- name: rocCurves
  type: Array
  title: ROC Curves
  visible: (showROC)
  template:
    type: Image
    width: 550
    height: 450
    renderFun: .plotROC
    clearWith:
      - dependentVars
      - classVar
      - method
```

### Real-World Example: Survival Analysis Plots

From `survival.r.yaml`:

```yaml
# Main survival curve
- name: plot
  title: '`Survival Plot - ${explanatory}`'
  type: Image
  width: 600
  height: 450
  renderFun: .plot
  visible: (sc)
  requiresData: true
  clearWith:
    - sc
    - endplot
    - byplot
    - explanatory
    - outcome
    - overalltime

# Cumulative events plot
- name: plot2
  title: '`Cumulative Events - ${explanatory}`'
  type: Image
  width: 600
  height: 450
  renderFun: .plot2
  visible: (ce)
  requiresData: true

# KMunicate-style plot
- name: plot6
  title: '`KMunicate-Style Plot - ${explanatory}`'
  type: Image
  width: 600
  height: 450
  renderFun: .plot6
  visible: (kmunicate)
  requiresData: true
  refs:
    - KMunicate
    - KMunicate2
```

### References and Citations

Include relevant citations for plot methods:

```yaml
refs:
  - ggplot2
  - survminer
  - KMunicate
```

## 3. Data Flow and State Management

### The State System

The `image$state` system is the backbone of jamovi plot data management:

```R
# In .run() function - Setting state
plotData <- list(
  data = processedData,
  options = list(
    colorBy = self$options$colorBy,
    showCI = self$options$confidenceInterval
  ),
  metadata = list(
    analysisType = "survival",
    timestamp = Sys.time()
  )
)

self$results$plotName$setState(plotData)
```

### State Data Structure Best Practices

#### Recommended State Structure

```R
plotState <- list(
  # Core data
  data = cleanedDataFrame,
  
  # Processed variables
  variables = list(
    x = "time_variable",
    y = "outcome_variable",
    group = "grouping_variable"
  ),
  
  # Analysis results
  results = list(
    summary = summaryStats,
    tests = statisticalTests
  ),
  
  # Plot configuration
  config = list(
    colors = colorPalette,
    theme = themeSettings,
    annotations = annotations
  ),
  
  # Metadata
  meta = list(
    n_obs = nrow(data),
    analysis_date = Sys.Date(),
    version = "1.0"
  )
)
```

### State Management Patterns

#### Conditional State Setting

```R
# Only set state if conditions are met
if (self$options$showPlot && !is.null(preparedData)) {
  plotImage <- self$results$mainPlot
  plotImage$setState(plotState)
}
```

#### Multi-Plot State Management

```R
# For multiple related plots
for (i in seq_along(variables)) {
  plotData <- prepareDataForVariable(variables[i])
  plotImage <- self$results$plots$get(key = variables[i])
  plotImage$setState(plotData)
}
```

### Data Validation Before State Setting

```R
# Validate data before setting state
validateData <- function(data) {
  if (is.null(data)) {
    return(list(valid = FALSE, message = "No data provided"))
  }
  
  if (nrow(data) == 0) {
    return(list(valid = FALSE, message = "Empty dataset"))
  }
  
  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE, 
      message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    ))
  }
  
  return(list(valid = TRUE, message = "Data valid"))
}

# Use in .run() function
validation <- validateData(processedData)
if (validation$valid) {
  self$results$plot$setState(plotData)
} else {
  # Handle invalid data
  self$results$plot$setState(NULL)
  warning(validation$message)
}
```

## 4. Plot Function Implementation

### Standard Function Signature

All jamovi plot functions follow this signature:

```R
.plotFunction = function(image, ggtheme, theme, ...) {
  # Function implementation
}
```

### Parameter Details

- **`image`**: The plot object containing state and metadata
- **`ggtheme`**: jamovi's ggplot2 theme for consistent styling
- **`theme`**: Additional theme elements (may be deprecated)
- **`...`**: Additional parameters for future extensibility

### Basic Plot Function Structure

```R
.plotExample = function(image, ggtheme, theme, ...) {
  # 1. State Retrieval and Validation
  plotData <- image$state
  
  if (is.null(plotData)) {
    return(FALSE)
  }
  
  # 2. Data Extraction
  data <- plotData$data
  config <- plotData$config
  
  # 3. Plot Creation
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x_var, y = y_var)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = config$title,
      x = config$xlab,
      y = config$ylab
    ) +
    ggtheme
  
  # 4. Output
  print(plot)
  return(TRUE)
}
```

### Error Handling in Plot Functions

```R
.robustPlotFunction = function(image, ggtheme, theme, ...) {
  tryCatch({
    # Main plotting logic
    plotData <- image$state
    
    if (is.null(plotData)) {
      return(FALSE)
    }
    
    # Validate required components
    if (!"data" %in% names(plotData) || nrow(plotData$data) == 0) {
      return(FALSE)
    }
    
    # Create plot
    plot <- createPlot(plotData, ggtheme)
    
    print(plot)
    return(TRUE)
    
  }, error = function(e) {
    # Log error for debugging
    warning(paste("Plot generation failed:", e$message))
    return(FALSE)
  })
}
```

### Return Values

Plot functions should return:
- `TRUE`: Successful plot generation
- `FALSE`: Failed or skipped plot generation
- `NULL` or nothing: Equivalent to FALSE

## 5. Complete Examples

### Example 1: Survival Analysis Plot (survival.b.R)

#### .r.yaml Definition

```yaml
- name: plot
  title: '`Survival Plot - ${explanatory}`'
  type: Image
  width: 600
  height: 450
  renderFun: .plot
  visible: (sc)
  requiresData: true
  clearWith:
    - sc
    - explanatory
    - outcome
    - overalltime
```

#### .run() Function (Data Preparation)

```R
# In the .run() method
if (self$options$sc) {
  # Prepare survival data
  survivalData <- list(
    cleanData = processedData,
    name1time = self$options$overalltime,
    name2outcome = self$options$outcome,
    name3explanatory = self$options$explanatory,
    formula = constructedFormula
  )
  
  # Set plot state
  self$results$plot$setState(survivalData)
}
```

#### .plot() Function (Rendering)

```R
.plot = function(image, ggtheme, theme, ...) {
  # Retrieve state
  results <- image$state
  
  if (is.null(results)) {
    return(FALSE)
  }
  
  # Extract variables
  mytime <- results$name1time
  myoutcome <- results$name2outcome
  myfactor <- results$name3explanatory
  plotData <- results$cleanData
  
  # Convert time variable to numeric
  plotData[[mytime]] <- jmvcore::toNumeric(plotData[[mytime]])
  
  # Construct survival formula
  myformula <- paste('survival::Surv(', mytime, ',', myoutcome, ') ~ ', myfactor)
  myformula <- as.formula(myformula)
  
  # Create survival plot using finalfit
  plot <- plotData %>%
    finalfit::surv_plot(
      .data = .,
      dependent = myformula,
      explanatory = myfactor,
      xlab = paste0('Time in ', self$options$timetypeoutput),
      pval = self$options$pplot,
      legend = 'bottom',
      break.time.by = self$options$byplot,
      xlims = c(0, self$options$endplot),
      ylims = c(self$options$ybegin_plot, self$options$yend_plot),
      conf.int = self$options$ci95,
      risk.table = self$options$risktable,
      censor = self$options$censored,
      linetype = 'strata',
      median.line = self$options$medianline
    )
  
  # Apply jamovi theme
  plot <- plot + ggtheme
  
  print(plot)
  return(TRUE)
}
```

### Example 2: ROC Analysis Plot (psychopdaroc.b.R)

#### .r.yaml Definition

```yaml
- name: plotROC
  type: Array
  title: ROC Curves
  visible: (plotROC)
  template:
    type: Image
    width: 550
    height: 450
    renderFun: .plotROC
    clearWith:
      - dependentVars
      - classVar
      - positiveClass
      - smoothing
      - cleanPlot
```

#### .plotROC() Function Implementation

```R
.plotROC = function(image, ggtheme, theme, ...) {
  # Retrieve ROC data
  plotData <- data.frame(image$state)
  
  if (nrow(plotData) == 0) {
    return(FALSE)
  }
  
  # Determine plot type
  if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
    # Multiple variables in one plot
    plot <- ggplot2::ggplot(plotData,
                            ggplot2::aes(
                              x = 1 - specificity,
                              y = sensitivity,
                              color = var,
                              linetype = var
                            )) +
      ggplot2::geom_abline(intercept = 0, slope = 1, 
                          linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_line(size = 1) +
      ggplot2::scale_color_brewer(palette = "Set1") +
      ggplot2::scale_linetype_manual(
        values = rep(c("solid", "dashed", "dotted", "longdash"),
                     length.out = length(unique(plotData$var)))
      )
  } else {
    # Single variable plot
    plot <- ggplot2::ggplot(plotData,
                            ggplot2::aes(
                              x = 1 - specificity,
                              y = sensitivity
                            )) +
      ggplot2::geom_abline(intercept = 0, slope = 1, 
                          linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 0.5, 
                         alpha = ifelse(self$options$cleanPlot, 0, 0.7))
  }
  
  # Add common elements
  plot <- plot +
    ggplot2::xlab("1 - Specificity (False Positive Rate)") +
    ggplot2::ylab("Sensitivity (True Positive Rate)") +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
  
  # Apply theme based on options
  if (self$options$cleanPlot) {
    plot <- plot +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "black", fill = NA)
      )
    
    # Handle legend for combined plots
    if (self$options$combinePlots && length(unique(plotData$var)) > 1) {
      if (self$options$legendPosition == "none") {
        plot <- plot + ggplot2::theme(legend.position = "none")
      } else {
        plot <- plot + ggplot2::theme(
          legend.position = self$options$legendPosition,
          legend.title = ggplot2::element_blank(),
          legend.key = ggplot2::element_rect(fill = "white"),
          legend.background = ggplot2::element_rect(fill = "white", color = NA)
        )
      }
    }
  } else {
    # Use jamovi theme
    plot <- plot + ggtheme
  }
  
  print(plot)
  return(TRUE)
}
```

### Example 3: Forest Plot Implementation

From `jforestmodel.b.R`:

```R
.plot_forest = function(image, ggtheme, theme, ...) {
  # Get the state data
  state_data <- image$state
  
  if (is.null(state_data) || is.null(state_data$plot_data)) {
    return(FALSE)
  }
  
  tryCatch({
    plot_data <- state_data$plot_data
    
    # Create forest plot
    plot <- ggplot2::ggplot(plot_data, 
                           ggplot2::aes(y = factor(term, levels = rev(term)))) +
      ggplot2::geom_vline(xintercept = ifelse(state_data$log_scale, 0, 1), 
                         linetype = "dashed", alpha = 0.5) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = conf_low, xmax = conf_high),
        height = 0.3
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = estimate),
        size = 3
      ) +
      ggplot2::labs(
        y = "Terms",
        x = ifelse(state_data$log_scale, "Log Estimate", "Estimate")
      ) +
      ggtheme
    
    print(plot)
    return(TRUE)
    
  }, error = function(e) {
    warning(paste("Forest plot generation failed:", e$message))
    return(FALSE)
  })
}
```

## 6. Advanced Plot Types

### Multiple Plot Arrays

For analyses requiring multiple similar plots:

#### .r.yaml Array Definition

```yaml
- name: groupPlots
  type: Array
  title: Analysis by Groups
  visible: (enableGrouping)
  template:
    type: Image
    width: 600
    height: 400
    renderFun: .plotByGroup
```

#### Implementation with Array Handling

```R
# In .run() - populate multiple plots
if (self$options$enableGrouping) {
  groups <- unique(data[[self$options$groupVar]])
  
  for (group in groups) {
    groupData <- data[data[[self$options$groupVar]] == group, ]
    
    plotState <- list(
      data = groupData,
      group = group,
      title = paste("Analysis for", group)
    )
    
    # Get or create plot for this group
    groupPlot <- self$results$groupPlots$get(key = group)
    groupPlot$setState(plotState)
  }
}

# Plot function handles individual groups
.plotByGroup = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  plot <- ggplot2::ggplot(state$data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = state$title) +
    ggtheme
  
  print(plot)
  return(TRUE)
}
```

### Interactive Plots

For specialized interactive visualizations:

```R
.plotInteractive = function(image, ggtheme, theme, ...) {
  # Create base ggplot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggtheme
  
  # Convert to interactive plot if package available
  if (requireNamespace("plotly", quietly = TRUE)) {
    plot <- plotly::ggplotly(plot)
  }
  
  print(plot)
  return(TRUE)
}
```

### Conditional Plot Types

Adapt plots based on data characteristics:

```R
.adaptivePlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  data <- state$data
  
  # Choose plot type based on data
  if (nrow(data) < 100) {
    # Small dataset - show all points
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_smooth(method = "lm", se = TRUE)
  } else {
    # Large dataset - use density or binning
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::stat_density_2d_filled(alpha = 0.5) +
      ggplot2::geom_smooth(method = "loess", color = "red")
  }
  
  plot <- plot + ggtheme
  print(plot)
  return(TRUE)
}
```

## 7. Theme Integration and Styling

### Understanding jamovi Themes

The `ggtheme` parameter contains jamovi's standardized styling:

```R
# Basic theme application
plot <- plot + ggtheme

# Combining with custom themes
plot <- plot + 
  ggtheme +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold"),
    axis.text = ggplot2::element_text(size = 10)
  )
```

### Custom Styling Options

#### Option-Based Theming

```R
.themedPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  data <- state$data
  
  # Base plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point()
  
  # Apply conditional theming
  if (self$options$cleanPlot) {
    plot <- plot +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_blank()
      )
  } else {
    plot <- plot + ggtheme
  }
  
  # Color scheme based on options
  if (self$options$colorScheme == "viridis") {
    plot <- plot + ggplot2::scale_color_viridis_d()
  } else if (self$options$colorScheme == "brewer") {
    plot <- plot + ggplot2::scale_color_brewer(palette = "Set1")
  }
  
  print(plot)
  return(TRUE)
}
```

### Color Management

#### Consistent Color Schemes

```R
# Define color palette function
getColorPalette <- function(n, scheme = "default") {
  switch(scheme,
    "clinical" = RColorBrewer::brewer.pal(min(n, 9), "Set1"),
    "viridis" = viridis::viridis(n),
    "grayscale" = gray.colors(n),
    "default" = ggplot2::scale_color_hue()$palette(n)
  )
}

# Use in plot function
plot <- plot + ggplot2::scale_color_manual(
  values = getColorPalette(nGroups, self$options$colorScheme)
)
```

## 8. Error Handling and Validation

### Comprehensive Error Handling

```R
.robustPlotFunction = function(image, ggtheme, theme, ...) {
  tryCatch({
    # 1. State validation
    state <- image$state
    if (is.null(state)) {
      return(FALSE)
    }
    
    # 2. Data validation
    if (!"data" %in% names(state) || is.null(state$data)) {
      warning("No data found in plot state")
      return(FALSE)
    }
    
    data <- state$data
    
    # 3. Check data dimensions
    if (nrow(data) == 0) {
      warning("Empty dataset provided to plot function")
      return(FALSE)
    }
    
    # 4. Validate required columns
    required_cols <- c("x", "y")
    missing_cols <- setdiff(required_cols, names(data))
    
    if (length(missing_cols) > 0) {
      warning(paste("Missing required columns:", 
                   paste(missing_cols, collapse = ", ")))
      return(FALSE)
    }
    
    # 5. Check for valid data
    if (all(is.na(data$x)) || all(is.na(data$y))) {
      warning("All data values are NA")
      return(FALSE)
    }
    
    # 6. Create plot with error handling
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggtheme
    
    # 7. Validate plot object
    if (is.null(plot)) {
      warning("Plot object creation failed")
      return(FALSE)
    }
    
    print(plot)
    return(TRUE)
    
  }, error = function(e) {
    # Log detailed error information
    error_msg <- paste("Plot generation error:", e$message)
    warning(error_msg)
    
    # Optional: Log to file for debugging
    # cat(paste(Sys.time(), error_msg, "\n"), 
    #     file = "plot_errors.log", append = TRUE)
    
    return(FALSE)
  }, warning = function(w) {
    # Handle warnings
    warning(paste("Plot warning:", w$message))
    return(FALSE)
  })
}
```

### Input Validation Helpers

```R
# Validation helper functions
validateNumericVariable <- function(data, var_name) {
  if (!var_name %in% names(data)) {
    return(list(valid = FALSE, message = paste("Variable", var_name, "not found")))
  }
  
  var_data <- data[[var_name]]
  
  if (!is.numeric(var_data)) {
    return(list(valid = FALSE, message = paste("Variable", var_name, "is not numeric")))
  }
  
  if (all(is.na(var_data))) {
    return(list(valid = FALSE, message = paste("Variable", var_name, "contains only NA values")))
  }
  
  return(list(valid = TRUE, message = "Valid"))
}

validateFactorVariable <- function(data, var_name, min_levels = 2) {
  if (!var_name %in% names(data)) {
    return(list(valid = FALSE, message = paste("Variable", var_name, "not found")))
  }
  
  var_data <- data[[var_name]]
  levels_count <- length(unique(var_data[!is.na(var_data)]))
  
  if (levels_count < min_levels) {
    return(list(
      valid = FALSE, 
      message = paste("Variable", var_name, "has only", levels_count, "levels")
    ))
  }
  
  return(list(valid = TRUE, message = "Valid"))
}
```

## 9. Performance Optimization

### Efficient Data Processing

```R
# Cache expensive computations
.optimizedPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  # Check if we have cached results
  if (!is.null(state$cached_plot) && !state$data_changed) {
    print(state$cached_plot)
    return(TRUE)
  }
  
  # Perform expensive computation
  processed_data <- expensiveDataProcessing(state$data)
  
  # Create plot
  plot <- ggplot2::ggplot(processed_data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggtheme
  
  # Cache the result
  state$cached_plot <- plot
  state$data_changed <- FALSE
  image$setState(state)
  
  print(plot)
  return(TRUE)
}
```

### Memory Management

```R
# Clean up large objects
.memoryEfficientPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  # Process data in chunks for large datasets
  data <- state$data
  
  if (nrow(data) > 10000) {
    # Sample data for visualization
    sample_size <- min(5000, nrow(data))
    data <- data[sample(nrow(data), sample_size), ]
    
    # Add note about sampling
    plot_subtitle <- paste("Showing", sample_size, "of", nrow(state$data), "observations")
  } else {
    plot_subtitle <- NULL
  }
  
  # Create plot
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::labs(subtitle = plot_subtitle) +
    ggtheme
  
  # Clean up
  rm(data)
  gc()
  
  print(plot)
  return(TRUE)
}
```

## 10. Clinical Applications

### Survival Analysis Visualization

```R
# Clinical survival plot with risk tables
.clinicalSurvivalPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  # Clinical-specific formatting
  plot <- state$data %>%
    finalfit::surv_plot(
      dependent = state$formula,
      explanatory = state$explanatory,
      xlab = "Time (months)",
      ylab = "Overall Survival Probability",
      pval = TRUE,
      pval.coord = c(0.1, 0.1),
      legend.title = "Treatment Group",
      legend.labs = state$group_labels,
      risk.table = TRUE,
      risk.table.title = "Number at risk",
      conf.int = TRUE,
      conf.int.alpha = 0.2,
      censor = TRUE,
      censor.shape = "+",
      censor.size = 4
    )
  
  # Clinical styling
  plot <- plot +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.title = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 11, face = "bold")
    ) +
    ggtheme
  
  print(plot)
  return(TRUE)
}
```

### Diagnostic Performance Visualization

```R
# ROC curves for diagnostic tests
.diagnosticROCPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  # Multi-biomarker ROC comparison
  plot <- ggplot2::ggplot(state$roc_data, 
                         ggplot2::aes(x = 1 - specificity, 
                                     y = sensitivity, 
                                     color = biomarker)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, 
                        linetype = "dashed", alpha = 0.6) +
    ggplot2::scale_x_continuous(
      "1 - Specificity (False Positive Rate)",
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    ggplot2::scale_y_continuous(
      "Sensitivity (True Positive Rate)",
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      title = "Diagnostic Performance Comparison",
      subtitle = paste("AUC values:", paste(state$auc_text, collapse = "; ")),
      color = "Biomarker"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggtheme
  
  print(plot)
  return(TRUE)
}
```

### Clinical Trial Visualization

```R
# Waterfall plot for treatment response
.waterfallPlot = function(image, ggtheme, theme, ...) {
  state <- image$state
  
  if (is.null(state)) return(FALSE)
  
  data <- state$data
  
  # Sort by response
  data <- data[order(data$percent_change), ]
  data$patient_id <- factor(data$patient_id, levels = data$patient_id)
  
  # Color by response type
  data$response_color <- ifelse(data$percent_change <= -30, "Complete/Partial Response",
                               ifelse(data$percent_change <= 20, "Stable Disease",
                                     "Progressive Disease"))
  
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = patient_id, 
                                            y = percent_change,
                                            fill = response_color)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = c(-30, 20), 
                       linetype = "dashed", alpha = 0.7) +
    ggplot2::scale_fill_manual(
      values = c("Complete/Partial Response" = "#2E8B57",
                "Stable Disease" = "#FFD700",
                "Progressive Disease" = "#DC143C")
    ) +
    ggplot2::labs(
      x = "Patient",
      y = "Best % Change from Baseline",
      title = "Treatment Response Waterfall Plot",
      fill = "Response Category"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom"
    ) +
    ggtheme
  
  print(plot)
  return(TRUE)
}
```

## 11. Best Practices

### Code Organization

1. **Consistent Naming**: Use descriptive function names with `.plot` prefix
2. **State Structure**: Maintain consistent state object structure
3. **Error Handling**: Always implement comprehensive error checking
4. **Documentation**: Include clear comments and parameter descriptions
5. **Theme Integration**: Always apply jamovi themes for consistency

### Performance Guidelines

1. **Data Validation**: Validate inputs early and fail fast
2. **Memory Management**: Clean up large objects when done
3. **Caching**: Cache expensive computations when possible
4. **Sampling**: Use data sampling for large datasets in visualization

### Accessibility and Usability

1. **Color Schemes**: Use colorblind-friendly palettes
2. **Font Sizes**: Ensure readability across different screen sizes
3. **Legend Placement**: Position legends for optimal space usage
4. **Error Messages**: Provide helpful error messages to users

### Testing and Validation

```R
# Test helper function
testPlotFunction <- function(plot_function, test_data) {
  # Create mock image object
  mock_image <- list(state = test_data)
  
  # Create mock theme
  mock_theme <- ggplot2::theme_minimal()
  
  # Test plot generation
  result <- tryCatch({
    plot_function(mock_image, mock_theme, NULL)
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
  
  return(result)
}
```

## 12. Troubleshooting Guide

### Common Issues and Solutions

#### Issue: Plot Not Appearing

**Possible Causes:**
- `visible` condition not met
- `requiresData` is true but no data available
- Plot function returning FALSE
- Error in plot generation

**Solutions:**
```R
# Debug visibility
cat("Plot visible:", self$options$showPlot, "\n")

# Check data availability
cat("Data rows:", nrow(plotData), "\n")

# Add debug output to plot function
.plotDebug = function(image, ggtheme, theme, ...) {
  cat("Plot function called\n")
  
  state <- image$state
  cat("State is null:", is.null(state), "\n")
  
  if (!is.null(state)) {
    cat("State contains:", names(state), "\n")
  }
  
  # ... rest of plot function
}
```

#### Issue: Theme Not Applied

**Problem:** jamovi theme not properly integrated

**Solution:**
```R
# Correct theme application
plot <- plot + ggtheme

# Not: plot + theme (deprecated parameter)
```

#### Issue: Dynamic Titles Not Working

**Problem:** Variables in title not being replaced

**Check:**
- Variable names match exactly
- Variables are available in current scope
- Syntax is correct: `${variableName}`

#### Issue: Memory Issues with Large Plots

**Solutions:**
```R
# Sample large datasets
if (nrow(data) > 10000) {
  data <- data[sample(nrow(data), 5000), ]
}

# Use efficient geoms
# Instead of geom_point() for many points:
ggplot2::stat_density_2d_filled()
# or
ggplot2::geom_hex()
```

#### Issue: State Not Persisting

**Problem:** Plot state not maintained between renders

**Check:**
- State being set in `.run()` function
- Clear conditions not too aggressive
- State object structure consistent

### Debugging Techniques

```R
# Add debug logging
.debugPlot = function(image, ggtheme, theme, ...) {
  debug_info <- list(
    timestamp = Sys.time(),
    state_available = !is.null(image$state),
    theme_available = !is.null(ggtheme)
  )
  
  if (!is.null(image$state)) {
    debug_info$state_structure <- str(image$state)
    debug_info$data_rows <- nrow(image$state$data)
  }
  
  # Log to console (remove in production)
  cat("DEBUG:", jsonlite::toJSON(debug_info, auto_unbox = TRUE), "\n")
  
  # Continue with normal plot generation
  # ...
}
```

This comprehensive guide provides the foundation for creating sophisticated, reliable, and user-friendly plots in jamovi modules. By following these patterns and best practices, developers can create visualizations that enhance the analytical capabilities of their jamovi modules while maintaining consistency with the jamovi ecosystem.
