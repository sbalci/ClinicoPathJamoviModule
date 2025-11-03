# A Comprehensive Guide to Creating and Using Actions in jamovi

This document provides a comprehensive guide to implementing actions in jamovi module development. Actions enable analyses to open new datasets in new jamovi windows (or tabs in the browser), providing powerful workflow capabilities for data export, transformation, and interactive analysis pipelines.

## Table of Contents

1. [Introduction: Architecture and Workflow](#1-introduction-architecture-and-workflow)
2. [Action Definition in .a.yaml Files](#2-action-definition-in-ayaml-files)
3. [Action Implementation in .b.R Files](#3-action-implementation-in-br-files)
4. [Complete Examples](#4-complete-examples)
5. [Advanced Action Patterns](#5-advanced-action-patterns)
6. [Error Handling and Validation](#6-error-handling-and-validation)
7. [Version Compatibility](#7-version-compatibility)
8. [Clinical Applications](#8-clinical-applications)
9. [Best Practices](#9-best-practices)
10. [Troubleshooting Guide](#10-troubleshooting-guide)

## 1. Introduction: Architecture and Workflow

### What are Actions?

Actions in jamovi allow an analysis to:

- Open new datasets in new jamovi windows/tabs
- Export transformed or filtered data
- Create derived datasets from analysis results
- Enable interactive data workflows
- Facilitate multi-stage analysis pipelines

### Key Capabilities

Actions enable you to:

1. **Data Export**: Export filtered, transformed, or analyzed data
2. **Result Extraction**: Convert analysis results into new datasets
3. **Workflow Integration**: Chain multiple analyses together
4. **Interactive Analysis**: Let users explore subsets or transformations
5. **Format Conversion**: Export data in various formats (CSV, Excel, etc.)

### The Two-Stage Action Process

1. **Definition Stage (`.a.yaml`)**: Define action button and metadata
2. **Execution Stage (`.b.R` - `.run()`)**: Implement action logic and data handling

### jamovi Action Architecture

```mermaid
graph TD
    A[.a.yaml Action Definition] --> B[UI Action Button]
    B --> C[User Clicks Button]
    C --> D[.run() Execution]
    D --> E{Action Type}
    E -->|Data Frame| F[Return data + title]
    E -->|File Export| G[Write file + return path]
    F --> H[New jamovi Window]
    G --> H
    H --> I[User Interacts with New Data]
```

### Version Requirements

- **Minimum Version**: jamovi 2.7.12 or newer
- **API Type**: Proper action API (replaces legacy hackish implementations)
- **Platform Support**: Desktop version (cloud requires additional setup)

## 2. Action Definition in .a.yaml Files

### Basic Action Structure

Every action in jamovi is defined as an `Action` type option in the `.a.yaml` file:

```yaml
- name: open
  title: Open
  type: Action
  action: open
```

### Essential Properties

#### Core Properties

| Property | Type | Description | Required |
|----------|------|-------------|----------|
| `name` | string | Unique identifier for the action | Yes |
| `title` | string | Button text displayed in UI | Yes |
| `type` | string | Must be "Action" for actions | Yes |
| `action` | string | Action type (currently only "open") | Yes |

### Supported Action Types

Currently, jamovi supports:

- **`open`**: Opens a new dataset in a new window/tab

Future versions may support additional action types.

### Basic Example

```yaml
# Simple action definition
- name: exportFiltered
  title: Export Filtered Data
  type: Action
  action: open

# Analysis action definition
- name: openResults
  title: Open Results as Data
  type: Action
  action: open
```

### Multiple Actions in One Analysis

You can define multiple actions for different purposes:

```yaml
# In .a.yaml file
options:
  - name: exportRaw
    title: Export Raw Data
    type: Action
    action: open

  - name: exportSummary
    title: Export Summary Statistics
    type: Action
    action: open

  - name: exportPlotData
    title: Export Plot Data
    type: Action
    action: open
```

### Conditional Actions

Actions can be conditionally enabled based on other options:

```yaml
- name: exportFiltered
  title: Export Filtered Data
  type: Action
  action: open
  # Note: Conditional visibility would be handled in .u.yaml
```

## 3. Action Implementation in .b.R Files

### Standard Implementation Pattern

All jamovi actions follow this implementation pattern in the `.run()` method:

```R
.run = function() {

    # Check if action was triggered
    if (self$options$actionName) {

        # 1. Retrieve the option
        option <- self$options$option('actionName')

        # 2. Check version compatibility
        if (is.null(option$perform)) {
            # Action system not supported
            return()
        }

        # 3. Perform the action
        option$perform(function(action) {

            # 4. Prepare data or file
            # ... your logic here ...

            # 5. Return result
            return(result)
        })
    }
}
```

### Action Callback Function

The `option$perform()` method takes a callback function with one parameter:

- **`action`**: Action context object containing parameters

### Return Value Types

#### 5a. Data Frame Return (In-Memory)

For returning data directly to jamovi:

```R
option$perform(function(action) {

    # Prepare your data frame
    exportData <- prepareDataFrame()

    # Return as list
    list(
        data = exportData,
        title = 'Filtered Patient Data'
    )
})
```

**Required fields:**
- `data`: Data frame to open
- `title`: Title for the new window/dataset

#### 5b. File Export Return

For writing data to a file first:

```R
option$perform(function(action) {

    # Write file to the provided path
    # IMPORTANT: Use action$params$fullPath for writing
    write.csv(exportData,
              file = action$params$fullPath,
              row.names = FALSE)

    # Return path information
    # IMPORTANT: Use action$params$path (not $fullPath) for return
    list(
        path = action$params$path,
        title = 'Exported Analysis Results',
        ext = 'csv'
    )
})
```

**Required fields:**
- `path`: Use `action$params$path` (not `$fullPath`)
- `title`: Title for the new window/dataset
- `ext`: File extension ('csv', 'xlsx', etc.)

**Critical Path Handling:**

- **Write to**: `action$params$fullPath`
- **Return**: `action$params$path`

#### 5c. Error Handling

Throw errors when action cannot be completed:

```R
option$perform(function(action) {

    # Check conditions
    if (!conditionsMet) {
        stop("Cannot export: No data available")
    }

    if (nrow(data) == 0) {
        stop("Cannot export: Dataset is empty")
    }

    # ... proceed with export ...
})
```

### Version Compatibility Check

Always check if the action system is available:

```R
if (self$options$actionName) {

    option <- self$options$option('actionName')

    # Version check
    if (is.null(option$perform)) {
        # Action system not available
        warning("Action system requires jamovi 2.7.12 or newer")
        return()
    }

    # Proceed with action
    option$perform(function(action) {
        # ... implementation ...
    })
}
```

## 4. Complete Examples

### Example 1: Export Filtered Data

A complete example of exporting filtered data based on user criteria.

#### .a.yaml Definition

```yaml
name: FilteredExport
title: Filtered Data Export
jrs: '1.1'

options:
  - name: filterVar
    title: Filter Variable
    type: Variable

  - name: filterValue
    title: Filter Value
    type: String

  - name: exportFiltered
    title: Export Filtered Data
    type: Action
    action: open
```

#### .b.R Implementation

```R
filteredExportClass <- R6::R6Class(
    "filteredExportClass",
    inherit = filteredExportBase,
    private = list(
        .run = function() {

            # Check if export action triggered
            if (self$options$exportFiltered) {

                # Get the action option
                option <- self$options$option('exportFiltered')

                # Check version compatibility
                if (is.null(option$perform)) {
                    jmvcore::reject(
                        "Export functionality requires jamovi 2.7.12 or newer"
                    )
                    return()
                }

                # Perform the action
                option$perform(function(action) {

                    # Get data
                    data <- self$data

                    # Validate filter settings
                    if (is.null(self$options$filterVar)) {
                        stop("Please select a filter variable")
                    }

                    if (self$options$filterValue == "") {
                        stop("Please enter a filter value")
                    }

                    # Apply filter
                    filterVar <- self$options$filterVar
                    filterValue <- self$options$filterValue

                    filteredData <- data[data[[filterVar]] == filterValue, ]

                    # Check if any data remains
                    if (nrow(filteredData) == 0) {
                        stop(paste(
                            "No data matches filter:",
                            filterVar, "=", filterValue
                        ))
                    }

                    # Prepare export
                    exportDF <- as.data.frame(filteredData)

                    # Return data frame
                    list(
                        data = exportDF,
                        title = paste("Filtered:", filterVar, "=", filterValue)
                    )
                })
            }
        }
    )
)
```

### Example 2: Export Analysis Results

Export statistical analysis results as a new dataset.

#### .a.yaml Definition

```yaml
name: SurvivalExport
title: Survival Analysis Export
jrs: '1.1'

options:
  - name: time
    title: Time Variable
    type: Variable

  - name: event
    title: Event Variable
    type: Variable

  - name: group
    title: Grouping Variable
    type: Variable

  - name: exportSummary
    title: Export Summary Statistics
    type: Action
    action: open

  - name: exportEvents
    title: Export Event Data
    type: Action
    action: open
```

#### .b.R Implementation

```R
.run = function() {

    # Export summary statistics
    if (self$options$exportSummary) {

        option <- self$options$option('exportSummary')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Fit survival model
                survFit <- survival::survfit(
                    survival::Surv(time, event) ~ group,
                    data = self$data
                )

                # Extract summary
                summaryData <- summary(survFit)

                # Create data frame
                resultDF <- data.frame(
                    Time = summaryData$time,
                    N_Risk = summaryData$n.risk,
                    N_Event = summaryData$n.event,
                    Survival = summaryData$surv,
                    Lower_CI = summaryData$lower,
                    Upper_CI = summaryData$upper,
                    Group = summaryData$strata
                )

                # Return
                list(
                    data = resultDF,
                    title = "Survival Analysis Summary"
                )
            })
        }
    }

    # Export event data
    if (self$options$exportEvents) {

        option <- self$options$option('exportEvents')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Prepare event data
                eventData <- self$data[self$data[[self$options$event]] == 1, ]

                if (nrow(eventData) == 0) {
                    stop("No events observed in dataset")
                }

                # Return
                list(
                    data = as.data.frame(eventData),
                    title = "Event Cases Only"
                )
            })
        }
    }
}
```

### Example 3: Export to CSV File

Export data with custom formatting to a CSV file.

#### .b.R Implementation

```R
.run = function() {

    if (self$options$exportToCSV) {

        option <- self$options$option('exportToCSV')

        if (is.null(option$perform)) {
            return()
        }

        option$perform(function(action) {

            # Prepare data
            exportData <- prepareExportData()

            # Validate data
            if (is.null(exportData) || nrow(exportData) == 0) {
                stop("No data available for export")
            }

            # Write to CSV using the full path
            tryCatch({
                write.csv(
                    exportData,
                    file = action$params$fullPath,
                    row.names = FALSE,
                    na = ""
                )
            }, error = function(e) {
                stop(paste("Failed to write CSV file:", e$message))
            })

            # Return path (use path, not fullPath)
            list(
                path = action$params$path,
                title = "Exported Data",
                ext = "csv"
            )
        })
    }
}

# Helper function
prepareExportData <- function() {
    # Data preparation logic
    exportDF <- data.frame(
        ID = 1:100,
        Value = rnorm(100),
        Category = sample(c("A", "B", "C"), 100, replace = TRUE)
    )

    return(exportDF)
}
```

### Example 4: Export with Excel Formatting

Export to Excel with multiple sheets and formatting.

#### .b.R Implementation

```R
.run = function() {

    if (self$options$exportToExcel) {

        option <- self$options$option('exportToExcel')

        if (is.null(option$perform)) {
            return()
        }

        option$perform(function(action) {

            # Check if package available
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
                stop("Package 'openxlsx' required for Excel export")
            }

            # Create workbook
            wb <- openxlsx::createWorkbook()

            # Add data sheet
            openxlsx::addWorksheet(wb, "Data")
            openxlsx::writeData(wb, "Data", exportData)

            # Add summary sheet
            openxlsx::addWorksheet(wb, "Summary")
            openxlsx::writeData(wb, "Summary", summaryStats)

            # Apply formatting
            openxlsx::addStyle(
                wb,
                sheet = "Data",
                style = openxlsx::createStyle(
                    fontSize = 12,
                    fontColour = "#000000",
                    halign = "center"
                ),
                rows = 1,
                cols = 1:ncol(exportData),
                gridExpand = TRUE
            )

            # Save workbook
            tryCatch({
                openxlsx::saveWorkbook(
                    wb,
                    file = action$params$fullPath,
                    overwrite = TRUE
                )
            }, error = function(e) {
                stop(paste("Failed to write Excel file:", e$message))
            })

            # Return
            list(
                path = action$params$path,
                title = "Analysis Results",
                ext = "xlsx"
            )
        })
    }
}
```

## 5. Advanced Action Patterns

### Multiple Related Actions

Organize multiple export options logically:

```R
.run = function() {

    # Helper to handle actions
    handleAction <- function(optionName, prepareFunc, title) {
        if (self$options[[optionName]]) {
            option <- self$options$option(optionName)

            if (!is.null(option$perform)) {
                option$perform(function(action) {
                    data <- prepareFunc()
                    list(data = data, title = title)
                })
            }
        }
    }

    # Export raw data
    handleAction(
        "exportRaw",
        function() as.data.frame(self$data),
        "Raw Data"
    )

    # Export filtered data
    handleAction(
        "exportFiltered",
        function() filterData(self$data, self$options),
        "Filtered Data"
    )

    # Export summary
    handleAction(
        "exportSummary",
        function() createSummary(self$data, self$options),
        "Summary Statistics"
    )
}
```

### Conditional Data Preparation

Adapt export based on analysis state:

```R
.run = function() {

    if (self$options$exportResults) {

        option <- self$options$option('exportResults')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Check analysis status
                if (!self$results$complete) {
                    stop("Please complete analysis before exporting")
                }

                # Determine export type based on options
                if (self$options$analysisType == "survival") {
                    exportData <- prepareSurvivalData()
                } else if (self$options$analysisType == "regression") {
                    exportData <- prepareRegressionData()
                } else {
                    exportData <- prepareGenericData()
                }

                # Add metadata
                attr(exportData, "analysis_date") <- Sys.Date()
                attr(exportData, "analysis_type") <- self$options$analysisType

                # Return
                list(
                    data = exportData,
                    title = paste(
                        self$options$analysisType,
                        "Analysis Results"
                    )
                )
            })
        }
    }
}
```

### Progressive Data Export

Export with data transformation pipeline:

```R
.run = function() {

    if (self$options$exportTransformed) {

        option <- self$options$option('exportTransformed')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Start with original data
                data <- self$data

                # Apply transformations progressively
                if (self$options$removeNA) {
                    data <- na.omit(data)
                }

                if (self$options$standardize) {
                    numericCols <- sapply(data, is.numeric)
                    data[numericCols] <- scale(data[numericCols])
                }

                if (self$options$filterOutliers) {
                    data <- removeOutliers(data, self$options$outlierSD)
                }

                # Add transformation log
                transformLog <- data.frame(
                    Transformation = c(
                        "Remove NA",
                        "Standardize",
                        "Filter Outliers"
                    ),
                    Applied = c(
                        self$options$removeNA,
                        self$options$standardize,
                        self$options$filterOutliers
                    ),
                    Rows_Affected = c(
                        originalRows - nrow(data),
                        ifelse(self$options$standardize, nrow(data), 0),
                        outlierCount
                    )
                )

                # Return both data and log
                list(
                    data = data,
                    title = "Transformed Data"
                )
            })
        }
    }
}
```

### Format-Specific Exports

Handle different file formats:

```R
.run = function() {

    # Common export preparation
    prepareExport <- function() {
        # Shared data preparation logic
        data <- self$data
        # ... transformations ...
        return(data)
    }

    # CSV Export
    if (self$options$exportCSV) {
        option <- self$options$option('exportCSV')
        if (!is.null(option$perform)) {
            option$perform(function(action) {
                data <- prepareExport()
                write.csv(data, action$params$fullPath, row.names = FALSE)
                list(
                    path = action$params$path,
                    title = "Data Export",
                    ext = "csv"
                )
            })
        }
    }

    # Excel Export
    if (self$options$exportXLSX) {
        option <- self$options$option('exportXLSX')
        if (!is.null(option$perform)) {
            option$perform(function(action) {
                data <- prepareExport()
                openxlsx::write.xlsx(data, action$params$fullPath)
                list(
                    path = action$params$path,
                    title = "Data Export",
                    ext = "xlsx"
                )
            })
        }
    }

    # RDS Export (R native format)
    if (self$options$exportRDS) {
        option <- self$options$option('exportRDS')
        if (!is.null(option$perform)) {
            option$perform(function(action) {
                data <- prepareExport()
                saveRDS(data, action$params$fullPath)
                list(
                    path = action$params$path,
                    title = "Data Export",
                    ext = "rds"
                )
            })
        }
    }
}
```

## 6. Error Handling and Validation

### Comprehensive Error Handling

```R
.run = function() {

    if (self$options$exportAction) {

        option <- self$options$option('exportAction')

        # Version check with user feedback
        if (is.null(option$perform)) {
            jmvcore::reject(
                "Export functionality requires jamovi 2.7.12 or newer. ",
                "Please update jamovi to use this feature."
            )
            return()
        }

        option$perform(function(action) {

            tryCatch({

                # Validate prerequisites
                validateExportConditions()

                # Prepare data
                exportData <- prepareExportData()

                # Validate output
                validateExportData(exportData)

                # Return success
                list(
                    data = exportData,
                    title = "Exported Data"
                )

            }, error = function(e) {
                # Provide helpful error message
                stop(paste(
                    "Export failed:",
                    e$message,
                    "\nPlease check your settings and try again."
                ))
            })
        })
    }
}

# Validation helpers
validateExportConditions <- function() {
    if (is.null(self$options$requiredVar)) {
        stop("Please select required variable")
    }

    if (nrow(self$data) == 0) {
        stop("Dataset is empty")
    }
}

validateExportData <- function(data) {
    if (is.null(data)) {
        stop("Export data preparation failed")
    }

    if (nrow(data) == 0) {
        stop("No data to export after filtering")
    }

    if (ncol(data) == 0) {
        stop("No columns to export")
    }
}
```

### Input Validation

```R
validateInputs <- function() {
    errors <- character()

    # Check required variables
    if (is.null(self$options$var1)) {
        errors <- c(errors, "Variable 1 is required")
    }

    # Check numeric constraints
    if (self$options$threshold < 0) {
        errors <- c(errors, "Threshold must be positive")
    }

    # Check logical constraints
    if (self$options$startDate > self$options$endDate) {
        errors <- c(errors, "Start date must be before end date")
    }

    # Report all errors
    if (length(errors) > 0) {
        stop(paste(
            "Validation errors:",
            paste(errors, collapse = "; ")
        ))
    }
}
```

### File Write Validation

```R
safeFileWrite <- function(data, filepath, format = "csv") {

    # Check write permissions
    if (!dir.exists(dirname(filepath))) {
        stop("Export directory does not exist")
    }

    # Attempt write with error handling
    tryCatch({
        switch(format,
            "csv" = write.csv(data, filepath, row.names = FALSE),
            "xlsx" = openxlsx::write.xlsx(data, filepath),
            "rds" = saveRDS(data, filepath),
            stop(paste("Unsupported format:", format))
        )
    }, error = function(e) {
        stop(paste(
            "Failed to write file:",
            e$message,
            "\nPlease check file permissions and disk space."
        ))
    })

    # Verify file was created
    if (!file.exists(filepath)) {
        stop("File creation verification failed")
    }

    return(TRUE)
}
```

## 7. Version Compatibility

### Supporting Multiple jamovi Versions

To support both old and new jamovi versions:

#### .a.yaml Definition (Backward Compatible)

```yaml
# For jamovi < 2.7.12, omit the 'action' property
- name: export
  title: Export Data
  type: Action
  # action: open  # <-- Omit this for backward compatibility
```

#### .b.R Implementation (Dual Support)

```R
.run = function() {

    if (self$options$export) {

        # Try new action system (2.7.12+)
        option <- self$options$option('export')

        if (!is.null(option$perform)) {
            # New action system available
            option$perform(function(action) {
                list(
                    data = prepareData(),
                    title = "Exported Data"
                )
            })
        } else {
            # Fall back to legacy action system
            legacyExport()
        }
    }
}

# Legacy export implementation
legacyExport <- function() {
    # Your old, terrible, shameful action code here
    # This supports users on jamovi < 2.7.12

    warning(paste(
        "Using legacy export system.",
        "Update to jamovi 2.7.12+ for improved functionality."
    ))

    # ... legacy implementation ...
}
```

### Version Detection

```R
getJamoviVersion <- function() {
    # Attempt to detect jamovi version
    tryCatch({
        as.character(packageVersion("jmvcore"))
    }, error = function(e) {
        "unknown"
    })
}

checkActionSupport <- function(option) {
    if (is.null(option$perform)) {
        version <- getJamoviVersion()
        message <- paste(
            "Action system not available in current jamovi version:",
            version,
            "\nRequired: 2.7.12 or newer"
        )
        jmvcore::reject(message)
        return(FALSE)
    }
    return(TRUE)
}
```

### Migration Timeline

Based on jamovi development guidance:

```R
# Phase 1: Support both systems (Current - 2.8)
if (!is.null(option$perform)) {
    # Use new system
    option$perform(...)
} else {
    # Use legacy system
    legacyAction()
}

# Phase 2: Drop legacy support (2.8+)
if (is.null(option$perform)) {
    jmvcore::reject("This module requires jamovi 2.7.12 or newer")
    return()
}

option$perform(...)
```

## 8. Clinical Applications

### Example 1: Export Patient Subgroups

```R
.run = function() {

    if (self$options$exportSubgroup) {

        option <- self$options$option('exportSubgroup')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Define clinical subgroups
                data <- self$data

                # Filter based on clinical criteria
                subgroup <- data[
                    data$stage >= self$options$minStage &
                    data$age >= self$options$minAge &
                    data$treatment == self$options$treatment,
                ]

                # Validate subgroup size
                if (nrow(subgroup) < 10) {
                    stop(paste(
                        "Subgroup too small (n =", nrow(subgroup), ").",
                        "Minimum 10 patients required."
                    ))
                }

                # Add clinical annotations
                subgroup$export_date <- Sys.Date()
                subgroup$criteria <- paste(
                    "Stage >=", self$options$minStage,
                    "Age >=", self$options$minAge,
                    "Treatment:", self$options$treatment
                )

                # Return
                list(
                    data = subgroup,
                    title = paste(
                        "Patient Subgroup (n =",
                        nrow(subgroup),
                        ")"
                    )
                )
            })
        }
    }
}
```

### Example 2: Export Survival Analysis Results

```R
.run = function() {

    if (self$options$exportSurvival) {

        option <- self$options$option('exportSurvival')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Fit survival model
                survFit <- survival::survfit(
                    survival::Surv(time, event) ~ treatment,
                    data = self$data
                )

                # Extract detailed results
                survSummary <- summary(survFit, times = seq(0, 60, by = 6))

                # Create comprehensive results table
                resultsDF <- data.frame(
                    Time_Months = survSummary$time,
                    Treatment = survSummary$strata,
                    N_Risk = survSummary$n.risk,
                    N_Event = survSummary$n.event,
                    N_Censored = survSummary$n.censor,
                    Survival = survSummary$surv,
                    SE = survSummary$std.err,
                    Lower_95CI = survSummary$lower,
                    Upper_95CI = survSummary$upper
                )

                # Add median survival
                medians <- summary(survFit)$table
                attr(resultsDF, "median_survival") <- medians

                # Return
                list(
                    data = resultsDF,
                    title = "Survival Analysis Results (6-month intervals)"
                )
            })
        }
    }
}
```

### Example 3: Export ROC Analysis Data

```R
.run = function() {

    if (self$options$exportROC) {

        option <- self$options$option('exportROC')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Calculate ROC curve
                rocObj <- pROC::roc(
                    response = self$data[[self$options$outcome]],
                    predictor = self$data[[self$options$predictor]]
                )

                # Extract ROC data
                rocData <- data.frame(
                    Threshold = rocObj$thresholds,
                    Sensitivity = rocObj$sensitivities,
                    Specificity = rocObj$specificities,
                    PPV = calculatePPV(rocObj),
                    NPV = calculateNPV(rocObj)
                )

                # Add optimal cutpoint
                optimal <- coords(
                    rocObj,
                    x = "best",
                    ret = c("threshold", "sens", "spec", "ppv", "npv")
                )

                attr(rocData, "optimal_cutpoint") <- optimal
                attr(rocData, "auc") <- as.numeric(rocObj$auc)
                attr(rocData, "auc_ci") <- as.numeric(ci.auc(rocObj))

                # Return
                list(
                    data = rocData,
                    title = paste(
                        "ROC Analysis (AUC =",
                        round(rocObj$auc, 3),
                        ")"
                    )
                )
            })
        }
    }
}
```

### Example 4: Export Pathology Report Data

```R
.run = function() {

    if (self$options$exportReport) {

        option <- self$options$option('exportReport')

        if (!is.null(option$perform)) {

            option$perform(function(action) {

                # Prepare pathology report data
                reportData <- data.frame(
                    Patient_ID = self$data$id,
                    Diagnosis = self$data$diagnosis,
                    Grade = self$data$grade,
                    Stage = self$data$stage,
                    Tumor_Size = self$data$size,
                    Margins = self$data$margins,
                    Nodes_Positive = self$data$nodes_pos,
                    Nodes_Total = self$data$nodes_total,
                    IHC_ER = self$data$er_status,
                    IHC_PR = self$data$pr_status,
                    IHC_HER2 = self$data$her2_status,
                    Ki67_Percent = self$data$ki67
                )

                # Add calculated fields
                reportData$Nodes_Ratio <- with(
                    reportData,
                    Nodes_Positive / Nodes_Total
                )

                reportData$Risk_Category <- calculateRiskCategory(reportData)

                # Format for clinical use
                reportData <- formatPathologyReport(reportData)

                # Return
                list(
                    data = reportData,
                    title = paste(
                        "Pathology Report Export (n =",
                        nrow(reportData),
                        ")"
                    )
                )
            })
        }
    }
}
```

## 9. Best Practices

### Code Organization

#### 1. Consistent Naming

```R
# Use descriptive action names
exportFiltered     # Good
export1           # Bad

exportSurvival    # Good
doExport          # Bad
```

#### 2. Modular Helper Functions

```R
# Separate preparation from action logic
prepareExportData <- function(data, options) {
    # Data preparation logic
    # ...
    return(preparedData)
}

validateExport <- function(data) {
    # Validation logic
    # ...
    return(TRUE)
}

.run = function() {
    if (self$options$export) {
        option <- self$options$option('export')

        if (!is.null(option$perform)) {
            option$perform(function(action) {
                # Clean action implementation
                data <- prepareExportData(self$data, self$options)
                validateExport(data)

                list(data = data, title = "Export")
            })
        }
    }
}
```

#### 3. Error Messages

```R
# Provide helpful, actionable error messages

# Good
stop("Cannot export: No patients match criteria (Stage >= 3, Age >= 65)")

# Bad
stop("Export failed")

# Good
stop(paste(
    "Export requires at least 10 observations.",
    "Current selection has", nrow(data), "observations.",
    "Please adjust filter criteria."
))

# Bad
stop("Not enough data")
```

### Performance Guidelines

#### 1. Efficient Data Handling

```R
# Avoid unnecessary copies
# Bad
temp1 <- self$data
temp2 <- temp1[temp1$var > 0, ]
temp3 <- temp2[, c("a", "b", "c")]
exportData <- temp3

# Good
exportData <- self$data[self$data$var > 0, c("a", "b", "c")]
```

#### 2. Large Dataset Handling

```R
# For very large datasets
if (nrow(exportData) > 100000) {
    warning(paste(
        "Large dataset export (", nrow(exportData), "rows).",
        "This may take a few moments."
    ))

    # Consider chunked writing for very large files
    chunkSize <- 10000
    # ... chunked export logic ...
}
```

#### 3. Memory Management

```R
# Clean up large temporary objects
option$perform(function(action) {

    # Create large temporary object
    tempData <- performExpensiveOperation()

    # Use it
    exportData <- processData(tempData)

    # Clean up
    rm(tempData)
    gc()

    # Return
    list(data = exportData, title = "Export")
})
```

### User Experience

#### 1. Informative Titles

```R
# Include useful information in export titles

# Good
list(
    data = exportData,
    title = paste(
        "Filtered Patients (n =", nrow(exportData), ")",
        "- Stage III+, Age 65+"
    )
)

# Acceptable
list(
    data = exportData,
    title = "Filtered Patient Data"
)

# Bad
list(
    data = exportData,
    title = "Export"
)
```

#### 2. Progress Indication

```R
# For long-running exports
option$perform(function(action) {

    # Indicate start (in console/log)
    message("Preparing export...")

    # Perform operation
    data <- longRunningOperation()

    message("Export prepared successfully")

    # Return
    list(data = data, title = "Export")
})
```

#### 3. Validation Feedback

```R
# Validate before action button becomes active
.run = function() {

    # Set ready state for action
    if (self$options$export) {

        # Check if conditions are met
        canExport <- (
            !is.null(self$options$var1) &&
            !is.null(self$options$var2) &&
            nrow(self$data) > 0
        )

        if (!canExport) {
            # Provide feedback why export isn't ready
            jmvcore::reject("Please complete required settings before exporting")
            return()
        }

        # Proceed with export
        option <- self$options$option('export')
        # ... rest of implementation ...
    }
}
```

## 10. Troubleshooting Guide

### Common Issues and Solutions

#### Issue: Action Button Not Appearing

**Possible Causes:**
- Incorrect .a.yaml definition
- Type not set to "Action"
- jamovi version < 2.7.12

**Solutions:**

```yaml
# Check .a.yaml definition
- name: export
  title: Export Data
  type: Action    # Must be exactly "Action"
  action: open    # Must be exactly "open"
```

```R
# Add version check with user feedback
if (is.null(option$perform)) {
    jmvcore::reject(
        "Export requires jamovi 2.7.12 or newer. ",
        "Please update jamovi."
    )
    return()
}
```

#### Issue: Action Button Clicked But Nothing Happens

**Possible Causes:**
- No implementation in .run()
- Missing option$perform() call
- Silent error in callback

**Solutions:**

```R
# Add debug logging
.run = function() {
    cat("Run method called\n")

    if (self$options$export) {
        cat("Export option triggered\n")

        option <- self$options$option('export')
        cat("Option retrieved:", !is.null(option), "\n")

        if (!is.null(option$perform)) {
            cat("Performing action\n")

            option$perform(function(action) {
                cat("Inside callback\n")

                # ... your logic ...

                cat("Returning result\n")
                return(result)
            })
        }
    }
}
```

#### Issue: Export Succeeds But New Window Doesn't Open

**Possible Causes:**
- Return value format incorrect
- Missing required fields (data/path, title)
- Error thrown after return

**Solutions:**

```R
# Verify return format
option$perform(function(action) {

    # For data frame
    result <- list(
        data = exportData,    # Must be data.frame
        title = "Title"       # Must be string
    )

    # Validate before return
    stopifnot(is.data.frame(result$data))
    stopifnot(is.character(result$title))

    return(result)
})

# For file export
option$perform(function(action) {

    # Write file
    write.csv(data, action$params$fullPath, row.names = FALSE)

    result <- list(
        path = action$params$path,    # NOT fullPath
        title = "Title",
        ext = "csv"
    )

    return(result)
})
```

#### Issue: File Export Path Errors

**Problem:** Confusion between `fullPath` and `path`

**Solution:**

```R
# CORRECT usage
option$perform(function(action) {

    # WRITE using fullPath
    write.csv(data, file = action$params$fullPath, row.names = FALSE)

    # RETURN using path (not fullPath!)
    list(
        path = action$params$path,    # ← Use path
        title = "Export",
        ext = "csv"
    )
})

# INCORRECT - will fail
option$perform(function(action) {
    write.csv(data, file = action$params$path)  # ✗ Wrong
    list(
        path = action$params$fullPath,  # ✗ Wrong
        title = "Export",
        ext = "csv"
    )
})
```

#### Issue: Action Works in Desktop But Not Cloud

**Current Status:**
- Desktop: Full support in 2.7.12+
- Cloud: Requires additional implementation

**Solution:**

```R
# Detect environment
isCloudVersion <- function() {
    # Detection logic
    # This is environment-specific
    return(FALSE)  # Placeholder
}

.run = function() {
    if (self$options$export) {

        if (isCloudVersion()) {
            jmvcore::reject(
                "Export functionality not yet available in cloud version. ",
                "Please use desktop version."
            )
            return()
        }

        # Proceed with desktop implementation
        # ...
    }
}
```

### Debugging Techniques

#### Enable Verbose Logging

```R
.run = function() {

    # Debug flag (remove in production)
    DEBUG <- TRUE

    if (DEBUG) {
        cat("=== Export Debug Info ===\n")
        cat("Export option:", self$options$export, "\n")
        cat("Data rows:", nrow(self$data), "\n")
        cat("Data cols:", ncol(self$data), "\n")
    }

    if (self$options$export) {
        option <- self$options$option('export')

        if (DEBUG) {
            cat("Option type:", class(option), "\n")
            cat("Has perform:", !is.null(option$perform), "\n")
        }

        if (!is.null(option$perform)) {
            option$perform(function(action) {

                if (DEBUG) {
                    cat("Action params:\n")
                    str(action$params)
                }

                # ... implementation ...
            })
        }
    }
}
```

#### Test Return Values

```R
# Validation helper
validateActionResult <- function(result, type = "data") {

    if (type == "data") {
        checks <- c(
            "has_data" = "data" %in% names(result),
            "has_title" = "title" %in% names(result),
            "data_is_df" = is.data.frame(result$data),
            "title_is_char" = is.character(result$title),
            "data_has_rows" = nrow(result$data) > 0
        )
    } else if (type == "file") {
        checks <- c(
            "has_path" = "path" %in% names(result),
            "has_title" = "title" %in% names(result),
            "has_ext" = "ext" %in% names(result),
            "path_is_char" = is.character(result$path),
            "title_is_char" = is.character(result$title),
            "ext_is_char" = is.character(result$ext)
        )
    }

    failed <- names(checks)[!checks]

    if (length(failed) > 0) {
        stop(paste(
            "Action result validation failed:",
            paste(failed, collapse = ", ")
        ))
    }

    return(TRUE)
}

# Use in action
option$perform(function(action) {

    result <- list(
        data = exportData,
        title = "Export"
    )

    validateActionResult(result, type = "data")

    return(result)
})
```

## Conclusion

This guide provides comprehensive coverage of the jamovi action system, from basic implementation to advanced patterns and troubleshooting. By following these patterns and best practices, developers can create powerful data export and workflow capabilities that enhance the analytical power of their jamovi modules.

### Key Takeaways

1. **Always check version compatibility** using `is.null(option$perform)`
2. **Use correct path variables**: Write to `fullPath`, return `path`
3. **Provide helpful error messages** for better user experience
4. **Validate inputs and outputs** to prevent silent failures
5. **Support legacy versions** during transition period (until 2.8+)
6. **Test thoroughly** in different jamovi versions and scenarios

### Resources

- Official Documentation: https://dev.jamovi.org/api_actions.html
- Minimum Version: jamovi 2.7.12
- Platform Support: Desktop (cloud pending)
- API Type: Proper action API (replaces legacy implementations)

---

*This guide is part of the ClinicoPath jamovi module documentation. For questions or issues, please refer to the module repository or jamovi development forums.*
