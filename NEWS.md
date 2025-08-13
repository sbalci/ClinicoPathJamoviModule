# ClinicoPath News

## Version 0.0.31.01

### Enhancements

*   **Automatic Plot Selection (statsplot2) Module:**
    *   **Enhanced Error Messages:** Implemented comprehensive contextual error messages with variable names, data counts, and actionable guidance for debugging
    *   **Performance Optimization:** Added analysis result caching to eliminate redundant calculations between `.init()` and `.plot()` methods
    *   **Code Quality:** Extracted magic numbers to constants for better maintainability
    *   **Robust Data Validation:** Added specific validation for dotplot statistics with detailed feedback on data requirements
    *   **Edge Case Handling:** Improved validation for empty factor levels in grouped plots with informative warnings
    *   **Package Dependency Validation:** Added defensive package checking with clear installation instructions for ggalluvial and easyalluvial
    *   **Variable Type Detection:** Enhanced unknown variable type detection with warnings and class information

*   **Comprehensive jamovi Development Documentation:**
    *   **Analysis Options Guide (.a.yaml):** Expanded from 223 to 1,570 lines with comprehensive option types, validation patterns, and clinical examples
    *   **Results Definition Guide (.r.yaml):** Enhanced from 197 to 1,611 lines covering all result types, table design, and integration patterns
    *   **User Interface Guide (.u.yaml):** Improved from 172 to 1,395 lines with complete UI component architecture and clinical workflow patterns
    *   **Table Creation Guide:** Transformed from 152 to 2,525 lines covering complete table development lifecycle from .r.yaml definition to .b.R population
    *   **Clinical Integration:** Added extensive clinical and research workflow examples throughout all documentation
    *   **Best Practices:** Implemented consistent documentation standards with practical examples and troubleshooting guides
    *   **Advanced Patterns:** Documented dynamic interfaces, conditional logic, error handling, and performance optimization techniques

## Version 0.0.3.96

### New Features & Enhancements

*   **Waterfall Plot Module:**
    *   Implemented group-based coloring for waterfall and spider plots.
    *   Added `colorBy`, `spiderColorBy`, and `spiderColorScheme` options for customization.
    *   Refactored code for quality and performance improvements.
    *   Improved data validation with user-friendly messages.
*   **IHC Expression Analysis Module:**
    *   Fixed issues with the `clear()` method, improving table population reliability.
*   **Medical Decision Tree Analysis:**
    *   Added a progress bar for real-time feedback.
    *   Fixed several runtime errors and improved parameter validation.

## Version 0.0.3.95

### Bug Fixes

*   **Tree Module:**
    *   Resolved critical syntax errors that were preventing module compilation.
    *   Restored the `.train_model` function.
    *   Fixed variable initialization and scoping issues.
*   **Decision Analysis Framework:**
    *   Enhanced the `decisiongraph` module with health economics features.
    *   Added Net Monetary Benefit (NMB) and Incremental Cost-Effectiveness Ratio (ICER) analysis.

## Version 0.0.3.90

### Documentation

*   Updated all submodule documentation links in the `README` to point to their respective documentation sites.

## Version 0.0.3.82

### New Features

*   **Clinical Utility Index:**
    *   Implemented a comprehensive framework for assessing the clinical utility of staging systems.
    *   Added Net Benefit Analysis and Number Needed to Treat (NNT) calculations.

## Version 0.0.3.81

### New Features

*   **Frailty Models:**
    *   Added support for frailty models for clustered survival data using mixed-effects Cox models.

## Version 0.0.3.80

### New Features

*   **Concordance Probability Estimates:**
    *   Added advanced concordance probability analysis for heavily censored data.
*   **Win Ratio Analysis:**
    *   Implemented win ratio analysis for composite endpoint evaluation.