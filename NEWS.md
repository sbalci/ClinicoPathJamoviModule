# ClinicoPath News

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