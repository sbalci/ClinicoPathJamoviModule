# Survival Endpoints Evaluation and Fixes

## Critical Evaluation
The `survivalendpoints` function was evaluated for statistical accuracy and robustness.

### Identified Issues
1.  **Negative Time Calculation:** If an event date or follow-up date was entered as being *before* the start date (data entry error), the function calculated a negative survival time. Negative times are invalid for standard survival analysis and can cause downstream errors.
2.  **Implicit Event Time Imputation:** When an event indicator (e.g., `progressionEvent=1`) was provided without a corresponding date (`progressionDate=NA`), the function silently imputed the event time using `lastFollowup`. While a reasonable fallback for some legacy data, this is statistically risky and the user should be warned that their data is incomplete.

## Applied Fixes

### 1. Data Quality Checks & Safety
*   **Action:** Added a comprehensive data quality check block in `.run`.
*   **Logic:** 
    *   Scans all calculated time columns (`*_time`) for negative values.
    *   **Auto-Correction:** Sets any negative time values to `NA` (missing) to prevent invalid survival calculations.
    *   **User Feedback:** Displays a warning in the results if any negative times were detected and removed.

### 2. Imputation Transparency
*   **Action:** Added detection logic for "Event without Date" scenarios.
*   **Logic:** Checks if `Event == 1` AND `Date == NA` for both Progression and Death.
*   **User Feedback:** Displays a specific warning counting how many patients had event times imputed from the Last Follow-up Date. This ensures researchers are aware of the data limitation.

## Conclusion
The function is now safer and more transparent. It prevents invalid negative time data from propagating and ensures users are aware of any assumptions made about missing event dates.
