# Time-Dependent DCA Evaluation and Fixes

## Critical Evaluation
The `timedependentdca` function was evaluated for statistical accuracy and functionality.

### Identified Issues
1.  **Misleading Metric:** The "Interventions Avoided" metric was calculated as the simple percentage of patients classified as low risk (`(N - N_treated) / N`). While intuitive, this is not the standard DCA metric ("Net Reduction in Interventions") and can be misleading if the model simply classifies everyone as low risk (poor sensitivity).
2.  **Inconsistent Smoothing:** The Net Benefit curves were smoothed, but the "Interventions Avoided" metric was calculated from raw counts, leading to inconsistencies between the plots and tables.
3.  **Unsubstantiated Claim:** The module instructions claimed to support "Competing risks" analysis, but the underlying code uses standard Kaplan-Meier/Cox methods which do not account for competing risks (potentially overestimating risk).
4.  **Plot Scaling:** The "Interventions Avoided" plot had fixed Y-axis limits (0-100), which is appropriate for percentages but not for the standard Net Reduction metric (which can be negative).

## Applied Fixes

### 1. Standardization of "Interventions Avoided"
*   **Action:** Updated the calculation logic in `.calculateTimeDependentNB`.
*   **New Formula:** Uses the standard DCA formula: `Net Reduction = (NB_model - NB_treat_all) / (pt / (1 - pt)) * 100`.
*   **Benefit:** This metric correctly reflects the utility of the model compared to the "Treat All" strategy, standardized per 100 patients.

### 2. Consistency with Smoothing
*   **Action:** Moved the calculation of "Net Reduction" to *after* the smoothing block.
*   **Benefit:** The metric is now derived from the smoothed Net Benefit values, ensuring that the "Interventions Avoided" curve matches the "Net Benefit" curve features.

### 3. Instruction Accuracy
*   **Action:** Removed the bullet point claiming support for "Competing risks" from the module instructions.
*   **Benefit:** Prevents users from mistakenly applying this tool to competing risk data without proper methodology (e.g., Fine-Gray).

### 4. Visualization Improvement
*   **Action:** Updated `.interventionsPlot` to use dynamic Y-axis limits (`range(...)`) and updated the axis label to "Net Reduction in Interventions (per 100 patients)".
*   **Benefit:** Ensures the plot correctly displays the full range of data, including potential negative values (harm), which were previously hidden or clipped.

## Conclusion
The module is now aligned with standard Decision Curve Analysis methodology. The metrics are rigorously defined, and the documentation accurately reflects the capabilities (standard survival analysis).
