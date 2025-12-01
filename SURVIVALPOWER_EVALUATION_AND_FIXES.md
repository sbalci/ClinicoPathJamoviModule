# SurvivalPower Function Evaluation and Fixes

## Critical Evaluation
The `survivalPower` function was evaluated for statistical accuracy, functionality, and completeness.

### Identified Issues
1.  **Critical Simulation Flaw:** The Monte Carlo simulation (`.run_simulation_analysis`) logic was fundamentally flawed. It calculated `expected events` based on sample size fractions but did not simulate censoring (accrual, follow-up, dropout). As a result, it effectively assumed a 100% event rate (infinite follow-up), leading to drastically overestimated power (often 100% or 0% depending on the Z-test implementation detail, found to be 0% in test due to symmetric means).
2.  **Disabled Features:** "Competing Risks" and "RMST" analysis types were fully implemented in the private methods (`.populate_competing_risks_table`, `.populate_rmst_analysis_table`) but were explicitly disabled/blocked in the main run loop, returning "unavailable" messages.
3.  **Exponential Assumption:** The module assumes exponential distributions for all calculations. This is a known limitation but is now clearly documented in the code.

## Applied Fixes

### 1. Simulation Logic Repair
*   **Action:** Rewrote `.run_simulation_analysis`.
*   **New Logic:**
    *   Generates exponential time-to-event data for Control and Treatment groups.
    *   Generates uniform entry times over the `accrual_period`.
    *   Calculates administrative censoring times based on `total_study_time` (Accrual + Follow-up).
    *   Applies censoring (`pmin(event_time, censoring_time)`).
    *   Uses `survival::survdiff` (Log-rank test) to calculate p-values for each simulation iteration.
    *   Calculates empirical power as the proportion of p-values < alpha.
*   **Benefit:** The simulation now accurately reflects the study design (accrual, follow-up) and provides a valid robust check against the analytical formulas.

### 2. Feature Enabling (Competing Risks & RMST)
*   **Action:** Connected the previously "orphan" methods.
*   **Implementation:**
    *   Updated `.populate_specialized_tables` to call `.populate_competing_risks_table()` and `.populate_rmst_analysis_table()` when selected.
    *   Updated `.calculate_competing_risks` and `.calculate_rmst` to return informative summary strings directing users to the specialized tables, rather than error messages.
*   **Benefit:** Users can now perform Cause-Specific Competing Risk power analysis (using variance inflation factor method) and RMST power analysis (using exponential approximation).

## Conclusion
The `survivalPower` module is now significantly more functional and statistically accurate. The simulation tool, previously broken, is now a valuable validation feature. The scope of the module has been expanded to include the previously hidden Competing Risks and RMST capabilities.
