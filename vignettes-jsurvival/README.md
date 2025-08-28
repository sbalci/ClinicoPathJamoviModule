Clinician Quickstart for jSurvival Vignettes
===========================================

Audience: Statistics‑naive clinicians (pathology/oncology) using jamovi.

Recommended learning path
-------------------------
- Getting started: 01-introduction-user-friendly.Rmd
- One complete workflow: 05-jamovi-workflow-user-friendly.Rmd
- Comparing groups: 14-comparing-survival-user-friendly.Rmd
- Competing risks: 15-competing-survival-user-friendly.Rmd
- Diagnostics (advanced): 16-cox-model-diagnostics.Rmd

What you’ll learn
-----------------
- Basic concepts: time-to-event, censoring, Kaplan–Meier (KM) plots
- Group comparisons: log-rank p-values, median survival, pairwise tests
- Competing risks: when and how to use CIF plots
- Cox models: hazard ratios, assumptions, and simple checks

Pick the right analysis
-----------------------
- Single-arm KM: Describe overall survival over time
- Comparing survival: Compare groups (e.g., treatment, biomarker)
- Competing risks: Multiple causes/events can preclude each other
- Cox regression: Adjust for multiple factors (age, grade, stage)

Key terms (plain language)
--------------------------
- Time-to-event: Follow-up time until an outcome (or last contact)
- Censoring: Patient didn’t have the event during observed follow-up
- Kaplan–Meier curve: Probability of remaining event-free over time
- Log-rank test: Are KM curves different beyond chance?
- Median survival: Time at which 50% have had the event
- Hazard ratio (HR): Relative risk at any moment (HR < 1 is lower risk)
- Proportional hazards: HR stays roughly constant over time
- Cumulative incidence (CIF): Probability an event has happened by a time accounting for competing risks

Common pitfalls to avoid
------------------------
- Mismatched coding: Ensure event is coded consistently (e.g., 1 = event)
- Time unit confusion: Keep time in consistent units (months/days)
- Missing dates: Check for impossible/negative times when deriving time
- Multiple groups: Use pairwise comparisons after overall log-rank
- Non-PH in Cox: Check assumptions; consider stratification or alternatives

jamovi navigation (at a glance)
-------------------------------
- Survival: Analyses > jSurvival > Survival Analysis
- Competing risks: Analyses > jSurvival > Competing Risks
- Cox model/diagnostics: Analyses > jSurvival > Cox Diagnostics

Next steps
----------
- Replace the “Screenshot Placeholder” notes in vignettes with actual images
- Use a consistent example dataset across user‑friendly vignettes
- Add “Interpretation” boxes (example language for Results sections)

Developer reference
-------------------
- Detailed feature mappings and UI-to-code references are in:
  - `survival_documentation.md`
  - `comparingSurvival_documentation.md`
  - Other `*_documentation.md` files
  These are intended for developers and power users; clinicians can skip them.
