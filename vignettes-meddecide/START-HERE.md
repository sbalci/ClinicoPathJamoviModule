# MedDecide: Start Here (10-minute Quickstart)

## Who this is for
- Pathologists, oncologists, and clinical researchers who want evidence-based, practical test and treatment decisions without deep statistics.

## What you can do
- Evaluate tests (ROC, AUC, sensitivity/specificity, PPV/NPV).
- Compare strategies with Decision Curves (net benefit).
- Build simple decision trees; explore thresholds.

## 1) Open in jamovi
- Install the meddecide module from the jamovi Library.
- Open the `meddecide` menu from the Modules toolbar.

## 2) Quick ROC (biomarker example)
- Open your dataset.
- meddecide > ROC Curve.
- Outcome: pick binary outcome (e.g., Response yes/no).
- Predictor: pick test score (e.g., PD-L1 TPS, gene score).
- Readout:
  - AUC ~ overall test discrimination.
  - Cutpoint table: pick threshold that fits your clinical goal (rule-in vs rule-out).

## 3) Decision Curve (net benefit)
- meddecide > Decision Panel > Decision Curve.
- Set threshold probability range (e.g., 0.1–0.5).
- Compare “Treat All”, “Treat None”, and your model.
- Interpretation: higher net benefit at your clinical threshold means better patient decisions.

## 4) Screening Calculator (PPV/NPV)
- meddecide > Screening Calculator.
- Enter sensitivity, specificity, and prevalence.
- See PPV/NPV and Fagan nomogram; try sequential tests.

## Tips
- Start with prevalence from your clinic; adjust thresholds to match harm/benefit trade-offs.
- See Glossary: `00-glossary.md`.
- Saved outputs: export tables/plots from jamovi.

### Threshold selection
- Rule‑out: pick a cutpoint with very high Sensitivity (e.g., ≥0.95) to minimize missed disease.
- Rule‑in: pick a cutpoint with very high Specificity (e.g., ≥0.95) to minimize unnecessary treatment.
- Decision Curves: choose the strategy with highest net benefit at your clinical threshold probability (p_t). If harms of overtreatment are small, use a lower p_t; if harms are larger, use a higher p_t.

## Examples next
- Decision Panel: `decisionpanel_clinician_guide.md`
- Screening: `screeningcalculator_clinician_guide.md`
- Overview: `01-introduction.Rmd`
