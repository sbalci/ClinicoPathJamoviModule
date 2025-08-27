# Screening Calculator: Clinician Guide

## Purpose
Estimate PPV/NPV from sensitivity, specificity, and prevalence; explore sequential testing.

## Steps in jamovi
1) Open `meddecide` menu > Screening Calculator.
2) Enter Sensitivity, Specificity, and Prevalence (use your clinic’s prevalence when possible).
3) Optionally enable 2× or 3× repeat testing; review Fagan nomograms.
4) Review PPV/NPV tables and interpret in clinical context.

## How to interpret
- PPV increases with prevalence; NPV decreases with prevalence.
- Use LR+/LR− to understand how results change odds; large LR+ or small LR− provide stronger shifts.
- Sequential positives disproportionately raise PPV in low-prevalence settings.

## Example takeaway
“In our 5% prevalence clinic, a test with 90%/95% yields PPV ~49%, NPV ~99%; repeating positives can raise PPV substantially.”

## Tips
- Don’t copy prevalence from literature if your setting differs.
- For screening, emphasize ruling out disease (prioritize high NPV and LR−).
- Consider downstream harms (false positives) when setting referral thresholds.
