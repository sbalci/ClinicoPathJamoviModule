# Decision Panel: Clinician Guide

## Purpose
Evaluate test/model utility across thresholds using net benefit (Decision Curve Analysis).

## Steps in jamovi
1) Open `meddecide` menu > Decision Panel > Decision Curve.
2) Select Outcome (event = “Yes/Positive”) and Predictor (score/probability).
3) Set threshold range (e.g., 0.1–0.5) and show “Treat all/none”.
4) Run and view the net benefit plot and table.

## How to interpret
- Pick your clinical threshold (risk at which you’d treat). Higher net benefit at that threshold is better.
- If “Treat all” is above your model, the model does not add value at that threshold.
- Parallel lines across thresholds suggest stable benefit; crossings suggest context-specific use.

## Example takeaway
“At a 20% treatment threshold, the model yields the highest net benefit, supporting biopsy in patients ≥20% predicted risk.”

## Tips
- For rule-out strategies, focus on lower thresholds (e.g., 5–10%).
- Calibrate predicted probabilities before DCA when possible.
- Add confidence intervals if available; otherwise, validate on a separate dataset.

## Threshold Selection
- Concept: The threshold probability (p_t) is where you would switch from “no treatment” to “treat”. It reflects your harm–benefit trade-off.
- Approximate rule: p_t = harm / (harm + benefit). If overtreatment harm ≈ missed‑benefit, p_t ≈ 0.50. If overtreatment harm is small (e.g., safe/cheap), p_t is lower (e.g., 0.10–0.20).
- Practical steps:
  1) Define consequences of false positives vs false negatives (e.g., biopsy risks vs missing cancer).
  2) Pick p_t that matches that trade‑off (common ranges: 0.05–0.15 for screening; 0.15–0.30 for diagnostics with moderate harms).
  3) Read the net‑benefit plot at your p_t. Prefer the strategy with the highest net benefit at that point.

### Quick Threshold Calculator
- Formula: `p_t = harm_false_positive / (harm_false_positive + benefit_true_positive)`
- Example: If false positive = 1 harm unit (e.g., minor biopsy harm) and true positive = 4 benefit units (e.g., major benefit of early treatment), then `p_t = 1 / (1 + 4) = 0.20` (20%).
- Tip: You can rescale “units” however you like (time, cost, discomfort); only the ratio matters.

### Where to read net benefit at p_t
1) Set your threshold range to include your p_t (e.g., 0.20).
2) Identify the vertical at p_t on the plot and compare the lines.
3) Highest line at p_t has the best net benefit.

Placeholder (replace with a real screenshot once captured):

![Reading net benefit at p_t](images/decisioncurve_read_pt.png)
