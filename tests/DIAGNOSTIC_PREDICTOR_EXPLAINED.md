# What is a Diagnostic Predictor and Why Do We Need It?

## The Confusion: Two Types of Nomograms

The `oddsratio` function actually creates **TWO different types of nomograms**, which can be confusing:

### 1. Risk Prediction Nomogram (Main Nomogram)
**What it is:**
- Uses ALL predictors in your logistic regression model
- Predicts probability of the outcome (e.g., death, recurrence)
- Each predictor gets a point scale
- You add up points to get total risk

**Example:**
```
Patient Profile:
- Age: 65 years       → 45 points
- Sex: Female         → 10 points
- LVI: Present        → 30 points
- PNI: Absent         →  0 points
                    ---------------
Total Points:          85 points
Predicted Risk:        65% chance of death
```

**When to use:**
- Predicting individual patient risk
- Clinical decision-making (should we treat aggressively?)
- Research: developing risk scores

### 2. Fagan's Diagnostic Nomogram (Diagnostic Predictor Nomogram)
**What it is:**
- Uses ONE binary test/predictor only
- Converts pre-test probability → post-test probability
- Based on likelihood ratios (LR+ and LR-)
- Classic tool for diagnostic test evaluation

**Example:**
```
Scenario: Using "LVI Present/Absent" as diagnostic test for mortality

Pre-test probability: 40% (population baseline)
Test result: LVI Present (LR+ = 2.5)

Draw line on nomogram:
40% pre-test → through LR 2.5 → 67% post-test

Interpretation: If LVI is present, this patient's risk
increases from 40% to 67%
```

**When to use:**
- Evaluating diagnostic test performance
- Medical education (understanding test utility)
- Evidence-based medicine (updating probabilities)

---

## Why Do We Need a "Diagnostic Predictor"?

### The Technical Reason:
Fagan's nomogram requires:
1. **Binary test** (Yes/No, Present/Absent, Positive/Negative)
2. **Sensitivity and specificity** (calculated from 2×2 table)
3. **Likelihood ratios** (derived from sens/spec)

You CAN'T calculate these for:
- ❌ Continuous variables (Age, Tumor Size) - infinite possible values
- ❌ Multi-category variables (Grade 1/2/3) - no single 2×2 table
- ✅ Binary variables only (LVI: Absent/Present)

### The Clinical Reason:
Different predictors serve different purposes:

**In the regression model (all predictors):**
```R
oddsratio(
  explanatory = c("Age", "Sex", "LVI", "PNI", "TumorSize"),
  outcome = "Mortality5yr"
)
```
→ Gives you ADJUSTED odds ratios (controlling for confounders)
→ Multi-variable risk prediction

**For diagnostic testing (single predictor):**
```R
diagnosticPredictor = "LVI"
```
→ Evaluates LVI as a standalone TEST
→ "If I only know LVI status, how useful is it?"
→ Calculates sensitivity, specificity, likelihood ratios

---

## Practical Examples

### Example 1: Lymphovascular Invasion (LVI) as Diagnostic Test

**Clinical Question:**
"How good is LVI at predicting 5-year mortality?"

**Setup:**
- Outcome: Mortality5yr = Dead
- Diagnostic Predictor: LVI (Present/Absent)

**What you get:**

**Contingency Table:**
```
              Mortality
         Alive    Dead
LVI  
Absent    60      40     (Specificity: 60/100 = 60%)
Present   20      60     (Sensitivity: 60/100 = 60%)
```

**Diagnostic Metrics:**
- Sensitivity: 60% (detects 60% of deaths)
- Specificity: 60% (correctly identifies 60% of survivors)
- LR+: 1.5 (weak positive evidence)
- LR-: 0.67 (weak negative evidence)

**Interpretation:**
LVI is a modest diagnostic test - not great, but provides some information.

### Example 2: Why Not Use Age?

**Problem:**
Age is continuous (25, 26, 27, ... 85)

**You'd need:**
```
              Mortality
         Alive    Dead
Age  
25        ?       ?
26        ?       ?
27        ?       ?
...
85        ?       ?
```

This doesn't make sense! You can't calculate sensitivity/specificity for 61 different age values.

**What you COULD do:**
1. Dichotomize Age: "Young" (<50) vs "Old" (≥50)
2. Then use as diagnostic predictor
3. But you lose information (why 50? why not 55?)

---

## Default Behavior: First Explanatory Variable

**When you don't specify a diagnostic predictor:**

```R
oddsratio(
  explanatory = c("LVI", "PNI", "Age"),
  outcome = "Mortality5yr",
  showNomogram = TRUE
  # diagnosticPredictor not specified
)
```

**What happens:**
1. Function tries to use **first explanatory variable** (LVI)
2. Checks if it's binary ✓
3. Calculates diagnostic metrics for LVI
4. Creates Fagan's nomogram using LVI

**Why this makes sense:**
- Often the first variable is your main predictor of interest
- Provides a reasonable default
- User can override if needed

---

## When to Specify a Different Diagnostic Predictor

### Scenario 1: Main Predictor is Continuous
```R
oddsratio(
  explanatory = c("Age", "LVI", "PNI"),  # Age is first
  outcome = "Mortality5yr",
  showNomogram = TRUE,
  diagnosticPredictor = "LVI"  # Specify binary variable
)
```

### Scenario 2: Want to Evaluate Specific Test
```R
oddsratio(
  explanatory = c("LVI", "PNI", "Sex"),
  outcome = "Mortality5yr",
  showNomogram = TRUE,
  diagnosticPredictor = "Sex"  # Evaluate Sex as test
)
```

### Scenario 3: Test Not in Regression Model
```R
oddsratio(
  explanatory = c("LVI", "PNI"),
  outcome = "Mortality5yr",
  showNomogram = TRUE,
  diagnosticPredictor = "NewBiomarker"  # Not in model!
)
```
→ Calculates diagnostic metrics independently
→ Useful for comparing new vs. established tests

---

## Common Questions

### Q: Why can't I use my regression model's prediction as the diagnostic predictor?

**A:** That's actually the MAIN nomogram! The regression nomogram already does this - it combines all predictors to give you a probability. The diagnostic nomogram is different - it evaluates a SINGLE test.

### Q: Can I create a diagnostic nomogram without odds ratio analysis?

**A:** Not in this function. But you could:
1. Run `oddsratio()` with just your test variable
2. Enable nomogram
3. Ignore the regression table if not needed

### Q: What if I have 3 binary tests (LVI, PNI, Sex) - which one should be diagnostic predictor?

**A:** Depends on your research question:
- **Clinical utility:** Which test is most practical to measure?
- **Novel biomarker:** Comparing new test to established tests
- **Best performer:** Check sensitivity/specificity for each, pick best

You can run the analysis 3 times with different diagnostic predictors to compare them!

---

## Summary

| Feature | Risk Prediction Nomogram | Diagnostic Nomogram |
|---------|------------------------|-------------------|
| **Uses** | All predictors | One binary predictor |
| **Purpose** | Predict individual risk | Evaluate test performance |
| **Output** | Probability of outcome | Pre→Post probability |
| **Input** | Patient characteristics | Test result + baseline risk |
| **Clinical Use** | Treatment decisions | Test selection, education |
| **Requirements** | Any predictors | Binary predictor only |

**The "diagnostic predictor" is the specific binary test you want to evaluate using Fagan's nomogram for diagnostic test performance assessment.**
