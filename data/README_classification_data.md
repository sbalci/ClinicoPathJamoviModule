# Classification Analysis Test Datasets

## Overview

Seven comprehensive datasets for testing the Clinical Classification Analysis module in ClinicoPath. Each dataset is designed to test specific features, edge cases, and warning conditions.

---

## Datasets

### 1. `classification_binary_balanced` (n=150)

**Purpose**: Primary test dataset for binary classification features

**Clinical Scenario**: Predicting post-operative complications

**Variables**:
- **Outcome**: `Complication` (No, Yes) - balanced distribution
- **Continuous Predictors**:
  - `Age` (mean=65, sd=12)
  - `BMI` (mean=27, sd=4.5)
  - `Biomarker` (mean=50, sd=15)
  - `BloodPressure` (mean=130, sd=15)
- **Categorical Predictors**:
  - `TumorGrade` (Grade1, Grade2, Grade3)
  - `LymphNodeStatus` (Negative, Positive)
  - `Gender` (Male, Female)
  - `Smoking` (Never, Former, Current)

**Use For**: Testing all classifiers, validation methods, clinical metrics, ROC/AUC analysis

**Example**:
```r
data(classification_binary_balanced)
classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade, LymphNodeStatus),
    classifier = "randomForest",
    reportClinicalMetrics = TRUE
)
```

---

### 2. `classification_binary_imbalanced` (n=200)

**Purpose**: Test class imbalance handling methods

**Clinical Scenario**: Rare adverse event prediction (10% prevalence)

**Variables**:
- **Outcome**: `RareEvent` (No, Yes) - severe imbalance (9:1 ratio)
- **Continuous Predictors**:
  - `RiskScore` (mean=5, sd=2)
  - `Age` (mean=60, sd=15)
  - `ExposureLevel` (mean=100, sd=25)
- **Categorical Predictors**:
  - `RiskCategory` (Low, Medium, High)
  - `Comorbidity` (None, One, Multiple)

**Use For**: Testing upsampling, downsampling, SMOTE, MCC metric, imbalance warnings

**Example**:
```r
data(classification_binary_imbalanced)
classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel),
    balancingMethod = "upsample",
    reportMCC = TRUE
)
```

---

### 3. `classification_multiclass` (n=180)

**Purpose**: Multiclass classification (3 classes)

**Clinical Scenario**: Cancer staging prediction

**Variables**:
- **Outcome**: `CancerStage` (StageI, StageII, StageIII) - balanced
- **Continuous Predictors**:
  - `TumorSize` (mean=3.5, sd=1.5)
  - `NodeCount` (mean=2, sd=3)
  - `KI67` (mean=20, sd=10)
- **Categorical Predictors**:
  - `Differentiation` (Well, Moderate, Poor)
  - `VascularInvasion` (Absent, Present)
  - `Receptor` (Positive, Negative)

**Use For**: Testing multiclass confusion matrix, per-class metrics

**Example**:
```r
data(classification_multiclass)
classification(
    data = classification_multiclass,
    dep = CancerStage,
    indep = vars(TumorSize, NodeCount, KI67, Differentiation),
    classifier = "randomForest",
    reporting = c("classifMetrices", "confusionMatrix")
)
```

---

### 4. `classification_small_sample` (n=25)

**Purpose**: Test small sample size warnings

**Clinical Scenario**: Pilot study with limited data

**Variables**:
- **Outcome**: `Outcome` (Poor, Good)
- **Continuous Predictors**: `Score1`, `Score2`
- **Categorical Predictors**: `Category` (A, B)

**Use For**: Triggering small sample warnings (n<30)

**Expected Warnings**:
- "Very small sample size (n=25)"
- "Very small cross-validation fold size"

**Example**:
```r
data(classification_small_sample)
classification(
    data = classification_small_sample,
    dep = Outcome,
    indep = vars(Score1, Score2),
    testing = "crossValidation",
    noOfFolds = 5
)
```

---

### 5. `classification_perfect` (n=100)

**Purpose**: Test perfect predictor edge case

**Clinical Scenario**: Diagnostic test with perfect sensitivity/specificity

**Variables**:
- **Outcome**: `Outcome` (Negative, Positive)
- **Continuous Predictors**:
  - `PerfectPredictor` (perfectly separates classes: 0 or 100)
  - `NoisyPredictor` (mean=50, sd=10)
- **Categorical Predictors**: `Group` (Control, Treatment)

**Use For**: Testing model behavior with perfect separation

**Example**:
```r
data(classification_perfect)
classification(
    data = classification_perfect,
    dep = Outcome,
    indep = vars(PerfectPredictor, NoisyPredictor),
    classifier = "logisticRegression"
)
```

---

### 6. `classification_high_dim` (n=50)

**Purpose**: Test high-dimensionality warnings

**Clinical Scenario**: Biomarker panel with many features, limited samples

**Variables**:
- **Outcome**: `Outcome` (No, Yes)
- **Predictors**: 15 variables (10 continuous `Var1`-`Var10`, 5 categorical `Cat1`-`Cat5`)
- **Feature-to-Sample Ratio**: 3.3 (well below recommended 10:1)

**Use For**: Triggering overfitting risk warnings

**Expected Warning**: "High feature-to-sample ratio: 15 features with 50 samples (ratio: 3.3)"

**Example**:
```r
data(classification_high_dim)
classification(
    data = classification_high_dim,
    dep = Outcome,
    indep = vars(Var1, Var2, Var3, Var4, Var5, Cat1, Cat2, Cat3),
    classifier = "randomForest"
)
```

---

### 7. `classification_with_missing` (n=120)

**Purpose**: Test missing data handling via complete case analysis

**Clinical Scenario**: Clinical trial with incomplete data

**Variables**:
- **Outcome**: `Outcome` (Control, Case)
- **Predictors**: `Predictor1`-`Predictor3`, `Category1`-`Category2`
- **Missing Pattern**: 20 observations with missing values (MCAR)
  - 10 missing in `Predictor2`
  - 10 missing in `Category1`

**Use For**: Verifying complete case analysis reduces sample size appropriately

**Example**:
```r
data(classification_with_missing)
classification(
    data = classification_with_missing,
    dep = Outcome,
    indep = vars(Predictor1, Predictor2, Predictor3),
    classifier = "logisticRegression"
)
# Expected: Analysis uses ~100 complete cases, not full 120
```

---

## Quick Start Testing

### Basic Functionality Test
```r
# Load balanced binary dataset
data(classification_binary_balanced)

# Run simple decision tree
classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "singleDecisionTree",
    plotDecisionTree = TRUE
)
```

### Clinical Metrics Test
```r
# Test full clinical reporting
classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade, LymphNodeStatus),
    classifier = "randomForest",
    testing = "crossValidation",
    noOfFolds = 5,
    reportClinicalMetrics = TRUE,
    reportConfidenceIntervals = TRUE,
    reportMCC = TRUE,
    positiveClass = "Yes",
    reporting = c("classifMetrices", "confusionMatrix", "AUC")
)
```

### Class Imbalance Test
```r
# Test SMOTE balancing
data(classification_binary_imbalanced)
classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel),
    classifier = "randomForest",
    balancingMethod = "smote",
    reportMCC = TRUE
)
```

---

## Data Generation

All datasets were generated using `data-raw/classification_comprehensive_data.R` with seed 42 for reproducibility. To regenerate:

```r
source("data-raw/classification_comprehensive_data.R")
```

---

## Loading Data in R

```r
# Load specific dataset
data(classification_binary_balanced)

# Load all classification datasets
data(package = "ClinicoPath")
```

---

## Loading Data in jamovi

1. Open jamovi
2. Click **File** > **Open**
3. Navigate to `ClinicoPath/data/`
4. Select desired CSV file:
   - `classification_binary_balanced.csv`
   - `classification_binary_imbalanced.csv`
   - `classification_multiclass.csv`
   - `classification_small_sample.csv`
   - `classification_perfect.csv`
   - `classification_high_dim.csv`
   - `classification_with_missing.csv`

---

## See Also

- **Comprehensive Testing Guide**: `tests/classification_TESTING_GUIDE.md`
- **Function Documentation**: `?classification`
- **Clinical Classification vignette**: (upcoming)

---

## Dataset Characteristics Summary

| Dataset | n | Outcome Classes | Predictors | Key Feature | Warning Expected |
|---------|---|----------------|-----------|-------------|------------------|
| binary_balanced | 150 | 2 (balanced) | 8 (4 cont, 4 cat) | Primary test | None |
| binary_imbalanced | 200 | 2 (9:1) | 5 (3 cont, 2 cat) | Class imbalance | Imbalance warning |
| multiclass | 180 | 3 (balanced) | 6 (3 cont, 3 cat) | Multiclass | None |
| small_sample | 25 | 2 | 3 (2 cont, 1 cat) | Small n | Small sample warning |
| perfect | 100 | 2 | 3 (2 cont, 1 cat) | Perfect separation | Possible convergence |
| high_dim | 50 | 2 | 15 (10 cont, 5 cat) | High p/n ratio | High dimensionality |
| with_missing | 120 | 2 | 5 (3 cont, 2 cat) | Missing data | Sample reduction |

---

**Last Updated**: 2024-12-23
**ClinicoPath Version**: 0.0.3
