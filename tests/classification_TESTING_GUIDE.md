# Comprehensive Testing Guide for Clinical Classification Analysis

## Overview

This guide provides systematic testing procedures for the `classification` function in the ClinicoPath jamovi module. Seven test datasets cover all features, edge cases, and warning conditions.

---

## Test Datasets

### Dataset 1: `classification_binary_balanced.csv` (n=150)
- **Purpose**: Primary test dataset for all binary classification features
- **Outcome**: `Complication` (No/Yes, balanced ~50/50)
- **Predictors**:
  - Continuous: Age, BMI, Biomarker, BloodPressure
  - Categorical: TumorGrade, LymphNodeStatus, Gender, Smoking
- **Tests**: All classifiers, validation methods, clinical metrics, ROC/AUC

### Dataset 2: `classification_binary_imbalanced.csv` (n=200)
- **Purpose**: Test class imbalance handling
- **Outcome**: `RareEvent` (No/Yes, 9:1 ratio)
- **Predictors**: RiskScore, Age, ExposureLevel, RiskCategory, Comorbidity
- **Tests**: Balancing methods (upsample, downsample, SMOTE), MCC metric

### Dataset 3: `classification_multiclass.csv` (n=180)
- **Purpose**: Multiclass classification (3 classes)
- **Outcome**: `CancerStage` (StageI/StageII/StageIII)
- **Predictors**: TumorSize, NodeCount, KI67, Differentiation, VascularInvasion, Receptor
- **Tests**: Multiclass metrics, confusion matrix, per-class performance

### Dataset 4: `classification_small_sample.csv` (n=25)
- **Purpose**: Test small sample warnings
- **Outcome**: `Outcome` (Poor/Good)
- **Predictors**: Score1, Score2, Category
- **Tests**: Small sample warnings (n<30), unstable estimates

### Dataset 5: `classification_perfect.csv` (n=100)
- **Purpose**: Test perfect separation edge case
- **Outcome**: `Outcome` (Negative/Positive)
- **Predictors**: PerfectPredictor (perfectly separates), NoisyPredictor, Group
- **Tests**: Model convergence with perfect predictor

### Dataset 6: `classification_high_dim.csv` (n=50)
- **Purpose**: Test high-dimensionality warnings
- **Outcome**: `Outcome` (No/Yes)
- **Predictors**: 15 variables (Var1-10, Cat1-5)
- **Tests**: High feature-to-sample ratio warnings (n/p < 10)

### Dataset 7: `classification_with_missing.csv` (n=120)
- **Purpose**: Test missing data handling
- **Outcome**: `Outcome` (Control/Case)
- **Predictors**: Predictor1-3, Category1-2 (with missing values)
- **Tests**: Complete case analysis, sample size reduction notice

---

## Testing Matrix

### 1. Classifier Testing (Dataset 1: Binary Balanced)

#### Test 1.1: Single Decision Tree
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, BMI, Biomarker, TumorGrade, LymphNodeStatus),
    classifier = "singleDecisionTree",
    testing = "trainSet",
    splitRule = "gini",
    minSplit = 20,
    maxDepth = 30,
    complexity = 0.01,
    plotDecisionTree = TRUE,
    reporting = c("classifMetrices", "confusionMatrix")
)
```
**Expected**: Decision tree plot, classification metrics, confusion matrix

#### Test 1.2: Random Forest
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, BMI, Biomarker, TumorGrade),
    classifier = "randomForest",
    testing = "trainSet",
    noOfTrees = 100,
    splitRule = "gini",
    sampleFraction = 0.8,
    printRandForest = TRUE,
    predictedFreqRF = TRUE
)
```
**Expected**: Random forest model summary, OOB error, predicted frequencies plot

#### Test 1.3: K-Nearest Neighbors
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, BMI, Biomarker, BloodPressure),
    classifier = "knn",
    testing = "crossValidation",
    noOfFolds = 5,
    knnNeighbors = 5,
    knnDistance = "euclidean",
    reporting = c("classifMetrices", "confusionMatrix")
)
```
**Expected**: KNN classification with 5-fold CV results

#### Test 1.4: Naive Bayes
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade, LymphNodeStatus),
    classifier = "naiveBayes",
    testing = "split",
    testSize = 0.3
)
```
**Expected**: Naive Bayes classification with train/test split

#### Test 1.5: Logistic Regression
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, BMI, Biomarker),
    classifier = "logisticRegression",
    testing = "crossValidation",
    noOfFolds = 10,
    reporting = c("classifMetrices", "AUC")
)
```
**Expected**: Logistic regression with ROC curve

#### Test 1.6: Support Vector Machine
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, BMI, Biomarker, BloodPressure),
    classifier = "svm",
    testing = "split",
    testSize = 0.25,
    svmKernel = "radial",
    svmCost = 1.0,
    svmGamma = 1.0
)
```
**Expected**: SVM classification with RBF kernel

---

### 2. Validation Method Testing (Dataset 1)

#### Test 2.1: Train Set Only (Optimistic)
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "singleDecisionTree",
    testing = "trainSet"
)
```
**Expected**: High accuracy (optimistic), no test set

#### Test 2.2: Train/Test Split (Holdout)
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "randomForest",
    testing = "split",
    testSize = 0.33,
    seed = 42
)
```
**Expected**: 67% training, 33% test split, reproducible results

#### Test 2.3: Cross-Validation
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "logisticRegression",
    testing = "crossValidation",
    noOfFolds = 10,
    seed = 42
)
```
**Expected**: 10-fold CV results, averaged performance

#### Test 2.4: Bootstrap Validation
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "randomForest",
    validateMethod = "bootstrap",
    bootstrapSamples = 100,
    seed = 42
)
```
**Expected**: Bootstrap validation with 100 samples

---

### 3. Clinical Metrics Testing (Dataset 1)

#### Test 3.1: Full Clinical Metrics with Confidence Intervals
```r
JamoviTest::classification(
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
    bootstrapSamples = 1000,
    reporting = c("classifMetrices", "confusionMatrix", "AUC"),
    seed = 42
)
```
**Expected**:
- Sensitivity, Specificity, PPV, NPV with 95% CIs
- LR+, LR-, Prevalence, NNT
- Matthews Correlation Coefficient with interpretation
- ROC curve with AUC

#### Test 3.2: Threshold Optimization (Youden J)
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "logisticRegression",
    testing = "split",
    testSize = 0.3,
    reportClinicalMetrics = TRUE,
    thresholdMethod = "youden",
    reporting = c("classifMetrices", "AUC")
)
```
**Expected**: Optimized threshold shown in general metrics table

#### Test 3.3: Manual Threshold
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "randomForest",
    testing = "split",
    testSize = 0.3,
    reportClinicalMetrics = TRUE,
    thresholdMethod = "manual",
    thresholdValue = 0.7,
    seed = 42
)
```
**Expected**: Custom 0.7 threshold used, likely higher specificity

---

### 4. Class Imbalance Testing (Dataset 2: Imbalanced)

#### Test 4.1: No Balancing (Baseline)
```r
JamoviTest::classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel, RiskCategory),
    classifier = "randomForest",
    testing = "crossValidation",
    noOfFolds = 5,
    balancingMethod = "none",
    reportMCC = TRUE,
    seed = 42
)
```
**Expected**:
- WARNING: "Severe class imbalance detected (10%)"
- High accuracy but possibly poor sensitivity
- MCC useful for evaluating performance

#### Test 4.2: Upsample Minority Class
```r
JamoviTest::classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel, RiskCategory),
    classifier = "randomForest",
    testing = "crossValidation",
    noOfFolds = 5,
    balancingMethod = "upsample",
    reportClinicalMetrics = TRUE,
    reportMCC = TRUE,
    seed = 42
)
```
**Expected**:
- Balanced training within each CV fold
- Improved sensitivity compared to no balancing
- Note: Balancing applied correctly (no data leakage)

#### Test 4.3: Downsample Majority Class
```r
JamoviTest::classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel),
    classifier = "singleDecisionTree",
    testing = "split",
    testSize = 0.3,
    balancingMethod = "downsample",
    reportMCC = TRUE,
    seed = 42
)
```
**Expected**:
- Reduced training set size (downsampled to minority)
- Balanced precision/recall

#### Test 4.4: SMOTE
```r
JamoviTest::classification(
    data = classification_binary_imbalanced,
    dep = RareEvent,
    indep = vars(RiskScore, Age, ExposureLevel),
    classifier = "logisticRegression",
    testing = "crossValidation",
    noOfFolds = 5,
    balancingMethod = "smote",
    reportClinicalMetrics = TRUE,
    seed = 42
)
```
**Expected**:
- If smotefamily package available: Synthetic examples
- Otherwise: WARNING fallback to upsampling

---

### 5. Multiclass Testing (Dataset 3: Multiclass)

#### Test 5.1: Multiclass Decision Tree
```r
JamoviTest::classification(
    data = classification_multiclass,
    dep = CancerStage,
    indep = vars(TumorSize, NodeCount, KI67, Differentiation, VascularInvasion),
    classifier = "singleDecisionTree",
    testing = "crossValidation",
    noOfFolds = 5,
    plotDecisionTree = TRUE,
    reporting = c("classifMetrices", "confusionMatrix"),
    seed = 42
)
```
**Expected**:
- 3×3 confusion matrix
- Per-class precision, recall, F-score
- Decision tree with 3 terminal classes

#### Test 5.2: Multiclass Random Forest
```r
JamoviTest::classification(
    data = classification_multiclass,
    dep = CancerStage,
    indep = vars(TumorSize, NodeCount, KI67, Differentiation),
    classifier = "randomForest",
    testing = "split",
    testSize = 0.25,
    noOfTrees = 200,
    printRandForest = TRUE,
    reporting = c("classifMetrices", "confusionMatrix"),
    seed = 42
)
```
**Expected**:
- Random forest OOB error
- 3-class confusion matrix
- Per-class metrics

---

### 6. Warning/Edge Case Testing

#### Test 6.1: Small Sample Warning (Dataset 4)
```r
JamoviTest::classification(
    data = classification_small_sample,
    dep = Outcome,
    indep = vars(Score1, Score2, Category),
    classifier = "singleDecisionTree",
    testing = "crossValidation",
    noOfFolds = 5
)
```
**Expected**:
- STRONG_WARNING: "Very small sample size (n=25)"
- WARNING: "Very small cross-validation fold size (avg. 5 samples per fold)"

#### Test 6.2: High Dimensionality Warning (Dataset 6)
```r
JamoviTest::classification(
    data = classification_high_dim,
    dep = Outcome,
    indep = vars(Var1, Var2, Var3, Var4, Var5, Var6, Var7, Var8, Var9, Var10,
                 Cat1, Cat2, Cat3, Cat4, Cat5),
    classifier = "randomForest",
    testing = "split",
    testSize = 0.3
)
```
**Expected**:
- WARNING: "High feature-to-sample ratio: 15 features with 50 samples (ratio: 3.3)"

#### Test 6.3: Missing Data Handling (Dataset 7)
```r
JamoviTest::classification(
    data = classification_with_missing,
    dep = Outcome,
    indep = vars(Predictor1, Predictor2, Predictor3, Category1, Category2),
    classifier = "logisticRegression",
    testing = "crossValidation",
    noOfFolds = 5
)
```
**Expected**:
- Complete case analysis (n reduced from 120 to ~100)
- Note in analysis summary about sample size

#### Test 6.4: Perfect Separation (Dataset 5)
```r
JamoviTest::classification(
    data = classification_perfect,
    dep = Outcome,
    indep = vars(PerfectPredictor, NoisyPredictor),
    classifier = "logisticRegression",
    testing = "split",
    testSize = 0.3
)
```
**Expected**:
- Possible convergence warnings (logistic regression)
- Perfect accuracy with PerfectPredictor

---

### 7. Educational Features Testing (Dataset 1)

#### Test 7.1: Natural Language Summary
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade, LymphNodeStatus),
    classifier = "randomForest",
    testing = "crossValidation",
    noOfFolds = 10,
    showSummary = TRUE,
    seed = 42
)
```
**Expected**:
- Plain-language summary panel
- Copy-ready text for clinical reports
- Per-class performance interpretation

#### Test 7.2: About Panel
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "singleDecisionTree",
    testing = "trainSet",
    showAbout = TRUE
)
```
**Expected**:
- Information about classification methods
- When to use each classifier
- Validation method explanations

#### Test 7.3: Statistical Glossary
```r
JamoviTest::classification(
    data = classification_binary_balanced,
    dep = Complication,
    indep = vars(Age, Biomarker, TumorGrade),
    classifier = "randomForest",
    testing = "split",
    testSize = 0.3,
    showGlossary = TRUE
)
```
**Expected**:
- Definitions of accuracy, precision, recall, F-score, AUC
- Clinical metrics explained (sensitivity, specificity, LRs)
- Class imbalance methods glossary

---

## Validation Checklist

### Correctness
- [ ] All classifiers produce results without errors
- [ ] Validation methods (holdout, CV, bootstrap) work correctly
- [ ] Class balancing applied within folds (no data leakage)
- [ ] Clinical metrics calculated correctly for binary classification
- [ ] Confusion matrices match predictions
- [ ] ROC curves and AUC values reasonable

### Warnings and Notices
- [ ] Small sample warning triggers at n<30
- [ ] Class imbalance warnings at 5% and 20% thresholds
- [ ] High dimensionality warning when n/p < 10
- [ ] Small fold size warning in CV
- [ ] Analysis complete INFO notice appears

### Edge Cases
- [ ] Perfect separation handled gracefully
- [ ] Missing data triggers complete case analysis
- [ ] Multiclass (3+ classes) works correctly
- [ ] Severe imbalance (9:1) triggers appropriate warnings

### Reproducibility
- [ ] Same seed produces identical results
- [ ] Different seeds produce different train/test splits
- [ ] Cross-validation folds consistent with seed

### Clinical Features
- [ ] Positive class correctly identified
- [ ] Manual positive class specification works
- [ ] Threshold optimization (Youden J) functional
- [ ] Manual threshold specification works
- [ ] Confidence intervals calculated correctly
- [ ] MCC interpretation accurate

### User Interface
- [ ] Plots render correctly (decision tree, ROC, frequencies)
- [ ] Tables populate without errors
- [ ] Educational panels display properly
- [ ] Glossary terms accurate and helpful

---

## Expected Performance Benchmarks

### Dataset 1 (Binary Balanced) - Typical Results
- **Decision Tree**: Accuracy 70-80%
- **Random Forest**: Accuracy 75-85%
- **Logistic Regression**: Accuracy 70-75%, AUC 0.75-0.85
- **KNN**: Accuracy 65-75%
- **SVM**: Accuracy 70-80%

### Dataset 2 (Binary Imbalanced) - With Balancing
- **No balancing**: Accuracy >90% (misleading), Poor sensitivity
- **With balancing**: Accuracy 70-85%, Balanced sensitivity/specificity
- **MCC**: 0.3-0.6 range expected

### Dataset 3 (Multiclass)
- **Random Forest**: Accuracy 60-75%
- **Decision Tree**: Accuracy 55-70%
- **Per-class F-scores**: 0.5-0.8 range

---

## Notes

1. **Reproducibility**: Always set `seed = 42` for consistent testing
2. **Balancing Verification**: Check that balancing doesn't inflate test performance
3. **Clinical Interpretation**: Verify clinical metrics make sense for the positive class
4. **Warning Thresholds**: Confirm warnings trigger at documented thresholds
5. **Complete Testing**: Run all 7 datasets through primary classifiers

---

## Quick Test Suite (Minimal)

```r
# Test 1: Basic functionality
classification_binary_balanced %>%
  classification(dep = Complication, indep = vars(Age, Biomarker, TumorGrade),
                 classifier = "randomForest", testing = "split", testSize = 0.3)

# Test 2: Clinical metrics
classification_binary_balanced %>%
  classification(dep = Complication, indep = vars(Age, Biomarker, TumorGrade),
                 classifier = "randomForest", testing = "crossValidation", noOfFolds = 5,
                 reportClinicalMetrics = TRUE, reportConfidenceIntervals = TRUE,
                 reportMCC = TRUE, reporting = c("classifMetrices", "AUC"))

# Test 3: Class imbalance
classification_binary_imbalanced %>%
  classification(dep = RareEvent, indep = vars(RiskScore, Age, ExposureLevel),
                 classifier = "randomForest", testing = "crossValidation", noOfFolds = 5,
                 balancingMethod = "upsample", reportMCC = TRUE)

# Test 4: Multiclass
classification_multiclass %>%
  classification(dep = CancerStage, indep = vars(TumorSize, NodeCount, KI67),
                 classifier = "randomForest", testing = "split", testSize = 0.3,
                 reporting = c("classifMetrices", "confusionMatrix"))

# Test 5: Warnings
classification_small_sample %>%
  classification(dep = Outcome, indep = vars(Score1, Score2),
                 classifier = "singleDecisionTree", testing = "crossValidation", noOfFolds = 5)
```

---

**Last Updated**: 2024-12-23
**Module Version**: ClinicoPath 0.0.3
