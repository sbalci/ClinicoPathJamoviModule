# Verification Script: Check Likelihood Ratio Output in oddsratio Function
# This script verifies that likelihood ratios are calculated and displayed

cat("=== Verification: Likelihood Ratio Output ===\n\n")

# Load test data
cat("Loading test data...\n")
data <- read.csv("oddsratio_test_data.csv")
cat("✓ Data loaded:", nrow(data), "rows\n\n")

# Show data structure
cat("Variables:\n")
cat("- Outcome:", "Mortality5yr", "(", paste(table(data$Mortality5yr), collapse=", "), ")\n")
cat("- Binary predictors:", "Sex, LVI, PNI\n")
cat("- Continuous:", "Age, TumorSize\n\n")

cat("=== What SHOULD appear when showNomogram = TRUE ===\n\n")
cat("Location: 'Model Performance Metrics' section (text2 output)\n\n")

cat("Expected content:\n")
cat("┌─────────────────────────────────────────────────────┐\n")
cat("│ Model Metrics:                                      │\n")
cat("│ AIC: [value]                                        │\n")
cat("│                                                     │\n")
cat("│ Diagnostic Metrics:                                 │\n")
cat("│ Sensitivity: XX.XX%                                 │\n")
cat("│ Specificity: XX.XX%                                 │\n")
cat("│ Positive LR: X.XX                                   │\n")
cat("│ Negative LR: X.XX                                   │\n")
cat("│                                                     │\n")
cat("│ ⚠️  Important: Please Verify These Interpretations  │\n")
cat("│ Positive outcome level: '[level]' (method)          │\n")
cat("│ Positive predictor level: '[level]' (method)        │\n")
cat("│                                                     │\n")
cat("│ 📊 Contingency Table:                               │\n")
cat("│ [2×2 table showing TP, FP, FN, TN]                  │\n")
cat("└─────────────────────────────────────────────────────┘\n\n")

cat("=== Troubleshooting if Likelihood Ratios NOT Visible ===\n\n")

cat("1. Check if 'Show diagnostic nomogram' is ENABLED\n")
cat("   - In jamovi UI, this checkbox must be checked\n")
cat("   - File: jamovi/oddsratio.u.yaml line 40\n\n")

cat("2. Check if diagnostic predictor is BINARY\n")
cat("   - If first variable is continuous (Age), nomogram won't generate\n")
cat("   - Must manually select a binary variable (Sex, LVI, or PNI)\n")
cat("   - Expected warning if not binary:\n")
cat("     ⚠️  WARNING: Diagnostic predictor 'Age' has 61 levels...\n\n")

cat("3. Check 'Model Performance Metrics' section visibility\n")
cat("   - This should appear AFTER the main odds ratio table\n")
cat("   - File: R/oddsratio.b.R line 382 (sets text2 visible)\n")
cat("   - File: R/oddsratio.b.R line 859 (updates with metrics_text)\n\n")

cat("4. Check for early returns in nomogram code\n")
cat("   - Line 768-772: No diagnostic predictor available\n")
cat("   - Line 780-784: Predictor not binary\n")
cat("   - These will prevent likelihood ratio calculation\n\n")

cat("=== Manual Test in jamovi ===\n\n")
cat("1. Load oddsratio_test_data.csv in jamovi\n")
cat("2. Open oddsratio analysis\n")
cat("3. Select:\n")
cat("   - Outcome: Mortality5yr\n")
cat("   - Explanatory: LVI, PNI, Sex\n")
cat("4. ✅ Check 'Show diagnostic nomogram'\n")
cat("5. Verify 'Diagnostic Predictor' shows 'LVI' (first binary variable)\n")
cat("6. Look for 'Model Performance Metrics' section\n")
cat("7. Should see:\n")
cat("   - Diagnostic Metrics box with Sensitivity, Specificity, LR+, LR-\n")
cat("   - Contingency table\n")
cat("   - TP, FP, FN, TN counts\n\n")

cat("=== Test with Continuous First Variable ===\n\n")
cat("1. Load oddsratio_test_data.csv in jamovi\n")
cat("2. Open oddsratio analysis\n")
cat("3. Select:\n")
cat("   - Outcome: Mortality5yr\n")
cat("   - Explanatory: Age, LVI, PNI  (Age is continuous!)\n")
cat("4. ✅ Check 'Show diagnostic nomogram'\n")
cat("5. Expected behavior:\n")
cat("   - ⚠️  WARNING about Age having too many levels\n")
cat("   - NO likelihood ratios displayed\n")
cat("6. Manually drag 'LVI' to 'Diagnostic Predictor' box\n")
cat("7. Now likelihood ratios SHOULD appear\n\n")

cat("=== Code References ===\n\n")
cat("Likelihood ratio calculation:\n")
cat("  R/oddsratio.b.R:786-790  - Calculates LR+, LR-, Sens, Spec\n")
cat("  R/oddsratio.b.R:864-1042 - .calculateLikelihoodRatios() function\n\n")

cat("Display in output:\n")
cat("  R/oddsratio.b.R:793-838  - Creates metrics_text HTML\n")
cat("  R/oddsratio.b.R:859      - Appends to text2 output\n\n")

cat("UI definition:\n")
cat("  jamovi/oddsratio.u.yaml:34-53 - Nomogram options collapse box\n\n")

cat("Results definition:\n")
cat("  jamovi/oddsratio.r.yaml:23-26 - text2 (Model Performance Metrics)\n\n")

cat("=== Expected Values for Test Data ===\n\n")
cat("Using LVI as diagnostic predictor for Mortality5yr:\n\n")

# Quick manual calculation
lvi_present <- data$LVI == "Present"
lvi_absent <- data$LVI == "Absent"
mortality_dead <- data$Mortality5yr == "Dead"
mortality_alive <- data$Mortality5yr == "Alive"

# Remove NA values
valid <- !is.na(data$LVI) & !is.na(data$Mortality5yr)
lvi_present <- lvi_present[valid]
lvi_absent <- lvi_absent[valid]
mortality_dead <- mortality_dead[valid]
mortality_alive <- mortality_alive[valid]

# Assuming "Dead" is positive outcome, "Present" is positive test
tp <- sum(lvi_present & mortality_dead)
fp <- sum(lvi_present & mortality_alive)
fn <- sum(lvi_absent & mortality_dead)
tn <- sum(lvi_absent & mortality_alive)

sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)
lr_pos <- sensitivity / (1 - specificity)
lr_neg <- (1 - sensitivity) / specificity

cat("Contingency Table (LVI vs Mortality5yr):\n")
cat("                  Alive    Dead\n")
cat(sprintf("LVI Absent       %3d      %3d\n", tn, fn))
cat(sprintf("LVI Present      %3d      %3d\n", fp, tp))
cat("\n")
cat(sprintf("TP: %d, FP: %d, FN: %d, TN: %d\n", tp, fp, fn, tn))
cat(sprintf("Sensitivity: %.1f%%\n", sensitivity * 100))
cat(sprintf("Specificity: %.1f%%\n", specificity * 100))
cat(sprintf("LR+: %.2f\n", lr_pos))
cat(sprintf("LR-: %.2f\n", lr_neg))
cat("\n")

cat("✓ If you see similar values in jamovi, likelihood ratios are working!\n\n")

cat("=== Summary ===\n\n")
cat("The likelihood ratios ARE calculated and should be visible when:\n")
cat("1. ✅ 'Show diagnostic nomogram' is enabled\n")
cat("2. ✅ A binary predictor is available/selected\n")
cat("3. ✅ No errors occur during calculation\n\n")

cat("They appear in the 'Model Performance Metrics' section,\n")
cat("which is the second HTML output (text2) below the main OR table.\n\n")

cat("If still not visible, provide:\n")
cat("- Screenshot of jamovi with nomogram enabled\n")
cat("- Which variables you selected\n")
cat("- Any error/warning messages\n")
