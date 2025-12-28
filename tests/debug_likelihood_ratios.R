# Debug Script: Check why likelihood ratios aren't displaying
# Run this to simulate the oddsratio function behavior

cat("=== Debugging Likelihood Ratio Display ===\n\n")

# Load data
data <- read.csv("oddsratio_test_data.csv")
cat("✓ Data loaded\n\n")

# Simulate the analysis
outcome <- "Mortality5yr"
explanatory <- c("LVI", "Sex", "Age", "PreinvasiveComponent", "PNI", "Grade")
diagnostic_predictor <- "LVI"

cat("Selected variables:\n")
cat("  Outcome:", outcome, "\n")
cat("  Explanatory:", paste(explanatory, collapse=", "), "\n")
cat("  Diagnostic Predictor:", diagnostic_predictor, "\n\n")

# Check if LVI is binary
cat("Checking diagnostic predictor:\n")
cat("  Variable:", diagnostic_predictor, "\n")
cat("  Type:", class(data[[diagnostic_predictor]]), "\n")
cat("  Levels:", nlevels(factor(data[[diagnostic_predictor]])), "\n")
cat("  Values:", paste(unique(na.omit(data[[diagnostic_predictor]])), collapse=", "), "\n\n")

if (!is.factor(data[[diagnostic_predictor]])) {
    data[[diagnostic_predictor]] <- factor(data[[diagnostic_predictor]])
}

n_levels <- nlevels(data[[diagnostic_predictor]])
cat("  After conversion to factor:", n_levels, "levels\n\n")

if (n_levels != 2) {
    cat("❌ PROBLEM FOUND: Diagnostic predictor has", n_levels, "levels (not binary)\n")
    cat("   This would trigger early return, preventing likelihood ratio calculation\n")
    cat("   Expected: 2 levels\n")
    cat("   Actual:", paste(levels(data[[diagnostic_predictor]]), collapse=", "), "\n\n")
} else {
    cat("✅ Diagnostic predictor is binary\n")
    cat("   Levels:", paste(levels(data[[diagnostic_predictor]]), collapse=", "), "\n\n")
}

# Check outcome variable
cat("Checking outcome variable:\n")
if (!is.factor(data[[outcome]])) {
    data[[outcome]] <- factor(data[[outcome]])
}
cat("  Type:", class(data[[outcome]]), "\n")
cat("  Levels:", paste(levels(data[[outcome]]), collapse=", "), "\n\n")

# Create contingency table
cat("Creating contingency table:\n")
valid <- !is.na(data[[diagnostic_predictor]]) & !is.na(data[[outcome]])
predictor <- factor(data[[diagnostic_predictor]][valid])
outcome_var <- factor(data[[outcome]][valid])

cont_table <- table(predictor, outcome_var)
print(cont_table)
cat("\n")

# Calculate metrics manually
predictor_levels <- levels(predictor)
outcome_levels <- levels(outcome_var)

# Assume second level is positive (alphabetically)
positive_predictor_level <- predictor_levels[2]
positive_outcome_level <- outcome_levels[2]

cat("Assumed positive levels:\n")
cat("  Predictor positive level:", positive_predictor_level, "\n")
cat("  Outcome positive level:", positive_outcome_level, "\n\n")

# Calculate TP, FP, FN, TN
tp <- cont_table[predictor_levels[2], outcome_levels[2]]
fp <- cont_table[predictor_levels[2], outcome_levels[1]]
fn <- cont_table[predictor_levels[1], outcome_levels[2]]
tn <- cont_table[predictor_levels[1], outcome_levels[1]]

cat("Contingency table breakdown:\n")
cat(sprintf("  True Positives (TP): %d\n", tp))
cat(sprintf("  False Positives (FP): %d\n", fp))
cat(sprintf("  False Negatives (FN): %d\n", fn))
cat(sprintf("  True Negatives (TN): %d\n\n", tn))

# Calculate metrics
sensitivity <- tp / (tp + fn)
specificity <- tn / (tn + fp)

if (specificity == 1) {
    positive_lr <- Inf
} else {
    positive_lr <- sensitivity / (1 - specificity)
}

if (specificity == 0) {
    negative_lr <- Inf
} else {
    negative_lr <- (1 - sensitivity) / specificity
}

cat("Calculated metrics:\n")
cat(sprintf("  Sensitivity: %.2f%%\n", sensitivity * 100))
cat(sprintf("  Specificity: %.2f%%\n", specificity * 100))
cat(sprintf("  Positive LR: %.2f\n", positive_lr))
cat(sprintf("  Negative LR: %.2f\n\n", negative_lr))

# Create the HTML that SHOULD appear
cat("=== Expected HTML Output ===\n\n")
cat("This should appear in the 'Model Performance Metrics' section:\n")
cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

metrics_html <- glue::glue("
<div style='background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0;'>
    <b>Diagnostic Metrics:</b><br>
    Sensitivity: {format(sensitivity * 100, digits=2)}%<br>
    Specificity: {format(specificity * 100, digits=2)}%<br>
    Positive LR: {format(positive_lr, digits=2)}<br>
    Negative LR: {format(negative_lr, digits=2)}<br>
</div>

<div style='background-color: #e8f5e9; padding: 15px; border-radius: 8px; margin: 10px 0;'>
    <b>⚠️ Important: Please Verify These Interpretations</b><br>
    <small>
    <b>Positive outcome level:</b> '{positive_outcome_level}'<br>
    <b>Positive predictor level:</b> '{positive_predictor_level}'<br><br>

    <b>📊 Contingency Table:</b><br>
    <table style='border-collapse: collapse; margin: 5px 0;'>
        <tr><th style='border: 1px solid #ddd; padding: 5px;'></th>
            <th style='border: 1px solid #ddd; padding: 5px;'>{outcome_levels[1]}</th>
            <th style='border: 1px solid #ddd; padding: 5px;'>{outcome_levels[2]}</th></tr>
        <tr><td style='border: 1px solid #ddd; padding: 5px;'><b>{predictor_levels[1]}</b></td>
            <td style='border: 1px solid #ddd; padding: 5px;'>{cont_table[1,1]}</td>
            <td style='border: 1px solid #ddd; padding: 5px;'>{cont_table[1,2]}</td></tr>
        <tr><td style='border: 1px solid #ddd; padding: 5px;'><b>{predictor_levels[2]}</b></td>
            <td style='border: 1px solid #ddd; padding: 5px;'>{cont_table[2,1]}</td>
            <td style='border: 1px solid #ddd; padding: 5px;'>{cont_table[2,2]}</td></tr>
    </table>
    TP: {tp}, FP: {fp}, FN: {fn}, TN: {tn}<br>
    </small>
</div>
")

# Print plain text version
cat("Diagnostic Metrics:\n")
cat(sprintf("Sensitivity: %.2f%%\n", sensitivity * 100))
cat(sprintf("Specificity: %.2f%%\n", specificity * 100))
cat(sprintf("Positive LR: %.2f\n", positive_lr))
cat(sprintf("Negative LR: %.2f\n\n", negative_lr))

cat("Contingency Table:\n")
cat(sprintf("                %s    %s\n", outcome_levels[1], outcome_levels[2]))
cat(sprintf("%-15s %3d      %3d\n", predictor_levels[1], cont_table[1,1], cont_table[1,2]))
cat(sprintf("%-15s %3d      %3d\n", predictor_levels[2], cont_table[2,1], cont_table[2,2]))
cat(sprintf("\nTP: %d, FP: %d, FN: %d, TN: %d\n\n", tp, fp, fn, tn))

cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n")

cat("=== Troubleshooting ===\n\n")
cat("If this output doesn't appear in jamovi:\n\n")
cat("1. Check if there are any WARNING or INFO notices\n")
cat("2. Look for text mentioning 'likelihood ratio' or 'diagnostic metrics'\n")
cat("3. Try scrolling down in the 'Model Performance Metrics' section\n")
cat("4. Check the jamovi log for errors (Tools > Console)\n\n")

cat("Possible issues:\n")
cat("- Early return at line 768-772 (diagnostic predictor not available)\n")
cat("- Early return at line 780-784 (predictor not binary)\n")
cat("- Error in .calculateLikelihoodRatios() function\n")
cat("- paste() failure at line 842\n")
cat("- text2 not being updated/refreshed\n")
