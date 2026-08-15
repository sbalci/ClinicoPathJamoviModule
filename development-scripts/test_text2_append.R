# Test script to verify text2 variable append logic
# This simulates the oddsratio function's text2 handling

cat("=== Testing text2 Variable Append Logic ===\n\n")

# Simulate the original text2 creation (like line 658)
text2 <- "
<br>
<b>Model Metrics:</b>
Number in dataframe = 246, Number in model = 246, Missing = 0, AIC = 312.1
<br>
"

cat("Initial text2:\n")
cat(text2)
cat("\n--- END Initial ---\n\n")

# Simulate metrics_text creation (like line 797)
metrics_text <- "
<div style='background-color: #f8f9fa; padding: 15px;'>
    <b>Diagnostic Metrics:</b><br>
    Sensitivity: 41.25%<br>
    Specificity: 58.86%<br>
    Positive LR: 1.00<br>
    Negative LR: 1.00<br>
</div>
"

cat("Metrics to append:\n")
cat(metrics_text)
cat("\n--- END Metrics ---\n\n")

# Simulate the append operation (like line 861-862)
text2 <- paste(text2, metrics_text, sep = "\n")

cat("Final text2 (after append):\n")
cat(text2)
cat("\n--- END Final ---\n\n")

# Check if both parts are present
if (grepl("Model Metrics", text2) && grepl("Diagnostic Metrics", text2)) {
    cat("✅ SUCCESS: Both Model Metrics and Diagnostic Metrics are present\n")
} else {
    cat("❌ FAILURE: Missing content\n")
    if (!grepl("Model Metrics", text2)) {
        cat("  - Missing Model Metrics\n")
    }
    if (!grepl("Diagnostic Metrics", text2)) {
        cat("  - Missing Diagnostic Metrics\n")
    }
}
