# Test Group-based Coloring in Waterfall Plot
# This tests the new functionality added in response to GitHub issue #117

# Create test data with percentage changes and patient groups
test_data <- data.frame(
  PatientID = paste0("PT", 1:20),
  Response = c(-75, -60, -45, -35, -25, -15, -10, -5, 5, 10,
               15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
  TreatmentArm = rep(c("Arm A", "Arm B"), each = 10),
  DiseaseStage = sample(c("Early", "Advanced"), 20, replace = TRUE)
)

print("Test data created:")
print(head(test_data, 10))

# Test 1: Traditional RECIST coloring
cat("\n=== Test 1: RECIST Coloring (Default) ===\n")
result1 <- try({
  waterfall(
    data = test_data,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    colorBy = "recist",
    colorScheme = "jamovi",
    showWaterfallPlot = TRUE
  )
}, silent = FALSE)

if (inherits(result1, "try-error")) {
  cat("RECIST coloring test failed:", result1)
} else {
  cat("RECIST coloring test passed successfully!\n")
}

# Test 2: Group-based coloring by Treatment Arm
cat("\n=== Test 2: Group Coloring by Treatment Arm ===\n")
result2 <- try({
  waterfall(
    data = test_data,
    patientID = "PatientID",
    responseVar = "Response",
    inputType = "percentage",
    groupVar = "TreatmentArm",
    colorBy = "group",
    colorScheme = "colorful",
    showWaterfallPlot = TRUE
  )
}, silent = FALSE)

if (inherits(result2, "try-error")) {
  cat("Group coloring test failed:", result2)
} else {
  cat("Group coloring by treatment arm test passed successfully!\n")
}

# Test 3: Group-based coloring by Disease Stage  
cat("\n=== Test 3: Group Coloring by Disease Stage ===\n")
result3 <- try({
  waterfall(
    data = test_data,
    patientID = "PatientID", 
    responseVar = "Response",
    inputType = "percentage",
    groupVar = "DiseaseStage",
    colorBy = "group",
    colorScheme = "jamovi",
    showWaterfallPlot = TRUE
  )
}, silent = FALSE)

if (inherits(result3, "try-error")) {
  cat("Group coloring by disease stage test failed:", result3)
} else {
  cat("Group coloring by disease stage test passed successfully!\n")
}

# Test 4: Invalid scenario (group selected but no group variable)
cat("\n=== Test 4: Error Handling (No Group Variable) ===\n")
result4 <- try({
  waterfall(
    data = test_data,
    patientID = "PatientID",
    responseVar = "Response", 
    inputType = "percentage",
    colorBy = "group",  # Group coloring without groupVar
    colorScheme = "jamovi",
    showWaterfallPlot = TRUE
  )
}, silent = FALSE)

if (inherits(result4, "try-error")) {
  cat("Error handling test passed - correctly failed when no group variable provided\n")
} else {
  cat("Error handling test: Function should gracefully handle missing group variable\n")
}

cat("\n=== Test Summary ===\n")
cat("All tests completed. The new group-based coloring feature should now be available!\n")
cat("Users can:\n")
cat("1. Select a Group Variable (e.g., treatment arm, disease subtype)\n")
cat("2. Set 'Color By' to 'Patient Groups'\n") 
cat("3. Choose 'Colorful' color scheme for best group distinction\n")
cat("4. View waterfall plot with group-based colors instead of RECIST colors\n")