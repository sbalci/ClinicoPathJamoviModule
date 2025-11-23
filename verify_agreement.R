
# Verification script for agreement function

library(jmvcore)
library(irr)
library(irrCAC)
library(lme4)

# Load the package functions (sourcing the file directly for testing)
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/agreement.b.R")
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/agreement.h.R")

# Mock the R6 class environment if needed, or use the generated function if available
# Since we are testing the R6 class logic, we might need to instantiate it or use the wrapper if it exists.
# However, the wrapper `agreement` is likely in `R/agreement.h.R`.

# Let's try to use the `agreement` function directly if it's exported or available.
# If not, we'll inspect the R6 class.

# Create dummy data
set.seed(123)
data <- data.frame(
  Rater1 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
  Rater2 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
  Rater3 = factor(sample(c("A", "B", "C"), 50, replace = TRUE)),
  Cluster = factor(rep(1:5, each = 10)),
  Subject = 1:50
)

# Test 1: Basic Kappa (2 raters)
print("Testing Basic Kappa...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2"),
    sft = TRUE
  )
  print("Basic Kappa: Success")
  # print(res) # Cannot easily print jmv results in script without rendering
}, error = function(e) {
  print(paste("Basic Kappa: Failed -", e$message))
})

# Test 2: Fleiss Kappa (3 raters)
print("Testing Fleiss Kappa...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2", "Rater3")
  )
  print("Fleiss Kappa: Success")
}, error = function(e) {
  print(paste("Fleiss Kappa: Failed -", e$message))
})

# Test 3: Weighted Kappa (Ordinal)
print("Testing Weighted Kappa...")
data_ord <- data
data_ord$Rater1 <- factor(data_ord$Rater1, levels = c("A", "B", "C"), ordered = TRUE)
data_ord$Rater2 <- factor(data_ord$Rater2, levels = c("A", "B", "C"), ordered = TRUE)

tryCatch({
  res <- agreement(
    data = data_ord,
    vars = c("Rater1", "Rater2"),
    wght = "squared"
  )
  print("Weighted Kappa: Success")
}, error = function(e) {
  print(paste("Weighted Kappa: Failed -", e$message))
})

# Test 4: Gwet's AC1
print("Testing Gwet's AC1...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2"),
    gwet = TRUE,
    gwetWeights = "unweighted"
  )
  print("Gwet's AC1: Success")
}, error = function(e) {
  print(paste("Gwet's AC1: Failed -", e$message))
})

# Test 5: Krippendorff's Alpha
print("Testing Krippendorff's Alpha...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2"),
    kripp = TRUE,
    krippMethod = "nominal"
  )
  print("Krippendorff's Alpha: Success")
}, error = function(e) {
  print(paste("Krippendorff's Alpha: Failed -", e$message))
})

# Test 6: Hierarchical Kappa
print("Testing Hierarchical Kappa...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2"),
    hierarchicalKappa = TRUE,
    clusterVariable = "Cluster",
    iccHierarchical = TRUE,
    betweenClusterVariance = TRUE
  )
  print("Hierarchical Kappa: Success")
}, error = function(e) {
  print(paste("Hierarchical Kappa: Failed -", e$message))
})

# Test 7: Consensus Variable
print("Testing Consensus Variable...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2", "Rater3"),
    consensusVar = TRUE,
    consensusRule = "majority"
  )
  print("Consensus Variable: Success")
}, error = function(e) {
  print(paste("Consensus Variable: Failed -", e$message))
})

# Test 8: Pairwise Analysis
print("Testing Pairwise Analysis...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2", "Rater3"),
    referenceRater = "Rater1",
    rankRaters = TRUE
  )
  print("Pairwise Analysis: Success")
}, error = function(e) {
  print(paste("Pairwise Analysis: Failed -", e$message))
})

# Test 9: Bland-Altman
# Need numeric data for Bland-Altman
data_num <- data.frame(
  M1 = rnorm(50, 10, 2),
  M2 = rnorm(50, 10.5, 2)
)

print("Testing Bland-Altman...")
tryCatch({
  res <- agreement(
    data = data_num,
    vars = c("M1", "M2"),
    blandAltmanPlot = TRUE,
    baConfidenceLevel = 0.95,
    proportionalBias = TRUE
  )
  print("Bland-Altman: Success")
}, error = function(e) {
  print(paste("Bland-Altman: Failed -", e$message))
})

# Test 10: Level of Agreement (LoA)
print("Testing Level of Agreement...")
tryCatch({
  res <- agreement(
    data = data,
    vars = c("Rater1", "Rater2"),
    loaVariable = TRUE,
    loaThresholds = "quartiles"
  )
  print("Level of Agreement: Success")
}, error = function(e) {
  print(paste("Level of Agreement: Failed -", e$message))
})

# Test 11: Kappa with Numeric Input (should be treated as ordered factors)
print("Testing Kappa with Numeric Input...")
data_num_cat <- data.frame(
  Rater1 = sample(1:3, 50, replace = TRUE),
  Rater2 = sample(1:3, 50, replace = TRUE)
)
tryCatch({
  res <- agreement(
    data = data_num_cat,
    vars = c("Rater1", "Rater2"),
    wght = "squared"
  )
  print("Kappa with Numeric Input: Success")
}, error = function(e) {
  print(paste("Kappa with Numeric Input: Failed -", e$message))
})

# Test 12: Hierarchical ICC with Numeric Input
print("Testing Hierarchical ICC with Numeric Input...")
data_icc <- data.frame(
  Rater1 = rnorm(50, 10, 2),
  Rater2 = rnorm(50, 10.5, 2),
  Cluster = factor(rep(1:5, each = 10))
)
tryCatch({
  res <- agreement(
    data = data_icc,
    vars = c("Rater1", "Rater2"),
    hierarchicalKappa = TRUE,
    clusterVariable = "Cluster",
    iccHierarchical = TRUE
  )
  print("Hierarchical ICC with Numeric Input: Success")
}, error = function(e) {
  print(paste("Hierarchical ICC with Numeric Input: Failed -", e$message))
})

