# Verification script for alluvial function
library(jmvcore)
library(easyalluvial)
library(ggalluvial)
library(ggplot2)

# Load the package functions (sourcing the file directly for testing)
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/alluvial.b.R")
source("/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/alluvial.h.R")

# Create dummy data
set.seed(123)
N <- 100
data <- data.frame(
  Stage = sample(c("I", "II", "III", "IV"), N, replace = TRUE),
  Grade = sample(c("Low", "High"), N, replace = TRUE),
  Response = sample(c("Complete", "Partial", "None"), N, replace = TRUE),
  Weight = runif(N, 1, 10),
  stringsAsFactors = TRUE
)

# Test 1: Basic Alluvial Plot (EasyAlluvial)
print("Testing Basic Alluvial (EasyAlluvial)...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "easyalluvial"
  )
  print("Basic Alluvial: Success")
}, error = function(e) {
  print(paste("Basic Alluvial: Failed -", e$message))
})

# Test 2: GG Alluvial Engine
print("Testing GG Alluvial Engine...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "ggalluvial"
  )
  print("GG Alluvial: Success")
}, error = function(e) {
  print(paste("GG Alluvial: Failed -", e$message))
})

# Test 3: Weighted Alluvial (GG Alluvial)
print("Testing Weighted Alluvial (GG Alluvial)...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "ggalluvial",
    weight = "Weight"
  )
  print("Weighted GG Alluvial: Success")
}, error = function(e) {
  print(paste("Weighted GG Alluvial: Failed -", e$message))
})

# Test 4: Weighted Alluvial (EasyAlluvial) - Check if it runs (even if ignored)
print("Testing Weighted Alluvial (EasyAlluvial)...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "easyalluvial",
    weight = "Weight"
  )
  print("Weighted EasyAlluvial: Success (Note: Weight might be ignored)")
}, error = function(e) {
  print(paste("Weighted EasyAlluvial: Failed -", e$message))
})

# Test 5: Sankey Style (GG Alluvial)
print("Testing Sankey Style...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "ggalluvial",
    sankeyStyle = TRUE
  )
  print("Sankey Style: Success")
}, error = function(e) {
  print(paste("Sankey Style: Failed -", e$message))
})

# Test 6: Condensation Plot
print("Testing Condensation Plot...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    condensationvar = "Grade"
  )
  print("Condensation Plot: Success")
}, error = function(e) {
  print(paste("Condensation Plot: Failed -", e$message))
})

# Test 7: Marginal Histograms (EasyAlluvial only)
print("Testing Marginal Histograms...")
tryCatch({
  alluvial(
    data = data,
    vars = c("Stage", "Grade", "Response"),
    engine = "easyalluvial",
    marg = TRUE
  )
  print("Marginal Histograms: Success")
}, error = function(e) {
  print(paste("Marginal Histograms: Failed -", e$message))
})
