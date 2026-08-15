# Test script to verify survival outcome validation fixes
# Date: 2025-12-27

# Test Case 1: Numeric outcome with values 0,1 (should PASS)
test_numeric_01 <- function() {
    cat("\n=== Test 1: Numeric outcome (0,1) ===\n")

    test_data <- data.frame(
        time = c(12, 24, 36, 48, 60),
        outcome = c(0, 1, 0, 1, 0),  # Numeric 0/1
        group = factor(c("A", "B", "A", "B", "A"))
    )

    cat("Data structure:\n")
    print(str(test_data$outcome))
    cat("Unique values:", unique(test_data$outcome), "\n")
    cat("Expected: PASS - Numeric 0/1 should work\n")

    return(test_data)
}

# Test Case 1b: Numeric outcome with values 0,1 AND NAs (should PASS with INFO notice)
test_numeric_01_with_na <- function() {
    cat("\n=== Test 1b: Numeric outcome (0,1) WITH NAs ===\n")

    test_data <- data.frame(
        time = c(12, 24, 36, 48, 60, 72),
        outcome = c(0, 1, 0, 1, NA, 0),  # Numeric 0/1 with NA
        group = factor(c("A", "B", "A", "B", "A", "B"))
    )

    cat("Data structure:\n")
    print(str(test_data$outcome))
    cat("Unique values (including NA):", unique(test_data$outcome), "\n")
    cat("NA count:", sum(is.na(test_data$outcome)), "\n")
    cat("Expected: PASS - NAs silently excluded (no notice due to serialization limits)\n")

    return(test_data)
}

# Test Case 2: Factor outcome with levels "0","1" but NO outcomeLevel (should FAIL with helpful message)
test_factor_no_level <- function() {
    cat("\n=== Test 2: Factor outcome WITHOUT outcomeLevel ===\n")

    test_data <- data.frame(
        time = c(12, 24, 36, 48, 60),
        outcome = factor(c("0", "1", "0", "1", "0")),  # Factor with "0" and "1"
        group = factor(c("A", "B", "A", "B", "A"))
    )

    cat("Data structure:\n")
    print(str(test_data$outcome))
    cat("Levels:", levels(test_data$outcome), "\n")
    cat("Expected: FAIL - Factor requires outcomeLevel selection\n")
    cat("New error message should say: 'Event level must be specified for factor outcomes'\n")

    return(test_data)
}

# Test Case 3: Factor outcome WITH outcomeLevel specified (should PASS)
test_factor_with_level <- function() {
    cat("\n=== Test 3: Factor outcome WITH outcomeLevel ===\n")

    test_data <- data.frame(
        time = c(12, 24, 36, 48, 60),
        outcome = factor(c("Alive", "Dead", "Alive", "Dead", "Alive")),
        group = factor(c("A", "B", "A", "B", "A"))
    )

    cat("Data structure:\n")
    print(str(test_data$outcome))
    cat("Levels:", levels(test_data$outcome), "\n")
    cat("Expected: PASS when outcomeLevel='Dead'\n")

    return(test_data)
}

# Test Case 4: Invalid values (should FAIL with improved error message)
test_invalid_values <- function() {
    cat("\n=== Test 4: Invalid outcome values ===\n")

    test_data <- data.frame(
        time = c(12, 24, 36, 48, 60),
        outcome = c(0, 1, 2, 3, 4),  # Contains invalid values 3, 4
        group = factor(c("A", "B", "A", "B", "A"))
    )

    cat("Data structure:\n")
    print(str(test_data$outcome))
    cat("Unique values:", unique(test_data$outcome), "\n")
    cat("Expected: FAIL - Values 3,4 are invalid\n")
    cat("New error should say: 'Outcome recode produced values outside {0,1,2}: 3, 4'\n")

    return(test_data)
}

# Run demonstration
cat("\n" , paste(rep("=", 60), collapse=""), "\n")
cat("SURVIVAL OUTCOME VALIDATION TEST CASES\n")
cat(paste(rep("=", 60), collapse=""), "\n")

# Generate test data
data1 <- test_numeric_01()
data1b <- test_numeric_01_with_na()  # NEW: Test NA handling
data2 <- test_factor_no_level()
data3 <- test_factor_with_level()
data4 <- test_invalid_values()

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("SOLUTION FOR USER'S SPECIFIC CASE\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("\nIf you have outcome with values '0' and '1' and outcomeLevel=NULL:\n\n")
cat("1. Check if outcome is NUMERIC or FACTOR:\n")
cat("   str(your_data$Outcome)\n\n")
cat("2. If it's FACTOR, convert to numeric:\n")
cat("   your_data$Outcome <- as.numeric(as.character(your_data$Outcome))\n\n")
cat("3. OR specify outcomeLevel in the analysis:\n")
cat("   outcomeLevel = '1'  # Specify which level is the event\n\n")

cat(paste(rep("=", 60), collapse=""), "\n")
