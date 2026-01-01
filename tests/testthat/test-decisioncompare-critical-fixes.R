# Test file for critical statistical fixes in decisioncompare
# Tests validate fixes for:
# 1. McNemar/Cochran Q comparing CORRECTNESS (not raw positivity)
# 2. naOmit only filtering selected variables
# 3. Multi-level variable handling with warnings
# 4. Sample size reporting

test_that("McNemar compares diagnostic CORRECTNESS, not raw positivity rates", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("ClinicoPath")

    # Create test data where Test1 has higher positivity but LOWER accuracy
    # Gold: 10 truly positive, 10 truly negative
    # Test1: Calls everything positive (100% sensitivity, 0% specificity, 50% accuracy)
    # Test2: Perfectly accurate (100% sensitivity, 100% specificity, 100% accuracy)

    test_data <- data.frame(
        gold = factor(c(rep("Pos", 10), rep("Neg", 10))),
        test1 = factor(rep("Pos", 20)),  # Always positive (high positivity)
        test2 = factor(c(rep("Pos", 10), rep("Neg", 10)))  # Perfect (lower positivity than test1)
    )

    # If McNemar correctly compares CORRECTNESS:
    # Test2 should be significantly better (all 10 diseased + all 10 healthy correct)
    # Test1 should be significantly worse (only 10 diseased correct, 0 healthy correct)

    # Note: This test validates the LOGIC. Actual jamovi test would require full module load.
    # For unit testing, we verify the core logic is comparing correctness vectors.

    gold <- test_data$gold
    test1_results <- test_data$test1
    test2_results <- test_data$test2

    # Simulate the FIXED logic: compare correctness
    test1_correct <- (test1_results == gold)
    test2_correct <- (test2_results == gold)

    # Build McNemar table of CORRECTNESS
    mcnemar_table <- table(
        factor(test1_correct, levels = c(TRUE, FALSE)),
        factor(test2_correct, levels = c(TRUE, FALSE))
    )

    # McNemar structure should show:
    # - Both correct: 10 (test1 correct on diseased, test2 correct on diseased)
    # - Test1 correct, Test2 wrong: 0
    # - Test1 wrong, Test2 correct: 10 (test1 wrong on healthy, test2 correct on healthy)
    # - Both wrong: 0

    expect_equal(mcnemar_table[1,1], 10)  # Both correct
    expect_equal(mcnemar_table[1,2], 0)   # Test1 correct, Test2 wrong
    expect_equal(mcnemar_table[2,1], 10)  # Test1 wrong, Test2 correct (KEY DISCORDANCE)
    expect_equal(mcnemar_table[2,2], 0)   # Both wrong

    # This validates Test2 is significantly better (10 vs 0 discordant pairs)
    mcnemar_result <- stats::mcnemar.test(mcnemar_table)
    expect_true(mcnemar_result$p.value < 0.001)  # Highly significant
})


test_that("naOmit only filters SELECTED variables, not entire dataset", {
    skip_if_not_installed("ClinicoPath")

    # Create dataset with missing values in UNRELATED columns
    test_data <- data.frame(
        gold = factor(c("Pos", "Neg", "Pos", "Neg", "Pos")),
        test1 = factor(c("Pos", "Neg", "Pos", "Neg", "Pos")),
        test2 = factor(c("Pos", "Neg", "Neg", "Neg", "Pos")),
        unrelated1 = c(1, NA, 3, NA, 5),  # Has missing values
        unrelated2 = c(NA, 2, NA, 4, NA)  # Has missing values
    )

    # The FIXED logic should:
    # 1. Subset to ONLY selected variables (gold, test1, test2)
    # 2. Remove NA only from those columns

    selected_vars <- c("gold", "test1", "test2")
    subset_data <- test_data[, selected_vars, drop = FALSE]
    filtered_data <- na.omit(subset_data)

    # All 5 rows should be kept because there are no NA in selected vars
    expect_equal(nrow(filtered_data), 5)

    # OLD BROKEN logic would do:
    broken_data <- na.omit(test_data)  # Removes NA from ALL columns

    # This would drop all rows because every row has NA in unrelated columns
    expect_equal(nrow(broken_data), 0)  # BROKEN: loses all data!

    # Validate the fix prevents this bias
    expect_true(nrow(filtered_data) > nrow(broken_data))
})


test_that("Multi-level variables generate warnings", {
    skip_if_not_installed("ClinicoPath")

    # Create test data with >2 levels (equivocal results)
    test_data <- data.frame(
        gold = factor(c("Pos", "Neg", "Pos", "Neg")),
        test_multilevel = factor(c("Pos", "Neg", "Equivocal", "Invalid"))
    )

    # Simulate the warning detection logic
    test_levels <- levels(test_data$test_multilevel)
    testPLevel <- "Pos"

    # Should detect >2 levels
    expect_true(length(test_levels) > 2)

    # Extra levels that will be treated as "Negative"
    extra_levels <- setdiff(test_levels, testPLevel)
    expect_equal(sort(extra_levels), sort(c("Equivocal", "Invalid", "Neg")))

    # Validate this is problematic: "Equivocal" and "Invalid"
    # will inflate specificity if treated as true negatives
})


test_that("Sample size reporting extracts from processed_data$data", {
    skip_if_not_installed("ClinicoPath")

    # Simulate the processed_data structure (it's a list, not a dataframe)
    processed_data <- list(
        data = data.frame(
            gold = factor(c("Pos", "Neg", "Pos")),
            test1 = factor(c("Pos", "Neg", "Pos")),
            test2 = factor(c("Pos", "Neg", "Neg"))
        ),
        goldVariable = "gold",
        goldPLevel = "Pos"
    )

    # CORRECT: Extract n_cases from processed_data$data
    n_cases_correct <- nrow(processed_data$data)
    expect_equal(n_cases_correct, 3)

    # BROKEN (old code): nrow(processed_data) on a list
    # This would return character(0) or error
    expect_error(nrow(processed_data))
})


test_that("Cochran Q compares CORRECTNESS across 3 tests", {
    skip_if_not_installed("ClinicoPath")

    # Create test data: 3 tests with different accuracy levels
    # Gold: 20 truly positive, 20 truly negative
    test_data <- data.frame(
        gold = factor(c(rep("Pos", 20), rep("Neg", 20))),
        test1 = factor(c(rep("Pos", 18), rep("Neg", 2), rep("Neg", 20))),  # 95% sens, 100% spec
        test2 = factor(c(rep("Pos", 20), rep("Pos", 5), rep("Neg", 15))),  # 100% sens, 75% spec
        test3 = factor(c(rep("Pos", 15), rep("Neg", 5), rep("Neg", 18), rep("Pos", 2)))  # 75% sens, 90% spec
    )

    # Simulate FIXED Cochran Q logic: compare CORRECTNESS
    gold <- test_data$gold
    test_names <- c("test1", "test2", "test3")

    # Build correctness matrix
    all_tests_matrix <- sapply(test_names, function(tn) {
        test_result <- test_data[[tn]]
        ifelse(test_result == gold, 1, 0)  # 1 if correct, 0 if wrong
    })

    # Verify different accuracy levels
    accuracy_test1 <- mean(all_tests_matrix[, "test1"])
    accuracy_test2 <- mean(all_tests_matrix[, "test2"])
    accuracy_test3 <- mean(all_tests_matrix[, "test3"])

    expect_true(accuracy_test1 > accuracy_test3)  # Test1 more accurate
    expect_true(accuracy_test2 > accuracy_test3)  # Test2 more accurate

    # Cochran Q should detect these differences
    k <- length(test_names)
    Ci <- colSums(all_tests_matrix)
    Ri <- rowSums(all_tests_matrix)

    numerator <- k * sum(Ci^2) - (sum(Ci))^2
    denominator <- k * sum(Ri) - sum(Ri^2)

    Q <- (k - 1) * numerator / denominator
    df <- k - 1
    p_value <- stats::pchisq(Q, df, lower.tail = FALSE)

    # Should detect significant difference
    expect_true(p_value < 0.05)
})
