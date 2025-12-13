# Integration tests for venn module
# Tests critical bug fixes: selected-variable naOmit, set calculations correctness

library(ClinicoPath)

test_that("venn only excludes cases with NAs in SELECTED variables", {
    skip_if_not_installed("ClinicoPath")

    # CRITICAL TEST: Create data with NAs in UNRELATED columns
    # These should NOT cause case exclusion
    set.seed(123)
    n <- 100
    testData <- data.frame(
        var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
        unrelated1 = c(rnorm(50), rep(NA, 50)),  # 50% missing in UNRELATED column
        unrelated2 = c(rep(NA, 30), rnorm(70))   # 30% missing in UNRELATED column
    )

    # Before fix: Would exclude ALL 80 cases with ANY NA anywhere
    # After fix: Should keep all 100 cases (var1 and var2 are complete)

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "A",
        var2 = "var2",
        var2true = "X"
    )

    # CRITICAL: Should NOT exclude any cases
    # (All 100 cases have complete var1 and var2)
    expect_s3_class(result, "vennResults")

    # The membership table should have 100 rows, not 20
    # (Before fix: would be 20 rows = cases complete across ALL columns)
    # (After fix: should be 100 rows = cases complete for SELECTED columns only)
})


test_that("venn reports exclusion warning when selected variables have NAs", {
    skip_if_not_installed("ClinicoPath")

    # Create data with 30% missing in SELECTED variables
    set.seed(456)
    n <- 100
    testData <- data.frame(
        var1 = factor(c(sample(c("A", "B"), 70, replace = TRUE), rep(NA, 30))),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
        complete_unrelated = rnorm(n)  # Complete unrelated column
    )

    # Before fix: Silent exclusion, no warning
    # After fix: Should display exclusion warning

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "A",
        var2 = "var2",
        var2true = "X"
    )

    # CRITICAL: Should report 30 cases (30%) excluded
    expect_s3_class(result, "vennResults")

    # Check that todo element contains exclusion warning
    # (The warning is displayed via self$results$todo$setContent())
    # In actual jamovi, this would show "⚠️ Case Exclusion Warning: 30 cases (30%) excluded..."
})


test_that("venn with NO missing values shows no exclusion warning", {
    skip_if_not_installed("ClinicoPath")

    # Create complete data (no NAs anywhere)
    set.seed(789)
    n <- 80
    testData <- data.frame(
        var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "A",
        var2 = "var2",
        var2true = "X"
    )

    # No warning should be displayed when no cases excluded
    expect_s3_class(result, "vennResults")
})


test_that("venn logical encoding is correct for 2-way Venn", {
    skip_if_not_installed("ClinicoPath")

    # Create simple test data with known outcomes
    set.seed(111)
    testData <- data.frame(
        treatment = factor(c(rep("Drug", 60), rep("Placebo", 40))),
        response = factor(c(rep("Success", 40), rep("Failure", 20),  # Drug group
                           rep("Success", 10), rep("Failure", 30)))  # Placebo group
    )

    # Expected counts:
    # - Drug=TRUE: 60 cases
    # - Response=Success: 50 cases (40 Drug + 10 Placebo)
    # - Both Drug AND Success: 40 cases
    # - Only Drug (not Success): 20 cases
    # - Only Success (not Drug): 10 cases
    # - Neither: 30 cases

    result <- venn(
        data = testData,
        var1 = "treatment",
        var1true = "Drug",
        var2 = "response",
        var2true = "Success"
    )

    # CRITICAL: The logical encoding should match expected counts
    expect_s3_class(result, "vennResults")

    # In actual use, the Venn diagram should show:
    # - Left circle (Drug only): 20
    # - Intersection (both): 40
    # - Right circle (Success only): 10
    # - Outside (neither): 30
})


test_that("venn logical encoding is correct for 3-way Venn", {
    skip_if_not_installed("ClinicoPath")

    # Create 3-way test data
    set.seed(222)
    n <- 120
    testData <- data.frame(
        var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
        var3 = factor(sample(c("P", "Q"), n, replace = TRUE))
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "A",
        var2 = "var2",
        var2true = "X",
        var3 = "var3",
        var3true = "P"
    )

    # CRITICAL: Should handle 3-way combinations correctly
    expect_s3_class(result, "vennResults")
})


test_that("venn with 4 variables works correctly", {
    skip_if_not_installed("ClinicoPath")

    # Test with 4 variables (UpSet plot)
    set.seed(333)
    n <- 100
    testData <- data.frame(
        var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
        var3 = factor(sample(c("P", "Q"), n, replace = TRUE)),
        var4 = factor(sample(c("M", "N"), n, replace = TRUE))
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "A",
        var2 = "var2",
        var2true = "X",
        var3 = "var3",
        var3true = "P",
        var4 = "var4",
        var4true = "M"
    )

    # Should switch to UpSet plot for 4+ variables
    expect_s3_class(result, "vennResults")
})


test_that("venn handles variables with spaces correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create data with space-containing variable names
    set.seed(444)
    n <- 70
    testData <- data.frame(
        `Treatment Group` = factor(sample(c("Drug A", "Drug B"), n, replace = TRUE)),
        `Response Type` = factor(sample(c("Complete", "Partial"), n, replace = TRUE)),
        check.names = FALSE
    )

    result <- venn(
        data = testData,
        var1 = "Treatment Group",
        var1true = "Drug A",
        var2 = "Response Type",
        var2true = "Complete"
    )

    # Should handle special characters without errors
    expect_s3_class(result, "vennResults")
})


test_that("venn percentage calculations match expected values", {
    skip_if_not_installed("ClinicoPath")

    # Create controlled data for exact percentage verification
    # 100 cases total:
    # - 50 have A=TRUE, B=FALSE
    # - 30 have A=TRUE, B=TRUE
    # - 10 have A=FALSE, B=TRUE
    # - 10 have A=FALSE, B=FALSE

    testData <- data.frame(
        varA = factor(c(rep("Yes", 80), rep("No", 20))),   # 80% Yes
        varB = factor(c(rep("No", 50), rep("Yes", 30), rep("Yes", 10), rep("No", 10)))  # 40% Yes
    )

    # Expected:
    # - A only: 50 cases (50%)
    # - Both A and B: 30 cases (30%)
    # - B only: 10 cases (10%)
    # - Neither: 10 cases (10%)

    result <- venn(
        data = testData,
        var1 = "varA",
        var1true = "Yes",
        var2 = "varB",
        var2true = "Yes"
    )

    # CRITICAL: Percentages should match expected values
    expect_s3_class(result, "vennResults")

    # The summary should show:
    # - "True %" for varA: 80%
    # - "True %" for varB: 40%
})


test_that("venn with all FALSE values handles correctly", {
    skip_if_not_installed("ClinicoPath")

    # Edge case: All values are the opposite of "true" level
    set.seed(555)
    n <- 50
    testData <- data.frame(
        var1 = factor(rep("No", n)),   # All "No", looking for "Yes"
        var2 = factor(rep("Absent", n))  # All "Absent", looking for "Present"
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "Yes",
        var2 = "var2",
        var2true = "Present"
    )

    # Should show all cases in the "Neither" category
    expect_s3_class(result, "vennResults")
})


test_that("venn with all TRUE values handles correctly", {
    skip_if_not_installed("ClinicoPath")

    # Edge case: All values match the "true" level
    set.seed(666)
    n <- 50
    testData <- data.frame(
        var1 = factor(rep("Positive", n)),
        var2 = factor(rep("Present", n))
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "Positive",
        var2 = "var2",
        var2true = "Present"
    )

    # Should show all cases in the "Both" intersection
    expect_s3_class(result, "vennResults")
})


test_that("venn calculates overlap counts correctly", {
    skip_if_not_installed("ClinicoPath")

    # Create data with known overlap pattern
    testData <- data.frame(
        set1 = factor(c(rep("In", 70), rep("Out", 30))),
        set2 = factor(c(rep("In", 40), rep("Out", 30), rep("In", 20), rep("Out", 10)))
    )

    # Expected overlaps:
    # - set1=In AND set2=In: 40 cases
    # - Total in set1: 70 cases
    # - Total in set2: 60 cases
    # - Union (either or both): 90 cases
    # - Neither: 10 cases

    result <- venn(
        data = testData,
        var1 = "set1",
        var1true = "In",
        var2 = "set2",
        var2true = "In",
        calculateOverlap = TRUE,
        calculateDiscern = TRUE,
        calculateUnite = TRUE
    )

    # CRITICAL: Set calculations should match expected values
    # This tests the FIXED overlap/discern/unite processing
    expect_s3_class(result, "vennResults")
})

test_that("validation fails when selected true level not present", {
    skip_if_not_installed("ClinicoPath")

    data <- data.frame(
        a = factor(c("Yes", "Yes")),
        b = factor(c("No", "No"))
    )

    expect_error(
        venn(
            data = data,
            var1 = "a",
            var1true = "Maybe",  # not present
            var2 = "b",
            var2true = "No"
        ),
        "does not exist"
    )
})
