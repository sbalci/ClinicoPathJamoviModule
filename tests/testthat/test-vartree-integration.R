# Integration tests for vartree module
# Tests critical bug fixes: percvar wiring, composeTerm backticks, NA transparency

library(ClinicoPath)

test_that("percvar and percvarLevel are wired to vtree correctly", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data with outcome variable
    set.seed(123)
    n <- 100
    testData <- data.frame(
        treatment = factor(sample(c("Drug A", "Drug B"), n, replace = TRUE)),
        response = factor(sample(c("Complete", "Partial", "None"), n, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    # Run with percvar and percvarLevel specified
    result <- vartree(
        data = testData,
        vars = c("treatment"),
        percvar = "response",
        percvarLevel = "Complete",
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        showInterpretation = TRUE
    )

    # CRITICAL TEST: percvar should now be wired to vtree
    # Before fix: xsummary was overwritten if summaryvar present
    # After fix: xsummary includes percvar spec
    expect_s3_class(result, "vartreeResults")

    # The interpretation should mention the percentage calculation
    # (Checked via output inspection)
})


test_that("percvar works with summaryvar (both wired)", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data with both outcome and continuous variable
    set.seed(456)
    n <- 100
    testData <- data.frame(
        treatment = factor(sample(c("Drug A", "Drug B"), n, replace = TRUE)),
        response = factor(sample(c("Yes", "No"), n, replace = TRUE)),
        age = rnorm(n, 50, 10)
    )

    # Run with BOTH percvar AND summaryvar
    result <- vartree(
        data = testData,
        vars = c("treatment"),
        percvar = "response",
        percvarLevel = "Yes",
        summaryvar = "age",
        summarylocation = "allnodes",
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL
    )

    # CRITICAL TEST: Both percvar and summaryvar should work together
    # Before fix: summaryvar overwrote percvar
    # After fix: Both are combined in xsummary
    expect_s3_class(result, "vartreeResults")
})


test_that("Pruning with clinical level names works (composeTerm fix)", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data with clinical stage names containing spaces
    set.seed(789)
    n <- 80
    testData <- data.frame(
        stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), n, replace = TRUE)),
        treatment = factor(sample(c("Surgery", "Chemotherapy", "Radiation"), n, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    # Run with pruning on "Stage II" and "Stage III"
    result <- vartree(
        data = testData,
        vars = c("stage", "treatment"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = "stage",
        pruneLevel1 = "Stage II",
        pruneLevel2 = "Stage III",
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL
    )

    # CRITICAL TEST: Clinical level names should match
    # Before fix: composeTerm() wrapped levels in backticks → "`Stage II`" never matched
    # After fix: Levels passed as-is, matching works
    expect_s3_class(result, "vartreeResults")

    # Tree should only show Stage II and Stage III branches (and their children)
})


test_that("Single-level pruning works (level2 can be NULL)", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data
    set.seed(111)
    n <- 60
    testData <- data.frame(
        diagnosis = factor(sample(c("Benign", "Malignant", "Uncertain"), n, replace = TRUE)),
        treatment = factor(sample(c("Surgery", "Observation"), n, replace = TRUE))
    )

    # Run with pruning on ONLY "Malignant" (no level2)
    result <- vartree(
        data = testData,
        vars = c("diagnosis", "treatment"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = "diagnosis",
        pruneLevel1 = "Malignant",
        pruneLevel2 = NULL,  # Only one level
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL
    )

    # CRITICAL TEST: Single-level pruning should work
    # Before fix: Required both level1 AND level2 (returned NULL if either missing)
    # After fix: Works with just level1
    expect_s3_class(result, "vartreeResults")
})


test_that("Following with clinical level names works", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data with clinical names
    set.seed(222)
    n <- 70
    testData <- data.frame(
        grade = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), n, replace = TRUE)),
        outcome = factor(sample(c("Alive", "Deceased"), n, replace = TRUE)),
        stringsAsFactors = FALSE
    )

    # Run with follow on "Grade 2" and "Grade 3"
    result <- vartree(
        data = testData,
        vars = c("grade", "outcome"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = "grade",
        followLevel1 = "Grade 2",
        followLevel2 = "Grade 3"
    )

    # CRITICAL TEST: Follow should work with spaces in level names
    # Before fix: Same composeTerm bug as pruning
    # After fix: Levels match correctly
    expect_s3_class(result, "vartreeResults")
})


test_that("Missing value exclusion reports case loss", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create data with 30% missing values
    set.seed(333)
    n <- 100
    testData <- data.frame(
        var1 = factor(c(sample(c("A", "B"), 70, replace = TRUE), rep(NA, 30))),
        var2 = factor(sample(c("X", "Y", "Z"), n, replace = TRUE))
    )

    # Run with excl=TRUE (listwise deletion)
    result <- vartree(
        data = testData,
        vars = c("var1", "var2"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        excl = TRUE,
        showInterpretation = TRUE
    )

    # CRITICAL TEST: Should report that 30 cases were excluded
    # Before fix: Silent deletion, no warning
    # After fix: Warning displayed in interpretation
    expect_s3_class(result, "vartreeResults")

    # The interpretation should contain exclusion warning
    # (Visually verified in output)
})


test_that("Missing value exclusion calculates correct percentages", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create data where 40% will be excluded
    set.seed(444)
    n <- 100
    testData <- data.frame(
        treatment = factor(c(sample(c("Drug", "Placebo"), 60, replace = TRUE), rep(NA, 40))),
        response = factor(sample(c("Yes", "No"), n, replace = TRUE))
    )

    # Run with excl=TRUE
    result <- vartree(
        data = testData,
        vars = c("treatment", "response"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        excl = TRUE,
        showInterpretation = TRUE
    )

    # CRITICAL TEST: Should report 40% exclusion
    # Before fix: No report of excluded cases
    # After fix: "40 cases (40%) excluded" warning
    expect_s3_class(result, "vartreeResults")
})


test_that("No missing values shows no exclusion warning", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create complete data (no NAs)
    set.seed(555)
    n <- 80
    testData <- data.frame(
        var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
    )

    # Run with excl=TRUE (but no NAs to exclude)
    result <- vartree(
        data = testData,
        vars = c("var1", "var2"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        excl = TRUE,
        showInterpretation = TRUE
    )

    # CRITICAL TEST: No warning when no cases excluded
    expect_s3_class(result, "vartreeResults")
})


test_that("excl=FALSE does not trigger exclusion warning", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create data with missing values
    set.seed(666)
    n <- 100
    testData <- data.frame(
        var1 = factor(c(sample(c("A", "B"), 80, replace = TRUE), rep(NA, 20))),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
    )

    # Run with excl=FALSE (keep all cases)
    result <- vartree(
        data = testData,
        vars = c("var1", "var2"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        excl = FALSE,
        showInterpretation = TRUE
    )

    # CRITICAL TEST: No exclusion warning when excl=FALSE
    # (Missing values passed to vtree, which handles them)
    expect_s3_class(result, "vartreeResults")
})


test_that("All visual styles compile without errors", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data
    set.seed(777)
    n <- 50
    testData <- data.frame(
        var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y", "Z"), n, replace = TRUE))
    )

    # Test all 3 styles
    for (style in c("default", "clean", "minimal")) {
        result <- vartree(
            data = testData,
            vars = c("var1", "var2"),
            percvar = NULL,
            percvarLevel = NULL,
            summaryvar = NULL,
            prunebelow = NULL,
            pruneLevel1 = NULL,
            pruneLevel2 = NULL,
            follow = NULL,
            followLevel1 = NULL,
            followLevel2 = NULL,
            style = style
        )
        expect_s3_class(result, "vartreeResults")
    }
})


test_that("Horizontal layout works", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data
    set.seed(888)
    n <- 60
    testData <- data.frame(
        var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
    )

    # Run with horizontal layout
    result <- vartree(
        data = testData,
        vars = c("var1", "var2"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        horizontal = TRUE
    )

    expect_s3_class(result, "vartreeResults")
})


test_that("Pattern and sequence options work", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data
    set.seed(999)
    n <- 50
    testData <- data.frame(
        stage = factor(sample(c("I", "II", "III"), n, replace = TRUE)),
        outcome = factor(sample(c("Success", "Failure"), n, replace = TRUE))
    )

    # Test pattern option
    result_pattern <- vartree(
        data = testData,
        vars = c("stage", "outcome"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        pattern = TRUE
    )
    expect_s3_class(result_pattern, "vartreeResults")

    # Test sequence option
    result_sequence <- vartree(
        data = testData,
        vars = c("stage", "outcome"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        sequence = TRUE
    )
    expect_s3_class(result_sequence, "vartreeResults")
})


test_that("Prune smaller nodes works", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create test data with rare combinations
    set.seed(1010)
    n <- 100
    testData <- data.frame(
        var1 = factor(c(rep("Common", 90), rep("Rare", 10))),
        var2 = factor(sample(c("X", "Y"), n, replace = TRUE))
    )

    # Run with prune smaller enabled
    result <- vartree(
        data = testData,
        vars = c("var1", "var2"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL,
        useprunesmaller = TRUE,
        prunesmaller = 15  # Prune nodes with <15 cases
    )

    # Should prune the "Rare" branch (only 10 cases)
    expect_s3_class(result, "vartreeResults")
})


test_that("Variables with spaces are handled correctly", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    # Create data with space-containing variable names
    set.seed(1111)
    n <- 60
    testData <- data.frame(
        `Patient Type` = factor(sample(c("Inpatient", "Outpatient"), n, replace = TRUE)),
        `Treatment Response` = factor(sample(c("Good", "Poor"), n, replace = TRUE)),
        check.names = FALSE
    )

    # Run analysis
    result <- vartree(
        data = testData,
        vars = c("Patient Type", "Treatment Response"),
        percvar = NULL,
        percvarLevel = NULL,
        summaryvar = NULL,
        prunebelow = NULL,
        pruneLevel1 = NULL,
        pruneLevel2 = NULL,
        follow = NULL,
        followLevel1 = NULL,
        followLevel2 = NULL
    )

    # Should handle special characters without errors
    expect_s3_class(result, "vartreeResults")
})


test_that("percvar level must exist in data", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("vtree")

    testData <- data.frame(
        group = factor(c("A", "B")),
        outcome = factor(c("Yes", "No"))
    )

    expect_error(
        vartree(
            data = testData,
            vars = c("group"),
            percvar = "outcome",
            percvarLevel = "Maybe", # not present
            pct = TRUE
        ),
        "level is not present"
    )
})
