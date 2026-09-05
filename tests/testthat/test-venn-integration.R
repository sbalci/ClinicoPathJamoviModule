# Integration tests for venn module
# Tests critical bug fixes: selected-variable naOmit, set calculations correctness
#
# NOTE: these carried skip_if_not_installed("ClinicoPath") on every test and a
# skip_if_not_installed('jmvReadWrite') that guarded nothing. ClinicoPath is not
# *installed* under devtools::load_all() or a sourced tree, so all 12 tests
# skipped silently in the normal dev loop and only ever ran against a built
# package. The package under test is loaded by the harness, so no guard is needed.


test_that("venn only excludes cases with NAs in SELECTED variables", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: Should NOT exclude any cases
    # (All 100 cases have complete var1 and var2)
    expect_s3_class(result, "vennResults")

    # var1/var2 are complete, so all 100 cases survive and nothing is excluded
    expect_equal(result$summary$asDF$totalCount, c(100L, 100L))
    expect_false(grepl("CASE EXCLUSION",
                       paste(as.character(result$validationWarnings$content), collapse = " ")))

    # The membership table should have 100 rows, not 20
    # (Before fix: would be 20 rows = cases complete across ALL columns)
    # (After fix: should be 100 rows = cases complete for SELECTED columns only)
})


test_that("venn reports exclusion warning when selected variables have NAs", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: Should report 30 cases (30%) excluded
    expect_s3_class(result, "vennResults")

    # 30 of 100 cases are missing on var1
    txt <- paste(as.character(result$validationWarnings$content), collapse = " ")
    expect_match(txt, "CASE EXCLUSION")
    expect_match(txt, "Original N=100")
    expect_match(txt, "Final N=70")
    expect_equal(result$summary$asDF$totalCount, c(70L, 70L))

    # Check that todo element contains exclusion warning
    # (The warning is displayed via the validationWarnings panel)
    # In actual jamovi, this would show "⚠️ Case Exclusion Warning: 30 cases (30%) excluded..."
})


test_that("venn with NO missing values shows no exclusion warning", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # No warning should be displayed when no cases excluded
    expect_s3_class(result, "vennResults")

    expect_false(grepl("CASE EXCLUSION",
                       paste(as.character(result$validationWarnings$content), collapse = " ")))
    expect_equal(result$summary$asDF$totalCount, c(80L, 80L))
})


test_that("venn logical encoding is correct for 2-way Venn", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: The logical encoding should match expected counts
    expect_s3_class(result, "vennResults")

    # Drug = 60 of 100; Success = 40 Drug + 10 Placebo = 50 of 100
    sm <- result$summary$asDF
    expect_equal(sm$trueCount,  c(60L, 50L))
    expect_equal(sm$falseCount, c(40L, 50L))
    expect_equal(sm$totalCount, c(100L, 100L))

    # In actual use, the Venn diagram should show:
    # - Left circle (Drug only): 20
    # - Intersection (both): 40
    # - Right circle (Success only): 10
    # - Outside (neither): 30
})


test_that("venn logical encoding is correct for 3-way Venn", {

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
    ,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: Should handle 3-way combinations correctly
    expect_s3_class(result, "vennResults")

    sm <- result$summary$asDF
    expect_equal(nrow(sm), 3L)
    expect_true(all(sm$totalCount == 120L))
    expect_true(all(sm$trueCount + sm$falseCount == sm$totalCount))
})


test_that("venn with 4 variables works correctly", {

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
    ,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # Should switch to UpSet plot for 4+ variables
    expect_s3_class(result, "vennResults")

    sm <- result$summary$asDF
    expect_equal(nrow(sm), 4L)
    expect_true(all(sm$totalCount == 100L))
})


test_that("venn handles variables with spaces correctly", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # Should handle special characters without errors
    expect_s3_class(result, "vennResults")

    # the ORIGINAL names must survive into the output, not the make.names() form
    expect_equal(result$summary$asDF$variable, c("Treatment Group", "Response Type"))
    expect_true(all(result$summary$asDF$totalCount == 70L))
})


test_that("venn percentage calculations match expected values", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: Percentages should match expected values
    expect_s3_class(result, "vennResults")

    # varA is Yes in 80 of 100, varB in 40 of 100
    sm <- result$summary$asDF
    expect_equal(sm$trueCount, c(80L, 40L))
    expect_equal(sm$truePercentage, c(0.8, 0.4))

    # The summary should show:
    # - "True %" for varA: 80%
    # - "True %" for varB: 40%
})


test_that("venn with all FALSE values handles correctly", {

    # Edge case: All values are the opposite of "true" level
    set.seed(555)
    n <- 50
    testData <- data.frame(
        # the positive level has to EXIST for this to be the all-negative case
        # rather than the absent-level case that the last test in this file covers
        var1 = factor(rep("No", n), levels = c("Yes", "No")),
        var2 = factor(rep("Absent", n), levels = c("Present", "Absent"))
    )

    result <- venn(
        data = testData,
        var1 = "var1",
        var1true = "Yes",
        var2 = "var2",
        var2true = "Present"
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # Should show all cases in the "Neither" category
    expect_s3_class(result, "vennResults")

    sm <- result$summary$asDF
    expect_equal(sm$trueCount,  c(0L, 0L))
    expect_equal(sm$falseCount, c(50L, 50L))
    expect_equal(sm$truePercentage, c(0, 0))
})


test_that("venn with all TRUE values handles correctly", {

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
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # Should show all cases in the "Both" intersection
    expect_s3_class(result, "vennResults")

    sm <- result$summary$asDF
    expect_equal(sm$trueCount,  c(50L, 50L))
    expect_equal(sm$falseCount, c(0L, 0L))
    expect_equal(sm$truePercentage, c(1, 1))
    # a set holding every case must draw the high-prevalence caution
    expect_match(as.character(result$notices$content), "Very High Prevalence")
})


test_that("venn calculates overlap counts correctly", {

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
        showSetCalculations = TRUE,
        calculateOverlap = TRUE,
        calculateDiscern = TRUE,
        calculateUnite = TRUE
    ,
                 var3 = NULL,
                 var3true = NULL,
                 var4 = NULL,
                 var4true = NULL,
                 var5 = NULL,
                 var5true = NULL,
                 var6 = NULL,
                 var6true = NULL,
                 var7 = NULL,
                 var7true = NULL)

    # CRITICAL: Set calculations should match expected values
    # This tests the FIXED overlap/discern/unite processing
    expect_s3_class(result, "vennResults")

    # set1 In = 70, set2 In = 60, both = 40, union = 90, neither = 10
    sm <- result$summary$asDF
    expect_equal(sm$trueCount, c(70L, 60L))
    calc <- as.character(result$setCalculations$content)
    expect_match(calc, "40 cases")   # the set1/set2 intersection
    expect_match(calc, "90 cases")   # the union
})

test_that("validation fails when selected true level not present", {

    data <- data.frame(
        a = factor(c("Yes", "Yes")),
        b = factor(c("No", "No"))
    )

    # A jamovi analysis reports a bad level in its validationErrors panel rather
    # than throwing, so expect_error() asserted the opposite of the wanted
    # behaviour. The message must name the level and list what is available.
    res <- venn(
        data = data,
        var1 = "a", var1true = "Maybe",   # not present
        var2 = "b", var2true = "No",
        var3 = NULL, var3true = NULL, var4 = NULL, var4true = NULL,
        var5 = NULL, var5true = NULL, var6 = NULL, var6true = NULL,
        var7 = NULL, var7true = NULL)

    expect_true(res$validationErrors$visible)
    msg <- as.character(res$validationErrors$content)
    expect_match(msg, "Maybe")
    expect_match(msg, "Available levels")
    expect_equal(nrow(res$summary$asDF), 0L)
})
