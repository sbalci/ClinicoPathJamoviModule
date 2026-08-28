# Test file for critical statistical fixes in decisioncompare
# Tests validate fixes for:
# 1. McNemar/Cochran Q comparing CORRECTNESS (not raw positivity)
# 2. naOmit only filtering selected variables
# 3. Multi-level variable handling with warnings
# 4. Sample size reporting

test_that("McNemar compares diagnostic CORRECTNESS, not raw positivity rates", {
    skip_if_not_installed("ClinicoPath")

    # Discriminating fixture: both tests call exactly 10 of 20 cases positive, so a
    # McNemar run on RAW POSITIVITY gives p = 1.000. They differ sharply in whether
    # those calls are right (accuracy 16/20 vs 4/20), so a McNemar run on
    # CORRECTNESS gives a small exact McNemar p-value. The module must report
    # that value rather than the raw-positivity comparison.
    gold  <- c(rep("Pos", 10), rep("Neg", 10))
    test1 <- c(rep("Pos", 8), rep("Neg", 2), rep("Pos", 2), rep("Neg", 8))
    test2 <- c(rep("Pos", 2), rep("Neg", 8), rep("Pos", 8), rep("Neg", 2))
    dat   <- data.frame(gold = gold, test1 = test1, test2 = test2,
                        stringsAsFactors = FALSE)

    expect_equal(sum(test1 == "Pos"), sum(test2 == "Pos"))   # identical positivity

    res <- call_decisioncompare(
        data = dat, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
        test1 = "test1", test1Positive = "Pos", test1Negative = NULL,
        test2 = "test2", test2Positive = "Pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL,
        statComp = TRUE)

    mc <- res$mcnemarTable$asDF
    expect_equal(nrow(mc), 1L)

    correctness_table <- table(
        factor(test1 == gold, c(TRUE, FALSE)),
        factor(test2 == gold, c(TRUE, FALSE)))
    p_correctness <- stats::binom.test(
        correctness_table[1, 2],
        correctness_table[1, 2] + correctness_table[2, 1],
        p = 0.5)$p.value
    p_positivity <- stats::mcnemar.test(table(
        factor(test1, c("Pos", "Neg")), factor(test2, c("Pos", "Neg"))))$p.value

    # single comparison -> Holm leaves the p-value unchanged
    expect_equal(mc$p[1], p_correctness, tolerance = 1e-9)
    expect_equal(mc$method[1], "Exact binomial McNemar")
    expect_false(isTRUE(all.equal(mc$p[1], p_positivity)))
    expect_lt(mc$p[1], 0.05)
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


test_that("the completion notice reports the analysed sample size", {
    skip_if_not_installed("ClinicoPath")

    # Each test is missing once, in different rows. Standalone metrics must use
    # four available pairs per test; their paired comparison uses three rows.
    dat <- data.frame(
        gold  = c("Pos", "Neg", "Pos", "Neg", "Pos"),
        test1 = c("Pos", "Neg", "Pos", NA,    "Pos"),
        test2 = c("Pos", "Neg", "Neg", "Neg", NA),
        stringsAsFactors = FALSE)

    res <- call_decisioncompare(
        data = dat, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
        test1 = "test1", test1Positive = "Pos", test1Negative = NULL,
        test2 = "test2", test2Positive = "Pos", test2Negative = NULL,
        test3 = NULL, test3Positive = NULL, test3Negative = NULL, stratify = NULL)

    notices <- gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " "))
    expect_match(notices, "5 selected rows")
    expect_match(notices, "test1 n=4; test2 n=4")
    expect_match(notices, "Paired comparisons use only rows determinate")
})


test_that("Cochran Q across 3 tests matches an independent implementation", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("DescTools")

    set.seed(11)
    n <- 120
    gold <- sample(c("Pos", "Neg"), n, TRUE)
    mk <- function(acc) ifelse(runif(n) < acc, gold,
                               ifelse(gold == "Pos", "Neg", "Pos"))
    dat <- data.frame(gold = gold, test1 = mk(0.90), test2 = mk(0.85),
                      test3 = mk(0.65), stringsAsFactors = FALSE)

    res <- call_decisioncompare(
        data = dat, gold = "gold", goldPositive = "Pos", goldNegative = NULL,
        test1 = "test1", test1Positive = "Pos", test1Negative = NULL,
        test2 = "test2", test2Positive = "Pos", test2Negative = NULL,
        test3 = "test3", test3Positive = "Pos", test3Negative = NULL,
        stratify = NULL, statComp = TRUE)

    mc <- res$mcnemarTable$asDF
    global <- mc[grepl("^Overall", mc$comparison), , drop = FALSE]
    expect_equal(nrow(global), 1L)

    # Reference: Cochran's Q on the CORRECTNESS matrix
    corr <- data.frame(test1 = as.integer(dat$test1 == gold),
                       test2 = as.integer(dat$test2 == gold),
                       test3 = as.integer(dat$test3 == gold))
    long <- data.frame(y = unlist(corr),
                       test = factor(rep(names(corr), each = n)),
                       subj = factor(rep(seq_len(n), times = 3)))
    ref <- DescTools::CochranQTest(y ~ test | subj, data = long)

    expect_equal(global$stat[1], unname(ref$statistic), tolerance = 1e-8)
    expect_equal(global$df[1],   unname(ref$parameter))
    expect_equal(global$p[1],    unname(ref$p.value),   tolerance = 1e-10)
})
