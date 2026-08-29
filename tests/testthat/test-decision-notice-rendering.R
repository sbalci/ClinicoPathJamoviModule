# ═══════════════════════════════════════════════════════════
# Regression tests: decision -- notice rendering and result reuse
#
# Each block here pins a defect found by /check-function-full on 2026-08-28 and
# repaired by /fix-function on 2026-08-29. They are deliberately behavioural:
# they assert what the pathologist SEES, not how it is computed.
# ═══════════════════════════════════════════════════════════
library(testthat)
data(decision_small, package = "ClinicoPath")

# Notices are rendered into the `notices` Html element as styled <div>s.
# Strip the markup so a message can be matched as plain prose.
notices_of <- function(res)
    gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(res$notices$content, collapse = " ")))


# ── Fix 1: notices must survive every early return ──────────────────────────
#
# .renderNotices() used to be called only on the last line of .run(), so the
# five return()s above it discarded every ERROR the validators had raised. The
# user got empty tables, a hidden welcome panel, and no explanation at all.

test_that("a single-level gold standard reports why the analysis stopped", {
    # A row filter that leaves one disease category is the common real-world
    # trigger: the variable still has two levels, the DATA has one.
    one_level <- decision_small[decision_small$GoldStandard == "Negative", ]

    res <- decision(
        data = one_level,
        gold = "GoldStandard", goldPositive = "Positive", goldNegative = NULL,
        newtest = "NewTest",   testPositive = "Positive", testNegative = NULL
    )

    expect_gt(nchar(res$notices$content), 0)
    expect_match(notices_of(res), "exactly 2 levels")
})

test_that("identical positive and negative levels report an error", {
    res <- decision(
        data = decision_small,
        gold = "GoldStandard", goldPositive = "Positive", goldNegative = "Positive",
        newtest = "NewTest",   testPositive = "Positive", testNegative = NULL
    )

    expect_gt(nchar(res$notices$content), 0)
    expect_match(notices_of(res), "cannot be the same")
})

test_that("a level absent from the data reports an error naming the level", {
    res <- decision(
        data = decision_small,
        gold = "GoldStandard", goldPositive = "NOT_A_LEVEL", goldNegative = NULL,
        newtest = "NewTest",   testPositive = "Positive",    testNegative = NULL
    )

    expect_gt(nchar(res$notices$content), 0)
    expect_match(notices_of(res), "NOT_A_LEVEL")
    expect_match(notices_of(res), "Available levels")
})


# ── Fix 2: the epiR CI tables must not accumulate rows ──────────────────────
#
# addRow() appends and accepts a duplicate rowKey, and clearWith listed only
# pp/pprob, so a re-run with a changed variable printed every statistic twice.

test_that("re-running an analysis object does not duplicate CI table rows", {
    opts <- ClinicoPath:::decisionOptions$new(
        gold = "GoldStandard", goldPositive = "Positive", goldNegative = NULL,
        newtest = "NewTest",   testPositive = "Positive", testNegative = NULL,
        ci = TRUE
    )
    analysis <- ClinicoPath:::decisionClass$new(options = opts, data = decision_small)

    analysis$run()
    n_ratio  <- nrow(as.data.frame(analysis$results$epirTable_ratio))
    n_number <- nrow(as.data.frame(analysis$results$epirTable_number))
    expect_gt(n_ratio, 0)

    analysis$run()
    expect_equal(nrow(as.data.frame(analysis$results$epirTable_ratio)),  n_ratio)
    expect_equal(nrow(as.data.frame(analysis$results$epirTable_number)), n_number)
})


# ── Fix 6: no third-party warning may leak into Analysis Notes ──────────────
#
# fct_relevel(x, "Positive") warned "1 unknown level in `f`" whenever the
# cohort held a single category -- raw package chatter shown to a clinician on
# exactly the run that otherwise displays nothing.

test_that("a single-category cohort produces no R warning", {
    one_level <- decision_small[decision_small$GoldStandard == "Negative", ]

    expect_no_warning(
        decision(
            data = one_level,
            gold = "GoldStandard", goldPositive = "Positive", goldNegative = NULL,
            newtest = "NewTest",   testPositive = "Positive", testNegative = NULL
        )
    )
})


# ── Fix 8: misuse warnings belong in the always-visible notices pane ────────
#
# These used to be spliced into clinicalInterpretation, which is
# visible: (showClinicalInterpretation) and defaults to FALSE -- so the
# warnings a clinician most needs were the ones behind an opt-in checkbox.

test_that("extreme prevalence is reported without enabling any output option", {
    # 2 diseased out of 200 -> prevalence 1%, below the 5% threshold.
    rare <- data.frame(
        GoldStandard = factor(c(rep("Positive", 2), rep("Negative", 198)),
                              levels = c("Negative", "Positive")),
        NewTest      = factor(c("Positive", "Negative",
                                rep("Positive", 20), rep("Negative", 178)),
                              levels = c("Negative", "Positive"))
    )

    res <- decision(
        data = rare,
        gold = "GoldStandard", goldPositive = "Positive", goldNegative = NULL,
        newtest = "NewTest",   testPositive = "Positive", testNegative = NULL
    )

    # showClinicalInterpretation is left at its default FALSE on purpose.
    expect_match(notices_of(res), "Very low disease prevalence")
})
