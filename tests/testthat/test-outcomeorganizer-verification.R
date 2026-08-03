# ═══════════════════════════════════════════════════════════
# Numerical Verification Tests: outcomeorganizer
# ═══════════════════════════════════════════════════════════
#
# These tests assert the RECODED VALUES, not merely that an object came back.
# The previous version computed nothing and asserted only
# `expect_s3_class(res, "outcomeorganizerResults")`, which passes just as
# happily when every patient is coded the wrong way round.
#
# outcomeorganizer feeds survival()/multisurvival() through a documented
# interchange format, so the round-trip through .defineEventIndicator() is
# tested here too -- that contract is the whole point of the analysis.

library(testthat)

quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

# `addOutcome` is an Output-type option, so it never reaches the public wrapper.
# Drive the class the way jamovi does and reproduce .run()'s export block.
oo_recode <- function(df, ...) {
    o <- outcomeorganizerOptions$new(...)
    a <- outcomeorganizerClass$new(options = o, data = df)
    quietly(a$run())
    p <- a$.__enclos_env__$private
    st <- quietly(p$.organizeOutcomes())$df_outcome$myoutcome
    if (!is.null(p$.causeFactor)) {
        lbl <- c("Censored", "Event", "Competing")
        idx <- suppressWarnings(as.integer(st)) + 1L
        idx[is.na(idx) | idx < 1L | idx > length(lbl)] <- NA_integer_
        factor(lbl[idx], levels = lbl)
    } else st
}

# vital: Alive / DeadDisease / DeadOther ; rec: No / Yes
oo_df <- function() data.frame(
    vital = factor(c("Alive", "Alive", "DeadDisease", "DeadOther", "Alive", "DeadDisease")),
    rec   = factor(c("No", "Yes", "No", "No", "Yes", "Yes")))

test_that("overall survival counts any death as the event", {
    df <- oo_df()
    single <- oo_recode(df, outcome = "vital", outcomeLevel = "DeadDisease", analysistype = "os")
    expect_equal(as.numeric(single), c(0, 0, 1, 0, 0, 1))

    multi <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "os",
                       dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    # with multiple event levels, BOTH deaths are events
    expect_equal(as.numeric(multi), c(0, 0, 1, 1, 0, 1))
})

test_that("cause-specific survival censors deaths from other causes", {
    df <- oo_df()
    got <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "cause",
                     dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    # row 4 is a death from another cause -> censored, NOT an event
    expect_equal(as.numeric(got), c(0, 0, 1, 0, 0, 1))
})

test_that("competing risks codes other-cause death as a competing event", {
    df <- oo_df()
    got <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "compete",
                     dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    expect_s3_class(got, "factor")
    expect_equal(as.character(got),
                 c("Censored", "Censored", "Event", "Competing", "Censored", "Event"))
})

test_that("RFS/PFS/DFS take the event as recurrence OR death", {
    df <- oo_df()
    for (at in c("rfs", "pfs", "dfs")) {
        got <- oo_recode(df, outcome = "vital", outcomeLevel = "DeadDisease",
                         recurrence = "rec", recurrenceLevel = "Yes", analysistype = at)
        expect_equal(as.numeric(got), c(0, 1, 1, 0, 1, 1), label = at)
    }
})

test_that("TTP counts progression only and censors death -- the PFS/TTP distinction", {
    # The clinically consequential difference between the two endpoints: a patient
    # who dies WITHOUT progressing is an event under PFS and censored under TTP.
    df <- oo_df()
    pfs <- as.numeric(oo_recode(df, outcome = "vital", outcomeLevel = "DeadDisease",
                                recurrence = "rec", recurrenceLevel = "Yes", analysistype = "pfs"))
    ttp <- as.numeric(oo_recode(df, outcome = "vital", outcomeLevel = "DeadDisease",
                                recurrence = "rec", recurrenceLevel = "Yes", analysistype = "ttp"))
    expect_equal(ttp, c(0, 1, 0, 0, 1, 1))
    # row 3 = died of disease, no recurrence recorded
    expect_equal(pfs[3], 1)   # PFS: death is an event
    expect_equal(ttp[3], 0)   # TTP: death is censored
})

test_that("missing outcome or recurrence propagates as NA, never as censored", {
    # Coercing a missing status to 0 would fabricate censoring out of missing data
    # and silently inflate the denominator.
    dfn <- data.frame(vital = factor(c("Alive", "DeadDisease", NA, "Alive")),
                      rec   = factor(c("No", NA, "Yes", "No")))
    os <- oo_recode(dfn, outcome = "vital", outcomeLevel = "DeadDisease", analysistype = "os")
    expect_equal(as.numeric(os), c(0, 1, NA, 0))

    rfs <- oo_recode(dfn, outcome = "vital", outcomeLevel = "DeadDisease",
                     recurrence = "rec", recurrenceLevel = "Yes", analysistype = "rfs")
    # a KNOWN event on either component wins even when the other is missing
    expect_equal(as.numeric(rfs), c(0, 1, 1, 0))

    ttp <- oo_recode(dfn, outcome = "vital", outcomeLevel = "DeadDisease",
                     recurrence = "rec", recurrenceLevel = "Yes", analysistype = "ttp")
    # TTP depends on recurrence alone, so a missing recurrence must stay NA
    expect_equal(as.numeric(ttp), c(0, NA, 1, 0))
})

test_that("multistate assigns one code per state", {
    df <- data.frame(st = factor(c("NED", "Relapse", "DeadDis", "DeadOther")))
    got <- oo_recode(df, outcome = "st", multievent = TRUE, analysistype = "multistate",
                     awod = "NED", awd = "Relapse", dod = "DeadDis", dooc = "DeadOther")
    expect_equal(as.numeric(got), c(0, 1, 2, 3))
})

test_that("an unassigned outcome level is refused rather than silently dropped", {
    # Leaving a level unmapped would make it NA and naOmit() would then delete
    # those patients, shrinking the denominator with no warning.
    df <- data.frame(vital = factor(c("Alive", "DeadDisease", "DeadOther", "Unknown")))
    expect_error(
        quietly(oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "compete",
                          dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")),
        regexp = "not assigned|Unknown")
})

# ---------------------------------------------------------------- regressions

test_that("the competing-risks export DECLARES all three levels", {
    # Regression: the export wrote a bare character vector. jmvcore's
    # Output$asProtoBuf() does `if (!is.factor(column)) column <- as.factor(column)`
    # and serialises levels(column), so only the levels that OCCURRED reached
    # jamovi. A cohort with no other-cause deaths therefore shipped a column
    # declaring just Censored/Event, and survival_utils' hand-off test
    #     setequal(levels(outcome), c("Censored","Event","Competing"))
    # failed -- the downstream analysis lost the competing-risk flag entirely.
    df <- oo_df()
    with_comp <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "compete",
                           dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    expect_s3_class(with_comp, "factor")
    expect_setequal(levels(with_comp), c("Censored", "Event", "Competing"))

    # the same analysis on a cohort that happens to contain NO competing events
    df2 <- droplevels(df[df$vital != "DeadOther", , drop = FALSE])
    no_comp <- oo_recode(df2, outcome = "vital", multievent = TRUE, analysistype = "compete",
                         dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    expect_false("Competing" %in% as.character(no_comp))          # none observed ...
    expect_setequal(levels(no_comp), c("Censored", "Event", "Competing"))  # ... still declared
})

test_that("the exported column round-trips through survival_utils as competing risks", {
    df <- oo_df()
    df2 <- droplevels(df[df$vital != "DeadOther", , drop = FALSE])

    for (nm in c("with competing", "without competing")) {
        d <- if (nm == "with competing") df else df2
        col <- oo_recode(d, outcome = "vital", multievent = TRUE, analysistype = "compete",
                         dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
        res <- .defineEventIndicator(outcome = col, outcomeLevel = NULL, multievent = FALSE,
                                     analysistype = "overall", outcome_name = "recoded")
        expect_null(res$error, label = nm)
        expect_equal(res$estimand, "competing risks", label = nm)
        expect_true(res$has_competing, label = nm)
    }

    # and the decoded status matches the codes the analysis itself produced
    col <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "compete",
                     dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    res <- .defineEventIndicator(outcome = col, outcomeLevel = NULL, multievent = FALSE,
                                 analysistype = "overall", outcome_name = "recoded")
    expect_equal(res$status, c(0, 0, 1, 2, 0, 1))
})

test_that("declared level order matches the 0/1/2 status codes", {
    # as.factor() on a character vector sorts alphabetically (Censored, Competing,
    # Event), which no longer lines up with the codes those labels stand for.
    df <- oo_df()
    col <- oo_recode(df, outcome = "vital", multievent = TRUE, analysistype = "compete",
                     dod = "DeadDisease", dooc = "DeadOther", awod = "Alive")
    expect_equal(levels(col), c("Censored", "Event", "Competing"))
    expect_equal(as.numeric(col) - 1L, c(0, 0, 1, 2, 0, 1))
})

test_that("non-competing analyses still export a plain numeric indicator", {
    # The factor export must be confined to the competing-risks hand-off.
    df <- oo_df()
    for (at in c("os", "rfs", "ttp")) {
        got <- oo_recode(df, outcome = "vital", outcomeLevel = "DeadDisease",
                         recurrence = if (at == "os") NULL else "rec",
                         recurrenceLevel = if (at == "os") NULL else "Yes",
                         analysistype = at)
        expect_false(is.factor(got), label = at)
        expect_true(all(stats::na.omit(as.numeric(got)) %in% c(0, 1)), label = at)
    }
})
