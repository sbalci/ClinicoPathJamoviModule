# Regression tests for the outcomeorganizer -> singlearm competing-risk hand-off.
#
# CR-01: every analysis decided whether to use competing-risk methods from the
# OPTIONS alone -- `self$options$multievent && analysistype == "compete"`. The
# hand-off arrives with multievent = FALSE (the user never fills
# dod/dooc/awd/awod; that is the point of a pre-recoded column), so the guard
# was FALSE and the 0/1/2 status went into an ordinary survival::Surv(). Surv()
# with a maximum status of 2 subtracts 1 and NAs anything outside 0/1:
# Censored -> NA (row DELETED), Event -> censored, Competing -> EVENT. The
# fixture below reported Records 4, Events 2 and Median 35 -- 35 being the
# midpoint of the two COMPETING times. The guard now consults the status vector
# that was actually built (private$.eventRecode$has_competing / the plot state).
#
# CR-02: the competing-risk survTable indexed survfit()$pstate at column 2
# literally. survfit() only creates states that occur, so a cohort with zero
# code-1 events has states "(s0), 2" and column 2 is the COMPETING event. The
# table -- headed and footnoted "cumulative incidence of the event of interest"
# -- therefore rose to 100% on data containing no event of interest at all.
# The column is now selected by state name.

.cr_ns <- NULL
# Prefer whichever distribution is already loaded, so the umbrella's suite tests
# the umbrella's copy and jsurvival's tests jsurvival's -- these files are synced
# copies and it is easy to end up asserting against the wrong one.
for (.p in c(intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces()),
             "jsurvival", "ClinicoPath")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("singlearm", envir = .cand, inherits = FALSE)) {
            .cr_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.cr_ns), "singlearm not available in this distribution")

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }

# The wrapper does NSE and the `type: Level` options cannot carry defaults, so
# they have to be passed explicitly (NULL) through do.call.
run_singlearm <- function(d) {
    quiet(do.call(get("singlearm", envir = .cr_ns), list(
        data = d, elapsedtime = "time", outcome = "out", outcomeLevel = "Event",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        multievent = FALSE, analysistype = "overall",
        timetypeoutput = "months", cutp = "12, 36, 60")))
}

handoff <- function(labels, times) {
    data.frame(
        time = times,
        out  = factor(labels, levels = c("Censored", "Event", "Competing")),
        stringsAsFactors = FALSE)
}

test_that("a recoded Censored/Event/Competing column keeps all rows and all events", {
    # 2 events @ 10,20  |  2 competing @ 30,40  |  2 censored @ 50,60
    d <- handoff(c("Event", "Event", "Competing", "Competing", "Censored", "Censored"),
                 c(10, 20, 30, 40, 50, 60))

    mt <- as.data.frame(run_singlearm(d)$medianTable)

    expect_equal(nrow(mt), 1L)
    # Was 4: the two censored rows were turned into NA by Surv() and dropped.
    expect_equal(as.numeric(mt$records[1]), 6)
    # Was 2 as well -- but they were the two COMPETING patients, not the events.
    expect_equal(as.numeric(mt$events[1]), 2)
    # Was 35, the midpoint of the competing times 30 and 40. The cumulative
    # incidence of the event of interest never reaches 50% here, so no median
    # is estimable; anything near 35 means the inversion is back.
    expect_true(is.na(mt$median[1]) || as.numeric(mt$median[1]) < 30)
})

test_that("the status vector, not the options, decides competing-risk mode", {
    gen <- get("singlearmClass", envir = .cr_ns)
    src <- paste(deparse(gen$private_methods$.isCompetingRisk), collapse = " ")
    src <- gsub("\\s+", " ", src)

    expect_true(grepl(".eventRecode$has_competing", src, fixed = TRUE))
    # The defect, precisely: the option pair as the sole test.
    expect_false(grepl("return(self$options$multievent && self$options$analysistype == \"compete\")",
                       src, fixed = TRUE))

    # ... and no plot renderer may re-implement the option test locally.
    all_src <- paste(vapply(gen$private_methods,
                            function(f) paste(deparse(f), collapse = " "),
                            character(1)), collapse = " ")
    all_src <- gsub("\\s+", " ", all_src)
    expect_false(grepl("self$options$multievent && self$options$analysistype == \"compete\"",
                       all_src, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# CR-03: survival() (the grouped Kaplan-Meier analysis) had the same defect, in
# ~20 places, and it survived the singlearm fix. Its medianTable was already
# right -- .medianSurv() consults .isCompetingRisk() -- which made the rest look
# fine, but the KM plot and the RMST table still tested the option pair.
# On the 30-row fixture below the plot came out FULLY INVERTED (the arm holding
# every real event drew a flat curve near 1.0) and rmstTable reported
# 38.85 / 39.20 -- Surv() had warned "Invalid status value, converted to NA",
# dropped the 10 censored rows and scored the 10 COMPETING events as events.
# Cause-specific RMST on the same data is 45.25 / 46.33.
handoff_grouped <- function() {
    lab <- c(rep("Event", 5), rep("Competing", 5), rep("Censored", 5),
             rep("Event", 5), rep("Competing", 5), rep("Censored", 5))
    tm  <- c(8, 14, 22, 31, 44,  18, 26, 35, 41, 52,  55, 58, 60, 60, 60,
             10, 17, 25, 36, 47, 20, 28, 33, 43, 50,  56, 59, 60, 60, 60)
    data.frame(time = tm,
               out  = factor(lab, levels = c("Censored", "Event", "Competing")),
               arm  = factor(rep(c("A", "B"), each = 15)),
               stringsAsFactors = FALSE)
}

run_survival <- function(d)
    quiet(do.call(get("survival", envir = .cr_ns), list(
        data = d, elapsedtime = "time", outcome = "out", explanatory = "arm",
        outcomeLevel = "Event", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        multievent = FALSE, analysistype = "overall",
        timetypeoutput = "months", cutp = "12, 36, 60",
        rmst_analysis = TRUE, sc = TRUE, pw = TRUE, person_time = TRUE)))

test_that("survival(): the KM plot state carries the competing-risk flag", {
    skip_if_not(exists("survival", envir = .cr_ns), "survival not available")
    res <- run_survival(handoff_grouped())

    # The flag has to be ON THE STATE, not only in private$.eventRecode:
    # jmvcore's Analysis$.load() re-renders a plot without ever calling .run(),
    # so a renderer that consults only private$.eventRecode sees NULL and draws
    # the inverted curve anyway. Was absent (NULL) entirely.
    expect_true(isTRUE(res$plot$state$has_competing))

    # And the medianTable stays right: all 30 rows, 5 real events per arm.
    mt <- as.data.frame(res$medianTable)
    expect_equal(as.numeric(mt$records), c(15, 15))
    expect_equal(as.numeric(mt$events), c(5, 5))
})

test_that("survival(): rmstTable refuses instead of reporting inverted RMSTs", {
    skip_if_not(exists("survival", envir = .cr_ns), "survival not available")
    res <- run_survival(handoff_grouped())

    # Was 2 rows: 38.85 and 39.20, computed on 20 of 30 records with the
    # competing events scored as events (correct cause-specific: 45.25 / 46.33).
    expect_equal(nrow(as.data.frame(res$rmstTable)), 0L)

    # An empty table with no note reads as a glitch, not a refusal.
    expect_true("cr" %in% names(res$rmstTable$notes))
    expect_match(res$rmstTable$notes$cr$note, "not available for competing-risks")

    # Same for every other single-event output that is switched on above.
    for (tbl in c("coxTable", "pairwiseTable", "personTimeTable")) {
        expect_equal(nrow(as.data.frame(res[[tbl]])), 0L, info = tbl)
        expect_true("cr" %in% names(res[[tbl]]$notes), info = tbl)
    }
})

test_that("no survival backend re-implements the option test locally", {
    for (cls in c("survivalClass", "survivalcontClass", "multisurvivalClass")) {
        if (!exists(cls, envir = .cr_ns)) next
        gen <- get(cls, envir = .cr_ns)
        src <- paste(vapply(gen$private_methods,
                            function(f) paste(deparse(f), collapse = " "),
                            character(1)), collapse = " ")
        src <- gsub("\\s+", " ", src)
        # The defect verbatim. .isCompetingRisk() itself contains the option pair
        # as its LAST fallback, guarded by isTRUE(...) -- that spelling differs.
        expect_false(
            grepl("self$options$multievent && self$options$analysistype == \"compete\"",
                  src, fixed = TRUE), info = cls)
        expect_false(
            grepl("self$options$multievent && self$options$analysistype == 'compete'",
                  src, fixed = TRUE), info = cls)
    }
})

test_that("survivalcont() refuses the hand-off instead of analysing it inverted", {
    skip_if_not(exists("survivalcont", envir = .cr_ns), "survivalcont not available")
    d <- handoff_grouped()
    d$biomarker <- seq(2, 10, length.out = nrow(d))

    res <- quiet(do.call(get("survivalcont", envir = .cr_ns), list(
        data = d, elapsedtime = "time", outcome = "out", contexpl = "biomarker",
        outcomeLevel = "Event", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        multievent = FALSE, analysistype = "overall",
        timetypeoutput = "months", cutp = "12, 36, 60", findcut = TRUE)))

    # The gate used to sit at the very top of .run(), before .cleandata(), where
    # private$.eventRecode does not exist yet -- so it could only test the
    # options and the hand-off walked straight past it. It reported a cut-off at
    # "biomarker = high" with 2 records / 2 events out of 30 patients.
    expect_equal(nrow(as.data.frame(res$medianTable)), 0L)
    expect_equal(nrow(as.data.frame(res$coxTable)), 0L)
    expect_match(res$errors$content, "Competing risks not available")
    # Running .cleandata() first also lets the refusal explain itself.
    expect_true(nchar(res$eventRecodeInfo$content) > 0)
})

test_that("multisurvival() routes the hand-off to Fine-Gray", {
    skip_if_not(exists("multisurvival", envir = .cr_ns), "multisurvival not available")
    d <- handoff_grouped()
    d$biomarker <- seq(2, 10, length.out = nrow(d))

    res <- quiet(do.call(get("multisurvival", envir = .cr_ns), list(
        data = d, elapsedtime = "time", outcome = "out", explanatory = "arm",
        contexpl = "biomarker", outcomeLevel = "Event",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        multievent = FALSE, analysistype = "overall", timetypeoutput = "months")))

    # .cox_model() took the STANDARD coxph branch with the three-level
    # Censored/Event/Competing factor, which survival reads as a multi-state
    # model: the whole analysis died with "an id statement is required for
    # multi-state models" and every output came back blank, under a generic
    # troubleshooting panel advising the user to "verify outcome is binary".
    expect_false(grepl("id statement is required", res$todo$content, fixed = TRUE))
    expect_true(nchar(res$text$content) > 0)
    expect_match(res$infoMessages$content, "Fine-Gray")
})

test_that("zero events of interest does not report the competing event's incidence", {
    # 4 competing @ 10,20,30,40  |  2 censored @ 50,60  |  no code-1 event at all.
    d <- handoff(c("Competing", "Competing", "Competing", "Competing", "Censored", "Censored"),
                 c(10, 20, 30, 40, 50, 60))

    st <- as.data.frame(run_singlearm(d)$survTable)

    expect_true(nrow(st) > 0)
    # `surv` is the cumulative incidence of the event of interest in this mode.
    # It used to be read off pstate column 2 -- the competing state -- and
    # climbed to 1.0 (100%) for an event that never occurred.
    expect_true(all(st$surv == 0, na.rm = TRUE),
                info = paste("CIF of the event of interest:",
                             paste(st$surv, collapse = ", ")))
    expect_true(all(as.numeric(st$n.event) == 0, na.rm = TRUE))
})
