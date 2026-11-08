# Regressions for the independent review of 2026-10-30
# (development-ideas/outcomeorganizer-independent-review-2026-10-30.md).

library(testthat)

.rr_ns <- function() asNamespace("ClinicoPath")

.rr_run <- function(d, ...) {
    args <- list(...)
    for (lv in c("outcomeLevel", "recurrenceLevel", "dod", "dooc", "awd", "awod"))
        if (!lv %in% names(args)) args[lv] <- list(NULL)
    ns <- .rr_ns()
    a <- ns$outcomeorganizerClass$new(options = do.call(ns$outcomeorganizerOptions$new, args), data = d)
    err <- tryCatch({ suppressWarnings(a$run()); NULL }, error = function(e) conditionMessage(e))
    out <- a$results$addOutcome$.__enclos_env__$private
    list(a = a, err = err, val = out$.values[[1]], rows = out$.rowNums, title = out$.titles)
}
.rr_html <- function(r, item) paste(as.character(r$a$results[[item]]$content), collapse = " ")

.vs4 <- c("ANED", "AWD", "DOD", "DOC")
.d4 <- do.call(rbind, rep(list(data.frame(
    vs = factor(rep(.vs4, 2), levels = .vs4), rec = factor(rep(c("No", "Yes"), each = 4)))), 3))
.d4$bin <- factor(ifelse(.d4$vs %in% c("DOD", "DOC"), "Dead", "Alive"))
.me <- function(d, type, ...) .rr_run(d, outcome = "vs", multievent = TRUE, analysistype = type,
                                      dod = "DOD", dooc = "DOC", awd = "AWD", awod = "ANED", ...)

test_that("F1: a third recurrence level is named with its row count", {
    d <- data.frame(bin = factor(c("Alive", "Alive", "Alive", "Alive", "Dead", "Dead")),
                    rec = factor(c("None", "Local", "Distant", "Unknown", "None", "Unknown")))
    d <- rbind(d, d, d)
    for (ty in c("rfs", "ttp")) {
        r <- .rr_run(d, outcome = "bin", outcomeLevel = "Dead", recurrence = "rec",
                     recurrenceLevel = "Distant", analysistype = ty)
        sw <- .rr_html(r, "strongWarnings")
        expect_match(sw, "'Local' (3)", fixed = TRUE)
        expect_match(sw, "'Unknown' (6)", fixed = TRUE)
        expect_match(sw, "'None' (6)", fixed = TRUE)
    }
    r <- .rr_run(d, outcome = "bin", outcomeLevel = "Dead", recurrence = "rec",
                 recurrenceLevel = "Distant", analysistype = "rfs")
    expect_match(.rr_html(r, "summary"), "Unknown", fixed = TRUE)

    # Two levels: nothing to name.
    r <- .rr_run(.d4, outcome = "bin", outcomeLevel = "Dead", recurrence = "rec",
                 recurrenceLevel = "Yes", analysistype = "rfs")
    expect_false(grepl("Other recurrence levels", .rr_html(r, "strongWarnings"), fixed = TRUE))
})

test_that("F2: Multiple Event Levels gives a PFS that counts every death", {
    r <- .me(.d4, "pfs", recurrence = "rec", recurrenceLevel = "Yes")
    expect_null(r$err)
    expect_equal(as.numeric(r$val), as.numeric(.d4$rec == "Yes" | .d4$vs %in% c("DOD", "DOC")))
    expect_equal(sum(r$val == 1), 18L)
    expect_match(r$title, "progression-free survival", fixed = TRUE)
    expect_match(.rr_html(r, "summary"), "PFS Analysis", fixed = TRUE)
    expect_false(grepl("other categories of death", .rr_html(r, "summary"), fixed = TRUE))

    r <- .me(.d4, "rfs", recurrence = "rec", recurrenceLevel = "Yes")
    expect_equal(sum(r$val == 1), 18L)
    # With a recurrence variable, recurrence decides DFS: Alive with Disease without a
    # recurrence is event-free, and both kinds of disagreement are named for checking.
    r <- .me(.d4, "dfs", recurrence = "rec", recurrenceLevel = "Yes")
    expect_equal(as.numeric(r$val), as.numeric(.d4$rec == "Yes" | .d4$vs %in% c("DOD", "DOC")))
    expect_equal(sum(r$val == 1), 18L)
    w <- .rr_html(r, "warnings")
    expect_match(w, "Vital status and recurrence disagree", fixed = TRUE)
    expect_match(w, "3 patient(s) coded 'AWD'", fixed = TRUE)
    expect_match(w, "3 patient(s) coded 'ANED'", fixed = TRUE)
})

test_that("F2: TTP under Multiple Event Levels reads the recurrence variable alone", {
    r <- .rr_run(.d4, outcome = "vs", multievent = TRUE, analysistype = "ttp",
                 recurrence = "rec", recurrenceLevel = "Yes")
    expect_null(r$err)
    expect_equal(as.numeric(r$val), as.numeric(.d4$rec == "Yes"))
})

test_that("F2: one Event Level on a multi-level outcome names what it leaves out", {
    r <- .rr_run(.d4, outcome = "vs", outcomeLevel = "DOD", recurrence = "rec",
                 recurrenceLevel = "Yes", analysistype = "pfs", showGlossary = TRUE)
    expect_equal(sum(r$val == 1), 15L)
    sw <- .rr_html(r, "strongWarnings")
    expect_match(sw, sprintf("'DOC' (%d)", sum(.d4$vs == "DOC")), fixed = TRUE)
    expect_match(sw, "Multiple Event Levels", fixed = TRUE)
    expect_false(grepl("progression-free survival", r$title, fixed = TRUE))
    expect_match(.rr_html(r, "eventRecodeInfo"), "one outcome level counted as death", fixed = TRUE)
    expect_false(grepl("counts death from", .rr_html(r, "glossary"), fixed = TRUE))

    r <- .rr_run(.d4, outcome = "vs", outcomeLevel = "DOD", analysistype = "os")
    expect_match(.rr_html(r, "strongWarnings"), "'DOC' (6)", fixed = TRUE)
    expect_false(grepl("overall survival", r$title, fixed = TRUE))

    # Alive/Dead is a real overall survival column and stays titled as one.
    r <- .rr_run(.d4, outcome = "bin", outcomeLevel = "Dead", analysistype = "os")
    expect_equal(.rr_html(r, "strongWarnings"), "")
    expect_match(r$title, "overall survival", fixed = TRUE)
})

test_that("F3: a recurrence endpoint with no recurrence variable is refused", {
    for (ty in c("rfs", "pfs", "dfs", "ttp")) {
        r <- .rr_run(.d4, outcome = "bin", outcomeLevel = "Dead", analysistype = ty)
        expect_match(r$err %||% "", "is built from a Recurrence/Progression variable", fixed = TRUE, info = ty)
    }
})

test_that("F3 limit: DFS under Multiple Event Levels can come from vital status, as in survival", {
    r <- .me(.d4, "dfs", showNaturalSummary = TRUE)
    expect_null(r$err)
    expect_equal(as.numeric(r$val), as.numeric(.d4$vs != "ANED"))
    expect_equal(r$title, "Recoded Outcome (disease-free survival)")
    expect_match(.rr_html(r, "warnings"), "DFS built from vital status alone", fixed = TRUE)
    expect_match(.rr_html(r, "eventRecodeInfo"), "Alive with Disease", fixed = TRUE)
    expect_match(.rr_html(r, "naturalSummary"), "AWD", fixed = TRUE)
    # The other endpoints have no such route.
    for (ty in c("rfs", "pfs", "ttp"))
        expect_match(.me(.d4, ty)$err %||% "", "is built from a Recurrence/Progression variable",
                     fixed = TRUE, info = ty)
})

test_that("F4: a blank patient ID joins nobody", {
    h <- data.frame(pid = c("A", "A", NA, NA, "B", "B", rep("C", 6)),
                    vs = factor(c("ANED", "DOD", "DOD", "ANED", "ANED", "AWD", rep("ANED", 6)), levels = .vs4),
                    t = c(5, 10, 8, 20, 5, 7, 1:6))
    r <- .me(h, "os", patientID = "pid", useHierarchy = TRUE, eventPriority = 1)
    expect_equal(as.numeric(r$val[3:4]), c(1, 0))
    expect_equal(as.numeric(r$val[1:2]), c(1, 1))
    expect_match(.rr_html(r, "warnings"), "2 row(s) have no patient ID", fixed = TRUE)

    h2 <- data.frame(pid = c(NA, NA, "A", "A", rep("C", 8)),
                     vs = factor(c("DOD", "DOD", "ANED", "DOD", rep("ANED", 8)), levels = .vs4),
                     t = c(8, 20, 1, 2, 1:8))
    r <- .me(h2, "os", patientID = "pid", useHierarchy = TRUE, followupTime = "t")
    expect_equal(as.numeric(r$val[1:2]), c(1, 1))
})

test_that("F5: a cut-off on a recurrence endpoint says what Follow-up Time must be", {
    d <- data.frame(bin = factor(c("Dead", "Alive", "Dead", "Alive", "Dead", "Alive", "Alive", "Alive")),
                    rec = factor(c("Yes", "Yes", "No", "No", "No", "No", "Yes", "No")),
                    fu = c(40, 36, 10, 50, 20, 30, 12, 8), cut = 24)
    for (ty in c("pfs", "ttp")) {
        r <- .rr_run(d, outcome = "bin", outcomeLevel = "Dead", recurrence = "rec", recurrenceLevel = "Yes",
                     analysistype = ty, followupTime = "fu", adminCensoring = TRUE, adminDate = "cut")
        sw <- .rr_html(r, "strongWarnings")
        expect_match(sw, "Follow-up Time must be the time to that endpoint", fixed = TRUE)
        expect_match(sw, "with a recorded time after the cut-off", fixed = TRUE)
        expect_false(grepl("occurring after the cut-off", sw, fixed = TRUE))
    }
    # OS has a real date of death in its time: ordinary warning, no recurrence sentence.
    r <- .rr_run(d, outcome = "bin", outcomeLevel = "Dead", analysistype = "os",
                 followupTime = "fu", adminCensoring = TRUE, adminDate = "cut")
    expect_false(grepl("Follow-up Time must be", paste(.rr_html(r, "warnings"), .rr_html(r, "strongWarnings")), fixed = TRUE))
})

test_that("F6: a hand-off column cannot be the outcome of a composite", {
    d <- data.frame(o = factor(c("Censored", "Event", "Competing", "Competing", "Censored", "Event"),
                               levels = c("Censored", "Event", "Competing")),
                    rec = factor(c("No", "No", "No", "Yes", "Yes", "Yes")))
    d <- rbind(d, d)
    for (ty in c("rfs", "pfs", "dfs")) {
        r <- .rr_run(d, outcome = "o", outcomeLevel = "Event", recurrence = "rec",
                     recurrenceLevel = "Yes", analysistype = ty)
        expect_match(r$err %||% "", "cannot be the outcome of", fixed = TRUE, info = ty)
    }
})

test_that("F7: row numbers come from the data frame, not a column called Row Names", {
    d <- data.frame(`Row Names` = 12:1, vs = factor(rep(.vs4, 3), levels = .vs4), check.names = FALSE)
    r <- .me(d, "os", patientID = "Row Names")
    expect_identical(r$rows, 1:12)

    d <- data.frame(row_names = letters[1:12], vs = factor(rep(.vs4, 3), levels = .vs4))
    rownames(d) <- seq(2, 24, 2)
    r <- .me(d, "os", patientID = "row_names", useHierarchy = TRUE)
    expect_identical(r$rows, seq(2L, 24L, 2L))
})

test_that("the outcome column is integer on every path, as its nominal type needs", {
    r <- .rr_run(.d4, outcome = "bin", outcomeLevel = "Dead", recurrence = "rec",
                 recurrenceLevel = "Yes", analysistype = "pfs")
    expect_true(is.integer(r$val))
    expect_true(is.integer(.me(.d4, "multistate")$val))
})

test_that("S4 notes: copy-ready text and glossary match what the run did", {
    d <- data.frame(vs = factor(c("DOD", NA, "ANED", "DOD", "ANED", "DOC", "DOD", "ANED", "ANED", "DOD"), levels = .vs4),
                    t = c(40, 40, 50, 10, 12, 14, 16, 18, 30, 20), cut = 24)
    r <- .me(d, "os", followupTime = "t", adminCensoring = TRUE, adminDate = "cut", showNaturalSummary = TRUE)
    expect_match(.rr_html(r, "warnings"), "1 row(s) with a missing status", fixed = TRUE)
    txt <- .rr_html(r, "naturalSummary")
    expect_match(txt, "administrative cut-off", fixed = TRUE)
    expect_false(grepl("myoutcome", txt, fixed = TRUE))
    expect_false(grepl("myoutcome", .rr_html(r, "infoMessages"), fixed = TRUE))

    h <- data.frame(pid = c("A", "A", rep("B", 10)), vs = factor(c("ANED", "DOD", rep("ANED", 10)), levels = .vs4))
    r <- .me(h, "os", patientID = "pid", useHierarchy = TRUE, showNaturalSummary = TRUE)
    expect_match(.rr_html(r, "naturalSummary"), "event hierarchy", fixed = TRUE)

    r <- .me(.d4, "os", showGlossary = TRUE)
    g <- .rr_html(r, "glossary")
    expect_match(g, "A second primary cancer counts only if it is coded in the recurrence variable", fixed = TRUE)
    expect_false(grepl("recurrence, second primary", g, fixed = TRUE))

    a <- .rr_run(data.frame(x = 1:3))
    expect_false(grepl("Time-dependent", .rr_html(a, "todo"), fixed = TRUE))
})

test_that("engine F3: panels do not outlive a run that stops early", {
    r <- .rr_run(.d4, outcome = "vs", multievent = TRUE, analysistype = "os")
    expect_equal(.rr_html(r, "summary"), "<div></div>")
    expect_equal(.rr_html(r, "eventRecodeInfo"), "<div></div>")

    a <- .me(.d4, "os")$a
    a$.__enclos_env__$private$.clearPanels()
    expect_equal(a$results$summary$content, "<div></div>")

    r <- .rr_run(.d4, outcome = "bin", outcomeLevel = "Dead", analysistype = "os")
    expect_equal(.rr_html(r, "todo"), "<div></div>")
})
