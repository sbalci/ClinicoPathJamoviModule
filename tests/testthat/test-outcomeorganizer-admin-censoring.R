# ═══════════════════════════════════════════════════════════
# Regression tests: outcomeorganizer administrative censoring,
# event hierarchy, and the multi-event level gate
# ═══════════════════════════════════════════════════════════
#
# Each block pins one defect found in the 2026-08-31 /check-function pass. The
# comment above each states what the analysis did BEFORE the fix, so a refactor
# that reintroduces the behaviour fails here with a readable reason.
#
# NOTE ON format(): ClinicoPath does a blanket import(jmvcore) and jmvcore
# exports its own format(). Use base::format in this file.

library(testthat)

fmt <- base::format
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
# Every `type: Level` option is a REQUIRED wrapper argument (the compiler forbids
# `default:` on Level), so the six of them must be passed explicitly, as NULL when
# their parent Variable is unset. Omitting one fails with
# `argument "dod" is missing, with no default` — which is a TEST bug, not a
# schema bug: never "fix" it by adding a default to the .a.yaml.
oo_opts <- function(...) {
    defaults <- list(recurrenceLevel = NULL, dod = NULL, dooc = NULL,
                     awd = NULL, awod = NULL)
    args <- utils::modifyList(defaults, list(...))
    do.call(outcomeorganizerOptions$new, args)
}
oo_recode <- function(df, ...) {
    o <- oo_opts(...)
    a <- outcomeorganizerClass$new(options = o, data = df)
    quietly(try(a$run(), silent = TRUE))
    list(a = a, r = a$results, out = a$.__enclos_env__$private$.organizeOutcomes())
}


# ------------------------------------------- ADMINISTRATIVE CENSORING: SCALE --
# Was: `fu > cut` compared a follow-up DURATION with an administrative DATE.
# A date coerced to numeric is a day count since 1970 (18992 for 2021-12-31), so
# "59 months > 18992" is never TRUE: the cut-off silently never fired, while
# `applied$admin <- TRUE` made the Summary claim "Observations are censored at a
# specified administrative date". Measured: 15/15 deaths retained as events.

test_that("a duration follow-up against a date cut-off is refused, not silently ignored", {
    set.seed(1); n <- 30
    df <- data.frame(
        status = factor(rep(c("Dead", "Alive"), length.out = n), levels = c("Alive", "Dead")),
        months = sample(1:60, n, TRUE),
        cutoff = as.Date("2021-12-31"),
        stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os",
                   followupTime = "months", adminCensoring = TRUE, adminDate = "cutoff",
                   diagnostics = TRUE)
    expect_false(h$out$applied$admin)                       # was TRUE
    expect_match(h$out$diagnostics$admin_censoring, "NOT applied")
    # the guard is now TYPE-based: a magnitude heuristic was tried first and failed
    # in both directions (follow-up in days vs a date is only a 5x ratio and slipped
    # through; a legitimate same-units registry cohort was wrongly refused).
    expect_match(h$out$diagnostics$admin_censoring,
                 "follow-up time is a number and the cut-off is a date")
})

test_that("a cut-off on the same scale as the follow-up is applied correctly", {
    df <- data.frame(
        status = factor(c(rep("Dead", 5), rep("Alive", 5)), levels = c("Alive", "Dead")),
        months = c(10, 20, 80, 95, 50, 15, 70, 30, 88, 45),
        cut    = rep(60, 10),
        stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "status", outcomeLevel = "Dead", analysistype = "os",
                   followupTime = "months", adminCensoring = TRUE, adminDate = "cut",
                   diagnostics = TRUE)
    expect_true(h$out$applied$admin)
    # the two deaths after the cut-off (80, 95) become censored; the three before stay events
    expect_equal(as.numeric(h$out$df_outcome$myoutcome), c(1, 1, 0, 0, 1, 0, 0, 0, 0, 0))
})


# ------------------------------- ADMINISTRATIVE CENSORING: THE TRUNCATED TIME --
# Was: the truncated follow-up was computed into mydata[["admin_time"]] and never
# exported — the analysis shipped only the status column. A censored patient was
# therefore paired with their ORIGINAL untruncated follow-up, inflating
# person-time and removing the event: survival biased upward in exactly the rows
# the cut-off existed to protect.

tick_output <- function(o, name) {
    p <- o$.__enclos_env__$private
    p[[paste0("..", name)]]$value <- list(value = TRUE, vars = list(name), synced = TRUE)
    p$.env[[name]] <- p[[paste0("..", name)]]$value
    invisible(o)
}

test_that("the truncated follow-up time is exported alongside the censored status", {
    df <- data.frame(
        status = factor(c(rep("Dead", 5), rep("Alive", 5)), levels = c("Alive", "Dead")),
        months = c(10, 20, 80, 95, 50, 15, 70, 30, 88, 45),
        cut    = rep(60, 10),
        stringsAsFactors = FALSE)
    o <- oo_opts(outcome = "status", outcomeLevel = "Dead", analysistype = "os",
                 followupTime = "months", adminCensoring = TRUE, adminDate = "cut",
                 addOutcome = TRUE)
    tick_output(o, "addOutcome"); tick_output(o, "addAdminTime")
    a <- outcomeorganizerClass$new(options = o, data = df); quietly(a$run())

    at <- a$results$addAdminTime
    # enabled, NOT isFilled: isFilled is TRUE even on an item jamovi never receives.
    expect_true(at$enabled)
    expect_equal(as.numeric(at$.__enclos_env__$private$.values[[1]]),
                 pmin(df$months, 60))
    expect_equal(length(at$.__enclos_env__$private$.rowNums), nrow(df))
})

test_that("no truncated-time column is produced when the cut-off did not apply", {
    set.seed(1); n <- 20
    df <- data.frame(
        status = factor(rep(c("Dead", "Alive"), length.out = n), levels = c("Alive", "Dead")),
        months = sample(1:60, n, TRUE), cutoff = as.Date("2021-12-31"),
        stringsAsFactors = FALSE)
    o <- oo_opts(outcome = "status", outcomeLevel = "Dead", analysistype = "os",
                 followupTime = "months", adminCensoring = TRUE, adminDate = "cutoff")
    tick_output(o, "addAdminTime")
    a <- outcomeorganizerClass$new(options = o, data = df); quietly(a$run())
    # scale mismatch -> nothing truncated -> nothing written, rather than a
    # column of untruncated times masquerading as truncated ones
    expect_true(a$results$addAdminTime$isNotFilled())
})


# ------------------------- ADMINISTRATIVE CENSORING: NO CENSORED CODE EXISTS --
# Was: censoring wrote 0 unconditionally. Under the multistate coding 0 means
# "Alive without disease" — a clinical state, not a censoring indicator — so the
# cut-off RELABELLED patients into the baseline state instead of censoring them.

test_that("administrative censoring refuses to run against a multistate coding", {
    df <- data.frame(
        v = factor(c("AWOD", "AWD", "DOD", "DOOC", "AWOD", "DOD"),
                   levels = c("AWOD", "AWD", "DOD", "DOOC")),
        months = c(10, 90, 20, 95, 30, 80), cut = rep(60, 6),
        stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "v", outcomeLevel = "DOD", analysistype = "multistate",
                   multievent = TRUE, dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                   followupTime = "months", adminCensoring = TRUE, adminDate = "cut",
                   diagnostics = TRUE)
    expect_false(h$out$applied$admin)
    expect_match(h$out$diagnostics$admin_censoring, "no censored code")
    # every state preserved: nobody relabelled into the baseline state
    expect_equal(as.numeric(h$out$df_outcome$myoutcome), c(0, 1, 2, 3, 0, 2))
})


# ------------------------------------------------------- THE FOUR-LEVEL GATE --
# Was: .run() returned early unless ALL FOUR of dod/dooc/awd/awod were non-NULL.
# That is a UI-slot check, not a data check, so an ordinary 3-state registry
# outcome (Alive / Dead of disease / Dead of other causes) could not be analysed
# for competing risks at all without inventing a fourth category — while the
# shared coder in R/survival_utils.R requires only that ONE bucket be filled and
# that every OBSERVED level be assigned, and its own comment records that an
# empty category "is perfectly normal and must not error".

test_that("a three-state registry outcome runs without inventing a fourth level", {
    set.seed(2); n <- 40
    df <- data.frame(
        vital = factor(sample(c("Alive", "Dead of disease", "Dead of other causes"),
                              n, TRUE, prob = c(.5, .3, .2)),
                       levels = c("Alive", "Dead of disease", "Dead of other causes")),
        stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "vital", outcomeLevel = "Dead of disease",
                   analysistype = "compete", multievent = TRUE,
                   dod = "Dead of disease", dooc = "Dead of other causes",
                   awod = "Alive", awd = NULL)          # awd deliberately empty
    coded <- as.numeric(h$out$df_outcome$myoutcome)
    expect_equal(sum(coded == 0), sum(df$vital == "Alive"))
    expect_equal(sum(coded == 1), sum(df$vital == "Dead of disease"))
    expect_equal(sum(coded == 2), sum(df$vital == "Dead of other causes"))
    expect_false(anyNA(coded))
})

test_that("assigning no level at all is still refused", {
    df <- data.frame(vital = factor(c("Alive", "Dead"), levels = c("Alive", "Dead")),
                     stringsAsFactors = FALSE)
    # Two layers refuse this, and both are correct. Through .run() -- the path a
    # user takes -- the gate stops before recoding and posts a strong warning.
    o <- oo_opts(outcome = "vital", outcomeLevel = "Dead",
                 analysistype = "compete", multievent = TRUE)
    a <- outcomeorganizerClass$new(options = o, data = df)
    quietly(try(a$run(), silent = TRUE))
    expect_match(as.character(a$results$strongWarnings$content), "no outcome level")

    # And the shared coder rejects independently if it is ever reached directly,
    # naming the four categories. Relaxing the gate to "at least one assigned"
    # must not have removed the zero-assigned refusal.
    expect_error(a$.__enclos_env__$private$.organizeOutcomes(),
                 "no outcome levels have been assigned")
})


# --------------------------------------------------------- EVENT PRIORITY -----
# Was: eventPriority was an unbounded Integer. 0 is the censored/baseline code,
# so giving it priority made "censored" outrank every event and silently erased
# them all; and a code the recode never produced was a no-op that the diagnostic
# still reported as "Event hierarchy applied".

test_that("a priority of 0 is rejected rather than erasing every event", {
    df <- data.frame(v = factor(c("Alive", "Dead"), levels = c("Alive", "Dead")),
                     id = c(1, 1), stringsAsFactors = FALSE)
    expect_error(
        outcomeorganizer(data = df, outcome = "v", outcomeLevel = "Dead",
                         recurrenceLevel = NULL, dod = NULL, dooc = NULL,
                         awd = NULL, awod = NULL,
                         patientID = "id", useHierarchy = TRUE, eventPriority = 0),
        "must be between 1")
})

test_that("a priority code that never occurs is reported, not claimed as applied", {
    # `compete` (0/1/2), not `multistate`: the multistate coding has no censored code,
    # so the hierarchy is refused there outright (see the block below) and could not
    # exercise the priority guard at all.
    df <- data.frame(
        v = factor(c("DOD", "Alive", "DOOC", "DOD", "Alive", "Alive", "DOOC", "Alive", "Alive", "DOD"),
                   levels = c("Alive", "DOD", "DOOC")),
        id = rep(1:5, each = 2), t = rep(c(5, 10), 5), stringsAsFactors = FALSE)
    common <- list(outcome = "v", outcomeLevel = "DOD", analysistype = "compete",
                   multievent = TRUE, dod = "DOD", dooc = "DOOC", awod = "Alive", awd = NULL,
                   patientID = "id", followupTime = "t", useHierarchy = TRUE,
                   diagnostics = TRUE)

    good <- do.call(oo_recode, c(list(df), common, list(eventPriority = 2)))
    expect_true(good$out$applied$hierarchy)
    expect_match(good$out$diagnostics$hierarchy, "Event hierarchy applied")

    bad <- do.call(oo_recode, c(list(df), common, list(eventPriority = 7)))
    expect_false(bad$out$applied$hierarchy)              # was TRUE
    expect_match(bad$out$diagnostics$hierarchy, "NOT applied")
    expect_match(bad$out$diagnostics$hierarchy, "does not occur in the recoded outcome")
})


# ------------------------------ EVENT HIERARCHY AGAINST A MULTISTATE CODING --
# Was: the hierarchy wrote literal 0 to every non-priority row. Under the
# multistate coding 0 is "Alive without disease" -- a clinical state, not a
# censoring indicator -- so a patient recorded as Alive WITH disease was silently
# rewritten as disease-free, and a competing event recorded before the priority
# event was erased. The administrative-censoring path already refused exactly
# this; the hierarchy had no equivalent guard.

test_that("the event hierarchy refuses to collapse a multistate coding", {
    df <- data.frame(
        v = factor(c("AWD", "DOD", "AWOD", "AWD", "AWD", "AWOD"),
                   levels = c("AWOD", "AWD", "DOD", "DOOC")),
        pid = c("A", "A", "B", "B", "B", "C"), t = c(6, 12, 6, 12, 18, 6),
        stringsAsFactors = FALSE)
    common <- list(outcome = "v", outcomeLevel = "DOD", analysistype = "multistate",
                   multievent = TRUE, dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                   patientID = "pid", followupTime = "t", eventPriority = 2, diagnostics = TRUE)

    off <- do.call(oo_recode, c(list(df), common, list(useHierarchy = FALSE)))
    on  <- do.call(oo_recode, c(list(df), common, list(useHierarchy = TRUE)))

    # not one state may change
    expect_equal(as.numeric(on$out$df_outcome$myoutcome),
                 as.numeric(off$out$df_outcome$myoutcome))
    expect_false(on$out$applied$hierarchy)
    expect_match(on$out$diagnostics$hierarchy, "no censored code")
    # and it must not still claim it did something
    expect_false(grepl("EVERY one of their rows",
                       paste(as.character(on$r$warnings$content), collapse = " "),
                       fixed = TRUE))
})

test_that("the event hierarchy still works where a censored code exists", {
    df <- data.frame(
        v = factor(c("DOD", "Alive", "DOOC", "DOD", "Alive", "Alive"),
                   levels = c("Alive", "DOD", "DOOC")),
        pid = c("A", "A", "B", "B", "C", "C"), t = c(12, 6, 6, 12, 6, 12),
        stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "v", outcomeLevel = "DOD", analysistype = "compete",
                   multievent = TRUE, dod = "DOD", dooc = "DOOC", awod = "Alive", awd = NULL,
                   patientID = "pid", followupTime = "t", useHierarchy = TRUE,
                   eventPriority = 1, diagnostics = TRUE)
    expect_true(h$out$applied$hierarchy)
    expect_match(h$out$diagnostics$hierarchy, "Event hierarchy applied")
})


# --------------------------------- THE SCALE GUARD DECIDES BY TYPE, NOT SIZE --
# A magnitude heuristic ("refuse when the cut-off exceeds 10x the largest
# follow-up") was tried and failed in BOTH directions: follow-up in days against a
# date is only a 5x ratio and slipped through, while a surgical registry whose
# administrative window is legitimately in the same units was wrongly refused.

test_that("the scale guard classifies the columns rather than measuring them", {
    mk <- function(fu, cut, n = 10) data.frame(
        status = factor(rep(c("Dead", "Alive"), length.out = n), levels = c("Alive", "Dead")),
        fu = fu, cut = cut, stringsAsFactors = FALSE)
    go <- function(d) oo_recode(d, outcome = "status", outcomeLevel = "Dead",
                                analysistype = "os", followupTime = "fu",
                                adminCensoring = TRUE, adminDate = "cut", diagnostics = TRUE)

    set.seed(4)
    # follow-up in DAYS against a date: only a ~5x ratio, so the old rule missed it
    expect_false(go(mk(sample(100:3650, 10, TRUE), as.Date("2021-12-31")))$out$applied$admin)
    # a FACTOR cut-off: as.numeric() would give level indices, not a date
    expect_false(go(mk(sample(10:90, 10, TRUE), factor(rep("2021-12-31", 10))))$out$applied$admin)
    # legitimate same-units cohort: must APPLY (the old rule refused this)
    expect_true(go(mk(c(10, 20, 80, 95, 50, 15, 70, 30, 88, 45), rep(60, 10)))$out$applied$admin)
    # both genuinely dates: must APPLY
    expect_true(go(mk(as.Date("2020-01-01") + c(10, 20, 800, 950, 500, 15, 700, 300, 880, 450),
                      as.Date("2021-12-31")))$out$applied$admin)
    # nothing comparable at all: must refuse rather than report success on 0 rows
    expect_false(go(mk(sample(1:60, 10, TRUE), NA_real_))$out$applied$admin)
})


# ------------------------------------------ VALIDATION THE ANALYSIS DID NOT DO --

test_that("one outcome level cannot be assigned to two multistate slots", {
    # The multistate branch is the only multi-event path that does not go through
    # .defineEventIndicator(), so it never got that function's duplicate check. The
    # four state writes are sequential, so a duplicate silently let the LAST write
    # win and the Summary then printed two contradictory lines about one level.
    df <- data.frame(v = factor(c("Alive", "DOD", "DOOC", "Alive"),
                                levels = c("Alive", "DOD", "DOOC")),
                     stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(df, outcome = "v", outcomeLevel = "DOD",
                                  analysistype = "multistate", multievent = TRUE,
                                  dod = "DOD", dooc = "DOOC", awd = "Alive", awod = "Alive")
    expect_error(a$.__enclos_env__$private$.organizeOutcomes(),
                 "assigned to only one state")
})

test_that("an interval whose start is after its end is refused", {
    df <- data.frame(v = factor(c("Alive", "Dead", "Alive", "Dead"), levels = c("Alive", "Dead")),
                     L = c(1, 9, 2, 8), R = c(5, 3, 6, 4),   # rows 2 and 4 run backwards
                     stringsAsFactors = FALSE)
    h <- oo_recode(df, outcome = "v", outcomeLevel = "Dead", analysistype = "os",
                   intervalCensoring = TRUE, intervalStart = "L", intervalEnd = "R",
                   diagnostics = TRUE)
    expect_false(h$out$applied$interval)
    expect_match(h$out$diagnostics$interval_censoring, "runs backwards")
})

test_that("a censoring option ticked without its variable says so", {
    df <- data.frame(v = factor(c("Alive", "Dead"), levels = c("Alive", "Dead")),
                     stringsAsFactors = FALSE)
    # interval censoring on, neither endpoint selected
    a <- run_outcomeorganizer_obj(df, outcome = "v", outcomeLevel = "Dead",
                                  intervalCensoring = TRUE)
    expect_match(paste(as.character(a$results$warnings$content), collapse = " "),
                 "Interval censoring not applied")
    # administrative censoring on, no cut-off variable
    b <- run_outcomeorganizer_obj(df, outcome = "v", outcomeLevel = "Dead",
                                  adminCensoring = TRUE)
    expect_match(paste(as.character(b$results$warnings$content), collapse = " "),
                 "Administrative censoring not applied")
})


# ------------------------------------- THE NARRATIVE MUST FOLLOW THE CODING ----

test_that("the copy-ready text does not call a multistate code an event", {
    # Was: the event_desc chain had no multistate arm, so it fell through to the
    # placeholder "the selected event type", and n_events counted every "alive with
    # disease" patient (code 1) as an event.
    set.seed(1); n <- 40
    df <- data.frame(v = factor(sample(c("Alive", "AWD", "DOD", "DOOC"), n, TRUE),
                                levels = c("Alive", "AWD", "DOD", "DOOC")),
                     stringsAsFactors = FALSE)
    a <- run_outcomeorganizer_obj(df, outcome = "v", outcomeLevel = "DOD",
                                  analysistype = "multistate", multievent = TRUE,
                                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "Alive",
                                  showNaturalSummary = TRUE)
    txt <- paste(as.character(a$results$naturalSummary$content), collapse = " ")
    expect_false(grepl("the selected event type", txt, fixed = TRUE))
    expect_match(txt, "not a single event indicator")
    expect_match(txt, "patients in the disease state")
})

test_that("cause-specific and competing risks do not call code 0 event-free", {
    # Was: censor_desc was hard-coded to "patients who remain alive or event-free"
    # for every type but TTP. Under cause-specific the 0 group contains patients who
    # died of another cause -- calling them event-free in a manuscript is wrong.
    set.seed(1); n <- 40
    df <- data.frame(v = factor(sample(c("Alive", "DOD", "DOOC"), n, TRUE),
                                levels = c("Alive", "DOD", "DOOC")),
                     stringsAsFactors = FALSE)
    common <- list(outcome = "v", outcomeLevel = "DOD", multievent = TRUE,
                   dod = "DOD", dooc = "DOOC", awod = "Alive", awd = NULL,
                   showNaturalSummary = TRUE)

    cause <- do.call(run_outcomeorganizer_obj,
                     c(list(df), common, list(analysistype = "cause")))
    expect_match(paste(as.character(cause$results$naturalSummary$content), collapse = " "),
                 "died of another cause")

    comp <- do.call(run_outcomeorganizer_obj,
                    c(list(df), common, list(analysistype = "compete")))
    expect_match(paste(as.character(comp$results$naturalSummary$content), collapse = " "),
                 "coded 2 as competing events, not censored")
})
