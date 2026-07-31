# Regression tests for the shared survival event coder.
#
# These cover the defects that five drifted copies of `.definemyoutcome()` had
# accumulated. Each `expect_` below fails if the corresponding bug comes back.
#
# `.defineEventIndicator()` / `.describeEventIndicator()` are internal, and this
# file ships to both the umbrella package and the jsurvival distribution, so
# resolve them from whichever namespace actually carries them instead of
# assuming they are on the search path.
.event_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists(".defineEventIndicator", envir = .cand, inherits = FALSE)) {
            .event_ns <- .cand
            break
        }
    }
}
if (!is.null(.event_ns)) {
    .defineEventIndicator   <- get(".defineEventIndicator",   envir = .event_ns)
    .describeEventIndicator <- get(".describeEventIndicator", envir = .event_ns)
}

test_that("factor outcome: event level maps to 1, every other level to 0", {
    x <- factor(c("Alive", "Dead", "Alive", "Dead"))
    r <- .defineEventIndicator(x, outcomeLevel = "Dead")
    expect_null(r$error)
    expect_equal(r$status, c(0L, 1L, 0L, 1L))
    expect_equal(r$n_event, 2)
    expect_equal(r$n_censored, 2)
    expect_equal(r$estimand, "overall survival")
})

test_that("NA is preserved, never coded as censored", {
    x <- factor(c("Alive", "Dead", NA, "Dead"))
    r <- .defineEventIndicator(x, outcomeLevel = "Dead")
    expect_true(is.na(r$status[3]))
    expect_equal(r$n_missing, 1)
    expect_equal(r$n_censored, 1)
})

test_that("three-level outcome collapses extras to censored and flags the estimand", {
    x <- factor(c("Alive", "DOD", "DOOC", "Alive"))
    r <- .defineEventIndicator(x, outcomeLevel = "DOD")
    expect_equal(r$status, c(0L, 1L, 0L, 0L))
    expect_equal(r$n_levels, 3)
    expect_equal(r$estimand, "cause-specific survival")
    expect_setequal(r$censored_labels, c("Alive", "DOOC"))
    # The disclosure must warn that the probability-scale outputs are biased.
    expect_match(.describeEventIndicator(r, "Outcome2"), "biased upward")
})

test_that("D1: missing event level is a message, not a crash", {
    x <- factor(c("Alive", "Dead"))
    r <- .defineEventIndicator(x, outcomeLevel = NULL)
    expect_match(r$error, "Event Level is not selected")
})

test_that("D2: numeric predicate accepts only genuine 0/1 coding", {
    # sum(unique(x)) == 1 used to accept this.
    expect_match(.defineEventIndicator(c(-1, 2, -1, 2))$error, "not coded 0/1")
    expect_match(.defineEventIndicator(c(1, 2, 1, 2))$error, "not coded 0/1")
    # ... and used to reject these, which are legitimate.
    expect_null(.defineEventIndicator(c(1, 1, 1))$error)
    expect_null(.defineEventIndicator(c(0, 0, 0))$error)
    expect_equal(.defineEventIndicator(c(1, 1, 1))$n_event, 3)
})

test_that("D3: numeric outcomes honour an explicitly chosen event level", {
    # 0 = dead, 1 = alive registry coding used to run inverted, silently.
    r <- .defineEventIndicator(c(0, 1, 0, 1), outcomeLevel = "0")
    expect_null(r$error)
    expect_equal(r$status, c(1L, 0L, 1L, 0L))
    expect_equal(r$n_event, 2)
    # 1/2 coding becomes usable once a level is chosen.
    expect_equal(.defineEventIndicator(c(1, 2, 1), outcomeLevel = "2")$status,
                 c(0L, 1L, 0L))
})

test_that("D4: an event level that is not a level of the variable errors", {
    # REVISED. This used to also reject a DECLARED but unobserved level, which
    # threw away a legitimate fully censored cohort (see
    # test-singlearm-zero-event-and-estimand.R). Only a level the variable does
    # not have at all is unusable.
    x <- factor(c("Alive", "Alive"), levels = c("Alive", "Dead"))
    expect_match(.defineEventIndicator(x, outcomeLevel = "Deceased")$error,
                 "is not a level of")

    # ... and the declared-but-unobserved level is now analysed, with 0 events.
    r <- .defineEventIndicator(x, outcomeLevel = "Dead")
    expect_null(r$error)
    expect_equal(r$n_event, 0)
    expect_equal(r$n_censored, 2)
})

test_that("M3: an unassigned level errors instead of deleting those patients", {
    x <- factor(c("DOD", "DOOC", "AWD", "AWOD"))
    r <- .defineEventIndicator(x, multievent = TRUE, analysistype = "overall",
                               dod = "DOD")
    expect_match(r$error, "not assigned to any category")
    # A blank bucket must not be reported as a duplicate.
    expect_false(grepl("only one category", r$error))
})

test_that("M3: reusing one level for two categories is reported as a duplicate", {
    x <- factor(c("DOD", "DOOC", "AWD", "AWOD"))
    r <- .defineEventIndicator(x, multievent = TRUE, analysistype = "overall",
                               dod = "DOD", dooc = "DOD", awd = "AWD", awod = "AWOD")
    expect_match(r$error, "only one category")
})

test_that("multievent recodes match the requested survival type", {
    x <- factor(c("DOD", "DOOC", "AWD", "AWOD"))
    args <- list(x, multievent = TRUE, dod = "DOD", dooc = "DOOC",
                 awd = "AWD", awod = "AWOD")

    overall <- do.call(.defineEventIndicator, c(args, analysistype = "overall"))
    expect_equal(overall$status, c(1L, 1L, 0L, 0L))

    cause <- do.call(.defineEventIndicator, c(args, analysistype = "cause"))
    expect_equal(cause$status, c(1L, 0L, 0L, 0L))

    compete <- do.call(.defineEventIndicator, c(args, analysistype = "compete"))
    expect_equal(compete$status, c(1L, 2L, 0L, 0L))
    expect_true(compete$has_competing)
    expect_equal(levels(compete$status_factor), c("Censored", "Event", "Competing"))
})

test_that("M1: disease-free survival counts Alive with Disease as an event", {
    x <- factor(c("DOD", "DOOC", "AWD", "AWOD"))
    r <- .defineEventIndicator(x, multievent = TRUE, analysistype = "dfs",
                               dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    expect_null(r$error)
    expect_equal(r$status, c(1L, 1L, 1L, 0L))   # AWD is an event, AWOD is not
    expect_equal(r$estimand, "disease-free survival")
})

test_that("D5: missing outcome in multievent stays missing, never censored", {
    x <- factor(c("DOD", NA, "AWOD", "DOOC"))
    r <- .defineEventIndicator(x, multievent = TRUE, analysistype = "compete",
                               dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    # AWD is unused in the data but assigned, which is fine; NA must survive.
    expect_true(is.na(r$status[2]))
    expect_equal(r$n_missing, 1)
})

test_that("M2: a labelled cause factor survives the outcomeorganizer hand-off", {
    x <- factor(c("Censored", "Event", "Competing", "Censored"),
                levels = c("Censored", "Event", "Competing"))
    r <- .defineEventIndicator(x)
    expect_null(r$error)
    expect_equal(r$status, c(0L, 1L, 2L, 0L))
    expect_true(r$has_competing)
    expect_equal(r$estimand, "competing risks")
})

test_that("the recode disclosure reports event, censored and excluded counts", {
    x <- factor(c("Alive", "Dead", NA, "Dead"))
    html <- .describeEventIndicator(.defineEventIndicator(x, outcomeLevel = "Dead"),
                                    "Death")
    expect_match(html, "Event level")
    expect_match(html, "Excluded \\(missing outcome\\)")
    expect_match(html, "overall survival")
})

# --- regressions found in the second review -------------------------------

test_that("a user factor containing a level named 'Event' is not mistaken for a hand-off", {
    # 20 "No event" + 10 "Event"; the user selects "No event" as the event.
    # This used to be read as an outcomeorganizer Censored/Event/Competing
    # column: it selected "Event" instead, and mapped all 20 "No event" rows to
    # NA -- deleting those patients and reporting a 100% event rate.
    x <- factor(c(rep("No event", 20), rep("Event", 10)))
    r <- .defineEventIndicator(x, outcomeLevel = "No event")
    expect_null(r$error)
    expect_equal(r$event_label, "No event")
    expect_equal(r$n_event, 20)
    expect_equal(r$n_censored, 10)
    expect_equal(r$n_missing, 0)
})

test_that("a genuine Censored/Event/Competing hand-off is still recognised", {
    x <- factor(c("Censored", "Event", "Competing"),
                levels = c("Censored", "Event", "Competing"))
    r <- .defineEventIndicator(x)
    expect_equal(r$status, c(0L, 1L, 2L))
    expect_true(r$has_competing)
    # ... and still recognised when the event level names one of its own levels
    expect_equal(.defineEventIndicator(x, outcomeLevel = "Event")$status, c(0L, 1L, 2L))
})

test_that("a declared hand-off stays in competing-risk mode when one state is unused", {
    x <- factor(c("Censored", "Event", "Censored", "Event"),
                levels = c("Censored", "Event", "Competing"))
    r <- .defineEventIndicator(x, outcomeLevel = "Event")
    expect_null(r$error)
    expect_true(r$has_competing)
    expect_equal(r$n_competing, 0)
    expect_equal(r$estimand, "competing risks")
    expect_equal(levels(r$status_factor), c("Censored", "Event", "Competing"))
})

test_that("Censored cannot be selected as the event in the hand-off format", {
    x <- factor(c("Censored", "Event", "Competing"),
                levels = c("Censored", "Event", "Competing"))
    expect_match(.defineEventIndicator(x, outcomeLevel = "Censored")$error,
                 "cannot be selected as the event")
})

test_that("an ordinary Censored/Event binary factor is not forced into competing-risk mode", {
    x <- factor(c("Censored", "Event", "Censored"),
                levels = c("Censored", "Event"))
    r <- .defineEventIndicator(x, outcomeLevel = "Event")
    expect_equal(r$status, c(0L, 1L, 0L))
    expect_false(r$has_competing)
    expect_equal(r$estimand, "overall survival")
})

test_that("competing-event labels are disclosed, not left blank", {
    x <- factor(c("Censored", "Event", "Competing"),
                levels = c("Censored", "Event", "Competing"))
    html <- .describeEventIndicator(.defineEventIndicator(x, outcomeLevel = "Event"),
                                    "Outcome")
    expect_match(html, "Competing event")
    expect_match(html, '"Competing"')
})

test_that("logical outcomes honour an explicitly selected FALSE event level", {
    l <- c(TRUE, FALSE, TRUE, FALSE)
    expect_equal(.defineEventIndicator(l)$status, c(1L, 0L, 1L, 0L))
    expect_equal(.defineEventIndicator(l, outcomeLevel = "FALSE")$status, c(0L, 1L, 0L, 1L))
})
