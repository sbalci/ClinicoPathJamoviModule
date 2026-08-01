# Regression tests for the shared event-indicator helpers in R/survival_utils.R
# and for the outcomeorganizer hand-off.

get_util <- function(nm) {
    if (exists(nm, inherits = TRUE)) return(get(nm, inherits = TRUE))
    pkg <- intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces())[1]
    if (is.na(pkg)) return(NULL)
    tryCatch(get(nm, envir = asNamespace(pkg)), error = function(e) NULL)
}

test_that("cause-specific censoring is disclosed even with only two observed levels", {
    define   <- get_util(".defineEventIndicator")
    describe <- get_util(".describeEventIndicator")
    skip_if(is.null(define) || is.null(describe), "survival_utils not available")

    # Only DOD and DOOC occur. analysistype = "cause" codes DOOC to 0, i.e. a
    # death is entered as a censoring -- which biases KM / median / x-year
    # survival upward. The disclosure used to be gated on n_levels > 2, so this
    # (2-level) case got no warning at all.
    oc <- factor(rep(c("DOD", "DOOC"), each = 25))
    res <- define(outcome = oc, multievent = TRUE, analysistype = "cause",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                  outcome_name = "outcome")

    expect_null(res$error)
    expect_equal(res$estimand, "cause-specific survival")
    expect_equal(res$n_levels, 2L)
    expect_true(grepl("overstates", describe(res, "outcome"), fixed = TRUE))
    expect_true(grepl("DOOC", describe(res, "outcome"), fixed = TRUE))
})

test_that("three-level cause-specific censoring is still disclosed", {
    define   <- get_util(".defineEventIndicator")
    describe <- get_util(".describeEventIndicator")
    skip_if(is.null(define) || is.null(describe), "survival_utils not available")

    oc <- factor(rep(c("Dead", "Alive", "Lost"), each = 20))
    res <- define(outcome = oc, outcomeLevel = "Dead", outcome_name = "outcome")
    expect_equal(res$estimand, "Kaplan-Meier survival for the selected event")
    expect_true(grepl("overstates", describe(res, "outcome"), fixed = TRUE))
})

test_that("an ordinary two-level outcome is not mislabeled as overall survival", {
    define   <- get_util(".defineEventIndicator")
    describe <- get_util(".describeEventIndicator")
    skip_if(is.null(define) || is.null(describe), "survival_utils not available")

    oc <- factor(rep(c("Dead", "Alive"), each = 20))
    res <- define(outcome = oc, outcomeLevel = "Dead", outcome_name = "outcome")
    expect_equal(res$estimand, "Kaplan-Meier survival for the selected event")
    expect_true(grepl("right-censored", describe(res, "outcome"), fixed = TRUE))
    expect_true(grepl("If a collapsed level is a competing terminal event",
                      describe(res, "outcome"), fixed = TRUE))
})

test_that("the censoring assumption is disclosed for explicit overall survival", {
    define   <- get_util(".defineEventIndicator")
    describe <- get_util(".describeEventIndicator")
    skip_if(is.null(define) || is.null(describe), "survival_utils not available")

    oc <- factor(c("DOD", "DOOC", "AWD", "AWOD"))
    res <- define(outcome = oc, multievent = TRUE, analysistype = "overall",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    expect_null(res$error)
    expect_match(describe(res), "Censoring assumption")
    expect_match(describe(res), "independent/non-informative")
})

test_that("an invalid logical event level is rejected", {
    define <- get_util(".defineEventIndicator")
    skip_if(is.null(define), "survival_utils not available")

    res <- define(c(TRUE, FALSE, TRUE), outcomeLevel = "Dead",
                  outcome_name = "logical_status")
    expect_match(res$error, "Select TRUE or FALSE")
})

test_that("a factor with extra declared levels is not hijacked as a hand-off", {
    define <- get_util(".defineEventIndicator")
    skip_if(is.null(define), "survival_utils not available")

    oc <- factor(c("Censored", "Event", "Competing"),
                 levels = c("Censored", "Event", "Competing", "Unknown"))
    res <- define(oc, outcomeLevel = "Event", outcome_name = "status")
    expect_null(res$error)
    expect_false(res$has_competing)
    expect_equal(res$estimand, "Kaplan-Meier survival for the selected event")
})

test_that("the outcomeorganizer hand-off keeps its competing-risk labels", {
    define <- get_util(".defineEventIndicator")
    skip_if(is.null(define), "survival_utils not available")

    # A Censored/Event/Competing column arrives with multievent = FALSE (the user
    # never fills dod/dooc -- that is the point of the recoded column). The status
    # vector comes back 0/1/2 with a non-NULL status_factor. outcomeorganizer used
    # to ask its options branch instead of this vector, leave .causeFactor NULL,
    # and export the raw numeric; round-tripping that column turned every
    # competing event into a censored observation.
    oc <- factor(c("Censored", "Event", "Competing", "Event", "Censored", "Competing"),
                 levels = c("Censored", "Event", "Competing"))
    res <- define(outcome = oc, outcomeLevel = "Event", multievent = FALSE,
                  outcome_name = "outcome")

    expect_null(res$error)
    expect_equal(as.integer(res$status), c(0L, 1L, 2L, 1L, 0L, 2L))
    expect_true(res$has_competing)
    expect_false(is.null(res$status_factor))
    expect_equal(as.character(res$status_factor),
                 c("Censored", "Event", "Competing", "Event", "Censored", "Competing"))
    # NOTE: the downstream half of this fix -- that outcomeorganizer exports the
    # character labels rather than 0/1/2 -- cannot be asserted here. `addOutcome`
    # is a jamovi Output option, so it is not an argument of the R wrapper and is
    # always FALSE outside the GUI.
})
