# Regressions for the 2026-10-29 release-check findings on outcomeorganizer:
#   1. Summary / copy-ready text named code 0 "alive" and code 1 "any cause" in
#      single-event mode, where an other-cause death is coded 0.
#   2. An unset level printed as an empty "()".
#   3. TTP demanded the outcome's Event Level, which it never reads.
#   4. The distribution plot ordered its bars alphabetically, not by code.

# Resolves the analysis from whatever namespace the tests run in (the umbrella
# under load_all(), jsurvival under test_file(package = "jsurvival")), unlike
# helper-outcomeorganizer.R, which looks only for ClinicoPath and skips otherwise.
.oo_obj <- function(data, ...) {
    Opt <- get0("outcomeorganizerOptions", inherits = TRUE)
    Cls <- get0("outcomeorganizerClass", inherits = TRUE)
    if (is.null(Opt) || is.null(Cls))
        testthat::skip("outcomeorganizer classes are not reachable from the test environment")
    args <- list(...)
    for (lv in c("outcomeLevel", "recurrenceLevel", "dod", "dooc", "awd", "awod"))
        if (!lv %in% names(args)) args[lv] <- list(NULL)
    a <- Cls$new(options = do.call(Opt$new, args), data = data)
    a$init()
    suppressWarnings(try(a$run(), silent = TRUE))
    a
}

.oo_text <- function(x) {
    h <- paste(as.character(x), collapse = " ")
    h <- gsub("<[^>]+>", " ", h)
    gsub("\\s+", " ", h)
}

.oo_four <- function(n = 40, seed = 11) {
    set.seed(seed)
    data.frame(
        o4  = factor(sample(c("AWOD", "AWD", "DOD", "DOOC"), n, TRUE),
                     levels = c("AWOD", "AWD", "DOD", "DOOC")),
        vit = factor(sample(c("Alive", "Dead"), n, TRUE)),
        rec = factor(sample(c("No", "Yes"), n, TRUE)),
        n01 = rbinom(n, 1, 0.5),
        stringsAsFactors = FALSE)
}

testthat::test_that("single-event OS names the levels in code 1 and code 0 (other-cause deaths are coded 0)", {
    a <- .oo_obj(.oo_four(), outcome = "o4", outcomeLevel = "DOD",
                                  analysistype = "os", showNaturalSummary = TRUE)
    s <- .oo_text(a$results$summary$content)
    testthat::expect_true(nzchar(s))
    testthat::expect_false(grepl("Alive (other levels)", s, fixed = TRUE))
    testthat::expect_match(s, "Event ('DOD'): coded as 1", fixed = TRUE)
    testthat::expect_match(s, "Censored \\([^)]*'DOOC'[^)]*\\): coded as 0")

    nat <- .oo_text(a$results$naturalSummary$content)
    testthat::expect_false(grepl("death from any cause", nat, fixed = TRUE))
    testthat::expect_false(grepl("remain alive", nat, fixed = TRUE))
    testthat::expect_match(nat, "Code 1 represents outcome level 'DOD'.", fixed = TRUE)
    testthat::expect_match(nat, "Code 0 represents outcome level\\(s\\) [^.]*'DOOC'")
})

testthat::test_that("RFS text does not call code 1 'death' or code 0 'event-free' beyond the chosen levels", {
    a <- .oo_obj(.oo_four(), outcome = "o4", outcomeLevel = "DOD",
                                  recurrence = "rec", recurrenceLevel = "Yes",
                                  analysistype = "rfs", showNaturalSummary = TRUE)
    s <- .oo_text(a$results$summary$content)
    testthat::expect_match(s, "Outcome level 'DOD': coded as 1", fixed = TRUE)
    testthat::expect_match(s, "Outcome level\\(s\\) [^:]*'DOOC'[^:]* without recurrence/progression: coded as 0")
    nat <- .oo_text(a$results$naturalSummary$content)
    testthat::expect_false(grepl("recurrence/progression or death", nat, fixed = TRUE))
    testthat::expect_match(nat, "recurrence/progression (level 'Yes') or outcome level 'DOD'", fixed = TRUE)
})

testthat::test_that("no empty brackets when a level is not set", {
    # numeric 0/1 outcome, no Event Level needed
    a <- .oo_obj(.oo_four(), outcome = "n01", analysistype = "os")
    s <- .oo_text(a$results$summary$content)
    testthat::expect_true(nzchar(s))
    testthat::expect_false(grepl("()", s, fixed = TRUE))
    testthat::expect_match(s, "Event ('1'): coded as 1", fixed = TRUE)

    # multi-event with an unassigned slot (no AWD patients)
    d <- .oo_four()
    d <- d[d$o4 != "AWD", ]
    d$o4 <- droplevels(d$o4)
    b <- .oo_obj(d, outcome = "o4", multievent = TRUE, analysistype = "os",
                                  dod = "DOD", dooc = "DOOC", awod = "AWOD")
    s2 <- .oo_text(b$results$summary$content)
    testthat::expect_true(nzchar(s2))
    testthat::expect_false(grepl("()", s2, fixed = TRUE))
    testthat::expect_match(s2, "Alive with disease (not set)", fixed = TRUE)
})

testthat::test_that("TTP runs without the outcome Event Level and codes from the recurrence variable", {
    d <- .oo_four()
    a <- .oo_obj(d, outcome = "vit", recurrence = "rec",
                                  recurrenceLevel = "Yes", analysistype = "ttp")
    rc <- a$.__enclos_env__$private$.eventRecode
    testthat::expect_false(is.null(rc))
    testthat::expect_equal(as.numeric(rc$status), as.numeric(d$rec == "Yes"))
    testthat::expect_identical(rc$event_label, "Yes")
    testthat::expect_identical(rc$censored_labels, "No")
    s <- .oo_text(a$results$summary$content)
    testthat::expect_match(s, "Progression ('Yes'): coded as 1", fixed = TRUE)
    testthat::expect_match(s, "No progression ('No'), including deaths: coded as 0", fixed = TRUE)

    # Same status as when an (unused) outcome Event Level is supplied.
    b <- .oo_obj(d, outcome = "vit", outcomeLevel = "Dead", recurrence = "rec",
                                  recurrenceLevel = "Yes", analysistype = "ttp")
    testthat::expect_equal(b$.__enclos_env__$private$.eventRecode$status, rc$status)

    # A Censored/Event/Competing outcome must not leave competing-risk export labels on a TTP status.
    d$cr <- factor(sample(c("Censored", "Event", "Competing"), nrow(d), TRUE),
                   levels = c("Censored", "Event", "Competing"))
    h <- .oo_obj(d, outcome = "cr", recurrence = "rec",
                                  recurrenceLevel = "Yes", analysistype = "ttp")
    testthat::expect_false(is.null(h$.__enclos_env__$private$.eventRecode))
    testthat::expect_null(h$.__enclos_env__$private$.causeFactor)
})

testthat::test_that("distribution plot bars follow code order, not alphabetical labels", {
    a <- .oo_obj(.oo_four(), outcome = "o4", multievent = TRUE,
                                  analysistype = "multistate", visualization = TRUE,
                                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    img <- a$results$outcomeViz
    testthat::expect_false(is.null(img$state))
    f <- tempfile(fileext = ".png")
    grDevices::png(f)
    on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
    a$.__enclos_env__$private$.plotOutcome(img, ggplot2::theme_grey(), NULL)
    lv <- levels(ggplot2::last_plot()$data$Label)
    testthat::expect_identical(lv, c("Disease-free (0)", "Disease state (1)",
                                     "Death from disease (2)", "Death from other causes (3)"))
})
