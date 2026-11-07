# Regressions for the 2026-10-30 jsurvival audit findings on outcomeorganizer:
#   1. The single-event DFS composite (outcome level OR recurrence) set the
#      "disease-free survival" estimand, and the shared describer then told the
#      user that "Alive with Disease" is counted as an event -- a category that
#      does not exist on that path. The multi-event DFS coding keeps the panel.
#   2. Syntax mode emitted data = "data", so the copied call failed with
#      "Argument 'data' must be a data frame".

.oo_obj2 <- function(data, ...) {
    Opt <- get0("outcomeorganizerOptions", inherits = TRUE)
    Cls <- get0("outcomeorganizerClass", inherits = TRUE)
    if (is.null(Opt) || is.null(Cls))
        testthat::skip("outcomeorganizer classes are not reachable from the test environment")
    args <- list(...)
    for (lv in c("outcomeLevel", "recurrenceLevel", "dod", "dooc", "awd", "awod"))
        if (!lv %in% names(args)) args[lv] <- list(NULL)
    a <- Cls$new(options = do.call(Opt$new, args), data = data)
    a$init()
    suppressWarnings(a$run())
    a
}

.oo_dfs_data <- function() {
    set.seed(7)
    data.frame(vital = factor(sample(c("Dead", "Alive"), 60, TRUE)),
               rec = factor(sample(c("Yes", "No"), 60, TRUE)))
}

test_that("single-event DFS panel does not claim an Alive-with-Disease event", {
    a <- .oo_obj2(.oo_dfs_data(), outcome = "vital", outcomeLevel = "Dead",
                  recurrence = "rec", recurrenceLevel = "Yes", analysistype = "dfs")
    txt <- a$results$eventRecodeInfo$content
    expect_true(nzchar(txt))
    expect_match(txt, "disease-free survival", fixed = TRUE)
    expect_false(grepl("Alive with Disease", txt, ignore.case = TRUE))
    # The time-to-first-event advice is still given, in the warnings panel.
    expect_match(a$results$warnings$content, "FIRST event", fixed = TRUE)
})

test_that("multi-event DFS coding keeps the Alive-with-Disease timing panel", {
    define <- get0(".defineEventIndicator", mode = "function", inherits = TRUE)
    describe <- get0(".describeEventIndicator", mode = "function", inherits = TRUE)
    if (is.null(define) || is.null(describe)) testthat::skip("event-indicator helpers not reachable")
    set.seed(3)
    st <- factor(sample(c("DOD", "DOOC", "AWD", "AWOD"), 80, TRUE))
    res <- define(st, multievent = TRUE, analysistype = "dfs",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    expect_match(describe(res, "st"), "\"Alive with Disease\" is counted as an event", fixed = TRUE)
})

test_that("Syntax-mode call passes the data frame and runs", {
    oo <- .oo_dfs_data()
    a <- .oo_obj2(oo, outcome = "vital", outcomeLevel = "Dead",
                  recurrence = "rec", recurrenceLevel = "Yes", analysistype = "dfs")
    src <- a$asSource()
    expect_match(src, "data = data,", fixed = TRUE)
    expect_false(grepl("data = \"data\"", src, fixed = TRUE))
    wrapper <- get0("outcomeorganizer", mode = "function", inherits = TRUE)
    call_src <- sub("^[A-Za-z.]+::", "", sub("data = data", "data = oo", src, fixed = TRUE))
    res <- suppressWarnings(eval(parse(text = call_src), envir = list(oo = oo, outcomeorganizer = wrapper)))
    expect_false(is.null(res))
})
