# Regression tests for R/singlearm.b.R:
#
#   1. .definemytime()            - numeric-date epoch scale (days vs seconds)
#   2. .baselineHazardAnalysis()  - avoid an arbitrary minimum-event verdict
#   3. .competingRiskPlotRefusal()- advice must match why competing risks are on
#   4. .survTable()               - competing-risk titles and explanation panel
#
# These call the SHIPPED private methods with stubbed `self` / `private`, the
# pattern already used by test-singlearm-hazard-weighting.R.

# The class generator: from whichever shipping package is loaded, else straight
# from the working-tree source (R6Class() substitutes `inherit`, so the
# undefined base class is not a problem until $new() is called).
singlearm_generator <- function() {
    pkg <- intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces())[1]
    if (!is.na(pkg)) {
        g <- tryCatch(get("singlearmClass", envir = asNamespace(pkg)),
                      error = function(e) NULL)
        if (!is.null(g)) return(g)
    }
    src <- Filter(file.exists,
                  c("../../R/singlearm.b.R", "R/singlearm.b.R",
                    testthat::test_path("..", "..", "R", "singlearm.b.R")))
    if (length(src) == 0) return(NULL)
    env <- new.env(parent = globalenv())
    tryCatch({
        sys.source(src[1], envir = env)
        get("singlearmClass", envir = env)
    }, error = function(e) NULL)
}

# A private method rebound onto stub self/private, with message capture.
bind_method <- function(name, options, private_extra = list(), results = NULL) {
    gen <- singlearm_generator()
    skip_if(is.null(gen), "singlearmClass not available")
    f <- gen$private_methods[[name]]
    skip_if(is.null(f), paste(name, "not defined"))

    log <- new.env(parent = emptyenv())
    log$errors <- character(0); log$warnings <- character(0); log$infos <- character(0)

    env <- new.env(parent = environment(f))
    # jmvcore::.() resolves translations through self$options$translate; when the
    # source is read straight from the working tree there is no namespace import
    # to resolve `.` from at all, so bind an identity shim either way.
    options$translate <- function(text, n = 1) text
    assign(".", function(text, n = 1) text, envir = env)
    env$self <- list(options = options, results = results)
    env$private <- utils::modifyList(list(
        .addError    = function(m) log$errors   <- c(log$errors, m),
        .addWarning  = function(m) log$warnings <- c(log$warnings, m),
        .addInfo     = function(m) log$infos    <- c(log$infos, m),
        .checkpoint  = function(...) invisible(NULL),
        .displayMessages = function() invisible(NULL),
        .isCompetingRisk = function(...) FALSE,
        # The REAL estimand label map, rebound into the stub environment: with
        # no declared estimand it returns the endpoint-neutral fallback. Copying
        # those strings into a hand-written stub instead would make the wording
        # assertions below test the stub, not the product.
        .estimandMeta = local({
            g <- gen$private_methods$.estimandMeta
            if (is.null(g)) NULL else `environment<-`(g, env)
        }),
        .safeExecute = function(expr, context = NULL) expr,
        .yearInUnits = function() switch(options$timetypeoutput,
            days = 365.25, weeks = 52.18, months = 12, years = 1, 12)
    ), private_extra)
    environment(f) <- env
    list(f = f, log = log, env = env)
}


# ---------------------------------------------------------------------------
# 1. Numeric date columns: classify each column and reject ambiguity
# ---------------------------------------------------------------------------
# The per-column all(x < 1e5) heuristic let a single sentinel or typo flip one
# column to Unix seconds while the other stayed on days-since-epoch: diagnosis
# read as 1970, follow-up as 2018, i.e. ~48-year survival times with no error
# and no notice.

run_definemytime <- function(dx, fu, unit = "years") {
    n <- length(dx)
    mydata <- data.frame(row_names = seq_len(n), stringsAsFactors = FALSE)
    mydata$dx <- dx
    mydata$fu <- fu
    b <- bind_method(".definemytime",
        options = list(tint = TRUE, timetypedata = "ymd", timetypeoutput = unit,
                       elapsedtime = "time", dxdate = "dx", fudate = "fu"),
        private_extra = list(
            .getData = function() list(mydata_labelled = mydata,
                                       mytime_labelled = "time",
                                       mydxdate_labelled = "dx",
                                       myfudate_labelled = "fu")))
    list(df = b$f(), log = b$log)
}

test_that("a single out-of-range value does not flip the epoch scale", {
    skip_if_not_installed("lubridate")

    dx <- c(17000, 17010, 17020, 17030, 17040, 17050)   # days since 1970
    fu <- dx + 365
    dx[3] <- 999999                                      # sentinel / typo

    out <- run_definemytime(dx, fu)
    expect_null(out$df)
    expect_true(any(grepl("both sides of the date-encoding boundary",
                          out$log$errors, fixed = TRUE)))
})

test_that("an implausible individual follow-up is flagged, not printed", {
    skip_if_not_installed("lubridate")

    dx <- seq(1.5e9, 1.5e9 + 5e6, length.out = 6)
    fu <- dx + 365 * 86400
    fu[3] <- dx[3] + 200 * 365.25 * 86400

    out <- run_definemytime(dx, fu)
    expect_null(out$df)
    expect_true(any(grepl("exceed 150 years", out$log$errors, fixed = TRUE)))
})

test_that("different numeric encodings across date columns are rejected", {
    skip_if_not_installed("lubridate")

    dx <- seq(17000, 17050, by = 10)
    fu <- seq(1.5e9, 1.5e9 + 5e6, length.out = 6)
    out <- run_definemytime(dx, fu)

    expect_null(out$df)
    expect_true(any(grepl("different numeric encodings",
                          out$log$errors, fixed = TRUE)))
})

test_that("ordinary numeric-day dates are unchanged (no new false alarm)", {
    skip_if_not_installed("lubridate")

    dx <- seq(17000, 17050, by = 10)
    out <- run_definemytime(dx, dx + 365)

    expect_equal(stats::median(out$df$mytime), 1, tolerance = 0.01)
    expect_length(out$log$warnings, 0)
    expect_true(any(grepl("DAYS", out$log$infos)))
})

test_that("genuine Unix-second dates are still read as seconds", {
    skip_if_not_installed("lubridate")

    dx <- seq(1.5e9, 1.5e9 + 5e6, length.out = 6)   # ~2017, in seconds
    out <- run_definemytime(dx, dx + 365 * 86400)

    expect_equal(stats::median(out$df$mytime), 1, tolerance = 0.01)
    expect_length(out$log$warnings, 0)
    expect_true(any(grepl("SECONDS", out$log$infos)))
})

test_that("the declared-unit check still fires on pre-calculated time", {
    # The median/implausibility computation was hoisted out of the !tint branch
    # so the dates path could use it too; the declared-unit warning must be
    # unchanged. (test-singlearm-unit-declaration.R covers this end to end.)
    set.seed(3)
    mydata <- data.frame(row_names = 1:60, time = runif(60, 1000, 2000))
    stub <- function(unit) {
        b <- bind_method(".definemytime",
            options = list(tint = FALSE, timetypeoutput = unit,
                           elapsedtime = "time", dxdate = "dx", fudate = "fu"),
            private_extra = list(
                .getData = function() list(mydata_labelled = mydata,
                                           mytime_labelled = "time",
                                           mydxdate_labelled = "dx",
                                           myfudate_labelled = "fu")))
        b$f()
        b$log$warnings
    }
    expect_true(any(grepl("longer than a human lifetime", stub("years"), fixed = TRUE)))
    expect_false(any(grepl("longer than a human lifetime", stub("days"), fixed = TRUE)))
})

test_that("Date-class columns need no scale decision at all", {
    skip_if_not_installed("lubridate")

    dx <- as.Date("2016-01-01") + seq(0, 50, by = 10)
    out <- run_definemytime(dx, dx + 365)

    expect_equal(stats::median(out$df$mytime), 1, tolerance = 0.01)
    expect_length(out$log$infos, 0)      # nothing to disclose: no bare numerics
    expect_length(out$log$warnings, 0)
})


# ---------------------------------------------------------------------------
# 2. Hazard variability verdict
# ---------------------------------------------------------------------------
# n_bins is floor(events / 10): under 20 events there is exactly ONE bin, its
# rate IS the pooled mean and the CV is 0 by construction -- which printed
# "relatively constant (CV = 0)" for every small cohort.

run_hazard_summary <- function(n, lambda = 0.1, seed = 7) {
    set.seed(seed)
    tt <- stats::rexp(n, lambda); cc <- stats::rexp(n, 0.02)
    d <- data.frame(mytime = pmin(tt, cc), myoutcome = as.integer(tt <= cc))

    captured <- NULL
    fake_col <- list(setTitle = function(...) invisible(NULL))
    res <- list(
        baselineHazardTable = list(
            setNote = function(...) invisible(NULL),
            addRow  = function(...) invisible(NULL)),
        baselineHazardSummary = list(
            setContent = function(x) captured <<- x),
        baselineHazardExplanation = list(
            setContent = function(x) invisible(NULL)))

    b <- bind_method(".baselineHazardAnalysis",
        options = list(baseline_hazard = TRUE, hazard_smoothing = FALSE,
                       showSummaries = TRUE, showExplanations = FALSE,
                       timetypeoutput = "months"),
        results = res)
    helper <- singlearm_generator()$private_methods$.hazardIntervals
    environment(helper) <- b$env
    b$env$private$.hazardIntervals <- helper
    b$f(list(name1time = "mytime", name2outcome = "myoutcome", cleanData = d))
    list(html = as.character(captured), n_events = sum(d$myoutcome))
}

test_that("no constant-hazard verdict when there are too few events", {
    skip_if_not_installed("survival")

    out <- run_hazard_summary(n = 18)
    expect_lt(out$n_events, 30)
    expect_false(grepl("relatively constant", out$html, fixed = TRUE))
    expect_true(grepl("not summarized", out$html, fixed = TRUE))
    # A "peak" read off a single bin is just the pooled mean.
    expect_true(grepl("not separable from the pooled rate", out$html, fixed = TRUE))
})

test_that("with enough events the interval range remains descriptive", {
    skip_if_not_installed("survival")

    out <- run_hazard_summary(n = 200)
    expect_gte(out$n_events, 30)
    expect_false(grepl("not summarized", out$html, fixed = TRUE))
    expect_true(grepl("equal-width intervals", out$html, fixed = TRUE))
    expect_true(grepl("descriptive range", out$html, fixed = TRUE))
    expect_true(grepl("not a test of a constant-hazard", out$html, fixed = TRUE))
    expect_false(grepl("little variation|substantial variation", out$html))
})


# ---------------------------------------------------------------------------
# 3. Competing-risk plot refusal: the advice has to be actionable
# ---------------------------------------------------------------------------

refusal_label <- function(multievent, analysistype) {
    captured <- NULL
    b <- bind_method(".competingRiskPlotRefusal",
        options = list(multievent = multievent, analysistype = analysistype,
                       outcome = "Outcome"))
    b$env$print <- function(x, ...) { captured <<- x; invisible(x) }
    b$f(feature = "The cumulative hazard plot")
    # the panel text is hard-wrapped for the plot; compare on one line
    gsub("\\s+", " ", captured$layers[[1]]$aes_params$label)
}

test_that("refusal advice is not 'change a setting that is already set'", {
    skip_if_not_installed("ggplot2")

    # Competing risks came from the DATA (outcomeorganizer hand-off):
    # Survival Type is already Overall, so pointing at it is useless.
    data_driven <- refusal_label(FALSE, "overall")
    expect_true(grepl("will not bring this plot back", data_driven, fixed = TRUE))
    expect_true(grepl("coded 0/1/2", data_driven, fixed = TRUE))

    # The user chose Competing Risk: changing Survival Type really is the fix.
    by_option <- refusal_label(TRUE, "compete")
    expect_true(grepl("Overall, Cause Specific or Disease-Free", by_option, fixed = TRUE))
    expect_false(grepl("will not bring this plot back", by_option, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# 4. Competing-risk survival table: estimand-correct titles, no empty panel
# ---------------------------------------------------------------------------

run_survtable <- function(competing) {
    set.seed(11)
    n <- 120
    d <- data.frame(
        mytime = stats::rexp(n, 0.05),
        myoutcome = if (competing)
            sample(c(0L, 1L, 2L), n, TRUE, prob = c(0.4, 0.4, 0.2))
        else
            sample(c(0L, 1L), n, TRUE))
    d$SingleArm <- "1"

    rec <- new.env(parent = emptyenv()); rec$titles <- list(); rec$content <- list()
    setter <- function(key) function(x) rec$titles[[key]] <- x
    contenter <- function(key) function(x) rec$content[[key]] <- x
    res <- list(
        survTableHeading = list(setVisible = function(...) invisible(NULL),
                                setTitle = setter("heading")),
        survTable = list(
            setTitle  = setter("table"),
            getColumn = function(name) list(setTitle = setter(paste0("col.", name))),
            addRow    = function(...) invisible(NULL),
            setNote   = function(key, note) rec$content[["note"]] <- note),
        survTableSummary = list(setTitle = setter("summary"),
                                setContent = contenter("summary")),
        # The explanation heading and panel are titled at run time too -- a panel
        # headed "Understanding Survival Probabilities" over cumulative-incidence
        # prose is the same estimand confusion one level up.
        survTableHeading3 = list(setTitle = setter("explanationHeading")),
        survivalProbabilityExplanation = list(setTitle = setter("explanationTitle"),
                                              setContent = contenter("explanation")))

    b <- bind_method(".survTable",
        options = list(cutp = "12, 36, 60", timetypeoutput = "months",
                       showExplanations = TRUE, outcome = "Outcome"),
        private_extra = list(
            .isCompetingRisk  = function(...) competing,
            .resolveCutpoints = function(s, what = "Cutpoints") c(5, 10, 20),
            .supportedCutpoints = function(utimes, time, status)
                utimes[utimes <= max(time, na.rm = TRUE)],
            .ciText = function(lo, hi) sprintf(" [%s-%s, 95%% CI]", round(lo, 2), round(hi, 2)),
            .getCachedSurvfit = function(formula, data, key) {
                environment(formula) <- asNamespace("survival")
                survival::survfit(formula, data = data)
            }),
        results = res)
    b$f(list(name1time = "mytime", name2outcome = "myoutcome",
             name3explanatory = "SingleArm", cleanData = d))
    rec
}

test_that("competing-risk table is titled cumulative incidence and explained", {
    skip_if_not_installed("survival")
    rec <- run_survtable(competing = TRUE)

    # Cumulative incidence is not survival.
    expect_true(grepl("Cumulative Incidence", rec$titles$table, fixed = TRUE))
    expect_true(grepl("Cumulative Incidence", rec$titles$heading, fixed = TRUE))
    expect_equal(rec$titles[["col.surv"]], "Cumulative incidence")
    # The cutpoints are user-set: the title must not claim 1, 3, 5 years.
    expect_false(grepl("1, 3, 5", rec$titles$table, fixed = TRUE))

    # The branch used to return before writing the explanation, leaving the
    # visible "Understanding Survival Probabilities" panel empty.
    expect_true(nzchar(rec$content$explanation))
    expect_true(grepl("not 1 minus a Kaplan-Meier", rec$content$explanation, fixed = TRUE))
    expect_true(grepl("Cumulative Incidence", rec$titles$explanationTitle, fixed = TRUE))
    expect_true(grepl("Cumulative Incidence", rec$titles$explanationHeading, fixed = TRUE))
})

test_that("the ordinary Kaplan-Meier table uses endpoint-neutral wording", {
    skip_if_not_installed("survival")
    rec <- run_survtable(competing = FALSE)

    expect_false(grepl("Cumulative Incidence", rec$titles$table, fixed = TRUE))
    expect_true(grepl("Kaplan-Meier event-free probability at Selected Time Points",
                      rec$titles$table, fixed = TRUE))
    expect_equal(rec$titles[["col.surv"]],
                 "Kaplan-Meier event-free probability")
    expect_true(nzchar(rec$content$explanation))
    expect_equal(rec$titles$explanationTitle,
                 "Understanding Kaplan-Meier event-free probability")
})
