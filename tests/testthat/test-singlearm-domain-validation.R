# Regression tests for the singlearm input-domain, risk-table, explanation,
# export and data-quality-grading fixes. Each test names the defect it locks
# down; if one fails, the corresponding bug is back.
#
# Every test that changes a message also asserts the NEW behaviour
# unconditionally -- no assertion is placed inside an if() on the wording being
# changed, because that pattern silently records as an empty pass.

# singlearmOptions / singlearmClass are INTERNAL: the module exports only the
# singlearm() wrapper, so the renderer tests below have to reach them through the
# namespace. Resolved once, the same way the other singlearm test files do it,
# because this suite runs against ClinicoPath (umbrella) and jsurvival (shipping
# module) alike.
.sa_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("singlearmOptions", envir = .cand, inherits = FALSE)) {
            .sa_ns <- .cand
            break
        }
    }
}
# Deliberately stop() rather than skip_if(): a file-scope skip would silently
# record this entire suite as zero tests, which is how a real regression hides.
# Every other test in this file calls the exported singlearm() directly, so the
# package is available by construction -- if this ever fails, the harness is
# broken and must say so out loud.
if (is.null(.sa_ns))
    stop("singlearm namespace not found: cannot reach internal singlearmOptions/singlearmClass")

# Bind the two internal generators ONCE, here, so every helper below can name
# them plainly. Resolving them at each call site is what let run_with_outputs()
# keep a bare `singlearmOptions` reference long after render_km() was fixed --
# the file then failed only in the two tests that happened to use that helper.
singlearmOptions <- get("singlearmOptions", envir = .sa_ns)
singlearmClass   <- get("singlearmClass",   envir = .sa_ns)

run_singlearm <- function(...) {
    args <- list(...)
    # Keep this helper compatible with installed versions whose generated
    # wrapper may predate the NULL defaults on optional Level arguments.
    for (lvl in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
        if (is.null(args[[lvl]])) args[lvl] <- list(NULL)
    do.call(singlearm, args)
}

strip_html <- function(x) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(x, collapse = " "))))

simple_data <- function(times, dead) {
    data.frame(time = times,
               status = factor(ifelse(dead, "Dead", "Alive"),
                               levels = c("Alive", "Dead")))
}

# Alternating dead/alive over 10 subjects: total person-time is exactly 55.
ten_subjects <- function()
    simple_data(1:10, c(TRUE, FALSE, TRUE, FALSE, TRUE,
                        FALSE, TRUE, FALSE, TRUE, FALSE))

col_of <- function(table, name)
    vapply(seq_len(table$rowCount),
           function(i) as.character(table$getCell(rowNo = i, name)$value),
           character(1))

num_of <- function(table, name)
    vapply(seq_len(table$rowCount),
           function(i) as.numeric(table$getCell(rowNo = i, name)$value),
           numeric(1))


# ---------------------------------------------------------------- V1: domain

test_that("an infinite follow-up time is rejected instead of poisoning every summary", {
    # Was: only `<= 0` was checked. One Inf gave total person-time Inf, an
    # incidence rate of events/Inf = 0 with a 0-0 confidence interval, a NaN
    # restricted mean, and a "Long-term" follow-up grade from max(time) = Inf.
    d <- simple_data(c(1, 2, 3, 4, 5, Inf),
                     c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", person_time = TRUE)

    expect_match(strip_html(res$errors$content), "infinite")
    expect_equal(res$personTimeTable$rowCount, 0)
    expect_equal(res$medianTable$rowCount, 0)
})

test_that("a missing follow-up time is still only excluded, not rejected", {
    # The regression risk of the test above: NA is not finite either, and
    # treating it like Inf would turn every ordinary incomplete row into a hard
    # error. NA must keep its own warning-and-exclude path.
    d <- simple_data(c(1, 2, 3, 4, 5, NA),
                     c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", person_time = TRUE)

    expect_equal(strip_html(res$errors$content), "")
    expect_match(strip_html(res$warnings$content), "missing value")
    expect_equal(res$personTimeTable$getCell(rowNo = 1, "person_time")$value, 15)
})


test_that("a negative rate multiplier is rejected instead of printing negative incidence rates", {
    # Was: rate_multiplier = -100 gave a rate of -9.09 with a "95% CI" of
    # -2.95 to -21.22 -- negative, and with the limits in reverse order.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         person_time = TRUE, rate_multiplier = -100, cutp = "5")

    expect_match(strip_html(res$errors$content), "Rate multiplier must be a finite positive number")
    expect_equal(res$personTimeTable$rowCount, 0)
    # Named regression of this change: only the person-time section is skipped.
    # The rest of the analysis is unaffected by a bad rate unit.
    expect_equal(res$medianTable$rowCount, 1)
    expect_true(res$survTable$rowCount > 0)
})

test_that("a valid rate multiplier still scales the incidence rate", {
    # The regression risk of the test above: a guard written as `< 0` or applied
    # to the wrong option would silently disable person-time for everyone.
    r100 <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                          outcome = "status", outcomeLevel = "Dead",
                          person_time = TRUE, rate_multiplier = 100)
    r1000 <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                           outcome = "status", outcomeLevel = "Dead",
                           person_time = TRUE, rate_multiplier = 1000)

    expect_equal(r100$personTimeTable$getCell(rowNo = 1, "rate")$value, 9.09)
    expect_equal(r1000$personTimeTable$getCell(rowNo = 1, "rate")$value, 90.91)
})

test_that("a user variable named row_names is not overwritten by the join key", {
    d <- data.frame(
        row_names = c(100, 200, 300, 400, 500, 600),
        status = factor(c("Dead", "Dead", "Dead", "Alive", "Alive", "Alive"),
                        levels = c("Alive", "Dead")))
    res <- run_singlearm(data = d, elapsedtime = "row_names",
                         outcome = "status", outcomeLevel = "Dead")

    expect_equal(strip_html(res$errors$content), "")
    expect_equal(res$medianTable$getCell(rowNo = 1, "median")$value, 300)
})

test_that("the same variable cannot serve as elapsed time and outcome", {
    d <- data.frame(x = c(0, 1, 1, 0, 1))
    res <- run_singlearm(data = d, elapsedtime = "x", outcome = "x",
                         outcomeLevel = "1")

    expect_match(strip_html(res$errors$content),
                 "Elapsed time and outcome must be different variables")
    expect_equal(res$medianTable$rowCount, 0)
})

test_that("start and end dates must be different variables", {
    d <- data.frame(
        date = as.Date("2020-01-01") + 0:4,
        status = factor(c("Alive", "Dead", "Alive", "Dead", "Alive")))
    res <- run_singlearm(data = d, tint = TRUE, dxdate = "date", fudate = "date",
                         outcome = "status", outcomeLevel = "Dead")

    expect_match(strip_html(res$errors$content),
                 "must be different variables")
    expect_equal(res$medianTable$rowCount, 0)
})

test_that("a time-zero event is not divided by later person-time", {
    d <- simple_data(c(0, 2, 4, 6), c(TRUE, FALSE, TRUE, FALSE))
    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", person_time = TRUE)

    expect_equal(res$personTimeTable$rowCount, 0)
    expect_match(strip_html(res$warnings$content),
                 "Person-time rates were not calculated.*time zero")
    expect_equal(res$medianTable$rowCount, 1)
})


test_that("negative person-time interval boundaries cannot invent person-time", {
    # Was: time_intervals = "-5, 5" built breaks c(0, -5, 5, max*1.1). The
    # interval labelled "-5-5" accrued 90 units of person-time from a cohort
    # containing 55 in total, because every subject was credited with the 5
    # units between -5 and 0 that nobody was ever observed for.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         person_time = TRUE, time_intervals = "-5, 5")

    expect_match(strip_html(res$warnings$content),
                 "Person-time intervals must be finite and zero or positive: -5 ignored")

    intervals <- col_of(res$personTimeTable, "interval")
    # "0-5" legitimately contains "-5"; what must not appear is a boundary that
    # IS negative, i.e. a label beginning with a minus sign.
    expect_false(any(grepl("^-", intervals)))

    # The stratified rows partition the follow-up, so their person-time must add
    # up to the overall row -- 55, never 105.
    pt <- num_of(res$personTimeTable, "person_time")
    expect_equal(pt[1], 55)
    expect_equal(sum(pt[-1]), 55)
})

test_that("valid person-time interval boundaries are untouched", {
    # The regression risk of the test above: an over-eager filter that drops
    # ordinary boundaries would silently collapse the stratified analysis.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         person_time = TRUE, time_intervals = "3, 6")

    expect_equal(col_of(res$personTimeTable, "interval")[1:3],
                 c("Overall (0-max)", "0-3", "3-6"))
    expect_equal(res$personTimeTable$rowCount, 4)
    pt <- num_of(res$personTimeTable, "person_time")
    expect_equal(sum(pt[-1]), 55)
})


test_that("a survival probability is not reported at a negative time point", {
    # Was: cutp = "-2, 1.5, 4" printed a row at time -2 with survival 100% and
    # a 100-100% confidence interval, because summary.survfit(extend = TRUE)
    # carries S(t) = 1 backwards without complaint.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         cutp = "-2, 1.5, 4")

    times <- num_of(res$survTable, "time")
    expect_true(all(times >= 0))
    expect_equal(times, c(1.5, 4))
    expect_match(strip_html(res$warnings$content), "Cutpoints must be finite and")
    expect_match(strip_html(res$warnings$content), "-2 ignored")
})

test_that("cutpoints that are all invalid fall back to the documented defaults, out loud", {
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         cutp = "-2, -5")

    warned <- strip_html(res$warnings$content)
    expect_match(warned, "Cutpoints must be finite and")
    expect_match(warned, "No usable cutpoints remained")
})

test_that("ordinary cutpoints are still used exactly as entered", {
    # The regression risk of the two tests above.
    res <- run_singlearm(data = simple_data(c(2, 5, 7, 9, 11, 14, 18, 22, 26, 30),
                                            c(TRUE, FALSE, TRUE, TRUE, FALSE,
                                              TRUE, FALSE, TRUE, FALSE, TRUE)),
                         elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", cutp = "6, 12, 24")

    expect_equal(num_of(res$survTable, "time"), c(6, 12, 24))
    expect_false(grepl("ignored", strip_html(res$warnings$content)))
    expect_false(grepl("No usable cutpoints", strip_html(res$warnings$content)))
})


test_that("invalid plot parameters are reported where the user can see them", {
    # Two defects at once. The y-axis carries a probability but nothing checked
    # that its limits were inside 0-1, and break.time.by was never checked at
    # all. Worse, the existing checks added their message from inside the plot
    # renderer -- which runs AFTER .run() has already rendered the notice list,
    # so the panel just went blank in silence.
    d <- ten_subjects()

    bad_y <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                           outcomeLevel = "Dead", sc = TRUE, yend_plot = 50)
    expect_match(strip_html(bad_y$errors$content), "must lie within 0 and 1")

    bad_by <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                            outcomeLevel = "Dead", sc = TRUE, byplot = 0)
    expect_match(strip_html(bad_by$errors$content), "axis ticks must be")

    bad_end <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                             outcomeLevel = "Dead", sc = TRUE, endplot = 0)
    expect_match(strip_html(bad_end$errors$content), "Plot end time")

    # Named regression of this change: a y-axis end of 1.05, used purely as
    # head-room above the curve, is now rejected too. Head-room on a bounded
    # probability scale is not worth a second option; state it in the message.
    headroom <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                              outcomeLevel = "Dead", sc = TRUE, yend_plot = 1.05)
    expect_match(strip_html(headroom$errors$content), "must lie within 0 and 1")
})

test_that("ordinary plot parameters raise nothing", {
    # The regression risk of the test above: these checks now run in .run() for
    # every analysis that requests a plot, so a wrong comparison would put an
    # error banner on every normal run.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         sc = TRUE, ybegin_plot = 0.25, yend_plot = 1,
                         byplot = 2, endplot = 12)

    expect_equal(strip_html(res$errors$content), "")
})


# ------------------------------------------------------------- V2: risk table

# Every text label actually drawn on the device, so the assertion is about the
# rendered figure rather than about the option having been read.
drawn_text <- function(expr) {
    grDevices::png(tempfile(fileext = ".png"), width = 900, height = 700)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(expr)
    grid::grid.force()
    gl <- grid::grid.ls(grobs = TRUE, viewports = FALSE, print = FALSE)
    out <- character(0)
    for (nm in gl$name) {
        g <- try(grid::grid.get(nm, grep = TRUE, global = FALSE), silent = TRUE)
        if (!inherits(g, "try-error") && inherits(g, "text") && !is.null(g$label))
            out <- c(out, as.character(g$label))
    }
    out
}

render_km <- function(...) {
    args <- list(...)
    data <- args$data
    args$data <- NULL
    opts <- do.call(singlearmOptions$new, args)
    analysis <- singlearmClass$new(options = opts, data = data)
    analysis$run()
    drawn_text(analysis$.__enclos_env__$private$.plot(
        analysis$results$plot, ggplot2::theme_bw(), NULL))
}

test_that("risktable = TRUE actually draws the numbers-at-risk table", {
    # Was: finalfit::surv_plot() returns a ggsurvplot whose $plot is the curve
    # and whose $table is the risk table. Only $plot was kept and printed, so
    # the option did nothing at all -- silently, because the curve looked right.
    d <- simple_data(c(2, 5, 7, 9, 11, 14, 18, 22, 26, 30),
                     c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

    labels <- render_km(data = d, elapsedtime = "time", outcome = "status",
                        outcomeLevel = "Dead", sc = TRUE, risktable = TRUE,
                        endplot = 30, byplot = 6)

    expect_true(any(grepl("Number at risk", labels)))
})

test_that("risktable = FALSE still draws the curve alone", {
    # The regression risk of the test above.
    d <- simple_data(c(2, 5, 7, 9, 11, 14, 18, 22, 26, 30),
                     c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

    labels <- render_km(data = d, elapsedtime = "time", outcome = "status",
                        outcomeLevel = "Dead", sc = TRUE, risktable = FALSE,
                        endplot = 30, byplot = 6)

    expect_false(any(grepl("Number at risk", labels)))
    expect_true(any(grepl("Probability", labels)))
})


# ---------------------------------------------------------- V3: explanations

test_that("plot explanations do not depend on person-time metrics", {
    # Was: the explanation was generated at the bottom of .personTimeAnalysis(),
    # which returns immediately when person_time is FALSE. Selecting a plot plus
    # "Analysis explanations" produced a visible heading above an empty box.
    d <- ten_subjects()

    without <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                             outcomeLevel = "Dead", sc = TRUE,
                             showExplanations = TRUE, person_time = FALSE)
    with <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                          outcomeLevel = "Dead", sc = TRUE,
                          showExplanations = TRUE, person_time = TRUE)

    expect_match(without$survivalPlotsExplanation$content, "Kaplan-Meier")
    expect_equal(without$survivalPlotsExplanation$content,
                 with$survivalPlotsExplanation$content)
    # ... and the person-time explanation still follows person_time.
    expect_match(with$personTimeExplanation$content, "Person-Time")
})

test_that("no plot selected still means no plot explanation", {
    # The regression risk of the test above: decoupling from person_time must
    # not decouple it from the plots it explains.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         sc = FALSE, ce = FALSE, ch = FALSE, kmunicate = FALSE,
                         showExplanations = TRUE)

    expect_equal(paste(res$survivalPlotsExplanation$content, collapse = ""), "")
})


# --------------------------------------------------------------- V4: exports

# The Output options cannot be set through the public wrapper, so the export
# path is exercised on the class directly.
run_with_outputs <- function(data, ..., outputs = character(0)) {
    opts <- singlearmOptions$new(...)
    for (nm in outputs)
        opts$.__enclos_env__$private[[paste0("..", nm)]]$value <- list(value = TRUE)
    analysis <- singlearmClass$new(options = opts, data = data)
    analysis$run()
    analysis
}

output_values <- function(element)
    unlist(element$.__enclos_env__$private$.values, use.names = FALSE)

output_rows <- function(element)
    unlist(element$.__enclos_env__$private$.rowNums, use.names = FALSE)

test_that("the exported calculated time is the raw interval between the dates", {
    # Was: written from cleanData, i.e. AFTER the landmark subtraction and AFTER
    # complete-case exclusion. With landmark = 40 the column contained 10, 30, 50
    # for three of six rows, under a title and description promising "Calculated
    # Time from given Dates".
    dx <- as.Date("2020-01-01")
    days <- c(10, 20, 30, 50, 70, 90)
    d <- data.frame(dxd = rep(dx, 6), fud = dx + days,
                    status = factor(c("Dead", "Alive", "Dead", "Dead", "Alive", "Dead"),
                                    levels = c("Alive", "Dead")))

    analysis <- run_with_outputs(
        d, tint = TRUE, dxdate = "dxd", fudate = "fud", timetypedata = "ymd",
        timetypeoutput = "days", outcome = "status", outcomeLevel = "Dead",
        uselandmark = TRUE, landmark = 40, outputs = "calculatedtime")

    expect_equal(output_values(analysis$results$calculatedtime), days)
    expect_equal(as.integer(output_rows(analysis$results$calculatedtime)), 1:6)

    # Named regression of this change: with no landmark and no exclusions the
    # exported column must be exactly what it always was.
    plain <- run_with_outputs(
        d, tint = TRUE, dxdate = "dxd", fudate = "fud", timetypedata = "ymd",
        timetypeoutput = "days", outcome = "status", outcomeLevel = "Dead",
        outputs = "calculatedtime")
    expect_equal(output_values(plain$results$calculatedtime), days)
})

test_that("the exported redefined outcome covers every row of the source data", {
    # Was: restricted to complete, landmark-eligible rows, so the recoded
    # indicator could not be checked against the rows it was recoded from.
    d <- data.frame(
        time = c(5, 10, 15, 20, 25, NA),
        status = factor(c("DOD", "DOOC", "AWD", "AWOD", "DOD", "DOD"),
                        levels = c("AWOD", "AWD", "DOOC", "DOD")))

    analysis <- run_with_outputs(
        d, elapsedtime = "time", outcome = "status", outcomeLevel = "DOD",
        multievent = TRUE, analysistype = "overall",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
        outputs = "outcomeredefined")

    expect_equal(as.integer(output_rows(analysis$results$outcomeredefined)), 1:6)
    expect_equal(length(output_values(analysis$results$outcomeredefined)), 6)
})


# -------------------------------------------------- V5: data-quality grading

test_that("follow-up is graded on a robust summary, not on the single longest observation", {
    # Was: assess_followup(max_time). One subject followed for 70 months made a
    # series with roughly 10 months of median follow-up read "Long-term".
    d <- simple_data(c(2, 4, 6, 8, 10, 12, 14, 16, 18, 70),
                     c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead", advancedDiagnostics = TRUE)

    metrics <- col_of(res$dataQualityTable, "metric")
    grades <- col_of(res$dataQualityTable, "assessment")

    range_row <- which(grepl("Follow-up Range", metrics))
    median_row <- which(grepl("^Median (Follow-up|Observed)", metrics))
    expect_length(range_row, 1)
    expect_length(median_row, 1)

    expect_equal(grades[range_row], "not graded")
    expect_false(grades[median_row] == "Long-term")
})

test_that("the observed event proportion is reported but not graded", {
    # Was: "20% or more = Good", i.e. a cohort in which more patients had died
    # was better data. Event frequency is a property of the disease and of when
    # the data were censused, not of data quality.
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         advancedDiagnostics = TRUE)

    metrics <- col_of(res$dataQualityTable, "metric")
    grades <- col_of(res$dataQualityTable, "assessment")
    rate_row <- which(metrics == "Observed Event Proportion")

    expect_length(rate_row, 1)
    expect_equal(grades[rate_row], "not graded")
    # The number itself is still reported.
    expect_equal(col_of(res$dataQualityTable, "value")[rate_row], "50%")
})

test_that("descriptive diagnostics assign no arbitrary adequacy grades", {
    res <- run_singlearm(data = ten_subjects(), elapsedtime = "time",
                         outcome = "status", outcomeLevel = "Dead",
                         advancedDiagnostics = TRUE)

    grades <- col_of(res$dataQualityTable, "assessment")
    expect_true(length(grades) >= 6)
    expect_true(all(grades == "not graded"))
})

test_that("event scarcity is reported without an arbitrary warning threshold", {
    d <- simple_data(c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
                     c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead")

    warned <- strip_html(res$warnings$content)
    expect_false(grepl("Very few events|Limited events", warned))
    expect_false(grepl("Low observed event proportion", warned))
    expect_equal(res$medianTable$getCell(rowNo = 1, "events")$value, 3)
})

test_that("a low observed event proportion is not labelled inadequate", {
    set.seed(1)
    n <- 250
    d <- simple_data(rep(c(13, 15, 17, 19, 21), length.out = n),
                     rep(c(TRUE, rep(FALSE, 11)), length.out = n))

    res <- run_singlearm(data = d, elapsedtime = "time", outcome = "status",
                         outcomeLevel = "Dead")

    warned <- strip_html(res$warnings$content)
    expect_false(grepl("Low observed event proportion", warned))
    expect_false(grepl("Very few events", warned))
})
