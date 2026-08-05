# Regression tests from the swimmerplot release review.
#
# Checked against survival::survfit and hand-computed quantiles rather than
# against the module's own arithmetic.

test_that("the summary table uses one estimator throughout", {
  # .calculateSummaryStats() reported a reverse Kaplan-Meier median beside a naive
  # mean and naive quartiles. Under censoring the KM estimate is systematically
  # larger, so the reported median could fall OUTSIDE its own reported IQR, and
  # median >> mean read as strong skew that was purely an artefact of mixing
  # estimators. Verified before the fix: median 23.0 with Q1 4.75, Q3 22.5.
  d <- data.frame(
    id    = paste0("P", 1:12),
    start = 0,
    end   = c(2, 3, 4, 5, 6, 7,  18, 20, 22, 24, 26, 28),
    cens  = c(rep(1, 6), rep(0, 6)),      # the six longest-followed are ongoing
    stringsAsFactors = FALSE)

  r <- suppressWarnings(swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    censorVar = "cens", personTimeAnalysis = TRUE))

  s <- as.data.frame(r$summary$asDF)
  med  <- as.numeric(s$value[grepl("Median Duration", s$metric)][1])
  mean <- as.numeric(s$value[grepl("Mean Duration", s$metric)][1])

  fu <- d$end
  expect_equal(med, stats::median(fu))       # observed median, not the KM estimate
  expect_equal(mean, base::mean(fu))
  # the median must lie inside the quartiles the same table's IQR is built from
  expect_gte(med, unname(stats::quantile(fu, 0.25)))
  expect_lte(med, unname(stats::quantile(fu, 0.75)))
})

test_that("median follow-up is reverse Kaplan-Meier and labelled as such", {
  skip_if_not_installed("survival")
  d <- data.frame(
    id = paste0("P", 1:12), start = 0,
    end = c(2, 3, 4, 5, 6, 7, 18, 20, 22, 24, 26, 28),
    cens = c(rep(1, 6), rep(0, 6)), stringsAsFactors = FALSE)

  r <- suppressWarnings(swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    censorVar = "cens", personTimeAnalysis = TRUE))
  am <- as.data.frame(r$advancedMetrics$asDF)

  row <- am[grepl("Median Follow-up", am$metric_name), ]
  expect_equal(nrow(row), 1)
  # the label must name the estimator, so it is not read as a plain duration
  expect_match(row$metric_name[1], "reverse Kaplan-Meier", fixed = TRUE)

  km <- survival::survfit(survival::Surv(d$end, 1 - d$cens) ~ 1)
  expect_equal(as.numeric(row$metric_value[1]),
               unname(summary(km)$table["median"]), tolerance = 1e-6)
})

test_that("the interquartile range says which durations it describes", {
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(2,4,6,8,10,12,14,16),
                  stringsAsFactors = FALSE)
  r <- suppressWarnings(swimmerplot(data = d, patientID = "id", startTime = "start",
                                    endTime = "end", personTimeAnalysis = TRUE))
  am <- as.data.frame(r$advancedMetrics$asDF)
  row <- am[grepl("Interquartile", am$metric_name), ]

  expect_match(row$metric_name[1], "observed", fixed = TRUE)
  q <- stats::quantile(d$end, c(0.25, 0.75))
  expect_equal(as.numeric(row$metric_value[1]), unname(q[2] - q[1]), tolerance = 1e-6)
})

test_that("renaming a milestone refreshes the plot", {
  # The plot draws its milestone legend labels from milestone*Name, but only
  # milestone*Date appeared in the plot's clearWith, so a rename left the figure
  # stale while the milestone table beside it updated.
  rl <- yaml::read_yaml("../../jamovi/swimmerplot.r.yaml")
  plot_item <- Filter(function(i) identical(i$name, "plot"), rl$items)[[1]]
  cw <- unlist(plot_item$clearWith)

  for (i in 1:5) {
    expect_true(paste0("milestone", i, "Date") %in% cw, info = i)
    expect_true(paste0("milestone", i, "Name") %in% cw, info = i)
  }
})

# ═══════════════════════════════════════════════════════════
# Regression: milestones must join to patients by ID, not row position
# ═══════════════════════════════════════════════════════════
#
# .validateAndProcessData() drops rows (missing ID/start/end, end < start) but
# .processMilestones() reads the milestone column from the UNFILTERED self$data.
# Pairing the two by position shifted every patient after a dropped row onto
# somebody else's lane and silently discarded the last patient's milestone.

test_that("milestone times stay with their own patient when a row is dropped", {
    # P2 fails validation (end_time is NA) and is removed.
    df <- data.frame(
        id    = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end   = c(10, NA, 30, 40),
        ms1   = c(1, 2, 3, 4),
        stringsAsFactors = FALSE
    )

    res <- ClinicoPath::swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        milestone1Date = "ms1", milestone1Name = "M1",
        timeType = "raw", timeDisplay = "absolute"
    )

    tab <- res$milestoneTable$asDF

    # Surviving patients are P1, P3, P4 -> their own milestones 1, 3, 4.
    # Positional pairing instead produced 1, 2, 3 (median 2, range 1-3).
    expect_equal(tab$n_events[1], 3)
    expect_equal(tab$median_time[1], 3)
    expect_match(tab$time_range[1], "^1 - 4")
})

test_that("milestone alignment is unaffected when no rows are dropped", {
    df <- data.frame(
        id    = c("P1", "P2", "P3", "P4"),
        start = c(0, 0, 0, 0),
        end   = c(10, 20, 30, 40),
        ms1   = c(1, 2, 3, 4),
        stringsAsFactors = FALSE
    )

    res <- ClinicoPath::swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        milestone1Date = "ms1", milestone1Name = "M1",
        timeType = "raw", timeDisplay = "absolute"
    )

    tab <- res$milestoneTable$asDF
    expect_equal(tab$n_events[1], 4)
    expect_equal(tab$median_time[1], 2.5)
})

# ═══════════════════════════════════════════════════════════
# Regression: plot aesthetics
# ═══════════════════════════════════════════════════════════

test_that("event marker glyphs are never empty strings", {
    # Every entry of the clinical glyph table was "" - the emoji were deleted
    # rather than escaped during a non-ASCII sweep, so ggswim drew nothing and
    # event markers were invisible for every labelled event.
    glyph_fn <- ClinicoPath:::swimmerplotClass$private_methods$.getEnhancedClinicalGlyphs

    labels <- c("Surgery", "irAE", "Death", "Scan", "Progression",
                "Complete Response", "Some Unmapped Label")
    glyphs <- glyph_fn(labels)

    expect_length(glyphs, length(labels))
    expect_true(all(nchar(glyphs) > 0))
    expect_false(any(is.na(glyphs)))
})

test_that("adding a milestone does not collapse response lane colours", {
    # Milestones used to map to `color` with their own scale_color_manual().
    # ggplot permits one colour scale, so it replaced the lane scale and every
    # response category fell through to NA grey.
    df <- data.frame(
        id = paste0("P", 1:8), start = rep(0, 8),
        end = c(5, 9, 12, 15, 18, 21, 24, 30),
        resp = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD"),
        ms1 = 2:9, stringsAsFactors = FALSE
    )

    lane_colours <- function(...) {
        opts <- ClinicoPath:::swimmerplotOptions$new(
            patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", timeType = "raw", timeDisplay = "absolute", ...
        )
        an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = df)
        an$run()
        tmp <- tempfile(fileext = ".png")
        grDevices::png(tmp); on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
        an$.__enclos_env__$private$.plot(an$results$plot,
            ggtheme = ggplot2::theme_bw(), theme = NULL)
        unique(ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]$colour)
    }

    # four response categories -> four lane colours, with or without milestones
    expect_length(lane_colours(), 4)
    expect_length(lane_colours(milestone1Date = "ms1", milestone1Name = "Surgery"), 4)
})

test_that("person-time is reported in the selected time unit, not epoch units", {
    # .mergeIntervalsAndSum() returned raw epoch units (seconds for POSIXct)
    # while .calculateFollowUp() returned the selected unit; both fed the same
    # person_time column, which every table labels with timeUnit.
    df <- data.frame(
        id = c("P1", "P2", "P3"),
        start = rep("2020-01-01", 3), end = rep("2020-12-31", 3),
        resp = c("CR", "PR", "SD"), stringsAsFactors = FALSE
    )

    res <- ClinicoPath::swimmerplot(
        data = df, patientID = "id", startTime = "start", endTime = "end",
        responseVar = "resp", timeType = "datetime", dateFormat = "ymd",
        timeUnit = "months", personTimeAnalysis = TRUE
    )

    pt <- res$personTimeTable$asDF
    # 365 days is ~11.99 months; must not be 365 (days) or 31,536,000 (seconds)
    expect_true(all(pt$total_time > 11 & pt$total_time < 13))
})

# ═══════════════════════════════════════════════════════════
# Regression: response-rate validity and relative time display
# ═══════════════════════════════════════════════════════════

test_that("ORR/DCR are omitted, not reported as 0%, for non-RECIST codes", {
    mk <- function(resp) data.frame(
        id = paste0("P", 1:6), start = rep(0, 6), end = c(5, 9, 12, 15, 18, 21),
        resp = resp, stringsAsFactors = FALSE)

    rates <- function(df) {
        res <- ClinicoPath::swimmerplot(data = df, patientID = "id",
            startTime = "start", endTime = "end", responseVar = "resp",
            timeType = "raw", timeDisplay = "absolute")
        m <- res$advancedMetrics$asDF
        m$metric_value[grepl("ORR|DCR", m$metric_name)]
    }

    # RECIST-coded: 4/6 CR+PR = 66.7%, 5/6 CR+PR+SD = 83.3%
    expect_equal(rates(mk(c("CR", "PR", "PR", "SD", "PD", "CR"))),
                 c(66.7, 83.3), tolerance = 0.05)

    # Not RECIST-coded: every patient responded, but nothing maps to CR/PR/SD/PD.
    # Reporting 0% here would call a fully responding cohort a total failure.
    expect_true(all(is.na(rates(mk(rep("Responder", 6))))))
})

test_that("relative time display shifts lanes, milestones and events together", {
    df <- data.frame(
        id = c("P1", "P2", "P3"), start = c(10, 20, 30), end = c(40, 60, 80),
        ms1 = c(15, 25, 35), stringsAsFactors = FALSE)

    built <- function(mode) {
        opts <- ClinicoPath:::swimmerplotOptions$new(
            patientID = "id", startTime = "start", endTime = "end",
            milestone1Date = "ms1", milestone1Name = "M",
            timeType = "raw", timeDisplay = mode)
        an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = df)
        an$run()
        tmp <- tempfile(fileext = ".png")
        grDevices::png(tmp); on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
        an$.__enclos_env__$private$.plot(an$results$plot,
            ggtheme = ggplot2::theme_bw(), theme = NULL)
        ggplot2::ggplot_build(ggplot2::last_plot())$data
    }

    abs_b <- built("absolute")
    expect_equal(sort(abs_b[[1]]$x), c(10, 20, 30))
    expect_equal(sort(abs_b[[2]]$x), c(15, 25, 35))

    # Raw-numeric lanes used to ignore "relative" entirely while milestones were
    # shifted, putting a milestone at t=5 against a lane running 10..40.
    rel_b <- built("relative")
    expect_equal(unique(rel_b[[1]]$x), 0)
    expect_equal(sort(rel_b[[1]]$xend), c(30, 40, 50))
    expect_equal(unique(rel_b[[2]]$x), 5)   # each milestone is 5 after its own start
})

test_that("export options produce populated tables", {
    df <- data.frame(
        id = paste0("P", 1:4), start = rep(0, 4), end = c(5, 9, 12, 15),
        resp = c("CR", "PR", "SD", "PD"), stringsAsFactors = FALSE)

    # timelineData/summaryData were `type: Output` in the .r.yaml with no matching
    # Output option in the .a.yaml, written via setState() - so they never wrote
    # anything. They are Tables now; this needs a regenerated header to run, so
    # probe the compiled result type BEFORE exercising the export path.
    probe <- ClinicoPath::swimmerplot(data = df, patientID = "id",
        startTime = "start", endTime = "end", timeType = "raw",
        timeDisplay = "absolute")
    skip_if_not(inherits(probe$timelineData, "Table"),
                "requires jmvtools::prepare() after the .r.yaml Output->Table change")

    res <- ClinicoPath::swimmerplot(data = df, patientID = "id",
        startTime = "start", endTime = "end", responseVar = "resp",
        timeType = "raw", timeDisplay = "absolute",
        exportTimeline = TRUE, exportSummary = TRUE)

    expect_equal(nrow(res$timelineData$asDF), 4)
    expect_true(nrow(res$summaryData$asDF) >= 6)
})

test_that("excluded rows are disclosed, with counts and reasons", {
    # .validateClinicalData() runs on the ALREADY-filtered frame, so its
    # "these will be excluded" warnings could never fire for these rows and
    # patients vanished from the figure and every denominator in silence.
    df <- data.frame(
        id    = c("P1", "P2", "P3", "P4", NA),
        start = c(0, 0, 0, 99, 0),
        end   = c(10, NA, 30, 5, 20),
        stringsAsFactors = FALSE
    )

    res <- ClinicoPath::swimmerplot(data = df, patientID = "id",
        startTime = "start", endTime = "end",
        timeType = "raw", timeDisplay = "absolute")

    txt <- res$notices$content
    expect_match(txt, "Rows excluded from analysis")
    expect_match(txt, "3 of 5 rows were excluded")
    expect_match(txt, "missing patient ID")
    expect_match(txt, "missing start or end time")
    expect_match(txt, "end time precedes the start time")
})

# ═══════════════════════════════════════════════════════════
# Regression: MULTI-ROW-PER-PATIENT correctness
#
# Every fix in the first pass above was verified against one-row-per-patient
# data. A swimmer plot is multi-row per patient by construction, and three of
# those fixes were wrong or incomplete there. These tests pin that shape down.
# ═══════════════════════════════════════════════════════════

test_that("relative display anchors each PATIENT at 0, not each row", {
    # P1 spans 0-20, P2 spans 0-15 => person-time 35, mean/median follow-up 17.5.
    # Rebasing row-wise stacked every episode back onto 0, collapsing follow-up
    # to the longest single episode (-43% here) in the DEFAULT configuration.
    d <- data.frame(id = c("P1", "P1", "P2", "P2"),
                    st = c(0, 10, 0, 5), en = c(10, 20, 5, 15))

    grab <- function(mode) {
        o <- ClinicoPath:::swimmerplotOptions$new(patientID = "id", startTime = "st",
             endTime = "en", timeType = "raw", timeUnit = "months",
             timeDisplay = mode, personTimeAnalysis = TRUE)
        a <- ClinicoPath:::swimmerplotClass$new(options = o, data = d); a$run()
        df <- a$results$summary$asDF
        stats::setNames(df[[2]], df[[1]])
    }

    for (mode in c("absolute", "relative")) {
        v <- grab(mode)
        expect_equal(unname(v[["Total Person-Time"]]), 35, tolerance = 1e-8,
                     info = mode)
        expect_equal(unname(v[["Mean Follow-up"]]), 17.5, tolerance = 1e-8,
                     info = mode)
        expect_equal(unname(v[["Median Duration (observed)"]]), 17.5,
                     tolerance = 1e-8, info = mode)
    }
})

test_that("a patient with two episodes contributes ONE milestone, not two", {
    # match() on the per-episode frame handed episode-1's milestone to both rows:
    # the summary double-counted it, and the copy attached to episode 2 was
    # re-based on that episode's start and came out negative.
    df <- data.frame(
        id   = c("P1", "P1", "P2", "P2", "P3"),
        st   = c(0, 10, 0, 5, 0),
        en   = c(8, 20, 4, 12, 30),
        ms   = c(3, NA, 2, NA, 15),
        resp = c("CR", "CR", "PR", "PR", "SD"), stringsAsFactors = FALSE)

    for (mode in c("relative", "absolute")) {
        res <- ClinicoPath::swimmerplot(data = df, patientID = "id",
            startTime = "st", endTime = "en", milestone1Name = "Surgery",
            milestone1Date = "ms", timeType = "raw", timeDisplay = mode,
            responseVar = "resp")
        tab <- res$milestoneTable$asDF
        expect_equal(tab$n_events[1], 3, info = mode)      # P1@3, P2@2, P3@15
        expect_equal(tab$median_time[1], 3, info = mode)
        expect_match(tab$time_range[1], "^2 - 15", info = mode)
    }
})

test_that("copy-ready manuscript text never fabricates ORR for non-RECIST codes", {
    mk <- function(resp) data.frame(id = paste0("P", 1:6), st = rep(0, 6),
        en = c(5, 9, 12, 15, 18, 21), resp = resp, stringsAsFactors = FALSE)

    txt <- function(df) {
        res <- ClinicoPath::swimmerplot(data = df, patientID = "id",
            startTime = "st", endTime = "en", responseVar = "resp",
            timeType = "raw", timeDisplay = "absolute", showCopyReady = TRUE)
        gsub("<[^>]+>", "", res$copyReadyReport$content)
    }

    expect_match(txt(mk(c("CR", "PR", "PR", "SD", "PD", "CR"))),
                 "objective response rate \\(ORR\\) of 66\\.7%")

    # Every patient responded; nothing maps to CR/PR/SD/PD. The metrics table
    # refuses to claim 0% - the manuscript text must refuse too.
    bad <- txt(mk(rep("Responder", 6)))
    expect_match(bad, "not RECIST-coded")
    expect_false(grepl("objective response rate \\(ORR\\) of 0\\.0%", bad))
})

test_that("the follow-up row names the estimator actually used", {
    base <- data.frame(id = paste0("P", 1:8), st = rep(0, 8),
                       en = c(4, 6, 8, 10, 12, 18, 24, 30), stringsAsFactors = FALSE)

    row_for <- function(cens) {
        df <- base; args <- list(data = df, patientID = "id", startTime = "st",
             endTime = "en", timeType = "raw", timeDisplay = "absolute")
        if (!is.null(cens)) { df$cs <- cens; args$data <- df; args$censorVar <- "cs" }
        m <- do.call(ClinicoPath::swimmerplot, args)$advancedMetrics$asDF
        m[grepl("Median Follow-up", m$metric_name), c("metric_name", "metric_value")]
    }

    # reverse-KM value cross-checked against survival::survfit
    fu <- c(4, 6, 8, 10, 12, 18, 24, 30); ev <- c(0, 0, 0, 1, 1, 1, 0, 0)
    km <- unname(stats::quantile(
        survival::survfit(survival::Surv(fu, 1 - ev) ~ 1), 0.5)$quantile)

    r01 <- row_for(ev)
    expect_match(r01$metric_name[1], "reverse Kaplan-Meier")
    expect_equal(r01$metric_value[1], km)

    # Yes/No is a legitimate coding and must reach the same estimate
    ryn <- row_for(c("No", "No", "No", "Yes", "Yes", "Yes", "No", "No"))
    expect_match(ryn$metric_name[1], "reverse Kaplan-Meier")
    expect_equal(ryn$metric_value[1], km)

    # no censoring, and unclassifiable censoring, must NOT claim reverse-KM
    expect_match(row_for(NULL)$metric_name[1], "no censoring information")
    expect_match(row_for(c("A","B","A","B","A","B","A","B"))$metric_name[1],
                 "censoring not recognised")
})

test_that("person-time table groups by the normalised response label", {
    df <- data.frame(id = paste0("P", 1:6), st = rep(0, 6),
        en = c(5, 9, 12, 15, 18, 21),
        resp = c("CR", "Complete Response", "complete response",
                 "PR", "partial response", "SD"), stringsAsFactors = FALSE)

    res <- ClinicoPath::swimmerplot(data = df, patientID = "id", startTime = "st",
        endTime = "en", responseVar = "resp", timeType = "raw",
        timeDisplay = "absolute", personTimeAnalysis = TRUE)

    pt <- res$personTimeTable$asDF
    expect_equal(nrow(pt), 3)
    expect_equal(sort(pt$n_patients), c(1, 2, 3))
})

test_that("a custom reference line renders at the default time unit", {
    # lubridate does not export months(); `months` is a base generic, so
    # lubridate::months() threw - and months is the DEFAULT timeUnit.
    expect_false("months" %in% getNamespaceExports("lubridate"))

    df <- data.frame(id = paste0("P", 1:4), st = rep("2020-01-01", 4),
        en = c("2020-06-01", "2020-09-01", "2021-01-01", "2021-06-01"),
        stringsAsFactors = FALSE)

    for (rt in c(12, 12.5)) {   # 12.5 also crashed: Periods reject fractions
        o <- ClinicoPath:::swimmerplotOptions$new(patientID = "id", startTime = "st",
             endTime = "en", timeType = "datetime", dateFormat = "ymd",
             timeUnit = "months", timeDisplay = "absolute",
             referenceLines = "custom", customReferenceTime = rt)
        a <- ClinicoPath:::swimmerplotClass$new(options = o, data = df); a$run()
        tmp <- tempfile(fileext = ".png")
        grDevices::png(tmp)
        ok <- tryCatch({
            suppressWarnings(a$.__enclos_env__$private$.plot(a$results$plot,
                ggtheme = ggplot2::theme_bw(), theme = NULL)); TRUE
        }, error = function(e) conditionMessage(e))
        grDevices::dev.off(); unlink(tmp)
        expect_true(isTRUE(ok), info = paste("customReferenceTime =", rt))
    }
})

test_that("no ongoing-treatment arrow is invented without a censoring variable", {
    # The fallback flagged whoever had the largest end time as still on
    # treatment. That patient is very often the one who died last.
    df <- data.frame(id = paste0("P", 1:5), st = rep(0, 5),
        en = c(5, 9, 12, 15, 30), cs = c(0, 1, 0, 1, 1), stringsAsFactors = FALSE)

    no_cens <- ClinicoPath::swimmerplot(data = df, patientID = "id",
        startTime = "st", endTime = "en", timeType = "raw", timeDisplay = "absolute")
    expect_match(no_cens$notices$content, "Ongoing-treatment arrows not drawn")

    with_cens <- ClinicoPath::swimmerplot(data = df, patientID = "id",
        startTime = "st", endTime = "en", censorVar = "cs",
        timeType = "raw", timeDisplay = "absolute")
    expect_false(grepl("Ongoing-treatment arrows not drawn", with_cens$notices$content))
})

test_that("datetime multi-episode data is invariant to timeDisplay", {
    # timeDisplay is a DISPLAY option: no reported statistic may depend on it.
    # P1: 2020-01-01..2020-07-01 and 2021-01-01..2021-07-01 -> span 18 months,
    #     12 months of person-time.  P2: 2020-01-01..2020-04-01 -> 3 and 3.
    d <- data.frame(
        id = c("P1", "P1", "P2"),
        st = c("2020-01-01", "2021-01-01", "2020-01-01"),
        en = c("2020-07-01", "2021-07-01", "2020-04-01"),
        ms = c("2020-03-01", NA, "2020-02-01"), stringsAsFactors = FALSE)

    grab <- function(mode) {
        o <- ClinicoPath:::swimmerplotOptions$new(patientID = "id", startTime = "st",
             endTime = "en", timeType = "datetime", dateFormat = "ymd",
             timeUnit = "months", timeDisplay = mode, milestone1Date = "ms",
             milestone1Name = "M", personTimeAnalysis = TRUE)
        a <- ClinicoPath:::swimmerplotClass$new(options = o, data = d); a$run()
        v <- stats::setNames(a$results$summary$asDF[[2]], a$results$summary$asDF[[1]])
        list(fu = unname(v[["Mean Follow-up"]]),
             pt = unname(v[["Total Person-Time"]]),
             ms = a$results$milestoneTable$asDF)
    }

    abs_r <- grab("absolute"); rel_r <- grab("relative")

    expect_equal(abs_r$fu, 10.5, tolerance = 1e-6)
    expect_equal(rel_r$fu, 10.5, tolerance = 1e-6)

    # 14.92 vs 15.00: the merge converted summed epoch seconds with a fixed
    # 30.4375-day month while .calculateFollowUp used calendar months.
    expect_equal(abs_r$pt, 15, tolerance = 1e-6)
    expect_equal(rel_r$pt, abs_r$pt, tolerance = 1e-6)

    # one milestone per patient, none negative
    expect_equal(rel_r$ms$n_events[1], 2)
    expect_match(rel_r$ms$time_range[1], "^1 - 2")
})

# ═══════════════════════════════════════════════════════════
# Regression: the three fixes that were verified by hand but never pinned
# ═══════════════════════════════════════════════════════════

test_that("one estimator per name across table, interpretation and manuscript", {
    df <- data.frame(id = paste0("P", 1:8), st = rep(0, 8),
        en = c(4, 6, 8, 10, 12, 18, 24, 30), cs = c(0, 0, 0, 1, 1, 1, 0, 0),
        stringsAsFactors = FALSE)

    res <- ClinicoPath::swimmerplot(data = df, patientID = "id", startTime = "st",
        endTime = "en", censorVar = "cs", timeType = "raw", timeDisplay = "absolute",
        showCopyReady = TRUE, showInterpretation = TRUE)

    m <- res$advancedMetrics$asDF
    tbl <- m$metric_value[grepl("Median Follow-up", m$metric_name)]
    strip <- function(x) gsub("<[^>]+>", "", x)

    # The table's reverse-KM value and the manuscript's must be the same number
    # under the same name; they used to read 15.5 and 10.5.
    expect_equal(tbl, 24)
    expect_match(strip(res$copyReadyReport$content),
                 "median follow-up \\(reverse Kaplan-Meier\\) of 24\\.0")

    # The interpretation prints the naive median beside the observed range, so it
    # must be named for what it is rather than "median follow-up".
    int <- strip(res$interpretation$content)
    expect_match(int, "Median observed duration")
    expect_false(grepl("Median follow-up was", int))
})

test_that("the fallback plot reports the error that actually occurred", {
    # .createFallbackPlot(patient_data, milestone_data, event_data, opts, stats,
    # error_message): a positional second argument put the real error into
    # milestone_data, where it was discarded, and the subtitle always blamed ggswim.
    df <- data.frame(id = paste0("P", 1:4), st = rep(0, 4), en = c(4, 6, 8, 10),
                     stringsAsFactors = FALSE)
    a <- ClinicoPath:::swimmerplotClass$new(
        options = ClinicoPath:::swimmerplotOptions$new(patientID = "id",
                    startTime = "st", endTime = "en", timeType = "raw"),
        data = df)
    a$run()

    pd <- data.frame(patient_id = df$id, start_time = df$st, end_time = df$en)
    p <- a$.__enclos_env__$private$.createFallbackPlot(pd, error_message = "BOOM-XYZ")

    expect_match(p$labels$subtitle, "BOOM-XYZ")
})
