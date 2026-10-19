# Regression tests for the swimmerplot findings of the 2026-10-18 OncoPath release-profile
# review (R44-R67), fixed by `/fix-function swimmerplot --apply` on 2026-10-19.
# Record: quality-reports/fix-swimmerplot-2026-10-19/README.md

sp <- function(...) suppressWarnings(ClinicoPath::swimmerplot(...))
sp_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))
sp_rate <- function(r, key) {
    m <- r$advancedMetrics$asDF
    m$metric_value[grepl(key, m$metric_name, fixed = TRUE)][1]
}
sp_fu <- function(r) {
    m <- r$advancedMetrics$asDF
    m[grepl("Median Follow-up", m$metric_name), c("metric_name", "metric_value")][1, ]
}

test_that("R46: a partly RECIST coding withholds ORR/DCR and names the labels", {
    d <- data.frame(id = sprintf("P%02d", 1:12), start = 0, end = 5:16,
                    resp = factor(c(rep("Response", 5), rep("Stable", 4), rep("Progression", 3))))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", showCopyReady = TRUE)
    expect_true(is.na(sp_rate(r, "Objective Response Rate")))
    expect_true(is.na(sp_rate(r, "Disease Control Rate")))
    nt <- sp_txt(r$notices$content)
    expect_match(nt, "not RECIST categories (CR, PR, SD, PD or NE): 'Response'", fixed = TRUE)
    cr <- sp_txt(r$copyReadyReport$content)
    expect_false(grepl("(ORR) of", cr, fixed = TRUE))
    expect_match(cr, "Not every response value was a RECIST category", fixed = TRUE)
})

test_that("R46: a non-RECIST row hidden behind a recognised best response still withholds", {
    d <- data.frame(id = c("A", "A", "B", "C"), start = c(0, 4, 0, 0), end = c(4, 8, 6, 7),
                    resp = c("Response", "PR", "SD", "PD"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
    expect_true(is.na(sp_rate(r, "Objective Response Rate")))
    expect_match(sp_txt(r$notices$content), "'Response'", fixed = TRUE)
})

test_that("R46: complete/partial remission are CR/PR; NE and missing stay non-responders", {
    d <- data.frame(id = sprintf("P%02d", 1:6), start = 0, end = 5:10,
                    resp = c("Complete remission", "Partial remission", "SD", "PD", "NE", NA))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
    ci <- stats::binom.test(2, 6)$conf.int * 100
    expect_equal(sp_rate(r, "Objective Response Rate"), round(2 / 6 * 100, 1))
    m <- r$advancedMetrics$asDF
    expect_equal(m$confidence_interval[grepl("Objective", m$metric_name)],
                 sprintf("%.1f - %.1f", ci[1], ci[2]))
})

test_that("R51: group tests do not post a second ORR counting notice", {
    d <- data.frame(id = sprintf("P%02d", 1:10), start = 0, end = 1:10,
                    resp = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD", "NE", NA),
                    grp = c(rep(c("A", "B"), 4), NA, NA))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", groupVar = "grp")
    nt <- sp_txt(r$notices$content)
    expect_equal(lengths(regmatches(nt, gregexpr("How ORR and DCR are counted", nt, fixed = TRUE))), 1)
    expect_match(nt, "computed over all 10 patients", fixed = TRUE)
})

test_that("R44: a best response derived from several rows is disclosed as unconfirmed", {
    d <- data.frame(id = c("A", "A", "B", "B", "C", "D"), start = c(0, 2, 0, 1, 0, 0),
                    end = c(2, 6, 1, 8, 12, 10), resp = c("PR", "PD", "SD", "PD", "CR", "PD"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", showCopyReady = TRUE)
    nt <- sp_txt(r$notices$content)
    expect_match(nt, "Unconfirmed best response", fixed = TRUE)
    expect_match(nt, "2 of 4 patients have more than one response row", fixed = TRUE)
    notes <- vapply(r$advancedMetrics$notes, function(n) n$note, "")
    expect_true(any(grepl("not a RECIST 1.1 best overall response", notes, fixed = TRUE)))
    expect_match(sp_txt(r$copyReadyReport$content), "responses were not confirmed", fixed = TRUE)

    # one row per patient: the category is the user's own, nothing derived
    r1 <- sp(data = d[c(1, 3, 5, 6), ], patientID = "id", startTime = "start",
             endTime = "end", responseVar = "resp")
    expect_false(grepl("Unconfirmed best response", sp_txt(r1$notices$content), fixed = TRUE))
})

test_that("R47/R50: tied start times give the same best response in any row order", {
    rates <- function(d) {
        r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
        c(sp_rate(r, "Objective Response Rate"), sp_rate(r, "Disease Control Rate"))
    }
    # same start, different ends: ordered by end, so PR [0,6] precedes PD [0,12]
    d1 <- data.frame(id = c("A", "A", "B"), start = c(0, 0, 0), end = c(6, 12, 8),
                     resp = c("PR", "PD", "SD"))
    expect_equal(rates(d1), rates(d1[c(2, 1, 3), ]))
    expect_equal(rates(d1), c(50, 100))
    # an exact tie: progression is never read as following the response
    d2 <- data.frame(id = c("A", "A", "B"), start = 0, end = c(6, 6, 8), resp = c("PR", "PD", "SD"))
    expect_equal(rates(d2), rates(d2[c(2, 1, 3), ]))
    expect_equal(rates(d2), c(0, 50))
    # the data.table path (more than 1000 rows)
    big <- rbind(d2, data.frame(id = sprintf("Q%04d", 1:1000), start = 0, end = 5, resp = "SD"))
    expect_equal(rates(big), rates(big[c(2, 1, 3:nrow(big)), ]))
})

cens8 <- data.frame(id = sprintf("P%02d", 1:8), start = 0,
                    end = c(10, 14, 9, 16, 12, 18, 20, 22),
                    Censored = c("Yes", "Yes", "No", "No", "Yes", "No", "No", "No"))

test_that("R45: a Yes/No censoring column is not read until its coding is chosen", {
    r <- sp(data = cens8, patientID = "id", startTime = "start", endTime = "end",
            censorVar = "Censored")
    expect_match(sp_txt(r$notices$content), "Choose the censoring coding", fixed = TRUE)
    expect_equal(NROW(r$plot$state$arrow_data), 0)
    fu <- sp_fu(r)
    expect_match(fu$metric_name, "censoring coding not chosen", fixed = TRUE)
    expect_equal(fu$metric_value, stats::median(cens8$end))
})

test_that("R45: either Yes/No polarity matches an independent reverse-KM oracle", {
    skip_if_not_installed("survival")
    yes <- cens8$Censored == "Yes"
    for (mode in c("yes_censored", "yes_event")) {
        censored <- if (mode == "yes_censored") yes else !yes
        r <- sp(data = cens8, patientID = "id", startTime = "start", endTime = "end",
                censorVar = "Censored", censorCoding = mode)
        km <- survival::survfit(survival::Surv(cens8$end, as.numeric(censored)) ~ 1)
        med <- unname(summary(km)$table[["median"]])
        if (is.na(med)) {   # reversed curve never reaches 50%: the observed median, labelled so
            expect_match(sp_fu(r)$metric_name, "reverse Kaplan-Meier not estimable", fixed = TRUE)
            expect_equal(sp_fu(r)$metric_value, stats::median(cens8$end))
        } else {
            expect_match(sp_fu(r)$metric_name, "(reverse Kaplan-Meier)", fixed = TRUE)
            expect_equal(sp_fu(r)$metric_value, round(med, 2))
        }
        expect_setequal(as.character(r$plot$state$arrow_data$patient_id), cens8$id[censored])
        expect_match(sp_txt(r$notices$content), "chosen Censoring coding", fixed = TRUE)
    }
    # a coding that does not fit the values is not forced onto them
    r <- sp(data = cens8, patientID = "id", startTime = "start", endTime = "end",
            censorVar = "Censored", censorCoding = "two_event")
    expect_match(sp_txt(r$notices$content), "does not match the data", fixed = TRUE)
    expect_equal(NROW(r$plot$state$arrow_data), 0)
})

test_that("R48/R57: arrows and follow-up share one coding of the whole column", {
    # rows hold 0/1/2; last episodes hold only 1/2. One reading for both.
    d <- data.frame(id = rep(sprintf("P%d", 1:6), each = 2), start = rep(c(0, 5), 6),
                    end = rep(c(5, 0), 6) + c(5, 10, 5, 14, 5, 9, 5, 16, 5, 12, 5, 18),
                    cs = c(0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "cs")
    expect_equal(NROW(r$plot$state$arrow_data), 0)
    expect_match(sp_fu(r)$metric_name, "censoring variable not readable", fixed = TRUE)
})

test_that("R50: a censoring tie at the last end resolves to the event in any row order", {
    d <- data.frame(id = c("A", "A", "B", "C"), start = c(0, 0, 0, 0), end = c(10, 10, 8, 12),
                    cs = c(0, 1, 0, 1))
    arrows <- function(x) sort(as.character(sp(data = x, patientID = "id", startTime = "start",
        endTime = "end", censorVar = "cs")$plot$state$arrow_data$patient_id))
    expect_equal(arrows(d), "B")
    expect_equal(arrows(d[c(2, 1, 3, 4), ]), "B")
})

test_that("R53: the observed-median fallback covers every patient", {
    d <- data.frame(id = sprintf("P%d", 1:6), start = 0, end = c(2, 4, 6, 8, 10, 12),
                    cs = c(1, 1, 1, 1, NA, NA))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "cs",
            censorCoding = "one_event_01")   # a single-valued column waits for a coding (SW-7)
    expect_equal(sp_fu(r)$metric_value, 7)
    m <- r$advancedMetrics$asDF
    interp <- m$clinical_interpretation[grepl("Median Follow-up", m$metric_name)]
    expect_false(grepl("Every patient was observed to the terminal outcome", interp, fixed = TRUE))
    expect_match(interp, "including those with no recorded status", fixed = TRUE)
})

test_that("R59: an all-missing censoring or group variable is explained", {
    d <- data.frame(id = sprintf("P%d", 1:6), start = 0, end = 3:8, cs = NA_real_,
                    grp = NA_character_, resp = c("CR", "PR", "SD", "PD", "SD", "PR"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            censorVar = "cs", groupVar = "grp", responseVar = "resp")
    expect_match(sp_txt(r$notices$content), "Censoring variable has no values", fixed = TRUE)
    notes <- vapply(r$groupComparisonTest$notes, function(n) n$note, "")
    expect_true(any(grepl("no patient has a value in the group variable", notes, fixed = TRUE)))
})

test_that("R61: episode-varying groups do not fail an analysis that does not use them", {
    d <- data.frame(id = rep(1:4, each = 2), start = rep(c(0, 2), 4), end = rep(c(1, 3), 4),
                    grp = rep(c("A", "B"), 4))
    expect_no_error(r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
                            groupVar = "grp"))
    expect_match(sp_txt(r$notices$content), "Grouping variable not used", fixed = TRUE)
})

test_that("R52: a milestone in a later episode is inside the patient's timeline", {
    d <- data.frame(id = c("A", "A", "B"), start = c(0, 10, 0), end = c(5, 20, 8),
                    ms = c(NA, 15, 4))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            milestone1Date = "ms")
    expect_false(grepl("Milestones outside the patient timeline", sp_txt(r$notices$content), fixed = TRUE))
})

test_that("R56/R63: unreadable milestone values are counted and named", {
    d <- data.frame(id = sprintf("P%d", 1:6), start = "2023-01-01",
                    end = c("2023-06-01", "2023-07-01", "2023-08-01", "2023-09-01", "2023-10-01", "2023-11-01"),
                    ms = c("2023-02-01", "unknown", "2023-03-01", "2023-02-30", "2023-04-01", "2023-05-01"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", timeType = "datetime",
            dateFormat = "ymd", milestone1Name = "Surgery", milestone1Date = "ms")
    expect_match(sp_txt(r$notices$content),
                 "Milestone 'Surgery': 2 of 6 recorded values could not be read with the selected Date Format (YYYY-MM-DD)",
                 fixed = TRUE)
    expect_equal(r$milestoneTable$asDF$n_events, 4)

    dr <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 10, ms = c("2", "ND", "12,5", "4"))
    rr <- sp(data = dr, patientID = "id", startTime = "start", endTime = "end",
             milestone1Name = "Scan", milestone1Date = "ms")
    expect_match(sp_txt(rr$notices$content), "Milestone 'Scan': 2 of 4 recorded values are not numbers", fixed = TRUE)
})

test_that("R65: an unnamed milestone slot takes its variable's name by default", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 10, Progression = c(3, 4, 5, 6))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            milestone1Date = "Progression")
    expect_equal(as.character(r$milestoneTable$asDF$milestone_name), "Progression")
})

test_that("R58: a sparse event column does not report rows without events as hidden markers", {
    d <- data.frame(id = sprintf("P%02d", 1:10), start = 0, end = 10,
                    ev = c("Toxicity", NA, NA, "Toxicity", NA, NA, NA, NA, NA, NA),
                    evt = c(3, NA, NA, 5, NA, NA, NA, NA, NA, NA))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            showEventMarkers = TRUE, eventVar = "ev", eventTimeVar = "evt")
    expect_false(grepl("event markers are not shown", sp_txt(r$notices$content), fixed = TRUE))
    d$evt[4] <- NA
    r2 <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
             showEventMarkers = TRUE, eventVar = "ev", eventTimeVar = "evt")
    expect_match(sp_txt(r2$notices$content), "1 of 2 event markers are not shown. 1 have no event time.",
                 fixed = TRUE)
})

test_that("R62: implausible follow-up is a WARNING, not an information note", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = c(400, 500, 600, 700))
    nt <- sp_txt(sp(data = d, patientID = "id", startTime = "start", endTime = "end")$notices$content)
    expect_match(nt, "WARNING: Implausible follow-up Total follow-up exceeds 10 years", fixed = TRUE)
})

test_that("R64: the export panel describes the tables it shows", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 3:6)
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            exportTimeline = TRUE, exportSummary = TRUE)
    tx <- sp_txt(r$exportInfo$content)
    expect_false(grepl("Output panel", tx, fixed = TRUE))
    expect_match(tx, "first 500 patients", fixed = TRUE)
    expect_match(tx, "right-click a table and choose Copy", fixed = TRUE)
})

test_that("R66: compact YYYYMMDD dates are read as dates, and refused as raw numbers", {
    d <- data.frame(id = sprintf("P%d", 1:4), s = c(20230115, 20230201, 20230301, 20231201),
                    e = c(20230615, 20230801, 20231001, 20240110))
    r <- sp(data = d, patientID = "id", startTime = "s", endTime = "e",
            timeType = "datetime", dateFormat = "ymd", timeUnit = "months")
    expect_true(r$summary$asDF$value[3] < 10)          # median follow-up in months, not 8909
    expect_error(ClinicoPath::swimmerplot(data = d, patientID = "id", startTime = "s", endTime = "e",
                                          timeType = "raw"))
})

test_that("R67, R49, R44: option and panel wording states what the analysis does", {
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "swimmerplot.a.yaml"))
    opt <- function(n) Filter(function(o) identical(o$name, n), a$options)[[1]]
    expect_identical(opt("timeUnit")$title, "Time unit")
    expect_match(opt("timeUnit")$description$R, "not rescaled", fixed = TRUE)
    for (i in 1:5) expect_identical(opt(paste0("milestone", i, "Name"))$default, "")
    expect_identical(opt("censorCoding")$default, "auto")

    r <- sp(data = data.frame(id = "P1", start = 0, end = 1), patientID = "id",
            startTime = "start", endTime = "end", showAbout = TRUE, showGlossary = TRUE)
    about <- sp_txt(r$aboutAnalysis$content)
    expect_false(grepl("excluded from calculations", about, fixed = TRUE))
    expect_match(about, "A missing response counts as a non-responder", fixed = TRUE)
    expect_match(sp_txt(r$clinicalGlossary$content), "unconfirmed best response", fixed = TRUE)
})

test_that("asSource quotes free-text options so the syntax parses", {
    d <- data.frame(id = "P1", start = 0, end = 1, ms = 0.5)
    o <- ClinicoPath:::swimmerplotOptions$new(patientID = "id", startTime = "start", endTime = "end",
                                              milestone1Name = 'Death/Last "FU"', milestone1Date = "ms",
                                              censorCoding = "yes_event")
    a <- ClinicoPath:::swimmerplotClass$new(options = o, data = d)
    src <- a$asSource()
    expect_no_error(parse(text = src))
    expect_match(src, 'milestone1Name = "Death/Last \\"FU\\""', fixed = TRUE)
})

# --- adversarial review round (2026-10-19) ---------------------------------------------

test_that("review: a coding that does not fit, and values no coding reads, are labelled as such", {
    r <- sp(data = cens8, patientID = "id", startTime = "start", endTime = "end",
            censorVar = "Censored", censorCoding = "two_event", showCopyReady = TRUE)
    expect_match(sp_fu(r)$metric_name, "censoring variable not readable with this coding", fixed = TRUE)
    expect_false(grepl("not chosen", sp_txt(r$copyReadyReport$content), fixed = TRUE))
    d <- data.frame(id = sprintf("P%d", 1:6), start = 0, end = 3:8, cs = c(0, 1, 2, 0, 1, 2))
    nt <- sp_txt(sp(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "cs")$notices$content)
    expect_match(nt, "Censoring variable not readable", fixed = TRUE)
    expect_false(grepl("Choose the censoring coding", nt, fixed = TRUE))
})

test_that("review: withheld rates are not described as rates anywhere", {
    rp <- data.frame(id = sprintf("P%02d", 1:12), start = 0, end = 5:16,
                     resp = c(rep("Response", 5), rep("Stable", 4), rep("Progression", 3)),
                     grp = c(rep(c("A", "B"), 5), NA, NA))
    r <- sp(data = rp, patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", groupVar = "grp")
    notes <- vapply(r$summary$notes, function(n) n$note, "")
    expect_false(any(grepl("same denominator as the Objective", notes, fixed = TRUE)))
    expect_false(grepl("response rates reported above", sp_txt(r$notices$content), fixed = TRUE))

    ne <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 3:6, resp = "NE")
    cr <- sp_txt(sp(data = ne, patientID = "id", startTime = "start", endTime = "end",
                    responseVar = "resp", showCopyReady = TRUE)$copyReadyReport$content)
    expect_match(cr, "No patient had a CR, PR, SD or PD response", fixed = TRUE)
})

test_that("review: blank group and milestone cells are missing values", {
    d <- data.frame(id = rep(sprintf("P%d", 1:4), each = 2), start = rep(c(0, 5), 4), end = rep(c(5, 10), 4),
                    resp = c("PR", "SD", "SD", "SD", "PR", "SD", "PD", "PD"), grp = c("A", "", "B", "", "A", "", "B", ""),
                    ms = c("", "7", "3", "", "4", "", "", "8"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end",
            responseVar = "resp", groupVar = "grp", milestone1Date = "ms")
    # the run completes (the group sits on the first row only) and no unnamed group appears
    gnotes <- vapply(r$groupComparisonTest$notes, function(n) n$note, "")
    expect_true(nrow(r$groupComparisonTest$asDF) > 0)
    expect_false(any(grepl("(- |; ): [0-9]+ of", gnotes)))
    expect_equal(r$milestoneTable$asDF$n_events, 4)
})

test_that("review: the person-time note does not call the strata a best overall response", {
    d <- data.frame(id = c("A", "A", "B", "B"), start = c(0, 2, 0, 1), end = c(2, 6, 1, 8),
                    resp = c("PR", "PD", "SD", "PD"))
    r <- sp(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
    notes <- vapply(r$personTimeTable$notes, function(n) n$note, "")
    expect_false(any(grepl("BEST overall response", notes, fixed = TRUE)))
})
