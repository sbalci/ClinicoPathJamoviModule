# Regression tests for the 2026-09-20 /check-function swimmerplot pass.

sw_strip <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))

test_that("a patient with no censoring value does not kill the analysis above 1000 rows", {
  # Was (S00, critical): the data.table fast path (nrow > 1000) seeded
  # censor_value/group_value with a bare logical NA. A patient whose censor or
  # group value was entirely missing returned a logical column beside other
  # patients' doubles, data.table refused the mixed type, and the WHOLE
  # analysis died - no plot, no tables, and the user saw
  # "Column 6 of result for group 2 is type 'double' but expecting type
  # 'logical'". `response_value` was already NA_character_, which is why a
  # missing response never triggered it.
  n <- 1001
  d <- data.frame(PatientID = paste0("P", seq_len(n)), Start = 0,
                  End = seq_len(n) %% 50 + 5, stringsAsFactors = FALSE)
  d$Censor <- rep(c(0, 1), length.out = n)
  d$Censor[1] <- NA
  d$Grp <- rep(c("A", "B"), length.out = n); d$Grp[2] <- NA

  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    censorVar = "Censor", groupVar = "Grp"))

  # the analysis completed: a plot state exists and no error reached the notices
  expect_false(is.null(r$plot$state))
  expect_false(grepl("Column 6 of result", sw_strip(r$notices$content), fixed = TRUE))
  expect_false(grepl("ERROR", sw_strip(r$notices$content), fixed = TRUE))

  # 999 rows takes the slow path and must keep working
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d[1:999, ], patientID = "PatientID", startTime = "Start",
    endTime = "End", censorVar = "Censor"))
  expect_false(is.null(r2$plot$state))
})


test_that("censoring coded 1/2 is read as survival's convention, not as all events", {
  # Was (S05, major): every non-zero value became "event", so a 1/2 column
  # (survival::Surv's convention: 1 = censored, 2 = event) was read as all
  # events. The reverse Kaplan-Meier was abandoned for "observed durations; no
  # censoring information" and the median follow-up came out ~50% low, silently.
  skip_if_not_installed("survival")
  d <- data.frame(PatientID = paste0("P", sprintf("%02d", 1:10)), Start = 0,
                  End = c(30, 24, 18, 12, 36, 9, 15, 21, 27, 33),
                  stringsAsFactors = FALSE)
  ev01 <- c(0, 0, 0, 1, 0, 1, 1, 1, 1, 1)      # 0 = censored, 1 = event

  fu <- function(censor) {
    d$Censor <- censor
    a <- suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
      censorVar = "Censor", personTimeAnalysis = TRUE))$advancedMetrics$asDF
    a[grepl("Follow-up", a[[1]]), , drop = FALSE]
  }

  # independent oracle: reverse KM flips the censoring indicator
  km <- survival::survfit(survival::Surv(d$End, 1 - ev01) ~ 1)
  target <- unname(summary(km)$table[["median"]])

  for (coding in list(`0/1` = ev01, `1/2` = ev01 + 1)) {
    row <- fu(coding)
    expect_match(row[[1]][1], "reverse Kaplan-Meier", fixed = TRUE)
    expect_equal(as.numeric(row[[2]][1]), target)
  }
})


test_that("the inferred censoring convention is disclosed, never assumed silently", {
  # The project rule is that the event value is never inferred in silence: if
  # the reading is wrong the median follow-up is wrong, so the user must be
  # able to see which way round it was read.
  d <- data.frame(PatientID = paste0("P", 1:10), Start = 0,
                  End = c(30, 24, 18, 12, 36, 9, 15, 21, 27, 33),
                  stringsAsFactors = FALSE)
  ev01 <- c(0, 0, 0, 1, 0, 1, 1, 1, 1, 1)

  d$Censor <- ev01 + 1
  n12 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    censorVar = "Censor"))$notices$content)
  expect_match(n12, "only the values 1 and 2", fixed = TRUE)
  expect_match(n12, "1 = censored", fixed = TRUE)

  d$Censor <- ev01
  n01 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    censorVar = "Censor"))$notices$content)
  expect_match(n01, "0 = censored", fixed = TRUE)
})


# --- response-rate denominator cluster (S01, S02, S06, S20, S38) -----------

sw_case12 <- function() data.frame(
  PatientID = paste0("P", sprintf("%02d", 1:12)), Start = 0,
  End = c(20, 18, 16, 14, 12, 11, 10, 8, 7, 6, 5, 4),
  Response = c("CR","CR","PR","PR","SD","SD","SD","PD","PD","NE","NE", NA),
  Grp = rep(c("A", "B"), 6), stringsAsFactors = FALSE)

sw_run <- function(d, ...) suppressWarnings(ClinicoPath::swimmerplot(
  data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
  responseVar = "Response", responseAnalysis = TRUE, personTimeAnalysis = TRUE, ...))

test_that("ORR and DCR divide by all patients, as RECIST 1.1 4.9.1 requires", {
  # Was (S01): the headline ORR divided by the CR/PR/SD/PD "evaluable" subset.
  # RECIST 1.1 section 4.9.1: "Trial conclusions should be based on the response
  # rate for all eligible (or all treated) patients and should not be based on a
  # selected 'evaluable' subset." NE is one of the five assigned outcomes, not
  # an exclusion. On this cohort the old code reported 44.4% (4/9).
  a <- sw_run(sw_case12())$advancedMetrics$asDF
  orr <- a[a[[1]] == "Objective Response Rate (ORR)", ]
  dcr <- a[a[[1]] == "Disease Control Rate (DCR)", ]

  expect_equal(as.numeric(orr[[2]]), round(4 / 12 * 100, 1))   # 33.3
  expect_equal(as.numeric(dcr[[2]]), round(7 / 12 * 100, 1))   # 58.3

  # the interval must use the SAME denominator as its own point estimate
  ci <- stats::binom.test(4, 12)$conf.int * 100
  expect_equal(orr[[3]][1], sprintf("%.1f - %.1f", ci[1], ci[2]))
})


test_that("every response figure on the page shares one denominator", {
  # Was (S06/S20/S38): summary rates over 11 (NE in, missing out), ORR over 9
  # (NE out), Fisher over 11, and "Study included 12 patients" - four cohorts in
  # one output, none of them labelled.
  r <- sw_run(sw_case12(), groupVar = "Grp", showCopyReady = TRUE)
  s <- r$summary$asDF
  a <- r$advancedMetrics$asDF

  # each category row states its own n/N, and the N is the patient count
  expect_match(s$metric[grepl("^CR Rate", s$metric)], "(2/12)", fixed = TRUE)
  expect_match(s$metric[grepl("^SD Rate", s$metric)], "(3/12)", fixed = TRUE)
  expect_equal(as.numeric(s$value[s$metric == "Number of Patients"]), 12)

  # the patients with no recorded response are shown, not silently dropped
  expect_true(any(grepl("No recorded response", s$metric)))

  # responders reconstructed from the rows equal the ORR numerator
  cr_n <- as.integer(sub(".*\\((\\d+)/\\d+\\).*", "\\1", s$metric[grepl("^CR Rate", s$metric)]))
  pr_n <- as.integer(sub(".*\\((\\d+)/\\d+\\).*", "\\1", s$metric[grepl("^PR Rate", s$metric)]))
  expect_equal(cr_n + pr_n, 4)
  expect_equal(round((cr_n + pr_n) / 12 * 100, 1),
               as.numeric(a[a[[1]] == "Objective Response Rate (ORR)", 2][1]))

  # the copy-ready sentence quotes the same denominator
  cr <- gsub("<[^>]+>", " ", paste(r$copyReadyReport$content, collapse = " "))
  expect_match(cr, "4/12", fixed = TRUE)
  expect_false(grepl("RECIST-evaluable", cr, fixed = TRUE))
})


test_that("a response recorded after progression does not become the best response", {
  # Was (S02): four patients who each progressed in episode 1 and were then
  # recorded CR or SD in episode 2 gave ORR 50% and DCR 100%. RECIST 1.1 takes
  # the best response up to and including the first PD.
  d <- data.frame(PatientID = rep(paste0("Q", 1:4), each = 2),
                  Start = rep(c(0, 6), 4), End = rep(c(5, 12), 4),
                  Response = c("PD","CR","PD","CR","PD","SD","PD","SD"),
                  stringsAsFactors = FALSE)
  a <- sw_run(d)$advancedMetrics$asDF
  expect_equal(as.numeric(a[a[[1]] == "Objective Response Rate (ORR)", 2][1]), 0)
  expect_equal(as.numeric(a[a[[1]] == "Disease Control Rate (DCR)", 2][1]), 0)

  # and the truncation is chronological, not row-order: shuffling the rows
  # within each patient must not change the answer
  shuffled <- d[c(2, 1, 4, 3, 6, 5, 8, 7), ]
  a2 <- sw_run(shuffled)$advancedMetrics$asDF
  expect_equal(as.numeric(a2[a2[[1]] == "Objective Response Rate (ORR)", 2][1]), 0)

  # a responder who progresses LATER still counts as a responder
  good <- d; good$Response <- c("CR","PD","PR","PD","SD","PD","PD","PD")
  a3 <- sw_run(good)$advancedMetrics$asDF
  expect_equal(as.numeric(a3[a3[[1]] == "Objective Response Rate (ORR)", 2][1]), 50)
})


# --- group-comparison cluster (S08, S09, S10, S11, S12, S24, S34) ----------

sw_groups <- function(resp, levels_order = c("A", "B")) {
  d <- data.frame(PatientID = paste0("P", 1:20), Start = 0, End = 1:20,
                  Response = resp, stringsAsFactors = FALSE)
  d$Grp <- factor(rep(c("A", "B"), each = 10), levels = levels_order)
  suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    responseVar = "Response", groupVar = "Grp", responseAnalysis = TRUE))
}

# group B responds 8/10, group A responds 2/10
sw_asym <- c(rep("PD", 8), rep("CR", 2), rep("PD", 2), rep("CR", 8))

test_that("the odds ratio names its direction and carries its interval", {
  # Was (S10/S11/S12): the label was a bare "OR = 1.30". The 2x2 was built by
  # table() on a CHARACTER group, so the rows were ordered alphabetically and
  # renaming a group inverted the estimate; fisher.test's own confidence
  # interval was computed and thrown away; nothing said which group the odds
  # belonged to. A reader could take the effect backwards.
  lab <- sw_groups(sw_asym)$groupComparisonTest$asDF$test_statistic[1]

  # direction is named, and points the right way: B responds more often
  expect_match(lab, "OR (B vs A)", fixed = TRUE)
  or <- as.numeric(sub(".*= ([0-9.]+),.*", "\\1", lab))
  expect_gt(or, 1)
  expect_match(lab, "95% CI", fixed = TRUE)

  # reversing the user's factor order reverses the stated comparison, and the
  # estimate becomes its reciprocal rather than silently meaning the opposite
  lab_rev <- sw_groups(sw_asym, levels_order = c("B", "A"))$groupComparisonTest$asDF$test_statistic[1]
  expect_match(lab_rev, "OR (A vs B)", fixed = TRUE)
  or_rev <- as.numeric(sub(".*= ([0-9.]+),.*", "\\1", lab_rev))
  expect_lt(or_rev, 1)
  # The label rounds to 2 dp, so a reciprocal near 0.08 can only be checked
  # loosely: 1/13.25 = 0.0755 prints as 0.08, a 6% gap that is display
  # rounding, not disagreement. The direction is the guarantee; this just
  # confirms the two runs describe the same comparison from opposite ends.
  expect_equal(or_rev, 1 / or, tolerance = 0.1)
})


test_that("per-group counts are shown beside the test", {
  # Was (S09/S10): no per-group n, responders or rate appeared anywhere, so the
  # test could not be checked against the rates above it.
  n <- vapply(sw_groups(sw_asym)$groupComparisonTest$notes,
              function(x) paste(x$note, collapse = " "), "")
  expect_true(any(grepl("A: 2 of 10 responded", n, fixed = TRUE)))
  expect_true(any(grepl("B: 8 of 10 responded", n, fixed = TRUE)))
})


test_that("no test, no low-cell warning, and the empty table says why", {
  # Was (S24): the low-cell warning read min() off any contingency table that
  # had been BUILT, so a run where no Fisher test was reported still warned
  # "Fisher exact test has cells with counts below 5".
  # Was (S34): the table was visible and empty with no explanation.
  r <- sw_groups(rep(c("CR", "PR"), 10))          # every patient responds
  expect_equal(nrow(r$groupComparisonTest$asDF), 0)
  expect_false(grepl("counts below 5", sw_strip(r$notices$content), fixed = TRUE))
  n <- vapply(r$groupComparisonTest$notes, function(x) paste(x$note, collapse = " "), "")
  expect_true(any(grepl("nothing to compare", n, fixed = TRUE)))

  # and when a test IS run on small cells the warning still appears
  r2 <- sw_groups(sw_asym)
  expect_equal(nrow(r2$groupComparisonTest$asDF), 2)
  expect_true(grepl("counts below 5", sw_strip(r2$notices$content), fixed = TRUE))
})


# --- censoring / follow-up cluster (S07, S14, S17, S18, S19) ---------------

test_that("censoring status comes from the latest episode, not the last row", {
  # Was (S14): status was read with tail()/[length()] in storage order while
  # the arrow's POSITION already used which.max(end_time). Re-sorting the same
  # rows therefore changed both the arrows and the reverse-KM median follow-up.
  skip_if_not_installed("survival")
  # P1 and P2 must have DIFFERENT follow-up times, or swapping their statuses
  # is symmetric and the estimate does not move - an earlier version of this
  # fixture gave both of them 12 and passed against the unfixed code.
  # P1: ends 5 (censored) then 12 (event)    -> follow-up 12, status EVENT
  # P2: ends 5 (event)    then 20 (censored) -> follow-up 20, status CENSORED
  mk <- function(rev) {
    d <- data.frame(PatientID = c("P1","P1","P2","P2","P3","P4","P5","P6"),
                    Start = c(0, 6, 0, 6, 0, 0, 0, 0),
                    End   = c(5, 12, 5, 20, 10, 14, 8, 16),
                    Censor = c(0, 1, 1, 0, 1, 0, 1, 0),
                    stringsAsFactors = FALSE)
    if (rev) d[c(2, 1, 4, 3, 5, 6, 7, 8), ] else d
  }
  fu <- function(d) {
    a <- suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
      censorVar = "Censor", personTimeAnalysis = TRUE))$advancedMetrics$asDF
    as.numeric(a[grepl("Follow-up", a[[1]]), 2][1])
  }
  # oracle: P1's latest episode (end 12) is an event, P2's (end 12) is censored
  km <- survival::survfit(survival::Surv(c(12, 20, 10, 14, 8, 16),
                                         1 - c(1, 0, 1, 0, 1, 0)) ~ 1)
  target <- unname(summary(km)$table[["median"]])

  expect_equal(fu(mk(FALSE)), target)
  expect_equal(fu(mk(TRUE)), target)     # row order must not matter
})


test_that("patients with no censoring value are excluded, not counted as events", {
  # Was (S17): `status %in% "censored"` is FALSE for NA, so as.numeric() made a
  # missing status 0 - a terminal event - biasing the reverse-KM median down.
  # Neither the unrecognised-value check nor any other counted them.
  d <- data.frame(PatientID = paste0("P", 1:8), Start = 0,
                  End = c(5, 12, 5, 12, 10, 14, 8, 16),
                  Censor = c(0, 1, NA, 1, NA, 0, 1, 0), stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    censorVar = "Censor", personTimeAnalysis = TRUE))
  nt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(r$notices$content, collapse = " ")))
  expect_match(nt, "2 of 8 patients have no censoring/event value", fixed = TRUE)
  expect_match(nt, "excluded from the reverse Kaplan-Meier", fixed = TRUE)
})


test_that("an inestimable reverse KM says so, instead of 'no censoring information'", {
  # Was (S18/S19): the fallback label claimed the censoring variable was absent
  # even when it was supplied and every value classified - here all four
  # patients are events, so the reversed curve never reaches 50%. The
  # estimator's own `reason` string had no consumer anywhere in the module.
  d <- data.frame(PatientID = paste0("P", 1:4), Start = 0, End = c(10, 20, 30, 40),
                  Censor = c(1, 1, 1, 1), stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    censorVar = "Censor", personTimeAnalysis = TRUE))
  a <- r$advancedMetrics$asDF
  row <- a[grepl("Follow-up", a[[1]]), ]
  expect_match(row[[1]][1], "reverse Kaplan-Meier not estimable", fixed = TRUE)
  expect_false(grepl("no censoring information", row[[1]][1], fixed = TRUE))
  # the reason is now shown rather than computed and discarded
  expect_true(nchar(row[[5]][1]) > nchar("Plain median of the observed durations"))
})


test_that("an unreadable custom reference date is reported, not silently dropped", {
  # Was (S07): an unparseable date fell through to the same offset fallback as
  # an EMPTY box, so the reference line was drawn somewhere the user did not
  # ask for and nothing said the date had been discarded.
  d <- data.frame(PatientID = paste0("P", 1:4),
                  Start = as.Date(c("2023-01-01","2023-02-01","2023-03-01","2023-04-01")),
                  End = as.Date(c("2023-06-01","2023-07-01","2023-08-01","2023-09-01")),
                  stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
    timeType = "datetime", dateFormat = "ymd", timeDisplay = "absolute",
    referenceLines = "custom", customReferenceDate = "not a date at all"))
  nt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(r$notices$content, collapse = " ")))
  expect_match(nt, "could not be read using the selected Date Format", fixed = TRUE)
})


test_that("milestone and event times are measured from each patient's own start", {
  # Was (S13/S16, major): the tables converted the plot's x coordinate straight
  # to a number. Under "Absolute (use actual start times)" that coordinate is a
  # study-time POSITION, so five patients starting 0/10/20/30/40 whose surgery
  # is ~3 months in published a "Median Time" of 22 (range 3 - 41) instead of 3
  # (range 1 - 4) - while the very same data entered as dates gave 3 in both
  # display modes. A display option must not change a statistic.
  d <- data.frame(
    PatientID = paste0("P", 1:5), Start = c(0, 10, 20, 30, 40),
    End = c(0, 10, 20, 30, 40) + c(12, 18, 9, 24, 15),
    Surgery = c(3, 13, 22, 34, 41),
    Ev = factor(rep("PD", 5)), EvTime = c(3, 13, 22, 34, 41),
    stringsAsFactors = FALSE)
  # hand reference: time from each patient's own start is 3, 3, 2, 4, 1
  expect_equal(stats::median(d$Surgery - d$Start), 3)

  tab <- function(disp) {
    r <- suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "PatientID", startTime = "Start", endTime = "End",
      timeDisplay = disp, milestone1Name = "Surgery", milestone1Date = "Surgery",
      showEventMarkers = TRUE, eventVar = "Ev", eventTimeVar = "EvTime"))
    list(ms = r$milestoneTable$asDF, ev = r$eventMarkerTable$asDF)
  }
  for (disp in c("relative", "absolute")) {
    o <- tab(disp)
    expect_equal(as.numeric(o$ms$median_time), 3, info = disp)
    expect_match(as.character(o$ms$time_range), "^1 - 4", info = disp)
    expect_equal(as.numeric(o$ev$median_time), 3, info = disp)
  }

  # the same timeline as dates: 30-day steps, so the answer is 90 days
  d2 <- d
  d2$Start <- as.Date("2020-01-01") + d$Start * 30
  d2$End <- as.Date("2020-01-01") + d$End * 30
  d2$Surgery <- as.character(as.Date("2020-01-01") + d$Surgery * 30)
  d2$EvTime <- d2$Surgery
  for (disp in c("relative", "absolute")) {
    r <- suppressWarnings(ClinicoPath::swimmerplot(
      data = d2, patientID = "PatientID", startTime = "Start", endTime = "End",
      timeType = "datetime", dateFormat = "ymd", timeUnit = "days",
      timeDisplay = disp, milestone1Name = "Surgery", milestone1Date = "Surgery",
      showEventMarkers = TRUE, eventVar = "Ev", eventTimeVar = "EvTime"))
    expect_equal(as.numeric(r$milestoneTable$asDF$median_time), 90, info = disp)
    expect_equal(as.numeric(r$eventMarkerTable$asDF$median_time), 90, info = disp)
  }
})


test_that("duration reference lines are withheld on an absolute axis, with the reason", {
  # Was (S28/S29, moderate): median and protocol lines were suppressed for date
  # scales only. On raw times with Relative off, lanes spanning study time
  # 100-256 still got a "Median: 12.5" line off the left-hand end and protocol
  # lines at 3/6/9/12/18/24 - drawn before any patient existed - and the
  # validation note stayed hidden.
  df <- data.frame(id = paste0("P", 1:6), start = c(100, 130, 160, 190, 220, 250),
                   end = c(112, 148, 169, 214, 232, 256), stringsAsFactors = FALSE)

  probe <- function(disp, ref, d = df) {
    opts <- ClinicoPath:::swimmerplotOptions$new(
      patientID = "id", startTime = "start", endTime = "end",
      timeType = "raw", timeDisplay = disp, referenceLines = ref)
    an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = d)
    an$run()
    tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
    on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
    suppressWarnings(an$.__enclos_env__$private$.plot(
      an$results$plot, ggtheme = ggplot2::theme_bw(), theme = NULL))
    p <- ggplot2::last_plot()
    list(vlines = sum(vapply(p$layers, function(l) inherits(l$geom, "GeomVline"), TRUE)),
         note = sw_strip(an$results$notices$content))
  }

  for (ref in c("median", "protocol")) {
    rel <- probe("relative", ref)
    expect_gt(rel$vlines, 0)                       # still drawn where x IS a duration
    expect_false(grepl("absolute axis", rel$note))

    abs <- probe("absolute", ref)
    expect_equal(abs$vlines, 0)
    expect_match(abs$note, "absolute axis")
    expect_match(abs$note, "Relative")             # names the way out
  }

  # every patient starting at 0 makes the absolute axis a duration axis again
  d0 <- df; d0$end <- d0$end - d0$start; d0$start <- 0
  expect_gt(probe("absolute", "median", d0)$vlines, 0)
  expect_gt(probe("absolute", "protocol", d0)$vlines, 0)
})


test_that("person-time does not move when the display mode changes", {
  # Was (S48, minor): under Relative, start/end were already rewritten as
  # durations measured from the patient's ANCHOR, and a calendar month measured
  # from the anchor is not the same length as one measured from the episode's
  # own start - so end-of-month cycles summed to 3.98 months relative and 4.02
  # absolute. The calendar answer is 4.0207.
  st <- c("2023-01-31", "2023-02-28", "2023-04-30", "2023-06-30", "2023-08-31", "2023-10-31")
  en <- c("2023-02-15", "2023-03-15", "2023-05-15", "2023-07-15", "2023-09-15", "2023-11-15")
  d <- rbind(data.frame(id = "A", st = st, en = en, stringsAsFactors = FALSE),
             data.frame(id = "B", st = "2023-01-01", en = "2023-02-01",
                        stringsAsFactors = FALSE))
  hand <- sum(lubridate::time_length(
    lubridate::interval(lubridate::ymd(st), lubridate::ymd(en)), "month")) + 1

  pt <- function(disp) {
    r <- suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "id", startTime = "st", endTime = "en",
      timeType = "datetime", dateFormat = "ymd", timeUnit = "months",
      timeDisplay = disp, personTimeAnalysis = TRUE))
    a <- r$advancedMetrics$asDF
    as.numeric(a[a[[1]] == "Total Study Person-Time", 2])
  }
  expect_equal(pt("relative"), pt("absolute"))
  expect_equal(pt("relative"), hand, tolerance = 0.005)
})


test_that("the validation panel counts patients, not rows", {
  # Was (S30/S44, moderate/minor): every check in .validateClinicalData counted
  # ROWS while saying "patients". Six patients on two lines each, with the
  # response on line one, were told "6 duplicate patient IDs" and "6 patients
  # with missing response data (50.0%)" on the same page whose summary showed
  # six patients and classified all of them.
  r5 <- data.frame(
    id = rep(paste0("P", 1:6), each = 2), start = rep(c(0, 6), 6),
    end = rep(c(6, 12), 6),
    resp = as.vector(rbind(c("CR", "PR", "PR", "SD", "SD", "PD"), NA)),
    stringsAsFactors = FALSE)
  panel <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = r5, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp"))$notices$content)
  expect_match(panel, "Multiple episodes recorded for 6 of 6 patients")
  expect_false(grepl("No response recorded", panel))     # none is missing
  expect_false(grepl("duplicate patient IDs", panel))

  # 3 patients, A on three rows with two blank responses: 1 multi-episode
  # patient, 0 patients without a response
  t09 <- data.frame(
    id = c("A", "A", "A", "B", "C"), start = c(0, 4, 8, 0, 0),
    end = c(4, 8, 12, 5, 7), resp = c("PR", NA, NA, "CR", "SD"),
    stringsAsFactors = FALSE)
  panel <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = t09, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp"))$notices$content)
  expect_match(panel, "Multiple episodes recorded for 1 of 3 patients")
  expect_false(grepl("No response recorded", panel))

  # a patient genuinely without any response IS still reported, per patient
  t09$resp[t09$id == "C"] <- NA
  panel <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = t09, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp"))$notices$content)
  expect_match(panel, "No response recorded for 1 of 3 patients")
})


test_that("event markers dropped outside the timeline are reported", {
  # Was (S15, major): events outside a patient's window vanished from the plot
  # AND the event table, and the table's percentages were then taken over the
  # survivors - 10 events with 3 Deaths after the last end and 1 scan before
  # the first start printed "Scan 4 (80%), Toxicity 1 (20%)" with no Death row
  # and no notice. Death after follow-up ends is the normal way it is recorded.
  d <- data.frame(id = c("A", "B", "C"), start = 0, end = c(10, 12, 8),
                  stringsAsFactors = FALSE)
  ev <- data.frame(
    id = c("A", "A", "B", "B", "C", "C", "A", "B", "C", "A"),
    et = c(2, 5, 3, 7, 1, 4, 20, 30, 40, -3),
    lab = factor(c(rep("Scan", 4), "Toxicity", "Scan", "Death", "Death", "Death", "Scan")),
    stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = merge(ev, d, by = "id", all.x = TRUE), patientID = "id",
    startTime = "start", endTime = "end", showEventMarkers = TRUE,
    eventVar = "lab", eventTimeVar = "et"))

  notes <- sw_strip(r$notices$content)
  expect_match(notes, "4 of 10 event markers are not shown")
  expect_match(notes, "1 before the patient's first start", fixed = TRUE)
  expect_match(notes, "3 after the patient's last end", fixed = TRUE)
  # and the table is explicitly scoped to what survived
  expect_match(notes, "only the 6 markers", fixed = TRUE)
  expect_equal(sum(r$eventMarkerTable$asDF$n_events), 6)

  # nothing dropped -> nothing said
  inside <- merge(ev[ev$et > 0 & ev$et < 8, ], d, by = "id", all.x = TRUE)
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = inside, patientID = "id", startTime = "start", endTime = "end",
    showEventMarkers = TRUE, eventVar = "lab", eventTimeVar = "et"))
  expect_false(grepl("event markers are not shown", sw_strip(r2$notices$content)))
})


test_that("milestone slots that are ignored, unnamed or duplicated say so", {
  # Was (S33, moderate): three silent losses. A slot above "Maximum milestones"
  # was skipped; a blank name skipped a slot that HAD a variable; and two slots
  # sharing a name collapsed into one row, so two different columns of 10 events
  # read as a single "Surgery" of 20.
  d <- data.frame(id = paste0("P", 1:10), start = 0, end = 10 + (1:10),
                  ms1 = 1:10, ms2 = 2:11, ms3 = 3:12, key = rev(1:10),
                  stringsAsFactors = FALSE)
  run <- function(...) suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end", ...))

  r <- run(maxMilestones = 2, milestone1Name = "A", milestone1Date = "ms1",
           milestone2Name = "B", milestone2Date = "ms2",
           milestone3Name = "C", milestone3Date = "ms3")
  expect_equal(nrow(r$milestoneTable$asDF), 2)
  expect_match(sw_strip(r$notices$content), "Milestone slot\\(s\\) 3")

  r <- run(milestone1Name = "", milestone1Date = "ms1")
  expect_equal(as.character(r$milestoneTable$asDF$milestone_name), "ms1")
  expect_match(sw_strip(r$notices$content), "the variable name is used instead")

  r <- run(milestone1Name = "Surgery", milestone1Date = "ms1",
           milestone2Name = "Surgery", milestone2Date = "ms2")
  expect_equal(as.character(r$milestoneTable$asDF$milestone_name),
               c("Surgery (ms1)", "Surgery (ms2)"))
  expect_equal(r$milestoneTable$asDF$n_events, c(10L, 10L))
  expect_match(sw_strip(r$notices$content), "share a name")

  # Sort Variable silently won over Sort Order
  expect_match(sw_strip(run(sortVariable = "key", sortOrder = "patient_id")$notices$content),
               "Sort Order setting is not applied")
  expect_false(grepl("Sort Order setting is not applied",
                     sw_strip(run(sortVariable = "key")$notices$content)))
})


# --- option-control cluster (S21, S22, S23) --------------------------------

sw_notes <- function(tbl) vapply(tbl$notes, function(x) paste(x$note, collapse = " "), "")

test_that("each analysis option controls exactly what its label says", {
  # Was (S21/S23, moderate): ORR and DCR required BOTH "Person-time analysis"
  # and "Response analysis", so unticking person-time removed the headline
  # response rates - while the interpretation kept its "Person-Time Analysis:
  # Total person-time ..." paragraph for a table that was no longer there. In
  # the other direction, unticking "Response analysis" left the ORR/DCR Fisher
  # tests reporting p-values and person-time still broken down by response.
  d <- data.frame(id = paste0("P", 1:12), start = 0, end = 5 + (1:12),
                  resp = rep(c("CR", "PR", "SD", "PD"), 3),
                  grp = rep(c("A", "B"), 6), stringsAsFactors = FALSE)
  run <- function(...) suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", groupVar = "grp", ...))
  rate_rows <- function(r) sum(grepl("Response Rate|Disease Control",
                                     as.character(r$advancedMetrics$asDF[[1]])))
  pt_para <- function(r) grepl("Person-Time Analysis",
                               sw_strip(r$interpretation$content))

  both <- run()
  expect_equal(rate_rows(both), 2)
  expect_equal(nrow(both$groupComparisonTest$asDF), 2)
  expect_gt(nrow(both$personTimeTable$asDF), 0)
  expect_true(pt_para(both))

  no_pt <- run(personTimeAnalysis = FALSE)
  expect_equal(rate_rows(no_pt), 2)                    # response rates survive
  expect_equal(nrow(no_pt$advancedMetrics$asDF), 2)    # and nothing else does
  expect_false(pt_para(no_pt))
  expect_equal(nrow(no_pt$personTimeTable$asDF), 0)

  no_resp <- run(responseAnalysis = FALSE)
  expect_equal(rate_rows(no_resp), 0)
  expect_equal(nrow(no_resp$groupComparisonTest$asDF), 0)
  expect_equal(nrow(no_resp$personTimeTable$asDF), 0)
  expect_equal(nrow(no_resp$advancedMetrics$asDF), 4)  # person-time still there
  expect_true(pt_para(no_resp))

  neither <- run(personTimeAnalysis = FALSE, responseAnalysis = FALSE)
  expect_equal(nrow(neither$advancedMetrics$asDF), 0)
  expect_false(pt_para(neither))
})


test_that("a table that is visible and empty says why", {
  # Was (S22, moderate): personTimeTable and milestoneTable were empty and
  # unexplained in the DEFAULT configuration, and groupComparisonTest sat empty
  # whenever the data could not support a test.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = 1:8,
                  grp = rep(c("A", "B"), 4), ms = NA_real_,
                  stringsAsFactors = FALSE)

  # a group variable but no response variable: the test cannot exist
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    groupVar = "grp"))
  expect_match(paste(sw_notes(r$groupComparisonTest), collapse = " "),
               "no Response/Status variable is selected")
  # and it must NOT invent per-group response rates for data with no responses
  expect_false(any(grepl("responded", sw_notes(r$groupComparisonTest))))

  # one group only
  d1 <- d; d1$grp <- "OnlyOne"; d1$resp <- rep(c("CR", "PR", "SD", "PD"), 2)
  r1 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d1, patientID = "id", startTime = "start", endTime = "end",
    groupVar = "grp", responseVar = "resp"))
  expect_match(paste(sw_notes(r1$groupComparisonTest), collapse = " "),
               "needs at least two groups")

  # a milestone variable with no usable value
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    milestone1Name = "S", milestone1Date = "ms"))
  expect_match(paste(sw_notes(r2$milestoneTable), collapse = " "),
               "No milestones to summarise")

  # event markers switched on with no event variable
  r3 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    showEventMarkers = TRUE))
  expect_match(paste(sw_notes(r3$eventMarkerTable), collapse = " "),
               "No event markers to summarise")
})


# --- figure cluster (S26, S27, S31, S45) -----------------------------------

sw_build <- function(d, ...) {
  opts <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", timeType = "raw", ...)
  an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = d)
  an$run()
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
  on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
  suppressWarnings(an$.__enclos_env__$private$.plot(
    an$results$plot, ggtheme = ggplot2::theme_bw(), theme = NULL))
  list(p = ggplot2::last_plot(), an = an)
}

test_that("lanes are coloured by the same labels the tables report", {
  # Was (S26, moderate): lanes were coloured by the RAW response factor while
  # every table tabulated the normalised label, so a file mixing "complete
  # response", "Complete Response" and "CR" drew six colours and six legend
  # keys for the three rows the summary reported.
  d <- data.frame(id = paste0("P", 1:6), start = 0, end = 1:6,
                  resp = c("complete response", "Complete Response", "CR",
                           "PD", "PR", "Progressive Disease"),
                  stringsAsFactors = FALSE)
  o <- sw_build(d)
  expect_equal(length(unique(ggplot2::ggplot_build(o$p)$data[[1]]$colour)), 3)
  # clinical order in the legend, not alphabetical (which put PD between PR and SD)
  expect_equal(levels(o$p$layers[[1]]$data$response), c("CR", "PR", "PD"))
  # and the tables agree: three rate rows, with the three CR spellings merged
  rates <- as.character(o$an$results$summary$asDF[[1]])
  expect_equal(sum(grepl("^(CR|PR|SD|PD|NE) Rate", rates)), 3)
  expect_true(any(grepl("^CR Rate \\(3/6\\)", rates)))
})


test_that("High Contrast survives more categories than it has colours", {
  # Was (S27, moderate): scale_color_manual() with 8 fixed values ERRORS at
  # build time above 8 levels ("Insufficient values in manual scale"). The
  # renderer's catch-all turned that into the simplified fallback plot, so
  # choosing High Contrast silently replaced the whole swimmer plot.
  d <- data.frame(id = paste0("P", 1:10), start = 0, end = 1:10,
                  resp = paste0("Cat", 1:10), stringsAsFactors = FALSE)
  o <- sw_build(d, colorPalette = "contrast")
  expect_silent(invisible(ggplot2::ggplot_build(o$p)))
  expect_equal(length(unique(ggplot2::ggplot_build(o$p)$data[[1]]$colour)), 10)
  expect_match(sw_strip(o$an$results$notices$content), "Palette changed")

  # at 8 or fewer it is still Okabe-Ito, and says nothing
  d8 <- d[1:8, ]
  o8 <- sw_build(d8, colorPalette = "contrast")
  expect_true("#E69F00" %in% ggplot2::ggplot_build(o8$p)$data[[1]]$colour)
  expect_false(grepl("Palette changed", sw_strip(o8$an$results$notices$content)))
})


test_that("the y axis reads in the sort order, from the top", {
  # Was (S31, moderate): ggplot places factor level 1 at the BOTTOM, so every
  # sort order was shown upside down - "Duration (Longest First)" put the
  # longest lane last. Patient ID also sorted lexicographically: 1 10 2 20 3.
  d <- data.frame(id = c("1", "2", "3", "10", "20"), start = 0,
                  end = c(5, 20, 1, 10, 30),
                  resp = c("PD", "CR", "PR", "SD", "CR"), stringsAsFactors = FALSE)
  top_down <- function(...) {
    b <- ggplot2::ggplot_build(sw_build(d, ...)$p)
    rev(as.character(b$layout$panel_params[[1]]$y$get_labels()))
  }
  expect_equal(top_down(sortOrder = "duration_desc"), c("20", "2", "10", "1", "3"))
  expect_equal(top_down(sortOrder = "duration_asc"),  c("3", "1", "10", "2", "20"))
  expect_equal(top_down(sortOrder = "patient_id"),    c("1", "2", "3", "10", "20"))
  # CR, CR, PR, SD, PD
  expect_equal(top_down(sortOrder = "response"),      c("2", "20", "3", "10", "1"))
})


test_that("the status arrow explains itself on the figure", {
  # Was (S45, minor): ggswim's arrow layer maps no aesthetic, so it produces no
  # legend key. The only explanation was in a glossary panel that is hidden by
  # default and does not travel with an exported image - and it said "ongoing
  # treatment", although ANY censored/alive status draws one.
  d <- data.frame(id = paste0("P", 1:6), start = 0, end = 1:6,
                  resp = rep(c("CR", "PR", "PD"), 2),
                  cens = c(0, 0, 1, 1, 1, 1), stringsAsFactors = FALSE)
  cap <- sw_build(d, censorVar = "cens")$p$labels$caption
  expect_match(cap, "still at risk")
  expect_false(grepl("still on treatment", cap, fixed = TRUE))
  # no arrows, no caption
  expect_null(sw_build(d[, setdiff(names(d), "cens")])$p$labels$caption)
})


# --- diagnostics and cost (S25, S32) ---------------------------------------

test_that("a parsing failure names the cause, not a downstream symptom", {
  # Was (S32, moderate): the raw-mode date sniffer knows only YYYY-MM-DD,
  # NN/NN/YYYY and YYYY/NN/NN, and only looks at the first three start values.
  # "1/5/2023" and "15.01.2023" fell through to as.numeric(), became NA and
  # died in the validity filter as "end times are >= start times" - an ordering
  # message for data containing no numbers at all. In datetime mode an
  # unreadable value was reported as "missing".
  raw_msg <- function(st, en) {
    d <- data.frame(id = paste0("P", 1:4), start = st, end = en,
                    stringsAsFactors = FALSE)
    sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "id", startTime = "start",
      endTime = "end"))$instructions$content)
  }
  m <- raw_msg(sprintf("%d/5/2023", 1:4), sprintf("%d/9/2023", 1:4))
  expect_match(m, "None of the start/end values are numbers")
  expect_match(m, "1/5/2023", fixed = TRUE)          # shows the offending value
  expect_match(m, "switch Time Input Type to Date/Time")
  expect_match(raw_msg(sprintf("1%d.01.2023", 5:8), sprintf("1%d.05.2023", 5:8)),
               "None of the start/end values are numbers")

  # some rows numeric, some not: a warning and a reason, not "missing"
  d3 <- data.frame(id = paste0("P", 1:4), start = c("0", "0", "1/5/2023", "2/5/2023"),
                   end = c("5", "7", "1/9/2023", "2/9/2023"), stringsAsFactors = FALSE)
  n3 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d3, patientID = "id", startTime = "start", endTime = "end"))$notices$content)
  expect_match(n3, "are not numbers")
  expect_match(n3, "start or end that is not a number")
  expect_false(grepl("missing start or end time", n3, fixed = TRUE))

  # datetime mode: an unreadable value and an empty one are reported apart
  d4 <- data.frame(id = paste0("P", 1:5),
                   start = c("2023-01-01", "2023-02-01", "2023-13-45", "", "2023-03-01"),
                   end = sprintf("2023-%02d-01", 6:10), stringsAsFactors = FALSE)
  n4 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d4, patientID = "id", startTime = "start", endTime = "end",
    timeType = "datetime", dateFormat = "ymd"))$notices$content)
  expect_match(n4, "1 had no start or end time.", fixed = TRUE)
  expect_match(n4, "could not be read with the selected Date Format")

  # dates on a raw timeline stop the analysis - and now say so
  d5 <- data.frame(id = paste0("P", 1:4), start = sprintf("2023-%02d-01", 1:4),
                   end = sprintf("2023-%02d-01", 6:9), stringsAsFactors = FALSE)
  n5 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d5, patientID = "id", startTime = "start", endTime = "end"))$notices$content)
  expect_match(n5, "Dates found on a raw numeric timeline")
})


test_that("the timeline export is bounded and says where it stopped", {
  # Was (S25, moderate): one jmvcore addRow() per patient, and addRow is
  # quadratic in the rows already present - measured on a bare Table, 250 rows
  # cost 2.1 s, 500 cost 8.3 s and 1000 cost 32.9 s, with the values themselves
  # free. A 2000-patient cohort spent 135 s of every run in that loop.
  big <- data.frame(id = paste0("P", 1:600), start = 0,
                    end = 1 + (1:600) %% 30, stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = big, patientID = "id", startTime = "start", endTime = "end",
    exportTimeline = TRUE))
  expect_equal(nrow(r$timelineData$asDF), 500)
  expect_match(paste(sw_notes(r$timelineData), collapse = " "),
               "Showing the first 500 of 600 patients")
  # the analysis itself still uses everyone
  expect_equal(as.numeric(r$summary$asDF[1, 2]), 600)

  # under the cap, nothing is said
  small <- big[1:20, ]
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = small, patientID = "id", startTime = "start", endTime = "end",
    exportTimeline = TRUE))
  expect_equal(nrow(r2$timelineData$asDF), 20)
  expect_false(any(grepl("Showing the first", sw_notes(r2$timelineData))))
})


# --- clinical wording cluster (S36, S37, S41, S42, S43) --------------------

test_that("a half-reached follow-up interval is reported, not discarded", {
  # Was (S42/S43, minor): the CI cell was blanked whenever EITHER bound was NA,
  # and with a reverse Kaplan-Meier the upper bound routinely is not reached.
  # survfit reported "28 (95% CI 22 - NA)" and the module showed an empty cell,
  # throwing away the lower bound - the useful half. The copy-ready sentence
  # never carried the interval at all.
  skip_if_not_installed("survival")
  d <- data.frame(id = paste0("P", sprintf("%02d", 1:12)), start = 0,
                  end = c(5, 8, 12, 15, 18, 20, 22, 25, 28, 30, 33, 36),
                  cens = c(1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0),
                  resp = rep(c("CR", "PR", "SD", "PD"), 3), stringsAsFactors = FALSE)
  tb <- summary(survival::survfit(
    survival::Surv(d$end, 1 - d$cens) ~ 1))$table                # independent oracle
  expect_equal(unname(tb[["median"]]), 28)
  expect_equal(unname(tb[["0.95LCL"]]), 22)
  expect_true(is.na(tb[["0.95UCL"]]))

  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    censorVar = "cens", responseVar = "resp", personTimeAnalysis = TRUE,
    showCopyReady = TRUE))
  a <- r$advancedMetrics$asDF
  row <- a[grepl("Follow-up Time", a[[1]]), , drop = FALSE]
  expect_equal(as.numeric(row$metric_value[1]), 28)
  expect_equal(as.character(row$confidence_interval[1]), "22.00 - NR")
  expect_match(sw_strip(r$copyReadyReport$content), "95% CI 22.00 - NR", fixed = TRUE)
})


test_that("person-time by response is labelled descriptive, with its bias named", {
  # Was (S41, minor): "Follow-up Density" is exactly 100/Mean Time in the same
  # row, so the column repeats its neighbour, and nothing warned that splitting
  # follow-up by BEST response is the textbook guarantee-time bias.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(22, 22, 5, 5, 9, 9, 2, 2),
                  resp = c("CR", "CR", "PD", "PD", "SD", "SD", "NE", "NE"),
                  stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", personTimeAnalysis = TRUE))
  pt <- r$personTimeTable$asDF
  # the identity the note now states
  expect_equal(pt$incidence_rate, round(100 / pt$mean_time, 3))
  notes <- paste(sw_notes(r$personTimeTable), collapse = " ")
  expect_match(notes, "100 divided by Mean Time")
  expect_match(notes, "guarantee-time bias", fixed = TRUE)
  expect_match(notes, "landmark")
})


test_that("the glossary states RECIST 1.1, including the nadir and the 5 mm rule", {
  # Was (S36/S37, minor): PD read ">=20% increase in sum of target lesion
  # diameters" - no nadir reference, no 5 mm absolute increase, and no new
  # lesions, all three of which RECIST 1.1 section 4.3.1 requires. SD named no
  # reference point and CR omitted the <10 mm lymph-node rule. PR and SD/PD use
  # DIFFERENT references (baseline vs nadir), which the glossary never said.
  d <- data.frame(id = paste0("P", 1:4), start = 0, end = 1:4, stringsAsFactors = FALSE)
  g <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    showGlossary = TRUE))$clinicalGlossary$content)

  expect_match(g, "nadir")
  expect_match(g, "5 mm", fixed = TRUE)
  expect_match(g, "new lesion")
  expect_match(g, "10 mm", fixed = TRUE)          # CR lymph-node rule
  expect_match(g, "BASELINE sum", fixed = TRUE)   # PR's reference differs
  expect_match(g, "RECIST 1.1", fixed = TRUE)
  expect_match(g, "Eisenhauer", fixed = TRUE)
  expect_match(g, "best overall response")
})


# --- presentation and plumbing (S35, S39, S40) -----------------------------

test_that("the static panels carry no fixed text colour", {
  # Was (S35, minor): 20 headings and paragraphs in the Instructions, Glossary
  # and About panels had hard-coded hues. Measured against the dark theme
  # background they fall well under WCAG's 4.5:1 floor - #7b1fa2 body text at
  # 1.73:1, #0056b3 headings at 2.01:1, #856404 at 2.58:1 - i.e. unreadable for
  # anyone using jamovi's dark theme. Hue belongs in borders and translucent
  # tints, where the background is known.
  src <- readLines(testthat::test_path("..", "..", "R", "swimmerplot-html.R"),
                   warn = FALSE)
  skip_if(length(src) == 0, "source not available from the installed package")
  text_hue <- grep("<(h[1-6]|p)[^>]*style='[^']*[^-]color: #", src, value = TRUE)
  expect_equal(length(text_hue), 0)
})


test_that("the time unit is translated, and markup stays out of the catalog", {
  # Was (S39, minor): self$options$timeUnit is the option KEY ("months") and it
  # was spliced into ~16 translated sentences, so every sentence translated
  # while the unit stayed English. Several msgids also carried HTML.
  src <- readLines(testthat::test_path("..", "..", "R", "swimmerplot.b.R"),
                   warn = FALSE)
  skip_if(length(src) == 0, "source not available from the installed package")

  # Markup inside a msgid is only a defect when the msgid is a FRAGMENT. The
  # house rule (locked by test-oncopath-library-audit.R:122-123) is that .()
  # wraps a COMPLETE sentence, and the error template below is exactly that -
  # one unit with a {message} placeholder. Splitting it into
  # "<p><strong>", .("Error:"), "</strong> " is the thing to catch: that hands
  # the translator a two-word fragment and the markup separately.
  sanctioned <- '.("<p><strong>Error:</strong> {message}</p>")'
  markup <- grep('\\.\\("[^"]*<[a-z]', src, value = TRUE)
  expect_equal(grep(sanctioned, markup, fixed = TRUE, invert = TRUE, value = TRUE),
               character(0))
  expect_false(any(grepl('.("Error:")', src, fixed = TRUE)))
  # every remaining raw-key use is a computation (lubridate unit=, the switch,
  # the plot state), never a displayed string
  raw <- grep("options\\$timeUnit", src, value = TRUE)
  displayed <- grep("unit = self\\$options\\$timeUnit|switch\\(self\\$options\\$timeUnit|timeUnit = self\\$options\\$timeUnit|timeUnitWord|^\\s*#", raw,
                    value = TRUE, invert = TRUE)
  expect_equal(displayed, character(0))

  # the helper returns a word, and falls back to the key for anything unknown
  d <- data.frame(id = paste0("P", 1:4), start = 0, end = 1:4, stringsAsFactors = FALSE)
  opts <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end", timeUnit = "weeks")
  an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = d)
  expect_equal(an$.__enclos_env__$private$.timeUnitWord(), "weeks")
  expect_equal(an$.__enclos_env__$private$.timeUnitWord("fortnights"), "fortnights")
})


test_that("the plot state carries only what the renderer draws from", {
  # Was (S40, minor): the state held the whole `stats` list - including
  # patient_summary, one row per patient - and `interpretation`, which no
  # renderer reads. 0.30 MB at 2000 patients and 1.51 MB at 10000, written into
  # every .omv and read back on every resize.
  n <- 800
  d <- data.frame(id = paste0("P", 1:n), start = 0, end = 1 + (1:n) %% 40,
                  resp = rep(c("CR", "PR", "SD", "PD"), length.out = n),
                  stringsAsFactors = FALSE)
  opts <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", timeType = "raw")
  an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = d)
  an$run()
  st <- an$results$plot$state

  expect_false("interpretation" %in% names(st))
  expect_setequal(names(st$stats),
                  c("n_patients", "median_duration", "max_duration", "total_person_time"))
  expect_false("patient_summary" %in% names(st$stats))

  # and the figure still says what it said
  tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
  on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
  expect_true(suppressWarnings(an$.__enclos_env__$private$.plot(
    an$results$plot, ggtheme = ggplot2::theme_bw(), theme = NULL)))
  expect_match(ggplot2::last_plot()$labels$subtitle, "N=800 patients", fixed = TRUE)
})


# --- post-audit fixes (F3, F4, F5, C2, C3) ---------------------------------

test_that("a STRONG_WARNING is not printed as a plain WARNING", {
  # Was (F3): .renderNotices() mapped STRONG_WARNING and WARNING to the same
  # literal "WARNING: ", so the small-sample caution read exactly like an
  # advisory note about an unparseable date. The level existed in the code and
  # nowhere in the output.
  d <- data.frame(id = paste0("P", 1:3), start = 0, end = c(5, 9, 14),
                  stringsAsFactors = FALSE)
  n <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end"))$notices$content)
  expect_match(n, "STRONG WARNING: Small sample size", fixed = TRUE)
  # ordinary notices keep their own prefixes
  expect_match(n, "NOTE: Time units", fixed = TRUE)
})


test_that("the advancedMetrics scaffold is idempotent and follows the options", {
  # Was (F4): .init() appended advancedMetrics rows with no guard, so a repeated
  # .init() gave 6 -> 12 -> 18 rows with duplicate keys. The guard used for
  # `summary` would have been WRONG here: this row set is option-dependent, and
  # .run() fills it with setRow(), which REJECTS a missing key - so freezing the
  # first set would abort the analysis rather than merely duplicate rows.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(3, 6, 9, 12, 5, 8, 11, 14),
                  resp = rep(c("CR", "PR", "SD", "PD"), 2), stringsAsFactors = FALSE)
  mk <- function(...) {
    o <- ClinicoPath:::swimmerplotOptions$new(
      patientID = "id", startTime = "start", endTime = "end", timeType = "raw", ...)
    ClinicoPath:::swimmerplotClass$new(options = o, data = d)
  }
  an <- mk(responseVar = "resp", personTimeAnalysis = TRUE, responseAnalysis = TRUE)
  p <- an$.__enclos_env__$private
  p$.init(); p$.init(); p$.init()
  keys <- unlist(an$results$advancedMetrics$rowKeys)
  expect_equal(keys, c("median_followup", "iqr", "person_time",
                       "followup_density", "orr", "dcr"))
  expect_false(any(duplicated(keys)))

  # and the set still tracks the options rather than being frozen
  an2 <- mk(responseVar = "resp", personTimeAnalysis = FALSE, responseAnalysis = TRUE)
  an2$.__enclos_env__$private$.init()
  expect_equal(unlist(an2$results$advancedMetrics$rowKeys), c("orr", "dcr"))
})


test_that("severity messages all arrive in one panel", {
  # Was (F5): the low-cell Fisher warning went to `warningNotice`, the reference-
  # line and custom-reference notes to `validationReport`, and the data-quality
  # messages to `instructions` - three Html side-channels the notices panel never
  # mentioned. Both Html items are now retired; nothing writes a severity message
  # outside .addNotice(), and no setVisible(FALSE) remains in the file.
  # the low-cell Fisher warning (was warningNotice) - sw_asym gives a 2-vs-8 split
  expect_match(sw_strip(sw_groups(sw_asym)$notices$content), "counts below 5")

  # the data-quality messages (was the instructions panel)
  d <- data.frame(id = rep(paste0("P", 1:6), each = 2), start = rep(c(0, 6), 6),
                  end = rep(c(6, 12), 6),
                  resp = as.vector(rbind(c("CR", "PR", "PR", "SD", "SD", "PD"), NA)),
                  stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp"))
  expect_match(sw_strip(r$notices$content), "Multiple episodes recorded")

  # the reference-line note (was validationReport)
  d2 <- data.frame(id = paste0("P", 1:6), start = c(100, 130, 160, 190, 220, 250),
                   end = c(112, 148, 169, 214, 232, 256), stringsAsFactors = FALSE)
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d2, patientID = "id", startTime = "start", endTime = "end",
    timeDisplay = "absolute", referenceLines = "median"))
  expect_match(sw_strip(r2$notices$content), "absolute axis")

  # the custom-reference note (was validationReport too), and its silence when a
  # valid date IS supplied
  d3 <- data.frame(id = paste0("P", 1:6), start = as.character(as.Date("2021-01-05")),
                   end = as.character(as.Date("2021-01-05") + seq(40, 45)),
                   stringsAsFactors = FALSE)
  cond <- list(data = d3, patientID = "id", startTime = "start", endTime = "end",
               timeType = "datetime", dateFormat = "ymd", timeDisplay = "absolute",
               referenceLines = "custom")
  blank <- suppressWarnings(do.call(ClinicoPath::swimmerplot,
                                    c(cond, list(customReferenceDate = ""))))
  dated <- suppressWarnings(do.call(ClinicoPath::swimmerplot,
                                    c(cond, list(customReferenceDate = "2021-02-20"))))
  expect_match(sw_strip(blank$notices$content), "Custom reference in absolute mode")
  expect_false(grepl("Custom reference in absolute mode", sw_strip(dated$notices$content)))

  # nothing writes a severity message outside .addNotice() any more, and the two
  # Html side-channels are gone from the source. (They survive in the COMPILED
  # R/swimmerplot.h.R until jmvtools::prepare() is re-run, so this asserts on the
  # source, which is what a developer controls.)
  src <- readLines(testthat::test_path("..", "..", "R", "swimmerplot.b.R"), warn = FALSE)
  skip_if(length(src) == 0, "source not available from the installed package")
  expect_equal(length(grep("setVisible\\(FALSE\\)", src)), 0)
  expect_equal(length(grep("warningNotice|validationReport", src)), 0)
  ryaml <- readLines(testthat::test_path("..", "..", "jamovi", "swimmerplot.r.yaml"), warn = FALSE)
  expect_equal(length(grep("name: (warningNotice|validationReport)", ryaml)), 0)
})


test_that("an aborted run does not leave the previous run's tables behind", {
  # Was (C2): the reset at the top of .run() cleared four items; the other nine
  # were cleared inside their own .update*() methods, which all run AFTER the
  # validation early-returns. Editing the spreadsheet changes no option, so
  # clearWith never fired and a validation error appeared above last run's
  # fully-populated tables.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = c(4, 8, 12, 16, 20, 24, 28, 32),
                  resp = rep(c("CR", "PR", "SD", "PD"), 2), stringsAsFactors = FALSE)
  o <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end", responseVar = "resp",
    timeType = "raw", personTimeAnalysis = TRUE, showInterpretation = TRUE,
    showCopyReady = TRUE, exportTimeline = TRUE)
  an <- ClinicoPath:::swimmerplotClass$new(options = o, data = d)
  an$run()
  expect_gt(an$results$personTimeTable$rowCount, 0)
  expect_gt(an$results$timelineData$rowCount, 0)
  expect_gt(nchar(paste(an$results$interpretation$content, collapse = "")), 0)

  # re-run the same results object after the data goes bad
  bad <- d; bad$end <- -1
  an$.__enclos_env__$private$.resetSummaryTable()
  an2 <- ClinicoPath:::swimmerplotClass$new(options = o, data = bad)
  an2$run()
  expect_equal(an2$results$personTimeTable$rowCount, 0)
  expect_equal(an2$results$timelineData$rowCount, 0)
  expect_equal(an2$results$groupComparisonTest$rowCount, 0)
  expect_equal(nchar(paste(an2$results$copyReadyReport$content, collapse = "")), 0)
  # the five scaffolded summary rows survive (setRow needs them) but hold no values
  expect_equal(an2$results$summary$rowCount, 5)
  expect_true(all(is.na(an2$results$summary$asDF$value)))
})


test_that("response rate rows cannot go stale, and account for every patient", {
  # Was (C3): response rows were appended once and thereafter setRow()'d, and
  # nothing ever removed one, so deleting the last SD patient left a stale
  # "SD Rate (2/8)" row with its old value and the rates summed to 125%.
  # The protection is .resetSummaryTable(), called at the top of every .run():
  # by the time .updateSummaryTable() runs, only the five fixed rows exist.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = 1:8,
                  resp = c("CR", "PR", "SD", "PD", "CR", "PR", "SD", "PD"),
                  stringsAsFactors = FALSE)
  o <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", timeType = "raw")
  an <- ClinicoPath:::swimmerplotClass$new(options = o, data = d)
  an$run()
  expect_true("response_SD" %in% unlist(an$results$summary$rowKeys))

  # the reset leaves the five fixed rows and NO response row
  an$.__enclos_env__$private$.resetSummaryTable()
  keys <- unlist(an$results$summary$rowKeys)
  expect_equal(length(keys), 5)
  expect_false(any(grepl("^response_|^no_recorded_response$", as.character(keys))))

  # and a category literally spelled "missing" no longer collides with the
  # no-response row. It did: paste0("response_", "missing") is exactly the key
  # the no-response row used, so three patients lost their row entirely and the
  # rates summed to 62.5%.
  d2 <- data.frame(id = paste0("P", 1:8), start = 0, end = 1:8,
                   resp = c("CR", "CR", "PR", "missing", "missing", "missing", NA, NA),
                   stringsAsFactors = FALSE)
  df <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d2, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp"))$summary$asDF
  lab <- as.character(df[[1]])
  expect_true(any(grepl("^missing Rate \\(3/8\\)", lab)))
  expect_true(any(grepl("^No recorded response \\(2/8\\)", lab)))
  expect_equal(sum(as.numeric(df[grepl("Rate|No recorded", lab), 2])), 100)
})


test_that("the shipped example uses only real options, real levels and real effects", {
  # Was (C1): 17 of the 19 swimmerplot() calls in inst/examples/ passed argument
  # names that have never existed (milestone1, sortBy, plotTitle, ...), so nearly
  # every call died with "unused argument"; six passed impossible enum values
  # (colorPalette = "Set2", dateFormat = "YYYY-MM-DD"); and eight supplied an
  # event variable without showEventMarkers = TRUE, so they drew a plot with no
  # events while the prose promised an adverse-event timeline.
  ex <- testthat::test_path("..", "..", "inst", "examples", "swimmerplot_example.R")
  ay <- testthat::test_path("..", "..", "jamovi", "swimmerplot.a.yaml")
  skip_if_not(file.exists(ex) && file.exists(ay), "sources not available")
  src <- paste(readLines(ex, warn = FALSE), collapse = "\n")
  ayl <- readLines(ay, warn = FALSE)

  # option names, straight out of the .a.yaml
  opts <- c("data", sub("^\\s*- name:\\s*", "", grep("^    - name:", ayl, value = TRUE)))
  calls <- regmatches(src, gregexpr("swimmerplot\\([^)]*\\)", src))[[1]]
  used <- unique(unlist(lapply(calls, function(c)
    regmatches(c, gregexpr("\\w+(?=\\s*=)", c, perl = TRUE))[[1]])))
  expect_equal(setdiff(used, opts), character(0))

  # every call that supplies an event variable must switch markers on, or it
  # silently renders nothing
  for (c in calls)
    if (grepl("eventVar", c)) expect_match(c, "showEventMarkers", fixed = TRUE)

  # and the file still runs start to finish
  expect_silent(suppressWarnings(suppressMessages(
    source(ex, local = new.env()))))
})


# --- /review-function findings (2026-09-21) --------------------------------

test_that("a Date start with a POSIXct end does not destroy person-time", {
  # Was (review, major): .asNumericTime() returned each class's own raw epoch
  # unit - DAYS for Date, SECONDS for POSIXct - and every caller compares a
  # start against an end. Measured: Total Person-Time 104,106,728 months beside
  # a correct Mean Duration of 2.5. Truth is 5.
  d <- data.frame(id = c("P1", "P2"), stringsAsFactors = FALSE)
  d$st <- as.Date(c("2020-01-01", "2020-01-01"))
  d$en <- as.POSIXct(c("2020-03-01", "2020-04-01"), tz = "UTC")
  pt <- function(dd) {
    s <- suppressWarnings(ClinicoPath::swimmerplot(
      data = dd, patientID = "id", startTime = "st", endTime = "en",
      timeType = "datetime", dateFormat = "ymd", timeUnit = "months"))$summary$asDF
    as.numeric(s[grepl("Person-Time", as.character(s[[1]])), 2])
  }
  expect_equal(pt(d), 5, tolerance = 0.05)
  # homogeneous classes were always right and must stay right
  d2 <- d; d2$en <- as.Date(c("2020-03-01", "2020-04-01"))
  expect_equal(pt(d2), 5, tolerance = 0.05)
  d3 <- d; d3$st <- as.POSIXct(c("2020-01-01", "2020-01-01"), tz = "UTC")
  expect_equal(pt(d3), 5, tolerance = 0.05)
})


test_that("milestones outside the patient's window are disclosed", {
  # Was (review, major): .processEventMarkers filters to the patient's window and
  # says what it dropped; .processMilestones never compared a milestone to the
  # timeline at all. In the module's OWN swimmerplot_test, 13 of 49 milestone
  # values fall outside it, drawn floating past the end of the lane and pooled
  # into the summary median with no notice.
  skip_if_not(exists("swimmerplot_test"), "bundled dataset not available")
  data(swimmerplot_test, envir = environment())
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = swimmerplot_test, patientID = "PatientID", startTime = "StartTime",
    endTime = "EndTime", milestone1Date = "Progression", milestone1Name = "Progression",
    milestone2Date = "BestResponse", milestone2Name = "Best Response"))
  n <- sw_strip(r$notices$content)
  expect_match(n, "Milestones outside the patient timeline")
  expect_match(n, "13 of 49 milestone values")

  # milestones that all sit inside the window say nothing
  d <- data.frame(id = paste0("P", 1:6), start = 0, end = 20, ms = 5:10,
                  stringsAsFactors = FALSE)
  r2 <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    milestone1Date = "ms", milestone1Name = "Surgery"))
  expect_false(grepl("outside the patient timeline", sw_strip(r2$notices$content)))
})


test_that("the group comparison refuses a non-RECIST response instead of printing 0%", {
  # Was (review, major): the notices panel said rates "have been omitted rather
  # than reported as 0%", Advanced Metrics correctly blanked ORR/DCR, and the
  # group table's own note then printed "Arm A: 0 of 19 responded (0.0%)".
  d <- data.frame(id = paste0("P", 1:20), start = 0, end = 1:20,
                  resp = rep(c("Grade 1-2", "Grade 3-4", "None", "Grade 1-2"), 5),
                  grp = rep(c("A", "B"), 10), stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", groupVar = "grp"))
  notes <- paste(sw_notes(r$groupComparisonTest), collapse = " ")
  expect_match(notes, "only for RECIST-coded responses")
  expect_false(grepl("responded \\(0.0%\\)", notes))
  expect_equal(nrow(r$groupComparisonTest$asDF), 0)
})


test_that("a grouping variable with one patient per group is flagged", {
  # Was (review, major): a continuous column produced a silent 24-group Fisher
  # test in which every group read "1 of 1 responded", and patients dropped for
  # a missing group value were never mentioned.
  d <- data.frame(id = paste0("P", 1:12), start = 0, end = 1:12,
                  resp = rep(c("CR", "PD"), 6),
                  cont = c(1:10, NA, NA), stringsAsFactors = FALSE)
  n <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", groupVar = "cont"))$notices$content)
  expect_match(n, "Grouping variable may not be categorical")
  expect_match(n, "2 of 12 patients have no value in the grouping variable")

  # a sensible two-group variable says neither thing
  d2 <- d; d2$grp <- rep(c("A", "B"), 6)
  n2 <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
    data = d2, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", groupVar = "grp"))$notices$content)
  expect_false(grepl("may not be categorical", n2))
})


test_that("an infinite odds ratio keeps the bound that is finite", {
  # Was (review, moderate): a zero cell sends the OR and one bound to Inf, and
  # the guard `!all(is.finite(ci))` threw the whole interval away - including a
  # finite lower bound of 1.20, the half carrying the evidence.
  # the zero cell must sit so that the ODDS RATIO runs to Inf: nobody in the
  # reference group responds, everybody in the other does.
  d <- data.frame(id = paste0("P", 1:15), start = 0, end = 1:15,
                  resp = c(rep("PD", 5), rep("CR", 10)),
                  grp = factor(c(rep("Control", 5), rep("Experimental", 10)),
                               levels = c("Control", "Experimental")),
                  stringsAsFactors = FALSE)
  lab <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", groupVar = "grp"))$groupComparisonTest$asDF$test_statistic[1]
  expect_match(lab, "95% CI", fixed = TRUE)   # not silently dropped
  expect_match(lab, "NR", fixed = TRUE)       # the unreachable bound is named
  # and the finite bound survives rather than being discarded with it
  expect_true(grepl("CI [0-9]", lab))
})


test_that("the implausible-duration guard covers every time unit", {
  # Was (review, major): switch() had arms for days and months only, so weeks
  # and years skipped the check entirely - the same data under years published
  # "median follow-up 332.0 years" with no plausibility notice at all.
  # over 10 years expressed in each unit
  over <- c(days = 4000, weeks = 600, months = 150, years = 12)
  for (u in names(over)) {
    d <- data.frame(id = paste0("P", 1:8), start = 0,
                    end = rep(over[[u]], 8), stringsAsFactors = FALSE)
    n <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "id", startTime = "start", endTime = "end",
      timeUnit = u))$notices$content)
    expect_match(n, "exceeds 10 years", info = u)
  }
  # and a plausible duration in each unit stays quiet
  under <- c(days = 300, weeks = 40, months = 10, years = 1)
  for (u in names(under)) {
    d <- data.frame(id = paste0("P", 1:8), start = 0,
                    end = rep(under[[u]], 8), stringsAsFactors = FALSE)
    n <- sw_strip(suppressWarnings(ClinicoPath::swimmerplot(
      data = d, patientID = "id", startTime = "start", endTime = "end",
      timeUnit = u))$notices$content)
    expect_false(grepl("exceeds 10 years", n), info = u)
  }
})


test_that("the capped export keeps the patients the figure shows first", {
  # Was (review, moderate): .applySorting sets levels to rev(ordered_ids)
  # because ggplot draws level 1 at the BOTTOM, so per_patient[1:500] took the
  # plot's bottom 500 - with the default duration_desc that is the 500 SHORTEST
  # timelines, and the 100 longest-followed patients were the ones dropped,
  # under a note claiming the order matched the plot.
  n <- 600
  d <- data.frame(id = rep(paste0("P", sprintf("%03d", 1:n)), each = 2),
                  start = rep(c(0, 100), n),
                  end = rep(c(50, 100), n) + rep(seq_len(n), each = 2),
                  stringsAsFactors = FALSE)
  r <- suppressWarnings(ClinicoPath::swimmerplot(
    data = d, patientID = "id", startTime = "start", endTime = "end",
    sortOrder = "duration_desc", exportTimeline = TRUE))
  exported <- as.character(r$timelineData$asDF$patient_id)
  top_down <- rev(levels(r$plot$state$patient_data$patient_id))
  expect_equal(length(exported), 500)
  expect_equal(exported, head(top_down, 500))

  dur <- tapply(d$end, d$id, max) - tapply(d$start, d$id, min)
  expect_gt(mean(dur[exported]), mean(dur[setdiff(names(dur), exported)]))
})


test_that("the jamovi palette follows the global theme (needs a regenerated .h.R)", {
  # Library-review rule 23: plot colours should be available from jamovi's own
  # palette, via jmvcore::colorPalette(n, theme$palette), with a
  # "jamovi (follow global)" level offered in the palette option.
  #
  # The level is added in jamovi/swimmerplot.a.yaml but the compiled
  # R/swimmerplot.h.R still carries the old four-level enum, so the option check
  # rejects it until jmvtools::prepare() is re-run. This test skips until then
  # and starts guarding the branch automatically once it does.
  d <- data.frame(id = paste0("P", 1:8), start = 0, end = 1:8,
                  resp = rep(c("CR", "PR", "SD", "PD"), 2), stringsAsFactors = FALSE)
  # Options$new() accepts any value; the enum is enforced by options$check()
  # inside run(), so guard on what the COMPILED header actually declares.
  # The compiler emits the enum as an OptionList block, so "jamovi" lands a few
  # lines BELOW the line naming the option - scan the block, not one line.
  hdr <- readLines(testthat::test_path("..", "..", "R", "swimmerplot.h.R"), warn = FALSE)
  start <- grep("\\.\\.colorPalette <- jmvcore::OptionList\\$new", hdr)
  block <- if (length(start)) hdr[start[1]:min(start[1] + 15L, length(hdr))] else character(0)
  skip_if(!any(grepl('"jamovi"', block, fixed = TRUE)),
          "R/swimmerplot.h.R predates the 'jamovi' palette level - run jmvtools::prepare()")

  opts <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "start", endTime = "end",
    responseVar = "resp", timeType = "raw", colorPalette = "jamovi")
  an <- ClinicoPath:::swimmerplotClass$new(options = opts, data = d)
  an$run()
  cols <- function(theme_obj) {
    tmp <- tempfile(fileext = ".png"); grDevices::png(tmp)
    on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
    suppressWarnings(an$.__enclos_env__$private$.plot(
      an$results$plot, ggtheme = ggplot2::theme_bw(), theme = theme_obj))
    sort(unique(ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]$colour))
  }
  expect_equal(cols(list(palette = "jmv")),
               sort(unname(jmvcore::colorPalette(4, "jmv", "color"))))
  # a different global palette gives different lane colours
  expect_false(identical(cols(list(palette = "jmv")), cols(list(palette = "spss"))))
  # and no theme at all still renders rather than erroring
  expect_length(cols(NULL), 4)
})


# --- security audit (2026-09-21) -------------------------------------------

test_that("every visible: expression evaluates to a logical, both set and unset", {
  # Was (audit finding 1, CRITICAL): five visible: expressions used a Variable
  # option bare inside && / ||. A .r.yaml visible: is evaluated IN R by jmvcore,
  # where a Variable is a CHARACTER string, so `TRUE && "col"` raises
  # "invalid 'y' type in 'x && y'" and the whole analysis halts with
  # "Could not resolve ...". It was inert only while R/swimmerplot.h.R was
  # stale; regenerating compiled it in and broke every run.
  #
  # The .u.yaml precedent that misled me (`enable: (outcome && multievent)`) is
  # parsed by the CLIENT-side JavaScript evaluator - a different engine.
  ay <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "swimmerplot.a.yaml"))
  ry <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "swimmerplot.r.yaml"))
  vars <- vapply(Filter(function(o) identical(o$type, "Variable") ||
                                    identical(o$type, "Variables"),
                        ay$options), function(o) o$name, "")
  bools <- vapply(Filter(function(o) identical(o$type, "Bool"), ay$options),
                  function(o) o$name, "")
  lists <- Filter(function(o) identical(o$type, "List"), ay$options)

  for (set in c(TRUE, FALSE)) {
    env <- new.env()
    for (v in vars)  assign(v, if (set) "somecol" else NULL, envir = env)
    for (b in bools) assign(b, TRUE, envir = env)
    for (l in lists) assign(l$name, l$default, envir = env)
    for (it in ry$items) {
      vis <- it$visible
      if (is.null(vis) || identical(vis, TRUE) || identical(vis, FALSE)) next
      val <- tryCatch(eval(parse(text = as.character(vis)), envir = env),
                      error = function(e) conditionMessage(e))
      expect_true(is.logical(val) && length(val) == 1L,
                  info = sprintf("%s: visible %s -> %s", it$name, vis, val))
    }
  }
})


test_that("group level names from the data cannot carry markup into a table", {
  # Was (audit finding 2, LOW): level names were interpolated into the
  # group-comparison note and the test-statistic cell unescaped. jamovi renders
  # string cells with renderMode = "rich" and notes through a markup parser
  # honouring i/em/b/strong/sub/sup, so a level named "<b>Arm A</b>" rendered
  # bold. Not XSS - the allow-list excludes script/img - but the output is the
  # user's data reshaping the report.
  d <- data.frame(id = paste0("P", 1:4), st = 0, en = 1:4, stringsAsFactors = FALSE)
  o <- ClinicoPath:::swimmerplotOptions$new(
    patientID = "id", startTime = "st", endTime = "en", timeType = "raw")
  p <- ClinicoPath:::swimmerplotClass$new(options = o, data = d)$.__enclos_env__$private

  payload <- '<img src=x onerror=alert(1)>'
  tab <- matrix(c(2, 6, 5, 3), nrow = 2,
                dimnames = list(c(payload, "Arm B"), c("no", "yes")))
  out <- p$.groupTestStatistic(tab, stats::fisher.test(tab))
  expect_false(grepl(payload, out, fixed = TRUE))
  expect_true(grepl("&lt;img src=x onerror", out, fixed = TRUE))

  # The adversarial payload run (which the audit prescribes after any HTML fix)
  # found three more cells of the same class the first pass missed: the
  # milestone name (free-text option), the event label and the response label.
  dd <- data.frame(id = paste0("P", 1:10), st = 0, en = 1:10,
                   resp = rep(c("CR", "PD"), 5), msq = 1:10,
                   check.names = FALSE, stringsAsFactors = FALSE)
  names(dd)[names(dd) == "msq"] <- payload
  rr <- suppressWarnings(do.call(ClinicoPath::swimmerplot, list(
    data = dd, patientID = "id", startTime = "st", endTime = "en",
    responseVar = "resp", milestone1Date = payload, milestone1Name = payload,
    personTimeAnalysis = TRUE)))
  cells <- paste(as.character(unlist(rr$milestoneTable$asDF)),
                 paste(as.character(unlist(rr$personTimeTable$asDF)), collapse = " "),
                 collapse = " ")
  expect_false(grepl(payload, cells, fixed = TRUE))
  expect_true(grepl("&lt;img", cells, fixed = TRUE))

  # ...but the PLOT keeps the literal label: an entity would render as visible
  # "&lt;" text on the figure.
  nm <- "Surgery & Biopsy <2cm>"
  d3 <- data.frame(id = paste0("P", 1:6), st = 0, en = 1:6, ms = 1:6,
                   stringsAsFactors = FALSE)
  r3 <- suppressWarnings(do.call(ClinicoPath::swimmerplot, list(
    data = d3, patientID = "id", startTime = "st", endTime = "en",
    milestone1Date = "ms", milestone1Name = nm)))
  expect_equal(unique(as.character(r3$plot$state$milestone_data$label)), nm)
  expect_match(as.character(r3$milestoneTable$asDF$milestone_name)[1],
               "&amp;", fixed = TRUE)
})


test_that("the syntax pane quotes a hostile variable name", {
  # Was (audit finding 3): I flagged the deparse() special-case in .asSource()
  # as a mixed-style smell. It is the opposite - measured on a column named
  # weird"name, .sourcifyOption() emits `responseVar = weird"name` (unbalanced)
  # while deparse() emits `responseVar = "weird\"name"`. The divergence is the
  # fix; this test stops anyone unifying it back.
  expect_equal(paste0(deparse('weird"name'), collapse = ""), "\"weird\\\"name\"")
  src <- readLines(testthat::test_path("..", "..", "R", "swimmerplot.b.R"), warn = FALSE)
  asrc <- grep("deparse\\(val\\)", src, value = TRUE)
  expect_length(asrc, 1)
})

# A freshly opened swimmerplot showed "ERROR: Missing required variables" before the user had
# touched anything, because the guard fired whenever ANY of the three required variables was
# NULL -- which includes the all-NULL state an analysis starts in. The instructions panel
# already lists the three and explains each, so on an empty analysis the error was pure noise
# telling the user they had made a mistake they had not yet had the chance to make.
test_that("an untouched swimmerplot shows guidance, not an error", {
    d <- data.frame(id = paste0("P", 1:6), st = rep(0, 6), en = c(4, 7, 3, 9, 5, 6),
                    stringsAsFactors = FALSE)
    run <- function(...) {
        o <- ClinicoPath:::swimmerplotOptions$new(...)
        an <- ClinicoPath:::swimmerplotClass$new(options = o, data = d); an$run(); an$results
    }
    err <- function(r) {
        x <- r$notices$content
        if (is.null(x)) "" else x
    }

    # None of the three required variables assigned: the welcome state. Instructions yes,
    # error no. A non-required variable is assigned only to get past jmvcore's harness: with
    # NOTHING selected, readDataset() picks zero columns and init() dies with
    # "invalid 'row.names' length" before .run() is ever reached. jamovi's own engine hands
    # the analysis the dataset and does reach .run(), which is why the bug was visible there
    # and not here. n_required is still 0, so this exercises the same branch.
    fresh <- run(sortVariable = "id")
    expect_false(grepl("Missing required variables", err(fresh), fixed = TRUE))
    expect_true(nzchar(fresh$instructions$content))
    expect_match(fresh$instructions$content, "Required Variables", fixed = TRUE)

    # Half-filled in is not an error either - setting an analysis up never is. The user gets
    # a NOTE naming the boxes still empty, and never the word ERROR.
    partials <- list(
        list(opts = list(patientID = "id"),
             empty = c("Start Time is still empty.", "End Time is still empty."),
             filled = "Patient ID is still empty."),
        list(opts = list(patientID = "id", startTime = "st"),
             empty = "End Time is still empty.",
             filled = "Start Time is still empty."),
        list(opts = list(startTime = "st", endTime = "en"),
             empty = "Patient ID is still empty.",
             filled = "End Time is still empty.")
    )
    for (p in partials) {
        r <- do.call(run, p$opts)
        txt <- err(r)
        expect_false(grepl("ERROR", txt, fixed = TRUE))
        expect_match(txt, "NOTE", fixed = TRUE)
        # names exactly the boxes that are still empty, and no others
        for (s in p$empty)  expect_match(txt, s, fixed = TRUE)
        expect_false(grepl(p$filled, txt, fixed = TRUE))
        expect_match(txt, "Fill them in under Core Data Variables", fixed = TRUE)
        expect_true(nzchar(r$instructions$content))
    }

    # all three assigned: no notice at all, the analysis proceeds
    done <- run(patientID = "id", startTime = "st", endTime = "en")
    expect_false(grepl("ERROR", err(done), fixed = TRUE))
    expect_false(grepl("is still empty", err(done), fixed = TRUE))
})
