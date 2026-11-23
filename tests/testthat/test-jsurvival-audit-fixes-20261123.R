# Regression tests for the 2026-11-23 module audit of the survival analyses.
# One block per finding; each failed on the tree the audit read.
af23_namespace <- function() {
  for (pkg in c("jsurvival", "ClinicoPath"))
    if (pkg %in% loadedNamespaces()) return(asNamespace(pkg))
  skip("analysis namespace is not loaded")
}
af23_run <- function(name, data, args, run = TRUE) {
  ns <- af23_namespace()
  options <- do.call(get(paste0(name, "Options"), ns)$new, args)
  a <- get(paste0(name, "Class"), ns)$new(options = options, data = data)
  a$init()
  if (run) a$run()
  a
}
af23_html <- function(a) {
  parts <- vapply(names(a$results), function(nm) {
    item <- a$results[[nm]]
    if (inherits(item, "Html") && is.character(item$content)) paste(item$content, collapse = " ") else ""
  }, character(1))
  paste(parts, collapse = "\n")
}

test_that("the multi-state recode panel counts the status column, not the data frame", {
  d <- data.frame(
    status = factor(c("AWOD", "AWOD", "AWD", "DOD", "DOD", "DOD", NA),
                    levels = c("AWOD", "AWD", "DOD", "DOOC")),
    t = c(5, 6, 7, 8, 9, 10, 11))
  a <- af23_run("outcomeorganizer", d, list(outcome = "status", multievent = TRUE,
    analysistype = "multistate", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
  panel <- a$results$eventRecodeInfo$content
  expect_true(nzchar(panel))
  expect_match(panel, "AWOD \\(n = 2\\).*AWD \\(n = 1\\).*DOD \\(n = 3\\).*DOOC \\(n = 0\\)")
})

test_that("time to progression names the recurrence variable it was coded from", {
  d <- data.frame(
    vital = factor(rep(c("Alive", "Dead"), 10)),
    prog = factor(rep(c("No", "Yes", "No", "No"), 5)),
    t = 1:20)
  a <- af23_run("outcomeorganizer", d, list(outcome = "vital", outcomeLevel = "Dead",
    analysistype = "ttp", recurrence = "prog", recurrenceLevel = "Yes", showNaturalSummary = TRUE))
  text <- a$results$naturalSummary$content
  expect_match(text, "variable '<b>prog</b>'", fixed = TRUE)
  expect_false(grepl("variable '<b>vital</b>'", text, fixed = TRUE))
})

test_that("the spline hazard-ratio curve is built on the rows the stratified model used", {
  set.seed(20261123)
  n <- 240
  d <- data.frame(
    time = round(rexp(n, 0.05), 2) + 0.1,
    status = factor(sample(c("Dead", "Alive"), n, TRUE, prob = c(0.7, 0.3))),
    grp = factor(sample(c("A", "B"), n, TRUE)),
    marker = round(rnorm(n, 50, 10), 1),
    centre = factor(sample(c("X", "Y", "Z"), n, TRUE)))
  # Rows without a stratum carry extreme marker values: a basis built before those rows are
  # dropped has other knots, another range and another reference median than the fitted model.
  lost <- 1:40
  d$centre[lost] <- NA
  d$marker[lost] <- d$marker[lost] + 200
  args <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", explanatory = "grp",
    rcs_analysis = TRUE, rcs_variable = "marker", stratified_cox = TRUE, strata_variable = "centre")
  with_missing <- af23_run("survival", d, args)$results$rcsPlot$state
  prefiltered <- af23_run("survival", droplevels(d[-lost, ]), args)$results$rcsPlot$state
  expect_false(is.null(with_missing))
  expect_equal(with_missing$var_median, prefiltered$var_median)
  expect_equal(range(with_missing$var_seq), range(prefiltered$var_seq))
  expect_equal(with_missing$hr_curve, prefiltered$hr_curve, tolerance = 1e-8)
  expect_equal(with_missing$hr_upper, prefiltered$hr_upper, tolerance = 1e-8)
})

# Text dates. lubridate reads text of the wrong shape at 100%: ymd_hms("15/01/2020 14:30") is
# 2015-01-20 and ymd("15.01.20") is 2015-01-20. Every interval below is positive under the
# wrong reading, so nothing but the reading itself can give the mistake away.
af23_date_data <- function(start_fmt, end_fmt) {
  day <- rep(10:19, 4)
  data.frame(
    dx = sprintf(start_fmt, day),
    fu = sprintf(end_fmt, day + 3L),
    status = factor(rep(c("Dead", "Alive", "Dead", "Dead"), 10)),
    grp = factor(rep(c("A", "B"), 20)),
    marker = rep(c(1.5, 2.5, 3.5, 4.5, 6), 8),
    stringsAsFactors = FALSE)
}
af23_date_args <- list(
  survival = list(explanatory = "grp"),
  survivalcont = list(contexpl = "marker"),
  singlearm = list(),
  multisurvival = list(explanatory = "grp"))
af23_date_run <- function(name, data, timetypedata) {
  args <- c(list(tint = TRUE, dxdate = "dx", fudate = "fu", timetypedata = timetypedata,
                 outcome = "status", outcomeLevel = "Dead"), af23_date_args[[name]])
  tryCatch(af23_html(af23_run(name, data, args)), error = function(e) paste("ERROR:", conditionMessage(e)))
}

test_that("date text the selected type cannot have is refused, not read as other dates", {
  d <- af23_date_data("%02d/01/2020 14:30", "%02d/06/2021 09:15")
  for (name in names(af23_date_args)) {
    out <- af23_date_run(name, d, "ymdhms")
    expect_match(out, "DateTime Converter", fixed = TRUE, info = name)
  }
})

test_that("two-digit years are reported with the reading they were given", {
  d <- af23_date_data("%02d.01.20", "%02d.06.21")
  for (name in names(af23_date_args)) {
    out <- af23_date_run(name, d, "ymd")
    expect_match(out, "two-digit year", fixed = TRUE, info = name)
    expect_match(out, "10.01.20", fixed = TRUE, info = name)
    expect_match(out, "2010-01-20", fixed = TRUE, info = name)
  }
})

test_that("ISO dates raise neither date warning", {
  d <- af23_date_data("2020-01-%02d", "2021-06-%02d")
  for (name in names(af23_date_args)) {
    out <- af23_date_run(name, d, "ymd")
    expect_false(grepl("two-digit year", out, fixed = TRUE), info = name)
    expect_false(grepl("DateTime Converter", out, fixed = TRUE), info = name)
    expect_false(startsWith(out, "ERROR:"), info = name)
  }
})

af23_surv_data <- function(n = 200, seed = 11) {
  set.seed(seed)
  data.frame(
    time = round(rexp(n, 0.05), 2) + 0.1,
    status = factor(sample(c("Dead", "Alive"), n, TRUE, prob = c(0.7, 0.3))),
    grp = factor(sample(c("A", "B"), n, TRUE)),
    age = round(rnorm(n, 60, 10)),
    marker = round(rnorm(n, 50, 10), 1))
}
af23_surv_args <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", explanatory = "grp")

test_that("the median table names its restricted mean and the horizon it runs to", {
  a <- af23_run("survival", af23_surv_data(), af23_surv_args)
  tbl <- a$results$medianTable
  expect_identical(tbl$getColumn("rmean")$title, "Restricted mean")
  expect_identical(tbl$getColumn("se_rmean")$title, "SE (restricted mean)")
  expect_true("rmean" %in% names(tbl$notes))
  expect_match(tbl$notes$rmean$note, "longest follow-up", fixed = TRUE)
})

test_that("the parametric model follows the same low-event rule as the other models", {
  d <- af23_surv_data(60)
  d$status <- factor(c(rep("Dead", 6), rep("Alive", 54)), levels = c("Alive", "Dead"))
  a <- af23_run("survival", d, c(af23_surv_args, list(use_parametric = TRUE)))
  tbl <- a$results$parametricModelSummary
  expect_equal(tbl$rowCount, 0)
  expect_true("lowevents" %in% names(tbl$notes))
})

test_that("the calibration note names every covariate of the model it assessed", {
  a <- af23_run("survival", af23_surv_data(), c(af23_surv_args,
    list(calibration_curves = TRUE, age_adjustment = TRUE, age_variable = "age")))
  note <- a$results$calibrationTable$notes$model$note
  expect_false(is.null(note))
  expect_match(note, "grp", fixed = TRUE)
  expect_match(note, "age", fixed = TRUE)
  # One variable used for both splines and age adjustment is in the model once.
  both <- af23_run("survival", af23_surv_data(), c(af23_surv_args, list(calibration_curves = TRUE,
    age_adjustment = TRUE, age_variable = "age", rcs_analysis = TRUE, rcs_variable = "age")))
  expect_match(both$results$calibrationTable$notes$model$note, "Model assessed: grp + age.", fixed = TRUE)
})

test_that("the weighted log-rank p-value is survdiff's own", {
  # Group C is censored before the first event: zero expected events, so the test has
  # one degree of freedom less than "groups - 1".
  d <- data.frame(
    time = c(rep(0.5, 6), 1:14, 2:15),
    status = factor(c(rep("Alive", 6), rep(c("Dead", "Dead", "Alive", "Dead", "Dead", "Dead", "Alive"), 4)),
                    levels = c("Alive", "Dead")),
    grp = factor(rep(c("C", "A", "B"), c(6, 14, 14))))
  a <- af23_run("survival", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = "grp", weightedLogRank = TRUE))
  got <- a$results$weightedLogRankTable$asDF
  ref <- survival::survdiff(survival::Surv(time, status == "Dead") ~ grp, data = d, rho = 0)
  expect_equal(got$pvalue[1], ref$pvalue, tolerance = 1e-8)
  expect_equal(got$df[1], 1)
})

af23_cont_data <- function(n = 240, seed = 7) {
  set.seed(seed)
  marker <- round(rnorm(n, 50, 10), 1)
  data.frame(
    time = round(rexp(n, 0.4 * exp((marker - 50) / 25)), 3) + 0.011,   # a years-like scale
    status = factor(sample(c("Dead", "Alive"), n, TRUE, prob = c(0.75, 0.25))),
    marker = marker)
}
af23_cont_args <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
  contexpl = "marker", timetypeoutput = "years")

test_that("the cut-off explanation does not deny the adjusted p-value the table reports", {
  a <- af23_run("survivalcont", af23_cont_data(), c(af23_cont_args, list(findcut = TRUE, showExplanations = TRUE)))
  text <- a$results$cutoffAnalysisExplanation$content
  expect_true(nzchar(text))
  expect_false(grepl("No multiplicity-adjusted p-value is reported", text, fixed = TRUE))
  expect_false(grepl("no adjusted p-value is computed here", text, fixed = TRUE))
  expect_match(text, "adjusted for the cut-off search", fixed = TRUE)
})

test_that("the restricted-mean narrative and the multi-cut medians keep the table's precision", {
  a <- af23_run("survivalcont", af23_cont_data(), c(af23_cont_args,
    list(rmst_analysis = TRUE, rmst_tau = 2.25, showSummaries = TRUE, multiple_cutoffs = TRUE)))
  expect_match(paste(a$results$rmstSummary$content, collapse = " "), "2.25", fixed = TRUE)
  med <- suppressWarnings(as.numeric(a$results$multipleMedianTable$asDF$median_survival))
  med <- med[is.finite(med)]
  expect_gt(length(med), 0)
  expect_true(any(abs(med * 10 - round(med * 10)) > 1e-6))
})

test_that("no method key that the code compares is passed through the translator", {
  ns <- af23_namespace()
  src <- paste(unlist(lapply(get("survivalcontClass", ns)$private_methods, deparse)), collapse = "\n")
  expect_false(grepl('.("quantile', src, fixed = TRUE))
})

af23_multi_src <- function() {
  paste(unlist(lapply(get("multisurvivalClass", af23_namespace())$private_methods, deparse)), collapse = "\n")
}
af23_multi_args <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
  explanatory = "grp", contexpl = "age")

test_that("the follow-up explanation is shown as markup, not as escaped source", {
  a <- af23_run("multisurvival", af23_surv_data(), c(af23_multi_args, list(showExplanations = TRUE)))
  info <- a$results$infoMessages$content
  expect_false(grepl("&lt;div", info, fixed = TRUE))
  expect_false(grepl("&lt;b&gt;", info, fixed = TRUE))
  expect_match(info, "Schemper", fixed = TRUE)
})

test_that("the summaries no longer rank a strongest predictor across measurement units", {
  d <- af23_surv_data(300)
  d$time <- round(rexp(nrow(d), 0.05 * exp(0.9 * (d$grp == "B") + 0.04 * (d$age - 60))), 2) + 0.1
  d$status <- factor("Dead", levels = c("Alive", "Dead"))
  a <- af23_run("multisurvival", d, af23_multi_args)
  out <- af23_html(a)
  expect_match(out, "statistically significant", fixed = TRUE)   # the summary ran and found effects
  expect_false(grepl("Strongest predictor", out, fixed = TRUE))
})

test_that("a competing-risk model fitted from a landmark says so", {
  d <- af23_surv_data(300, seed = 5)
  d$status <- factor(sample(c("DOD", "DOOC", "AWD", "AWOD"), nrow(d), TRUE, prob = c(0.4, 0.2, 0.2, 0.2)))
  a <- af23_run("multisurvival", d, list(elapsedtime = "time", outcome = "status", explanatory = "grp",
    contexpl = "age", multievent = TRUE, analysistype = "compete", dod = "DOD", dooc = "DOOC",
    awd = "AWD", awod = "AWOD", uselandmark = TRUE, landmark = 3))
  expect_match(a$results$text2$content, "Landmark analysis at 3", fixed = TRUE)
})

test_that("Kaplan-Meier curves keep their legend and the risk-group p-value is captioned", {
  src <- af23_multi_src()
  expect_false(grepl('legend = "none"', src, fixed = TRUE))
  expect_match(src, "not an independent test", fixed = TRUE)
})

test_that("a failed optional step states its reason on the table when summaries are off", {
  d <- af23_surv_data()
  a <- af23_run("multisurvival", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = "grp", compare_models = TRUE, showSummaries = FALSE))
  expect_true("unavailable" %in% names(a$results$modelContributionTable$notes))
})

af23_dtc <- function(x, ...) af23_run("datetimeconverter", data.frame(x = x), list(datetime_var = "x", ...))

test_that("a numeric column straddling the serial-date range is questioned", {
  a <- af23_dtc(c(4500, 6000, 9000, 12000, 15000, 8000))
  expect_match(a$results$notices$content, "outside the range a date column", fixed = TRUE)
})

test_that("a text format replaced by the serial reading is reported as replaced", {
  a <- af23_dtc(c(44000, 44010, 44020, 44100, 44200), datetime_format = "mdy")
  panel <- a$results$formatInfo$content
  expect_false(grepl("Parsing enforced manually", panel, fixed = TRUE))
  expect_match(panel, "does not fit this numeric column", fixed = TRUE)
})

test_that("serial dates with a too-large value get advice about that value", {
  a <- af23_dtc(c(44000, 44010, 44020, 44100, 44200, 999999))
  expect_match(a$results$notices$content, "too large to be serial dates", fixed = TRUE)
  expect_match(a$results$notices$content, "999999", fixed = TRUE)
})

test_that("the copy-ready sentence carries the caveat that withheld the grade", {
  a <- af23_dtc(c(100, 200, 300, 400, 500), show_summary = TRUE)   # small serials: a date-suspect notice
  expect_match(a$results$notices$content, "May Not Be Dates", fixed = TRUE)
  expect_match(a$results$nlSummary$content, "question how these values were read", fixed = TRUE)
})

test_that("a colon in a factor level is refused with advice that works in jamovi", {
  set.seed(4)
  d <- data.frame(y = factor(sample(c("yes", "no"), 120, TRUE)),
                  titre = factor(sample(c("1:40", "1:160", "1:640"), 120, TRUE)))
  args <- list(explanatory = "titre", outcome = "y", outcomeLevel = "yes", outcomeNegativeLevel = "no")
  msg <- tryCatch({af23_run("oddsratio", d, args); "ran"}, error = function(e) conditionMessage(e))
  expect_match(msg, "contain a colon", fixed = TRUE)
  expect_match(msg, "Firth", fixed = TRUE)
  expect_false(grepl("ff_glimpse|fct_recode", msg))
  firth <- tryCatch({af23_run("oddsratio", d, c(args, list(usePenalized = TRUE))); "ran"},
                    error = function(e) conditionMessage(e))
  expect_identical(firth, "ran")
})

test_that("LASSO Cox shows the warnings that explain a refusal", {
  set.seed(8)
  n <- 80
  d <- data.frame(time = round(rexp(n, 0.1), 2) + 0.1,
                  status = factor(sample(c("Dead", "Alive"), n, TRUE)),
                  a = rnorm(n), constant1 = 5, constant2 = "same")
  a <- af23_run("lassocox", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = c("a", "constant1", "constant2")), run = FALSE)
  expect_error(a$run())
  expect_match(a$results$todo$content, "constant1", fixed = TRUE)
})

test_that("LASSO Cox explains a one-valued outcome without talking about competing events", {
  n <- 60
  d <- data.frame(time = seq_len(n), status = factor(rep("Dead", n)), a = rnorm(n), b = rnorm(n))
  msg <- tryCatch({af23_run("lassocox", d, list(elapsedtime = "time", outcome = "status",
    outcomeLevel = "Dead", explanatory = c("a", "b"))); "ran"}, error = function(e) conditionMessage(e))
  expect_match(msg, "censored", fixed = TRUE)
  expect_false(grepl("competing events", msg, fixed = TRUE))
})

test_that("Time Interval counts values it could not read apart from missing ones", {
  dx <- c(sprintf("2020-01-%02d", 1:28), "unknown", "n/a")
  fu <- sprintf("2021-03-%02d", 1:30)
  a <- af23_run("timeinterval", data.frame(dx = dx, fu = fu, stringsAsFactors = FALSE),
    list(dx_date = "dx", fu_date = "fu", time_format = "ymd"))
  out <- af23_html(a)
  expect_match(out, "could not be read", fixed = TRUE)
  expect_match(out, "unknown", fixed = TRUE)
})

test_that("a level that starts like an HTML tag is kept readable in native table cells", {
  # The client parses a cell as HTML, so "<LOD" would open a tag and render blank.
  set.seed(9)
  d <- data.frame(y = factor(sample(c("yes", "no"), 150, TRUE)),
                  assay = factor(sample(c("<LOD", "Detected"), 150, TRUE), levels = c("Detected", "<LOD")))
  a <- af23_run("oddsratio", d, list(explanatory = "assay", outcome = "y", outcomeLevel = "yes",
    outcomeNegativeLevel = "no"))
  cells <- unlist(lapply(a$results$fittedTable$asDF, as.character))
  expect_true(any(grepl("< LOD", cells, fixed = TRUE)))
  expect_false(any(grepl("<LOD", cells, fixed = TRUE)))
})

test_that("the factory plot window means five years and one year in every time unit", {
  ns <- af23_namespace()
  expect_true(exists(".plotWindow", envir = ns, inherits = FALSE))
  window <- get(".plotWindow", ns)
  expect_equal(window(60, 12, "months"), list(end = 60, by = 12))
  expect_equal(window(60, 12, "days"), list(end = 1825, by = 365))
  expect_equal(window(60, 12, "years"), list(end = 5, by = 1))
  expect_equal(window(100, 10, "days"), list(end = 100, by = 10))   # the user's own values stand
  for (name in c("survival", "singlearm", "multisurvival", "survivalcont")) {
    src <- paste(unlist(lapply(get(paste0(name, "Class"), ns)$private_methods, deparse)), collapse = "\n")
    expect_false(grepl("xlim = c(0, self$options$endplot)", src, fixed = TRUE), info = name)
    expect_false(grepl("break.time.by = self$options$byplot", src, fixed = TRUE), info = name)
  }
})

test_that("the single-arm summary keeps the interval when one bound is not reached", {
  d <- data.frame(time = c(1:12, rep(30, 8)),
                  status = factor(c(rep("Dead", 12), rep("Alive", 8)), levels = c("Alive", "Dead")))
  fit <- summary(survival::survfit(survival::Surv(time, status == "Dead") ~ 1, data = d,
                                   conf.type = "log-log"))$table
  expect_true(is.finite(fit[["median"]]) && is.na(fit[["0.95UCL"]]))   # the case under test
  a <- af23_run("singlearm", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    showSummaries = TRUE))
  expect_match(a$results$clinicalSummary$content, "95% CI", fixed = TRUE)
  expect_match(a$results$clinicalSummary$content, "not reached", fixed = TRUE)
})

test_that("zero-time rows are not called retained when the landmark removes them", {
  d <- data.frame(time = c(0, 0, 3:30),
                  status = factor(rep(c("Dead", "Alive"), 15), levels = c("Alive", "Dead")))
  a <- af23_run("singlearm", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    uselandmark = TRUE, landmark = 2))
  expect_false(grepl("They are retained", af23_html(a), fixed = TRUE))
})

test_that("multi-event mode with no level assigned says what is missing", {
  d <- data.frame(time = 1:30, status = factor(rep(c("DOD", "AWD", "AWOD"), 10)), marker = rnorm(30))
  for (case in list(list("singlearm", list()), list("survivalcont", list(contexpl = "marker")))) {
    a <- af23_run(case[[1]], d, c(list(elapsedtime = "time", outcome = "status", multievent = TRUE), case[[2]]))
    expect_match(af23_html(a), "no outcome level has been assigned", fixed = TRUE, info = case[[1]])
  }
})

test_that("static help panels are filled before any variable is selected", {
  d <- data.frame(x = c("2020-01-01", "2020-02-01"), s = factor(c("a", "b")))
  dtc <- af23_run("datetimeconverter", d, list(show_explanations = TRUE, show_glossary = TRUE))
  expect_gt(nchar(dtc$results$aboutPanel$content), 50)
  expect_gt(nchar(dtc$results$glossaryPanel$content), 50)
  oo <- af23_run("outcomeorganizer", d, list(showGlossary = TRUE))
  expect_gt(nchar(oo$results$glossary$content), 50)
})

# Review pass on the fixes above (same day).
test_that("a date-time with a zone offset is not reported as a two-digit year", {
  ns <- af23_namespace()
  two <- get(".twoDigitYears", ns)
  raw <- c("2020-12-31T23:30:00-05:00", "2020-06-01T10:00:00+02:00")
  parsed <- get(".parseDateText", ns)(raw, "ymdhms")
  expect_false(anyNA(parsed))
  expect_equal(base::format(parsed[1], "%Y"), "2021")  # honestly in 2021 once converted to UTC; base::, jmvcore masks format()
  expect_equal(two(raw, parsed)$n, 0)
  expect_equal(two(c("15.01.20", NA), as.Date(c("2015-01-20", NA)))$n, 1)
  expect_equal(two(character(0), as.Date(character(0)))$n, 0)
})

test_that("a short custom plot window still gets several ticks", {
  window <- get(".plotWindow", af23_namespace())
  short <- window(90, 12, "days")            # the user's 90 days, untouched spacing
  expect_equal(short$end, 90)
  expect_lte(short$by, 45)
  expect_equal(window(1000, 12, "days")$by, 365)
  expect_equal(window(90, 30, "days")$by, 30)   # the user's own spacing stands
})

test_that("a forced Excel reading of a straddling column is questioned too", {
  a <- af23_dtc(c(4500, 6000, 9000, 12000, 15000, 8000), datetime_format = "excel_serial")
  expect_match(a$results$notices$content, "outside the range a date column", fixed = TRUE)
  b <- af23_dtc(c(44000, 44010, 44020, 44100, 44200, 999999))
  expect_false(grepl("set DateTime Format to Excel Serial", b$results$notices$content, fixed = TRUE))
})

test_that("LASSO Cox words a one-valued outcome for either kind of cohort", {
  n <- 60
  for (value in c("Dead", "Alive")) {
    d <- data.frame(time = seq_len(n), status = factor(rep(value, n)), a = rnorm(n), b = rnorm(n))
    msg <- tryCatch({af23_run("lassocox", d, list(elapsedtime = "time", outcome = "status",
      outcomeLevel = "Dead", explanatory = c("a", "b"))); "ran"}, error = function(e) conditionMessage(e))
    expect_match(msg, "one observed value", fixed = TRUE, info = value)
    expect_false(grepl("every patient had the event", msg, fixed = TRUE), info = value)
  }
})

test_that("the distribution comparison also says why it is empty below 10 events", {
  d <- af23_surv_data(60)
  d$status <- factor(c(rep("Dead", 6), rep("Alive", 54)), levels = c("Alive", "Dead"))
  a <- af23_run("survival", d, c(af23_surv_args, list(use_parametric = TRUE, compare_distributions = TRUE)))
  expect_true("lowevents" %in% names(a$results$parametricModelComparison$notes))
})

# Follow-up pass: the low-severity items of the same audit.
test_that("Outcome Organizer says when a switched-on step or a selected variable has no effect", {
  d <- data.frame(vital = factor(rep(c("DOD", "DOOC", "AWD", "AWOD"), 10)),
                  rec = factor(rep(c("No", "Yes"), 20)), t = 1:40)
  args <- list(outcome = "vital", multievent = TRUE, analysistype = "os", dod = "DOD", dooc = "DOOC",
               awd = "AWD", awod = "AWOD")
  hier <- af23_html(af23_run("outcomeorganizer", d, c(args, list(useHierarchy = TRUE, eventPriority = 1))))
  expect_match(hier, "needs a Patient ID", fixed = TRUE)
  rec <- af23_html(af23_run("outcomeorganizer", d, c(args, list(recurrence = "rec", recurrenceLevel = "Yes"))))
  expect_match(rec, "recurrence variable is not used", fixed = TRUE)
  cause <- af23_html(af23_run("outcomeorganizer", d, modifyList(args, list(analysistype = "cause"))))
  expect_false(grepl("Cumulative incidence function with competing risks", cause, fixed = TRUE))
})

test_that("Odds Ratio does not recommend Firth regression when it is already in use", {
  set.seed(31)
  d <- data.frame(y = factor(c(rep("yes", 8), rep("no", 52))), a = rnorm(60),
                  b = factor(sample(c("p", "q"), 60, TRUE)), c = rnorm(60))
  args <- list(explanatory = c("a", "b", "c"), outcome = "y", outcomeLevel = "yes", outcomeNegativeLevel = "no")
  plain <- af23_html(af23_run("oddsratio", d, args))
  expect_match(plain, "events-per-variable", fixed = TRUE)
  expect_match(plain, "Firth", fixed = TRUE)
  firth <- af23_html(af23_run("oddsratio", d, c(args, list(usePenalized = TRUE))))
  expect_match(firth, "events-per-variable", fixed = TRUE)
  expect_false(grepl("consider penalized/Firth", firth, ignore.case = TRUE))
})

test_that("LASSO Cox plots name factor columns the way its tables do", {
  set.seed(12)
  n <- 160
  d <- data.frame(stage = factor(sample(c("I", "II", "III"), n, TRUE)), age = rnorm(n, 60, 8), noise = rnorm(n))
  d$time <- round(rexp(n, 0.05 * exp(0.9 * (d$stage == "III") + 0.05 * (d$age - 60))), 2) + 0.1
  d$status <- factor(sample(c("Dead", "Alive"), n, TRUE, prob = c(0.8, 0.2)), levels = c("Alive", "Dead"))
  a <- af23_run("lassocox", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = c("stage", "age", "noise"), lambda = "lambda.min", coef_plot = TRUE, path_plot = TRUE))
  paths <- unique(a$results$path_plot$state$paths$column)
  expect_true(any(grepl("stage: ", paths, fixed = TRUE)))
  expect_false(any(paths %in% c("stageII", "stageIII")))
  coef_names <- a$results$coef_plot$state$var_names
  if (length(coef_names)) expect_false(any(coef_names %in% c("stageII", "stageIII")))
})

af23_ti <- function(dx, fu, ...) af23_run("timeinterval", data.frame(dx = dx, fu = fu, stringsAsFactors = FALSE),
  list(dx_date = "dx", fu_date = "fu", ...))

test_that("Time Interval states the timezone for date-times and the basis only where it applies", {
  days <- af23_html(af23_ti(sprintf("2020-01-%02d", 1:28), sprintf("2021-03-%02d", 1:28), time_format = "ymd",
                            output_unit = "days"))
  expect_false(grepl("Time basis:", days, fixed = TRUE))
  expect_false(grepl("Timezone:", days, fixed = TRUE))
  months <- af23_html(af23_ti(sprintf("2020-01-%02d", 1:28), sprintf("2021-03-%02d", 1:28), time_format = "ymd",
                              output_unit = "months"))
  expect_match(months, "Time basis:", fixed = TRUE)
  stamped <- af23_html(af23_ti(sprintf("2020-01-%02d 10:00:00", 1:28), sprintf("2021-03-%02d 12:30:00", 1:28),
                               time_format = "ymdhms", output_unit = "days"))
  expect_match(stamped, "Timezone:", fixed = TRUE)
})

test_that("Time Interval's ambiguity text does not claim rows that prove nothing", {
  # every day is 12 or less: no row settles day-first against month-first
  out <- af23_html(af23_ti(sprintf("%02d/01/2020", 1:12), sprintf("%02d/06/2021", 1:12), time_format = "dmy",
                           include_quality_metrics = TRUE))
  expect_false(grepl("because 0 other rows", out, fixed = TRUE))
  expect_false(grepl("other rows can only be read that way", out, fixed = TRUE))
})

test_that("a jamovi restart raised inside the guarded search is passed on, and a real error keeps its message", {
  a <- af23_run("survivalcont", af23_cont_data(), af23_cont_args, run = FALSE)
  private <- a$.__enclos_env__$private
  err <- tryCatch({private$.safeAnalysis(function() stop(jmvcore::createError("restarting", "restart")),
                                         "search", fallback_value = "fallback"); NULL},
                  error = function(e) e)
  expect_false(is.null(err))
  expect_identical(err$code, "restart")
  # an ordinary failure still returns the fallback, and its reason is kept for the notice
  expect_identical(private$.safeAnalysis(function() stop("singular matrix"), "search", fallback_value = "fallback"),
                   "fallback")
  expect_match(private$.lastAnalysisError, "singular matrix", fixed = TRUE)
})

test_that("one covariate's proportional-hazards violation is reported once", {
  set.seed(77)
  n <- 400
  d <- data.frame(marker = rnorm(n))
  # a strongly time-varying effect: early harm, late protection
  early <- rexp(n, 0.2 * exp(1.5 * d$marker)); late <- 3 + rexp(n, 0.2 * exp(-1.5 * d$marker))
  d$time <- round(ifelse(early < 3, early, late), 3) + 0.001
  d$status <- factor("Dead", levels = c("Alive", "Dead"))
  out <- af23_html(af23_run("survivalcont", d, list(elapsedtime = "time", outcome = "status",
    outcomeLevel = "Dead", contexpl = "marker")))
  expect_match(out, "Proportional Hazards Assumption Violated", fixed = TRUE)
  expect_false(grepl("Variable-Specific PH Violation", out, fixed = TRUE))
  expect_false(grepl("stratified Cox regression (Advanced Options)", out, fixed = TRUE))
})

test_that("secondary tables name variables as the user wrote them", {
  d <- af23_surv_data(300)
  names(d)[names(d) == "age"] <- "Age (years)"
  names(d)[names(d) == "grp"] <- "pT stage"
  a <- af23_run("multisurvival", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = "pT stage", contexpl = "Age (years)", compare_models = TRUE, ph_cox = TRUE))
  ph <- as.character(a$results$cox_phTable$asDF$term)
  expect_true("Age (years)" %in% ph)
  expect_false(any(c("age_years", "p_t_stage") %in% ph))
  contrib <- as.character(a$results$modelContributionTable$asDF$term)
  expect_true("Age (years)" %in% contrib)
  expect_false(any(c("age_years", "p_t_stage") %in% contrib))
})

test_that("Survival's median sentences name the variable and leave level labels alone", {
  d <- af23_surv_data()
  d$grp <- factor(ifelse(d$grp == "A", "EGFR=mut", "EGFR=wt"))
  a <- af23_run("survival", d, c(af23_surv_args, list(showSummaries = TRUE)))
  text <- paste(a$results$medianSummary$content, collapse = " ")
  expect_match(text, "grp is EGFR=mut", fixed = TRUE)
  expect_false(grepl("EGFR is mut", text, fixed = TRUE))
})

test_that("an all-missing time column is reported as missing data even with a landmark", {
  d <- af23_surv_data(); d$time <- NA_real_
  msg <- tryCatch({af23_run("survival", d, c(af23_surv_args, list(uselandmark = TRUE, landmark = 3))); "ran"},
                  error = function(e) conditionMessage(e))
  expect_match(msg, "No complete records remain", fixed = TRUE)
})

test_that("Single Arm notice cards show a readable severity, not the internal key", {
  d <- data.frame(time = c(0, 0, 3:30), status = factor(rep(c("Dead", "Alive"), 15), levels = c("Alive", "Dead")))
  out <- af23_html(af23_run("singlearm", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead")))
  expect_false(grepl("STRONG_WARNING:|WARNING:|INFO:", out))
  expect_match(out, "Warning:", fixed = TRUE)
})

test_that("the factory person-time intervals follow the time unit in Continuous Survival", {
  d <- af23_cont_data()
  d$time <- round(d$time * 365)                       # a days scale: 12, 36, 60 days would be meaningless
  a <- af23_run("survivalcont", d, modifyList(af23_cont_args, list(timetypeoutput = "days", person_time = TRUE)))
  rows <- as.character(a$results$personTimeTable$asDF$interval)
  expect_false(any(grepl("^0-12$|12-36|36-60", rows)))
  expect_true(any(grepl("365", rows)))
})

# Third pass: the items left open after the follow-up pass.
test_that("Survival names the reference level under the age-adjusted and parametric tables", {
  d <- af23_surv_data(300)
  a <- af23_run("survival", d, c(af23_surv_args, list(age_adjustment = TRUE, age_variable = "age",
    use_parametric = TRUE, parametric_covariates = TRUE)))
  for (tbl in c("ageAdjustedCoxTable", "parametricModelSummary")) {
    note <- a$results[[tbl]]$notes$reference$note
    expect_false(is.null(note), info = tbl)
    expect_match(note, "reference level, A", fixed = TRUE, info = tbl)
  }
  expect_false(any(a$results$ageAdjustedCoxTable$asDF$variable == "grp_1"))
})

test_that("the Cox explanation is not shown as if a Cox model had been fitted for competing risks", {
  d <- af23_surv_data(300, seed = 5)
  d$status <- factor(sample(c("DOD", "DOOC", "AWD", "AWOD"), nrow(d), TRUE, prob = c(0.4, 0.2, 0.2, 0.2)))
  a <- af23_run("survival", d, list(elapsedtime = "time", outcome = "status", explanatory = "grp",
    multievent = TRUE, analysistype = "compete", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
    showExplanations = TRUE))
  text <- a$results$coxRegressionExplanation$content
  expect_match(text, "not fitted for a competing-risk outcome", fixed = TRUE)
  expect_false(grepl("Understanding Your Cox Regression Results", text, fixed = TRUE))
})

test_that("the residual plot carries the model it shows, and a tiny per-unit hazard ratio is given in a note", {
  d <- af23_cont_data()
  d$marker <- d$marker * 1000                      # a large-unit marker: per-unit HR is 1.000xx
  d$site <- factor(rep(c("X", "Y", "Z"), length.out = nrow(d)))
  a <- af23_run("survivalcont", d, c(af23_cont_args, list(findcut = TRUE, residual_diagnostics = TRUE,
    stratified_cox = TRUE, strata_variable = "site")))
  expect_match(a$results$residualsPlot$state$model, "cut-off groups", fixed = TRUE)
  expect_true("precision" %in% names(a$results$stratifiedCoxTable$notes))
})

test_that("Single Arm explains an empty time-point table and blanks a meaningless standard error", {
  d <- data.frame(time = seq(1, 10, length.out = 40), status = factor(rep(c("Dead", "Alive"), 20), levels = c("Alive", "Dead")))
  a <- af23_run("singlearm", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    cutp = "500, 900", showExplanations = TRUE))
  expect_match(a$results$survivalProbabilityExplanation$content, "within the observed follow-up", fixed = TRUE)
  none <- d; none$status <- factor("Alive", levels = c("Alive", "Dead"))
  b <- af23_run("singlearm", none, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead"))
  se <- b$results$medianTable$asDF$se_rmean
  expect_true(is.na(suppressWarnings(as.numeric(se))) || identical(as.character(se), ""))
})

test_that("Odds Ratio keeps two columns apart when their names differ only by case", {
  set.seed(3); n <- 200
  d <- data.frame(y = factor(sample(c("yes", "no"), n, TRUE)), Age = factor(sample(c("young", "old"), n, TRUE)),
                  age = rnorm(n, 60, 10), check.names = FALSE)
  a <- af23_run("oddsratio", d, list(explanatory = c("Age", "age"), outcome = "y", outcomeLevel = "yes",
    outcomeNegativeLevel = "no"))
  labels <- as.character(a$results$fittedTable$asDF[[1]])
  expect_true("Age" %in% labels)     # the factor
  expect_true("age" %in% labels)     # the numeric column
})

test_that("LASSO Cox reuses the cross-validated fit when only a display option changes", {
  set.seed(12)
  n <- 160
  d <- data.frame(stage = factor(sample(c("I", "II", "III"), n, TRUE)), age = rnorm(n, 60, 8), noise = rnorm(n))
  d$time <- round(rexp(n, 0.05 * exp(0.9 * (d$stage == "III") + 0.05 * (d$age - 60))), 2) + 0.1
  d$status <- factor(sample(c("Dead", "Alive"), n, TRUE, prob = c(0.8, 0.2)), levels = c("Alive", "Dead"))
  base_args <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
    explanatory = c("stage", "age", "noise"), lambda = "lambda.min")
  cache <- get(".lassocoxCvCache", af23_namespace())
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  coefs <- function(a) a$results$coefficients$asDF
  first <- af23_run("lassocox", d, base_args)
  expect_null(cache$hits)                                    # nothing to reuse yet
  second <- af23_run("lassocox", d, c(base_args, list(showSummary = TRUE, coef_plot = TRUE)))
  expect_identical(cache$hits, 1L)
  expect_equal(coefs(second), coefs(first))
  # a warning the fit raised is part of the result, so a reused fit says it again
  cache$entry$warnings <- "planted fit warning"
  third <- af23_run("lassocox", d, c(base_args, list(showSummary = TRUE)))
  expect_identical(cache$hits, 2L)
  expect_match(third$results$todo$content, "planted fit warning", fixed = TRUE)
  # anything that changes the fit is a different fit
  af23_run("lassocox", d, c(base_args, list(random_seed = 99)))
  af23_run("lassocox", d[-1, ], base_args)
  af23_run("lassocox", d, c(base_args[names(base_args) != "explanatory"], list(explanatory = c("stage", "age"))))
  af23_run("lassocox", d, c(base_args, list(standardize = FALSE)))
  expect_identical(cache$hits, 2L)
})

test_that("an Add-to-data box of the date converter is never greyed out while it still acts", {
  # The backend adds a component when either its preview box or its Add-to-data box is
  # ticked. A disabled control keeps its value, so `enable: (extract_year)` left a ticked,
  # greyed-out box that went on adding a column.
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "datetimeconverter.u.yaml"))
  outputs <- list()
  walk <- function(node) {
    if (!is.list(node)) return(invisible())
    if (identical(node$type, "Output")) outputs[[length(outputs) + 1L]] <<- node
    for (child in node) walk(child)
  }
  walk(ui)
  expect_gte(length(outputs), 11L)
  expect_identical(Filter(function(o) !is.null(o$enable), outputs), list())
  # (An Output option reads FALSE outside the app, so the lone-box run cannot be made here.)
})

test_that("the competing-risks analysis type has one name in every analysis that offers it", {
  titles <- unlist(lapply(c("survival", "singlearm", "multisurvival", "survivalcont", "outcomeorganizer"), function(nm) {
    opts <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(nm, ".a.yaml")))$options
    type <- Filter(function(o) identical(o$name, "analysistype"), opts)[[1]]
    Filter(function(o) identical(o$name, "compete"), type$options)[[1]]$title
  }))
  expect_length(titles, 5L)
  expect_true(all(startsWith(titles, "Competing Risks")))
})

test_that("the recode notes of a DFS built with a recurrence variable describe the coding that was used", {
  set.seed(1); n <- 120
  d <- data.frame(status = factor(sample(c("DOD", "DOOC", "AWD", "AWOD"), n, TRUE)),
                  rec = factor(sample(c("Yes", "No"), n, TRUE, prob = c(.3, .7))))
  a <- af23_run("outcomeorganizer", d, list(outcome = "status", analysistype = "dfs", multievent = TRUE,
    recurrence = "rec", recurrenceLevel = "Yes", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"))
  recode <- a$results$eventRecodeTable$asDF
  expect_match(as.character(recode$levels[recode$category == "Censored"]), "AWD", fixed = TRUE)
  # Alive with Disease is coded 0 here (recurrence decides), so the notes must not say it is an event.
  expect_no_match(a$results$eventRecodeInfo$content, "is counted as an event", fixed = TRUE)
  expect_no_match(a$results$eventRecodeInfo$content, "No recurrence is counted", fixed = TRUE)
})

test_that("day-first text dates are not read as year-first in the interval check", {
  d <- data.frame(status = factor(rep(c("Dead", "Alive"), 10)),
                  l = rep(c("25/03/2020", "05/01/2020"), 10), r = rep(c("02/04/2020", "20/01/2020"), 10),
                  stringsAsFactors = FALSE)
  a <- af23_run("outcomeorganizer", d, list(outcome = "status", outcomeLevel = "Dead",
    intervalCensoring = TRUE, intervalStart = "l", intervalEnd = "r"))
  shown <- af23_html(a)
  # as.Date("25/03/2020") is 0025-03-20, which made 25 March "later" than 2 April.
  expect_no_match(shown, "interval start later than", fixed = TRUE)
  expect_match(shown, "year first", fixed = TRUE)
  iso <- transform(d, l = rep(c("2020-03-25", "2020-01-05"), 10), r = rep(c("2020-04-02", "2020-01-20"), 10))
  b <- af23_run("outcomeorganizer", iso, list(outcome = "status", outcomeLevel = "Dead",
    intervalCensoring = TRUE, intervalStart = "l", intervalEnd = "r"))
  expect_no_match(af23_html(b), "year first", fixed = TRUE)
  expect_no_match(af23_html(b), "interval start later than", fixed = TRUE)
})

test_that("a day or month order chosen by hand also reads date-times, so an ambiguous column can be settled", {
  # Month first, every day 12 or less, with a clock: the automatic reading cannot tell the
  # order and says to choose it by hand, but the list has no month-first format with a time.
  dx <- sprintf("%02d/%02d/2020 00:00:00", rep(1:6, 5), rep(c(3, 5, 7, 9, 11), each = 6))
  fu <- sprintf("%02d/%02d/2020 00:00:00", rep(1:6, 5) + 1L, rep(c(3, 5, 7, 9, 11), each = 6))
  days <- function(fmt) {
    a <- af23_ti(dx, fu, time_format = fmt, output_unit = "days", calculated_time = TRUE)
    a$results$calculated_time$.__enclos_env__$private$.values[[1]]
  }
  # month/day: 01/03 -> 02/03 is one month later (31 days); day/month would make it one day.
  expect_equal(unname(days("mdy"))[1], 31)
  expect_equal(unname(days("dmy"))[1], 1)
  shown <- af23_html(af23_ti(dx, fu, time_format = "mdy", output_unit = "days"))
  expect_match(shown, "mdy_hms", fixed = TRUE)          # the summary names what was read
  # a date-only column under the same choice is unchanged
  plain <- af23_ti(sub(" .*", "", dx), sub(" .*", "", fu), time_format = "mdy", output_unit = "days")
  expect_no_match(af23_html(plain), "mdy_hms", fixed = TRUE)
})

test_that("the per-SD odds ratio names the interval it carries, Wald or Firth's profile interval", {
  set.seed(4); n <- 300
  d <- data.frame(platelets = round(rnorm(n, 250000, 60000)))
  d$y <- factor(ifelse(runif(n) < plogis((d$platelets - 250000) / 60000), "yes", "no"))
  args <- list(explanatory = "platelets", outcome = "y", outcomeLevel = "yes", outcomeNegativeLevel = "no")
  wald <- af23_html(af23_run("oddsratio", d, args))
  expect_match(wald, "per 1 SD", fixed = TRUE)
  expect_match(wald, "(Wald 95% CI", fixed = TRUE)
  firth <- af23_html(af23_run("oddsratio", d, c(args, list(usePenalized = TRUE))))
  expect_match(firth, "per 1 SD", fixed = TRUE)
  expect_no_match(firth, "(Wald 95% CI", fixed = TRUE)
  expect_match(firth, "(profile penalized likelihood 95% CI", fixed = TRUE)
})

test_that("a boundary cumulative incidence has blank, footnoted interval cells, not NaN", {
  set.seed(2); n <- 150
  d <- data.frame(time = round(runif(n, 6, 80), 1),
    status = factor(sample(c("Censored", "Event", "Competing"), n, TRUE, prob = c(.4, .4, .2)),
                    levels = c("Censored", "Event", "Competing")))
  d$status[d$time < 20 & d$status == "Competing"] <- "Censored"     # no competing event before 20
  a <- af23_run("singlearm", d, list(elapsedtime = "time", outcome = "status", outcomeLevel = "Event",
    cutp = "12, 36, 60"))
  cif <- a$results$cifTable
  at12 <- which(cif$asDF$time == 12 & cif$asDF$estimate == 0)
  expect_length(at12, 1L)
  for (col in c("lower", "upper")) {
    cell <- cif$getCell(rowNo = at12, col)
    expect_identical(cell$value, "")                     # a numeric NA is drawn as "NaN"
    expect_gte(length(cell$footnotes), 1L)
  }
  inside <- cif$getCell(rowNo = which(cif$asDF$time == 36)[1], "lower")
  expect_true(is.numeric(inside$value) && length(inside$footnotes) == 0L)
})

test_that("packed six-digit text dates stay text dates when the column also holds a stray word", {
  # "010315" is 15 March 2001 written YYMMDD. One "N/A" used to switch the packed-text guard
  # off, the zeros were dropped (10315) and the value was read as an Excel serial: 1928-03-28.
  v <- c("010315", "020716", "030101", "040229", "011231", "020505", "030808", "041111", "010909", "N/A")
  a <- af23_dtc(v)
  converted <- as.character(a$results$conversionRows$asDF[[3]])
  expect_identical(converted[1:2], c("2001-03-15 00:00:00", "2002-07-16 00:00:00"))
  expect_no_match(af23_html(a), "Excel Serial Origin Assumed", fixed = TRUE)
})

test_that("a column left in Time Elapsed is not validated when the time comes from dates", {
  set.seed(4); n <- 150
  d <- data.frame(dx = base::format(as.Date("2015-01-01") + sample(0:700, n, TRUE)), age = rnorm(n, 60, 10),
                  sex = factor(sample(c("F", "M"), n, TRUE)), ev = rbinom(n, 1, .6))
  d$fu <- base::format(as.Date(d$dx) + sample(30:1500, n, TRUE))
  d$old_time <- c(-5, runif(n - 1, 1, 50))                 # unused: the dates give the time
  args <- list(tint = TRUE, dxdate = "dx", fudate = "fu", timetypedata = "ymd", elapsedtime = "old_time",
               outcome = "ev", explanatory = "sex", contexpl = "age")
  a <- af23_run("multisurvival", d, args)                  # used to stop: "Negative survival times detected"
  expect_gt(a$results$fittedTable$rowCount, 0)
  # the same column IS checked when it is the time
  expect_error(af23_run("multisurvival", d, c(args[setdiff(names(args), "tint")], list(tint = FALSE))),
               "Negative survival times")
})

test_that("the adjusted time-point table says which time points it could not report", {
  set.seed(4); n <- 150
  d <- data.frame(age = rnorm(n, 60, 10), sex = factor(sample(c("F", "M"), n, TRUE)), ev = rbinom(n, 1, .6),
                  t_years = runif(n, 0.2, 8))
  # The factory time points 12, 36, 60 are months; in years they lie beyond any follow-up.
  a <- af23_run("multisurvival", d, list(elapsedtime = "t_years", outcome = "ev", explanatory = "sex",
    contexpl = "age", timetypeoutput = "years", ac = TRUE, adjexplanatory = "sex", ac_summary = TRUE))
  tbl <- a$results$adjustedSurvTable
  expect_equal(tbl$rowCount, 0)
  expect_match(tbl$notes[["beyond"]]$note, "12, 36, 60", fixed = TRUE)
  b <- af23_run("multisurvival", d, list(elapsedtime = "t_years", outcome = "ev", explanatory = "sex",
    contexpl = "age", timetypeoutput = "years", ac = TRUE, adjexplanatory = "sex", ac_summary = TRUE, cutp = "1, 3, 60"))
  expect_gt(b$results$adjustedSurvTable$rowCount, 0)
  expect_match(b$results$adjustedSurvTable$notes[["beyond"]]$note, "60", fixed = TRUE)
  expect_no_match(b$results$adjustedSurvTable$notes[["beyond"]]$note, "1, 3", fixed = TRUE)
})

test_that("a bracket in the marker's name does not cut the table notes short", {
  # setNote() re-translates its text and reads " [" as the start of a context suffix.
  set.seed(3); n <- 200
  d <- data.frame(time = rexp(n, .05), ev = rbinom(n, 1, .6), check.names = FALSE)
  d[["Ki67 [%]"]] <- rnorm(n, 30, 10)
  a <- af23_run("survivalcont", d, list(elapsedtime = "time", outcome = "ev", contexpl = "Ki67 [%]",
    findcut = TRUE, residual_diagnostics = TRUE))
  note <- function(table, key) a$results[[table]]$notes[[key]]$note
  expect_match(note("coxTable", "scale"), "proportional hazards over time", fixed = TRUE)
  expect_match(note("coxTable", "scale"), "Ki67 (%)", fixed = TRUE)
  expect_match(note("rescutTable", "clinical"), "values above it the high group", fixed = TRUE)
  expect_match(note("residualsTable", "model"), "Ki67 (%)", fixed = TRUE)
})

test_that("a multi-event mapping without a death level still runs in Continuous Survival", {
  # Disease-free survival on a recurrence outcome: only the two "alive" categories exist.
  set.seed(3); n <- 200
  m <- rnorm(n, 50, 10)
  d <- data.frame(time = rexp(n, .05), m = m, st = factor(ifelse(runif(n) < plogis((m - 50) / 10), "Recurred", "Free")))
  a <- af23_run("survivalcont", d, list(elapsedtime = "time", outcome = "st", contexpl = "m", multievent = TRUE,
    awd = "Recurred", awod = "Free", analysistype = "dfs"))
  expect_gt(a$results$coxTable$rowCount, 0)       # used to sit on the welcome panel with no message
})

test_that("Single Arm says which ticked plots a competing-risk outcome does not draw", {
  set.seed(2); n <- 150
  d <- data.frame(time = round(runif(n, 6, 80), 1),
    status = factor(sample(c("Censored", "Event", "Competing"), n, TRUE, prob = c(.4, .4, .2)),
                    levels = c("Censored", "Event", "Competing")))
  base <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Event")
  a <- af23_run("singlearm", d, c(base, list(ce = TRUE, ch = TRUE, kmunicate = TRUE)))
  shown <- af23_html(a)
  expect_match(shown, "Cumulative event probability, Cumulative hazard, KMunicate-style plot", fixed = TRUE)
  expect_match(shown, "not drawn for a competing-risk outcome", fixed = TRUE)
  expect_no_match(af23_html(af23_run("singlearm", d, base)), "not drawn for a competing-risk outcome", fixed = TRUE)
})

test_that("the change after age adjustment is the change in the hazard ratio, not in its logarithm", {
  # Near HR = 1 the relative change of the log hazard ratio has a denominator near zero:
  # 1.03 -> 1.00 was reported as "changed by 100%" and called a confounding signal.
  d <- af23_surv_data(400, seed = 21)                       # grp is unrelated to survival
  a <- af23_run("survival", d, c(af23_surv_args, list(age_adjustment = TRUE, age_variable = "age")))
  shown <- a$results$ageAdjustedInterpretation$content
  d$event <- as.integer(d$status == "Dead")
  unadj <- stats::coef(survival::coxph(survival::Surv(time, event) ~ grp, data = d))[[1]]
  adj <- stats::coef(survival::coxph(survival::Surv(time, event) ~ grp + age, data = d))[[1]]
  expect_match(shown, sprintf("The hazard ratio changed by %.1f%%", 100 * (exp(abs(adj - unadj)) - 1)), fixed = TRUE)
  expect_no_match(shown, "log hazard ratio (the model coefficient) changed", fixed = TRUE)
})

test_that("the clinical summary counts significance at the three decimals the table prints", {
  # p = 0.0496 prints as 0.050 in the table, so it is not "p < 0.05" in the summary either.
  summarise <- get(".generateClinicalSummary", af23_namespace())
  coefs <- data.frame(HR = c(1.8, 1.1), p = c(0.0496, 0.30))
  text <- summarise(coefs, n_vars = 2, n_events = 80, term_map = list(a = 1L, b = 2L))
  expect_no_match(paste(text, collapse = " "), "1 out of 2", fixed = TRUE)
  clear <- summarise(transform(coefs, p = c(0.012, 0.30)), n_vars = 2, n_events = 80, term_map = list(a = 1L, b = 2L))
  expect_match(paste(clear, collapse = " "), "1 out of 2", fixed = TRUE)
})
