# Regression tests for the swimmerplot findings of the 2026-10-19 OncoPath module audit
# (docs/audit/MODULE_AUDIT_REPORT_20261019-1535.md in OncoPath).

sa <- function(...) suppressWarnings(ClinicoPath::swimmerplot(...))
sa_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(x, collapse = " ")))

test_that("F1: an Ongoing/Completed column is not read as censoring automatically", {
    d <- data.frame(id = sprintf("P%d", 1:6), start = 0, end = c(3, 5, 7, 9, 11, 14),
                    st = rep(c("Ongoing", "Completed"), 3))
    r <- sa(data = d, patientID = "id", startTime = "start", endTime = "end", censorVar = "st",
            personTimeAnalysis = TRUE)
    expect_match(sa_txt(r$notices$content), "Censoring variable not readable", fixed = TRUE)
    expect_equal(NROW(r$plot$state$arrow_data), 0)
    m <- r$advancedMetrics$asDF
    expect_match(m$metric_name[1], "not readable", fixed = TRUE)
    expect_equal(m$metric_value[1], stats::median(d$end))
    # the plain state words are still read as named
    d$st <- rep(c("alive", "dead"), 3)
    expect_match(sa_txt(sa(data = d, patientID = "id", startTime = "start", endTime = "end",
                           censorVar = "st")$notices$content), "was read by its words", fixed = TRUE)
})

test_that("F2: a response variable with no values says why ORR and DCR are blank", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 3:6, resp = factor(rep(NA, 4), levels = "CR"))
    r <- sa(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
    expect_match(sa_txt(r$notices$content), "No patient has a recorded value in the response variable", fixed = TRUE)
})

test_that("F3: a row dropped as invalid does not anchor the relative display", {
    d <- data.frame(id = c("A", "A", "B"), start = c(0, 5, 0), end = c(NA, 10, 8))
    r <- sa(data = d, patientID = "id", startTime = "start", endTime = "end", timeDisplay = "relative")
    pd <- r$plot$state$patient_data
    expect_equal(min(pd$start_time[as.character(pd$patient_id) == "A"]), 0)
})

test_that("F4/F5: an unreadable event time is named, in whole sentences", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 10,
                    ev = c("Tox", "Tox", NA, NA), evt = c("3", "abc", NA, NA))
    r <- sa(data = d, patientID = "id", startTime = "start", endTime = "end",
            showEventMarkers = TRUE, eventVar = "ev", eventTimeVar = "evt")
    nt <- sa_txt(r$notices$content)
    expect_match(nt, "1 of 2 event markers are not shown.", fixed = TRUE)
    expect_match(nt, "1 have an event time that could not be read, for example 'abc'.", fixed = TRUE)
    expect_false(grepl("with no event time", nt, fixed = TRUE))
})

test_that("D-LOW: a response level starting with < stays visible in the summary cell", {
    d <- data.frame(id = sprintf("P%d", 1:4), start = 0, end = 3:6, resp = c("CR", "<LOD", "PR", "SD"))
    r <- sa(data = d, patientID = "id", startTime = "start", endTime = "end", responseVar = "resp")
    expect_true(any(grepl("< LOD Rate", r$summary$asDF$metric, fixed = TRUE)))
})

test_that("F6: clearWith lists cover the options the content depends on", {
    r <- yaml::read_yaml(test_path("..", "..", "jamovi", "swimmerplot.r.yaml"))
    item <- function(n) Filter(function(i) identical(i$name, n), r$items)[[1]]
    expect_true(all(c("sortOrder", "sortVariable") %in% item("timelineData")$clearWith))
    expect_true("responseAnalysis" %in% item("personTimeTable")$clearWith)
    expect_true("dateFormat" %in% item("groupComparisonTest")$clearWith)
})
