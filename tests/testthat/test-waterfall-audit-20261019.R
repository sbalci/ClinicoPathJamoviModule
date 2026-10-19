# Regression tests for the waterfall findings of the 2026-10-19 OncoPath module audit
# (docs/audit/MODULE_AUDIT_REPORT_20261019-1535.md in OncoPath).

wa <- function(d, ...) suppressWarnings(ClinicoPath::waterfall(data = d, ...))
wa_n <- function(res) paste(res$notices$content, collapse = " ")

test_that("no evaluable patient stops the run instead of an ERROR banner over empty tables", {
    d <- data.frame(pid = c("A", "B", "C"), t = 0, size = c(20, 30, 40))
    expect_error(ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "size", timeVar = "t",
                                        inputType = "raw"),
                 "None of the 3 patients supplied could be evaluated", fixed = TRUE)
})

test_that("negative measurements are a strong warning: the run proceeds without them", {
    d <- data.frame(pid = rep(c("A", "B", "C"), each = 2), t = rep(c(0, 2), 3),
                    size = c(20, -5, 30, 15, 40, 44))
    nt <- wa_n(wa(d, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw"))
    expect_match(nt, "NEGATIVE TUMOUR MEASUREMENTS", fixed = TRUE)
    expect_false(grepl("ERROR: NEGATIVE", nt, fixed = TRUE))
    expect_false(grepl("rgba(220, 38, 38", nt, fixed = TRUE))     # the ERROR tint
})

test_that("the fatal validation message shows patient IDs as typed", {
    d <- data.frame(pid = c("A&B", "A&B", "C", "C"), t = c(1, 2, 0, 2), size = c(20, 15, 30, 20))
    expect_error(ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "size", timeVar = "t",
                                        inputType = "raw"),
                 "A&B", fixed = TRUE)
    err <- tryCatch(ClinicoPath::waterfall(data = d, patientID = "pid", responseVar = "size", timeVar = "t",
                                           inputType = "raw"), error = conditionMessage)
    expect_false(grepl("&amp;", err, fixed = TRUE))
})

test_that("the baseline notice describes what a missing baseline does", {
    d <- data.frame(pid = rep(c("A", "B"), each = 2), t = rep(c(0, 2), 2), size = c(20, 10, 30, 33))
    expect_match(wa_n(wa(d, patientID = "pid", responseVar = "size", timeVar = "t", inputType = "raw")),
                 "A patient with no time=0 row stops the analysis", fixed = TRUE)
})

test_that("a group level starting with <letter stays visible in the comparison cells", {
    d <- data.frame(pid = paste0("P", 1:8), chg = c(-50, -40, 5, 30, -35, 10, 25, -60),
                    g = rep(c("<Neg", "Pos"), 4))
    res <- wa(d, patientID = "pid", responseVar = "chg", groupVar = "g")
    # "<" before a letter would open a tag in the HTML-parsed cell ("<5" cannot, and is left alone)
    expect_true(any(res$groupComparisonTable$asDF$group == "< Neg"))
    expect_match(paste(res$groupComparisonTest$asDF$test_statistic, collapse = " "), "< Neg", fixed = TRUE)
})

test_that("guided status tags and quick tips are current", {
    d <- data.frame(pid = paste0("P", 1:4), chg = c(-50, -40, 5, 30))
    g <- paste(wa(d, patientID = "pid", responseVar = "chg", enableGuidedMode = TRUE)$guidedAnalysis$content,
               collapse = " ")
    expect_match(g, "[DONE]", fixed = TRUE)
    expect_false(grepl("Show RECIST Thresholds", g, fixed = TRUE))
})

test_that("results definitions: no retired items, declarative person-time, glossary dependencies", {
    r <- yaml::read_yaml(test_path("..", "..", "jamovi", "waterfall.r.yaml"))
    names_ <- vapply(r$items, `[[`, "", "name")
    expect_false(any(c("naturalLanguageSummary", "explanations") %in% names_))
    item <- function(n) r$items[[which(names_ == n)]]
    expect_identical(item("personTimeTable")$visible, "(length(timeVar) > 0 && inputType:raw)")
    expect_true(all(c("timeVar", "inputType", "measurementUnit") %in% item("clinicalGlossary")$clearWith))
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "waterfall.a.yaml"))
    opt <- function(n) Filter(function(o) identical(o$name, n), a$options)[[1]]
    expect_identical(opt("showCI")$title, "Bootstrap CI of the median")
})
