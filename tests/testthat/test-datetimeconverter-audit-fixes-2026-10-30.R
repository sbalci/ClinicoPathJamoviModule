library(testthat)

# Audit fixes, 2026-10-30. Each block states what the analysis did before.

strip_html <- function(x) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}
dtc <- function(df, ...) {
    suppressWarnings(datetimeconverter(data = df, datetime_var = "v",
                                       show_quality_metrics = TRUE, ...))
}

test_that("integer DDMMYY that lost its leading zero is read as the selected text format", {
    # Was: every value set in [0, 600000] was read as Excel serials whatever was
    # selected: 50321/150321/311299 under dmy became 2037/2311/2752, "Excellent".
    r <- dtc(data.frame(v = c(50321, 150321, 311299)), datetime_format = "dmy")
    pv <- strip_html(r$previewTable$content)
    expect_match(pv, "2021-03-05", fixed = TRUE)
    expect_match(pv, "2021-03-15", fixed = TRUE)
    expect_match(pv, "1999-12-31", fixed = TRUE)
    expect_false(grepl("2037-", pv, fixed = TRUE))
    expect_match(strip_html(r$formatInfo$content), "Restored a lost leading zero on 1 value(s)",
                 fixed = TRUE)
})

test_that("digits that do not parse under the text format keep the warned serial path", {
    # 44000 is no DDMMYY/YYMMDD reading, so it stays a serial, with the misuse notice.
    r <- dtc(data.frame(v = c(44000, 44100, 45123)), datetime_format = "ymd")
    expect_match(strip_html(r$previewTable$content), "2020-06-18", fixed = TRUE)
    expect_match(strip_html(r$notices$content), "this column is numeric", fixed = TRUE)
})

test_that("a far-future result on the numeric-serial path is warned and never graded Excellent", {
    # Was: the serial path had no implausible-year check; INFO only, "Excellent".
    # (This fixture was 50321, 150321, 311299 until DC-3: those now read as DDMMYY dates.)
    r <- dtc(data.frame(v = c(44000, 44100, 44200, 99999)), datetime_format = "auto")
    expect_match(strip_html(r$notices$content), "Implausible Dates Detected", fixed = TRUE)
    expect_false(grepl("Excellent", strip_html(r$qualityAssessment$content), fixed = TRUE))

    # genuine 2020-2023 serials stay clean
    ok <- dtc(data.frame(v = c(43900, 44200, 44800, 45100)), datetime_format = "auto")
    expect_false(grepl("Implausible", strip_html(ok$notices$content), fixed = TRUE))
    expect_match(strip_html(ok$qualityAssessment$content), "Excellent", fixed = TRUE)
})

test_that("a serial conversion is not described as input already stored as datetimes", {
    r <- dtc(data.frame(v = c(43900, 44200, 44800)), datetime_format = "excel_serial")
    expect_false(grepl("already stored as datetime values",
                       strip_html(r$formatInfo$content), fixed = TRUE))
})

test_that("panels and notices are blanked with a non-empty Html on the welcome path", {
    # The jamovi client skips an empty Html update, so "" left the previous
    # variable's panels and notices on screen under the welcome panel.
    # Run on a variable that raises a notice, then empty the variable box.
    o <- datetimeconverterOptions$new(datetime_var = "v", datetime_format = "ymd",
                                      show_quality_metrics = TRUE)
    an <- datetimeconverterClass$new(options = o,
        data = data.frame(v = c("2020-01-01", "not a date", "2020-03-01")))
    suppressWarnings(an$run())
    expect_true(nzchar(gsub("<[^>]*>", "", an$results$notices$content)))
    o$.__enclos_env__$private$..datetime_var$.__enclos_env__$private$.value <- NULL
    suppressWarnings(an$.__enclos_env__$private$.run())
    for (item in c("formatInfo", "qualityMetrics", "previewTable", "componentPreview",
                   "qualityAssessment", "nlSummary", "notices"))
        expect_identical(an$results[[item]]$content, "<div></div>", info = item)
})
