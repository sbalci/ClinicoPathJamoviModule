test_that("G6 reflective confirmation: function refuses when unchecked", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = FALSE
    )

    notices_html <- res$notices$content
    expect_match(notices_html, "Reflective-measurement confirmation",
                 fixed = TRUE)
    expect_equal(res$loadingsTable$rowCount, 0)
    expect_equal(res$coxTable$rowCount, 0)
})

test_that("G6 reflective confirmation: function proceeds when checked", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")

    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = TRUE
    )

    notices_html <- res$notices$content
    expect_false(grepl("Reflective-measurement confirmation",
                       notices_html, fixed = TRUE))
})
