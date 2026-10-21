# Regression coverage for the 2026-10-02 full audit.
dc_fix_fixture <- function() {
    counts <- c(8, 2, 1, 1, 8, 1, 3, 2)
    rows <- rep(seq_along(counts), counts)
    data.frame(
        gold = factor(c(rep("P", 4), rep("N", 4))[rows]),
        a = factor(c("P", "P", "N", "N", "N", "N", "P", "P")[rows]),
        b = factor(c("P", "N", "P", "N", "N", "P", "N", "P")[rows])
    )
}

dc_fix_analysis <- function(...) {
    opts <- do.call(ClinicoPath:::decisioncompareOptions$new, c(list(
        gold = "gold", goldPositive = "P", goldNegative = "N", test1 = "a", test1Positive = "P", test1Negative = "N",
        test2 = "b", test2Positive = "P", test2Negative = "N", statComp = TRUE
    ), list(...)))
    analysis <- ClinicoPath:::decisioncompareClass$new(options = opts, data = dc_fix_fixture())
    analysis$run()
    analysis
}

# Update both representations used by the backend and declarative visibility evaluator.
dc_fix_option <- function(analysis, name, value) {
    option <- analysis$options$option(name)
    option$value <- value
    values <- analysis$options$values()
    values[[name]] <- value
}

test_that("generated difference tables expose independently checked class-specific p-values", {
    analysis <- dc_fix_analysis()
    rows <- analysis$results$diffTable$asDF
    expect_true("p" %in% names(rows))
    # Mid-p: diseased b=2,c=1 gives 5/8; non-diseased b=1,c=3 gives 3/8.
    expect_equal(rows$p[rows$metric == "Sensitivity"], 5 / 8)
    expect_equal(rows$p[rows$metric == "Specificity"], 3 / 8)
    expect_true(is.na(rows$p[rows$metric == "Accuracy"]))

    dc_fix_option(analysis, "mcnemarMethod", "asymptotic")
    analysis$run()
    rows <- analysis$results$diffTable$asDF
    expect_equal(rows$p[rows$metric == "Sensitivity"],
                 stats::pchisq(1 / 3, 1, lower.tail = FALSE))
    expect_equal(rows$p[rows$metric == "Specificity"],
                 stats::pchisq(1, 1, lower.tail = FALSE))
})

test_that("conditional notes follow method and informativeness on the same object", {
    analysis <- dc_fix_analysis()
    table <- analysis$results$mcnemarTable
    expect_false(is.null(table$notes$mcnemar_midp_nostat))
    dc_fix_option(analysis, "mcnemarMethod", "asymptotic")
    analysis$run()
    expect_null(table$notes$mcnemar_midp_nostat)
    expect_true(all(is.finite(table$asDF$stat)))
    dc_fix_option(analysis, "mcnemarMethod", "midp")
    analysis$run()
    expect_false(is.null(table$notes$mcnemar_midp_nostat))

    # Same selected columns, but no discordance after a data edit.
    private <- analysis$.__enclos_env__$private
    private$.data$b <- private$.data$a
    analysis$run()
    expect_null(table$notes$mcnemar_midp_nostat)
    expect_null(table$notes$mcnemar_multiplicity)
    expect_null(analysis$results$diffTable$notes$diff_multiplicity)
    expect_equal(table$asDF$p, 1)
})

test_that("invalid reruns clear old values while preserving fixed table rows", {
    skip_if_not_installed("epiR")
    analysis <- dc_fix_analysis(ci = TRUE, od = TRUE, plot = TRUE,
                                radarplot = TRUE, heatmap = TRUE)
    expect_true(any(is.finite(analysis$results$epirTable1$asDF$est)))
    expect_false(is.null(analysis$results$plot1$state))
    dc_fix_option(analysis, "test1Positive", "absent")
    expect_error(analysis$run(), "Validation failed")
    for (name in c("cTable1", "cTable2")) {
        table <- analysis$results[[name]]$asDF
        expect_equal(nrow(table), 3L)
        expect_true(all(is.na(table[, c("GP", "GN", "Total")])))
    }
    for (name in c("epirTable1", "epirTable2"))
        expect_true(all(is.na(analysis$results[[name]]$asDF[, c("est", "lower", "upper")])))
    for (name in c("plot1", "plotRadar", "plotHeatmap"))
        expect_null(analysis$results[[name]]$state)
    expect_identical(analysis$results$text1$content, "")
    # Html panels are blanked with "<div></div>" since 2026-10-21: the jamovi client skips an Html update of "".
    expect_identical(analysis$results$text2$content, "<div></div>")
    expect_equal(analysis$results$comparisonTable$rowCount, 0)

    dc_fix_option(analysis, "test1Positive", "P")
    expect_no_error(analysis$run())
    expect_equal(analysis$results$cTable1$asDF$Total[3], 26)
    expect_false(is.null(analysis$results$plot1$state))
})

test_that("guidance is first and warnings are ordered by severity", {
    analysis <- dc_fix_analysis(showExplanations = TRUE)
    expect_identical(analysis$results$items[[1]]$name, "notices")
    private <- analysis$.__enclos_env__$private
    private$.noticeList <- list()
    for (type in c("INFO", "WARNING", "ERROR", "STRONG_WARNING"))
        private$.addNotice(type, paste0("Marker-", type), "Guidance")
    private$.renderNotices()
    html <- analysis$results$notices$content
    positions <- vapply(c("ERROR", "STRONG_WARNING", "WARNING", "INFO"), function(type)
        as.integer(regexpr(paste0("Marker-", type), html, fixed = TRUE)), integer(1))
    expect_true(all(positions > 0))
    expect_true(all(diff(positions) > 0))
    glossary <- analysis$results$explanationsContent$content
    expect_match(glossary, "cases must be independent across rows", fixed = TRUE)
    expect_match(glossary, "class-specific p-values are not adjusted", fixed = TRUE)
    expect_false(grepl("does not separately test", glossary, fixed = TRUE))
})
