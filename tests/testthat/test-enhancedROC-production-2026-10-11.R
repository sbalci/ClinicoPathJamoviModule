# Production pass 2026-10-11 for the /check-function-full audit (TODO.md, "enhancedROC - /fix-function
# production pass"). One test_that per finding, each written to FAIL on the code before its fix.

prod_data <- function(n = 200, seed = 1) {
    set.seed(seed)
    y <- factor(rep(c("neg", "pos"), each = n / 2), levels = c("neg", "pos"))
    data.frame(y = y,
               a = rnorm(n, ifelse(y == "pos", 1.5, 0)),
               b = rnorm(n, ifelse(y == "pos", 0.6, 0)),
               c = rnorm(n, ifelse(y == "pos", 0.3, 0)))
}
prod_obj <- function(data, ...) {
    options <- do.call(enhancedROCOptions$new, list(...))
    analysis <- enhancedROCClass$new(options = options, data = data)
    suppressWarnings(analysis$run())
    analysis
}
prod_notices <- function(analysis) analysis$.__enclos_env__$private$.noticeList
prod_titled <- function(analysis, title) Filter(function(n) identical(n$title, title), prod_notices(analysis))
prod_base <- list(outcome = "y", positiveClass = "pos")
prod_run <- function(data, ...) do.call(prod_obj, c(list(data), prod_base, list(...)))

test_that("P-01: a Venkatraman permutation p-value is never below 1/(B + 1)", {
    a <- prod_run(prod_data(), predictors = c("a", "c"), analysisType = "comparative",
                  pairwiseComparisons = TRUE, statisticalComparison = TRUE,
                  comparisonMethod = "venkatraman", bootstrapSamples = 200, seed = 42)
    r <- a$results$results
    expect_gte(min(r$rocComparisons$asDF$p_value), 1 / 201)
    expect_gte(min(r$statisticalSummary$asDF$p_value), 1 / 201)
    expect_false(any(grepl("p < 0.001", r$statisticalSummary$asDF$conclusion, fixed = TRUE)))
})

test_that("P-02: a resampled result does not depend on what was resampled before it", {
    d <- prod_data()
    ci <- function(preds) {
        t <- prod_run(d, predictors = preds, useBootstrap = TRUE, bootstrapSamples = 200, seed = 7)$results$results$aucSummary$asDF
        unlist(t[t$predictor == "b", c("auc_lower", "auc_upper")])
    }
    expect_equal(ci(c("a", "b")), ci("b"))
    band <- function(preds) prod_run(d, predictors = preds, showConfidenceBands = TRUE, bootstrapSamples = 200,
                                     seed = 7)$results$results$rocCurvePlot$state$roc$b$bands
    expect_false(is.null(band("b")))
    expect_equal(band(c("a", "b")), band("b"))
    vk <- function(boot) prod_run(d, predictors = c("a", "b"), analysisType = "comparative", pairwiseComparisons = TRUE,
                                  comparisonMethod = "venkatraman", bootstrapSamples = 200, seed = 7,
                                  useBootstrap = boot)$results$results$rocComparisons$asDF$p_value
    expect_equal(vk(FALSE), vk(TRUE))
})

test_that("P-03: one notice states every excluded row, whatever the number of predictors", {
    d <- prod_data()
    d$y[1:5] <- NA                                   # plain NA
    levels(d$y) <- c(levels(d$y), "")
    d$y[6:7] <- ""                                    # blank level
    d$a[10:12] <- NA                                  # missing predictor, outcome present
    n <- prod_titled(prod_run(d, predictors = "a"), "Rows Excluded")
    expect_length(n, 1L)
    expect_identical(n[[1]]$type, "INFO")
    expect_match(n[[1]]$content, "190 of 200 rows can be analyzed", fixed = TRUE)
    expect_match(n[[1]]$content, "7 rows had a missing outcome", fixed = TRUE)
    expect_match(n[[1]]$content, "3 further rows had a missing value of a selected predictor (a: 3)", fixed = TRUE)
    d$b[20] <- NA
    n2 <- prod_titled(prod_run(d, predictors = c("a", "b")), "Rows Excluded")
    expect_match(n2[[1]]$content, "4 further rows had a missing value of a selected predictor (a: 3, b: 1)", fixed = TRUE)
    expect_match(n2[[1]]$content, "could use up to 192 rows", fixed = TRUE)
    expect_length(prod_titled(prod_run(prod_data(), predictors = "a"), "Rows Excluded"), 0L)
})

test_that("P-04: a constant predictor is an ERROR, not a completed analysis with a weak AUC", {
    d <- prod_data()
    d$k <- 5
    a <- prod_run(d, predictors = "k")
    n <- prod_titled(a, "Constant Predictor: k")
    expect_length(n, 1L)
    expect_identical(n[[1]]$type, "ERROR")
    expect_length(prod_titled(a, "Limited Diagnostic Performance: k"), 0L)
    expect_length(prod_titled(a, "Analysis Complete"), 0L)
})

test_that("P-05: class-count and sample-size notices appear once per run, not once per predictor", {
    d <- prod_data()
    d <- d[c(which(d$y == "neg")[1:16], which(d$y == "pos")[1:8]), ]
    a <- prod_run(d, predictors = c("a", "b", "c"))
    expect_length(prod_titled(a, "Low Per-Class Count"), 1L)
    expect_length(prod_titled(a, "Small Sample Size"), 1L)
    expect_match(prod_titled(a, "Low Per-Class Count")[[1]]$content, "8 positive, 16 negative", fixed = TRUE)
})

test_that("P-06: notices are ordered by severity and name their severity in words", {
    d <- prod_data()
    d <- d[c(which(d$y == "neg")[1:40], which(d$y == "pos")[1:8]), ]
    d$a[1] <- NA                                       # INFO (Rows Excluded) raised before the ERROR
    a <- prod_run(d, predictors = "a")
    html <- a$results$results$notices$content
    expect_lt(regexpr("Low Per-Class Count", html, fixed = TRUE), regexpr("Rows Excluded", html, fixed = TRUE))
    expect_match(html, "Error: Low Per-Class Count", fixed = TRUE)
    expect_match(html, "Note: Rows Excluded", fixed = TRUE)
})

test_that("P-07: a STRONG_WARNING turns 'completed successfully' into 'finished with warnings'", {
    a <- prod_run(prod_data(), predictors = "c")           # AUC about 0.6: Limited Diagnostic Performance
    expect_length(prod_titled(a, "Limited Diagnostic Performance: c"), 1L)
    expect_length(prod_titled(a, "Analysis Complete"), 0L)
    expect_length(prod_titled(a, "Analysis Finished with Warnings"), 1L)
})

test_that("P-08: no warning is written into the instructions panel; the screening and prevalence notes are notices", {
    d <- prod_data(60)
    a <- prod_run(d, predictors = "a", clinicalContext = "screening")
    expect_length(prod_titled(a, "Screening Context Note"), 1L)
    a2 <- prod_run(d, predictors = "a", clinicalMetrics = TRUE, useObservedPrevalence = FALSE, prevalence = 0.1)
    for (x in list(a, a2)) {
        ins <- x$results$results$instructions$content
        expect_false(grepl("Clinical Assumptions", ins, fixed = TRUE))
        expect_false(grepl("Prevalence note", ins, fixed = TRUE))
        expect_false(grepl("Warnings:", ins, fixed = TRUE))
    }
    expect_length(prod_titled(a2, "User-Specified Prevalence"), 1L)
})

test_that("P-09: a blank or malformed optional number list warns instead of aborting the analysis (F-51)", {
    d <- prod_data()
    for (cut in c("", "abc", "0.5,")) {
        a <- prod_run(d, predictors = "a", clinicalImpact = TRUE, impactCutoffs = cut)
        expect_gt(nrow(a$results$results$aucSummary$asDF), 0L)
        expect_length(prod_titled(a, "Fixed Score Cutoffs Not Read"), 1L)
        expect_equal(a$results$results$clinicalImpactTable$rowCount, 0)
    }
    a <- prod_run(d, predictors = "a", clinicalImpact = TRUE, impactCutoffs = "0", prevalenceScenarios = "2")
    expect_length(prod_titled(a, "Additional Prevalences Not Read"), 1L)
    expect_gt(a$results$results$screeningWorkload$rowCount, 0L)
})

test_that("P-10: the cut-off rule stays visible with table notes off; the backwards-marker text uses the chosen level", {
    a <- prod_run(prod_data(), predictors = "a")
    note <- a$results$results$optimalCutoffSummary$.__enclos_env__$private$.notes[["rule"]]
    expect_false(is.null(note))
    expect_match(note$note, "at or above the cutoff", fixed = TRUE)
    d <- prod_data()
    d$r <- -d$a
    b <- prod_run(d[c(which(d$y == "neg")[1:12], which(d$y == "pos")[1:12]), ], predictors = "r", confidenceLevel = 90)
    n <- prod_titled(b, "Marker Reads Backwards: r")
    expect_length(n, 1L)
    expect_match(n[[1]]$content, "DeLong's interval is approximate", fixed = TRUE)
    expect_false(grepl("95%", n[[1]]$content, fixed = TRUE))
})

test_that("P-11: notices show the UI labels, not option keys", {
    d <- prod_data()
    a <- prod_run(d, predictors = c("a", "b"), pairwiseComparisons = TRUE)
    txt <- prod_titled(a, "Comparison Options Need Comparative Analysis")[[1]]$content
    expect_match(txt, "Single ROC Analysis", fixed = TRUE)
    expect_false(grepl("\"single\"", txt, fixed = TRUE))
    p <- prod_run(d, predictors = "a", clinicalPresets = "biomarker_screening")
    txt <- prod_titled(p, "Clinical Preset Overrode Your Settings")[[1]]$content
    expect_match(txt, "Biomarker Screening (High Sensitivity)", fixed = TRUE)
    expect_match(txt, "Custom Configuration", fixed = TRUE)
    expect_false(grepl("biomarker_screening", txt, fixed = TRUE))
})

test_that("P-12: comparison notices say what happened", {
    d <- prod_data()
    one <- prod_run(d, predictors = "a", analysisType = "comparative", pairwiseComparisons = TRUE)
    expect_match(prod_titled(one, "Insufficient Predictors")[[1]]$content, "1 predictor has ROC results", fixed = TRUE)
    d$a2 <- d$a + rnorm(nrow(d), 0, 1e-3)            # near-identical markers: fewer than 10 discordant pairs
    s <- prod_run(d, predictors = c("a", "a2"), analysisType = "comparative", showMetricsDiff = TRUE)
    txt <- prod_titled(s, "Some Comparison P-Values Could Not Be Computed")[[1]]$content
    expect_false(grepl("only the test failed", txt, fixed = TRUE))
})

test_that("P-13: plot themes differ on every plot and never paint a white panel", {
    d <- prod_data()
    theme_for <- function(choice) prod_run(d, predictors = "a", plotTheme = choice)$.__enclos_env__$private$.plotThemeFor()
    clinical <- theme_for("clinical")
    expect_s3_class(clinical$panel.grid.major, "element_line")
    expect_false(isTRUE(all.equal(clinical, theme_for("classic"))))
    expect_false(isTRUE(all.equal(clinical, theme_for("modern"))))
    src <- paste(readLines(test_path("..", "..", "R", "enhancedROC.b.R"), warn = FALSE), collapse = "\n")
    skip_if(!nzchar(src))
    expect_false(grepl('panel.background = ggplot2::element_rect(fill = "white")', src, fixed = TRUE))
})

test_that("P-14: a failed confidence band leaves the ROC curve drawn", {
    a <- prod_run(prod_data(), predictors = "a", showConfidenceBands = TRUE, bootstrapSamples = 200)
    image <- a$results$results$rocCurvePlot
    state <- image$state
    state$roc$a$bands <- NULL
    state$roc$a$band_error <- "simulated failure"
    image$setState(state)
    theme <- jmvcore:::getGlobalTheme("default", "jmv")
    f <- tempfile(fileext = ".png")
    grDevices::png(f)
    on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
    a$.__enclos_env__$private$.plotROCCurve(image, ggtheme = theme$ggtheme, theme = theme$theme)
    p <- ggplot2::last_plot()
    expect_identical(p$labels$title, "ROC Curve Analysis")
    expect_match(p$labels$caption, "simulated failure", fixed = TRUE)
})

test_that("P-15: schema wiring - bootstrap controls stay live, stratifiedBootstrap clears its outputs, every option is documented", {
    a_path <- test_path("..", "..", "jamovi", "enhancedROC.a.yaml")
    skip_if_not(file.exists(a_path))
    a <- yaml::read_yaml(a_path)
    r <- yaml::read_yaml(test_path("..", "..", "jamovi", "enhancedROC.r.yaml"))
    u <- yaml::read_yaml(test_path("..", "..", "jamovi", "enhancedROC.u.yaml"))
    undocumented <- vapply(a$options, function(o) !identical(o$name, "data") && is.null(o$description), logical(1))
    expect_identical(vapply(a$options[undocumented], `[[`, "", "name"), character(0))
    items <- list()
    walk <- function(x) for (it in x) { items[[it$name]] <<- it; if (!is.null(it$items)) walk(it$items) }
    walk(r$items)
    for (nm in c("rocCurvePlot", "rocComparisons", "detailedComparison", "statisticalSummary"))
        expect_true("stratifiedBootstrap" %in% unlist(items[[nm]]$clearWith), info = nm)
    controls <- list()
    walkc <- function(x) for (it in x) { if (!is.null(it$name)) controls[[it$name]] <<- it; if (!is.null(it$children)) walkc(it$children) }
    walkc(u$children)
    for (nm in c("bootstrapSamples", "stratifiedBootstrap", "plotTheme")) expect_null(controls[[nm]]$enable, info = nm)
    expect_match(controls$comparisonMethod$enable, "statisticalComparison", fixed = TRUE)
    expect_match(controls$bootstrapMethod$enable, "bootstrapPartialAUC", fixed = TRUE)
    expect_match(controls$prevalence$enable, "useObservedPrevalence", fixed = TRUE)
    # the additional prevalence scenarios use it even with observed prevalence (Codex review)
    expect_null(controls$workloadPopulationSize$enable)
})
