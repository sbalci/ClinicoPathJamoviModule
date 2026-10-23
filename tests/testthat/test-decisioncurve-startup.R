# Isolate this backend so the umbrella's imports cannot hide eager namespace loading.
dca_startup_env <- function() {
    e <- new.env(parent = asNamespace("jmvcore"))
    root <- if (file.exists("R/decisioncurve.b.R")) "." else "../.."
    for (x in parse(file.path(root, "R/utils.R"))) {
        if (is.call(x) && identical(x[[1]], as.name("<-")) &&
            as.character(x[[2]]) %in% c(".rocLabel", ".fmt")) eval(x, e)
    }
    sys.source(file.path(root, "R/decisioncurve.h.R"), e)
    sys.source(file.path(root, "R/decisioncurve.b.R"), e)
    e
}

test_that("empty and pending decisioncurve runs do not request analysis dependencies", {
    e <- dca_startup_env()
    requested <- character()
    e$requireNamespace <- function(package, ...) {
        requested <<- c(requested, package)
        base::requireNamespace(package, ...)
    }
    d <- data.frame(y = factor(c("no", "yes")), m = c(.1, .9))
    for (configured in c(FALSE, TRUE)) {
        o <- if (configured) e$decisioncurveOptions$new(outcome = "y", models = "m")
             else e$decisioncurveOptions$new()
        a <- e$decisioncurveClass$new(options = o, data = if (configured) d else data.frame())
        expect_true(a$run())
        expect_null(a$results$dcaPlot$state)
        if (configured)
            expect_match(a$results$notices$content, "Choose the Positive Outcome Level", fixed = TRUE)
        else
            expect_match(a$results$instructions$content, "To get started", fixed = TRUE)
    }
    expect_length(requested, 0L)
})

test_that("comparison labels are only translated for the visible comparison table", {
    e <- dca_startup_env()
    original_fmt <- e$.fmt
    pair_labels <- 0L
    e$.fmt <- function(.format_string, ...) {
        if (identical(.format_string, "{a} vs {b}")) pair_labels <<- pair_labels + 1L
        original_fmt(.format_string, ...)
    }
    d <- data.frame(y = factor(c("no", "yes")), m1 = c(.1, .9),
                    m2 = c(.2, .8), m3 = c(.3, .7))
    for (mode in c("off", "enhanced", "standard", "both")) {
        pair_labels <- 0L
        o <- e$decisioncurveOptions$new(outcome = "y", outcomePositive = "yes",
            models = c("m1", "m2", "m3"), compareModels = mode %in% c("standard", "both"),
            multiModelComparison = mode %in% c("enhanced", "both"))
        a <- e$decisioncurveClass$new(options = o, data = d)
        a$init()
        expect_equal(pair_labels, if (mode %in% c("standard", "both")) 3L else 0L,
                     info = mode)
        expect_equal(a$results$comparisonTable$rowCount,
                     if (mode %in% c("standard", "both")) 3L else 0L)
        expect_equal(a$results$modelComparisonEnhanced$rowCount,
                     if (mode %in% c("enhanced", "both")) 3L else 0L)
    }
})

test_that("configured table-only runs defer plot dependencies", {
    e <- dca_startup_env()
    requested <- character()
    e$requireNamespace <- function(package, ...) {
        requested <<- c(requested, package)
        base::requireNamespace(package, ...)
    }
    d <- data.frame(y = factor(rep(c("no", "yes"), c(60, 40))),
                    m = seq(.01, .99, length.out = 100))
    for (mode in c("table", "curve", "impact")) {
        requested <- character()
        o <- e$decisioncurveOptions$new(outcome = "y", outcomePositive = "yes", models = "m",
            showPlot = mode == "curve", showClinicalImpactPlot = mode == "impact")
        a <- e$decisioncurveClass$new(options = o, data = d)
        expect_true(a$run())
        expect_true(all(is.finite(a$results$resultsTable$asDF$model_m)))
        expected <- c("dplyr", if (mode != "table") "ggplot2", if (mode == "impact") "tidyr")
        expect_setequal(requested, expected)
    }
})
