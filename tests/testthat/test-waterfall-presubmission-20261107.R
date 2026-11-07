# Regression tests for the waterfall findings of the 2026-11-07 pre-submission review
# (oracles: hand RECIST v1.1 best response; binom.test for the printed rates).

wp <- function(d, ...) suppressWarnings(ClinicoPath::waterfall(data = d, ...))
wp_metric <- function(res, pattern) {
    m <- res$clinicalMetrics$asDF
    m$value[grepl(pattern, m$metric, fixed = TRUE)][1]
}
wp_notices <- function(res) paste(res$notices$content, collapse = " ")
wp_cats <- function(res, pid) {
    wf <- res$waterfallplot$state$data$waterfall
    stats::setNames(as.character(wf$recist_category), as.character(wf[[pid]]))
}

test_that("W1: values on a percent-of-baseline scale are flagged, also with a time variable", {
    # long data, no time = 0 row, 100 = no change
    d <- data.frame(id = rep(paste0("G", 1:5), each = 2), t = rep(c(6, 12), 5),
                    rel = c(60, 0, 65, 55, 90, 95, 110, 130, 70, 68))
    r <- wp(d, patientID = "id", responseVar = "rel", timeVar = "t")
    expect_match(wp_notices(r), "If 100 means no change", fixed = TRUE)
    # following the advice gives the hand oracle: CR, PR, SD, SD, PR -> ORR 3/5
    d$pct <- d$rel - 100
    r2 <- wp(d, patientID = "id", responseVar = "pct", timeVar = "t")
    expect_match(wp_metric(r2, "Objective Response Rate (CR+PR)"), "60.0%", fixed = TRUE)
    expect_false(grepl("If 100 means no change", wp_notices(r2), fixed = TRUE))
    # one row per patient, no time variable: the same notice, now a strong warning
    r3 <- wp(data.frame(id = paste0("F", 1:4), rel = c(0, 40, 90, 125)),
             patientID = "id", responseVar = "rel")
    expect_match(wp_notices(r3), "Strong warning: No value below zero", fixed = TRUE)
})

test_that("W1: genuine growth with explicit 0% baselines is not flagged", {
    d <- data.frame(id = rep(paste0("P", 1:4), each = 3), t = rep(c(0, 6, 12), 4),
                    pct = c(0, 25, 40, 0, 10, 30, 0, 5, 22, 0, 30, 50))
    r <- wp(d, patientID = "id", responseVar = "pct", timeVar = "t")
    expect_false(grepl("If 100 means no change", wp_notices(r), fixed = TRUE))
})

test_that("W2: per-visit supplied categories give the best response up to the first PD", {
    d <- data.frame(id = c(rep("P1", 4), rep("P2", 3), rep("P3", 3)),
                    t = c(0, 6, 12, 18, 0, 6, 12, 0, 6, 12),
                    y = c(100, 90, 60, 55, 100, 65, 85, 100, 95, 98),
                    tpr = c(NA, "SD", "PR", "PR", NA, "PR", "PD", NA, "SD", "SD"))
    r <- wp(d, patientID = "id", responseVar = "y", timeVar = "t", inputType = "raw",
            responseCategoryVar = "tpr")
    expect_equal(unname(wp_cats(r, "id")[c("P1", "P2", "P3")]), c("PR", "PR", "SD"))
    expect_match(wp_metric(r, "Objective Response Rate (CR+PR)"), "66.7%", fixed = TRUE)
    expect_match(wp_notices(r), "read as per-visit responses", fixed = TRUE)
    # PD first: a later PR does not count
    d2 <- data.frame(id = c("a", "a", "a", "b"), t = c(0, 6, 12, 0), y = c(100, 130, 60, 100),
                     tpr = c(NA, "PD", "PR", "SD"))
    r2 <- wp(d2, patientID = "id", responseVar = "y", timeVar = "t", inputType = "raw",
             responseCategoryVar = "tpr")
    expect_equal(unname(wp_cats(r2, "id")["a"]), "PD")
})

test_that("W3-W5: a supplied NE has no bar, is named, and the missing-value text is true", {
    d <- data.frame(id = paste0("Q", 1:6), pct = c(-50, -40, 10, 30, NA, -20),
                    cat = c(NA, NA, NA, NA, "PD", "NE"))
    r <- wp(d, patientID = "id", responseVar = "pct", responseCategoryVar = "cat", showMedian = TRUE)
    n <- wp_notices(r)
    expect_match(n, "reported as not evaluable: Q6", fixed = TRUE)
    expect_false(grepl("These will be excluded from analysis", n, fixed = TRUE))
    f <- tempfile(fileext = ".png")
    r$waterfallplot$saveAs(f)
    p <- ggplot2::last_plot()
    bars <- ggplot2::layer_data(p, 1)
    expect_equal(nrow(bars), 4)                      # Q1-Q4; not Q5 (no value) nor Q6 (NE)
    hl <- unlist(lapply(seq_along(p$layers), function(i) {
        ld <- ggplot2::layer_data(p, i)
        if ("yintercept" %in% names(ld)) ld$yintercept
    }))
    expect_true(-15 %in% hl)                         # median of -50, -40, 10, 30
    expect_false(-20 %in% hl)
})

test_that("W6: full category names are accepted as a supplied category", {
    d <- data.frame(id = paste0("C", 1:4), pct = c(-45, -10, 5, -35),
                    cat = c("Progressive Disease", "Partial response", NA, "complete response"))
    r <- wp(d, patientID = "id", responseVar = "pct", responseCategoryVar = "cat")
    expect_equal(unname(wp_cats(r, "id")[c("C1", "C2", "C4")]), c("PD", "PR", "CR"))
    expect_false(grepl("unrecognized label", wp_notices(r), fixed = TRUE))
})

test_that("W7: a negative size in a jamovi Nominal-Integer column reaches the validation text", {
    d <- data.frame(id = rep(c("a", "b", "c"), each = 2), t = rep(c(0, 6), 3),
                    y = c(50, -1, 40, 20, 30, 33))
    d$y <- factor(d$y); attr(d$y, "values") <- as.integer(levels(d$y))
    warns <- character(0)
    r <- withCallingHandlers(
        ClinicoPath::waterfall(data = d, patientID = "id", responseVar = "y", timeVar = "t",
                               inputType = "raw"),
        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
    expect_false(any(grepl("not meaningful for factors", warns, fixed = TRUE)))
    expect_match(wp_notices(r), "Some measurements are negative", fixed = TRUE)
})

test_that("W8: the >200% growth notice lists patients, not a printed data frame", {
    d <- data.frame(id = paste0("A", 1:4), pct = c(-40, 10, 250, 0))
    r <- wp(d, patientID = "id", responseVar = "pct")
    n <- wp_notices(r)
    expect_match(n, "A3 (250%)", fixed = TRUE)
    expect_false(grepl("id pct", n, fixed = TRUE))
})

test_that("Docs: a.yaml descriptions match the behavior", {
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "waterfall.a.yaml"))
    opt <- function(n) Filter(function(o) identical(o$name, n), a$options)[[1]]
    expect_match(opt("responseCategoryVar")$description$R, "per-visit responses", fixed = TRUE)
    expect_false(grepl("raw (per-visit) input", opt("showResponseDuration")$description$jamovi, fixed = TRUE))
    expect_false(grepl("misuse detection", opt("enableGuidedMode")$description$jamovi, fixed = TRUE))
    expect_match(opt("seed")$description$R, "Bootstrap CI of the median", fixed = TRUE)
})
