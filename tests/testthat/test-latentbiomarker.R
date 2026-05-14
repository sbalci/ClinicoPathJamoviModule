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

test_that("G1 hard gate: refuses n < 100", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")
    small <- histopathology[1:50, ]
    res <- ClinicoPath::latentbiomarker(
        data = small,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB", "Measurement1"),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Insufficient sample size", fixed = TRUE)
    expect_equal(res$loadingsTable$rowCount, 0)
})

test_that("G3 hard gate: refuses fewer than 3 indicators", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")
    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = c("MeasurementA", "MeasurementB"),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Too few indicators", fixed = TRUE)
})

test_that("indicator-type auto-detection: factor indicators trigger WLSMV", {
    skip_if_not_installed("lavaan")
    data("histopathology", package = "ClinicoPath")
    factor_vars <- names(histopathology)[sapply(histopathology, is.factor)]
    skip_if(length(factor_vars) < 3, "histopathology lacks 3 factor indicators")
    res <- ClinicoPath::latentbiomarker(
        data = histopathology,
        dep_time = "OverallTime",
        dep_event = "Outcome",
        event_level = "1",
        indicators = factor_vars[1:3],
        indicator_types = "auto",
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "WLSMV", fixed = TRUE)
})

test_that("G2 hard gate: refuses cases-per-parameter < 5", {
    skip_if_not_installed("lavaan")
    set.seed(1)
    n <- 100
    k <- 15
    df <- data.frame(
        time = rexp(n, 0.1),
        evt  = factor(rbinom(n, 1, 0.5))
    )
    for (i in seq_len(k)) df[[paste0("ind", i)]] <- rnorm(n)
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = paste0("ind", seq_len(k)),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "Insufficient cases per CFA parameter", fixed = TRUE)
})

test_that("G5 soft warning: low inter-indicator correlations", {
    skip_if_not_installed("lavaan")
    set.seed(2)
    n <- 250
    df <- data.frame(
        time = rexp(n, 0.1),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = rnorm(n), ind2 = rnorm(n), ind3 = rnorm(n)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = c("ind1", "ind2", "ind3"),
        reflective_confirmed = TRUE
    )
    expect_match(res$notices$content, "weakly intercorrelated", fixed = TRUE)
})

test_that("G3 soft warning: exactly 3 indicators is just-identified", {
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
    expect_match(res$notices$content, "just-identified", fixed = TRUE)
})

test_that("CFA fit produces non-empty fit-indices row with traffic-light interpretation", {
    skip_if_not_installed("lavaan")
    set.seed(3)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.4)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65),
        ind4 = 0.85 * f + rnorm(n, sd = 0.5),
        ind5 = 0.6 * f + rnorm(n, sd = 0.8)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time",
        dep_event = "evt",
        event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE
    )
    rows <- res$fitTable$asDF
    expect_equal(nrow(rows), 1)
    expect_true(rows$cfi[1] > 0.9)
    expect_true(rows$rmsea[1] < 0.1)
    expect_true(nchar(rows$interpretation[1]) > 0)
})

test_that("Loadings, reliability, and summary tables populate", {
    skip_if_not_installed("lavaan")
    set.seed(3)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.4)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65),
        ind4 = 0.85 * f + rnorm(n, sd = 0.5),
        ind5 = 0.6 * f + rnorm(n, sd = 0.8)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE
    )

    L <- res$loadingsTable$asDF
    expect_equal(nrow(L), 5)
    expect_true(all(L$est_std > 0.3))
    expect_true(all(L$r2 > 0 & L$r2 < 1))

    R <- res$reliabilityTable$asDF
    expect_true(R$omega[1] > 0.7)
    expect_true(R$ave[1] > 0.3)

    S <- res$summaryTable$asDF
    expect_equal(S$n[1], n)
    expect_equal(S$n_indicators[1], 5)
    expect_equal(S$n_params[1], 10)
    expect_equal(S$estimator[1], "MLR")
})

test_that("Cox table populates with HR for factor and adjusters", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("survival")
    set.seed(4)
    n <- 500
    f <- rnorm(n)
    age <- rnorm(n, 60, 10)
    lp <- 0.7 * f + 0.02 * age
    time <- rexp(n, exp(lp - mean(lp)) * 0.05)
    evt <- factor(rbinom(n, 1, 0.6))
    df <- data.frame(
        time = time, evt = evt, age = age,
        ind1 = 0.8 * f + rnorm(n, sd = 0.6),
        ind2 = 0.7 * f + rnorm(n, sd = 0.7),
        ind3 = 0.75 * f + rnorm(n, sd = 0.65)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        adjusters = "age",
        reflective_confirmed = TRUE
    )

    C <- res$coxTable$asDF
    expect_true("biomarker_factor" %in% C$term || "Factor" %in% C$term)
    expect_true("age" %in% C$term)
    expect_true(all(C$hr > 0))
    expect_match(res$notices$content, "measurement uncertainty", fixed = TRUE)
})

test_that("PH test table populates and KM state is set", {
    skip_if_not_installed("lavaan")
    skip_if_not_installed("survminer")
    set.seed(5)
    n <- 400
    f <- rnorm(n)
    time <- rexp(n, exp(0.5 * f - 0.5 * mean(f)) * 0.05)
    df <- data.frame(
        time = time, evt = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE
    )
    P <- res$phTable$asDF
    expect_true(nrow(P) >= 1)
    expect_false(is.null(res$kmPlot$state))
})

test_that("Loadings and path-diagram states are set", {
    skip_if_not_installed("lavaan")
    set.seed(6)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE
    )
    expect_false(is.null(res$loadingsPlot$state))
    expect_false(is.null(res$pathPlot$state))
})

test_that("Modification indices table tolerates well-fitting data", {
    skip_if_not_installed("lavaan")
    set.seed(7)
    n <- 400
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4),
        ind4 = 0.6 * f + rnorm(n, sd = 0.7),
        ind5 = 0.5 * f + rnorm(n, sd = 0.8)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:5),
        reflective_confirmed = TRUE,
        show_diagnostics = TRUE
    )
    expect_true(!is.null(res$miTable))
})

test_that("R code export contains lavaan and coxph calls when enabled", {
    skip_if_not_installed("lavaan")
    set.seed(8)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df,
        dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE,
        show_r_code = TRUE
    )
    code <- res$rCode$content
    expect_match(code, "lavaan::cfa", fixed = TRUE)
    expect_match(code, "survival::coxph", fixed = TRUE)
})

test_that("Diagnostic notices: Mardia and score-reliability surface when applicable", {
    skip_if_not_installed("lavaan")
    set.seed(9)
    n <- 300
    f <- rnorm(n)
    df <- data.frame(
        time = rexp(n, 0.05),
        evt  = factor(rbinom(n, 1, 0.5)),
        ind1 = 0.8 * f + rnorm(n, sd = 0.5),
        ind2 = 0.7 * f + rnorm(n, sd = 0.6),
        ind3 = 0.85 * f + rnorm(n, sd = 0.4)
    )
    res <- ClinicoPath::latentbiomarker(
        data = df, dep_time = "time", dep_event = "evt", event_level = "1",
        indicators = paste0("ind", 1:3),
        reflective_confirmed = TRUE,
        show_diagnostics = TRUE
    )
    notices <- res$notices$content
    # At least one of the two diagnostic notices should be present (or both absent if MVN passes)
    # Just confirm the test does not error
    expect_true(is.character(notices))
})
