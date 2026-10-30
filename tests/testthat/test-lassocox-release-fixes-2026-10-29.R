# Regression tests for the 2026-10-29 lassocox release-check fixes.
# Runs against the umbrella (devtools::test) and an installed jsurvival
# (testthat::test_file(package = "jsurvival", load_package = "installed")):
# lassocoxOptions / lassocoxClass resolve from whichever namespace is under test.

lassocox_rf_run <- function(data, ...) {
    skip_if_not_installed("glmnet")
    opts <- lassocoxOptions$new(...)
    a <- lassocoxClass$new(options = opts, data = data)
    a$init()
    a$run()
    a
}

lassocox_rf_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))

test_that("PH check prints a small p as 'p < 0.001', never 'p = 0.000'", {
    # Crossing hazards for x1 (Weibull shapes 0.5 vs 3): the Schoenfeld test p is ~1e-28.
    set.seed(3); n <- 400
    x1 <- rbinom(n, 1, .5); x2 <- rnorm(n); x3 <- rnorm(n)
    t1 <- ifelse(x1 == 1, rweibull(n, shape = 0.5, scale = 1), rweibull(n, shape = 3, scale = 4)) * exp(-0.5 * x2)
    cen <- runif(n, 0, 8)
    sim <- data.frame(time = pmin(t1, cen), st = factor(ifelse(t1 <= cen, "D", "A")), x1 = x1, x2 = x2, x3 = x3)
    a <- lassocox_rf_run(sim, elapsedtime = "time", outcome = "st", outcomeLevel = "D", censorLevel = "A",
        explanatory = c("x1", "x2", "x3"), lambda = "lambda.min", random_seed = 42)
    html <- a$results$suitabilityReport$content
    expect_true(nzchar(html))
    expect_match(html, "Selected-refit test p &lt; 0.001", fixed = TRUE)  # escaped inside the HTML cell
    expect_false(grepl("p = 0.000", html, fixed = TRUE))
})

test_that("factor indicators are labelled 'variable: level (vs reference)'; numbers keyed by design column", {
    set.seed(7); n <- 300
    syn <- data.frame(check.names = FALSE,
        `Tumour grade` = factor(sample(c("G1", "G2", "G3"), n, TRUE), levels = c("G1", "G2", "G3")),
        stage = factor(sample(c("I", "II", "III"), n, TRUE), levels = c("I", "II", "III"), ordered = TRUE),
        age = rnorm(n, 60, 10), x2 = rnorm(n), x3 = rnorm(n))
    lp <- 0.9 * (syn[["Tumour grade"]] == "G3") + 0.5 * (syn[["Tumour grade"]] == "G2") +
        0.6 * (syn$stage == "III") + 0.03 * (syn$age - 60)
    tt <- rexp(n, 0.05 * exp(lp)); cc <- runif(n, 5, 40)
    syn$time <- pmin(tt, cc); syn$status <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    a <- lassocox_rf_run(syn, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
        censorLevel = "Alive", explanatory = c("Tumour grade", "stage", "age", "x2", "x3"),
        nfolds = 5, random_seed = 42, lambda = "lambda.min",
        showEncoding = TRUE, showVariableImportance = TRUE, showSummary = TRUE)

    coef_df <- a$results$coefficients$asDF
    expect_true("Tumour grade: G3 (vs G1)" %in% coef_df$variable)
    expect_true("stage: III (vs I)" %in% coef_df$variable)
    expect_true("age" %in% coef_df$variable)
    expect_false(any(c("Tumour gradeG3", "stageIII") %in% coef_df$variable))

    # The importance lookup is still keyed by the design-column name: no blank cells.
    expect_true(all(is.finite(coef_df$importance)))
    vi <- a$results$variableImportance$asDF
    expect_identical(vi$variable, coef_df$variable)

    # Rows still map one-to-one onto the selected design columns, in order.
    enc <- as.data.frame(a$results$encoding)
    sel <- enc[enc$selected == "Yes", ]
    expect_equal(nrow(sel), nrow(coef_df))
    expect_identical(sel$column[sel$variable == "age"], "age")

    summary_txt <- lassocox_rf_text(a$results$summaryText$content)
    expect_match(summary_txt, "Tumour grade: G3 (vs G1)", fixed = TRUE)
})

test_that("'standard Cox may suffice' is advice, not a yellow verdict", {
    # p = 8, EPV >= 20, uncorrelated, PH holds: the configuration that used to be the
    # only route into the regularization-yellow special case. That branch was already
    # unreachable when it was deleted, so this guards against its return; it cannot
    # fail on the build before the deletion.
    set.seed(11); n <- 420
    X <- matrix(rnorm(n * 8), n, 8, dimnames = list(NULL, paste0("v", 1:8)))
    d <- as.data.frame(X)
    tt <- rexp(n, 0.1 * exp(0.8 * d$v1)); cc <- rexp(n, 0.1)
    d$time <- pmin(tt, cc) + 0.01; d$status <- factor(ifelse(tt <= cc, "Dead", "Alive"))
    a <- lassocox_rf_run(d, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
        censorLevel = "Alive", explanatory = paste0("v", 1:8), lambda = "lambda.min", random_seed = 42)
    html <- a$results$suitabilityReport$content
    expect_true(nzchar(html))
    expect_false(grepl("standard Cox may also be appropriate", html, fixed = TRUE))
    expect_false(grepl("Use <strong>Multivariable Survival</strong> in ClinicoPath for standard Cox regression with fewer predictors", html, fixed = TRUE))
    expect_match(lassocox_rf_text(html), "Overall: No concern was identified by these limited checks", fixed = TRUE)
})
