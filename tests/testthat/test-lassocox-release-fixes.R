# Regression tests for the release-blocking defects fixed in lassocox.
#
# Each block corresponds to a defect that shipped, and each asserts against what
# lassocox() actually returned rather than re-deriving the answer in the test body.
# Two blocks in test-lassocox.R previously did the latter and so could never fail.

lassocox_testdata <- function(n = 220, p = 8, seed = 20260820) {
    set.seed(seed)
    X <- matrix(stats::rnorm(n * p), n, p)
    colnames(X) <- paste0("v", seq_len(p))
    # v1 and v2 both RAISE risk; nothing is protective. This is the configuration
    # that mislabelled every bar "Protective" in the coefficient plot.
    lp <- 0.9 * X[, 1] + 0.7 * X[, 2]
    time <- stats::rexp(n, rate = exp(lp))
    status <- stats::rbinom(n, 1, 0.75)
    df <- as.data.frame(X)
    df$time <- time
    df$status <- factor(ifelse(status == 1, "dead", "alive"), levels = c("alive", "dead"))
    df
}

run_lassocox <- function(df, ...) {
    ClinicoPath::lassocox(
        data        = df,
        elapsedtime = "time",
        outcome     = "status",
        outcomeLevel = "dead",
        censorLevel  = "alive",
        explanatory = paste0("v", 1:8),
        ...
    )
}


test_that("the model comparison table computes real numbers", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min", showModelComparison = TRUE)
    tab <- as.data.frame(res$modelComparison)

    # survival::concordance() rejects reverse= on a coxph object, deterministically, on
    # every dataset. That call was first inside its tryCatch, so AIC() and logLik() were
    # never reached either and all six numeric cells were permanently NA.
    expect_equal(nrow(tab), 2L)
    expect_true(all(is.finite(tab$cindex)),
                info = "C-index must be computed, not swallowed by the error handler")
    expect_true(all(is.finite(tab$aic)))
    expect_true(all(is.finite(tab$log_likelihood)))

    # and a C-index is a concordance, not its complement
    expect_true(all(tab$cindex > 0.5 & tab$cindex <= 1))
})


test_that("the results summary interpolates instead of printing placeholders", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min", showSummary = TRUE)
    txt <- as.character(res$summaryText$content)

    # jmvcore::format needs named dots AND camelCase tokens: a list() wrapper matches
    # nothing, and its token regex does not accept an underscore, so {n_obs} stayed
    # literal even once the wrapper was removed. Both were wrong here.
    expect_false(grepl("\\{", txt), info = "no unresolved {placeholder} tokens")
    expect_false(grepl("…", txt), info = "no ellipsis fallbacks")
    expect_match(txt, "LASSO Cox regression was performed on [0-9]+ observations")
})


test_that("the lambda rule reported is the rule that actually ran", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    # When the 1-SE rule retains nothing the backend silently refits at lambda.min.
    # Every summary surface used to keep reporting the requested rule.
    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.1se", showSummary = TRUE)
    summary_tab <- as.data.frame(res$modelSummary)

    expect_true("Penalty Selected By" %in% summary_tab$statistic)
    rule <- summary_tab$value[summary_tab$statistic == "Penalty Selected By"]
    expect_true(nzchar(rule))

    # If the fallback fired, the row must say so rather than claiming lambda.1se.
    if (grepl("lambda.min", rule)) {
        expect_match(rule, "retained no variables")
        expect_match(as.character(res$summaryText$content), "lambda.min was used instead")
    }
})


test_that("bootstrap-free results are reproducible at a fixed seed", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    df <- lassocox_testdata()
    a <- as.data.frame(run_lassocox(df, random_seed = 4242)$coefficients)
    b <- as.data.frame(run_lassocox(df, random_seed = 4242)$coefficients)

    # Cross-validation folds are random; without a fixed seed the selected variable set
    # changes between runs on the same data.
    expect_equal(a$variable, b$variable)
    expect_equal(a$coefficient, b$coefficient, tolerance = 0)
})


test_that("the apparent-performance caveat survives a missing C-index", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min")
    notes <- res$performance$notes

    # The caveat used to sit inside the C-index branch, so when concordance failed the
    # two most optimistic numbers on screen - the log-rank p and the group hazard ratio -
    # were left with nothing qualifying them.
    expect_true("apparent" %in% names(notes))
    expect_match(notes[["apparent"]], "apparent \\(training\\) performance")
    expect_match(notes[["apparent"]], "median split")
})


test_that("the selected-variables note names every column the refit touches", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min")
    notes <- res$coefficients$notes

    if ("refit" %in% names(notes)) {
        # Coefficient and Hazard Ratio come from the refit too, while Importance is the
        # absolute penalized coefficient - two estimators in adjacent cells of one row.
        for (col in c("Coefficient", "Hazard Ratio", "Importance"))
            expect_match(notes[["refit"]], col, fixed = TRUE)
    }
})

test_that("the risk-group survival plot renders a readable number-at-risk table at the declared image size", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survminer")

    # Regression: jamovi's 16-pt ggtheme was also applied to the risk table, whose
    # margins/titles consumed the whole 25% strip at 600x400 and left an empty table.
    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min", survival_plot = TRUE)
    img <- res$survival_plot
    expect_false(is.null(img$state))

    png_file <- tempfile(fileext = ".png")
    grDevices::png(png_file, width = 600, height = 400)
    on.exit(grDevices::dev.off(), add = TRUE)
    ok <- img$.render()
    expect_true(ok)

    # .survivalPlot prints and returns TRUE, so assert on the source: the risk table
    # must NOT inherit jamovi's 16-pt ggtheme (tables.theme defaults to ggtheme).
    src <- paste(deparse(ClinicoPath:::lassocoxClass$private_methods$.survivalPlot), collapse = "\n")
    expect_match(src, "tables.theme = survminer::theme_cleantable()", fixed = TRUE)
})
