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

lassocox_source_file <- function(...) {
    relative <- file.path(...)
    candidates <- c(
        relative,
        file.path("..", "..", relative),
        system.file(..., package = "ClinicoPath")
    )
    hit <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(hit) == 0L)
        skip(paste("Source artifact not available:", relative))
    hit[[1L]]
}

collect_lassocox_calls <- function(expr) {
    calls <- list()
    walk <- function(node) {
        if (!is.call(node))
            return(invisible(NULL))
        if (identical(node[[1L]], as.name("lassocox")))
            calls[[length(calls) + 1L]] <<- node
        for (part in as.list(node)[-1L])
            walk(part)
        invisible(NULL)
    }
    for (node in expr)
        walk(node)
    calls
}


test_that("the bundled breast-cancer dataset exposes its documented object name", {
    path <- lassocox_source_file("data", "lassocox_breast_cancer.rda")
    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)

    expect_identical(loaded, "lassocox_breast_cancer")
    expect_s3_class(env$lassocox_breast_cancer, "data.frame")
    expect_identical(levels(env$lassocox_breast_cancer$death), c("Alive", "Dead"))
})


test_that("every shipped R example specifies event and censor levels", {
    path <- lassocox_source_file("inst", "examples", "lassocox_example.R")
    calls <- collect_lassocox_calls(parse(path))

    expect_gt(length(calls), 0L)
    argument_names <- lapply(calls, function(call) names(as.list(call))[-1L])
    expect_true(all(vapply(argument_names, function(x) {
        all(c("outcomeLevel", "censorLevel") %in% x)
    }, logical(1))))
})


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

    # The requested rule is preserved, including when it selects an empty model.
    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.1se", showSummary = TRUE)
    summary_tab <- as.data.frame(res$modelSummary)

    expect_true("Penalty Selected By" %in% summary_tab$statistic)
    rule <- summary_tab$value[summary_tab$statistic == "Penalty Selected By"]
    expect_equal(rule, "lambda.1se")
    expect_false(grepl("lambda.min was used instead", as.character(res$summaryText$content), fixed = TRUE))
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


test_that("the apparent-performance caveat explains tuning and selection", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min")
    notes <- res$performance$notes

    expect_true("apparent" %in% names(notes))
    expect_match(notes[["apparent"]]$note, "apparent \\(training\\) performance")
    expect_match(notes[["apparent"]]$note, "preprocessing, penalty selection, and model fitting")
})


test_that("the coefficient table identifies the penalized estimator", {
    skip_if_not_installed("ClinicoPath")
    skip_if_not_installed("glmnet")

    res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min")
    notes <- res$coefficients$notes

    expect_true("penalized" %in% names(notes))
    expect_match(notes[["penalized"]]$note, "from the penalized LASSO Cox fit", fixed = TRUE)
    expect_match(notes[["penalized"]]$note, "intentionally not reported", fixed = TRUE)
    tab <- as.data.frame(res$coefficients)
    expect_equal(tab$hazardRatio, exp(tab$coefficient))
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
    on.exit(unlink(png_file), add = TRUE)
    grDevices::png(png_file, width = 600, height = 400)
    on.exit(grDevices::dev.off(), add = TRUE)
    ok <- img$.render()
    expect_true(ok)

    # Inspect rendered content; layout geometry is covered in the audit regressions.
    grid::grid.force()
    grobs <- grid::grid.ls(print = FALSE)$name
    labels <- unlist(lapply(grobs[grepl("text", grobs)], function(name) {
        item <- grid::grid.get(name)
        if (inherits(item, "text")) as.character(item$label) else character()
    }))
    expect_true(all(c("Number at risk", "110") %in% labels))
})
