# Extracted from test-lassocox-release-fixes.R:119

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
skip_if_not_installed("ClinicoPath")
skip_if_not_installed("glmnet")
res <- run_lassocox(lassocox_testdata(), lambda = "lambda.min")
notes <- res$performance$notes
expect_true("apparent" %in% names(notes))
expect_match(notes[["apparent"]], "apparent \\(training\\) performance")
