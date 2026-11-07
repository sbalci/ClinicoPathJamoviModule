# Regressions from the 2026-10-30 independent review of lassocox.
#
# F1: glmnet 5.0 (coxdev) caps exp(eta) at eta = 30 inside the Cox deviance while the linear term
# keeps the full eta. Once a fold's eta passes 30 the cross-validation curve is wrong, then negative,
# and lambda came from it: real signal was reported as an empty model, or every column was kept.
# F2-F4: disclosures for standardize = FALSE, the fold-dependent selection, and the path caption.

lassocox_rr_run <- function(d, ...) {
    skip_if_not_installed("glmnet")
    skip_if_not_installed("survival")
    a <- ClinicoPath:::lassocoxClass$new(options = ClinicoPath:::lassocoxOptions$new(
        elapsedtime = "time", outcome = "ev", outcomeLevel = NULL, censorLevel = NULL,
        explanatory = setdiff(names(d), c("time", "ev")), suitabilityCheck = FALSE,
        coef_plot = FALSE, survival_plot = FALSE, ...), data = d)
    a$init(); suppressWarnings(a$run())
    a
}
# Three true columns among p, about 47 events; n and p put the columns near the event count.
lassocox_rr_data <- function(seed, n = 100, p = 55) {
    set.seed(555000 + seed)
    X <- matrix(rnorm(n * p), n, dimnames = list(NULL, paste0("x", seq_len(p))))
    tt <- rexp(n, .1 * exp(.7 * X[, 1] - .6 * X[, 2] + .5 * X[, 3])); cc <- runif(n, 1, 11)
    data.frame(time = pmin(tt, cc), ev = as.integer(tt <= cc), X)
}
lassocox_rr_folds <- function(status, k, seed) {
    set.seed(seed); f <- integer(length(status)); e <- which(status == 1); c <- which(status == 0)
    f[e] <- sample(rep(seq_len(k), length.out = length(e)))
    f[c] <- sample(rep(seq_len(k), length.out = length(c))); f
}
lassocox_rr_notes <- function(a) paste(vapply(a$results$coefficients$notes, function(n) n$note, ""), collapse = " ")
lassocox_rr_repro <- function(a, item) {
    x <- a$results$reproducibility$asDF
    x$value[x$item == item]
}

test_that("impossible CV curve no longer empties the default model (seed 8) and the stop is counted", {
    skip_if_not(utils::packageVersion("glmnet") >= "5.0")
    a <- lassocox_rr_run(lassocox_rr_data(8), random_seed = 8, cv_plot = TRUE, showReproducibility = TRUE,
        showSummary = TRUE)
    st <- a$results$cv_plot$state
    expect_gte(min(st$cvm), 0)
    expect_false(identical(a$results$coefficients$asDF$variable[1], "No variables selected"))

    used <- lassocox_rr_repro(a, "Lambda values used / searched")
    counts <- as.integer(strsplit(used, " / ", fixed = TRUE)[[1]])
    expect_equal(counts[1], length(st$lambda))
    expect_lt(counts[1], counts[2])

    sentence <- sprintf("stopped after %d of %d lambda values", counts[1], counts[2])
    expect_match(a$results$modelSummary$notes$lambdaStop$note, sentence, fixed = TRUE)
    expect_match(a$results$todo$content, sentence, fixed = TRUE)
    expect_match(a$results$summaryText$content, sentence, fixed = TRUE)
})

test_that("impossible CV curve no longer keeps every column (seed 48)", {
    skip_if_not(utils::packageVersion("glmnet") >= "5.0")
    a <- lassocox_rr_run(lassocox_rr_data(48), random_seed = 48, cv_plot = TRUE)
    expect_gte(min(a$results$cv_plot$state$cvm), 0)
    expect_lt(nrow(a$results$coefficients$asDF), 25)
})

test_that("the kept CV curve equals a hand partial-likelihood curve on the same folds", {
    skip_if_not(utils::packageVersion("glmnet") >= "5.0")
    d <- lassocox_rr_data(8)
    a <- lassocox_rr_run(d, random_seed = 8, cv_plot = TRUE)
    L <- a$results$cv_plot$state$lambda
    X <- as.matrix(d[, -(1:2)]); X <- sweep(X, 2, colMeans(X), "-")
    y <- survival::Surv(d$time, d$ev); f <- lassocox_rr_folds(d$ev, 10, 8)
    pl <- function(eta, time, status) {  # no ties in this fixture
        o <- order(-time)
        sum((eta[o] - log(cumsum(exp(eta[o]))))[status[o] == 1])
    }
    hand <- rep(0, length(L))
    for (k in 1:10) {
        i <- f != k
        b <- as.matrix(glmnet::glmnet(X[i, ], y[i, ], family = "cox", cox.ties = "breslow", lambda = L)$beta)
        for (j in seq_along(L)) {
            e <- drop(X %*% b[, j])
            hand[j] <- hand[j] - 2 * (pl(e, d$time, d$ev) - pl(e[i], d$time[i], d$ev[i]))
        }
    }
    expect_equal(a$results$cv_plot$state$cvm, hand / nrow(d), tolerance = 1e-6)
})

test_that("generated R code stops at the same lambda and reproduces the saved score", {
    skip_if_not(utils::packageVersion("glmnet") >= "5.0")
    d <- lassocox_rr_data(8)
    a <- lassocox_rr_run(d, random_seed = 8, showRCode = TRUE)
    code <- gsub("<pre[^>]*><code>|</code></pre>", "", a$results$rCode$content)
    for (p in list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", "\""), c("&#39;", "'"), c("&amp;", "&")))
        code <- gsub(p[1], p[2], code, fixed = TRUE)
    e <- new.env(parent = globalenv()); e$data <- d
    suppressWarnings(eval(parse(text = code), envir = e))
    score <- a$results$riskScore$.__enclos_env__$private$.values[[1]]
    expect_equal(e$lassocox_fit$risk_scores, score, tolerance = 1e-10)
    expect_lt(length(e$lassocox_fit$cv_fit$lambda), 100)
})

test_that("a stopped path keeps the empty model at its first lambda", {
    skip_if_not(utils::packageVersion("glmnet") >= "5.0")
    # n = 40, p = 30, seed 7001: lambda.1se is the first lambda. Refitting that lambda as supplied
    # used to let a column in with a coefficient of machine-rounding size.
    set.seed(7001); n <- 40; p <- 30
    X <- matrix(rnorm(n * p), n, dimnames = list(NULL, paste0("x", seq_len(p))))
    tt <- rexp(n, .1 * exp(.7 * X[, 1] - .6 * X[, 7] + .5 * X[, 13])); cc <- runif(n, 1, 11)
    a <- lassocox_rr_run(data.frame(time = pmin(tt, cc), ev = as.integer(tt <= cc), X), random_seed = 1)
    expect_false(is.null(a$results$modelSummary$notes$lambdaStop))
    expect_identical(a$results$coefficients$asDF$variable, "No variables selected")
})

test_that("a healthy path is untouched: no stop note and the same lambda as plain cv.glmnet", {
    d <- lassocox_rr_data(3, n = 200, p = 8)
    a <- lassocox_rr_run(d, random_seed = 5, showReproducibility = TRUE)
    expect_null(a$results$modelSummary$notes$lambdaStop)
    expect_false(grepl("lambda values", a$results$todo$content, fixed = TRUE))

    X <- as.matrix(d[, -(1:2)]); X <- sweep(X, 2, colMeans(X), "-")
    cv <- glmnet::cv.glmnet(X, survival::Surv(d$time, d$ev), family = "cox", cox.ties = "breslow",
        foldid = lassocox_rr_folds(d$ev, 10, 5))
    used <- as.integer(strsplit(lassocox_rr_repro(a, "Lambda values used / searched"), " / ", fixed = TRUE)[[1]])
    expect_equal(used, rep(length(cv$lambda), 2))
    expect_equal(as.numeric(lassocox_rr_repro(a, "Selected lambda")), cv$lambda.1se)
})

test_that("standardize = FALSE says that units drive selection, and only then", {
    d <- lassocox_rr_data(3, n = 200, p = 8)
    sentence <- "Selection depends on the units of each predictor"
    off <- lassocox_rr_run(d, standardize = FALSE, showSummary = TRUE)
    expect_match(lassocox_rr_notes(off), sentence, fixed = TRUE)
    expect_match(off$results$summaryText$content, sentence, fixed = TRUE)
    on <- lassocox_rr_run(d, showSummary = TRUE)
    expect_false(grepl(sentence, lassocox_rr_notes(on), fixed = TRUE))
    expect_false(grepl(sentence, on$results$summaryText$content, fixed = TRUE))
})

test_that("the output says that the selected set depends on the cross-validation split", {
    d <- lassocox_rr_data(3, n = 200, p = 8)
    a <- lassocox_rr_run(d, showSummary = TRUE)
    sentence <- "depend on the cross-validation split"
    expect_match(lassocox_rr_notes(a), sentence, fixed = TRUE)
    expect_match(a$results$summaryText$content, sentence, fixed = TRUE)
})

test_that("the path caption states the ranking the code applies", {
    src <- paste(readLines(test_path("../../R/lassocox.b.R"), warn = FALSE), collapse = "\n")
    expect_match(src, "selected columns first, then by summed absolute coefficient times column SD", fixed = TRUE)
    expect_false(grepl("ranked by summed absolute coefficients", src, fixed = TRUE))
})
