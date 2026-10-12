# Regressions from the independent re-audit of 2026-10-12.
# Checks public results, actual glmnet inputs, and reused native result objects.

ll84_data <- function(n = 240, seed = 401) {
  withr::with_seed(seed, {
    d <- data.frame(x1 = seq(.00001, .00040, length.out = n), x2 = rnorm(n))
    d$y <- rbinom(n, 1, plogis(-3 + 15000 * d$x1 + .3 * d$x2))
    d
  })
}
ll84_args <- function(...) utils::modifyList(list(
  outcome = "y", outcomeLevel = NULL, explanatory = c("x1", "x2"),
  lambda = "lambda.min", nfolds = 5, random_seed = 42,
  cv_plot = TRUE, coef_plot = TRUE, roc_plot = TRUE), list(...), keep.null = TRUE)
ll84_run <- function(d = ll84_data(), ...) {
  suppressWarnings(do.call(ClinicoPath::lassologistic, c(list(data = d), ll84_args(...))))
}
ll84_analysis <- function(d, ...) {
  opts <- do.call(ClinicoPath:::lassologisticOptions$new, ll84_args(...))
  a <- ClinicoPath:::lassologisticClass$new(options = opts, data = d)
  a$init(); a$postInit()
  a
}
ll84_p <- function(r) r$predictions$.__enclos_env__$private$.values[[1]]
ll84_key <- function(t, k) t$asDF[gsub('^"|"$', "", rownames(t$asDF)) == k, , drop = FALSE]

test_that("IR-01 printed scoring rules reproduce every award, including tiny cutoffs", {
  d <- ll84_data()
  r <- ll84_run(d, scoringSystem = TRUE, scoringMethod = "maxscaled")
  st <- r$scoringTable$asDF
  expect_true("x1" %in% st$variable)
  cut <- as.numeric(sub("^> ", "", st$criterion))
  raw_cut <- vapply(st$variable, function(v) median(d[[v]]), 0)
  expect_identical(cut, unname(raw_cut))
  published <- rowSums(vapply(seq_len(nrow(st)), function(i)
    (d[[st$variable[i]]] > cut[i]) * st$points[i], numeric(nrow(d))))
  expected <- rowSums(vapply(seq_len(nrow(st)), function(i)
    (d[[st$variable[i]]] > raw_cut[i]) * st$points[i], numeric(nrow(d))))
  expect_identical(published, expected)
  lookup <- r$lookupTable$asDF
  expect_equal(lookup$n_cases, as.integer(table(factor(published, levels = lookup$score))))
})

test_that("IR-01 a two-valued numeric criterion retains its exact scoring value", {
  d <- ll84_data()
  d$x1 <- ifelse(d$x1 > median(d$x1), .0002051, .0000101)
  r <- ll84_run(d, scoringSystem = TRUE)
  rule <- r$scoringTable$asDF$criterion[r$scoringTable$asDF$variable == "x1"]
  expect_match(rule, "^= ")
  expect_identical(as.numeric(sub("^= ", "", rule)), max(d$x1))
})

test_that("IR-03 CV and every bootstrap fit receive raw data with fold-local scaling", {
  d <- ll84_data(n = 120); d$x1 <- d$x1 * 1e4; d$x1[1] <- 30
  calls <- list(); original <- glmnet::cv.glmnet
  testthat::local_mocked_bindings(cv.glmnet = function(...) {
    a <- list(...); calls[[length(calls) + 1L]] <<- a
    do.call(original, a)
  }, .package = "glmnet")
  r <- ll84_run(d, bootstrapValidation = TRUE, bootstrapN = 50)
  expect_length(calls, 51)
  expect_equal(unname(calls[[1]]$x), unname(as.matrix(d[c("x1", "x2")])) )
  expect_true(all(vapply(calls, function(a) isTRUE(a$standardize), logical(1))))
  # Every resampled row is an untransformed original row; repeated copies stay in
  # one fold, so no held-out observation is also used to train that fold's model.
  for (a in calls[-1]) {
    original_row <- match(a$x[, "x2"], d$x2)
    expect_false(anyNA(original_row))
    expect_equal(unname(a$x), unname(as.matrix(d[original_row, c("x1", "x2")])))
    expect_true(all(vapply(split(a$foldid, original_row), function(f) length(unique(f)) == 1L, logical(1))))
  }
  expect_true(all(is.finite(r$validationTable$asDF$corrected)))
})

test_that("fold assignment refuses insufficient independent cases instead of random fallback", {
  a <- ll84_analysis(ll84_data())
  folds <- a$.__enclos_env__$private$.stratifiedFolds
  expect_error(folds(c(1, 1, 0, 0), 3), "Too few independent")
  y <- rep(c(0, 1), each = 15); ids <- rep(seq_len(10), each = 3)
  f <- withr::with_seed(42, folds(y, 5, ids))
  expect_true(all(vapply(split(f, ids), function(v) length(unique(v)) == 1, logical(1))))
  expect_true(all(table(y, f) > 0))
})

test_that("IR-04 the observation unit is explicit and repeated or missing IDs are rejected", {
  d <- ll84_data(); d$id <- seq_len(nrow(d))
  r <- ll84_run(d, patientID = "id", showSummary = TRUE)
  expect_match(r$summaryText$content, "240 independent observations", fixed = TRUE)
  expect_match(r$notices$content, "Patient IDs are unique", fixed = TRUE)
  expect_match(ll84_run(d)$notices$content, "Each analyzed row", fixed = TRUE)
  d$id[2] <- d$id[1]
  expect_error(ll84_run(d, patientID = "id"), "Repeated patient IDs")
  d$id[2] <- NA
  expect_error(ll84_run(d, patientID = "id"), "Patient IDs are missing")
  d$id <- 2^52 + seq_len(nrow(d))
  expect_no_error(ll84_run(d, patientID = "id"))
  expect_error(ll84_run(d, patientID = "x1"), "separate identifier")
})

test_that("IR-05 colliding design names preserve numeric-source cuts and factor status", {
  d <- withr::with_seed(14, {
    x <- data.frame(Tumor = factor(sample(c("Low", "High"), 400, TRUE),
                                   levels = c("High", "Low")), TumorLow = rnorm(400))
    x$y <- rbinom(400, 1, plogis(-.5 + 1.5 * (x$Tumor == "Low") + 1.2 * x$TumorLow))
    x
  })
  for (vars in list(c("Tumor", "TumorLow"), c("TumorLow", "Tumor"))) {
    a <- ll84_analysis(d, explanatory = vars, scoringSystem = TRUE,
                        scoreCutMethod = "manual", scoreCutPoints = "TumorLow=1")
    a$run(); pr <- a$.__enclos_env__$private
    clean <- pr$.cleanData()
    numeric_name <- names(clean$column_source)[clean$column_source == "TumorLow"]
    factor_name <- names(clean$column_source)[clean$column_source == "Tumor"]
    st <- a$results$scoringTable$asDF
    expect_equal(st$criterion[st$variable == numeric_name], "> 1")
    expect_equal(st$criterion[st$variable == factor_name], "present")
    note <- a$results$scoringTable$notes$source_names$note
    expect_match(note, "numeric predictor TumorLow", fixed = TRUE)
    expect_match(note, "Tumor = Low", fixed = TRUE)
  }
})

test_that("IR-06 penalty labels and coefficient plot scales match the selected model", {
  d <- ll84_data()
  for (penalty in c("lasso", "ridge", "elasticnet")) {
    r <- ll84_run(d, penalty = penalty, showModelComparison = TRUE)
    want <- switch(penalty, lasso = "LASSO", ridge = "Ridge", elasticnet = "Elastic Net")
    expect_match(ll84_key(r$modelComparison, "1")$model_type, want, fixed = TRUE)
    expect_match(r$coef_plot$state$penalty, want, fixed = TRUE)
    expect_match(r$cv_plot$state$penalty, want, fixed = TRUE)
    cf <- r$coefficients$asDF; state <- r$coef_plot$state
    b <- cf$coefficient[match(state$var_names, cf$variable)]
    expect_equal(state$coef_values, unname(b * vapply(d[state$var_names], sd, 0)), tolerance = 1e-12)
  }
})

test_that("IR-07 restored methodology and plots clear when the event is unset", {
  d <- ll84_data(); d$y <- factor(d$y)
  a <- ll84_analysis(d, outcomeLevel = "1", showMethodologyNotes = TRUE, scoringSystem = TRUE)
  a$run()
  b <- ll84_analysis(d, outcomeLevel = "", showMethodologyNotes = TRUE, scoringSystem = TRUE)
  b$results$methodologyNotes$fromProtoBuf(list(stale = FALSE, state = raw(),
    html = list(content = a$results$methodologyNotes$content,
                scripts = list(), stylesheets = list())),
    oChanges = "outcomeLevel", vChanges = character(0))
  b$results$roc_plot$setState(a$results$roc_plot$state)
  b$run()
  fresh <- ll84_analysis(d, outcomeLevel = "", showMethodologyNotes = TRUE, scoringSystem = TRUE)
  fresh$run()
  expect_identical(b$results$methodologyNotes$content, fresh$results$methodologyNotes$content)
  expect_identical(b$results$methodologyNotes$content, "")
  expect_null(b$results$roc_plot$state)
  deps <- b$results$methodologyNotes$.__enclos_env__$private$.clearWith
  expect_true(all(c("outcomeLevel", "patientID", "alpha", "nfolds", "random_seed") %in% deps))
})

test_that("exact null optimum yields the event rate and a count-defined F1 of zero", {
  x <- expand.grid(x1 = -1:1, x2 = 0:1)
  d <- x[rep(seq_len(6), 30), ]; d$y <- rep(c(rep(0, 12), rep(1, 6)), 10)
  for (penalty in c("lasso", "ridge", "elasticnet")) {
    r <- ll84_run(d, penalty = penalty, scoringSystem = TRUE, showVariableImportance = TRUE)
    expect_equal(ll84_p(r), rep(1 / 3, nrow(d)), tolerance = 1e-12)
    expect_equal(ll84_key(r$performance, "f1")$value, "0.000")
    expect_true(is.na(ll84_key(r$performance, "precision")$value))
    expect_match(r$notices$content, "Intercept-only optimum", fixed = TRUE)
    expect_equal(nrow(r$scoringTable$asDF), 0L)
    expect_false(any(is.nan(r$coefficients$asDF$importance)))
  }
})

test_that("saved ROC state contains only drawing coordinates and supports old states", {
  d <- ll84_data(); r <- ll84_run(d)
  s <- r$roc_plot$state
  expect_setequal(names(s), c("schema", "specificity", "sensitivity", "auc", "ci"))
  expect_equal(s$schema, 2L)
  # Mann-Whitney calculation independent of pROC.
  p <- ll84_p(r)
  auc <- mean(outer(p[d$y == 1], p[d$y == 0], ">") + .5 * outer(p[d$y == 1], p[d$y == 0], "=="))
  expect_equal(s$auc, auc, tolerance = 1e-12)
  a <- ll84_analysis(d); a$.__enclos_env__$private$.data <- NULL
  for (state in list(s, list(y = d$y, probabilities = p))) {
    a$results$roc_plot$setState(state)
    grDevices::pdf(NULL)
    expect_true(a$.createImage(".rocPlot", a$results$roc_plot))
    grDevices::dev.off()
  }
})
