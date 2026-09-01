# Exercise the backend and independently execute its upstream R export.
lassocox_safety_data <- function() {
  withr::local_seed(110)
  d <- data.frame(x = rnorm(200), z = rnorm(200))
  event_time <- rexp(200, rate = exp(d$x))
  censor_time <- rexp(200, rate = 0.3)
  d$time <- pmin(event_time, censor_time)
  d$status <- as.integer(event_time <= censor_time)
  d
}

lassocox_safety_backend <- function(data, ...) {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("jmvcore")
  args <- utils::modifyList(list(elapsedtime = "time", outcome = "status",
    outcomeLevel = "1", censorLevel = "0", explanatory = c("x", "z"),
    lambda = "lambda.min", cv_plot = FALSE, coef_plot = FALSE,
    survival_plot = FALSE, suitabilityCheck = FALSE), list(...))
  options_class <- getFromNamespace("lassocoxOptions", "ClinicoPath")
  backend_class <- getFromNamespace("lassocoxClass", "ClinicoPath")
  backend_class$new(options = do.call(options_class$new, args), data = data)
}

test_that("time and outcome roles cannot leak into the predictors", {
  d <- lassocox_safety_data()
  for (predictors in list(c("x", "time"), c("x", "status"))) {
    a <- lassocox_safety_backend(d, explanatory = predictors)
    expect_error(a$.__enclos_env__$private$.cleanData(), "cannot also be predictors")
  }
  a <- lassocox_safety_backend(d, elapsedtime = "status")
  expect_error(a$.__enclos_env__$private$.cleanData(), "must be different")
})

test_that("invalid predictors and nonpositive times fail before fitting", {
  d <- lassocox_safety_data()
  for (value in c(Inf, -Inf)) {
    bad <- d
    bad$x[1] <- value
    a <- lassocox_safety_backend(bad)
    expect_error(a$.__enclos_env__$private$.cleanData(), "infinite values")
  }
  bad <- d
  bad$x <- NA_real_
  a <- lassocox_safety_backend(bad)
  expect_error(a$.__enclos_env__$private$.cleanData(), "only missing values")
  for (value in c(0, -1)) {
    bad <- d
    bad$time[1] <- value
    a <- lassocox_safety_backend(bad)
    expect_error(a$.__enclos_env__$private$.cleanData(), "strictly positive")
  }
})

test_that("ordered factors have explicit treatment coding and exact origins", {
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  d <- lassocox_safety_data()
  d$grade <- ordered(rep(c("G1", "G2", "G3"), length.out = nrow(d)))
  d$gradeG2 <- rnorm(nrow(d))
  a <- lassocox_safety_backend(d, explanatory = c("x", "grade", "gradeG2"))
  clean <- suppressWarnings(a$.__enclos_env__$private$.cleanData())
  idx <- which(clean$encoding$variable == "grade")
  expect_equal(unname(clean$X[, idx]),
    unname(cbind(as.numeric(d$grade == "G2"), as.numeric(d$grade == "G3")) ))
  expect_equal(clean$encoding$reference[idx], rep("G1", 2))
  expect_equal(clean$encoding$level[idx], c("G2", "G3"))
  expect_equal(clean$encoding$coding[idx], rep("ordered_treatment", 2))
  expect_equal(clean$encoding$variable[4], "gradeG2")
  expect_equal(anyDuplicated(colnames(clean$X)), 0L)
})

test_that("exported upstream code reproduces mixed-factor fitting and missing rows", {
  d <- lassocox_safety_data()
  name <- "grade ` unusual \" <tag>"
  d[[name]] <- ordered(rep(c("low", "medium", "high"), length.out = nrow(d)),
    levels = c("low", "medium", "high"))
  d$x[2] <- NA_real_
  d$status[3] <- NA_integer_
  d$time[4] <- NA_real_
  d$constant <- 7
  for (standardize in c(TRUE, FALSE)) {
    a <- lassocox_safety_backend(d, explanatory = c("x", "z", name, "constant"),
      standardize = standardize, showEncoding = TRUE, showReproducibility = TRUE,
      showRCode = TRUE, path_plot = TRUE, showSummary = TRUE)
    pr <- a$.__enclos_env__$private
    clean <- suppressWarnings(pr$.cleanData())
    withr::local_seed(1234)
    seed <- .Random.seed
    fit <- suppressWarnings(pr$.fitModel(clean))
    expect_identical(.Random.seed, seed)
    expect_equal(clean$complete_cases, setdiff(seq_len(nrow(d)), 2:4))
    expect_equal(clean$removed_constants, "constant")
    pr$.init()
    pr$.populateModelSummary(fit)
    pr$.populateReproducibility(fit)
    pr$.savePlotData(fit)
    pr$.generateRCode(fit)
    code <- pr$.buildRCode(fit)
    exported <- new.env(parent = globalenv())
    exported$data <- d
    expect_no_error(suppressWarnings(eval(parse(text = code), envir = exported)))
    reproduced <- exported$lassocox_fit
    expect_identical(.Random.seed, seed)
    expect_equal(reproduced$lambda, fit$lambda_optimal, tolerance = 0)
    expect_equal(unname(reproduced$coefficients), unname(fit$coef_matrix), tolerance = 1e-12)
    expect_equal(reproduced$risk_scores[clean$complete_cases], fit$risk_scores, tolerance = 1e-12)
    expect_true(all(is.na(reproduced$risk_scores[2:4])))
    expect_equal(reproduced$foldid, fit$foldid)
    expect_equal(reproduced$apparent_c, fit$performance_metrics$cindex)
    expect_match(a$results$rCode$content, "&lt;tag&gt;", fixed = TRUE)
    encoding <- as.data.frame(a$results$encoding)
    expect_equal(encoding$column, clean$encoding$column)
    expect_equal(sum(encoding$selected == "Yes"), length(fit$selected_vars))
    summary <- as.data.frame(a$results$modelSummary)
    expect_equal(summary$value[summary$statistic == "Original Candidate Predictors"], "4")
    path <- a$results$path_plot$state
    expect_equal(nrow(path$paths), ncol(clean$X) * length(path$lambda))
    expect_equal(path$nzero, as.integer(fit$cv_fit$glmnet.fit$df))
    expect_no_error(serialize(path, NULL))
    pr$.clearAnalysisOutputs()
    expect_null(a$results$path_plot$state)
    expect_equal(a$results$encoding$rowCount, 0L)
    expect_equal(a$results$rCode$content, "")
    expect_equal(a$results$summaryText$content, "")
    expect_false(pr$.pathPlot(a$results$path_plot, ggplot2::theme_minimal(), NULL))
  }
})

test_that("the final coefficients are the selected CV path, not a separate refit", {
  a <- lassocox_safety_backend(lassocox_safety_data())
  pr <- a$.__enclos_env__$private
  fit <- pr$.fitModel(pr$.cleanData())
  reference <- as.matrix(stats::coef(fit$cv_fit, s = fit$lambda_optimal))
  expect_equal(fit$coef_matrix, reference, tolerance = 0)
  expect_equal(fit$risk_scores, as.numeric(fit$data$X %*% reference[, 1]), tolerance = 0)
})

test_that("an empty 1-SE model is kept without manufactured discrimination", {
  d <- lassocox_safety_data()
  # Balanced identical predictor distributions in each event/censor time block
  # produce a null maximum partial likelihood, without seed-searching for a result.
  d <- expand.grid(time = seq_len(40), x = c(-1, 1), z = c(-1, 1), status = 0:1)
  a <- lassocox_safety_backend(d, lambda = "lambda.1se", path_plot = TRUE)
  pr <- a$.__enclos_env__$private
  fit <- suppressWarnings(pr$.fitModel(pr$.cleanData()))
  expect_equal(fit$lambda_rule_used, "lambda.1se")
  expect_equal(length(fit$selected_vars), 0L)
  expect_equal(fit$risk_scores, rep(0, nrow(d)))
  expect_equal(fit$performance_metrics$cindex, 0.5)
})
