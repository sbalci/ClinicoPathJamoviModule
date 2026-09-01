# Exercise user-visible validation, not merely whether the R wrapper returns.
lassocox_edge_data <- function(n = 80, seed = 123) {
  withr::local_seed(seed)
  x <- matrix(rnorm(n * 5), n, 5, dimnames = list(NULL, paste0("var", 1:5)))
  event <- rexp(n, exp(.5 * x[, 1] - .3 * x[, 2]))
  censor <- rexp(n, .3)
  data.frame(time = pmin(event, censor),
    status = factor(ifelse(event <= censor, "event", "censored")), x)
}

lassocox_edge_run <- function(data, predictors = paste0("var", 1:5), ...) {
  skip_if_not_installed("glmnet")
  args <- modifyList(list(elapsedtime = "time", outcome = "status",
    outcomeLevel = "event", censorLevel = "censored", explanatory = predictors,
    cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE), list(...))
  opts <- do.call(getFromNamespace("lassocoxOptions", "ClinicoPath")$new, args)
  a <- getFromNamespace("lassocoxClass", "ClinicoPath")$new(options = opts, data = data)
  a$run()
  a$results
}

lassocox_edge_valid <- function(result) {
  expect_gt(result$modelSummary$rowCount, 0)
  expect_false(grepl("Analysis Error", result$todo$content, fixed = TRUE))
  expect_true(all(is.finite(as.numeric(as.data.frame(result$performance)$value))))
}

lassocox_edge_error <- function(result, message) {
  expect_equal(result$modelSummary$rowCount, 0)
  expect_match(result$todo$content, message)
  expect_true(result$todo$visible)
  expect_null(result$cv_plot$state)
}

test_that("lassocox reports missing predictor rows and retained sample size", {
  d <- lassocox_edge_data()
  d$var1[1:12] <- NA_real_
  d$var3[15:24] <- NA_real_
  r <- lassocox_edge_run(d)
  lassocox_edge_valid(r)
  expect_match(r$todo$content, "Excluded 22 row")
  tab <- as.data.frame(r$modelSummary)
  expect_equal(tab$value[tab$statistic == "Sample Size"], "58")
})

test_that("lassocox rejects all-censored data visibly", {
  d <- lassocox_edge_data()
  d$status[] <- "censored"
  lassocox_edge_error(lassocox_edge_run(d), "exactly 2 observed")
})

test_that("lassocox warns about few events and reduces stratified folds", {
  d <- lassocox_edge_data()
  d$status[] <- "censored"
  d$status[1:5] <- "event"
  r <- lassocox_edge_run(d)
  lassocox_edge_valid(r)
  expect_match(r$todo$content, "Only 5 events")
  expect_match(r$todo$content, "from 10 to 5")
})

test_that("lassocox reports a constant numeric predictor without dropping the fit", {
  d <- lassocox_edge_data()
  d$var5 <- 1
  r <- lassocox_edge_run(d)
  lassocox_edge_valid(r)
  expect_match(r$todo$content, "Removed constant explanatory variables: var5")
  expect_match(r$suitabilityReport$content, "Removed constant candidate predictors: var5")
  expect_false(grepl("Complete data with no constant predictors", r$suitabilityReport$content))
})

test_that("lassocox handles a single-level factor as an explicit constant", {
  d <- lassocox_edge_data()
  d$group <- factor(rep("one", nrow(d)))
  r <- lassocox_edge_run(d, predictors = c(paste0("var", 1:3), "group"))
  lassocox_edge_valid(r)
  expect_match(r$todo$content, "Removed constant explanatory variables: group")
})

test_that("lassocox accepts predictor names containing spaces and punctuation", {
  d <- lassocox_edge_data()
  names(d)[3:4] <- c("Marker A (ng/ml)", "Gene-B")
  r <- lassocox_edge_run(d, predictors = names(d)[3:7], showEncoding = TRUE)
  lassocox_edge_valid(r)
  expect_setequal(as.data.frame(r$encoding)$variable, names(d)[3:7])
})

test_that("lassocox handles a finite large-scale predictor without erasing it", {
  d <- lassocox_edge_data()
  d$var1 <- d$var1 * 1e8
  r <- lassocox_edge_run(d, showEncoding = TRUE)
  lassocox_edge_valid(r)
  expect_true("var1" %in% as.data.frame(r$encoding)$variable)
})

test_that("lassocox rejects negative follow-up times visibly", {
  d <- lassocox_edge_data()
  d$time[1] <- -1
  lassocox_edge_error(lassocox_edge_run(d), "negative values")
})

test_that("lassocox rejects zero follow-up without automatic adjustment", {
  d <- lassocox_edge_data()
  d$time[1] <- 0
  lassocox_edge_error(lassocox_edge_run(d), "not been automatically adjusted")
})

test_that("lassocox distinguishes usable missingness from too few complete rows", {
  d <- lassocox_edge_data()
  d$var1[1:40] <- NA_real_
  lassocox_edge_valid(lassocox_edge_run(d))
  d$var1[1:72] <- NA_real_
  lassocox_edge_error(lassocox_edge_run(d), "Too few complete cases")
})

test_that("lassocox retains a valid fit with perfectly correlated predictors", {
  d <- lassocox_edge_data()
  d$var4 <- d$var1
  d$var5 <- 2 * d$var1 + 3
  r <- lassocox_edge_run(d)
  lassocox_edge_valid(r)
  expect_match(r$suitabilityReport$content, "collinearity", ignore.case = TRUE)
  # LASSO need not select exactly one representative of every correlated group.
})

test_that("lassocox supports the two-predictor minimum", {
  d <- lassocox_edge_data()
  r <- lassocox_edge_run(d, predictors = c("var1", "var2"))
  lassocox_edge_valid(r)
  tab <- as.data.frame(r$modelSummary)
  expect_equal(tab$value[tab$statistic == "Encoded Predictor Columns"], "2")
})
