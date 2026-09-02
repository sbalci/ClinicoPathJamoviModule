# ═══════════════════════════════════════════════════════════
# Release-review regression tests: lassologistic
# ═══════════════════════════════════════════════════════════
#
# Coefficients, lambda, AUC and the reported intercept are checked against a
# hand-built glmnet + pROC pipeline that consumes the RNG in the same order as
# the module (stratified folds: events first, then non-events).

library(testthat)

ll_fixture <- function(n = 400, seed = 7) {
  set.seed(seed)
  d <- data.frame(
    p53  = factor(sample(c("wt", "mut"), n, TRUE), levels = c("wt", "mut")),
    Rb1  = factor(sample(c("intact", "lost"), n, TRUE), levels = c("intact", "lost")),
    ki67 = rnorm(n, 30, 15),
    age  = rnorm(n, 60, 10)
  )
  lp <- -2 + 1.6 * (d$p53 == "mut") + 1.1 * (d$Rb1 == "lost") + 0.03 * (d$ki67 - 30)
  d$dx <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(lp)) == 1, "NEC", "NET"),
                 levels = c("NET", "NEC"))
  d
}

ll_run <- function(d, ...) {
  args <- list(data = d, outcome = "dx", outcomeLevel = "NEC",
               explanatory = c("p53", "Rb1", "ki67", "age"),
               nfolds = 10, random_seed = 123456)
  do.call(ClinicoPath::lassologistic, c(args, list(...)))
}

ll_reference <- function(d, seed = 123456, k = 10) {
  skip_if_not_installed("glmnet"); skip_if_not_installed("pROC")
  X <- stats::model.matrix(~ ., d[c("p53", "Rb1", "ki67", "age")])[, -1]
  y <- as.integer(d$dx == "NEC")
  Xs <- scale(X); sds <- attr(Xs, "scaled:scale"); ctr <- attr(Xs, "scaled:center")
  set.seed(seed)
  pos <- which(y == 1); neg <- which(y == 0)
  fid <- integer(length(y))
  fid[pos] <- sample(rep(1:k, length.out = length(pos)))
  fid[neg] <- sample(rep(1:k, length.out = length(neg)))
  cv <- glmnet::cv.glmnet(Xs, y, family = "binomial", alpha = 1, standardize = FALSE,
                          type.measure = "deviance", foldid = fid)
  lam <- cv$lambda.1se
  cf <- as.numeric(stats::coef(cv, s = lam)); b_z <- cf[-1]; names(b_z) <- colnames(X)
  sel <- names(b_z)[abs(b_z) > 1e-10]
  p <- as.numeric(stats::predict(cv, Xs, s = lam, type = "response"))
  list(lambda = lam, b0_z = cf[1], b_z = b_z, sel = sel, sds = sds, ctr = ctr, X = X, y = y, p = p,
       auc = as.numeric(pROC::auc(pROC::roc(y, p, direction = "<", levels = c(0, 1), quiet = TRUE))))
}

test_that("lambda, coefficients and apparent AUC match a hand-built glmnet pipeline", {
  d <- ll_fixture(); ref <- ll_reference(d)
  res <- ll_run(d)
  ms <- res$modelSummary$asDF
  expect_equal(ms$value[ms$statistic == "Lambda (optimal)"], sprintf("%.4f", ref$lambda))
  expect_equal(as.integer(ms$value[ms$statistic == "Terms selected"]), length(ref$sel))

  cf <- res$coefficients$asDF
  got <- cf[cf$variable %in% ref$sel, ]
  expect_setequal(got$variable, ref$sel)
  want <- ref$b_z[got$variable] / ref$sds[got$variable]
  expect_equal(unname(got$coefficient), unname(want), tolerance = 1e-8)
  expect_equal(unname(got$oddsRatio), unname(exp(want)), tolerance = 1e-8)

  # the AUC cell is text: "0.759 (0.709-0.809)" -> compare the 3-decimal point estimate
  perf <- res$performance$asDF
  auc_txt <- perf$value[grepl("^AUC", perf$metric)][1]
  expect_equal(as.numeric(sub(" .*$", "", auc_txt)), round(ref$auc, 3), tolerance = 1e-9)
})

test_that("the reported intercept reproduces the model's predicted probabilities", {
  d <- ll_fixture(); ref <- ll_reference(d)
  cf <- ll_run(d)$coefficients$asDF
  b0 <- cf$coefficient[cf$variable == "(Intercept)"]
  expect_length(b0, 1); expect_true(is.finite(b0))
  expect_true(is.na(cf$oddsRatio[cf$variable == "(Intercept)"]))
  sel <- cf$variable[cf$variable != "(Intercept)"]
  lp <- b0 + as.numeric(ref$X[, sel, drop = FALSE] %*% cf$coefficient[match(sel, cf$variable)])
  expect_equal(stats::plogis(lp), ref$p, tolerance = 1e-6)
})

test_that("re-running the same analysis object does not duplicate table rows", {
  d <- ll_fixture()
  opts <- ClinicoPath:::lassologisticOptions$new(
    outcome = "dx", outcomeLevel = "NEC", explanatory = c("p53", "Rb1", "ki67", "age"),
    nfolds = 10, random_seed = 123456, showVariableImportance = TRUE, showModelComparison = TRUE)
  an <- ClinicoPath:::lassologisticClass$new(options = opts, data = d)
  pr <- an$.__enclos_env__$private
  pr$.run()
  n1 <- vapply(c("modelSummary", "coefficients", "performance", "variableImportance", "modelComparison"),
               function(t) nrow(an$results[[t]]$asDF), integer(1))
  pr$.run()
  n2 <- vapply(names(n1), function(t) nrow(an$results[[t]]$asDF), integer(1))
  expect_equal(n2, n1)
  expect_equal(unname(n1["performance"]), 8L)
})

test_that("a numeric 0/1 outcome gives the same fit as the factor outcome", {
  d <- ll_fixture()
  d$dx01 <- as.integer(d$dx == "NEC")
  a <- ll_run(d)$coefficients$asDF
  b <- do.call(ClinicoPath::lassologistic, list(data = d, outcome = "dx01", outcomeLevel = "1",
              explanatory = c("p53", "Rb1", "ki67", "age"), nfolds = 10, random_seed = 123456))$coefficients$asDF
  expect_equal(a$variable, b$variable)
  expect_equal(a$coefficient, b$coefficient, tolerance = 1e-10)
})

test_that("the same seed reproduces the same lambda and coefficients; plots render", {
  d <- ll_fixture()
  r1 <- ll_run(d, cv_plot = TRUE, roc_plot = TRUE, coef_plot = TRUE)
  r2 <- ll_run(d)
  expect_equal(r1$coefficients$asDF$coefficient, r2$coefficients$asDF$coefficient)
  expect_equal(r1$modelSummary$asDF$value, r2$modelSummary$asDF$value)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  for (p in c("cv_plot", "roc_plot", "coef_plot")) expect_no_error(r1[[p]]$.render())
})

test_that("stratified folds put events in every fold, for the main fit and the bootstrap", {
  d <- ll_fixture()
  opts <- ClinicoPath:::lassologisticOptions$new(
    outcome = "dx", outcomeLevel = "NEC", explanatory = c("p53", "Rb1", "ki67", "age"))
  an <- ClinicoPath:::lassologisticClass$new(options = opts, data = d)
  y <- c(rep(1L, 7), rep(0L, 93))
  set.seed(1)
  f <- an$.__enclos_env__$private$.stratifiedFolds(y, 5)
  expect_length(f, 100)
  expect_true(all(tabulate(f[y == 1], 5) >= 1))
  expect_true(all(tabulate(f[y == 0], 5) >= 18))
})

test_that("bootstrap validation runs with the stratified fold rule and reports optimism", {
  d <- ll_fixture()
  res <- ll_run(d, bootstrapValidation = TRUE, bootstrapN = 50)
  vt <- res$validationTable$asDF
  expect_equal(nrow(vt), 3)
  auc_row <- vt[grepl("AUC", vt$metric), ]
  expect_true(is.finite(auc_row$corrected))
  expect_lte(auc_row$corrected, auc_row$apparent + 1e-8)
})
