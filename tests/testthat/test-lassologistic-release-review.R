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
  cv <- glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1, standardize = TRUE,
                          type.measure = "deviance", foldid = fid)
  lam <- cv$lambda.1se
  cf <- as.numeric(stats::coef(cv, s = lam)); b_z <- cf[-1] * sds; names(b_z) <- colnames(X)
  sel <- names(b_z)[abs(b_z) > 1e-10]
  p <- as.numeric(stats::predict(cv, X, s = lam, type = "response"))
  list(lambda = lam, b0_z = cf[1] + sum(cf[-1] * ctr), b_z = b_z, sel = sel, sds = sds, ctr = ctr, X = X, y = y, p = p,
       auc = as.numeric(pROC::auc(pROC::roc(y, p, direction = "<", levels = c(0, 1), quiet = TRUE))))
}

test_that("lambda, coefficients and apparent AUC match a hand-built glmnet pipeline", {
  d <- ll_fixture(); ref <- ll_reference(d)
  res <- ll_run(d)
  ms <- res$modelSummary$asDF
  expect_equal(ms$value[ms$statistic == "Lambda (optimal)"], sprintf("%.4g", ref$lambda))
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
  # library-audit 2026-09-16 meddecide [LOW]: the fixed-row tables (performance among them)
  # now get their rows in .init() and .run() fills them by key, so the object is initialised
  # first, as jamovi always does, before .run() is called twice on it.
  an$init()
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

# ═══════════════════════════════════════════════════════════
# Independent review 2026-10-12 (IR-lassologistic-01 .. -10)
# development-ideas/lassologistic-independent-review-2026-10-12.md
# ═══════════════════════════════════════════════════════════

ll_notes <- function(tbl) vapply(tbl$notes, function(n) n$note, "")
ll_noise <- function() {   # seed chosen so that the 1SE model selects nothing
  set.seed(3)
  d <- data.frame(a = stats::rnorm(60), b = stats::rnorm(60), c = stats::rnorm(60))
  d$dx <- factor(sample(c("NET", "NEC"), 60, TRUE), levels = c("NET", "NEC"))
  d
}

test_that("IR-01 outcomeLevel offers None, so jamovi does not fill in the first level", {
  # jamovi 28.3 LevelSelector without allowNone calls setValue(levels[0]); the R side of
  # jmvcore ignores the flag, so the generated Options class is the only place to see it.
  init <- paste(deparse(ClinicoPath:::lassologisticOptions$public_methods$initialize), collapse = "\n")
  expect_match(init, "allowNone = TRUE", fixed = TRUE)
})

test_that("IR-01 a factor outcome with no event level is pending, not fitted on an inferred level", {
  d <- ll_fixture()
  res <- ClinicoPath::lassologistic(data = d, outcome = "dx", outcomeLevel = NULL,
                                    explanatory = c("p53", "Rb1", "ki67", "age"))
  expect_equal(nrow(res$coefficients$asDF), 0)
  expect_match(res$todo$content, "NET \\(n=[0-9]+\\), NEC \\(n=[0-9]+\\)")
})

test_that("IR-03 a manual cut above every value is moved inside the data, never scored on a 1-SD contrast", {
  d <- ll_fixture()
  res <- ll_run(d, scoringSystem = TRUE, scoreCutMethod = "manual", scoreCutPoints = "ki67=1000")
  st <- res$scoringTable$asDF
  expect_true("ki67" %in% st$variable)
  # the printed cut is the applied cut and splits the cases exactly as the median does (printed as
  # the shortest such decimal since review R5, 2026-10-12)
  crit <- st$criterion[st$variable == "ki67"]
  expect_match(crit, "^> ")
  expect_identical(d$ki67 > as.numeric(sub("^> ", "", crit)), d$ki67 > stats::median(d$ki67))
  notes <- ll_notes(res$scoringTable)
  expect_true(any(grepl("ki67", notes, fixed = TRUE) & grepl("left no case on one side", notes, fixed = TRUE)))
})

test_that("IR-03 ties at the top that make the median the maximum still split the cases", {
  d <- ll_fixture()
  d$ki67 <- ifelse(d$ki67 > 25, 90, round(d$ki67))
  res <- ll_run(d, scoringSystem = TRUE)
  st <- res$scoringTable$asDF
  expect_true("ki67" %in% st$variable)
  second <- sort(unique(d$ki67), decreasing = TRUE)[2]
  expect_equal(st$criterion[st$variable == "ki67"], paste0("> ", base::format(second, trim = TRUE)))
  expect_true(any(grepl("left no case on one side", ll_notes(res$scoringTable), fixed = TRUE)))
})

test_that("IR-05 Importance is the per-SD magnitude with or without standardization", {
  d <- ll_fixture()
  X <- stats::model.matrix(~ ., d[c("p53", "Rb1", "ki67", "age")])[, -1]
  for (std in c(TRUE, FALSE)) {
    cf <- ll_run(d, standardize = std)$coefficients$asDF
    cf <- cf[cf$variable != "(Intercept)", ]
    per_sd <- abs(cf$coefficient) * apply(X[, cf$variable, drop = FALSE], 2, stats::sd)
    expect_equal(cf$importance, unname(per_sd / max(per_sd)), tolerance = 1e-8, label = paste("standardize", std))
  }
})

test_that("IR-02 the calibration note explains a corrected slope above 1; probabilities carry a shrinkage note", {
  d <- ll_fixture()
  res <- ll_run(d, bootstrapValidation = TRUE, bootstrapN = 50)
  expect_true(any(grepl("corrected slope above 1 means they are too moderate", ll_notes(res$validationTable), fixed = TRUE)))
  expect_true(any(grepl("pulls them toward the overall event rate", ll_notes(res$performance), fixed = TRUE)))
})

test_that("IR-08/IR-09 a model that selects nothing shows no zero-width interval and no orphan slope optimism", {
  res <- ClinicoPath::lassologistic(data = ll_noise(), outcome = "dx", outcomeLevel = "NEC",
                                    explanatory = c("a", "b", "c"), bootstrapValidation = TRUE, bootstrapN = 50)
  expect_equal(nrow(res$coefficients$asDF[res$coefficients$asDF$variable == "No variables selected", ]), 1)
  auc <- res$performance$asDF[gsub('^"|"$', "", rownames(res$performance$asDF)) == "auc", ]
  expect_equal(auc$value, "0.500 (CI not estimable)")
  expect_equal(auc$interpretation, "No discrimination")
  expect_false(grepl("Poor Discrimination", res$notices$content, fixed = TRUE))
  slope <- res$validationTable$asDF[gsub('^"|"$', "", rownames(res$validationTable$asDF)) == "slope", ]
  expect_true(is.na(slope$apparent) && is.na(slope$optimism) && is.na(slope$corrected))
  expect_true(any(grepl("calibration slope is not estimable", ll_notes(res$validationTable), fixed = TRUE)))
})

test_that("IR-04 the lookup column says observed event rate, which is what it holds", {
  d <- ll_fixture()
  res <- ll_run(d, scoringSystem = TRUE)
  expect_equal(res$lookupTable$getColumn("probability")$title, "Observed Event Rate")
})

test_that("IR-07/IR-10 guidance and suitability text do not overclaim", {
  d <- ll_fixture()
  res <- ll_run(d, includeClinicalGuidance = TRUE)
  expect_false(grepl("most useful", res$clinicalGuidance$content, fixed = TRUE))
  # library-audit 2026-10-23 meddecide [HIGH] DONE: suitabilityReport is a Table now
  expect_false(any(grepl("adequate", unlist(res$suitabilityReport$asDF), fixed = TRUE)))
})

test_that("Printed cut points are exact and readable: a typed 19.65 prints as 19.65", {
  # %.17g printed "19.649999999999999"; rounding to 3 dp misstated the rule applied.
  d <- ll_fixture()
  res <- ll_run(d, scoringSystem = TRUE, scoreCutMethod = "manual", scoreCutPoints = "ki67=19.65")
  st <- res$scoringTable$asDF
  expect_true("ki67" %in% st$variable)
  expect_equal(st$criterion[st$variable == "ki67"], "> 19.65")
  # A data-derived cut is printed as the shortest decimal that splits the cases exactly as the
  # mean does (review R5, 2026-10-12): it used to print all 15 digits of mean(ki67).
  res_mean <- ll_run(d, scoringSystem = TRUE, scoreCutMethod = "mean")
  crit <- res_mean$scoringTable$asDF$criterion[res_mean$scoringTable$asDF$variable == "ki67"]
  cut <- as.numeric(sub("^> ", "", crit))
  expect_identical(d$ki67 > cut, d$ki67 > mean(d$ki67))
  expect_lte(nchar(sub("^> ", "", crit)), 6)
})

test_that("A separated model reports no calibration slope instead of an arbitrary glm value", {
  # The bundled PanNEN example used to separate completely at lambda.1se (apparent AUC 1); glm then
  # stopped at slope 11.9 (apparent) and 48.6 (corrected), shown as calibration slopes. That data
  # now overlaps (review R2, 2026-10-12), so the separation is built here: x1 splits the classes.
  set.seed(11)
  y <- rep(0:1, each = 40)
  d <- data.frame(x1 = ifelse(y == 1, runif(80, 3, 5), runif(80, 0, 2)), x2 = rnorm(80), x3 = rnorm(80),
                  dx = factor(ifelse(y == 1, "case", "ctrl")))
  args <- list(data = d, outcome = "dx", outcomeLevel = "case", explanatory = c("x1", "x2", "x3"),
               bootstrapValidation = TRUE, bootstrapN = 50)
  res <- suppressWarnings(do.call(ClinicoPath::lassologistic, args))
  vt <- res$validationTable$asDF
  slope <- vt[gsub('^"|"$', "", rownames(vt)) == "slope", ]
  expect_true(is.na(slope$apparent) && is.na(slope$optimism) && is.na(slope$corrected))
  expect_true(any(grepl("separate the two outcome classes completely", ll_notes(res$validationTable), fixed = TRUE)))
  auc <- vt[gsub('^"|"$', "", rownames(vt)) == "auc", ]
  expect_true(is.finite(auc$corrected))
})

test_that("a predictor constant among the complete cases is dropped and reported, not a design-matrix error", {
  # /release-review-function 2026-10-12: droplevels() on the complete cases left such a factor one
  # level and model.matrix() aborted with "contrasts can be applied only to factors with 2 or more
  # levels"; a numeric one was dropped as a degenerate column without a word, and its own missing
  # values still excluded rows from a model it was not in.
  skip_if_not_installed("glmnet")
  d <- ClinicoPath::lassologistic_pannen
  pv <- c("p53_aberrant", "rb1_loss", "sstr2a_3plus", "ki67_pct", "mitotic_count", "age_years")
  ki_na <- is.na(d$ki67_pct)
  d$coexist_carcinoma <- factor(ifelse(ki_na, "Yes", "No"), levels = c("No", "Yes"))  # varies only where Ki-67 is missing
  d$marker <- ifelse(ki_na, 5, 1)
  d$marker[which(complete.cases(d[pv]))[1:2]] <- NA   # and is itself missing in two analysable rows
  run <- function(vars) suppressWarnings(do.call(ClinicoPath::lassologistic, list(
    data = d, outcome = "diagnosis", outcomeLevel = "PanNEC", explanatory = vars)))
  with_both <- run(c(pv, "coexist_carcinoma", "marker"))
  without <- run(pv)
  # the oracle: the same analysis as never having selected them (same rows, same fit)
  expect_equal(with_both$coefficients$asDF, without$coefficients$asDF)
  expect_equal(with_both$modelSummary$asDF, without$modelSummary$asDF)
  txt <- with_both$notices$content
  expect_match(txt, "Predictors constant among the analyzed cases", fixed = TRUE)
  expect_match(txt, "Removed before fitting: coexist_carcinoma, marker.", fixed = TRUE)
  expect_false(grepl("same value in every case", txt, fixed = TRUE))
  # fewer than two left is still a refusal, with the existing message
  expect_error(run(c("ki67_pct", "coexist_carcinoma", "marker")), "Fewer than 2 non-constant predictors remain")
})
