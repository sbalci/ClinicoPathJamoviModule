# Regressions for the /review-function lassologistic findings R1-R8 fixed on 2026-10-12.
# The bundled-data change (R2) also moved the "separated model" test in
# test-lassologistic-release-review.R onto a synthetic separated fixture.

rv_pv <- c("p53_aberrant", "rb1_loss", "sstr2a_3plus", "ki67_pct", "daxx_atrx_loss", "organoid_pattern",
           "plasmacytoid_cells", "interstitial_reaction", "coexist_carcinoma", "mitotic_count", "age_years",
           "tumor_size_cm")
rv_run <- function(data, ...) {
  skip_if_not_installed("glmnet")
  suppressWarnings(do.call(ClinicoPath::lassologistic, c(list(data = data), list(...))))
}
rv_pannen <- function(...) rv_run(ClinicoPath::lassologistic_pannen, outcome = "diagnosis",
                                  outcomeLevel = "PanNEC", explanatory = rv_pv, ...)
rv_note <- function(tbl, key) tbl$.__enclos_env__$private$.notes[[key]]$note
rv_row <- function(tbl, key) { df <- tbl$asDF; df[gsub('^"|"$', "", rownames(df)) == key, ] }

test_that("R1 a corrected slope beside a poor corrected AUC is not read as shrinkage", {
  # No true predictor; Minimum CV Error still retains a chance term and the bootstrap leaves the
  # corrected AUC at ~0.57 with a corrected slope of ~4.9 ("too moderate" by the old note).
  set.seed(2)
  X <- matrix(rnorm(150 * 10), 150, dimnames = list(NULL, paste0("x", 1:10)))
  d <- data.frame(X, dx = factor(ifelse(rbinom(150, 1, plogis(-0.8)) == 1, "ev", "no")))
  r <- rv_run(d, outcome = "dx", outcomeLevel = "ev", explanatory = colnames(X), lambda = "lambda.min",
              bootstrapValidation = TRUE, bootstrapN = 50)
  auc <- rv_row(r$validationTable, "auc"); slope <- rv_row(r$validationTable, "slope")
  expect_lt(round(auc$corrected, 3), 0.7)
  expect_true(is.finite(slope$corrected))
  expect_match(rv_note(r$validationTable, "slope_low_auc"), "read the corrected calibration slope", fixed = TRUE)
  expect_match(rv_note(r$validationTable, "slope_low_auc"), "average optimism", fixed = TRUE)
  expect_match(rv_note(r$validationTable, "calibration_note"), "close to 0.5", fixed = TRUE)
  # A model that discriminates gets no such note.
  ok <- rv_pannen(bootstrapValidation = TRUE, bootstrapN = 50)
  expect_gt(rv_row(ok$validationTable, "auc")$corrected, 0.9)
  expect_null(rv_note(ok$validationTable, "slope_low_auc"))
})

test_that("R2 the bundled example overlaps: no separation, an estimable calibration slope", {
  d <- ClinicoPath::lassologistic_pannen
  expect_equal(nrow(d), 200)
  for (rule in c("lambda.1se", "lambda.min")) {
    r <- rv_pannen(lambda = rule, bootstrapValidation = TRUE, bootstrapN = 50, scoringSystem = TRUE)
    auc_txt <- rv_row(r$performance, "auc")$value
    expect_false(grepl("not estimable", auc_txt, fixed = TRUE))
    expect_lt(as.numeric(sub(" .*", "", auc_txt)), 0.99)
    expect_false(grepl("Perfect Apparent Separation", r$notices$content, fixed = TRUE))
    slope <- rv_row(r$validationTable, "slope")
    expect_true(is.finite(slope$apparent) && is.finite(slope$corrected))
    # every replicate had a finite slope, and the events per term are not in the red band
    expect_null(rv_note(r$validationTable, "slope_subset"))
    expect_null(rv_note(r$validationTable, "slope_na"))
    expect_false(grepl("major concern", r$notices$content, fixed = TRUE))
    expect_lt(max(r$scoringTable$asDF$oddsRatio), 100)
  }
})

test_that("the bootstrap cache key is a hash: an edit preserving column sums and moments recomputes", {
  # Codex review 2026-10-12: adding (1, -2, 1) to three consecutive rows kept colSums and
  # crossprod(seq_len(n), X), so the old fingerprint reused a summary of different data.
  skip_if_not_installed("glmnet")
  set.seed(8); n <- 120
  d <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n))
  d$y <- factor(ifelse(rbinom(n, 1, plogis(0.9 * d$a)) == 1, "ev", "no"))
  opts <- ClinicoPath:::lassologisticOptions$new(outcome = "y", outcomeLevel = "ev", explanatory = c("a", "b", "c"),
                                                bootstrapValidation = TRUE, bootstrapN = 50)
  a <- ClinicoPath:::lassologisticClass$new(options = opts, data = d)
  a$init(); suppressWarnings(a$run())
  key1 <- a$results$validationTable$state$key
  d2 <- d; d2$a[10:12] <- d2$a[10:12] + c(1, -2, 1)
  expect_equal(colSums(cbind(d$a)), colSums(cbind(d2$a)))
  expect_equal(sum(seq_len(n) * d$a), sum(seq_len(n) * d2$a))
  b <- ClinicoPath:::lassologisticClass$new(options = opts, data = d2)
  b$init(); suppressWarnings(b$run())
  expect_false(identical(key1$data, b$results$validationTable$state$key$data))
})

test_that("R3 factor terms name the reference level they are compared with", {
  r <- rv_pannen()
  ref <- rv_note(r$coefficients, "reference_note")
  expect_match(ref, "p53_aberrant = Normal", fixed = TRUE)
  expect_match(ref, "rb1_loss = Retained", fixed = TRUE)
  expect_false(grepl("ki67_pct =", ref, fixed = TRUE))       # numeric: no reference level
  # Only factors with a selected term are listed.
  sel <- r$coefficients$asDF$variable
  expect_false("organoid_patternPresent" %in% sel)
  expect_false(grepl("organoid_pattern", ref, fixed = TRUE))
})

test_that("R4 lambda is printed with four significant digits, not four decimals", {
  r <- rv_pannen(lambda = "lambda.min", cv_plot = TRUE)
  ms <- r$modelSummary$asDF
  shown <- ms$value[ms$statistic == "Lambda (optimal)"]
  expect_identical(shown, sprintf("%.4g", r$cv_plot$state$lambda_min))
  expect_equal(as.numeric(shown), r$cv_plot$state$lambda_min, tolerance = 1e-3)
})

test_that("R5 a data-derived cut is printed short and awards exactly the same patients", {
  set.seed(5)
  n <- 160
  d <- data.frame(marker = rnorm(n, 50, 12), grade = factor(sample(c("G1", "G2"), n, TRUE)))
  d$dx <- factor(ifelse(rbinom(n, 1, plogis(-3 + 0.06 * d$marker + 0.8 * (d$grade == "G2"))) == 1, "ev", "no"))
  for (m in c("median", "mean", "tertile", "quartile")) {
    r <- rv_run(d, outcome = "dx", outcomeLevel = "ev", explanatory = c("marker", "grade"),
                lambda = "lambda.min", scoringSystem = TRUE, scoreCutMethod = m)
    st <- r$scoringTable$asDF
    crit <- sub("^> ", "", st$criterion[st$variable == "marker"])
    expect_lte(nchar(crit), 6)
    stat <- switch(m, median = median(d$marker), mean = mean(d$marker),
                   tertile = quantile(d$marker, 2 / 3, names = FALSE), quartile = quantile(d$marker, 0.75, names = FALSE))
    expect_identical(d$marker > as.numeric(crit), d$marker > stat)
    expect_match(rv_note(r$scoringTable, "cut_rounded"), "same patients earn the points", fixed = TRUE)
  }
  # A typed cut stays exactly as typed, with no rounding note.
  r <- rv_run(d, outcome = "dx", outcomeLevel = "ev", explanatory = c("marker", "grade"), lambda = "lambda.min",
              scoringSystem = TRUE, scoreCutMethod = "manual", scoreCutPoints = "marker=51.234")
  expect_identical(r$scoringTable$asDF$criterion[r$scoringTable$asDF$variable == "marker"], "> 51.234")
  expect_null(rv_note(r$scoringTable, "cut_rounded"))
})

test_that("R6 character and logical predictors are scored as factors (R API)", {
  set.seed(6)
  n <- 200
  d <- data.frame(sex = sample(c("F", "M"), n, TRUE), smoker = sample(c(TRUE, FALSE), n, TRUE),
                  age = rnorm(n, 60, 10), stringsAsFactors = FALSE)
  d$out <- factor(ifelse(rbinom(n, 1, plogis(-1 + 1.2 * (d$sex == "M") + 1.0 * d$smoker + 0.05 * (d$age - 60))) == 1,
                         "yes", "no"))
  args <- list(outcome = "out", outcomeLevel = "yes", explanatory = c("sex", "smoker", "age"),
               lambda = "lambda.min", scoringSystem = TRUE)
  chr <- do.call(rv_run, c(list(d), args))
  fac <- d; fac$sex <- factor(fac$sex); fac$smoker <- factor(fac$smoker)
  ref <- do.call(rv_run, c(list(fac), args))
  st <- chr$scoringTable$asDF
  expect_identical(st$criterion[st$variable %in% c("sexM", "smokerTRUE")], c("present", "present"))
  expect_equal(chr$coefficients$asDF, ref$coefficients$asDF)
  expect_match(rv_note(chr$coefficients, "reference_note"), "sex = F", fixed = TRUE)
})

test_that("R7 individual controls are sentence case and the backend quotes them as such", {
  path <- testthat::test_path("..", "..", "jamovi", "lassologistic.a.yaml")
  skip_if_not(file.exists(path), "the jamovi/ sources are not beside an installed package's tests")
  skip_if_not_installed("yaml")
  opts <- yaml::read_yaml(path)$options
  titles <- stats::setNames(vapply(opts, function(o) if (is.null(o$title)) NA_character_ else o$title, ""),
                            vapply(opts, `[[`, "", "name"))
  expect_identical(unname(titles[c("outcomeLevel", "penalty", "alpha", "lambda", "nfolds", "random_seed",
                                   "bootstrapN", "scoringMethod", "scoringMaxPoints", "scoreCutMethod",
                                   "scoreCutPoints")]),
                   c("Event level", "Penalty type", "Elastic net mixing (0 = ridge, 1 = LASSO)",
                     "Lambda selection method", "Number of CV folds", "Random seed", "Bootstrap iterations",
                     "Scoring method", "Maximum points per feature", "Cut point for continuous predictors",
                     "Manual cut points"))
  d <- ClinicoPath::lassologistic_pannen
  pending <- rv_run(d, outcome = "diagnosis", outcomeLevel = NULL, explanatory = rv_pv)
  expect_match(pending$todo$content, "Choose the 'Event level'", fixed = TRUE)
})

test_that("R8 the CV plot labels its top axis with the number of non-zero terms", {
  skip_if_not_installed("glmnet")
  opts <- ClinicoPath:::lassologisticOptions$new(outcome = "diagnosis", outcomeLevel = "PanNEC",
                                                explanatory = rv_pv, cv_plot = TRUE)
  a <- ClinicoPath:::lassologisticClass$new(options = opts, data = ClinicoPath::lassologistic_pannen)
  a$init(); suppressWarnings(a$run())
  r <- a$results
  st <- r$cv_plot$state
  expect_length(st$nzero, length(st$lambda))
  # Same zero rule as the coefficient table: the count at the chosen lambda is "Terms selected".
  ms <- r$modelSummary$asDF
  expect_equal(st$nzero[which.min(abs(st$lambda - st$lambda_1se))],
               as.integer(ms$value[ms$statistic == "Terms selected"]))
  # Render a copy of the renderer whose print() hands back the ggplot instead of drawing it.
  captured <- NULL
  render <- a$.__enclos_env__$private$.cvPlot
  environment(render) <- list2env(list(print = function(x, ...) captured <<- x), parent = environment(render))
  theme <- jmvcore:::getGlobalTheme("default", "jmv")
  expect_true(render(r$cv_plot, ggtheme = theme$ggtheme, theme = theme$theme))
  sec <- captured$scales$get_scales("x")$secondary.axis
  expect_s3_class(sec, "AxisSecondary")
  expect_true(all(as.character(sec$labels) %in% as.character(st$nzero)))
  expect_identical(sec$name, "Non-zero terms")
})
