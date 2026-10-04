gas_env <- new.env(parent = globalenv())
gas_root <- testthat::test_path("..", "..")
for (file in c("utils.R", "utils-diagnostic-validation.R", "enhancedROC-validation.R"))
  sys.source(file.path(gas_root, "R", file), gas_env)

test_that("PR area is correct for perfect, tied, reversed and single-class data", {
  pr <- gas_env$.dvPR
  expect_equal(pr(c(.1, .2, .8, .9), c(0, 0, 1, 1), 1)$auprc, 1)
  expect_equal(pr(c(.9, .8, .2, .1), c(0, 0, 1, 1), 1, FALSE)$avg_precision, 1)
  expect_equal(pr(rep(1, 4), c(0, 0, 1, 1), 1)$avg_precision, .5)
  expect_equal(pr(rep(1, 4), c(0, 0, 1, 1), 1)$auprc, .75)
  # Descending labels N,N,P,P: AP = (.3333 + .5)/2; trapezoid = .2916667.
  inverted <- pr(4:1, c(0, 0, 1, 1), 1)
  expect_equal(inverted$avg_precision, 5/12)
  expect_equal(inverted$auprc, 7/24)
  expect_null(pr(1:4, rep(1, 4), 1))
  expect_null(pr(1:4, rep(0, 4), 1))
  expect_equal(pr(c(1, 1, 2, 2), c(0, 1, 0, 1), 1)$auprc,
               pr(c(1, 2, 1, 2), c(1, 1, 0, 0), 1)$auprc)
  expect_equal(pr(c(NA, 1, 2, Inf), c(1, 0, 1, 0), 1)$auprc, 1)
})

test_that("workload matches the published-summary arithmetic without changing cutoff", {
  w <- gas_env$.dvWorkload(1, .905, 45/7056, 7056)
  expect_equal(w$fp, 666.045)
  expect_equal(w$ppv, 45 / 711.045)
  expect_equal(w$tp + w$fp + w$fn + w$tn, 7056)
  expect_equal(gas_env$.dvWorkload(1, .956, 45/7056, 7056)$fp, 308.484)
  for (p in c(0, .0001, .001, .01, 1)) {
    z <- gas_env$.dvWorkload(.9, .95, p, 10000)
    expect_equal(z$tp + z$fp + z$fn + z$tn, 10000)
    expect_equal(z$ppv, z$tp / (z$tp + z$fp))
  }
  first <- gas_env$.dvWorkload(.8, .9, .001, 10000, .1)
  next_utility <- gas_env$.dvWorkload(.8, .9, .001, 10000, .3)
  expect_equal(first[setdiff(names(first), "net_benefit")],
               next_utility[setdiff(names(first), "net_benefit")])
  expect_lt(next_utility$net_benefit, first$net_benefit)
  expect_true(is.na(gas_env$.dvWorkload(0, 1, .1, 100)$flags_per_detected))
  expect_error(gas_env$.dvWorkload(.8, .9, -1, 100), "Invalid")
  expect_error(gas_env$.dvNumbers("0.1,nan"), "numbers")
  expect_error(gas_env$.dvNumbers("0.1,"), "numbers")
})

test_that("whole-patient bootstrap is paired, reproducible and preserves RNG state", {
  set.seed(193)
  y <- rep(c(0, 1), 30)
  score <- y * .2 + runif(60)
  x <- cbind(A = score, B = score)
  old_seed <- .Random.seed
  a <- gas_env$.dvBootstrap(y, x, 1:60, .5, draws = 100, seed = 29)
  expect_identical(.Random.seed, old_seed)
  b <- gas_env$.dvBootstrap(rep(y, each = 3), x[rep(1:60, each = 3), ],
                            rep(1:60, each = 3), .5, draws = 100, seed = 29)
  expect_equal(a, b, tolerance = 1e-10)
  pair <- a[a$model == "B minus A", ]
  expect_equal(pair$estimate[is.finite(pair$estimate)], rep(0, sum(is.finite(pair$estimate))))
  expect_true(all(is.na(pair$lower)))
  expect_error(gas_env$.dvBootstrap(y, x, rep(1:30, each = 2), .5, draws = 100),
               "consistent")
  perfect <- gas_env$.dvBootstrap(y, cbind(A = y), 1:60, .5, draws = 100)
  perfect <- perfect[perfect$metric != "flags_per_1000", ]
  expect_true(all(is.na(perfect$lower)))
  expect_true(all(grepl("Degenerate|undefined", perfect$status)))
})

gas_options <- function(...) {
  utils::modifyList(list(outcome = "y", positiveClass = "P", validationNegative = "N",
    predictors = c("score", "other"), direction = "higher", cohortRole = "frozen",
    modelVersion = "locked-v1", thresholdSource = "prespecified", impactCutoffs = "0.5",
    patientID = "id", siteID = "site", partitionID = NULL, predictionScale = "probability",
    samplingDesign = "consecutive", referenceDescription = "Blinded histology",
    analysisUnit = "slide", validationBootstrap = FALSE, validationDraws = 100,
    seed = 17, confidenceLevel = 95, clinicalImpact = TRUE, useObservedPrevalence = TRUE,
    prevalenceScenarios = "0.0001", prevalence = .001, workloadPopulationSize = 10000,
    utilityThreshold = .1), list(...), keep.null = TRUE)
}

test_that("frozen evaluation audits reference mappings, partitions and exclusions", {
  set.seed(53)
  data <- data.frame(y = rep(c("N", "P"), 20), score = runif(40), other = runif(40),
                     id = 1:40, site = rep(c("A", "B"), each = 20))
  run <- gas_env$.enhancedROCValidation
  a <- run(data, gas_options())
  expect_equal(a$ledger$n[1], 40)
  expect_equal(a$ledger$n[4], 40)
  expect_equal(unique(a$metrics$site), c("All", "Site: A", "Site: B"))
  expect_equal(unique(a$workload$source), c("Observed", "Projected"))
  expect_equal(unique(a$metrics$n[a$metrics$site == "All"]), 40)
  expect_error(run(data, gas_options(validationNegative = "")), "distinct negative")
  expect_error(run(data, gas_options(direction = "auto")), "explicit score direction")
  expect_error(run(data, gas_options(thresholdSource = "exploratory")), "Frozen evaluation")
  bad <- data; bad$y[1] <- "ND"
  expect_error(run(bad, gas_options()), "Unmapped")
  bad <- data; bad$id[2] <- bad$id[1]
  expect_error(run(bad, gas_options()), "inconsistent")
  bad <- rbind(data, data); bad$partition <- rep(c("train", "test"), each = 40)
  expect_error(run(bad, gas_options(partitionID = "partition")), "overlap")
  bad <- data; bad$y[1] <- NA; bad$score[2] <- NA
  b <- run(bad, gas_options())
  expect_equal(b$ledger$n[2:4], c(1, 1, 38))
  expect_equal(unique(b$metrics$n[b$metrics$site == "All"]), 38)
  one_class <- run(subset(data, y == "P"), gas_options(samplingDesign = "positive_only"))
  expect_true(all(is.na(one_class$metrics$estimate[one_class$metrics$metric %in%
                       c("auc", "ap", "pr_auc", "specificity", "ppv")])) )
  tiny <- run(data[1, , drop = FALSE], gas_options(validationBootstrap = TRUE))
  expect_true(all(is.na(tiny$metrics$lower)))
})

test_that("probability calibration is explicit and does not clip exact boundary risks", {
  y <- rep(c(0, 1), each = 20)
  x <- rep(c(.1, .4, .6, .9), 10)
  z <- gas_env$.dvScoreMetrics(y, x, .5, probability = TRUE)
  expect_equal(z['brier'], c(brier = mean((x-y)^2)))
  expect_true(all(is.finite(z[c('calibration_intercept', 'calibration_slope', 'calibration_in_large')])))
  expect_error(gas_env$.dvScoreMetrics(y, x, .5, higher = FALSE, probability = TRUE), "positive class")
  boundary <- gas_env$.dvScoreMetrics(y, y, .5, probability = TRUE)
  expect_equal(unname(boundary['brier']), 0)
  expect_true(is.na(boundary['calibration_slope']))
})
