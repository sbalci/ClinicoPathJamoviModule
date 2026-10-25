# Execute the guide's recipes in the existing target backends. This verifies the
# configuration contract, not release certification of every target feature.
.agreementIntroRecommend <- getFromNamespace(".agreementIntroRecommend", "ClinicoPath")
target_guide_design <- function(...) utils::modifyList(list(purpose = "categorical",
  scale = "nominal", nRaters = 2L, panel = "same", missingRatings = "no",
  repeatedReadings = "no", analysisUnit = "independent"), list(...))
target_guide_recipe <- function(...) .agreementIntroRecommend(target_guide_design(...))$
  recommendations[[1]]$settings
target_guide_run <- function(name, data, settings) {
  ns <- asNamespace("ClinicoPath")
  options <- do.call(ns[[paste0(name, "Options")]]$new, settings)
  a <- ns[[paste0(name, "Class")]]$new(options = options, data = data)
  suppressMessages(a$run())
  a$results
}

test_that("categorical recipes produce their named Cohen, Conger, Fleiss and alpha coefficients", {
  d <- data.frame(A = factor(rep(c("no", "yes", "yes", "no", "yes"), 8)),
    B = factor(rep(c("no", "yes", "no", "no", "yes"), 8)),
    C = factor(rep(c("yes", "yes", "no", "no", "yes"), 8)))
  s <- c(target_guide_recipe(), list(vars = c("A", "B")))
  r <- target_guide_run("agreement", d, s)
  expect_equal(r$irrtable$asDF$kappa, irr::kappa2(d[1:2])$value, tolerance = 1e-10)
  for (fixed in c(TRUE, FALSE)) {
    s <- c(target_guide_recipe(nRaters = 3L, panel = if (fixed) "same" else "different"),
      list(vars = names(d)))
    r <- target_guide_run("agreement", d, s)
    expect_equal(r$irrtable$asDF$kappa, irr::kappam.fleiss(d, exact = fixed)$value,
      tolerance = 1e-10)
  }
  d$B[c(1, 3, 6)] <- NA
  s <- c(target_guide_recipe(nRaters = 3L, missingRatings = "yes"), list(vars = names(d)))
  r <- target_guide_run("agreement", d, s)
  numeric_ratings <- vapply(d, as.integer, integer(nrow(d)))
  expect_equal(r$krippTableGroup$krippTable$asDF$alpha,
    irr::kripp.alpha(cbind(t(numeric_ratings), NA), method = "nominal")$value, tolerance = 1e-10)
  expect_equal(r$krippTableGroup$krippTable$asDF$subjects, nrow(d))
  expect_equal(r$irrtable$asDF$subjects, nrow(d))
  expect_gt(nrow(d), sum(stats::complete.cases(d)))
})

test_that("all six recommended ICC settings reproduce the explicit model definition and unit", {
  x <- seq(10.1, 90.1, length.out = 40)
  d <- data.frame(A = x + sin(x), B = x + cos(x) + 4, C = x + 2 * sin(x) - 3)
  for (base in c("1", "2", "3")) for (unit in c("single", "average")) {
    design <- target_guide_design(purpose = "reliability", scale = "continuous", nRaters = 3L,
      panel = if (base == "1") "different" else "same",
      raterSelection = if (base == "3") "fixed" else "random",
      iccDefinition = if (base == "3") "consistency" else "absolute", reportingUnit = unit)
    recipes <- .agreementIntroRecommend(design)$recommendations
    expected <- irr::icc(d, model = if (base == "1") "oneway" else "twoway",
      type = if (base == "3") "consistency" else "agreement", unit = unit)$value
    for (recipe in recipes) {
      r <- target_guide_run(recipe$target, d, c(recipe$settings, list(vars = names(d))))
      value <- if (recipe$target == "agreement") r$iccGroup$iccTable$asDF$icc_value else
        r$icc_results$asDF$icc_value
      expect_length(value, 1L)
      expect_equal(value, expected, tolerance = 1e-10)
    }
  }
})

test_that("measurement, ranking and marginal recipes populate their intended outputs", {
  x <- seq(10.1, 90.1, length.out = 40)
  d <- data.frame(A = x + sin(x), B = x + cos(x) + 4)
  s <- c(target_guide_recipe(purpose = "measurement", scale = "continuous"),
    list(vars = names(d)))
  r <- target_guide_run("agreement", d, s)
  expect_equal(r$iccGroup$linCCCTable$asDF$ccc, epiR::epi.ccc(d$A, d$B)$rho.c$est, tolerance = 1e-10)
  ranks <- data.frame(A = c(1:8, 10, 9), B = c(1:7, 10, 8, 9))
  s <- c(target_guide_recipe(purpose = "ranking", scale = "rankings"), list(vars = names(ranks)))
  r <- target_guide_run("agreement", ranks, s)
  expect_equal(r$lightKappaTableGroup$kendallWTable$asDF$w, irr::kendall(ranks)$value, tolerance = 1e-10)
  d <- data.frame(A = factor(rep(c("a", "b", "c", "a", "b"), 10)),
    B = factor(rep(c("b", "b", "c", "a", "a"), 10)))
  r <- target_guide_run("agreement", d, c(target_guide_recipe(purpose = "bias"),
    list(vars = names(d))))
  expect_equal(nrow(r$raterBiasGroup$stuartMaxwellTable$asDF), 1L)
  expect_true(is.finite(r$raterBiasGroup$stuartMaxwellTable$asDF$chisq))
})

test_that("reader recipes preserve comparator semantics, threshold direction and cluster units", {
  ns <- asNamespace("ClinicoPath")
  d <- data.frame(case = paste0("c", 1:20), patient = rep(paste0("p", 1:10), each = 2),
    A = rep(c("yes", "no"), 10), B = rep(c("yes", "no", "no", "yes"), 5),
    truth = rep(c("disease", "healthy"), 10))
  s <- target_guide_recipe(purpose = "readerAccuracy", scale = "binary", referenceType = "consensus",
    positiveLabel = "yes", negativeLabel = "no", referencePositive = "disease",
    referenceNegative = "healthy", analysisUnit = "clustered")
  s <- c(s, list(vars = c("A", "B"), caseID = "case", readerCluster = "patient",
    referenceStandard = "truth"))
  p <- ns$.paReaderPrepare(d, s)
  expect_true(p$ready)
  expect_true(p$reference_ready)
  expect_identical(p$options$referenceType, "consensus")
  expect_equal(length(unique(p$cluster)), 10L)
  d$A <- seq_len(20); d$B <- 21 - seq_len(20)
  s <- c(target_guide_recipe(purpose = "cutoff", scale = "continuous", referenceType = "none",
    cutoff = "10", cutoffPrespecified = "yes", cutoffDirection = "le"),
    list(readerScores = c("A", "B"), caseID = "case"))
  p <- ns$.paReaderPrepare(d, s)
  expect_true(p$ready)
  expect_identical(unname(p$calls[, 1]), d$A <= 10)
  expect_false(p$reference_ready)
})

test_that("planning recipes are accepted for supported counts with explicit planning inputs", {
  for (objective in c("precision", "power", "fixed")) {
    recipe <- .agreementIntroRecommend(target_guide_design(purpose = "planning",
      planningGoal = objective, nCategories = 3L, nRaters = 3L))$recommendations[[1]]
    r <- target_guide_run(recipe$target, data.frame(),
      c(recipe$settings, list(props = "0.3, 0.3, 0.4")))
    expect_gt(nchar(r$text1$content), 10L)
    expect_false(grepl("error|invalid", r$text1$content, ignore.case = TRUE))
  }
})

test_that("weighted recipes respect a nonalphabetic declared order", {
  lev <- c("Low", "Middle", "High")
  d <- data.frame(A = ordered(rep(lev[c(1, 1, 2, 3, 3)], 8), levels = lev),
    B = ordered(rep(lev[c(1, 2, 2, 2, 3)], 8), levels = lev),
    C = ordered(rep(lev[c(1, 1, 3, 2, 3)], 8), levels = lev))
  for (weight in c("linear", "quadratic")) {
    recipes <- .agreementIntroRecommend(target_guide_design(scale = "ordinal",
      categoryOrder = "confirmed", weights = weight))$recommendations
    codes <- as.data.frame(lapply(d[1:2], as.integer))
    expected <- irr::kappa2(codes, weight = if (weight == "linear") "equal" else
      "squared", sort.levels = TRUE)$value
    for (recipe in recipes) {
      vars <- if (recipe$target == "agreement") list(vars = c("A", "B")) else
        list(rater1 = "A", rater2 = "B")
      r <- target_guide_run(recipe$target, d, c(recipe$settings, vars))
      value <- if (recipe$target == "agreement") r$irrtable$asDF$kappa else r$kappaTable$asDF$kappa
      expect_equal(value, expected, tolerance = 1e-10)
    }
    recipes <- .agreementIntroRecommend(target_guide_design(scale = "ordinal", nRaters = 3L,
      categoryOrder = "confirmed", weights = weight))$recommendations
    codes <- vapply(d, as.integer, integer(nrow(d)))
    r <- target_guide_run("agreement", d, c(recipes[[1]]$settings, list(vars = names(d))))
    expect_equal(r$krippTableGroup$krippTable$asDF$alpha,
      irr::kripp.alpha(cbind(t(codes), NA), method = "ordinal")$value, tolerance = 1e-10)
    r <- target_guide_run("agreement", d, c(recipes[[2]]$settings, list(vars = names(d))))
    expect_equal(r$gwetGroup$gwetTable$asDF$coefficient,
      irrCAC::gwet.ac1.raw(codes, weights = weight, categ.labels = seq_along(lev))$est$coeff.val,
      tolerance = 1e-10)
  }
})

test_that("paired agreement recipe compares the requested two-rater coefficients", {
  d <- data.frame(A = rep(c("yes", "no", "yes", "no", "yes"), 10),
    B = rep(c("no", "no", "yes", "no", "yes"), 10),
    C = rep(c("yes", "no", "yes", "no", "yes"), 10),
    D = rep(c("no", "no", "no", "no", "yes"), 10))
  s <- c(target_guide_recipe(purpose = "paired", comparisonGoal = "agreement"),
    list(vars = c("A", "B"), conditionBVars = c("C", "D"), pairedBootN = 500L, seed = 31L))
  r <- target_guide_run("agreement", d, s)$pairedAgreementGroup$pairedAgreementTable$asDF
  kappa <- rownames(r) == '"kappa"'
  expect_equal(sum(kappa), 1L)
  expect_equal(r$condition_a[kappa], irr::kappa2(d[1:2])$value, tolerance = 1e-10)
  expect_equal(r$condition_b[kappa], irr::kappa2(d[3:4])$value, tolerance = 1e-10)
  expect_equal(r$difference[kappa], r$condition_b[kappa] - r$condition_a[kappa])
  expect_equal(r$condition_a[!kappa], mean(d$A == d$B))
  expect_equal(r$condition_b[!kappa], mean(d$C == d$D))
})

test_that("specialist recipes limit outputs to their supported configuration", {
  d <- data.frame(A = rep(c("yes", "no", "yes", "no", "yes"), 8),
    B = rep(c("no", "no", "yes", "no", "yes"), 8))
  s <- c(target_guide_recipe(purpose = "paired", comparisonGoal = "specialized"),
    list(modality1_var = "A", modality2_var = "B"))
  r <- target_guide_run("modalitycomparison", d, s)$agreementTable$asDF
  expect_equal(r$value[r$statistic == "Cohen's Kappa"], irr::kappa2(d)$value, tolerance = 1e-10)
  expect_equal(r$value[r$statistic == "Overall Agreement"], 100 * mean(d$A == d$B))
  d <- data.frame(A = seq_len(40) + sin(seq_len(40)), B = seq_len(40) + 2)
  recipe <- .agreementIntroRecommend(target_guide_design(purpose = "measurement",
    scale = "continuous"))$recommendations[[2]]
  r <- target_guide_run("pathologyagreement", d,
    c(recipe$settings, list(dep1 = "A", dep2 = "B", show_plots = FALSE)))$agreementtable$asDF
  expect_equal(r$value[r$metric == "Concordance Correlation Coefficient"],
    epiR::epi.ccc(d$A, d$B)$rho.c$est, tolerance = 1e-10)
  expect_equal(r$value[r$metric == "Bland-Altman Mean Difference (Bias)"], mean(d$A - d$B))
  d <- data.frame(patient = rep(paste0("p", 1:4), each = 2),
    focus = rep(c("primary", "metastasis"), 4), marker = c(1, 1, 0, 1, 0, 0, 1, 1))
  s <- c(target_guide_recipe(purpose = "multifocal", analysisUnit = "clustered"),
    list(caseId = "patient", focusId = "focus", markers = "marker", showPlot = FALSE))
  r <- target_guide_run("multifocalconcordance", d, s)$perMarkerTable$asDF
  expect_equal(r$nCases, 4)
  expect_equal(r$rate, 0.75)
})

test_that("the long reader modality recipe requires an explicit matched contrast", {
  d <- expand.grid(case = paste0("c", 1:10), reader = c("A", "B"),
    condition = c("before", "after"), stringsAsFactors = FALSE)
  d$truth <- rep(c("yes", "no"), 20)
  d$value <- d$truth
  s <- c(target_guide_recipe(purpose = "paired", comparisonGoal = "readerModality",
    scale = "binary", referenceType = "consensus", positiveLabel = "yes", negativeLabel = "no",
    referencePositive = "yes", referenceNegative = "no", repeatedReadings = "yes"),
    list(caseID = "case", readerID = "reader", rating = "value", modality = "condition",
      referenceStandard = "truth", baselineModality = "before", comparisonModality = "after"))
  p <- getFromNamespace(".paReaderPrepare", "ClinicoPath")(d, s)
  expect_true(p$ready)
  expect_true(p$reference_ready)
  expect_equal(p$long$modalities, c("before", "after"))
  expect_true(all(getFromNamespace(".paReaderModalityPopulation", "ClinicoPath")(p)))
})
