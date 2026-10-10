.statROC_data <- function() data.frame(
  status = factor(rep(c("negative", "positive"), each = 6)), id = seq_len(12),
  x = c(0, 1, 2, 3, 3, 5, 1, 2, 3, 4, 5, 6),
  z = c(1, 0, 3, 2, 4, 4, 0, 2, 4, 3, 6, 5))

.statROC_run <- function(data = .statROC_data(), ...) {
  args <- utils::modifyList(list(data = data, dependentVars = c("x", "z"),
    classVar = "status", positiveClass = "positive", negativeClass = "negative",
    refVar = NULL), list(...), keep.null = TRUE)
  suppressMessages(suppressWarnings(do.call(ClinicoPath::psychopdaROC, args)))
}

test_that("default single-AUC interval and null test agree with logit placements", {
  # Independent pairwise rank calculation, without the production inference helper.
  d <- .statROC_data(); positive <- d$status == "positive"
  h <- outer(d$x[positive], d$x[!positive], function(a, b) (a > b) + .5 * (a == b))
  a <- mean(h)
  se <- sqrt(var(rowMeans(h)) / nrow(h) + var(colMeans(h)) / ncol(h))
  logit <- log(a / (1 - a)); logit_se <- se / (a * (1 - a))
  r <- .statROC_run()$aucSummaryTable$asDF[1, ]
  expect_equal(r$auc, 49 / 72)
  expect_equal(c(r$ci_lower, r$ci_upper),
    plogis(logit + c(-1, 1) * qnorm(.975) * logit_se), tolerance = 1e-12)
  expect_equal(r$p, 2 * pnorm(-abs(logit / logit_se)), tolerance = 1e-12)
  reverse <- .statROC_run(direction = "<=")$aucSummaryTable$asDF[1, ]
  expect_equal(c(reverse$ci_lower, reverse$ci_upper), 1 - c(r$ci_upper, r$ci_lower))
  expect_equal(reverse$p, r$p)
  legacy <- .statROC_run(aucInference = "delong")$aucSummaryTable$asDF[1, ]
  expect_equal(c(legacy$ci_lower, legacy$ci_upper),
    pmax(0, pmin(1, a + c(-1, 1) * qnorm(.975) * se)))
  expect_equal(legacy$p, 2 * pnorm(-abs((a - .5) / se)))
})

test_that("zero empirical variance cannot produce exact AUC intervals", {
  d <- .statROC_data(); d$x <- seq_len(12); d$z <- 1
  for (method in c("logit", "delong")) {
    r <- .statROC_run(d, aucInference = method)$aucSummaryTable$asDF
    expect_equal(r$auc, c(1, .5))
    expect_true(all(is.na(r[c("ci_lower", "ci_upper", "p")])))
  }
})

test_that("patient summaries remove repeat-count effects from all selected analyses", {
  d <- .statROC_data(); d$ref <- d$z + sin(d$id)
  args <- list(delongTest = TRUE, calculateIDI = TRUE, calculateNRI = TRUE,
    refVar = "ref", idiNriBootRuns = 100, clinicalUtilityAnalysis = TRUE,
    treatmentThreshold = "0.1,0.9,0.2", compareClassifiers = TRUE,
    effectSizeAnalysis = TRUE, fixedSensSpecAnalysis = TRUE,
    bootstrapCI = TRUE, bootstrapReps = 100)
  original <- do.call(.statROC_run, c(list(data = d), args))
  repeated <- d[rep(seq_len(nrow(d)), times = rep(c(1, 4, 7), 4)), ]
  result <- do.call(.statROC_run, c(list(data = repeated,
    patientID = "id", analysisUnit = "patientMean"), args))
  for (name in c("aucSummaryTable", "delongComparisonTable", "idiTable", "nriTable",
    "clinicalUtilityTable", "decisionCurveTable", "rocComparisonTable", "effectSizeTable",
    "fixedSensSpecTable", "bootstrapCITable")) {
    expect_equal(result[[name]]$asDF, original[[name]]$asDF, info = name)
  }
  expect_equal(result$resultsTable$get("x")$asDF, original$resultsTable$get("x")$asDF)
  notes <- result$aucSummaryTable$.__enclos_env__$private$.notes
  expect_match(notes$independent_observations$note, "12 patients", fixed = TRUE)
})

test_that("mean and median operate on observed scores and preserve patient-level subgroups", {
  d <- .statROC_data(); d$group <- factor(rep(c("A", "B"), 6))
  repeated <- d[rep(seq_len(12), each = 3), ]
  repeated$x <- repeated$x + rep(c(0, 0, 9), 12)
  repeated$z[c(1, 2)] <- NA
  for (method in c("patientMean", "patientMedian")) {
    expected <- d
    if (method == "patientMean") expected$x <- expected$x + 3
    r <- .statROC_run(repeated, patientID = "id", analysisUnit = method, subGroup = "group")
    reference <- .statROC_run(expected, subGroup = "group")
    expect_equal(r$aucSummaryTable$asDF, reference$aucSummaryTable$asDF)
    for (key in reference$resultsTable$itemKeys)
      expect_equal(r$resultsTable$get(key)$asDF, reference$resultsTable$get(key)$asDF)
  }
})

test_that("patient identity uses original values rather than rounded numeric labels", {
  d <- .statROC_data(); d$id <- 1e15 + seq_len(nrow(d))
  expect_equal(length(unique(d$id)), nrow(d))
  expect_lt(length(unique(as.character(d$id))), nrow(d))
  result <- .statROC_run(d, patientID = "id", analysisUnit = "patientMean")
  expect_equal(result$aucSummaryTable$asDF, .statROC_run(d)$aucSummaryTable$asDF)
})

test_that("patient design errors stop inference with actionable messages", {
  d <- .statROC_data(); repeated <- d[rep(seq_len(12), each = 2), ]
  expect_error(.statROC_run(d, analysisUnit = "patientMean"), "Select a Patient ID")
  expect_error(.statROC_run(repeated, patientID = "id"), "Repeated Patient IDs")
  bad <- repeated; bad$id[1] <- NA
  expect_error(.statROC_run(bad, patientID = "id", analysisUnit = "patientMean"),
    "Patient ID is missing")
  bad <- repeated; bad$status[1] <- "positive"
  expect_error(.statROC_run(bad, patientID = "id", analysisUnit = "patientMean"),
    "conflicting outcome")
  # Conflict with an excluded third outcome must not silently disappear during filtering.
  bad$status <- as.character(bad$status); bad$status[1] <- "excluded"
  expect_error(.statROC_run(bad, patientID = "id", analysisUnit = "patientMean"),
    "conflicting outcome")
  bad <- repeated; bad$group <- factor(rep(c("A", "B"), 12))
  expect_error(.statROC_run(bad, patientID = "id", analysisUnit = "patientMean", subGroup = "group"),
    "conflicting or partly missing subgroup")
})

test_that("Holm p-values use the full displayed family including unavailable comparisons", {
  d <- .statROC_data(); d$third <- d$z + cos(d$id)
  r <- .statROC_run(d, dependentVars = c("x", "z", "third"), delongTest = TRUE)
  tab <- r$delongComparisonTable$asDF
  order <- order(tab$p); expected <- numeric(3)
  expected[order] <- pmin(1, cummax(tab$p[order] * c(3, 2, 1)))
  expect_equal(tab$p_holm, expected)
  expect_match(r$delongTest$content, "P.Holm", fixed = TRUE)
  d$third <- d$x
  tab <- .statROC_run(d, dependentVars = c("x", "z", "third"),
    delongTest = TRUE)$delongComparisonTable$asDF
  expect_true(is.na(tab$p_holm[2]))
  expect_equal(tab$p_holm[-2], pmin(1, 3 * tab$p[-2]))
})

test_that("refitted IDI and NRI do not expose unsupported significance tests", {
  d <- .statROC_data(); y <- as.integer(d$status == "positive")
  withr::local_seed(42)
  for (fun in list(ClinicoPath::bootstrapIDI, ClinicoPath::bootstrapNRI)) {
    estimate <- suppressWarnings(fun(d$x, d$z, y, n_boot = 100))
    expect_true(is.na(estimate$p_value))
    expect_true(all(is.finite(c(estimate$ci_lower, estimate$ci_upper))))
  }
  r <- .statROC_run(calculateIDI = TRUE, calculateNRI = TRUE, idiNriBootRuns = 100)
  for (table in list(r$idiTable, r$nriTable)) {
    expect_false("p" %in% names(table$asDF))
    expect_match(table$.__enclos_env__$private$.notes$descriptive_inference$note,
      "No p-value is reported", fixed = TRUE)
  }
})

test_that("patient summaries invalidate saved result and plot state", {
  for (option in c("patientID", "analysisUnit")) {
    r <- ClinicoPath:::psychopdaROCResults$new(ClinicoPath:::psychopdaROCOptions$new())
    r$decisionCurvePlot$addItem(key = "x")
    image <- r$decisionCurvePlot$get("x")
    image$fromProtoBuf(list(state = raw(), image = list(path = "previous.png")),
      oChanges = option, vChanges = character())
    expect_null(image$.__enclos_env__$private$.filePath)
  }
})
