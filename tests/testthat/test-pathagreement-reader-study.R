pa_study_options <- function(...) utils::modifyList(list(readerWorkflow = "binary",
  readerPositive = "P", readerNegative = "N", referencePositive = "P",
  referenceNegative = "N", referenceStandard = "reference", readerNondiagnostic = TRUE,
  readerNondiagnosticLevel = "ND", readerAllObservedAgreement = TRUE,
  referenceType = "consensus"), list(...))

pa_study_data <- function(n = 40L, readers = 3L) {
  withr::with_seed(20261004, {
    d <- expand.grid(case = sprintf("C%03d", seq_len(n)),
      reader = paste0("R", seq_len(readers)), modality = c("Baseline", "AI"),
      stringsAsFactors = FALSE)
    case <- match(d$case, unique(d$case))
    truth <- rep(c("N", "P"), length.out = n)
    difficulty <- stats::runif(n, .05, .4)
    d$reference <- truth[case]
    d$rating <- ifelse(stats::runif(nrow(d)) < difficulty[case],
      ifelse(d$reference == "P", "N", "P"), d$reference)
    d$rating[stats::runif(nrow(d)) < .08] <- "ND"
    d$patient <- paste0("Patient", ceiling(case / 2))
    d
  })
}

pa_study_long_options <- function(...) pa_study_options(readerFormat = "long",
  caseID = "case", readerID = "reader", modality = "modality", rating = "rating",
  baselineModality = "Baseline", comparisonModality = "AI", readerModalityComparison = TRUE, ...)

test_that("article aggregate counts preserve binary contracts and observed denominators", {
  # A count fixture, not reconstructed patient records or reader-study inference.
  x <- rep(c("P", "P", "N", "N", "ND", "ND"), c(108, 13, 63, 67, 9, 25))
  g <- rep(c("P", "N", "P", "N", "P", "N"), c(108, 13, 63, 67, 9, 25))
  d <- data.frame(a = x, b = x, reference = g)
  p <- .paReaderPrepare(d, pa_study_options(vars = c("a", "b")))
  a <- .paReaderAccuracy(p$calls[, 1], p$reference)
  expect_equal(unname(a[c("n", "tp", "fp", "fn", "tn")]), c(251, 108, 13, 63, 67))
  expect_equal(a[["sensitivity"]], 108 / 171)
  expect_equal(a[["specificity"]], 67 / 80)
  tab <- .paReaderObserved(p$calls[, 1], p$reference, p$status[, 1])
  expect_equal(tab$numerator, c(108, 67, 175, 251))
  expect_equal(tab$denominator, c(180, 105, 285, 285))
  r <- .paReaderReport(p)
  expect_equal(r$readerAccuracyTable$metric[1:3], c("PPA", "NPA", "Mean PPA and NPA"))
  expect_equal(sum(r$readerAvailability$n[r$readerAvailability$reader == "a" &
    r$readerAvailability$result == "nondiagnostic"]), 34)
})

test_that("missing, unavailable reference and all-nondiagnostic readings remain distinct", {
  d <- data.frame(a = c("ND", NA, "ND", "ND"), b = c("ND", "ND", NA, "ND"),
    reference = c("P", "N", "I", NA))
  p <- .paReaderPrepare(d, pa_study_options(vars = c("a", "b")))
  expect_equal(p$status[, 1], c("nondiagnostic", "missing", "nondiagnostic", "nondiagnostic"), ignore_attr = TRUE)
  expect_equal(p$reference_state, c("eligible", "eligible", "excluded_level", "missing"))
  r <- .paReaderReport(p)
  z <- r$readerObservedAgreement[r$readerObservedAgreement$reader == "a", ]
  expect_equal(z$denominator, c(1, 0, 1, 1))
  expect_equal(z$estimate[c(1, 3, 4)], c(0, 0, 0))
  expect_true(is.na(z$estimate[2]))
  d$a[1] <- "UNMAPPED"
  expect_error(.paReaderPrepare(d, pa_study_options(vars = c("a", "b"))), "Unmapped reader")
})

test_that("ordinal nondiagnostic status is removed before threshold and weights", {
  lv <- c("I", "II", "III", "IV", "V", "VI")
  d <- data.frame(a = ordered(c("I", "II", "III", "IV"), lv),
    b = ordered(c("II", "III", "I", "VI"), lv), reference = c("N", "N", "P", "P"))
  o <- pa_study_options(vars = c("a", "b"), readerWorkflow = "ordinal_cutoff",
    ordinalCutoff = "III", readerNondiagnosticLevel = "I", readerTransitions = TRUE,
    transitionBaseline = "a", transitionComparison = "b")
  p <- .paReaderPrepare(d, o)
  expect_equal(p$scale, lv[-1])
  expect_true(is.na(p$calls[1, 1]))
  r <- .paReaderTransitions(p)
  expect_equal(r$readerTransitionsTable$numerator, c(0, 2, 0, 1, 0, 1, 1, 0, 0))
  expect_equal(nrow(r$readerTransitionMatrix), 36)
  expect_equal(sum(r$readerTransitionMatrix$n[r$readerTransitionMatrix$baseline == "V"]), 0)
  expect_equal(sum(r$readerTransitionMatrix$n), 4)
})

test_that("long validation rejects duplicates, missing IDs and inconsistent references", {
  d <- pa_study_data()
  o <- pa_study_long_options()
  expect_error(.paReaderPrepare(rbind(d, d[1, ]), o), "Duplicate")
  bad <- d; bad$case[1] <- NA
  expect_error(.paReaderPrepare(bad, o), "nonempty")
  bad <- d; bad$reference[1] <- "I"
  expect_error(.paReaderPrepare(bad, o), "consistent")
  bad <- d; bad$patient[1] <- "different"
  expect_error(.paReaderPrepare(bad, pa_study_long_options(readerCluster = "patient")), "consistent")
})

test_that("complete-grid populations exclude missing readings but retain declared abstentions", {
  d <- pa_study_data(20, 2)
  d$rating <- d$reference
  d$rating[1] <- "ND"
  d$rating[2] <- NA
  d <- d[-3, ]
  o <- pa_study_long_options()
  p <- .paReaderPrepare(d, o)
  expect_equal(sum(.paReaderModalityPopulation(p)), 17)
  p$options$modalityPopulation <- "all_observed"
  expect_equal(sum(.paReaderModalityPopulation(p)), 18)
  r <- .paReaderModalityReport(p)
  expect_true(all(r$readerModalityContrast$cases == 18))
  expect_true(all(r$readerModalityContrast$excluded_cases == 2))
})

test_that("modality contrasts preserve pairing, fixed reader weights and row-order invariance", {
  d <- pa_study_data(40, 3)
  o <- pa_study_long_options(readerBootstrap = TRUE, bootstrapSamples = 100L,
    modalityPopulation = "all_observed", readerCluster = "patient")
  p <- .paReaderPrepare(d, o)
  q <- .paReaderPrepare(d[rev(seq_len(nrow(d))), ], o)
  a <- .paReaderModalityReport(p); b <- .paReaderModalityReport(q)
  expect_equal(a, b)
  expect_true(all(a$readerModalityContrast$units == 20))
  expect_true(all(a$readerModalityContrast$attempted == 100))
  expect_equal(a$readerModalityContrast$difference,
    100 * (a$readerModalitySummary$estimate[5:8] - a$readerModalitySummary$estimate[1:4]))
  # A replicate may sample the same case twice; these rows must not be deduplicated.
  z <- .paReaderModalityStatistic(p, c(1, 1, 2))
  expect_equal(z$tables[[1]]$denominator[3], 9)
})

test_that("identical modalities have undefined constant bootstrap intervals, not fake precision", {
  d <- pa_study_data(30, 1)
  d$rating <- d$reference
  o <- pa_study_long_options(readerBootstrap = TRUE, bootstrapSamples = 50L)
  r <- .paReaderModalityReport(.paReaderPrepare(d, o))$readerModalityContrast
  expect_equal(r$difference, c(0, 0, 0))
  expect_true(all(is.na(r$lower)))
  expect_true(all(grepl("Constant", r$status)))
})

test_that("layout allocates exactly every new report row before computation", {
  d <- pa_study_data(20, 2)
  p <- .paReaderPrepare(d, pa_study_long_options(modalityPopulation = "all_observed"))
  r <- .paReaderReport(p); layout <- .paReaderLayout(p$options, p)
  for (name in c("readerSettings", "readerAvailability", "readerMapping",
    "readerObservedAgreement", "readerModalitySummary", "readerModalityContrast")) {
    expect_equal(nrow(r[[name]]), length(layout[[name]]), info = name)
  }
})

test_that("compiled reader analysis exposes new tables and comparator labels", {
  skip_if_not("ClinicoPath" %in% loadedNamespaces())
  constructor <- getFromNamespace("pathagreementOptions", "ClinicoPath")
  skip_if_not("readerFormat" %in% names(formals(constructor$public_methods$initialize)))
  d <- pa_study_data(24, 2)
  args <- pa_study_long_options(readerBootstrap = TRUE, bootstrapSamples = 100L,
    modalityPopulation = "all_observed", readerCluster = "patient")
  analysis <- getFromNamespace("pathagreementClass", "ClinicoPath")$new(data = d,
    options = do.call(constructor$new, args), datasetId = "1", analysisId = 1L, revision = 1L)
  expect_no_error(analysis$run())
  expect_equal(analysis$results$readerModalityContrast$rowCount, 4)
  expect_equal(analysis$results$readerAvailability$rowCount, 64)
  expect_true(all(c("PPA", "NPA") %in% analysis$results$readerAccuracyTable$asDF$metric))
  expect_match(analysis$results$pathReferenceDefinition$asDF$value[1], "consensus")
  expect_true(all(analysis$results$readerModalityContrast$asDF$units == 12))
})
