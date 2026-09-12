pa_lifecycle_data <- function() {
  withr::with_seed(93, {
    levels <- c("Low", "Middle", "High")
    truth <- sample(levels, 48, TRUE)
    d <- as.data.frame(lapply(seq_len(4), function(i) {
      factor(ifelse(runif(48) < 0.2, sample(levels, 48, TRUE), truth),
             levels = levels, ordered = TRUE)
    }))
    names(d) <- c("Rater A", "Rater-B", "Patolog Ş", "Rater/D")
    d$reference <- factor(truth, levels = levels, ordered = TRUE)
    d$id <- paste0("Case ", seq_len(nrow(d)))
    d
  })
}

pa_lifecycle_new <- function(data, ...) {
  class <- getFromNamespace("pathagreementClass", "ClinicoPath")
  options <- getFromNamespace("pathagreementOptions", "ClinicoPath")
  class$new(
    data = data,
    options = do.call(options$new, list(...)),
    datasetId = "1",
    analysisId = 1L,
    revision = 1L
  )
}

# Direct R6 construction does not receive the protobuf option request that the
# jamovi desktop supplies. Add that request so .load() can distinguish unchanged
# options from real option changes when exercising the saved-analysis lifecycle.
pa_lifecycle_options_pb <- function(options) {
  getFromNamespace("initProtoBuf", "jmvcore")()
  encode <- function(value) {
    item <- RProtoBuf::P("jamovi.coms.AnalysisOption")$new()
    if (is.null(value)) {
      item$o <- 2L
    } else if (is.list(value) || length(value) != 1L) {
      nested <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
      nested$hasNames <- !is.null(names(value))
      if (!is.null(names(value))) nested$names <- names(value)
      nested$options <- lapply(value, encode)
      item$c <- nested
    } else if (is.logical(value)) {
      item$o <- as.integer(value)
    } else if (is.integer(value)) {
      item$i <- value
    } else if (is.numeric(value)) {
      item$d <- value
    } else {
      item$s <- value
    }
    item
  }
  option_names <- names(options$.__enclos_env__$private$.options)
  pb <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
  pb$hasNames <- TRUE
  pb$names <- option_names
  pb$options <- lapply(option_names, function(name) encode(options$option(name)$value))
  options$fromProtoBuf(pb)
  stopifnot(length(options$compProtoBuf(pb)) == 0)
  invisible(pb)
}

test_that("option-determined rows exist before computation and are only filled", {
  d <- pa_lifecycle_data()
  a <- pa_lifecycle_new(d, vars = names(d)[1:4], referenceStandard = "reference",
    sft = TRUE, icc = TRUE, kripp = TRUE, gwetAC = TRUE, pabak = TRUE,
    pairwiseAnalysis = TRUE, categoryAnalysis = TRUE, pathologyContext = TRUE,
    consensus = TRUE, show_consensus_table = TRUE, performClustering = TRUE,
    raterBiasAnalysis = TRUE, caseDifficultyScoring = TRUE,
    agreementTrendAnalysis = TRUE, agreementStabilityAnalysis = TRUE,
    sampleSizePlanning = TRUE, bootstrapSamples = 100)
  a$init()
  counts <- c(kappaTable = 1, iccTable = 1, krippTable = 1, gwetACTable = 2,
    pabakTable = 1, pairwiseTable = 6, categoryTable = 3,
    diagnosticAccuracyTable = 4, diagnosticStyleTable = 4, consensusSummary = 8,
    consensusTable = 48, raterBiasTable = 4, caseDifficultyTable = 48,
    agreementTrendTable = 5, stabilityTable = 2, sampleSizeTable = 3)
  for (name in names(counts)) {
    expect_equal(a$results[[name]]$rowCount, counts[[name]], info = name)
  }
  expect_no_error(a$run())
  for (name in names(counts)) {
    expect_equal(a$results[[name]]$rowCount, counts[[name]], info = name)
  }
  expect_true(all(is.finite(a$results$pairwiseTable$asDF$kappa)))
  expect_equal(sum(as.numeric(a$results$consensusSummary$asDF$value[-c(1, 2)])), 48)
})

test_that("jamovi saved results restore every plot without source data or recomputation", {
  skip_if_not_installed("RProtoBuf")
  getFromNamespace("initProtoBuf", "jmvcore")()
  d <- pa_lifecycle_data()
  args <- list(vars = names(d)[1:4], referenceStandard = "reference", caseID = "id",
    heatmap = TRUE, pairwiseAnalysis = TRUE, categoryAnalysis = TRUE,
    pathologyContext = TRUE, performClustering = TRUE, agreementTrendAnalysis = TRUE,
    raterBiasAnalysis = TRUE, caseDifficultyScoring = TRUE)
  a <- do.call(pa_lifecycle_new, c(list(data = d), args))
  pa_lifecycle_options_pb(a$options)
  a$run()
  state_file <- tempfile()
  a$.setStatePathSource(function() state_file)
  expect_no_error(a$.save())
  expect_true(file.exists(state_file))
  restored <- do.call(pa_lifecycle_new, c(list(data = d), args))
  pa_lifecycle_options_pb(restored$options)
  restored$.setStatePathSource(function() state_file)
  restored$init()
  restored$postInit()
  expect_no_error(restored$.load())
  p <- restored$.__enclos_env__$private
  p$.data <- NULL
  p$.data_matrix <- NULL
  expect_null(restored$data)
  expect_null(p$.data_matrix)
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = 1100, height = 900, type = "cairo")
  withr::defer({grDevices::dev.off(); unlink(c(file, state_file))})
  for (name in p$.plotNames()) {
    p$.data_matrix <- NULL
    p$.style_clustering_results <- NULL
    expect_true(p[[paste0(".", name)]](
      image = restored$results[[name]], ggtheme = ggplot2::theme_grey(), theme = list()
    ), info = name)
    expect_equal(p$.referenceStandardValues(), d$reference)
  }
  expect_equal(restored$results$kappaTable$asDF, a$results$kappaTable$asDF)
  expect_equal(p$.caseLabels(), d$id)
  for (name in p$.plotNames()) {
    expect_false(p[[paste0(".", name)]](
      image = list(state = NULL), ggtheme = ggplot2::theme_grey(), theme = list()
    ), info = name)
  }
})

test_that("reference bootstrap preserves the complete case as the sampling unit", {
  d <- pa_lifecycle_data()
  a <- pa_lifecycle_new(d, vars = names(d)[1:4], referenceStandard = "reference",
    performClustering = TRUE, bootstrap = TRUE, bootstrapCIType = "percentile",
    bootstrapSamples = 100, seed = 19)
  a$run()
  p <- a$.__enclos_env__$private
  groups <- p$.style_clustering_results$cluster_assignments
  dm <- d[1:4]
  # Independent calculation with irr using the same sampled case indices.
  statistic <- function(data, rows) {
    estimates <- vapply(seq_len(4), function(j) {
      irr::kappa2(data[rows, c(j, 5)])$value
    }, numeric(1))
    vapply(sort(unique(groups)), function(k) mean(estimates[groups == k]), numeric(1))
  }
  expected <- withr::with_seed(19, {
    indices <- replicate(100, sample.int(nrow(d), nrow(d), replace = TRUE))
    draws <- apply(indices, 2, function(rows) statistic(d[1:5], rows))
    t(apply(draws, 1, stats::quantile, c(0.025, 0.975), names = FALSE))
  })
  result <- a$results$referenceComparison$asDF
  expect_equal(unname(as.matrix(result[c("ci_lower", "ci_upper")])), unname(expected))
  expect_true(all(result$ci_lower <= result$ci_upper))
  expect_equal(p$.referenceBootstrap(dm, d$reference, groups)$intervals, t(expected))
  # Replicating a rater within a fixed group cannot create artificial precision.
  same <- p$.referenceBootstrap(dm[c(1, 1)], d$reference, c(1L, 1L))$intervals
  single <- p$.referenceBootstrap(dm[1], d$reference, 1L)$intervals
  expect_equal(same, single)
  off <- pa_lifecycle_new(d, vars = names(d)[1:4], referenceStandard = "reference",
                          performClustering = TRUE)
  off$run()
  expect_true(all(is.na(off$results$referenceComparison$asDF$ci_lower)))
})

test_that("correlation clustering distinguishes reversed ordinal ratings", {
  x <- rep(1:3, 16)
  d <- data.frame(a = ordered(x), b = ordered(4 - x), c = ordered(x))
  a <- pa_lifecycle_new(d, vars = names(d), performClustering = TRUE,
                        styleDistanceMetric = "correlation")
  a$run()
  fit <- a$.__enclos_env__$private$.style_clustering_results
  distances <- as.matrix(fit$distance_matrix)
  expect_equal(distances[1, 2], 2)
  expect_equal(distances[1, 3], 0)
  expect_equal(fit$hclust_object$method, "ward.D2")
  d[] <- lapply(d, factor, ordered = FALSE)
  nominal <- pa_lifecycle_new(d, vars = names(d), performClustering = TRUE,
                              styleDistanceMetric = "correlation")
  nominal$run()
  expect_match(nominal$results$warnings$content, "require ordered factors")
  expect_equal(as.matrix(nominal$.__enclos_env__$private$.style_clustering_results$distance_matrix)[1, 2],
               mean(x != 4 - x))
})

test_that("progress announcements are removed while explanatory warnings remain", {
  options <- getFromNamespace("pathagreementOptions", "ClinicoPath")
  expect_false("showProgressIndicators" %in% names(formals(options$public_methods$initialize)))
  expect_false("enhancedErrorGuidance" %in% names(formals(options$public_methods$initialize)))
  d <- pa_lifecycle_data()
  d[1:20, 1] <- NA
  a <- pa_lifecycle_new(d, vars = names(d)[1:4])
  expect_message(a$run(), NA)
  expect_match(a$results$warnings$content, "High missing data")
  expect_match(a$results$warnings$content, "data-severity='strong_warning'", fixed = TRUE)
  expect_match(a$results$warnings$content, "Strong warning")
})
