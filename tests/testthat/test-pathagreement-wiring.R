pa_wiring_data <- function() {
  withr::with_seed(812, {
    levels <- c("Low", "Intermediate", "High")
    truth <- sample(levels, 40, replace = TRUE)
    data <- as.data.frame(lapply(seq_len(6), function(i) {
      factor(ifelse(runif(40) < 0.25, sample(levels, 40, TRUE), truth),
             levels = levels, ordered = TRUE)
    }))
    names(data) <- c("Rater A", "Rater-A", "Patolog Ş", "Rater`D", "Rater/E", "Rater_F")
    data
  })
}

pa_wiring_analysis <- function(data, ...) {
  options <- getFromNamespace("pathagreementOptions", "ClinicoPath")
  class <- getFromNamespace("pathagreementClass", "ClinicoPath")
  analysis <- class$new(data = data, options = do.call(options$new, list(...)))
  analysis$run()
  analysis
}

test_that("the glossary is populated with selected raters", {
  data <- pa_wiring_data()
  analysis <- pa_wiring_analysis(data, vars = names(data), showStatisticalGlossary = TRUE)
  expect_match(analysis$results$statisticalGlossary$content, "Kappa")
})

test_that("reference and case IDs stay aligned with arbitrary row names", {
  data <- pa_wiring_data()
  raters <- names(data)
  data$reference <- data[[1]]
  data$case_id <- paste0("Specimen ", seq_len(nrow(data)))
  data[2, 1] <- NA
  rownames(data) <- paste0("source_", seq_len(nrow(data)))
  analysis <- pa_wiring_analysis(
    data, vars = raters, referenceStandard = "reference", caseID = "case_id",
    pathologyContext = TRUE, consensus = TRUE, show_consensus_table = TRUE,
    outlierAnalysis = TRUE, caseDifficultyScoring = TRUE
  )
  private <- analysis$.__enclos_env__$private
  expect_equal(private$.referenceStandardValues(), data$reference[-2])
  expect_equal(analysis$results$diagnosticAccuracyTable$asDF$accuracy[1], 100)
  expect_equal(analysis$results$consensusTable$asDF$case_id, data$case_id[-2])
  expect_equal(analysis$results$caseDifficultyTable$asDF$case_id, data$case_id[-2])
  expect_true(all(analysis$results$outlierTable$asDF$case_id %in% data$case_id[-2]))
})

test_that("missing reference ratings do not abort category agreement", {
  data <- pa_wiring_data()
  raters <- names(data)
  data$reference <- data[[1]]
  data$reference[c(2, 8)] <- NA
  analysis <- pa_wiring_analysis(data, vars = raters, referenceStandard = "reference",
                                 categoryAnalysis = TRUE)
  category <- analysis$results$categoryTable$asDF
  gold <- as.character(data$reference)
  for (i in seq_len(nrow(category))) {
    positive <- !is.na(gold) & gold == category$category[i]
    negative <- !is.na(gold) & gold != category$category[i]
    expect_equal(category$sensitivity[i], mean(vapply(data[raters], function(x) {
      mean(as.character(x)[positive] == category$category[i])
    }, numeric(1))))
    expect_equal(category$specificity[i], mean(vapply(data[raters], function(x) {
      mean(as.character(x)[negative] != category$category[i])
    }, numeric(1))))
  }
  data$reference[] <- NA
  empty <- pa_wiring_analysis(data, vars = raters, referenceStandard = "reference",
                              categoryAnalysis = TRUE)$results$categoryTable$asDF
  expect_true(all(is.na(empty$sensitivity)))
  expect_true(all(is.na(empty$specificity)))
})

test_that("clustering reference comparisons use retained cases", {
  data <- pa_wiring_data()
  raters <- names(data)
  data$reference <- data[[1]]
  data$reference[4] <- NA
  data[2, 1] <- NA
  analysis <- pa_wiring_analysis(data, vars = raters, referenceStandard = "reference",
                                 performClustering = TRUE)
  result <- analysis$results$referenceComparison$asDF
  groups <- analysis$.__enclos_env__$private$.style_clustering_results$cluster_assignments
  expect_equal(nrow(result), length(unique(groups)))
  for (group in sort(unique(groups))) {
    expected <- mean(vapply(which(groups == group), function(j) {
      mean(as.character(data[[j]][-c(2, 4)]) == as.character(data$reference[-c(2, 4)]))
    }, numeric(1))) * 100
    expect_equal(result$agreement_percent[result$style_group == paste("Group", group)],
                 expected)
  }
})

test_that("distance and automatic group choices reach every clustering output", {
  data <- pa_wiring_data()
  data[[2]] <- data[[1]]
  data[[4]] <- data[[3]]
  data[[6]] <- data[[5]]
  analysis <- pa_wiring_analysis(data, vars = names(data), performClustering = TRUE,
                                 styleDistanceMetric = "euclidean", autoSelectGroups = TRUE,
                                 nStyleGroups = 2)
  private <- analysis$.__enclos_env__$private
  clusters <- private$.style_clustering_results
  expected_distance <- stats::dist(t(vapply(data, as.integer, integer(nrow(data)))))
  expect_equal(as.matrix(clusters$distance_matrix), as.matrix(expected_distance))
  expect_equal(analysis$results$diagnosticStyleTable$asDF$style_group,
               paste("Style", clusters$cluster_assignments))
  expect_equal(length(unique(clusters$cluster_assignments)), 3L)
})

test_that("column names and factor labels survive agreement outputs", {
  data <- pa_wiring_data()
  attr(data[[1]], "label") <- "First pathologist"
  analysis <- pa_wiring_analysis(data, vars = names(data), sft = TRUE,
                                 pairwiseAnalysis = TRUE, categoryAnalysis = TRUE)
  expect_equal(unique(analysis$results$raterFrequencyTables$frequencyTable$asDF$rater),
               names(data))
  expect_equal(analysis$results$pairwiseTable$rowCount, choose(ncol(data), 2))
  expect_setequal(analysis$results$categoryTable$asDF$category, levels(data[[1]]))
  expect_identical(analysis$.__enclos_env__$private$.data_matrix[[1]], data[[1]])
})

test_that("empty and incomplete selections have a controlled response", {
  data <- pa_wiring_data()
  # jamovi provides a zero-column frame when no variables are selected.
  empty <- pa_wiring_analysis(data.frame(), vars = character())
  expect_match(empty$results$todo$content, "Inter-rater")
  class <- getFromNamespace("pathagreementClass", "ClinicoPath")
  options <- getFromNamespace("pathagreementOptions", "ClinicoPath")
  analysis <- class$new(data = data[FALSE, ], options = options$new(vars = names(data)))
  expect_error(analysis$.__enclos_env__$private$.run(), "at least 2 cases")
})

test_that("clustering heatmaps render the supplied categories", {
  data <- pa_wiring_data()
  analysis <- pa_wiring_analysis(data, vars = names(data), performClustering = TRUE)
  private <- analysis$.__enclos_env__$private
  expect_equal(private$.numericRatingMatrix(), vapply(data, as.integer, integer(nrow(data))))
  expect_identical(names(private$.diagnosisColors()), levels(data[[1]]))
  withr::local_options(lifecycle_verbosity = "quiet")
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = 1100, height = 900, type = "cairo")
  withr::defer({grDevices::dev.off(); unlink(file)})
  for (renderer in c(".diagnosticStyleDendrogram", ".diagnosticStyleHeatmap",
                     ".diagnosticStyleCombined", ".clusteringHeatmap")) {
    expect_no_warning(expect_true(private[[renderer]](
      image = analysis$results[[substring(renderer, 2)]],
      ggtheme = ggplot2::theme_grey(), theme = list()
    )))
  }
})
