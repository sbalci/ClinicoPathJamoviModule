dc_original_analysis <- function(data, od = TRUE) {
  options <- ClinicoPath:::decisioncompareOptions$new(
    gold = "gold", goldPositive = "v1",
    test1 = "a", test1Positive = "v1",
    test2 = "b", test2Positive = "v1",
    test3 = if ("c" %in% names(data)) "c" else NULL,
    test3Positive = if ("c" %in% names(data)) "v1" else NULL,
    od = od, statComp = TRUE, opa = TRUE, ci = TRUE,
    plot = TRUE, radarplot = TRUE, heatmap = TRUE,
    showSummary = TRUE, showReportSentence = TRUE,
    showDescriptiveReport = TRUE
  )
  analysis <- ClinicoPath:::decisioncompareClass$new(options = options, data = data)
  analysis$run()
  analysis
}

dc_original_data <- function(k) {
  values <- paste0("v", seq_len(k))
  data.frame(gold = factor(values), a = factor(values), b = factor(rev(values)))
}

dc_original_cell_count <- function(analysis) {
  hits <- gregexpr("<td", analysis$results$text2$content, fixed = TRUE)[[1]]
  if (identical(hits[1], -1L)) 0L else length(hits)
}

test_that("original tables discard unused levels without changing diagnostic results", {
  data <- dc_original_data(2)[rep(1:2, 10), ]
  baseline <- dc_original_analysis(data)
  unused <- data
  for (name in names(unused)) levels(unused[[name]]) <- paste0("v", 1:120)
  analysis <- dc_original_analysis(unused)
  # Two 2x2 tables, each with two additional row-label cells.
  expect_equal(dc_original_cell_count(analysis), 12L)
  expect_identical(analysis$results$text2$content, baseline$results$text2$content)
  expect_identical(analysis$results$text1$content, baseline$results$text1$content)
  expect_equal(analysis$results$comparisonTable$asDF, baseline$results$comparisonTable$asDF)
  expect_equal(analysis$results$mcnemarTable$asDF, baseline$results$mcnemarTable$asDF)
  expect_false(grepl("Original Data Display Omitted", analysis$results$notices$content,
                     fixed = TRUE))
  expect_equal(nlevels(analysis$.__enclos_env__$private$.cached_processed_data$data$gold), 120)
  expect_equal(nlevels(unused$gold), 120)
})

test_that("original table limit allows 10000 cells and omits larger cross-tabs", {
  boundary <- dc_original_analysis(dc_original_data(100))
  expect_equal(dc_original_cell_count(boundary), 2L * 100L * 101L)
  expect_false(grepl("Original Data Display Omitted", boundary$results$notices$content,
                     fixed = TRUE))

  data <- dc_original_data(101)
  omitted <- dc_original_analysis(data)
  hidden <- dc_original_analysis(data, od = FALSE)
  expect_identical(omitted$results$text1$content, "")
  expect_identical(omitted$results$text2$content, "")
  expect_match(omitted$results$notices$content, "Original Data Display Omitted", fixed = TRUE)
  expect_match(omitted$results$notices$content, "10,000 cells", fixed = TRUE)
  expect_false(grepl("Original Data Display Omitted", hidden$results$notices$content,
                     fixed = TRUE))
  for (table in c("comparisonTable", "opaTable", "mcnemarTable", "diffTable",
                  "epirTable1", "epirTable2")) {
    expect_equal(omitted$results[[table]]$asDF, hidden$results[[table]]$asDF)
  }
  for (panel in c("summaryReport", "reportSentence", "clinicalReport")) {
    expect_identical(omitted$results[[panel]]$content, hidden$results[[panel]]$content)
    expect_true(nzchar(omitted$results[[panel]]$content))
  }
  for (plot in c("plot1", "plotRadar", "plotHeatmap")) {
    expect_equal(omitted$results[[plot]]$state, hidden$results[[plot]]$state)
    expect_false(is.null(omitted$results[[plot]]$state))
  }
})

test_that("preflight covers the optional third test before building any original table", {
  data <- dc_original_data(101)
  data$c <- data$a
  data$a <- factor(ifelse(data$gold == "v1", "v1", "v2"))
  data$b <- data$a
  analysis <- dc_original_analysis(data)
  expect_identical(analysis$results$text1$content, "")
  expect_identical(analysis$results$text2$content, "")
  expect_match(analysis$results$notices$content, "Original Data Display Omitted", fixed = TRUE)
  expect_equal(analysis$results$comparisonTable$getCell(rowKey = "test_3", col = "n")$value,
               101)
})

test_that("original display and its warning reset when data or display options change", {
  analysis <- dc_original_analysis(dc_original_data(2))
  expect_true(nzchar(analysis$results$text2$content))
  private <- analysis$.__enclos_env__$private
  private$.data <- dc_original_data(101)
  analysis$run()
  expect_identical(analysis$results$text2$content, "")
  expect_match(analysis$results$notices$content, "Original Data Display Omitted", fixed = TRUE)

  option <- analysis$options$option("od")
  option$value <- FALSE
  values <- analysis$options$values()
  values$od <- FALSE
  analysis$run()
  expect_false(grepl("Original Data Display Omitted", analysis$results$notices$content,
                     fixed = TRUE))
  option$value <- TRUE
  values$od <- TRUE
  private$.data <- dc_original_data(2)
  analysis$run()
  expect_equal(dc_original_cell_count(analysis), 12L)
  expect_false(grepl("Original Data Display Omitted", analysis$results$notices$content,
                     fixed = TRUE))
})
