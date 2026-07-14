audit_source_root <- function() {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = FALSE
  )
  testthat::skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")),
    "package source tree not available in the installed test context"
  )
  root
}

test_that("meddecide release metadata is synchronized", {
  root <- audit_source_root()
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  description_version <- unname(desc[1, "Version"])

  module_lines <- readLines(file.path(root, "jamovi", "0000.yaml"), warn = FALSE)
  module_version <- unname(trimws(sub(
    "^version:[[:space:]]*",
    "",
    grep("^version:", module_lines, value = TRUE)[1]
  )))

  expect_gte(package_version(description_version), package_version("1.0.0"))
  expect_identical(module_version, description_version)

  if (identical(desc[1, "Package"], "meddecide")) {
    citation_lines <- readLines(file.path(root, "CITATION.cff"), warn = FALSE)
    citation_version <- gsub(
      '["\' ]',
      "",
      trimws(sub(
        "^version:[[:space:]]*",
        "",
        grep("^version:", citation_lines, value = TRUE)[1]
      ))
    )
    expect_identical(citation_version, description_version)
  }
})

test_that("free-text sequential test names are compared literally", {
  root <- audit_source_root()
  source_lines <- readLines(
    file.path(root, "R", "sequentialtests.b.R"),
    warn = FALSE
  )
  call_line <- grep("test_similarity <- agrepl", source_lines, value = TRUE)

  expect_length(call_line, 1L)
  expect_match(call_line, "fixed = TRUE", fixed = TRUE)
})

test_that("AUC summary invalidation matches its fixed DeLong calculation", {
  root <- audit_source_root()
  result_lines <- readLines(
    file.path(root, "jamovi", "psychopdaROC.r.yaml"),
    warn = FALSE
  )
  start <- grep("^    - name: aucSummaryTable$", result_lines)
  next_item <- grep("^    - name:", result_lines)
  end <- next_item[next_item > start][1] - 1L
  block <- result_lines[start:end]
  clear_with <- sub(
    "^[[:space:]]*-[[:space:]]*",
    "",
    grep("^          - ", block, value = TRUE)
  )

  expect_identical(
    clear_with,
    c("dependentVars", "classVar", "positiveClass", "subGroup", "direction")
  )
})

test_that("audit-sensitive output strings remain export-safe", {
  root <- audit_source_root()
  output_files <- file.path(
    root,
    "R",
    c("agreement.b.R", "psychopdaroc.b.R")
  )
  output_text <- paste(
    unlist(lapply(output_files, readLines, warn = FALSE)),
    collapse = "\n"
  )
  unsafe_entities <- paste0(
    "&(",
    paste(
      c(
        "plusmn", "times", "ge", "le", "minus", "alpha", "rarr",
        "check", "cross", "nbsp", "ldquo", "rdquo"
      ),
      collapse = "|"
    ),
    ");"
  )

  expect_false(grepl(unsafe_entities, output_text, perl = TRUE))

  no_gold_text <- paste(
    readLines(file.path(root, "R", "nogoldstandard.b.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    no_gold_text,
    'jmvcore::format(.("Error in plot: {msg}")',
    fixed = TRUE
  )
  expect_match(
    no_gold_text,
    'jmvcore::format(.("Error in ggplot: {msg}")',
    fixed = TRUE
  )
})

test_that("meddecide sources do not request whole dependency namespaces", {
  root <- audit_source_root()
  source_files <- file.path(
    root,
    "R",
    c(
      "decision.b.R",
      "decisioncombine.b.R",
      "enhancedROC.b.R",
      "psychopdaroc.b.R",
      "nomogrammer.r"
    )
  )
  source_text <- paste(
    unlist(lapply(source_files, readLines, warn = FALSE)),
    collapse = "\n"
  )

  expect_false(grepl(
    "(?m)^#' @import (boot|cutpointr|dplyr|ggplot2|magrittr|pROC|scales)[[:space:]]*$",
    source_text,
    perl = TRUE
  ))
})

test_that("redundant utility definitions do not override canonical helpers", {
  root <- audit_source_root()
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  no_gold_text <- readLines(
    file.path(root, "R", "nogoldstandard.b.R"),
    warn = FALSE
  )

  expect_false(any(grepl("^.escapeVariableNames <-", no_gold_text)))

  if (identical(desc[1, "Package"], "meddecide")) {
    redundant_files <- file.path(
      root,
      "R",
      c("basic_metrics.R", "diagnostic_metrics.R", "stats_utils.R")
    )
    expect_false(any(file.exists(redundant_files)))
  }
})

test_that("fixed decision tables are populated without changing row structure", {
  data <- data.frame(
    gold = factor(c("pos", "pos", "neg", "neg", "pos", "neg")),
    test1 = factor(c("pos", "neg", "pos", "neg", "pos", "neg")),
    test2 = factor(c("pos", "pos", "neg", "neg", "neg", "neg"))
  )

  decision_result <- decision(
    data = data,
    gold = "gold",
    goldPositive = "pos",
    newtest = "test1",
    testPositive = "pos",
    goldNegative = "neg",
    testNegative = "neg",
    showMisclassified = TRUE
  )
  summary <- decision_result$confusionMatrixSummary$asDF
  expect_identical(
    as.character(summary$classification),
    c("True Positive", "False Positive", "False Negative", "True Negative")
  )
  expect_equal(summary$count, c(2, 1, 1, 2))

  combined_result <- decisioncombine(
    data = data,
    gold = "gold",
    goldPositive = "pos",
    test1 = "test1",
    test1Positive = "pos",
    test2 = "test2",
    test2Positive = "pos",
    test3 = NULL,
    test3Positive = NULL,
    showIndividual = TRUE
  )
  contingency <- combined_result$individualTest1$test1Contingency$asDF
  statistics <- combined_result$individualTest1$test1Stats$asDF

  expect_identical(
    as.character(contingency$testResult),
    c("Test Positive", "Test Negative", "Total")
  )
  expect_identical(
    as.character(statistics$statistic),
    c("Sensitivity", "Specificity", "PPV", "NPV")
  )

  pattern_result <- decisioncombine(
    data = data,
    gold = "gold",
    goldPositive = "pos",
    test1 = "test1",
    test1Positive = "pos",
    test2 = "test2",
    test2Positive = "pos",
    test3 = NULL,
    test3Positive = NULL,
    addPatternToData = TRUE
  )
  expect_true(pattern_result$addedPattern$isFilled())
})
