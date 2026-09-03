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
    '.fmt(.("Error in plot: {msg}")',
    fixed = TRUE
  )
  expect_match(
    no_gold_text,
    '.fmt(.("Error in ggplot: {msg}")',
    fixed = TRUE
  )
})

test_that("agreement has no declared-but-unpopulated audit headings", {
  root <- audit_source_root()
  results_text <- paste(
    readLines(file.path(root, "jamovi", "agreement.r.yaml"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("name: allPairsKappaHeading", results_text, fixed = TRUE))
  expect_false(grepl("name: itemModalAgreementHeading", results_text, fixed = TRUE))
})

test_that("meddecide updater manifest includes all translation catalogs", {
  root <- audit_source_root()
  config_path <- file.path(root, "_updateModules_config.yaml")
  if (!file.exists(config_path)) {
    config_path <- file.path(root, "..", "ClinicoPathJamoviModule", "_updateModules_config.yaml")
  }
  testthat::skip_if_not(
    file.exists(config_path),
    "updater config not available"
  )
  config <- yaml::read_yaml(config_path)

  expect_setequal(
    unlist(config$modules$meddecide$i18n_files, use.names = FALSE),
    c("catalog.pot", "en.po", "tr.po")
  )
  expect_true(isTRUE(config$modes$copy_i18n_files))
})

test_that("meddecide Boolean controls use state labels rather than action labels", {
  root <- audit_source_root()
  analyses <- c(
    "agreement", "cotest", "decision", "decisioncalculator",
    "decisioncombine", "decisioncompare", "decisioncurve", "enhancedROC",
    "kappaSizeCI", "kappaSizeFixedN", "kappaSizePower", "lassologistic",
    "nogoldstandard", "psychopdaROC", "sequentialtests"
  )
  action_label <- paste0(
    "^(Show|Enable|Include|Export|Generate|Calculate|Highlight|Detect|Use|",
    "Apply|Add|Create|Perform)\\b"
  )

  option_violations <- character(0)
  ui_violations <- character(0)
  inspect_ui <- function(node, analysis) {
    if (!is.list(node))
      return(invisible(NULL))
    if (
      identical(node$type, "CheckBox") &&
      !is.null(node$label) &&
      grepl(action_label, node$label)
    ) {
      ui_violations <<- c(
        ui_violations,
        paste(analysis, node$name, node$label, sep = ": ")
      )
    }
    invisible(lapply(node, inspect_ui, analysis = analysis))
  }

  for (analysis in analyses) {
    options <- yaml::read_yaml(
      file.path(root, "jamovi", paste0(analysis, ".a.yaml"))
    )$options
    for (option in options) {
      if (
        identical(option$type, "Bool") &&
        grepl(action_label, option$title)
      ) {
        option_violations <- c(
          option_violations,
          paste(analysis, option$name, option$title, sep = ": ")
        )
      }
    }

    ui <- yaml::read_yaml(
      file.path(root, "jamovi", paste0(analysis, ".u.yaml"))
    )
    inspect_ui(ui, analysis)
  }

  expect_identical(option_violations, character(0))
  expect_identical(ui_violations, character(0))
})

test_that("IDI and NRI consolidate unstable calibration warnings", {
  actual <- rep(c(0, 1), each = 20)
  reference <- c(seq(-20, -1), seq(1, 20))
  candidate <- reference + seq_along(reference) / 100

  raw <- expect_silent(raw_to_prob(reference, actual, warn = FALSE))
  expect_gt(length(attr(raw, "fit_warnings")), 0L)

  idi_warnings <- character(0)
  set.seed(1708)
  idi <- withCallingHandlers(
    bootstrapIDI(candidate, reference, actual, n_boot = 30),
    warning = function(w) {
      idi_warnings <<- c(idi_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_lte(length(idi_warnings), 2L)
  expect_true(any(grepl("Logistic calibration", idi_warnings, fixed = TRUE)))
  expect_true(isTRUE(idi$fit_warning))
  expect_equal(idi$fit_warning_boots, 30L)

  nri_warnings <- character(0)
  set.seed(1708)
  nri <- withCallingHandlers(
    bootstrapNRI(candidate, reference, actual, n_boot = 30),
    warning = function(w) {
      nri_warnings <<- c(nri_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(nri_warnings, 1L)
  expect_match(nri_warnings, "Logistic calibration", fixed = TRUE)
  expect_true(isTRUE(nri$fit_warning))
  expect_equal(nri$fit_warning_boots, 30L)
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
    test3Positive = NULL
  )
  # isFilled() alone is what masked the original defect: the backend stored the values, so
  # this passed, while jmvcore's Output$enabled resolved through options$get("addedPattern")
  # -- an option that did not exist -- and jamovi never wrote the column. `enabled` is
  # driven by the Output control in the GUI and cannot be set through the R wrapper (a
  # `type: Output` option is not a wrapper argument), so the schema half of that check
  # lives in test-decisioncombine-release-review.R instead.
  expect_true(pattern_result$addedPattern$isFilled())
})
