find_oncopath_audit_root <- function() {
  candidates <- c(".", "..", "../..")
  for (candidate in candidates) {
    description <- file.path(candidate, "DESCRIPTION")
    if (!file.exists(description)) {
      next
    }
    package <- read.dcf(description, fields = "Package")[[1]]
    if (package %in% c("ClinicoPath", "OncoPath")) {
      return(normalizePath(candidate))
    }
  }
  NA_character_
}

oncopath_root <- find_oncopath_audit_root()
oncopath_file <- function(...) file.path(oncopath_root, ...)
read_oncopath <- function(...) {
  paste(readLines(oncopath_file(...), warn = FALSE), collapse = "\n")
}
skip_if_no_oncopath_source <- function() {
  skip_if(
    is.na(oncopath_root),
    "package source tree not available in the installed test context"
  )
}

test_that("OncoPath analyses and manifest use library-ready versions", {
  skip_if_no_oncopath_source()
  analysis_files <- file.path(
    oncopath_root,
    "jamovi",
    paste0(
      c("diagnosticmeta", "ihcheterogeneity", "swimmerplot", "waterfall"),
      ".a.yaml"
    )
  )

  for (analysis_file in analysis_files) {
    schema <- paste(readLines(analysis_file, warn = FALSE), collapse = "\n")
    expect_match(schema, "version: ['\"]1\\.0\\.0['\"]")
  }
  expect_match(read_oncopath("jamovi", "0000.yaml"), "version: 1\\.0\\.0")
})

test_that("disabled clinical presets and orphan stage migration files stay removed", {
  skip_if_no_oncopath_source()
  swimmer_schema <- read_oncopath("jamovi", "swimmerplot.a.yaml")
  swimmer_source <- read_oncopath("R", "swimmerplot.b.R")
  waterfall_schema <- read_oncopath("jamovi", "waterfall.a.yaml")
  waterfall_source <- read_oncopath("R", "waterfall.b.R")

  expect_false(grepl("clinicalPreset", swimmer_schema, fixed = TRUE))
  expect_false(grepl("clinicalPreset", swimmer_source, fixed = TRUE))
  expect_false(grepl("clinicalPreset", waterfall_schema, fixed = TRUE))
  expect_false(grepl("clinicalPreset", waterfall_source, fixed = TRUE))
  expect_false(grepl("Clinical Presets", waterfall_source, fixed = TRUE))

  orphan_names <- c(
    "stagemigration.a.yaml",
    "stagemigration.r.yaml",
    "stagemigration.u.yaml"
  )
  stage_schema_exists <- file.exists(file.path(oncopath_root, "jamovi", orphan_names))
  stage_source_exists <- file.exists(file.path(
    oncopath_root,
    "R",
    c("stagemigration.b.R", "stagemigration.h.R")
  ))
  package <- read.dcf(oncopath_file("DESCRIPTION"), fields = "Package")[[1]]
  if (package == "OncoPath") {
    expect_false(any(stage_schema_exists))
    expect_false(any(stage_source_exists))
  } else {
    expect_true(all(stage_schema_exists))
    expect_true(all(stage_source_exists))
  }
})

test_that("swimmer controls and errors follow jamovi UI and i18n conventions", {
  skip_if_no_oncopath_source()
  schema <- read_oncopath("jamovi", "swimmerplot.a.yaml")
  source <- read_oncopath("R", "swimmerplot.b.R")

  expect_match(schema, 'title: "Person-time analysis"', fixed = TRUE)
  expect_match(schema, 'title: "Response analysis"', fixed = TRUE)
  expect_false(grepl('title: "Include ', schema, fixed = TRUE))
  expect_false(grepl('.("Error:")', source, fixed = TRUE))
  expect_match(
    source,
    '.("<p><strong>Error:</strong> {message}</p>")',
    fixed = TRUE
  )
})

test_that("diagnostic SROC labels match the implemented mada model", {
  skip_if_no_oncopath_source()
  analysis_schema <- read_oncopath("jamovi", "diagnosticmeta.a.yaml")
  result_schema <- read_oncopath("jamovi", "diagnosticmeta.r.yaml")
  source <- read_oncopath("R", "diagnosticmeta.b.R")

  expect_match(analysis_schema, "Proportional-hazards SROC analysis", fixed = TRUE)
  expect_match(result_schema, "Proportional-Hazards SROC Model Results", fixed = TRUE)
  expect_match(source, "mada::phm(mada_data)", fixed = TRUE)
  expect_match(source, "Diagnostic accuracy parameter (theta)", fixed = TRUE)
  expect_match(source, "Between-study variance (tau^2)", fixed = TRUE)
  expect_false(grepl("HSROC Threshold", source, fixed = TRUE))
  expect_false(grepl("HSROC Accuracy", source, fixed = TRUE))
})

test_that("auxiliary meta-analysis honors options and guards infinite values", {
  skip_if_no_oncopath_source()
  source <- read_oncopath("R", "diagnosticmeta.b.R")

  expect_match(source, ".metaforMethod = function", fixed = TRUE)
  expect_match(source, ".metaforLevel = function", fixed = TRUE)
  expect_match(source, "level = rma_level", fixed = TRUE)
  expect_match(source, "method = rma_method", fixed = TRUE)
  expect_match(source, "is.finite(analysis_data$var_logit_sens)", fixed = TRUE)
  expect_match(source, "is.finite(analysis_data$var_logit_spec)", fixed = TRUE)
  expect_match(source, "Choose a zero-cell correction", fixed = TRUE)
})

test_that("diagnostic source remains ASCII-clean and renders real symbols", {
  skip_if_no_oncopath_source()
  path <- oncopath_file("R", "diagnosticmeta.b.R")
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)

  expect_false(any(as.integer(bytes) > 127L))
  source <- rawToChar(bytes)
  expect_false(grepl("&[A-Za-z]+;", source))
  expect_false(grepl("&#[0-9]+;", source))
  expect_match(source, '"[[GE]]" = intToUtf8(0x2265)', fixed = TRUE)
  expect_match(source, '"[[TIMES]]" = intToUtf8(0x00D7)', fixed = TRUE)
  expect_match(source, "private$.renderSymbols(html)", fixed = TRUE)
})

test_that("ggswim is selectively imported from a reproducible revision", {
  skip_if_no_oncopath_source()
  description <- read_oncopath("DESCRIPTION")
  swimmer_source <- read_oncopath("R", "swimmerplot.b.R")

  expect_match(
    description,
    "CHOP-CGTInformatics/ggswim@b3c67a0796a850624745a439cac33d981d83e1dc",
    fixed = TRUE
  )
  expect_match(swimmer_source, "@importFrom ggswim", fixed = TRUE)
  expect_false(grepl("@import ggswim", swimmer_source, fixed = TRUE))
})

test_that("standalone OncoPath metadata is internally consistent", {
  skip_if_no_oncopath_source()
  package <- read.dcf(oncopath_file("DESCRIPTION"), fields = "Package")[[1]]
  skip_if(package != "OncoPath")

  description <- read.dcf(oncopath_file("DESCRIPTION"))
  citation <- read_oncopath("CITATION.cff")

  expect_identical(unname(description[1, "Version"]), "1.0.0")
  expect_match(citation, 'title: "OncoPath:', fixed = TRUE)
  expect_match(citation, 'version: "1.0.0"', fixed = TRUE)
  expect_match(citation, "https://github.com/sbalci/OncoPath/", fixed = TRUE)

  removed_imports <- c("boot", "cmprsk", "haven", "Hmisc", "maxstat", "survminer", "survRM2")
  imports <- trimws(strsplit(description[1, "Imports"], ",", fixed = TRUE)[[1]])
  expect_false(any(removed_imports %in% imports))
})

test_that("diagnostic models honor estimator choices and zero-cell guards", {
  skip_if_not(exists("diagnosticmeta", mode = "function"))

  data <- data.frame(
    study = paste0("S", seq_len(8)),
    tp = c(45, 38, 52, 41, 33, 47, 50, 44),
    fp = c(5, 8, 6, 9, 7, 4, 10, 6),
    fn = c(7, 10, 5, 8, 11, 6, 9, 7),
    tn = c(93, 84, 87, 82, 89, 95, 81, 90),
    covariate = seq_len(8)
  )

  result <- diagnosticmeta(
    data = data,
    study = "study",
    true_positives = "tp",
    false_positives = "fp",
    false_negatives = "fn",
    true_negatives = "tn",
    covariate = "covariate",
    bivariate_analysis = TRUE,
    hsroc_analysis = TRUE,
    heterogeneity_analysis = TRUE,
    meta_regression = TRUE,
    confidence_level = 90,
    method = "fixed",
    zero_cell_correction = "none"
  )

  phm <- as.data.frame(result$hsrocresults)
  bivariate <- as.data.frame(result$bivariateresults)
  heterogeneity <- as.data.frame(result$heterogeneity)
  expect_identical(
    phm$parameter,
    c(
      "Diagnostic accuracy parameter (theta)",
      "Between-study variance (tau^2)"
    )
  )
  expect_equal(nrow(bivariate), 5)
  expect_true(all(is.finite(bivariate$estimate)))
  expect_true(all(is.finite(bivariate$ci_lower)))
  expect_true(all(is.finite(bivariate$ci_upper)))
  expect_equal(heterogeneity$tau_squared, c(0, 0))

  symbol_outputs <- c(
    result$interpretation$content,
    result$about$content,
    result$funnelplot_explanation$content
  )
  expect_true(any(grepl(intToUtf8(0x2265), symbol_outputs, fixed = TRUE)))
  expect_true(any(grepl(intToUtf8(0x00D7), symbol_outputs, fixed = TRUE)))
  expect_false(any(grepl("&#", symbol_outputs, fixed = TRUE)))

  zero_data <- data
  zero_data$tp[1] <- 0
  zero_data$fn[2] <- 0
  zero_data$tn[3] <- 0
  zero_result <- diagnosticmeta(
    data = zero_data,
    study = "study",
    true_positives = "tp",
    false_positives = "fp",
    false_negatives = "fn",
    true_negatives = "tn",
    bivariate_analysis = FALSE,
    hsroc_analysis = FALSE,
    heterogeneity_analysis = TRUE,
    method = "reml",
    zero_cell_correction = "none"
  )
  zero_heterogeneity <- as.data.frame(zero_result$heterogeneity)
  expect_equal(nrow(zero_heterogeneity), 2)
  expect_true(all(is.finite(zero_heterogeneity$q_statistic)))
})
