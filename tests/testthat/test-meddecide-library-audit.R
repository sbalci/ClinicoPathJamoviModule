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

# library-audit 2026-09-16 meddecide [HIGH] DONE: renderers draw from image$state; export and .omv reopen
#   rebuild the analysis without .run(), so private$ fields filled there are NULL (guide section 15)
test_that("psychopdaROC meta-analysis forest plot redraws from state alone", {
  skip_if_not(exists("psychopdaROCClass"))
  skip_if_not_installed("pROC")
  set.seed(20260916)
  n <- 160
  cls <- factor(rep(c("Disease", "Healthy"), each = n / 2))
  d <- data.frame(
    marker1 = rnorm(n, ifelse(cls == "Disease", 1.2, 0)),
    marker2 = rnorm(n, ifelse(cls == "Disease", 0.9, 0)),
    marker3 = rnorm(n, ifelse(cls == "Disease", 0.6, 0)),
    status = cls
  )
  ran <- suppressWarnings(psychopdaROC(
    data = d, dependentVars = c("marker1", "marker2", "marker3"), classVar = "status",
    positiveClass = "Disease", refVar = NULL, metaAnalysis = TRUE, forestPlot = TRUE,
    overrideMetaAnalysisWarning = TRUE
  ))
  state <- ran$metaAnalysisForestPlot$get(key = "forestPlot")$state
  expect_true(is.data.frame(state$data) && nrow(state$data) == 4)

  # What jamovi does on export: a fresh analysis, no .run(), each image's state restored.
  analysis <- ran$.__enclos_env__$private$.parent
  fresh <- psychopdaROCClass$new(options = analysis$options, data = NULL)
  fresh$.setReadDatasetHeaderSource(function(vars) d[0, intersect(unlist(vars), names(d)), drop = FALSE])
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  quiet(fresh$init())
  image <- fresh$results$metaAnalysisForestPlot$addItem(key = "forestPlot")
  image$setState(state)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  drawn <- quiet(fresh$.createImage(image$.__enclos_env__$private$.renderFun, image))
  expect_true(isTRUE(drawn))
})

# library-audit 2026-09-16 meddecide [MEDIUM] DONE: ROC overlays (smoothed curve, confidence bands,
#   quantile CIs) are drawn from image$state, so an export shows what the live plot shows
test_that("psychopdaROC overlays survive a redraw without .run()", {
  skip_if_not(exists("psychopdaROCClass"))
  skip_if_not_installed("pROC")
  set.seed(20260917)
  n <- 160
  cls <- factor(rep(c("Disease", "Healthy"), each = n / 2))
  d <- data.frame(
    marker1 = rnorm(n, ifelse(cls == "Disease", 1.2, 0)),
    marker2 = rnorm(n, ifelse(cls == "Disease", 0.7, 0)),
    status = cls
  )
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  ran <- quiet(psychopdaROC(
    data = d, dependentVars = c("marker1", "marker2"), classVar = "status",
    positiveClass = "Disease", refVar = NULL, plotROC = TRUE, combinePlots = FALSE,
    showConfidenceBands = TRUE, quantileCIs = TRUE, rocSmoothingMethod = "binormal"
  ))
  analysis <- ran$.__enclos_env__$private$.parent

  # Count the ggplot layers the renderer hands to print(): the overlays are extra layers.
  layers <- NULL
  suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                         tracer = bquote(assign("n", length(plot$layers), envir = .(environment())))))
  on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))), add = TRUE)
  draw <- function(obj, image) {
    n <<- NA_integer_
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    quiet(obj$.createImage(image$.__enclos_env__$private$.renderFun, image))
    n
  }
  n <- NA_integer_
  live_image <- analysis$results$plotROC$get(key = "marker1")
  live <- draw(analysis, live_image)

  fresh <- psychopdaROCClass$new(options = analysis$options, data = NULL)
  fresh$.setReadDatasetHeaderSource(function(vars) d[0, intersect(unlist(vars), names(d)), drop = FALSE])
  quiet(fresh$init())
  plots <- fresh$results$plotROC
  image <- if ("marker1" %in% plots$itemKeys) plots$get(key = "marker1") else plots$addItem(key = "marker1")
  image$setState(live_image$state)
  exported <- draw(fresh, image)

  expect_true(is.finite(live) && live > 1)
  expect_identical(exported, live)
})

# library-audit 2026-09-16 meddecide [LOW] DONE: requiresData: true only where the renderer, or a
#   private$ helper it calls, reads self$data (guide section 15)
test_that("meddecide images declare requiresData exactly when their renderer reads self$data", {
  root <- audit_source_root()
  # jmvcore nulls the data once .run() returns and re-reads it for a redraw or export only for an
  # Image that asks. Missing flag: the plot errors on resize/.omv reopen/export. Surplus flag: the
  # whole dataset is re-read before every redraw and export for a plot drawn from image$state.
  analyses <- c("agreement", "cotest", "decision", "decisioncalculator", "decisioncombine",
                "decisioncompare", "decisioncurve", "enhancedROC", "kappaSizeCI", "kappaSizeFixedN",
                "kappaSizePower", "lassologistic", "nogoldstandard", "psychopdaROC", "sequentialtests")
  images <- 0
  mismatches <- character()
  for (name in analyses) {
    src <- sub("#.*$", "", readLines(file.path(root, "R", paste0(name, ".b.R")), warn = FALSE))
    heads <- grep("^\\s*\\.[A-Za-z_][A-Za-z0-9_.]*\\s*=\\s*function\\s*\\(", src)
    ends <- c(heads[-1] - 1, length(src))
    body <- stats::setNames(
      lapply(seq_along(heads), function(i) paste(src[heads[i]:ends[i]], collapse = "\n")),
      sub("^\\s*(\\.[A-Za-z_][A-Za-z0-9_.]*).*$", "\\1", src[heads])
    )
    walk <- function(x, label = NULL) {
      if (!is.list(x)) return(invisible())
      if (!is.null(x[["name"]])) label <- x[["name"]]
      if (identical(x[["type"]], "Image") && !is.null(x[["renderFun"]])) {
        images <<- images + 1
        todo <- x[["renderFun"]]
        seen <- character()
        reads <- FALSE
        while (length(todo) && !reads) {
          f <- todo[1]
          todo <- todo[-1]
          if (f %in% seen || !f %in% names(body)) next
          seen <- c(seen, f)
          reads <- grepl("self\\$data\\b|self\\$readDataset|private\\$\\.data\\b", body[[f]], perl = TRUE)
          calls <- regmatches(body[[f]], gregexpr("private\\$\\.[A-Za-z_][A-Za-z0-9_.]*(?=\\s*\\()",
                                                  body[[f]], perl = TRUE))[[1]]
          todo <- c(todo, sub("^private\\$", "", calls))
        }
        if (reads != isTRUE(x[["requiresData"]]))
          mismatches <<- c(mismatches, sprintf("%s:%s (requiresData %s, renderer %s self$data)",
                                               name, label, isTRUE(x[["requiresData"]]),
                                               if (reads) "reads" else "never reads"))
      }
      for (item in x) walk(item, label)
    }
    walk(yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".r.yaml"))))
  }
  expect_gt(images, 30)
  expect_equal(mismatches, character(0))
})

# library-audit 2026-09-16 meddecide [LOW] DONE: seeded resampling uses withr::local_seed(); an analysis
#   leaves the session's random-number stream as it found it (every analysis shares one R process)
test_that("seeded meddecide analyses leave the caller's RNG stream untouched", {
  root <- audit_source_root()
  # Static: no set.seed() call in any meddecide backend (strings and comments do not count).
  analyses <- c("agreement", "cotest", "decision", "decisioncalculator", "decisioncombine",
                "decisioncompare", "decisioncurve", "enhancedROC", "kappaSizeCI", "kappaSizeFixedN",
                "kappaSizePower", "lassologistic", "nogoldstandard", "psychopdaROC", "sequentialtests")
  bare <- character()
  for (name in analyses) {
    pd <- utils::getParseData(parse(file.path(root, "R", paste0(name, ".b.R")), keep.source = TRUE))
    hit <- pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "set.seed"
    bare <- c(bare, sprintf("%s.b.R:%d", name, pd$line1[hit]))
  }
  expect_equal(bare, character(0))

  # Behaviour: the stream after a seeded run equals the stream without it.
  skip_if_not(exists("agreementClass") && exists("lassologisticClass"))
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  set.seed(1)
  n <- 60
  base <- rnorm(n, 50, 10)
  raters <- data.frame(r1 = base + rnorm(n, 0, 3), r2 = base + rnorm(n, 0, 3), r3 = base + rnorm(n, 0, 3))
  X <- as.data.frame(matrix(rnorm(n * 5), n, 5, dimnames = list(NULL, paste0("x", 1:5))))
  X$y <- factor(ifelse(X$x1 + rnorm(n) > 0, "Yes", "No"))
  stream_after <- function(analysis) {
    set.seed(99)
    quiet(analysis$run())
    stats::runif(3)
  }
  set.seed(99)
  untouched <- stats::runif(3)
  agree <- agreementClass$new(options = agreementOptions$new(
    vars = c("r1", "r2", "r3"), kripp = TRUE, bootstrap = TRUE, nBoot = 100, robinsonA = TRUE, tdi = TRUE),
    data = raters)
  quiet(agree$init())
  expect_identical(stream_after(agree), untouched)
  lasso <- lassologisticClass$new(options = lassologisticOptions$new(
    outcome = "y", outcomeLevel = "Yes", explanatory = paste0("x", 1:5), nfolds = 3), data = X)
  quiet(lasso$init())
  expect_identical(stream_after(lasso), untouched)
})

# Found while proving the requiresData removals (2026-09-16 meddecide): with as many clusters as raters
# (3 raters, the default nClusters of 3) every rater was its own cluster and the dendrogram renderer
# stopped in rect.hclust() with "k must be between 2 and 2". The same held for cases.
test_that("rater and case clustering cap k below the number of units, and dendrograms draw", {
  skip_if_not(exists("agreementClass"))
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  set.seed(7)
  base <- rnorm(30, 50, 10)
  raters <- data.frame(r1 = base + rnorm(30, 0, 2), r2 = base + rnorm(30, 0, 4), r3 = base + rnorm(30, 0, 8))
  draw <- function(analysis, image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    tryCatch(isTRUE(quiet(analysis$.createImage(image$.__enclos_env__$private$.renderFun, image))),
             error = function(e) conditionMessage(e))
  }

  rc <- agreementClass$new(options = agreementOptions$new(
    vars = c("r1", "r2", "r3"), raterClustering = TRUE, clusterMethod = "hierarchical",
    showDendrogram = TRUE, nClusters = 3), data = raters)
  quiet(rc$init()); quiet(rc$run())
  expect_identical(draw(rc, rc$results$raterDendrogram), TRUE)
  expect_true("k_capped" %in% names(rc$results$raterClusterTable$notes))
  expect_equal(length(unique(rc$results$raterDendrogram$state$cluster_labels[[1]])), 2)

  cases <- raters[1:3, ]
  cc <- agreementClass$new(options = agreementOptions$new(
    vars = c("r1", "r2", "r3"), caseClustering = TRUE, caseClusterMethod = "hierarchical",
    showCaseDendrogram = TRUE, nCaseClusters = 3), data = cases)
  quiet(cc$init()); quiet(cc$run())
  expect_identical(draw(cc, cc$results$caseDendrogram), TRUE)
  expect_true("k_capped" %in% names(cc$results$caseClusterTable$notes))
})

# library-audit 2026-09-16 meddecide [LOW] DONE: every Imports package is used by the code; unused ones
#   (ggraph, igraph, Matrix, glue, htmlTable) installed with every copy of the module. rlang stays: `.data`.
test_that("meddecide declares no dependency its code never uses", {
  root <- audit_source_root()
  attached <- c("base", "stats", "utils", "graphics", "grDevices", "methods", "datasets")
  analyses <- c("agreement", "cotest", "decision", "decisioncalculator", "decisioncombine",
                "decisioncompare", "decisioncurve", "enhancedROC", "kappaSizeCI", "kappaSizeFixedN",
                "kappaSizePower", "lassologistic", "nogoldstandard", "psychopdaROC", "sequentialtests")
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  if (identical(unname(desc[1, "Package"]), "meddecide")) {
    # The module: DESCRIPTION Imports against every shipped R file and the NAMESPACE.
    files <- setdiff(list.files(file.path(root, "R"), "\\.[Rr]$", full.names = TRUE),
                     file.path(root, "R", "00jmv.R"))
    declared <- trimws(sub("\\(.*", "", strsplit(unname(desc[1, "Imports"]), ",")[[1]]))
    ns <- parseNamespaceFile(basename(root), dirname(root))$imports
    whole <- vapply(Filter(function(x) length(x) == 1, ns), function(x) x[[1]], "")
    from <- Filter(function(x) length(x) == 2, ns)
    imported <- split(unlist(lapply(from, function(x) unname(x[[2]]))),
                      rep(vapply(from, function(x) x[[1]], ""), lengths(lapply(from, `[[`, 2))))
  } else {
    # The umbrella: the packages the meddecide backends' own @importFrom tags pull into the module.
    files <- file.path(root, "R", paste0(analyses, ".b.R"))
    tags <- regmatches(unlist(lapply(files, readLines, warn = FALSE)),
                       regexpr("^#' @importFrom [[:alnum:].]+ .*$", unlist(lapply(files, readLines, warn = FALSE))))
    parts <- strsplit(sub("^#' @importFrom ", "", tags), "[[:space:]]+")
    declared <- unique(vapply(parts, `[`, "", 1))
    imported <- tapply(unlist(lapply(parts, `[`, -1)), rep(vapply(parts, `[`, "", 1), lengths(parts) - 1),
                       function(x) unique(x), simplify = FALSE)
    whole <- character()
  }
  declared <- setdiff(declared[nzchar(declared)], attached)
  pd <- do.call(rbind, lapply(files, function(f) {
    d <- utils::getParseData(parse(f, keep.source = TRUE))
    d <- d[d$terminal, c("line1", "col1", "token", "text")]
    d[order(d$line1, d$col1), ]
  }))
  loaders <- which(pd$token == "SYMBOL_FUNCTION_CALL" &
                     pd$text %in% c("requireNamespace", "loadNamespace", "library", "require"))
  loaded <- gsub("[\"'`]", "", pd$text[pmin(loaders + 2, nrow(pd))])
  symbols <- pd$text[pd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SPECIAL")]
  strings <- pd$text[pd$token == "STR_CONST"]
  used <- vapply(declared, function(pkg) {
    pkg %in% c(pd$text[pd$token == "SYMBOL_PACKAGE"], whole, loaded) ||
      any(grepl(paste0(pkg, "::"), strings, fixed = TRUE)) ||
      any(gsub("`", "", imported[[pkg]]) %in% symbols)
  }, logical(1))
  expect_equal(declared[!used], character(0), info = paste(declared[!used], collapse = ", "))
})

# A result that changes with the random seed names that seed next to it, so a reader can see which
# resamples produced it and change them (Random seed option). Deterministic outputs show no seed.
test_that("every seed-dependent meddecide result shows the seed that drew it", {
  skip_if_not(exists("agreementClass") && exists("psychopdaROCClass"))
  skip_if_not_installed("pROC")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  shown <- "Random seed: 777"
  notes <- function(table) unname(vapply(table$notes, function(n) n$note, ""))
  run <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    quiet(a$init())
    quiet(a$run())
    a
  }
  set.seed(11)
  n <- 120
  truth <- factor(ifelse(stats::runif(n) < 0.4, "Yes", "No"), levels = c("No", "Yes"))
  m1 <- stats::rnorm(n, ifelse(truth == "Yes", 1.2, 0))
  m2 <- stats::rnorm(n, ifelse(truth == "Yes", 0.6, 0))
  d <- data.frame(truth, m1, m2, m3 = stats::rnorm(n), m4 = stats::rnorm(n), p = stats::plogis(m1 - 0.5))
  test <- function(sens, spec) factor(ifelse(ifelse(truth == "Yes", stats::runif(n) < sens, stats::runif(n) > spec),
                                             "Positive", "Negative"), levels = c("Negative", "Positive"))
  d$t1 <- test(0.85, 0.90); d$t2 <- test(0.75, 0.85); d$t3 <- test(0.80, 0.80)

  # agreement: Krippendorff bootstrap, TDI, and k-means rater and case clustering
  ag <- run("agreement", data.frame(r1 = m1, r2 = m1 + stats::rnorm(n, 0, 0.3), r3 = m1 + stats::rnorm(n, 0, 0.6)),
            vars = c("r1", "r2", "r3"), seed = 777, kripp = TRUE, bootstrap = TRUE, nBoot = 100, tdi = TRUE,
            raterClustering = TRUE, clusterMethod = "kmeans", nClusters = 2,
            caseClustering = TRUE, caseClusterMethod = "kmeans", nCaseClusters = 2)
  for (t in c("krippTable", "tdiTable", "raterClusterTable", "caseClusterTable"))
    expect_true(shown %in% notes(ag$results[[t]]), info = paste("agreement", t))
  hier <- run("agreement", data.frame(r1 = m1, r2 = m1 + 0.1, r3 = m1 - 0.1)[1:30, ],
              vars = c("r1", "r2", "r3"), seed = 777, raterClustering = TRUE, clusterMethod = "hierarchical")
  expect_false(shown %in% notes(hier$results$raterClusterTable))

  # decisioncurve: the bootstrap bands are named in the plot caption
  dc <- run("decisioncurve", d, outcome = "truth", outcomePositive = "Yes", models = "p",
            confidenceIntervals = TRUE, bootReps = 100, seed = 777)
  caption <- NULL
  suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                         tracer = bquote(assign("caption", plot$labels$caption, envir = .(environment())))))
  on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))), add = TRUE)
  grDevices::pdf(NULL)
  quiet(dc$.__enclos_env__$private$.plotDCA(dc$results$dcaPlot, ggplot2::theme_grey(), list()))
  grDevices::dev.off()
  expect_match(caption %||% "", shown, fixed = TRUE)

  # enhancedROC: bootstrap AUC intervals, bootstrap comparisons, internal validation
  er <- run("enhancedROC", d, outcome = "truth", positiveClass = "Yes", predictors = c("m1", "m2"),
            useBootstrap = TRUE, bootstrapSamples = 100, analysisType = "comparative",
            pairwiseComparisons = TRUE, statisticalComparison = TRUE,
            comparisonMethod = "bootstrap", internalValidation = TRUE, validationMethod = "bootstrap", seed = 777)
  for (t in c("aucSummary", "rocComparisons", "statisticalSummary"))
    expect_true(shown %in% notes(er$results$results[[t]]), info = paste("enhancedROC", t))
  expect_match(er$results$results$analysisSummary$content, shown, fixed = TRUE)

  # lassologistic: the cross-validation folds (model summary) and the bootstrap validation
  ll <- run("lassologistic", d, outcome = "truth", outcomeLevel = "Yes", explanatory = c("m1", "m2", "m3", "m4"),
            nfolds = 3, random_seed = 777, bootstrapValidation = TRUE, bootstrapN = 50)
  ms <- ll$results$modelSummary$asDF
  expect_identical(as.character(ms$value[ms$statistic == "Random seed"]), "777")
  expect_true(shown %in% notes(ll$results$validationTable))

  # nogoldstandard: latent-class random starts
  skip_if_not_installed("poLCA")
  ng <- run("nogoldstandard", d, test1 = "t1", test1Positive = "Positive", test2 = "t2", test2Positive = "Positive",
            test3 = "t3", test3Positive = "Positive", method = "latent_class", seed = 777)
  for (t in c("prevalence", "test_metrics"))
    expect_true(shown %in% notes(ng$results[[t]]), info = paste("nogoldstandard", t))

  # psychopdaROC: bootstrap cutpoint, bootstrap and partial-AUC intervals, IDI/NRI, bootstrap "Bayesian" ROC
  pr <- run("psychopdaROC", d, dependentVars = c("m1", "m2"), classVar = "truth", positiveClass = "Yes",
            method = "maximize_boot_metric", seed = 777, bootstrapCI = TRUE, bootstrapReps = 100,
            partialAUC = TRUE, calculateIDI = TRUE, calculateNRI = TRUE, idiNriBootRuns = 100,
            bayesianAnalysis = TRUE)
  expect_true(shown %in% notes(pr$results$resultsTable$get(key = "m1")))
  for (t in c("bootstrapCITable", "partialAUCTable", "idiTable", "nriTable", "bayesianROCTable"))
    expect_true(shown %in% notes(pr$results[[t]]), info = paste("psychopdaROC", t))
})

# The ROC confidence bands are drawn by pROC::ci.coords(), a stratified bootstrap with no analytic
# alternative, and it runs in the RENDERER - after .run()'s seed has been restored. Unseeded, the
# bands moved on every resize, reopen and export, and the seed option could not reach them.
test_that("enhancedROC confidence bands are seeded in the renderer and name their seed", {
  skip_if_not(exists("enhancedROCClass"))
  skip_if_not_installed("pROC")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  set.seed(12)
  n <- 120
  truth <- factor(ifelse(stats::runif(n) < 0.4, "Yes", "No"), levels = c("No", "Yes"))
  d <- data.frame(truth, m1 = stats::rnorm(n, ifelse(truth == "Yes", 1.1, 0)))
  analysis <- enhancedROCClass$new(options = enhancedROCOptions$new(
    outcome = "truth", positiveClass = "Yes", predictors = "m1",
    rocCurve = TRUE, showConfidenceBands = TRUE, seed = 777), data = d)
  quiet(analysis$init())
  quiet(analysis$run())

  # capture the ribbon the renderer builds, and the caption it draws
  band <- NULL
  caption <- NULL
  suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                         tracer = bquote({
                           .env <- .(environment())
                           assign("caption", as.character(plot$labels$caption), envir = .env)
                           .rib <- Filter(function(l) inherits(l$geom, "GeomRibbon"), plot$layers)
                           if (length(.rib)) assign("band", .rib[[1]]$data, envir = .env)
                         })))
  on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))), add = TRUE)
  draw <- function() {
    band <<- NULL
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    quiet(analysis$.createImage(
      analysis$results$results$rocCurvePlot$.__enclos_env__$private$.renderFun,
      analysis$results$results$rocCurvePlot))
    band
  }
  first <- draw()
  skip_if(is.null(first), "no confidence band was drawn")
  second <- draw()
  # same seed, same bootstrap: a redraw reproduces the band exactly
  expect_equal(second, first)
  expect_true(any(grepl("Random seed: 777", caption, fixed = TRUE)))
})

# library-audit 2026-09-16 meddecide [LOW] DONE: the epiR tables have a row set that is fixed before any
#   data is seen, so their rows are scaffolded in .init() and .run() only fills values with setRow();
#   footnotes attach by rowKey, which is what kept LR+ from being described as the diagnostic odds ratio
test_that("fixed epiR rows are scaffolded in .init() and footnotes follow their row", {
  skip_if_not(exists("decisionClass") && exists("decisioncalculatorClass"))
  skip_if_not_installed("epiR")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  ratio_keys <- c("se", "sp", "pv.pos", "pv.neg")
  number_keys <- c("lr.pos", "lr.neg", "diag.or", "youden", "nndx")

  set.seed(5)
  n <- 200
  gold <- factor(rep(c("Positive", "Negative"), each = n / 2), levels = c("Negative", "Positive"))
  test <- factor(ifelse(stats::runif(n) < ifelse(gold == "Positive", 0.85, 0.2), "Positive", "Negative"),
                 levels = c("Negative", "Positive"))
  d <- data.frame(gold, test)

  dec <- decisionClass$new(options = decisionOptions$new(
    gold = "gold", goldPositive = "Positive", newtest = "test", testPositive = "Positive",
    ci = TRUE, fnote = TRUE), data = d)
  quiet(dec$init())
  # before any computation the tables already have their rows
  expect_identical(dec$results$epirTable_ratio$rowKeys, as.list(ratio_keys))
  expect_identical(dec$results$epirTable_number$rowKeys, as.list(number_keys))
  quiet(dec$run())
  ratio <- dec$results$epirTable_ratio$asDF
  expect_identical(nrow(ratio), length(ratio_keys))
  expect_true(all(is.finite(ratio$est)))
  number <- dec$results$epirTable_number$asDF
  expect_identical(nrow(number), length(number_keys))
  # each footnote sits on its own statistic's row, whatever order epiR returns them in
  note_of <- function(key) dec$results$epirTable_number$getCell(rowKey = key, col = "statsnames")$footnotes
  expect_match(note_of("lr.pos"), "positive result", fixed = TRUE)
  expect_match(note_of("lr.neg"), "negative result", fixed = TRUE)
  expect_match(note_of("diag.or"), "correct diagnosis", fixed = TRUE)
  expect_match(note_of("nndx"), "need to be tested", fixed = TRUE)

  # decisioncompare shows one epiR table per test, all with the same fixed statistic set
  d2 <- d
  d2$test2 <- factor(ifelse(stats::runif(nrow(d2)) < ifelse(d2$gold == "Positive", 0.7, 0.25),
                            "Positive", "Negative"), levels = c("Negative", "Positive"))
  cmp <- decisioncompareClass$new(options = decisioncompareOptions$new(
    gold = "gold", goldPositive = "Positive", test1 = "test", test1Positive = "Positive",
    test2 = "test2", test2Positive = "Positive", ci = TRUE), data = d2)
  quiet(cmp$init())
  expect_true(all(c("se", "sp", "pv.pos") %in% unlist(cmp$results$epirTable1$rowKeys)))
  expect_identical(cmp$results$epirTable2$rowKeys, cmp$results$epirTable1$rowKeys)
  quiet(cmp$run())
  expect_identical(cmp$results$epirTable1$rowKeys, cmp$results$epirTable2$rowKeys)
  expect_true(any(is.finite(cmp$results$epirTable1$asDF$est)))

  # decisioncalculator takes no variables, so it runs through its wrapper rather than the class
  # (a no-variable calculator cannot be constructed with data = NULL). The row keys it ends up with
  # are the scaffold's, in .init() order, because .run() only fills them.
  calc <- quiet(decisioncalculator(TP = 90, TN = 80, FP = 20, FN = 10, ci = TRUE))
  expect_identical(calc$epirTable_number$rowKeys, as.list(number_keys))
  expect_true(all(c("se", "sp", "bal.acc", "f1.score") %in% unlist(calc$epirTable_ratio$rowKeys)))
  expect_true(all(is.finite(calc$epirTable_number$asDF$est)))
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class): the sweep behind the epiR finding - every other
#   meddecide table whose row set is fixed by the code or by an option has its rows before .run() starts,
#   under stable named keys, so it never renders as an empty header that then restructures
test_that("fixed-row tables across meddecide are scaffolded before .run()", {
  skip_if_not(exists("agreementClass") && exists("lassologisticClass") && exists("psychopdaROCClass"))
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  set.seed(8)
  n <- 40
  r <- data.frame(r1 = stats::rnorm(n), r2 = stats::rnorm(n), r3 = stats::rnorm(n))

  ag <- after_init("agreement", r, vars = c("r1", "r2", "r3"), hierarchicalKappa = TRUE,
                   varianceDecomposition = TRUE, iccHierarchical = TRUE, mixedEffectsComparison = TRUE,
                   multiAnnotatorConcordance = TRUE, agreementSampleSize = TRUE)
  expect_identical(ag("varianceDecompositionTable"), c("case", "rater", "cluster", "residual", "total"))
  expect_identical(ag("hierarchicalICCTable"), c("icc1", "icc2", "g_coeff"))
  expect_identical(ag("mixedEffectsVarianceTable"), c("case", "rater", "residual"))
  expect_identical(ag("concordanceF1Table"), c("conc_acc", "strict_acc", "annotator_agree", "n_info"))
  expect_identical(ag("agreementSampleSizeTable"),
                   c("metric", "n_required", "n_raters", "n_cat", "kappa_null", "kappa_alt", "alpha", "power", "total_reads"))
  # an option-decided row: the number of categories does not apply to an ICC design
  icc <- after_init("agreement", r, vars = c("r1", "r2", "r3"), agreementSampleSize = TRUE, ssMetric = "icc")
  expect_false("n_cat" %in% icc("agreementSampleSizeTable"))

  y <- factor(ifelse(stats::rnorm(n) > 0, "Yes", "No"))
  ll <- after_init("lassologistic", data.frame(y, x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n)),
                   outcome = "y", outcomeLevel = "Yes", explanatory = c("x1", "x2", "x3"),
                   scoringSystem = TRUE, scoringMethod = "compare", bootstrapValidation = TRUE)
  expect_identical(ll("performance"),
                   c("auc", "threshold", "accuracy", "sensitivity", "specificity", "precision", "f1", "brier"))
  expect_identical(ll("scoringPerformance"),
                   c("method", "auc", "cutoff", "accuracy", "sensitivity", "specificity", "precision", "f1",
                     "mean_pos", "mean_neg", "range"))
  expect_identical(ll("validationTable"), c("auc", "brier", "slope"))
  expect_identical(ll("methodComparison"), c("beta10", "schneeweiss", "maxscaled", "full"))

  cls <- factor(ifelse(stats::rnorm(n) > 0, "Positive", "Negative"))
  pr <- after_init("psychopdaROC", data.frame(cls, m1 = stats::rnorm(n), m2 = stats::rnorm(n)),
                   dependentVars = c("m1", "m2"), classVar = "cls", positiveClass = "Positive", refVar = NULL,
                   metaAnalysis = TRUE, overrideMetaAnalysisWarning = TRUE, metaAnalysisMethod = "both")
  expect_identical(pr("metaAnalysisTable"), c("fixed", "random"))

  dd <- data.frame(gold = factor(sample(c("Present", "Absent"), n, TRUE)),
                   test = factor(sample(c("Positive", "Negative"), n, TRUE)))
  dc <- after_init("decision", dd, gold = "gold", goldPositive = "Present", newtest = "test",
                   testPositive = "Positive", od = TRUE)
  expect_identical(dc("rawContingency"), c("test_pos", "test_neg", "total"))
})
