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

# The analyses this audit covers. The umbrella keeps all of them; a parked one (T/P/D menuGroup) is
# deleted from the generated module, and a maintainer's checkout can be mid-removal. So each test
# audits the ones present and ends with a skip that names the rest: an absent file only says "cannot
# open the connection", and gating a whole test on one analysis hides the others.
audit_analyses <- c(
  "agreement", "cotest", "decision", "decisioncalculator", "decisioncombine",
  "decisioncompare", "decisioncurve", "enhancedROC", "kappaSizeCI", "kappaSizeFixedN",
  "kappaSizePower", "lassologistic", "nogoldstandard", "psychopdaROC", "sequentialtests"
)

# Which of `names` lack a file here; `files` are sprintf() templates relative to `root`. Matched
# against list.files(), not file.exists(): macOS disks ignore case, so file.exists() finds
# "R/psychopdaroc.b.R" there and Linux CI does not.
audit_absent <- function(root, names, files = "R/%s.b.R") {
  on_disk <- function(path) basename(path) %in% list.files(file.path(root, dirname(path)))
  Filter(function(name) !all(vapply(sprintf(files, name), on_disk, logical(1))), names)
}

# Runtime tests: which of `analyses` have their class loaded (the namespace under R CMD check,
# load_all() in development).
audit_loaded <- function(analyses) Filter(function(a) exists(paste0(a, "Class")), analyses)

# Ends a test that could not reach `absent` with a skip naming them, and `note` if given. Given the
# umbrella's `root` it errors instead: the umbrella keeps every analysis, parked or not, so a missing
# name is a typo.
audit_skip_absent <- function(absent, root = NULL, note = NULL) {
  if (length(absent) == 0) return(invisible())
  absent <- paste(absent, collapse = ", ")
  if (!is.null(root) &&
      identical(unname(read.dcf(file.path(root, "DESCRIPTION"))[1, "Package"]), "ClinicoPath"))
    stop("the umbrella keeps every analysis, parked or not, yet lacks: ", absent, call. = FALSE)
  testthat::skip(paste(c(paste0("not in this module (parked or not shipped): ", absent), note), collapse = "; "))
}

# Ends a runtime test with a skip naming each of `wanted` whose class is not loaded, and why: its files
# are absent from this tree, or present while the package is not (the file was run on its own). R CMD
# check has no source tree; there the loaded namespace is the module, so a missing class is absent.
audit_skip_unloaded <- function(wanted) {
  unloaded <- setdiff(wanted, audit_loaded(wanted))
  if (length(unloaded) == 0) return(invisible())
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) audit_skip_absent(unloaded)
  absent <- audit_absent(root, unloaded)
  not_loaded <- paste0("class not loaded (package not loaded in this run): ",
                       paste(setdiff(unloaded, absent), collapse = ", "))
  audit_skip_absent(absent, root, if (length(absent) < length(unloaded)) not_loaded)
  testthat::skip(not_loaded)
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
  audit_skip_absent(audit_absent(root, "sequentialtests"), root)
  source_lines <- readLines(
    file.path(root, "R", "sequentialtests.b.R"),
    warn = FALSE
  )
  # The comparison now runs through a small similar_name() helper (both directions, 2026-10-08), so
  # check every agrepl() call in code rather than one exact assignment line.
  code <- source_lines[!grepl("^\\s*#", source_lines)]
  call_lines <- grep("agrepl(", code, value = TRUE, fixed = TRUE)

  expect_gte(length(call_lines), 1L)
  for (line in call_lines) expect_match(line, "fixed = TRUE", fixed = TRUE)
})

test_that("AUC summary invalidation matches its fixed DeLong calculation", {
  root <- audit_source_root()
  audit_skip_absent(audit_absent(root, "psychopdaROC", "jamovi/%s.r.yaml"), root)
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

  # Every entry changes the AUC summary: patientID/analysisUnit aggregate rows to patients,
  # negativeClass decides which rows are negatives, aucInference picks the interval (psychopdaROC
  # refactor 2026-10-10). Bootstrap and display options must stay out.
  expect_identical(
    clear_with,
    c("dependentVars", "patientID", "analysisUnit", "classVar", "positiveClass",
      "negativeClass", "subGroup", "direction", "aucInference")
  )
})

test_that("audit-sensitive output strings remain export-safe", {
  root <- audit_source_root()
  absent <- audit_absent(root, c("agreement", "psychopdaROC", "nogoldstandard"))
  output_files <- file.path(
    root,
    "R",
    sprintf("%s.b.R", setdiff(c("agreement", "psychopdaROC"), absent))
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

  if (!"nogoldstandard" %in% absent) {
    no_gold_text <- paste(
      readLines(file.path(root, "R", "nogoldstandard.b.R"), warn = FALSE),
      collapse = "\n"
    )
    # Audit 2026-07-13 [LOW]: a translatable fragment glued to e$message,
    # message(.("Error in plot: "), e$message). Test the pattern, not one string: the
    # "Error in plot" base-graphics renderer was dead (reachable only without ggplot2,
    # an Import) and was deleted on 2026-10-05, which broke the old exact-string match.
    expect_false(grepl('\\.\\("[^"\n]*[:;,] "\\)', no_gold_text))
    expect_match(
      no_gold_text,
      '.fmt(.("Error in ggplot: {msg}")',
      fixed = TRUE
    )
  }
  audit_skip_absent(absent, root)
})

test_that("agreement has no declared-but-unpopulated audit headings", {
  root <- audit_source_root()
  audit_skip_absent(audit_absent(root, "agreement", "jamovi/%s.r.yaml"), root)
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
  # The `modes: copy_i18n_files` gate was removed once the copy became unconditional
  # in _updateModules_plan.R, so asserting the flag tested a key that no longer exists
  # (isTRUE(NULL) is FALSE). Assert the actual guarantee: the plan copies i18n_files
  # for every module, with no mode to switch it off.
  expect_null(config$modes$copy_i18n_files)
  # config_path is already resolved to the umbrella, which is the only tree that
  # holds the updater sources -- the generated module has neither file.
  plan_src <- readLines(file.path(dirname(config_path), "_updateModules_plan.R"), warn = FALSE)
  expect_true(any(grepl("for (f in unlist(m$i18n_files))", plan_src, fixed = TRUE)))
})

test_that("meddecide Boolean controls use state labels rather than action labels", {
  root <- audit_source_root()
  absent <- audit_absent(root, audit_analyses, c("jamovi/%s.a.yaml", "jamovi/%s.u.yaml"))
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

  for (analysis in setdiff(audit_analyses, absent)) {
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
  audit_skip_absent(absent, root)
})

test_that("IDI and NRI consolidate unstable calibration warnings", {
  # raw_to_prob() and the bootstrap helpers live in R/psychopdaROC-nri-idi.R and ship with it
  audit_skip_unloaded("psychopdaROC")
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
  sources <- c(
    "decision.b.R",
    "decisioncombine.b.R",
    "enhancedROC.b.R",
    "psychopdaROC.b.R",
    "utils-nomogrammer.R"
  )
  absent <- audit_absent(root, sources, "R/%s")
  source_files <- file.path(root, "R", setdiff(sources, absent))
  source_text <- paste(
    unlist(lapply(source_files, readLines, warn = FALSE)),
    collapse = "\n"
  )

  expect_false(grepl(
    "(?m)^#' @import (boot|cutpointr|dplyr|ggplot2|magrittr|pROC|scales)[[:space:]]*$",
    source_text,
    perl = TRUE
  ))
  audit_skip_absent(absent, root)
})

test_that("redundant utility definitions do not override canonical helpers", {
  root <- audit_source_root()
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  absent <- audit_absent(root, "nogoldstandard")
  if (length(absent) == 0) {
    no_gold_text <- readLines(
      file.path(root, "R", "nogoldstandard.b.R"),
      warn = FALSE
    )

    expect_false(any(grepl("^.escapeVariableNames <-", no_gold_text)))
  }

  if (identical(desc[1, "Package"], "meddecide")) {
    redundant_files <- file.path(
      root,
      "R",
      c("basic_metrics.R", "diagnostic_metrics.R", "stats_utils.R")
    )
    expect_false(any(file.exists(redundant_files)))
  }
  audit_skip_absent(absent, root)
})

test_that("fixed decision tables are populated without changing row structure", {
  wanted <- c("decision", "decisioncombine")
  loaded <- audit_loaded(wanted)
  data <- data.frame(
    gold = factor(c("pos", "pos", "neg", "neg", "pos", "neg")),
    test1 = factor(c("pos", "neg", "pos", "neg", "pos", "neg")),
    test2 = factor(c("pos", "pos", "neg", "neg", "neg", "neg"))
  )

  if ("decision" %in% loaded) {
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
  }

  if ("decisioncombine" %in% loaded) {
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
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-16 meddecide [HIGH] DONE: renderers draw from image$state; export and .omv reopen
#   rebuild the analysis without .run(), so private$ fields filled there are NULL (guide section 15).
# Since the 2026-10-10 refactor psychopdaROC refuses this meta-analysis outright: markers measured on
# the same cohort are not independent studies, so there is no pooled AUC and no forest plot to redraw.
# The test now pins the refusal (and that nothing is drawn or shown in its place).
test_that("psychopdaROC refuses a meta-analysis of markers from one cohort and draws no forest plot", {
  audit_skip_unloaded("psychopdaROC")
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
    positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, metaAnalysis = TRUE, forestPlot = TRUE,
    overrideMetaAnalysisWarning = TRUE
  ))
  expect_length(ran$metaAnalysisForestPlot$itemKeys, 0L)
  expect_false(ran$metaAnalysisForestPlot$visible)
  expect_false(ran$metaAnalysisTable$visible)
  expect_true(all(is.na(ran$metaAnalysisTable$asDF$pooled_auc)))
  expect_match(ran$metaAnalysisWarning$content, "markers from the same cohort are not independent studies",
               fixed = TRUE)
})

# library-audit 2026-09-16 meddecide [MEDIUM] DONE: ROC overlays (smoothed curve, confidence bands,
#   quantile CIs) are drawn from image$state, so an export shows what the live plot shows
test_that("psychopdaROC overlays survive a redraw without .run()", {
  audit_skip_unloaded("psychopdaROC")
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
    positiveClass = "Disease", negativeClass = "Healthy", refVar = NULL, plotROC = TRUE, combinePlots = FALSE,
    showConfidenceBands = TRUE, quantileCIs = TRUE, rocSmoothingMethod = "binormal",
    run = TRUE   # smoothing is an intensive option: it waits for the Run action (refactor 2026-10-10)
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

  # "Distribution fitting" is pROC's MASS::fitdistr smoother, which fails silently (try) when MASS is
  # missing; meddecide imports MASS for it (R CMD check 2026-10-17).
  fitted <- analysis$.__enclos_env__$private$.createSmoothedROC(d$marker1, d$status, "Disease", "fitdistr")
  expect_true(is.finite(as.numeric(fitted$auc)))
})

# library-audit 2026-09-16 meddecide [LOW] DONE: requiresData: true only where the renderer, or a
#   private$ helper it calls, reads self$data (guide section 15)
test_that("meddecide images declare requiresData exactly when their renderer reads self$data", {
  root <- audit_source_root()
  # jmvcore nulls the data once .run() returns and re-reads it for a redraw or export only for an
  # Image that asks. Missing flag: the plot errors on resize/.omv reopen/export. Surplus flag: the
  # whole dataset is re-read before every redraw and export for a plot drawn from image$state.
  absent <- audit_absent(root, audit_analyses, c("R/%s.b.R", "jamovi/%s.r.yaml"))
  images <- 0
  mismatches <- character()
  for (name in setdiff(audit_analyses, absent)) {
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
  # The floor proves the walk found the images, without which no mismatch could show. It is the full
  # set's: a parked analysis takes its images with it (psychopdaROC and enhancedROC hold 12 each), so
  # a partial set is held to the mismatch check alone and the skip below names what is missing.
  if (length(absent) == 0) expect_gt(images, 30)
  expect_equal(mismatches, character(0))
  audit_skip_absent(absent, root)
})

# library-audit 2026-09-16 meddecide [LOW] DONE: seeded resampling uses withr::local_seed(); an analysis
#   leaves the session's random-number stream as it found it (every analysis shares one R process)
test_that("seeded meddecide analyses leave the caller's RNG stream untouched", {
  root <- audit_source_root()
  # Static: no set.seed() call in any meddecide backend (strings and comments do not count).
  absent <- audit_absent(root, audit_analyses)
  bare <- character()
  for (name in setdiff(audit_analyses, absent)) {
    pd <- utils::getParseData(parse(file.path(root, "R", paste0(name, ".b.R")), keep.source = TRUE))
    hit <- pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "set.seed"
    bare <- c(bare, sprintf("%s.b.R:%d", name, pd$line1[hit]))
  }
  expect_equal(bare, character(0))

  # Behaviour: the stream after a seeded run equals the stream without it.
  wanted <- c("agreement", "lassologistic")
  loaded <- audit_loaded(wanted)
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
  if ("agreement" %in% loaded) {
    agree <- agreementClass$new(options = agreementOptions$new(
      vars = c("r1", "r2", "r3"), kripp = TRUE, bootstrap = TRUE, nBoot = 100, robinsonA = TRUE, tdi = TRUE),
      data = raters)
    quiet(agree$init())
    expect_identical(stream_after(agree), untouched)
  }
  if ("lassologistic" %in% loaded) {
    lasso <- lassologisticClass$new(options = lassologisticOptions$new(
      outcome = "y", outcomeLevel = "Yes", explanatory = paste0("x", 1:5), nfolds = 3), data = X)
    quiet(lasso$init())
    expect_identical(stream_after(lasso), untouched)
  }
  audit_skip_absent(absent, root)
  audit_skip_unloaded(wanted)
})

# Found while proving the requiresData removals (2026-09-16 meddecide): with as many clusters as raters
# (3 raters, the default nClusters of 3) every rater was its own cluster and the dendrogram renderer
# stopped in rect.hclust() with "k must be between 2 and 2". The same held for cases.
test_that("rater and case clustering cap k below the number of units, and dendrograms draw", {
  audit_skip_unloaded("agreement")
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
  absent <- audit_absent(root, audit_analyses)
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
    files <- file.path(root, "R", sprintf("%s.b.R", setdiff(audit_analyses, absent)))
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
  # A parked analysis leaves its packages in Imports, where they read as unused: that is routing, not
  # dead weight, so skip and never prune on it (a prune_imports entry is a standing delete order). The
  # skip lists them, so a package no parked analysis explains is still in view.
  if (!all(used))
    audit_skip_absent(absent, root, paste("Imports unused here:", paste(declared[!used], collapse = ", ")))
  expect_equal(declared[!used], character(0), info = paste(declared[!used], collapse = ", "))
})

# A result that changes with the random seed names that seed next to it, so a reader can see which
# resamples produced it and change them (Random seed option). Deterministic outputs show no seed.
test_that("every seed-dependent meddecide result shows the seed that drew it", {
  wanted <- c("agreement", "decisioncurve", "enhancedROC", "lassologistic", "nogoldstandard", "psychopdaROC")
  loaded <- audit_loaded(wanted)
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

  if ("agreement" %in% loaded) {
    # agreement: Krippendorff bootstrap and k-means rater and case clustering. Interval alpha: the
    # ratings are measurements, and Nominal alpha on measurements is refused (no bootstrap runs).
    ag <- run("agreement", data.frame(r1 = m1, r2 = m1 + stats::rnorm(n, 0, 0.3), r3 = m1 + stats::rnorm(n, 0, 0.6)),
              vars = c("r1", "r2", "r3"), seed = 777, kripp = TRUE, krippMethod = "interval",
              bootstrap = TRUE, nBoot = 100, tdi = TRUE,
              raterClustering = TRUE, clusterMethod = "kmeans", nClusters = 2,
              caseClustering = TRUE, caseClusterMethod = "kmeans", nCaseClusters = 2)
    for (t in c("krippTable", "raterClusterTable", "caseClusterTable"))
      expect_true(shown %in% notes(ag$results[[t]]), info = paste("agreement", t))
    # TDI's interval is a distribution-free order-statistic bound: nothing is resampled, so no seed
    expect_false(shown %in% notes(ag$results$tdiTable))
    nominal <- run("agreement", data.frame(r1 = m1, r2 = m1 + 0.2, r3 = m1 - 0.2)[1:30, ],
                   vars = c("r1", "r2", "r3"), seed = 777, kripp = TRUE, bootstrap = TRUE, nBoot = 100)
    expect_false(shown %in% notes(nominal$results$krippTable))
    hier <- run("agreement", data.frame(r1 = m1, r2 = m1 + 0.1, r3 = m1 - 0.1)[1:30, ],
                vars = c("r1", "r2", "r3"), seed = 777, raterClustering = TRUE, clusterMethod = "hierarchical")
    expect_false(shown %in% notes(hier$results$raterClusterTable))
  }

  if ("decisioncurve" %in% loaded) {
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
  }

  if ("enhancedROC" %in% loaded) {
    # enhancedROC: bootstrap AUC intervals, bootstrap comparisons, internal validation
    er <- run("enhancedROC", d, outcome = "truth", positiveClass = "Yes", predictors = c("m1", "m2"),
              useBootstrap = TRUE, bootstrapSamples = 100, analysisType = "comparative",
              pairwiseComparisons = TRUE, statisticalComparison = TRUE,
              comparisonMethod = "bootstrap", internalValidation = TRUE, validationMethod = "bootstrap", seed = 777)
    for (t in c("aucSummary", "rocComparisons", "statisticalSummary"))
      expect_true(shown %in% notes(er$results$results[[t]]), info = paste("enhancedROC", t))
    expect_match(er$results$results$analysisSummary$content, shown, fixed = TRUE)
  }

  if ("lassologistic" %in% loaded) {
    # lassologistic: the cross-validation folds (model summary) and the bootstrap validation
    ll <- run("lassologistic", d, outcome = "truth", outcomeLevel = "Yes", explanatory = c("m1", "m2", "m3", "m4"),
              nfolds = 3, random_seed = 777, bootstrapValidation = TRUE, bootstrapN = 50)
    ms <- ll$results$modelSummary$asDF
    expect_identical(as.character(ms$value[ms$statistic == "Random seed"]), "777")
    expect_true(shown %in% notes(ll$results$validationTable))
  }

  if ("nogoldstandard" %in% loaded) {
    # nogoldstandard: latent-class random starts
    skip_if_not_installed("poLCA")
    ng <- run("nogoldstandard", d, test1 = "t1", test1Positive = "Positive", test2 = "t2", test2Positive = "Positive",
              test3 = "t3", test3Positive = "Positive", method = "latent_class", seed = 777)
    for (t in c("prevalence", "test_metrics"))
      expect_true(shown %in% notes(ng$results[[t]]), info = paste("nogoldstandard", t))
  }

  if ("psychopdaROC" %in% loaded) {
    # psychopdaROC: bootstrap cutpoint, bootstrap and partial-AUC intervals, IDI/NRI, bootstrap "Bayesian" ROC
    # an explicit negative class and the Run action are required since the 2026-10-10 refactor
    pr <- run("psychopdaROC", d, dependentVars = c("m1", "m2"), classVar = "truth", positiveClass = "Yes",
              negativeClass = "No", method = "maximize_boot_metric", seed = 777, bootstrapCI = TRUE,
              bootstrapReps = 100, partialAUC = TRUE, calculateIDI = TRUE, calculateNRI = TRUE,
              idiNriBootRuns = 100, bayesianAnalysis = TRUE, run = TRUE)
    expect_true(shown %in% notes(pr$results$resultsTable$get(key = "m1")))
    for (t in c("bootstrapCITable", "partialAUCTable", "idiTable", "nriTable", "bayesianROCTable"))
      expect_true(shown %in% notes(pr$results[[t]]), info = paste("psychopdaROC", t))
  }
  audit_skip_unloaded(wanted)
})

# The ROC confidence bands are drawn by pROC::ci.coords(), a stratified bootstrap with no analytic
# alternative, and it runs in the RENDERER - after .run()'s seed has been restored. Unseeded, the
# bands moved on every resize, reopen and export, and the seed option could not reach them.
test_that("enhancedROC confidence bands are seeded in the renderer and name their seed", {
  audit_skip_unloaded("enhancedROC")
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
  wanted <- c("decision", "decisioncompare", "decisioncalculator")
  loaded <- audit_loaded(wanted)
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

  if ("decision" %in% loaded) {
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
    # 2026-09-25 (decision VAL-decision-09): the DOR and NNDx notes now give the Glas et al.
    # (2003) and Linn & Grunau (2006) definitions; the old epiR-help wording was wrong.
    expect_match(note_of("diag.or"), "odds of a positive test in patients with the disease", fixed = TRUE)
    expect_match(note_of("nndx"), "one more true-positive than false-positive", fixed = TRUE)
  }

  if ("decisioncompare" %in% loaded) {
    # decisioncompare shows one epiR table per test, all with the same fixed statistic set
    d2 <- d
    d2$test2 <- factor(ifelse(stats::runif(nrow(d2)) < ifelse(d2$gold == "Positive", 0.7, 0.25),
                              "Positive", "Negative"), levels = c("Negative", "Positive"))
    # decisioncompare 1.1.0 requires an explicit negative level for every selected variable.
    cmp <- decisioncompareClass$new(options = decisioncompareOptions$new(
      gold = "gold", goldPositive = "Positive", goldNegative = "Negative",
      test1 = "test", test1Positive = "Positive", test1Negative = "Negative",
      test2 = "test2", test2Positive = "Positive", test2Negative = "Negative", ci = TRUE), data = d2)
    quiet(cmp$init())
    expect_true(all(c("se", "sp", "pv.pos") %in% unlist(cmp$results$epirTable1$rowKeys)))
    expect_identical(cmp$results$epirTable2$rowKeys, cmp$results$epirTable1$rowKeys)
    quiet(cmp$run())
    expect_identical(cmp$results$epirTable1$rowKeys, cmp$results$epirTable2$rowKeys)
    expect_true(any(is.finite(cmp$results$epirTable1$asDF$est)))
  }

  if ("decisioncalculator" %in% loaded) {
    # decisioncalculator takes no variables, so it runs through its wrapper rather than the class
    # (a no-variable calculator cannot be constructed with data = NULL). The row keys it ends up with
    # are the scaffold's, in .init() order, because .run() only fills them.
    calc <- quiet(decisioncalculator(TP = 90, TN = 80, FP = 20, FN = 10, ci = TRUE))
    expect_identical(calc$epirTable_number$rowKeys, as.list(number_keys))
    expect_true(all(c("se", "sp") %in% unlist(calc$epirTable_ratio$rowKeys)))
    # Balanced accuracy and F1 have no interval, so they left this interval table (their limits printed
    # "NaN", 2026-10-08) and are shown in advancedMetricsTable instead.
    expect_false(any(c("bal.acc", "f1.score") %in% unlist(calc$epirTable_ratio$rowKeys)))
    expect_true(all(is.finite(unlist(calc$advancedMetricsTable$asDF[c("balancedAccuracy", "f1Score")]))))
    expect_true(all(is.finite(calc$epirTable_number$asDF$est)))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class): the sweep behind the epiR finding - every other
#   meddecide table whose row set is fixed by the code or by an option has its rows before .run() starts,
#   under stable named keys, so it never renders as an empty header that then restructures
test_that("fixed-row tables across meddecide are scaffolded before .run()", {
  wanted <- c("agreement", "lassologistic", "psychopdaROC", "decision")
  loaded <- audit_loaded(wanted)
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  set.seed(8)
  n <- 40
  r <- data.frame(r1 = stats::rnorm(n), r2 = stats::rnorm(n), r3 = stats::rnorm(n))

  if ("agreement" %in% loaded) {
    ag <- after_init("agreement", r, vars = c("r1", "r2", "r3"), hierarchicalKappa = TRUE,
                     varianceDecomposition = TRUE, iccHierarchical = TRUE, mixedEffectsComparison = TRUE,
                     multiAnnotatorConcordance = TRUE, agreementSampleSize = TRUE)
    expect_identical(ag("varianceDecompositionTable"), c("case", "rater", "cluster", "residual", "total"))
    expect_identical(ag("hierarchicalICCTable"), c("icc1", "icc2"))
    expect_identical(ag("mixedEffectsVarianceTable"), c("case", "rater", "residual"))
    expect_identical(ag("concordanceF1Table"), c("conc_acc", "strict_acc", "annotator_agree", "n_info"))
    expect_identical(ag("agreementSampleSizeTable"),
                     c("metric", "n_required", "n_raters", "n_cat", "kappa_null", "kappa_alt", "alpha", "power", "total_reads"))
    # an option-decided row: the number of categories does not apply to an ICC design
    icc <- after_init("agreement", r, vars = c("r1", "r2", "r3"), agreementSampleSize = TRUE, ssMetric = "icc")
    expect_false("n_cat" %in% icc("agreementSampleSizeTable"))
  }

  if ("lassologistic" %in% loaded) {
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
    # "smallest", not "schneeweiss": divide-by-smallest is a Sullivan-type scale; Schneeweiss 2003 is
    # +1 point per 0.3 ln(OR) (lassologistic key renamed 2026-10-12).
    expect_identical(ll("methodComparison"), c("beta10", "smallest", "maxscaled", "full"))
  }

  if ("psychopdaROC" %in% loaded) {
    cls <- factor(ifelse(stats::rnorm(n) > 0, "Positive", "Negative"))
    pr <- after_init("psychopdaROC", data.frame(cls, m1 = stats::rnorm(n), m2 = stats::rnorm(n)),
                     dependentVars = c("m1", "m2"), classVar = "cls", positiveClass = "Positive", refVar = NULL,
                     metaAnalysis = TRUE, overrideMetaAnalysisWarning = TRUE, metaAnalysisMethod = "both")
    expect_identical(pr("metaAnalysisTable"), c("fixed", "random"))
  }

  if ("decision" %in% loaded) {
    dd <- data.frame(gold = factor(sample(c("Present", "Absent"), n, TRUE)),
                     test = factor(sample(c("Positive", "Negative"), n, TRUE)))
    dc <- after_init("decision", dd, gold = "gold", goldPositive = "Present", newtest = "test",
                     testPositive = "Positive", od = TRUE)
    expect_identical(dc("rawContingency"), c("test_pos", "test_neg", "total"))
  }
  audit_skip_unloaded(wanted)
})


# library-audit 2026-09-16 meddecide [LOW] PARTIAL: the fragment/whitespace finding named six sites -
#   three severity prefixes ("ERROR: "), a leading-space NNT sentence and two leading-comma clauses.
#   All six are fixed, and the sweep behind them covers three adjacent classes that are invisible in
#   English and to every behaviour test: a braced \u{XXXX} inside a .() (the compiler stores the escape
#   literally, so the msgid can never match), a msgid containing " [..]" (jmvcore's Translator cuts the
#   string off there when untranslated), and a template with two or more sprintf conversions and no %n$
#   (unreorderable, and the shape that made sprintf() raise in Turkish). This asserts the class, not the
#   six sites, so a later edit cannot reintroduce it under a different name.
test_that("no meddecide msgid is a fragment, carries padding, or cannot be reordered", {
  root <- audit_source_root()
  absent <- audit_absent(root, audit_analyses)
  # sprintf(), not paste0(): paste0(character(0), ".b.R") is ".b.R", a file that is never there
  files <- file.path(root, "R", sprintf("%s.b.R", setdiff(audit_analyses, absent)))

  pad <- ctx <- braced <- unordered <- character(0)

  for (f in files) {
    src <- readLines(f, warn = FALSE)
    code <- src[!grepl("^\\s*#", src)]
    tag <- basename(f)

    # a separator inside the msgid: leading space/comma/semicolon/colon, or a trailing space
    hit <- grep('\\.\\(\\s*"(?:[\\s,;:])', code, perl = TRUE)
    if (length(hit)) pad <- c(pad, paste0(tag, ":", hit))
    hit <- grep('\\.\\(\\s*"[^"\n]*\\s"\\s*[,)]', code)
    if (length(hit)) pad <- c(pad, paste0(tag, ":", hit))

    # " [..]" anywhere in a msgid - the Translator splits on it and drops the remainder
    hit <- grep('\\.\\(\\s*"(?:[^"\\\\]|\\\\.)* \\[[^"]*\\]', code, perl = TRUE)
    if (length(hit)) ctx <- c(ctx, paste0(tag, ":", hit))

    # \u{XXXX} inside a .() - resolved by R at parse time, stored literally in the catalog
    hit <- grep('\\.\\(\\s*"[^"]*\\\\u\\{', code)
    if (length(hit)) braced <- c(braced, paste0(tag, ":", hit))

    # two or more sprintf conversions with no positional markers
    lits <- regmatches(code, gregexpr('\\.\\(\\s*"(?:[^"\\\\]|\\\\.)*"', code, perl = TRUE))
    for (i in seq_along(lits)) {
      for (lit in lits[[i]]) {
        body <- sub('^\\.\\(\\s*"', "", sub('"$', "", lit))
        segs <- strsplit(body, "%%", fixed = TRUE)[[1]]
        convs <- unlist(regmatches(
          segs,
          # no space flag: in prose "95% interval" would read as the conversion "% i"; no meddecide
          # msgid uses a space-flag conversion
          gregexpr("%(\\d+\\$)?[-+0#]*(\\*|\\d+)?(\\.\\d+)?[sdifeEgGxX]", segs, perl = TRUE)
        ))
        if (length(convs) >= 2 && !any(grepl("\\$", convs))) {
          unordered <- c(unordered, paste0(tag, ":", i))
        }
      }
    }
  }

  expect_equal(pad, character(0))
  expect_equal(ctx, character(0))
  expect_equal(braced, character(0))
  expect_equal(unordered, character(0))
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [HIGH] DONE: results that are tables are Table elements - an HTML
#   <table> loses the number format, Copy as a grid, Copy Latex and screen readers (guide section 26).
test_that("confusion matrices, cross-tabs and check lists are Tables, not HTML", {
  wanted <- c("psychopdaROC", "decisioncalculator", "decisioncompare", "lassologistic", "agreement")
  loaded <- audit_loaded(wanted)
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))

  if ("psychopdaROC" %in% loaded) {
    set.seed(11)
    cls <- factor(rep(c("H", "D"), c(60, 40)), levels = c("H", "D"))
    d <- data.frame(m = stats::rnorm(100, ifelse(cls == "D", 1.2, 0)), cls = cls)
    a <- psychopdaROCClass$new(options = psychopdaROCOptions$new(
      dependentVars = "m", classVar = "cls", positiveClass = "D", negativeClass = "H",
      sensSpecTable = TRUE, run = TRUE), data = d)
    quiet(a$init())
    t <- a$results$sensSpecTable$get(key = "m")
    expect_true(inherits(t, "Table"))
    expect_identical(t$rowKeys, list("1_pos", "1_neg"))          # scaffolded before .run()
    quiet(a$run())
    cm <- t$asDF
    expect_equal(sum(cm$pos[1:2]), 40)                            # criterion columns hold the classes
    expect_equal(sum(cm$neg[1:2]), 60)
    sens <- a$results$resultsTable$get(key = "m")$asDF$sensitivity[1]
    expect_equal(cm$pos[1] / 40, sens, tolerance = 1e-8)          # TP / (TP + FN)
    expect_identical(t$getColumn("pos")$title, "Positive (D)")
  }
  if ("agreement" %in% loaded) {
    set.seed(8)
    r1 <- factor(sample(c("a", "b"), 40, TRUE))
    r2 <- factor(ifelse(stats::runif(40) < 0.8, as.character(r1), "a"), levels = c("a", "b"))
    ag <- agreementClass$new(options = agreementOptions$new(vars = c("r1", "r2"), showSummary = TRUE),
                             data = data.frame(r1, r2))
    quiet(ag$init()); quiet(ag$run())
    expect_true(nzchar(ag$results$summary$content))
    expect_false(grepl("<table", ag$results$summary$content, fixed = TRUE))
  }
  if ("decisioncalculator" %in% loaded) {
    dc <- decisioncalculatorClass$new(options = decisioncalculatorOptions$new(
      TP = 90, FP = 30, FN = 20, TN = 80, showSummary = TRUE), data = data.frame())
    quiet(dc$init()); quiet(dc$run())
    expect_true(nzchar(dc$results$summary$content))
    expect_false(grepl("<table", dc$results$summary$content, fixed = TRUE))
  }
  if ("decisioncompare" %in% loaded) {
    v <- c("v1", "v2", "v3")
    d <- data.frame(gold = factor(rep(v, c(5, 7, 3))), a = factor(rep(c("v1", "v2"), c(9, 6))),
                    b = factor(rep(c("v2", "v1"), c(4, 11))))
    cmp <- decisioncompareClass$new(options = decisioncompareOptions$new(
      gold = "gold", goldPositive = "v1", goldNegative = "v2",
      test1 = "a", test1Positive = "v1", test1Negative = "v2",
      test2 = "b", test2Positive = "v1", test2Negative = "v2", excludeIndeterminate = TRUE,
      od = TRUE), data = d)
    quiet(cmp$init()); quiet(cmp$run())
    gf <- cmp$results$goldFrequency$asDF
    used <- droplevels(cmp$.__enclos_env__$private$.cached_processed_data$data)
    expect_equal(gf$n, as.vector(table(used$gold)))
    ct <- cmp$results$originalCrossTab$asDF
    oracle <- table(used$a, used$gold)
    got <- as.matrix(ct[ct$test == "a", grep("^gold_", names(ct))])
    expect_equal(unname(got), unname(matrix(as.integer(oracle), nrow(oracle))))
  }
  if ("lassologistic" %in% loaded && requireNamespace("glmnet", quietly = TRUE)) {
    set.seed(3)
    X <- as.data.frame(matrix(stats::rnorm(120 * 5), 120, dimnames = list(NULL, paste0("x", 1:5))))
    X$y <- factor(ifelse(X$x1 + stats::rnorm(120) > 0, "Yes", "No"))
    lasso <- lassologisticClass$new(options = lassologisticOptions$new(
      outcome = "y", outcomeLevel = "Yes", explanatory = paste0("x", 1:5), nfolds = 3,
      suitabilityCheck = TRUE), data = X)
    quiet(lasso$init()); quiet(lasso$run())
    s <- lasso$results$suitabilityReport
    expect_true(inherits(s, "Table"))
    expect_identical(unlist(s$rowKeys), c("epv", "n", "balance", "p", "collinearity"))
    expect_true(all(s$asDF$status %in% c("No concern", "Marginal", "Major concern")))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: a failure path neither hides results nor blanks them
#   before reject(): jmvcore never restores visibility between runs, and jamovi greys an errored
#   analysis with its results in place. Html is cleared with "<div></div>" before a normal early
#   return (the client skips ""), never by an on.exit() hide (guide section 5).
test_that("validation failures leave results in place for jamovi's error presentation", {
  root <- audit_source_root()
  files <- c("decisioncombine", "decisioncalculator", "enhancedROC", "psychopdaROC",
             "kappaSizeCI", "kappaSizeFixedN", "kappaSizePower")
  absent <- audit_absent(root, files)
  for (name in setdiff(files, absent)) {
    src <- paste(readLines(file.path(root, "R", paste0(name, ".b.R")), warn = FALSE), collapse = "\n")
    src <- gsub("(?m)^[ \t]*#[^\n]*", "", src, perl = TRUE)
    # the on.exit() call's own argument, balanced parentheses
    calls <- regmatches(src, gregexpr("on\\.exit(\\((?:[^()]++|(?1))*\\))", src, perl = TRUE))[[1]]
    expect_false(any(grepl("setVisible", calls, fixed = TRUE)), label = paste(name, "on.exit hide"))
    run <- sub("(?s).*?\\n\\s*\\.run = function\\(\\) \\{", "", src, perl = TRUE)
    first_reject <- regexpr("reject\\(", run)
    first_blank <- regexpr("setContent\\(\\s*\"(<div></div>)?\"\\s*\\)", run)
    if (name %in% c("kappaSizeCI", "kappaSizeFixedN", "kappaSizePower"))
      expect_true(first_blank < 0 || first_blank > first_reject, label = paste(name, "blank before reject"))
  }
  if ("decisioncombine" %in% audit_loaded("decisioncombine")) {
    src <- paste(readLines(file.path(root, "R", "decisioncombine.b.R"), warn = FALSE), collapse = "\n")
    for (tbl in c("crossTabTable", "recommendationTable", "pairedComparisonTable", "paired", "recTable", "crossTable"))
      expect_false(grepl(paste0("\\b", tbl, "\\$setVisible\\("), src), label = paste("decisioncombine", tbl))
  }
  if ("kappaSizeCI" %in% audit_loaded("kappaSizeCI")) {
    a <- kappaSizeCIClass$new(options = kappaSizeCIOptions$new(props = "0.5, 0.6"), data = data.frame())
    a$init()
    a$results$text1$setContent("previous n")
    expect_error(suppressWarnings(a$run()))
    expect_identical(a$results$text1$content, "previous n")
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: a table whose rows follow from the options has
#   them after .init(), and .run() fills those same rows - it never appears empty and then
#   restructures (guide section 6). Keys are compared, so a row added or dropped by .run() fails.
test_that("option-fixed rows exist after .init() and .run() keeps the same keys", {
  wanted <- c("decisioncompare", "decisioncombine", "decisioncalculator")
  loaded <- audit_loaded(wanted)
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  same_rows <- function(a, tables) {
    before <- lapply(tables, function(t) unlist(a$results[[t]]$rowKeys))
    quiet(a$run())
    expect_identical(a$status, "complete")
    for (i in seq_along(tables)) {
      expect_gt(length(before[[i]]), 0, label = paste(tables[i], "rows after .init()"))
      expect_identical(unlist(a$results[[tables[i]]]$rowKeys), before[[i]], label = tables[i])
    }
  }
  set.seed(4)
  n <- 300
  gold <- sample(c("pos", "neg"), n, TRUE, c(0.4, 0.6))
  mk <- function(se, sp) factor(ifelse(gold == "pos", sample(c("pos", "neg"), n, TRUE, c(se, 1 - se)),
                                       sample(c("neg", "pos"), n, TRUE, c(sp, 1 - sp))), levels = c("neg", "pos"))
  d <- data.frame(gold = factor(gold, levels = c("neg", "pos")), t1 = mk(.8, .85), t2 = mk(.7, .9), t3 = mk(.9, .6))

  if ("decisioncompare" %in% loaded) {
    a <- decisioncompareClass$new(options = decisioncompareOptions$new(
      gold = "gold", goldPositive = "pos", goldNegative = "neg",
      test1 = "t1", test1Positive = "pos", test1Negative = "neg",
      test2 = "t2", test2Positive = "pos", test2Negative = "neg",
      test3 = "t3", test3Positive = "pos", test3Negative = "neg",
      opa = TRUE, statComp = TRUE), data = d)
    quiet(a$init())
    same_rows(a, c("comparisonTable", "opaTable", "mcnemarTable", "diffTable"))
  }
  if ("decisioncombine" %in% loaded) {
    for (t3 in list(NULL, "t3")) {
      a <- decisioncombineClass$new(options = decisioncombineOptions$new(
        gold = "gold", goldPositive = "pos", test1 = "t1", test1Positive = "pos",
        test2 = "t2", test2Positive = "pos", test3 = t3, test3Positive = if (!is.null(t3)) "pos"),
        data = d)
      quiet(a$init())
      same_rows(a, c("combinationTable", "combinationTableCI", "combinationTableCIRatios"))
    }
  }
  if ("decisioncalculator" %in% loaded) {
    a <- decisioncalculatorClass$new(options = decisioncalculatorOptions$new(
      TP = 90, FP = 30, FN = 20, TN = 80, showWorkload = TRUE, prevalenceScenarios = "0.01,0.05",
      pp = TRUE, pprob = 0.2), data = data.frame())
    quiet(a$init())
    same_rows(a, "screeningWorkload")
    expect_length(a$results$screeningWorkload$rowKeys, 4)   # observed + 0.2, 0.01, 0.05
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: every variable role sits in the panel's first,
#   top-level VariableSupplier (optional roles greyed with enable:), as jmv's linreg does - never a
#   second supplier inside a collapsed box, never another control above the variables.
test_that("variable boxes come first, all in one top-level supplier", {
  root <- audit_source_root()
  skip_if_not_installed("yaml")
  absent <- audit_absent(root, audit_analyses, "jamovi/%s.u.yaml")
  suppliers_below <- function(node, depth = 0L) {
    if (!is.list(node)) return(0L)
    here <- if (identical(node$type, "VariableSupplier") && depth > 0L) 1L else 0L
    here + sum(vapply(node$children %||% list(), suppliers_below, integer(1), depth = depth + 1L))
  }
  for (name in setdiff(audit_analyses, absent)) {
    top <- yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".u.yaml")))$children
    types <- vapply(top, function(x) x$type %||% "", "")
    if (!"VariableSupplier" %in% types) next   # calculators take no variables
    expect_identical(types[1], "VariableSupplier", label = paste(name, "first control"))
    expect_identical(sum(vapply(top, suppliers_below, integer(1))), 0L, label = paste(name, "nested suppliers"))
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: enhancedROC's negative reference level is a Level
#   (LevelSelector) bound to the outcome, so it lists the levels instead of matching typed text; an unset
#   one is written as NULL in the syntax, because a Level is a required argument of the R function.
test_that("enhancedROC's negative reference level is chosen from the outcome's levels", {
  wanted <- "enhancedROC"
  if ("enhancedROC" %in% audit_loaded(wanted)) {
    opts <- enhancedROCOptions$new(outcome = "y", positiveClass = "pos", predictors = "s")
    # jmvcore's OptionLevel is its OptionString class, so read the schema instead
    root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
    if (file.exists(file.path(root, "jamovi", "enhancedROC.a.yaml"))) {
      a_yaml <- yaml::read_yaml(file.path(root, "jamovi", "enhancedROC.a.yaml"))$options
      vn <- Filter(function(o) identical(o$name, "validationNegative"), a_yaml)[[1]]
      expect_identical(c(vn$type, vn$variable), c("Level", "(outcome)"))
    }
    expect_null(opts$validationNegative)
    set.seed(9)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 30)), s = c(stats::rnorm(30), stats::rnorm(30, 1)))
    a <- enhancedROCClass$new(options = opts, data = d)
    src <- a$asSource()
    expect_match(src, "validationNegative = NULL", fixed = TRUE)
    call <- str2lang(src)
    call[[1]] <- quote(enhancedROC)
    res <- suppressWarnings(eval(call, envir = list2env(list(data = d), parent = environment())))
    expect_true(inherits(res, "enhancedROCResults"))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: a fixed number of numbers is Number options, whose
#   boxes check their own input (enhancedROC partialAucFrom/To, psychopdaROC treatmentThresholdMin/Max/Step);
#   a String holding a number list must say why it stays one (`# number-list:` above its option).
test_that("fixed number ranges are Number options; number-list Strings are marked", {
  root <- audit_source_root()
  absent <- audit_absent(root, audit_analyses, "jamovi/%s.a.yaml")
  for (name in setdiff(audit_analyses, absent)) {
    lines <- readLines(file.path(root, "jamovi", paste0(name, ".a.yaml")), warn = FALSE)
    opts <- yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".a.yaml")))$options
    for (o in opts) {
      d <- as.character(o$default %||% "")
      if (!identical(o$type, "String") || !grepl("^\\s*-?[0-9.]+(\\s*[,;]\\s*-?[0-9.]+){1,5}\\s*$", d)) next
      at <- grep(paste0("^\\s*- name: ", o$name, "\\s*$"), lines)[1]
      expect_true(any(grepl("number-list:", lines[max(1, at - 3):(at - 1)], fixed = TRUE)),
                  label = paste(name, o$name, "marked"))
    }
  }
  if ("enhancedROC" %in% audit_loaded("enhancedROC")) {
    o <- enhancedROCOptions$new(outcome = "y", positiveClass = "p", predictors = "s")
    expect_identical(c(o$partialAucFrom, o$partialAucTo), c(0.8, 1))
  }
  if ("psychopdaROC" %in% audit_loaded("psychopdaROC")) {
    o <- psychopdaROCOptions$new()
    expect_identical(c(o$treatmentThresholdMin, o$treatmentThresholdMax, o$treatmentThresholdStep), c(0.05, 0.5, 0.05))
    expect_identical(.psychopdaThresholdGrid(0.1, 0.3, 0.1)$values, c(0.1, 0.2, 0.3))
    expect_true(.psychopdaThresholdGrid(0.4, 0.2, 0.1)$fallback)   # reversed: default grid + note
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [MEDIUM] DONE: a clinical preset greys the controls it overrides,
#   so the panel never shows a value the analysis is not using as if it were in force.
test_that("enhancedROC presets grey the controls they override", {
  root <- audit_source_root()
  absent <- audit_absent(root, "enhancedROC", "jamovi/%s.u.yaml")
  if (!length(absent)) {
    u <- yaml::read_yaml(file.path(root, "jamovi", "enhancedROC.u.yaml"))
    enable_of <- function(node, name) {
      if (!is.list(node)) return(NULL)
      if (identical(node$name, name)) return(node$enable %||% "")
      for (child in c(node$children, if (is.null(names(node))) node)) {
        hit <- enable_of(child, name)
        if (!is.null(hit)) return(hit)
      }
      NULL
    }
    expect_identical(enable_of(u$children, "youdenOptimization"), "(clinicalPresets:custom)")
    for (box in c("sensitivityThreshold", "specificityThreshold"))
      expect_identical(enable_of(u$children, box),
                       "(clinicalPresets:custom || clinicalPresets:research_comprehensive)", label = box)
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [LOW] PARTIAL: the notice panels stay Html until jamovi's Notice takes
#   a title and a body, but each states its severity in words, an error is reported once (by reject()),
#   and no comment still gives the retracted "Notice cannot be serialized" reason (guide section 13).
test_that("notice panels name their severity, errors are reported once, no stale Notice reason", {
  root <- audit_source_root()
  absent <- audit_absent(root, audit_analyses)
  for (name in setdiff(audit_analyses, absent)) {
    raw <- readLines(file.path(root, "R", paste0(name, ".b.R")), warn = FALSE)
    code <- raw[!grepl("^\\s*#", raw)]
    expect_false(any(grepl('icon = ""', code, fixed = TRUE)), label = paste(name, "empty severity icon"))
    expect_false(any(grepl("alert-danger", code, fixed = TRUE)), label = paste(name, "red banner"))
    expect_false(any(grepl("<b>Error:</b>", code, fixed = TRUE)), label = paste(name, "summary error line"))
    for (i in grep("seriali[sz]", raw, ignore.case = TRUE)) {
      near <- paste(raw[max(1, i - 2):min(length(raw), i + 2)], collapse = " ")
      if (grepl("^\\s*#", raw[i]) && grepl("Notice", near))
        expect_match(near, "retract|corrected|fine|not about|never existed|fails identically",
                     label = paste0(name, ".b.R:", i))
    }
  }
  if ("lassologistic" %in% audit_loaded("lassologistic") && requireNamespace("glmnet", quietly = TRUE)) {
    d <- data.frame(y = factor(rep(c("a", "b"), 3)), x1 = 1:6, x2 = c(2, 1, 4, 3, 6, 5))  # far too few rows
    a <- lassologisticClass$new(options = lassologisticOptions$new(outcome = "y", outcomeLevel = "b",
                                                                   explanatory = c("x1", "x2")), data = d)
    suppressWarnings(a$init()); suppressWarnings(a$run(noThrow = TRUE))
    if (identical(a$status, "error")) {
      expect_false(grepl("alert-danger", a$results$todo$content, fixed = TRUE))
      expect_false(grepl("Data Error", a$results$notices$content, fixed = TRUE))
    }
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [LOW] DONE: help sits in the empty state - cotest's guidance is off
#   by default (it has no empty state) and enhancedROC's instructions show only until inputs are chosen.
test_that("instructional panels do not sit above every result", {
  wanted <- c("cotest", "enhancedROC")
  loaded <- audit_loaded(wanted)
  if ("cotest" %in% loaded) expect_false(cotestOptions$new()$showGuidance)
  if ("enhancedROC" %in% loaded) {
    set.seed(2)
    d <- data.frame(y = factor(rep(c("neg", "pos"), each = 20)), s = c(stats::rnorm(20), stats::rnorm(20, 1)))
    # outcome chosen, no predictor yet: still the empty state (no variables at all trips jmvcore's select())
    empty <- enhancedROCClass$new(options = enhancedROCOptions$new(outcome = "y"), data = d)
    suppressWarnings(empty$init())
    expect_true(empty$results$results$instructions$visible)
    full <- enhancedROCClass$new(options = enhancedROCOptions$new(outcome = "y", positiveClass = "pos",
                                                                  predictors = "s"), data = d)
    suppressWarnings(full$init())
    expect_false(full$results$results$instructions$visible)
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-10-23 meddecide [LOW] PARTIAL: image state holds what the plot draws. The rater
#   profile's bars keep counts, not every rating; the dot plot keeps one value and class flag per
#   analyzed patient (its points) with the threshold, direction and labels once. Old saved states render.
test_that("plot states hold drawing data, and old saved states still render", {
  wanted <- c("psychopdaROC", "agreement")
  loaded <- audit_loaded(wanted)
  draw <- function(img) { f <- tempfile(fileext = ".png"); on.exit(unlink(f)); img$saveAs(f); file.exists(f) }
  if ("psychopdaROC" %in% loaded) {
    set.seed(12)
    cls <- factor(rep(c("H", "D"), c(50, 30)), levels = c("H", "D"))
    d <- data.frame(m = stats::rnorm(80, ifelse(cls == "D", 1, 0)), cls = cls)
    a <- psychopdaROCClass$new(options = psychopdaROCOptions$new(dependentVars = "m", classVar = "cls",
      positiveClass = "D", negativeClass = "H", showDotPlot = TRUE, run = TRUE), data = d)
    suppressWarnings({ a$init(); a$run() })
    img <- a$results$dotPlot$get(key = "m")
    st <- img$state
    expect_false(is.data.frame(st))
    expect_identical(lengths(st[c("value", "positive")]), c(value = 80L, positive = 80L))
    expect_length(st$threshold, 1L)
    expect_true(draw(img))
    img$setState(data.frame(value = d$m, class = ifelse(cls == "D", "Positive", "Negative"),
                            threshold = 0.5, direction = ">=", stringsAsFactors = FALSE))   # pre-2026-10-25 shape
    expect_true(draw(img))
  }
  if ("agreement" %in% loaded) {
    set.seed(13)
    lv <- c("Benign", "Atypical", "Malignant")
    d <- as.data.frame(replicate(4, factor(sample(lv, 120, TRUE), lv), simplify = FALSE))
    names(d) <- paste0("R", 1:4)
    a <- agreementClass$new(options = agreementOptions$new(vars = names(d), raterProfiles = TRUE), data = d)
    suppressWarnings({ a$init(); a$run() })
    img <- a$results$raterProfilePlot
    expect_identical(nrow(img$state$plot_data), 4L * 3L)        # raters x categories, not 480 ratings
    expect_equal(sum(img$state$plot_data$n), 480)
    expect_true(draw(img))
  }
  audit_skip_unloaded(wanted)
})

# 2026-10-23 meddecide [LOW], not applicable as raised: every table named there already lists its own
#   clearWith (placed after columns:), and no display-only toggle is in it, so ticking one restores the
#   cells instead of clearing them. This keeps it that way (and guards the 2026-09-16 fix).
test_that("display-only toggles are not in the clearWith of the result tables", {
  root <- audit_source_root()
  skip_if_not_installed("yaml")
  named <- list(decision = c("cTable", "nTable", "ratioTable"),
                lassologistic = c("modelSummary", "coefficients", "performance"),
                agreement = c("irrtable", "krippTable"))
  absent <- audit_absent(root, names(named), "jamovi/%s.r.yaml")
  find <- function(n, name) {
    if (!is.list(n)) return(NULL)
    if (identical(n$name, name)) return(n)
    for (x in n) { hit <- find(x, name); if (!is.null(hit)) return(hit) }
    NULL
  }
  for (a in setdiff(names(named), absent)) {
    r <- yaml::read_yaml(file.path(root, "jamovi", paste0(a, ".r.yaml")))
    opts <- vapply(yaml::read_yaml(file.path(root, "jamovi", paste0(a, ".a.yaml")))$options,
                   function(o) o$name %||% "", "")
    toggles <- grep("^show|Guide$|Explanations$|Guidance$|Methodology|NaturalLanguage|ReportTemplate|AboutAnalysis",
                    opts, value = TRUE)
    for (t in named[[a]]) {
      cw <- find(r$items, t)$clearWith
      expect_gt(length(cw), 0L, label = paste(a, t, "has clearWith"))
      expect_identical(intersect(toggles, unlist(cw)), character(0), label = paste(a, t))
    }
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [LOW] DONE: an optional agreement sub-analysis that cannot run raises
#   a WARNING notice naming it, not an "error" footnote on a table whose cells stay empty.
test_that("agreement reports a failed sub-analysis as a named warning notice", {
  root <- audit_source_root()
  absent <- audit_absent(root, "agreement")
  if (!length(absent)) {
    src <- readLines(file.path(root, "R", "agreement.b.R"), warn = FALSE)
    code <- paste(src[!grepl("^\\s*#", src)], collapse = "\n")
    expect_false(grepl('setNote\\(\\s*"error"\\s*,(?!\\s*NULL)', code, perl = TRUE))
  }
  if ("agreement" %in% audit_loaded("agreement")) {
    set.seed(21)
    d <- data.frame(r1 = factor(sample(c("a", "b"), 30, TRUE)), r2 = factor(sample(c("a", "b"), 30, TRUE)))
    a <- agreementClass$new(options = agreementOptions$new(vars = c("r1", "r2"), raterClustering = TRUE), data = d)
    suppressWarnings({ a$init(); a$run() })
    notices <- Filter(function(x) inherits(x, "Notice") && isTRUE(x$visible), a$results$items)
    expect_true(any(vapply(notices, function(x) grepl("3 raters", x$content, fixed = TRUE), logical(1))))
    expect_false("error" %in% names(a$results$raterClusterTable$notes))
  }
  audit_skip_absent(absent, root)
})

# library-audit 2026-10-23 meddecide [LOW] PARTIAL: the validation-context boxes stay (they are a
#   structured reporting block in the results and the R output), and in decision they are now greyed
#   with "Show validation context" off, as decisioncompare's already were.
test_that("the validation-context boxes are greyed until their table is shown", {
  root <- audit_source_root()
  pair <- c("decision", "decisioncompare")
  absent <- audit_absent(root, pair, "jamovi/%s.u.yaml")
  boxes <- c("cohortRole", "samplingDesign", "thresholdSource", "modelVersion", "analysisUnit")
  for (name in setdiff(pair, absent)) {
    nodes <- list()
    walk <- function(node) {
      if (!is.list(node)) return()
      if (!is.null(node$name)) nodes[[node$name]] <<- node
      for (child in c(node$children, if (is.null(names(node))) node)) walk(child)
    }
    walk(yaml::read_yaml(file.path(root, "jamovi", paste0(name, ".u.yaml")))$children)
    for (box in boxes)
      expect_identical(nodes[[box]]$enable, "(showValidationContext)", label = paste(name, box))
  }
  audit_skip_absent(absent, root)
})
