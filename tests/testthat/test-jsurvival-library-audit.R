# A parked analysis (T/P/D menuGroup) is deleted from the generated module, and a maintainer's
# checkout can be mid-removal. So each test audits the analyses present and ends with a skip that
# names the rest: an absent file only says "cannot open the connection", and gating a whole test on
# one analysis hides the others. The helpers below are the ones in test-meddecide-library-audit.R.

# Which of `names` lack a file here; `files` are sprintf() templates relative to `root`. Matched
# against list.files(), not file.exists(): macOS disks ignore case, so file.exists() finds a
# misspelt name there and Linux CI does not.
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

# Every analysis on the production Survival menu (the ones the updater ships to
# jsurvival), read from the .a.yaml files so a routing change cannot leave this stale,
# and from jamovi/0000.yaml, which still lists one whose files a checkout mid-removal
# has already deleted (the updater drops the entry together with the files).
jsurvival_audit_analyses <- local({
  jamovi <- testthat::test_path("..", "..", "jamovi")
  files <- list.files(jamovi, "\\.a\\.yaml$", full.names = TRUE)
  group <- vapply(files, function(f) {
    g <- grep("^menuGroup:", readLines(f, warn = FALSE), value = TRUE)
    if (length(g)) trimws(sub("^menuGroup:", "", g[1])) else ""
  }, "")
  manifest <- file.path(jamovi, "0000.yaml")
  listed <- if (file.exists(manifest)) {
    Filter(function(a) identical(a$menuGroup, "Survival"), yaml::read_yaml(manifest)$analyses)
  }
  sort(unique(c(
    sub("\\.a\\.yaml$", "", basename(files[group == "Survival"])),
    vapply(listed, function(a) as.character(a$name), "")
  )))
})

jsurvival_audit_root <- normalizePath(
  testthat::test_path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

jsurvival_source_available <- dir.exists(file.path(jsurvival_audit_root, "jamovi")) &&
  dir.exists(file.path(jsurvival_audit_root, "R")) &&
  file.exists(file.path(jsurvival_audit_root, "DESCRIPTION"))

# The audited analyses this tree lacks a file for, and the ones it has.
jsurvival_absent <- audit_absent(
  jsurvival_audit_root,
  jsurvival_audit_analyses,
  c("jamovi/%s.a.yaml", "jamovi/%s.r.yaml", "jamovi/%s.u.yaml", "R/%s.b.R")
)
jsurvival_present <- setdiff(jsurvival_audit_analyses, jsurvival_absent)

read_jsurvival_definition <- function(analysis, kind) {
  yaml::read_yaml(file.path(
    jsurvival_audit_root,
    "jamovi",
    paste0(analysis, ".", kind, ".yaml")
  ))
}

collect_jsurvival_nodes <- function(value) {
  found <- list()
  visit <- function(node) {
    if (!is.list(node)) return()
    if (!is.null(node$name)) found[[length(found) + 1L]] <<- node
    for (child in node) visit(child)
  }
  visit(value)
  found
}

collect_jsurvival_values <- function(value, key) {
  found <- character()
  visit <- function(node) {
    if (!is.list(node)) return()
    if (!is.null(names(node))) {
      for (i in seq_along(node)) {
        if (identical(names(node)[i], key)) {
          found <<- c(found, unlist(node[[i]], use.names = FALSE))
        } else {
          visit(node[[i]])
        }
      }
    } else {
      for (child in node) visit(child)
    }
  }
  visit(value)
  unique(as.character(found))
}

read_jsurvival_source <- function(analysis) {
  paste(
    readLines(
      file.path(jsurvival_audit_root, "R", paste0(analysis, ".b.R")),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

test_that("jsurvival schema references remain internally consistent", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  for (analysis in jsurvival_present) {
    analysis_def <- read_jsurvival_definition(analysis, "a")
    result_def <- read_jsurvival_definition(analysis, "r")
    ui_def <- read_jsurvival_definition(analysis, "u")
    source <- read_jsurvival_source(analysis)

    option_names <- vapply(analysis_def$options, `[[`, character(1), "name")
    option_controls <- Filter(
      function(node) {
        !is.null(node$type) && node$type %in% c(
          "CheckBox", "ComboBox", "TextBox", "VariableTargetListBox",
          "LevelSelector", "Output"
        )
      },
      collect_jsurvival_nodes(ui_def$children)
    )
    ui_names <- vapply(
      option_controls,
      function(node) as.character(node$name),
      character(1)
    )
    result_nodes <- collect_jsurvival_nodes(result_def$items)
    clear_with <- unique(unlist(lapply(result_nodes, `[[`, "clearWith")))

    expect_true(
      all(ui_names %in% option_names),
      info = paste(analysis, "contains a UI name without a matching option")
    )
    expect_true(
      all(clear_with %in% option_names),
      info = paste(analysis, "contains an invalid clearWith entry")
    )

    for (render_fun in collect_jsurvival_values(result_def, "renderFun")) {
      pattern <- paste0(
        gsub("[.]", "\\\\.", render_fun),
        "\\s*=\\s*function\\s*\\("
      )
      expect_match(
        source,
        pattern,
        perl = TRUE,
        info = paste(analysis, "does not implement", render_fun)
      )
    }
  }
  audit_skip_absent(jsurvival_absent, jsurvival_audit_root)
})

test_that("jsurvival release and citation versions are synchronized", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  description <- read.dcf(file.path(jsurvival_audit_root, "DESCRIPTION"))
  module_meta <- yaml::read_yaml(file.path(jsurvival_audit_root, "jamovi", "0000.yaml"))
  citation <- yaml::read_yaml(file.path(jsurvival_audit_root, "CITATION.cff"))
  version <- unname(description[1, "Version"])

  expect_gte(utils::compareVersion(version, "1.0.0"), 0)
  expect_equal(as.character(module_meta$version), version)
  expect_equal(as.character(citation$version), version)
  expect_equal(as.character(citation$`date-released`), unname(description[1, "Date"]))
})

test_that("jsurvival citation catalog contains exactly the required standalone keys", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  # as.character(): with every analysis absent unlist() gives NULL, which expect_setequal() rejects
  referenced <- unique(as.character(unlist(lapply(jsurvival_present, function(analysis) {
    collect_jsurvival_values(read_jsurvival_definition(analysis, "r"), "refs")
  }))))
  defined <- names(yaml::read_yaml(
    file.path(jsurvival_audit_root, "jamovi", "00refs.yaml")
  )$refs)
  package_name <- unname(read.dcf(
    file.path(jsurvival_audit_root, "DESCRIPTION"),
    fields = "Package"
  )[1, 1])

  expect_setequal(intersect(referenced, defined), referenced)
  if (identical(package_name, "jsurvival")) {
    # An absent analysis's citations stay until the updater rewrites 00refs.yaml: routing, not dead
    # keys. The skip lists them, so a key no absent analysis explains is still in view.
    extra <- setdiff(defined, referenced)
    if (length(extra))
      audit_skip_absent(jsurvival_absent, jsurvival_audit_root,
                        paste("keys no present analysis cites:", paste(extra, collapse = ", ")))
    expect_setequal(defined, referenced)
  }
})

test_that("oddsratio validation rejects cleanly on both audited error paths", {
  description_path <- file.path(jsurvival_audit_root, "DESCRIPTION")
  package_name <- if (file.exists(description_path)) {
    unname(read.dcf(description_path, fields = "Package")[1, 1])
  } else {
    "jsurvival"
  }
  skip_if_not(
    identical(package_name, "jsurvival"),
    "runtime error paths are exercised in the standalone distribution"
  )
  # the class loaded here, not whatever jsurvival is installed: that may be older, or still hold a
  # parked oddsratio
  audit_skip_unloaded("oddsratio")
  oddsratio <- getExportedValue(package_name, "oddsratio")

  empty_data <- data.frame(
    outcome = factor(character(), levels = c("no", "yes")),
    predictor = numeric()
  )
  # Both outcome levels are required (outcomeNegativeLevel, added 2026-10-28); with
  # the negative level unset the analysis shows guidance instead of reaching validation.
  expect_error(
    oddsratio(
      data = empty_data,
      explanatory = "predictor",
      outcome = "outcome",
      outcomeLevel = "yes",
      outcomeNegativeLevel = "no",
      predictorLevel = NULL,
      predictorNegativeLevel = NULL
    ),
    "No data available for analysis",
    fixed = TRUE
  )

  invalid_data <- data.frame(
    outcome = factor(rep("yes", 20), levels = c("no", "yes")),
    predictor = seq_len(20)
  )
  expect_error(
    oddsratio(
      data = invalid_data,
      explanatory = "predictor",
      outcome = "outcome",
      outcomeLevel = "yes",
      outcomeNegativeLevel = "no",
      predictorLevel = NULL,
      predictorNegativeLevel = NULL
    ),
    "Outcome variable must have at least 2 different values for logistic regression",
    fixed = TRUE
  )
})

test_that("symbol entities and fragmented translation calls are absent", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  sources <- vapply(
    jsurvival_present,
    read_jsurvival_source,
    character(1)
  )
  expect_false(any(grepl("&(mdash|times|ndash|beta|ge);", sources, perl = TRUE)))

  translated <- c("survival", "singlearm")
  absent <- audit_absent(jsurvival_audit_root, translated)
  translation_sources <- vapply(setdiff(translated, absent), read_jsurvival_source, character(1))
  active_lines <- unlist(lapply(strsplit(translation_sources, "\n", fixed = TRUE), function(lines) {
    lines[!grepl("^\\s*#", lines)]
  }))
  expect_false(any(grepl("paste0?\\(\\.\\(", active_lines, perl = TRUE)))
  audit_skip_absent(union(jsurvival_absent, absent), jsurvival_audit_root)
})

test_that("audited backends use selective namespace imports", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  sources <- vapply(
    jsurvival_present,
    read_jsurvival_source,
    character(1)
  )
  broad_packages <- paste(
    c("dplyr", "finalfit", "glue", "jmvcore", "lubridate", "magrittr", "survival", "survminer"),
    collapse = "|"
  )
  expect_false(any(grepl(
    paste0("#'\\s+@import\\s+(", broad_packages, ")\\b"),
    sources,
    perl = TRUE
  )))
  expect_false(any(grepl("@rawNamespace\\s+import\\(", sources, perl = TRUE)))

  package_name <- unname(read.dcf(
    file.path(jsurvival_audit_root, "DESCRIPTION"),
    fields = "Package"
  )[1, 1])
  if (identical(package_name, "jsurvival")) {
    namespace <- readLines(file.path(jsurvival_audit_root, "NAMESPACE"), warn = FALSE)
    expect_false(any(grepl(
      paste0("^import\\((", broad_packages, ")(,|\\))"),
      namespace,
      perl = TRUE
    )))
    expect_true(any(grepl('^importFrom\\(jmvcore,("[.]"|[.])\\)$', namespace)))
  }
  audit_skip_absent(jsurvival_absent, jsurvival_audit_root)
})

test_that("checkbox labels use noun phrases", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  action_pattern <- paste0(
    "^(Show|Display|Enable|Use|Include|Perform|Calculate|Find|Extract|Test|",
    "Stratify|Add|Remove)( |$)"
  )

  for (analysis in jsurvival_present) {
    analysis_def <- read_jsurvival_definition(analysis, "a")
    ui_def <- read_jsurvival_definition(analysis, "u")
    option_titles <- setNames(
      vapply(analysis_def$options, function(option) {
        if (is.null(option$title)) "" else as.character(option$title)
      }, character(1)),
      vapply(analysis_def$options, `[[`, character(1), "name")
    )
    checkboxes <- Filter(
      function(node) identical(node$type, "CheckBox"),
      collect_jsurvival_nodes(ui_def$children)
    )

    for (checkbox in checkboxes) {
      # timeinterval's two row-dropping filters keep the verb on purpose: the
      # label must say rows are deleted from every statistic, person-time
      # included (rationale in jamovi/timeinterval.u.yaml, 2026-09-01).
      if (analysis == "timeinterval" &&
          checkbox$name %in% c("remove_negative", "remove_extreme")) next
      label <- checkbox$label
      if (is.null(label) || !nzchar(label)) label <- option_titles[[checkbox$name]]
      expect_false(
        grepl(action_pattern, label),
        info = paste(analysis, checkbox$name, "uses an action-style checkbox label")
      )
    }
  }
  audit_skip_absent(jsurvival_absent, jsurvival_audit_root)
})

test_that("disabled survival-tree scaffolding is not exposed in the UI", {
  skip_if_not(jsurvival_source_available, "package source tree not available")
  audit_skip_absent(
    audit_absent(jsurvival_audit_root, "multisurvival", c("jamovi/%s.a.yaml", "jamovi/%s.u.yaml")),
    jsurvival_audit_root
  )

  analysis_def <- read_jsurvival_definition("multisurvival", "a")
  ui_def <- read_jsurvival_definition("multisurvival", "u")
  option_names <- vapply(analysis_def$options, `[[`, character(1), "name")
  ui_names <- vapply(
    collect_jsurvival_nodes(ui_def$children),
    function(node) as.character(node$name),
    character(1)
  )
  tree_names <- c("use_tree", "min_node", "complexity", "max_depth", "show_terminal_nodes")

  expect_length(intersect(option_names, tree_names), 0L)
  expect_length(intersect(ui_names, tree_names), 0L)
})

test_that("survivalPower support is shipped only where the analysis exists", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  description <- read.dcf(file.path(jsurvival_audit_root, "DESCRIPTION"))
  module_meta <- yaml::read_yaml(file.path(jsurvival_audit_root, "jamovi", "0000.yaml"))
  analysis_names <- vapply(module_meta$analyses, `[[`, character(1), "name")

  if (identical(unname(description[1, "Package"]), "jsurvival")) {
    expect_false(file.exists(file.path(
      jsurvival_audit_root,
      "R",
      "survivalPower_distributions.R"
    )))
    expect_false(file.exists(file.path(
      jsurvival_audit_root,
      "jamovi",
      "js",
      "survivalPower.events.js"
    )))
  } else {
    expect_true("survivalPower" %in% analysis_names)
  }
})

test_that("audited backends contain no direct code-execution calls", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  forbidden <- c("eval", "parse", "system", "system2", "shell", "source")
  for (analysis in jsurvival_present) {
    parsed <- parse(
      file.path(jsurvival_audit_root, "R", paste0(analysis, ".b.R")),
      keep.source = TRUE
    )
    parse_data <- getParseData(parsed)
    calls <- parse_data$text[parse_data$token == "SYMBOL_FUNCTION_CALL"]
    expect_equal(
      sort(intersect(calls, forbidden)),
      character(),
      info = paste(analysis, "contains a forbidden execution call")
    )
  }
  audit_skip_absent(jsurvival_absent, jsurvival_audit_root)
})

# A result that changes with the random seed names that seed next to it (Random seed option).
test_that("seed-dependent jsurvival results show the seed that drew them", {
  wanted <- c("survival", "multisurvival", "lassocox")
  loaded <- audit_loaded(wanted)
  skip_if_not_installed("glmnet")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  shown <- "Random seed: 777"
  notes <- function(table) unname(vapply(table$notes, function(n) n$note, ""))
  run <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    quiet(a$init())
    quiet(a$run())
    a
  }
  set.seed(9)
  n <- 160
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n), x4 = stats::rnorm(n),
                  age = stats::rnorm(n, 60, 10), trt = factor(sample(c("A", "B"), n, TRUE)))
  d$time <- stats::rexp(n, exp(0.6 * d$x1 + 0.4 * (d$trt == "B")) / 24)
  d$status <- stats::rbinom(n, 1, 0.75)
  d$event <- factor(ifelse(d$status == 1, "Yes", "No"))

  if ("survival" %in% loaded) {
    sv <- run("survival", d, elapsedtime = "time", outcome = "status", outcomeLevel = "1", explanatory = "trt",
              bootstrapValidation = TRUE, bootstrapValN = 50, seed = 777)
    expect_true(shown %in% notes(sv$results$bootstrapValidationTable))
  }

  if ("multisurvival" %in% loaded) {
    ms <- run("multisurvival", d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
              explanatory = "trt", contexpl = "age", ci_optimism = TRUE, ci_optimism_boot = 50, seed = 777)
    expect_true(shown %in% notes(ms$results$cindexValidation))
  }

  if ("lassocox" %in% loaded) {
    lc <- run("lassocox", d, elapsedtime = "time", outcome = "event", outcomeLevel = "Yes", censorLevel = "No",
              explanatory = c("x1", "x2", "x3", "x4", "age"), random_seed = 777,
              cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE)
    expect_true(shown %in% notes(lc$results$modelSummary))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class): tables whose row set is fixed by the code or by
#   an option have their rows before .run(); a statistic that cannot be computed stays as a blank row
#   rather than vanishing, so "not estimable" is distinguishable from "never tried"
test_that("fixed-row jsurvival tables are scaffolded before .run()", {
  wanted <- c("survival", "singlearm", "multisurvival")
  loaded <- audit_loaded(wanted)
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  set.seed(10)
  n <- 80
  d <- data.frame(time = round(stats::rexp(n, 0.025) + 1, 2), status = stats::rbinom(n, 1, 0.7),
                  trt = factor(sample(c("A", "B"), n, TRUE)), age = stats::rnorm(n, 60, 10))
  if ("survival" %in% loaded) {
    sv <- after_init("survival", d, elapsedtime = "time", outcome = "status", outcomeLevel = "1", explanatory = "trt",
                     calibration_curves = TRUE, use_parametric = TRUE, compare_distributions = TRUE)
    expect_identical(sv("calibrationTable"), c("slope", "meancal", "mae", "cindex"))
    expect_identical(sv("parametricModelComparison"),
                     c("exp", "weibull", "lnorm", "llogis", "gamma", "gengamma", "gompertz"))
  }
  if ("singlearm" %in% loaded) {
    sa <- after_init("singlearm", d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                     advancedDiagnostics = TRUE)
    expect_identical(sa("dataQualityTable"),
                     c("n_total", "n_events", "event_rate", "followup_range", "median_followup", "memory",
                       "time_complete", "outcome_complete"))
  }
  if ("multisurvival" %in% loaded) {
    ms <- after_init("multisurvival", d, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
                     explanatory = "trt", contexpl = "age", ci_optimism = TRUE)
    expect_identical(ms("cindexValidation"), c("apparent", "optimism", "corrected"))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-15 jsurvival [LOW] DONE: image state holds what is drawn, never patient rows
# or a model fit; the nomogram stores its rms::nomogram object (guide §17)
test_that("jsurvival image states hold drawing data, not patient rows", {
  wanted <- "oddsratio"
  if ("oddsratio" %in% audit_loaded(wanted)) {
    set.seed(3)
    n <- 200
    d <- data.frame(grp = factor(sample(c("A", "B"), n, TRUE)), age = round(stats::rnorm(n, 60, 10)))
    risk <- stats::plogis(-1 + 0.8 * (d$grp == "B") + 0.02 * (d$age - 60))
    d$y <- factor(ifelse(stats::runif(n) < risk, "Dead", "Alive"))
    a <- get("oddsratioClass")$new(
      options = get("oddsratioOptions")$new(explanatory = c("grp", "age"), outcome = "y",
                                            outcomeLevel = "Dead", outcomeNegativeLevel = "Alive",
                                            showNomogram = TRUE),
      data = d)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    st <- a$results$plot_nomogram$state
    expect_identical(names(st), "nom")
    expect_s3_class(st$nom, "nomogram")
    has_rows <- function(x) is.data.frame(x) || (is.list(x) && any(vapply(x, has_rows, logical(1))))
    expect_false(has_rows(st))
    # what a reopened .omv has: no private cache, only the saved state
    a$.__enclos_env__$private$.nom_object <- NULL
    f <- tempfile(fileext = ".png")
    a$results$plot_nomogram$saveAs(f)
    expect_gt(file.size(f), 5000)
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-15 jsurvival [LOW] DONE: cleaning and suitability delegate their
#   coding/design and collinearity/PH phases without changing the fitted inputs.
test_that("lassocox extracted outcome-coding, design-matrix, and suitability helpers match .cleanData()", {
  wanted <- "lassocox"
  skip_if_not_installed("glmnet")
  if ("lassocox" %in% audit_loaded(wanted)) {
    set.seed(42)
    n <- 80
    d <- data.frame(
      time = round(stats::rexp(n, 0.05) + 0.5, 2),
      event = factor(ifelse(stats::rbinom(n, 1, 0.6) == 1, "Event", "Censored")),
      x1 = stats::rnorm(n),
      x2 = stats::rnorm(n),
      grp = factor(sample(c("Ref", "Alt"), n, replace = TRUE), levels = c("Ref", "Alt"))
    )
    a <- get("lassocoxClass")$new(
      options = get("lassocoxOptions")$new(
        elapsedtime = "time",
        outcome = "event",
        outcomeLevel = "Event",
        censorLevel = "Censored",
        explanatory = c("x1", "x2", "grp"),
        suitabilityCheck = TRUE,
        cv_plot = FALSE,
        coef_plot = FALSE,
        survival_plot = FALSE
      ),
      data = d
    )
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    priv <- a$.__enclos_env__$private

    cleaned <- suppressWarnings(priv$.cleanData())
    enc <- priv$.encodeOutcome(d$event)
    expect_identical(enc$status, cleaned$status)
    expect_identical(enc$event_level_used, "Event")
    expect_identical(enc$censor_level_used, "Censored")

    dm <- suppressWarnings(priv$.buildDesignMatrix(
      d[c("x1", "x2", "grp")],
      !is.na(d$time) & !is.na(enc$status),
      rep(TRUE, n),
      sum(enc$status == 1)
    ))
    expect_identical(dm$X, cleaned$X)
    expect_identical(dm$encoding, cleaned$encoding)

    col_check <- priv$.assessCollinearity(cleaned)
    expect_true(col_check$color %in% c("green", "yellow", "red"))
    ph_check <- priv$.assessPhAssumption(cleaned, c(1L, 2L))
    expect_true(ph_check$color %in% c("green", "yellow", "red"))
  }
  audit_skip_unloaded(wanted)
})

test_that("pre-release mathematical and clinical edge cases remain guarded", {
  wanted <- c("outcomeorganizer", "datetimeconverter", "survival", "survivalcont")
  loaded <- audit_loaded(wanted)
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  run <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    quiet(a$init())
    quiet(a$run())
    a
  }

  if ("outcomeorganizer" %in% loaded) {
    d_oo <- data.frame(
      id = c("P1", "P1", "P1"),
      time = c(5, 10, 15),
      status = factor(c("Alive", "Dead", NA), levels = c("Alive", "Dead"))
    )
    oo <- run("outcomeorganizer", d_oo, outcome = "status", outcomeLevel = "Dead",
              patientID = "id", followupTime = "time", useHierarchy = TRUE, eventPriority = 1)
    oo_priv <- oo$.__enclos_env__$private
    cleaned <- quiet(oo_priv$.organizeOutcomes(oo_priv$.getData()))
    expect_identical(as.integer(cleaned$mydata$myoutcome), c(0L, 1L, NA_integer_))
  }

  if ("datetimeconverter" %in% loaded) {
    d_dt <- data.frame(dt = as.Date(c("2023-06-15", "2023-12-01")))
    dtc <- run("datetimeconverter", d_dt, datetime_var = "dt", timezone = "America/New_York",
               extract_day = TRUE)
    prep <- quiet(dtc$.__enclos_env__$private$.prepareDatetimeInput(d_dt$dt))
    expect_identical(as.integer(lubridate::day(prep$parsed_dates)), c(15L, 1L))
  }

  if ("survival" %in% loaded) {
    set.seed(11)
    d_sv <- data.frame(
      time = c(rep(10, 8), seq(12, 30, length.out = 22)),
      status = c(rep(1, 15), rep(0, 15)),
      grp = factor(rep(c("A", "B"), each = 15))
    )
    sv <- run("survival", d_sv, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
              explanatory = "grp", rmst_analysis = TRUE, rmst_tau = 5)
    rmst_df <- sv$results$rmstTable$asDF
    expect_equal(nrow(rmst_df), 2L)
    expect_equal(as.numeric(rmst_df$rmst), c(5, 5))
    expect_equal(as.numeric(rmst_df$se), c(0, 0))
  }

  if ("survivalcont" %in% loaded) {
    set.seed(12)
    d_sc <- data.frame(
      time = seq(1, 40, length.out = 40),
      status = c(rep(1, 12), rep(0, 28)),
      marker = seq(10, 50, length.out = 40)
    )
    sc <- run("survivalcont", d_sc, elapsedtime = "time", outcome = "status", outcomeLevel = "1",
              contexpl = "marker", findcut = TRUE, showSummaries = TRUE)
    med_txt <- as.character(sc$results$medianSummary$content)
    expect_false(grepl("NA \\[NA - NA", med_txt))
  }

  audit_skip_unloaded(wanted)
})

# library-audit 2026-09-15 jsurvival [LOW] DONE: reviewed entry points delegate phases;
#   endpoint coding and optional censoring must not grow back into one orchestration body.
test_that("reviewed survival orchestration methods remain readable phase entry points", {
  sites <- list(
    timeinterval = ".run", oddsratio = ".run",
    outcomeorganizer = c(".run", ".organizeOutcomes"),
    datetimeconverter = ".run", survivalcont = ".run",
    lassocox = c(".cleanData", ".assessSuitability")
  )
  for (name in audit_loaded(names(sites))) {
    methods <- get(paste0(name, "Class"))$private_methods
    for (method in sites[[name]]) {
      # Deparse measures executable structure independently of comment density.
      expect_true(length(deparse(body(methods[[method]]), width.cutoff = 100L)) <= 200L,
        info = paste(name, method))
    }
  }
  audit_skip_unloaded(names(sites))
})


# R1: compare the rendered probabilities with an independent baseline-hazard oracle.
.sac_ns <- function() {
  for (p in c("jsurvival", "ClinicoPath"))
    if (p %in% loadedNamespaces()) return(asNamespace(p))
  stop("Load the analysis namespace before running this regression")
}
.sac_data <- function() {
  set.seed(1893); n <- 360
  site <- factor(sample(c("low", "high"), n, TRUE))
  group <- factor(ifelse(runif(n) < ifelse(site == "high", .85, .15), "B", "A"))
  age <- runif(n, 35, 85)
  data.frame(time = rexp(n, .02 * ifelse(site == "high", 6, 1) * exp(.025 * (age - 60))),
             event = rbinom(n, 1, .8), group = group, age = age, site = site)
}
.sac_run <- function(d, mode = "none", ...) {
  opts <- list(elapsedtime = "time", outcome = "event", outcomeLevel = "1",
    explanatory = "group", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    age_adjustment = TRUE, age_variable = "age", adjusted_curves = TRUE, cutp = "6,12")
  if (mode %in% c("site", "cross"))
    opts <- c(opts, list(stratified_cox = TRUE, strata_variable = "site"))
  if (mode %in% c("age", "cross")) opts$age_stratified_cox <- TRUE
  opts <- utils::modifyList(opts, list(...), keep.null = TRUE)
  ns <- .sac_ns()
  a <- get("survivalClass", ns)$new(options = do.call(get("survivalOptions", ns)$new, opts), data = d)
  a$init(); suppressMessages(a$run())
  a
}
.sac_plot <- function(a) {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  p <- a$.__enclos_env__$private
  ok <- p$.plotAdjustedCurves(a$results$adjustedCurvesPlot, ggplot2::theme_minimal(), list(palette = "jmv"))
  testthat::expect_true(isTRUE(ok))
  ggplot2::last_plot()$data
}
.sac_oracle <- function(d, mode, times) {
  age_strata <- mode %in% c("age", "cross")
  site_strata <- mode %in% c("site", "cross")
  d <- d[stats::complete.cases(d[, c("time", "event", "group", "age",
    if (site_strata) "site")]), , drop = FALSE]
  d$band <- cut(d$age, c(-Inf, 50, 65, 75, Inf), include.lowest = TRUE)
  d$stratum <- if (age_strata && site_strata) interaction(d$site, d$band, drop = TRUE)
    else if (age_strata) d$band else if (site_strata) d$site else factor("all")
  rhs <- c("group", if (!age_strata) "age", if (age_strata || site_strata) "strata(stratum)")
  f <- stats::as.formula(paste("survival::Surv(time,event) ~", paste(rhs, collapse = " + ")))
  fit <- survival::coxph(f, d, model = TRUE)
  bh <- survival::basehaz(fit, centered = FALSE)
  rows <- lapply(levels(d$group), function(g) {
    nd <- d; nd$group <- factor(g, levels = levels(d$group))
    risk <- exp(stats::predict(fit, nd, type = "lp", reference = "zero"))
    s <- vapply(times, function(t) {
      h <- vapply(as.character(nd$stratum), function(k) {
        use <- bh$time <= t
        if ("strata" %in% names(bh)) use <- use & as.character(bh$strata) == k
        if (any(use)) tail(bh$hazard[use], 1) else 0
      }, 0)
      mean(exp(-h * risk))
    }, numeric(1))
    data.frame(group = g, time = times, surv = s)
  })
  list(curves = do.call(rbind, rows), fit = fit, data = d)
}
.sac_at <- function(curves, groups, times) {
  key <- if ("group" %in% names(curves)) "group" else "variable"
  unlist(lapply(groups, function(g) vapply(times, function(t) {
    rows <- curves[as.character(curves[[key]]) == g & curves$time <= t, , drop = FALSE]
    if (nrow(rows)) tail(rows$surv, 1) else 1
  }, 0)), use.names = FALSE)
}

testthat::test_that("adjusted curves honor baseline, age and crossed stratification", {
  d <- .sac_data(); times <- c(1, 3, 6)
  for (mode in c("none", "site", "age", "cross")) {
    a <- .sac_run(d, mode); actual <- .sac_plot(a)
    ref <- .sac_oracle(d, mode, times)
    testthat::expect_equal(.sac_at(actual, levels(d$group), times), ref$curves$surv,
                          tolerance = 1e-8, info = mode)
    hr <- a$results$ageAdjustedCoxTable$asDF$hr_age_adjusted[1]
    testthat::expect_equal(as.numeric(sub(" .*", "", hr)),
      unname(exp(stats::coef(ref$fit)["groupB"])), tolerance = .0051, info = mode)
  }
})

testthat::test_that("standardized curves stop at common stratum follow-up and retain source exclusions", {
  d <- .sac_data(); d$time[d$site == "high"] <- pmin(d$time[d$site == "high"], 8)
  d$event[d$site == "high" & d$time == 8] <- 0
  d$age[c(2, 11)] <- NA; d$site[4] <- NA
  rownames(d) <- as.character(seq_len(nrow(d)) * 3L)
  a <- .sac_run(d, "site", uselandmark = TRUE, landmark = 1)
  actual <- .sac_plot(a)
  use <- d[d$time > 1, , drop = FALSE]; use$time <- use$time - 1
  ref <- .sac_oracle(use, "site", c(1, 3, 6))
  testthat::expect_equal(.sac_at(actual, levels(d$group), c(1, 3, 6)), ref$curves$surv,
                        tolerance = 1e-8)
  testthat::expect_lte(max(actual$time), min(tapply(ref$data$time, ref$data$site, max)))
})

testthat::test_that("adjusted curve state holds coordinates and renders after dataset-free restoration", {
  a <- .sac_run(.sac_data(), "cross")
  state <- a$results$adjustedCurvesPlot$state
  testthat::expect_false("plot_data" %in% names(state))
  restored <- .sac_run(.sac_data(), "cross")
  restored$results$adjustedCurvesPlot$setState(unserialize(serialize(state, NULL)))
  restored$.__enclos_env__$private$.data <- NULL
  testthat::expect_equal(.sac_plot(restored), .sac_plot(a))
})

testthat::test_that("adjusted curves invalidate on every stratification control", {
  ns <- .sac_ns(); result <- get("survivalResults", ns)$new(get("survivalOptions", ns)$new())
  clear <- result$adjustedCurvesPlot$.__enclos_env__$private$.clearWith
  testthat::expect_true(all(c("stratified_cox", "strata_variable", "age_stratified_cox",
                             "age_group_cutpoints") %in% clear))
})


.fr_ns <- function() {
  for(p in c('jsurvival','ClinicoPath')) if(p %in% loadedNamespaces()) return(asNamespace(p))
  stop('Load the analysis namespace first')
}
.fr_data <- function() {
  set.seed(2811);n<-350
  d<-data.frame(arm=factor(sample(c('Control','Treated'),n,TRUE)),
    age=runif(n,40,80),stage=factor(sample(c('I','II','III'),n,TRUE)),marker=rnorm(n))
  d$time<-rexp(n,.02*exp(.3*(d$arm=='Treated')+.02*(d$age-60)+.15*d$marker))+.1
  d$event<-rbinom(n,1,.8);d
}
.fr_run <- function(d,style,interactions=list(c('arm','age'))) {
  ns<-.fr_ns();o<-get('multisurvivalOptions',ns)$new(elapsedtime='time',outcome='event',
    outcomeLevel='1',explanatory=c('arm','stage'),contexpl=c('age','marker'),
    dod=NULL,dooc=NULL,awd=NULL,awod=NULL,hr=TRUE,sty=style,interactions=interactions)
  a<-get('multisurvivalClass',ns)$new(options=o,data=d);a$init();suppressWarnings(suppressMessages(a$run()));a
}
.fr_render <- function(a,style) {
  grDevices::pdf(NULL);on.exit(grDevices::dev.off(),add=TRUE)
  p<-a$.__enclos_env__$private;im<-a$results[[if(style=='t1')'plot' else 'plot3']]
  ok<-suppressWarnings(suppressMessages(if(style=='t1')p$.plot(im,ggplot2::theme_minimal(),list(palette='jmv')) else p$.plot3(im,ggplot2::theme_minimal(),list(palette='jmv'))))
  testthat::expect_true(isTRUE(ok));ggplot2::last_plot()
}

testthat::test_that('both interaction forest styles retain conditional rows and ratio-of-HR meaning',{
  d<-.fr_data()
  for(style in c('t1','t3')) {
    a<-.fr_run(d,style);p<-.fr_render(a,style)
    caption<-gsub('[[:space:]]+',' ',paste(p$labels$caption,collapse=' '))
    testthat::expect_match(caption,'age = 0',fixed=TRUE,info=style)
    testthat::expect_match(caption,'outside the observed range',fixed=TRUE,info=style)
    testthat::expect_match(caption,'ratios of hazard ratios',fixed=TRUE,info=style)
    wanted<-c('term','estimate','lower','upper','label','estimable')
    has<-is.data.frame(p$data)&&all(wanted %in% names(p$data))
    testthat::expect_true(has,info=paste(style,'each fitted coefficient must have its own labelled row'))
    if(!has)next
    fit<-survival::coxph(survival::Surv(time,event)~arm+stage+age+marker+arm:age,d)
    cf<-stats::coef(fit);se<-sqrt(diag(stats::vcov(fit)));z<-stats::qnorm(.975)
    testthat::expect_equal(p$data$term,names(cf),info=style)
    testthat::expect_equal(p$data$estimate,unname(exp(cf)),tolerance=1e-10,info=style)
    testthat::expect_equal(p$data$lower,unname(exp(cf-z*se)),tolerance=1e-10,info=style)
    testthat::expect_equal(p$data$upper,unname(exp(cf+z*se)),tolerance=1e-10,info=style)
    testthat::expect_match(p$data$label[match('armTreated',p$data$term)],'age = 0',fixed=TRUE)
    testthat::expect_match(p$data$label[match('age',p$data$term)],'arm = Control',fixed=TRUE)
    testthat::expect_match(p$data$label[match('armTreated:age',p$data$term)],'ratio of hazard ratios',fixed=TRUE)
  }
})

testthat::test_that('multi-level and higher-order forest interactions retain all design-matrix rows',{
  d<-.fr_data()
  for(terms in list(list(c('stage','arm')),list(c('stage','arm','age')))) {
    a<-.fr_run(d,'t1',terms);p<-.fr_render(a,'t1');fit<-a$.__enclos_env__$private$.cox_model()
    has<-is.data.frame(p$data)&&all(c('term','label')%in%names(p$data))
    testthat::expect_true(has)
    if(has) {
      testthat::expect_equal(p$data$term,names(stats::coef(fit)))
      testthat::expect_equal(length(unique(p$data$label)),length(stats::coef(fit)))
    }
  }
})

testthat::test_that('non-interaction models keep their existing forest rendering',{
  a<-.fr_run(.fr_data(),'t1',list());p<-.fr_render(a,'t1')
  testthat::expect_null(p$labels$caption)
})

testthat::test_that('restored interaction forests reserve the complete caption layout', {
  for (style in c('t1', 't3')) {
    a <- .fr_run(.fr_data(), style)
    im <- a$results[[if (style == 't1') 'plot' else 'plot3']]
    testthat::expect_setequal(names(im$state), c('has_competing', 'forest_size'))
    size <- im$state$forest_size
    testthat::expect_length(size, 4L)
    im$setSize2(600, 450)
    # The native lifecycle calls postInit after loading the previous drawing state.
    a$.__enclos_env__$private$.postInit()
    ip <- im$.__enclos_env__$private
    if (length(size) == 4L) for (i in seq_along(size))
      testthat::expect_equal(ip[[c('.widthM', '.heightM', '.widthB', '.heightB')[i]]], size[i])
  }
})

# library-audit 2026-11-20 jsurvival [HIGH] DONE: computed values are rows of a Table -- no HTML
# cell that splices a value, no printed model object, in any analysis or the event-indicator helper
test_that("jsurvival computed values are Table rows, not HTML grids or printed text", {
  code_of <- function(path) {
    lines <- readLines(path, warn = FALSE)
    paste(lines[!grepl("^\\s*#", lines)], collapse = "\n")
  }
  helper <- file.path(jsurvival_audit_root, "R", "utils-eventindicator.R")
  sources <- c(
    vapply(file.path(jsurvival_audit_root, "R", paste0(jsurvival_present, ".b.R")), code_of, ""),
    code_of(helper))
  names(sources) <- c(jsurvival_present, "utils-eventindicator")

  # A printed object pushed into a result, and a table cell that splices anything but a
  # translated literal (help tables splice .() / .fmt() only).
  printed <- "setContent\\(\\s*paste0?\\(\\s*(utils::)?capture\\.output\\(\\s*print\\("
  data_cell <- "<t[dh]\\b[^>]*>\\s*\\\\?[\"']\\s*,\\s*(?!\\s|\\.\\(|\\.fmt\\(|jmvcore::format\\(|[\"'])"
  for (name in names(sources)) {
    expect_false(grepl(printed, sources[[name]], perl = TRUE), info = paste(name, "prints an object into a result"))
    expect_false(grepl(data_cell, sources[[name]], perl = TRUE), info = paste(name, "splices a value into an HTML cell"))
  }
  expect_false(grepl("<table|<tr", sources[["utils-eventindicator"]]))

  tables <- list(
    survival = c("eventRecodeTable", "cox_phTable"),
    multisurvival = c("eventRecodeTable", "cox_phTable", "adjustedCoxPHTable", "nomogramPoints", "nomogramRisk"),
    singlearm = "eventRecodeTable", survivalcont = "eventRecodeTable",
    outcomeorganizer = "eventRecodeTable", datetimeconverter = "qualityCounts")
  for (analysis in intersect(names(tables), jsurvival_present)) {
    items <- read_jsurvival_definition(analysis, "r")$items
    types <- stats::setNames(vapply(items, function(i) i$type, ""), vapply(items, function(i) i$name, ""))
    expect_identical(unname(types[tables[[analysis]]]), rep("Table", length(tables[[analysis]])), info = analysis)
  }

  wanted <- c("survival", "multisurvival")
  set.seed(20261120)
  n <- 240
  d <- data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = factor(sample(c("Alive", "Dead"), n, TRUE, prob = c(0.4, 0.6))),
    arm = factor(sample(c("A", "B"), n, TRUE)),
    grade = factor(sample(c("G1", "G2", "G3"), n, TRUE)),
    age = round(stats::rnorm(n, 60, 10)))
  d$status[1:6] <- NA
  run <- function(analysis, ...) {
    a <- get(paste0(analysis, "Class"))$new(
      options = get(paste0(analysis, "Options"))$new(
        elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", ph_cox = TRUE, ...),
      data = d)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    a$results
  }
  zph_rows <- function(fit) {
    tab <- survival::cox.zph(fit)$table
    data.frame(term = rownames(tab), chisq = unname(tab[, "chisq"]),
               df = as.integer(round(tab[, "df"])), p = unname(tab[, "p"]))
  }
  ok <- d[!is.na(d$status), ]
  ok$event <- as.integer(ok$status == "Dead")
  recode_n <- c(sum(ok$event == 1), sum(ok$event == 0), sum(is.na(d$status)))

  if ("survival" %in% audit_loaded(wanted)) {
    r <- run("survival", explanatory = "arm")
    expect_equal(r$eventRecodeTable$asDF$n, recode_n)
    expect_match(r$eventRecodeTable$asDF$levels[1], "Dead", fixed = TRUE)
    expect_false(grepl("<table|<tr", r$eventRecodeInfo$content))
    got <- r$cox_phTable$asDF
    want <- zph_rows(survival::coxph(survival::Surv(time, event) ~ arm, data = ok))
    expect_equal(got$term, want$term)
    expect_equal(got$chisq, want$chisq, tolerance = 1e-6)
    expect_equal(got$df, want$df)
    expect_equal(got$p, want$p, tolerance = 1e-6)
  }
  if ("multisurvival" %in% audit_loaded(wanted)) {
    r <- run("multisurvival", explanatory = c("arm", "grade"), contexpl = "age",
             ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE, showNomogram = TRUE, cutp = "12, 24")
    expect_equal(r$eventRecodeTable$asDF$n, recode_n)
    adj <- r$adjustedCoxPHTable$asDF
    expect_gt(nrow(adj), 1)
    expect_identical(adj$term[nrow(adj)], "GLOBAL")
    expect_true(all(adj$p >= 0 & adj$p <= 1))
    expect_false(isTRUE(r$adjustedCoxPH$visible))
    pts <- r$nomogramPoints$asDF
    expect_true(all(c("arm", "grade", "age") %in% pts$variable))
    expect_true(is.numeric(pts$points) && max(pts$points) == 100)
    risk <- r$nomogramRisk$asDF
    expect_gt(nrow(risk), 1)
    expect_true(all(risk$risk > 0 & risk$risk < 1))
    expect_false(grepl("<table|<tr", r$nomogram_display$content))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-11-20 jsurvival [LOW] PARTIAL: the recode caveats and every explanation panel
# reach the translator as whole paragraphs; sentences built around values in other methods are
# listed by release_gate.py check_untranslated_prose and are converted method by method
test_that("jsurvival explanation panels and recode caveats go through the translator", {
  # Pseudo-translate: every .() string comes back in guillemets, so English left outside them is
  # text the catalogue never sees, and a nested mark is a translated fragment spliced into another.
  mark <- function(text, n = 1) paste0("«", text, "»")
  untranslated <- function(html) {
    text <- gsub("<style.*?</style>", " ", paste(html, collapse = " "), perl = TRUE)
    text <- gsub("«[^»]*»", " ", text)
    text <- gsub("\\s+", " ", gsub("<[^>]*>", " ", text))
    regmatches(text, gregexpr("[A-Za-z][A-Za-z'/-]*(?: [A-Za-z(][A-Za-z'/().,;:-]*){3,}", text, perl = TRUE))[[1]]
  }
  panels <- function(analysis, data, ...) {
    options <- get(paste0(analysis, "Options"))$new(showExplanations = TRUE, ...)
    assign(".translator", list(translate = mark), envir = options$.__enclos_env__$private)
    a <- get(paste0(analysis, "Class"))$new(options = options, data = data)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    items <- a$results$items
    keep <- vapply(items, function(i) inherits(i, "Html") &&
                     (grepl("Explanation$", i$name) || identical(i$name, "eventRecodeInfo")), logical(1))
    content <- lapply(items[keep], function(i) i$content)
    names(content) <- vapply(items[keep], function(i) i$name, "")
    content[nzchar(gsub("<[^>]*>|\\s", "", unlist(content)))]
  }

  set.seed(20261120)
  n <- 200
  d <- data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = factor(sample(c("Alive", "Dead"), n, TRUE, prob = c(0.4, 0.6))),
    arm = factor(sample(c("A", "B"), n, TRUE)),
    age = round(stats::rnorm(n, 60, 10)))
  runs <- list(
    singlearm = function() panels("singlearm", d, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                                  person_time = TRUE, baseline_hazard = TRUE),
    survival = function() panels("survival", d, elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                                 explanatory = "arm", person_time = TRUE, rmst_analysis = TRUE,
                                 residual_diagnostics = TRUE, ph_cox = TRUE),
    survivalcont = function() panels("survivalcont", d, elapsedtime = "time", outcome = "status",
                                     outcomeLevel = "Dead", contexpl = "age", findcut = TRUE))
  wanted <- names(runs)
  for (analysis in audit_loaded(wanted)) {
    shown <- runs[[analysis]]()
    expect_gte(length(shown), 3L)
    expect_true("eventRecodeInfo" %in% names(shown), info = analysis)
    for (name in names(shown)) {
      expect_identical(untranslated(shown[[name]]), character(0), info = paste(analysis, name))
      expect_false(grepl("«[^»]*«", shown[[name]]), info = paste(analysis, name, "nests"))
    }
  }

  # The zero-event and disease-free caveats, through the file-level helper's caller translator.
  helpers <- intersect(c("jsurvival", "ClinicoPath"), loadedNamespaces())
  if (length(helpers) > 0) {
    ns <- asNamespace(helpers[1])
    define <- get(".defineEventIndicator", envir = ns)
    describe <- get(".describeEventIndicator", envir = ns)
    caller <- function(res) { self <- list(options = list(translate = mark)); describe(res, "Outcome") }
    zero <- define(factor(c("Alive", "Alive", "Alive"), levels = c("Alive", "Dead")), outcomeLevel = "Dead")
    dfs <- define(factor(rep(c("DOD", "DOOC", "AWOD"), 5)), multievent = TRUE, analysistype = "dfs",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outcome_name = "Outcome")
    for (res in list(zero, dfs)) {
      expect_null(res$error)
      html <- caller(res)
      expect_match(html, "«")
      expect_identical(untranslated(html), character(0))
    }
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-11-20 jsurvival [LOW] DONE: one route per plot - a curve image draws from the
#   rows and labels in its own state and never loads the dataset, and the rows go only to the
#   images that are switched on (guide section 17)
test_that("jsurvival curve images draw from state alone and only switched-on images hold rows", {
  has_rows <- function(x) is.data.frame(x) || (is.list(x) && any(vapply(x, has_rows, logical(1))))
  set.seed(20261120)
  n <- 160
  d <- data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = factor(sample(c("Alive", "Dead"), n, TRUE, prob = c(0.4, 0.6))),
    cause = factor(sample(c("AWOD", "AWD", "DOD", "DOOC"), n, TRUE)),
    age = round(stats::rnorm(n, 60, 10)))
  d[["Treatment Arm"]] <- factor(sample(c("A", "B"), n, TRUE))
  run <- function(analysis, ...) {
    a <- get(paste0(analysis, "Class"))$new(options = get(paste0(analysis, "Options"))$new(...), data = d)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    a
  }
  state <- function(a, name) a$results$get(name)$state
  # What a resize, an export and a reopened .omv have: no dataset and no per-run cache.
  redraws <- function(a, name) {
    p <- a$.__enclos_env__$private
    for (field in intersect(c(".data", ".cachedGetData"), names(p))) assign(field, NULL, envir = p)
    f <- tempfile(fileext = ".png")
    ok <- tryCatch({ suppressWarnings(suppressMessages(a$results$get(name)$saveAs(f))); TRUE },
                   error = function(e) FALSE)
    ok && file.exists(f) && file.size(f) > 5000
  }
  wanted <- c("survival", "singlearm", "survivalcont")
  loaded <- audit_loaded(wanted)

  if ("survival" %in% loaded) {
    km <- c("plot", "plot2", "plot3", "plot6", "plot7")
    a <- run("survival", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
             explanatory = "Treatment Arm", sc = TRUE, ce = FALSE, ch = FALSE, kmunicate = FALSE,
             loglog = FALSE)
    for (name in km) expect_false(a$results$get(name)$requiresData, info = name)
    expect_identical(ncol(state(a, "plot")$cleanData), 3L)   # time, status, group: no bookkeeping columns
    expect_identical(state(a, "plot")$title2, "Treatment Arm")
    for (name in setdiff(km, "plot")) expect_null(state(a, name), info = paste("survival", name))
    expect_true(redraws(a, "plot"))
    all_on <- run("survival", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                  explanatory = "Treatment Arm", sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE,
                  loglog = TRUE)
    for (name in km) expect_true(redraws(all_on, name), info = paste("survival", name))
  }

  if ("singlearm" %in% loaded) {
    others <- c("plot2", "plot3", "plot6", "baselineHazardPlot", "smoothedHazardPlot")
    a <- run("singlearm", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", sc = TRUE)
    expect_identical(ncol(state(a, "plot")$cleanData), 3L)
    expect_null(state(a, "plot_cif"))
    for (name in others) expect_null(state(a, name), info = paste("singlearm", name))
    all_on <- run("singlearm", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", sc = TRUE,
                  ce = TRUE, ch = TRUE, kmunicate = TRUE, baseline_hazard = TRUE, hazard_smoothing = TRUE)
    for (name in c("plot", others)) expect_true(redraws(all_on, name), info = paste("singlearm", name))
    cr <- run("singlearm", elapsedtime = "time", outcome = "cause", outcomeLevel = "DOD", sc = TRUE,
              multievent = TRUE, analysistype = "compete", dod = "DOD", dooc = "DOOC", awd = "AWD",
              awod = "AWOD")
    expect_null(state(cr, "plot"))
    expect_true(has_rows(state(cr, "plot_cif")))
    expect_true(redraws(cr, "plot_cif"))
    # Competing risks can also arrive in the data: a pre-recoded outcome with multievent off.
    d$recoded <- factor(c("Censored", "Event", "Competing")[1 + (seq_len(n) %% 3)],
                        levels = c("Censored", "Event", "Competing"))
    handoff <- run("singlearm", elapsedtime = "time", outcome = "recoded", outcomeLevel = "Event", sc = TRUE)
    expect_null(state(handoff, "plot"))
    expect_true(has_rows(state(handoff, "plot_cif")))
  }

  if ("survivalcont" %in% loaded) {
    a <- run("survivalcont", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
             contexpl = "age", findcut = TRUE, sc = TRUE, ce = FALSE, ch = FALSE, kmunicate = FALSE,
             loglog = FALSE)
    expect_identical(ncol(state(a, "plot5")$cutoffdata), 3L)
    for (name in c("plot2", "plot3", "plot6", "plot7")) expect_null(state(a, name), info = paste("survivalcont", name))
    all_on <- run("survivalcont", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                  contexpl = "age", findcut = TRUE, sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE,
                  loglog = TRUE)
    for (name in c("plot5", "plot2", "plot3", "plot6", "plot7"))
      expect_true(redraws(all_on, name), info = paste("survivalcont", name))
  }
  audit_skip_unloaded(wanted)
})

# library-audit 2026-11-20 jsurvival [INFO] PARTIAL: the counting-process and interval branches of
#   .buildSurvivalFormula() are kept on purpose; an unknown type is named in its message
test_that("the survival formula builder keeps its counting-process branch and names an unknown type", {
  ns <- intersect(c("jsurvival", "ClinicoPath"), loadedNamespaces())
  ns <- Filter(function(p) exists(".buildSurvivalFormula", envir = asNamespace(p), inherits = FALSE), ns)
  if (length(ns) == 0) audit_skip_unloaded("multisurvival")
  skip_if(length(ns) == 0, "formula builder not in a loaded namespace")
  build <- get(".buildSurvivalFormula", envir = asNamespace(ns[1]))
  counting <- build("t0", "event", "arm", survival_type = "counting", start_var = "t0", stop_var = "t1")
  expect_identical(paste(deparse(counting), collapse = ""), "survival::Surv(t0, t1, event) ~ arm")
  expect_error(build("t0", "event", "arm", survival_type = "counting"), "Start and stop")
  expect_error(build("t0", "event", "arm", survival_type = "bogus"), "bogus")
})

# Shared test files run in the umbrella and in the module. Where the umbrella package is also
# installed, a preamble that tries requireNamespace("ClinicoPath") first tests that installed build
# in place of the module's tree (2026-11-22: 8 of 27 files did, and three failures carried as the
# module's were the installed build's).
test_that("shared tests resolve the package under test, never an installed umbrella copy", {
  files <- list.files(testthat::test_path(), pattern = "^test-.*\\.R$", full.names = TRUE)
  bare <- Filter(function(f) any(grepl("for \\(\\.p in c\\(", readLines(f, warn = FALSE))), files)
  expect_identical(basename(bare), character(0))
  under_test <- Sys.getenv("TESTTHAT_PKG")
  if (nzchar(under_test) && !identical(under_test, "ClinicoPath"))
    expect_false("ClinicoPath" %in% loadedNamespaces())
})

# library-audit 2026-11-20 jsurvival [LOW] DONE: a sentence built around a value is one template,
#   .pasteT(.("... {value} ..."), value = x), so summaries, messages and welcome texts reach the
#   translator whole (guide section 9)
test_that("jsurvival summaries and messages built around values go through the translator", {
  mark <- function(text, n = 1) paste0("«", text, "»")
  outside <- function(text) {                      # English runs left outside balanced marks
    ch <- strsplit(gsub("<style.*?</style>", " ", text, perl = TRUE), "")[[1]]
    depth <- 0L; keep <- logical(length(ch))
    for (i in seq_along(ch)) {
      if (ch[i] == "«") depth <- depth + 1L
      keep[i] <- depth == 0L
      if (ch[i] == "»") depth <- max(0L, depth - 1L)
    }
    t <- gsub("\\s+", " ", gsub("<[^>]*>", " ", paste(ifelse(keep, ch, " "), collapse = "")))
    m <- regmatches(t, gregexpr("[A-Za-z][A-Za-z'/-]*(?: [A-Za-z(][A-Za-z'/().,;:-]*){3,}", t, perl = TRUE))[[1]]
    m[!grepl("Number in dataframe", m, fixed = TRUE)]   # finalfit's own model-metrics line
  }
  set.seed(20261122)
  n <- 160
  d <- data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = factor(sample(c("Alive", "Dead"), n, TRUE, prob = c(0.4, 0.6))),
    arm = factor(sample(c("A", "B"), n, TRUE)), age = round(stats::rnorm(n, 60, 10)),
    cause = factor(sample(c("AWOD", "AWD", "DOD", "DOOC"), n, TRUE)),
    dx = format(as.Date("2018-01-01") + sample(0:700, n, TRUE)), stringsAsFactors = FALSE)
  d$fu <- format(as.Date(d$dx) + sample(30:900, n, TRUE))
  shown <- function(analysis, ...) {
    options <- get(paste0(analysis, "Options"))$new(...)
    assign(".translator", list(translate = mark), envir = options$.__enclos_env__$private)
    a <- get(paste0(analysis, "Class"))$new(options = options, data = d)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    keep <- Filter(function(i) inherits(i, c("Html", "Preformatted")), a$results$items)
    stats::setNames(lapply(keep, function(i) paste(i$content, collapse = "\n")), vapply(keep, function(i) i$name, ""))
  }
  S <- list(elapsedtime = "time", outcome = "status", outcomeLevel = "Dead")
  runs <- list(
    singlearm = function() do.call(shown, c(list("singlearm"), S, list(showSummaries = TRUE, person_time = TRUE, baseline_hazard = TRUE))),
    survival = function() do.call(shown, c(list("survival"), S, list(explanatory = "arm", showSummaries = TRUE, person_time = TRUE, pw = TRUE, ph_cox = TRUE, rmst_analysis = TRUE))),
    survivalcont = function() do.call(shown, c(list("survivalcont"), S, list(contexpl = "age", findcut = TRUE, showSummaries = TRUE, person_time = TRUE, rmst_analysis = TRUE))),
    multisurvival = function() do.call(shown, c(list("multisurvival"), S, list(explanatory = "arm", contexpl = "age", showSummaries = TRUE))),
    oddsratio = function() shown("oddsratio", explanatory = c("arm", "age"), outcome = "status", outcomeLevel = "Dead", outcomeNegativeLevel = "Alive", showExplanations = TRUE),
    outcomeorganizer = function() shown("outcomeorganizer", outcome = "cause", multievent = TRUE, analysistype = "compete", dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", showNaturalSummary = TRUE, showGlossary = TRUE),
    datetimeconverter = function() shown("datetimeconverter", datetime_var = "dx", show_summary = TRUE, show_explanations = TRUE, show_glossary = TRUE),
    timeinterval = function() shown("timeinterval", dx_date = "dx", fu_date = "fu", use_landmark = TRUE, landmark_time = 3, show_summary = TRUE, show_glossary = TRUE))
  wanted <- names(runs)
  for (analysis in audit_loaded(wanted)) {
    items <- runs[[analysis]]()
    expect_gte(length(items), 3L)
    for (name in names(items))
      expect_identical(outside(items[[name]]), character(0), info = paste(analysis, name))
  }
  audit_skip_unloaded(wanted)
})

# Release-profile check 2026-11-23: with no complete row left, the message has to name that, not a
# symptom further down (multisurvival blamed the explanatory variable, lassocox the predictors).
test_that("an entirely missing time column is reported as missing data", {
  set.seed(7)
  n <- 80
  d <- data.frame(time = NA_real_, status = factor(sample(c("Alive", "Dead"), n, TRUE)),
                  arm = factor(sample(c("A", "B"), n, TRUE)), age = round(stats::rnorm(n, 60, 10)),
                  marker = stats::rnorm(n))
  failure <- function(analysis, ...) {
    a <- get(paste0(analysis, "Class"))$new(options = get(paste0(analysis, "Options"))$new(...), data = d)
    tryCatch({ suppressWarnings(suppressMessages({ a$init(); a$run() })); "" }, error = function(e) conditionMessage(e))
  }
  wanted <- c("survival", "multisurvival", "lassocox")
  loaded <- audit_loaded(wanted)
  if ("survival" %in% loaded) {
    msg <- failure("survival", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", explanatory = "arm")
    expect_match(msg, "No complete records remain")
    # The horizon a reader sees is the horizon the estimate used (it was shown to one decimal).
    d$time <- round(stats::rexp(n, 0.05) + 0.513, 3)
    a <- get("survivalClass")$new(options = get("survivalOptions")$new(
      elapsedtime = "time", outcome = "status", outcomeLevel = "Dead", explanatory = "arm", rmst_analysis = TRUE), data = d)
    suppressWarnings(suppressMessages({ a$init(); a$run() }))
    shown <- a$results$rmstTable$asDF
    fit <- survival::survfit(survival::Surv(time, status == "Dead") ~ arm, data = d)
    # Unrounded since the 2026-11-23 audit: the table formats the number, the backend does not round it.
    expect_equal(shown$rmst, unname(summary(fit, rmean = shown$tau[1], extend = TRUE)$table[, "rmean"]), tolerance = 1e-8)
    notes <- paste(vapply(a$results$rmstTable$notes, function(x) x$note, ""), collapse = " ")
    expect_match(notes, base::format(signif(shown$tau[1], 6), trim = TRUE), fixed = TRUE)
    d$time <- NA_real_
  }
  if ("multisurvival" %in% loaded) {
    msg <- failure("multisurvival", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                   explanatory = "arm", contexpl = "age")
    expect_match(msg, "No complete records remain")
    expect_no_match(msg, "has only one value")
  }
  if ("lassocox" %in% loaded) {
    msg <- failure("lassocox", elapsedtime = "time", outcome = "status", outcomeLevel = "Dead",
                   explanatory = c("age", "marker", "arm"))
    expect_match(msg, "Too few complete cases")
    expect_no_match(msg, "constant predictors")
  }
  audit_skip_unloaded(wanted)
})
