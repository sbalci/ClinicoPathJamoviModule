audit_analyses <- c(
  "tableone", "summarydata", "reportcat", "benford", "checkdata",
  "dataquality", "outlierdetection", "agepyramid", "venn", "vartree",
  "alluvial", "crosstable", "chisqposttest", "categorize"
)

audit_root <- normalizePath(
  testthat::test_path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

audit_source_available <- dir.exists(file.path(audit_root, "jamovi")) &&
  dir.exists(file.path(audit_root, "R")) &&
  file.exists(file.path(audit_root, "DESCRIPTION"))

read_definition <- function(analysis, kind) {
  yaml::read_yaml(file.path(audit_root, "jamovi", paste0(analysis, ".", kind, ".yaml")))
}

collect_named_nodes <- function(value) {
  found <- list()
  visit <- function(node) {
    if (!is.list(node)) return()
    if (!is.null(node$name)) found[[length(found) + 1L]] <<- node
    for (child in node) visit(child)
  }
  visit(value)
  found
}

collect_refs <- function(value) {
  found <- character()
  visit <- function(node) {
    if (!is.list(node)) return()
    if (!is.null(node$refs)) found <<- c(found, unlist(node$refs, use.names = FALSE))
    for (child in node) visit(child)
  }
  visit(value)
  unique(found)
}

test_that("audited YAML definitions have internally consistent option names", {
  skip_if_not(audit_source_available, "package source tree not available")

  for (analysis in audit_analyses) {
    analysis_def <- read_definition(analysis, "a")
    result_def <- read_definition(analysis, "r")
    ui_def <- read_definition(analysis, "u")

    option_names <- vapply(analysis_def$options, `[[`, character(1), "name")
    ui_names <- vapply(collect_named_nodes(ui_def$children), function(node) {
      as.character(node$name)
    }, character(1))
    result_nodes <- collect_named_nodes(result_def$items)
    clear_with <- unique(unlist(lapply(result_nodes, `[[`, "clearWith")))

    expect_true(
      all(ui_names %in% option_names),
      info = paste(analysis, "contains a UI control without a matching option")
    )
    expect_true(
      all(clear_with %in% option_names),
      info = paste(analysis, "contains an invalid clearWith entry")
    )
  }
})

test_that("release and dependency metadata satisfy the audit", {
  skip_if_not(audit_source_available, "package source tree not available")

  description <- read.dcf(file.path(audit_root, "DESCRIPTION"))
  imports <- trimws(strsplit(description[1, "Imports"], ",")[[1]])
  imports <- sub("\\s*\\(.*$", "", imports)
  suggests <- trimws(strsplit(description[1, "Suggests"], ",")[[1]])
  suggests <- sub("\\s*\\(.*$", "", suggests)
  module_meta <- yaml::read_yaml(file.path(audit_root, "jamovi", "0000.yaml"))

  version_string <- unname(description[1, "Version"])
  expect_gte(utils::compareVersion(version_string, "1.0.0"), 0)
  expect_equal(as.character(module_meta$version), version_string)
  expect_true(all(c("MASS", "boot") %in% imports))
  expect_true("naniar" %in% c(imports, suggests))
})

test_that("citation references resolve with exact casing", {
  skip_if_not(audit_source_available, "package source tree not available")

  references <- yaml::read_yaml(file.path(audit_root, "jamovi", "00refs.yaml"))
  reference_names <- names(references$refs)

  for (analysis in audit_analyses) {
    result_def <- read_definition(analysis, "r")
    referenced <- collect_refs(result_def)
    expect_true(
      all(referenced %in% reference_names),
      info = paste(analysis, "contains an unresolved citation key")
    )
  }

  expect_true(all(c("UpSetR", "ComplexUpset", "naniar") %in% reference_names))
  expect_false("BaylorEdPsych" %in% reference_names)
})

test_that("result visibility and invalidation match audited behavior", {
  skip_if_not(audit_source_available, "package source tree not available")

  benford_results <- collect_named_nodes(read_definition("benford", "r")$items)
  benford_text <- Filter(function(node) identical(node$name, "text"), benford_results)[[1]]
  expect_false(identical(benford_text$visible, FALSE))

  crosstable_results <- collect_named_nodes(read_definition("crosstable", "r")$items)
  data_results <- Filter(function(node) {
    node$name %in% c("todo", "todo2", "analysisInfo", paste0("tablestyle", 1:4))
  }, crosstable_results)
  expect_true(all(vapply(data_results, function(node) {
    "excl" %in% node$clearWith
  }, logical(1))))

  chisq_ui <- read_definition("chisqposttest", "u")
  chisq_groups <- Filter(function(node) {
    identical(node$type, "CollapseBox")
  }, chisq_ui$children)
  expect_equal(
    vapply(chisq_groups, `[[`, character(1), "label"),
    c("Analysis Options", "Advanced Settings", "Output Panels")
  )
  expect_true(all(vapply(chisq_groups[-1], `[[`, logical(1), "collapsed")))

  checkdata_ui <- read_definition("checkdata", "u")
  checkdata_groups <- Filter(function(node) {
    identical(node$type, "CollapseBox")
  }, checkdata_ui$children)
  expect_equal(
    vapply(checkdata_groups, `[[`, character(1), "label"),
    c("Analysis Options", "Advanced Settings", "Educational Panels")
  )
  expect_true(all(vapply(checkdata_groups[-1], `[[`, logical(1), "collapsed")))

  alluvial_results <- read_definition("alluvial", "r")$items
  alluvial_notices <- Filter(function(node) {
    identical(node$name, "notices")
  }, alluvial_results)[[1]]
  expect_true(all(c("excl", "fillGgalluvial", "marg", "usetitle") %in%
    alluvial_notices$clearWith))
})

test_that("alluvial caches prepared data and preserves an external fill", {
  data <- data.frame(
    axis_a = factor(c("x", "y", NA, "x")),
    axis_b = factor(c("m", NA, "n", "m")),
    flow_group = factor(c("u", "v", "u", NA)),
    count = c(1, 2, 3, 4)
  )

  keep <- alluvial(
    data = data,
    vars = c("axis_a", "axis_b"),
    engine = "ggalluvial",
    fillGgalluvial = "flow_group",
    weight = "count",
    excl = FALSE
  )
  drop <- alluvial(
    data = data,
    vars = c("axis_a", "axis_b"),
    engine = "ggalluvial",
    fillGgalluvial = "flow_group",
    weight = "count",
    excl = TRUE
  )

  expect_equal(nrow(keep$plot$state$data), 4)
  expect_equal(nrow(drop$plot$state$data), 1)
  expect_equal(keep$plot$state$fill_var, "flow_group")
  expect_equal(sum(keep$plot$state$data$count), sum(data$count))
  expect_false(grepl(
    "self\\$data",
    paste(deparse(body(alluvialClass$private_methods$.plot)), collapse = "\n")
  ))
})

test_that("alluvial rejects invalid weight roles before aggregation", {
  all_missing <- data.frame(
    axis_a = factor(c("x", "y")),
    axis_b = factor(c("m", "n")),
    count = c(NA_real_, NA_real_)
  )
  missing_result <- alluvial(
    data = all_missing,
    vars = c("axis_a", "axis_b"),
    engine = "ggalluvial",
    weight = "count"
  )
  expect_null(missing_result$plot$state)
  expect_match(missing_result$notices$content, "No Valid Weights")

  reused <- data.frame(
    axis_a = c(1, 2),
    axis_b = factor(c("m", "n"))
  )
  reused_result <- alluvial(
    data = reused,
    vars = c("axis_a", "axis_b"),
    engine = "ggalluvial",
    weight = "axis_a"
  )
  expect_null(reused_result$plot$state)
  expect_match(reused_result$notices$content, "Weight Variable Reused")
})

test_that("HTML table backends escape data-derived labels", {
  payload <- "<img src=x onerror=alert(1)>"
  data <- data.frame(
    feature = factor(rep(c(payload, "safe"), 20)),
    group = factor(rep(c("A", "B"), each = 20)),
    check.names = FALSE
  )
  attr(data$feature, "label") <- payload

  table_one <- tableone(
    data = data,
    vars = "feature",
    sty = "t3",
    showSummary = FALSE,
    showAbout = FALSE,
    showReportSentence = FALSE
  )
  table_one_html <- paste(as.character(table_one$tablestyle3$content), collapse = "\n")
  expect_false(grepl(payload, table_one_html, fixed = TRUE))
  expect_match(table_one_html, "&lt;img", fixed = TRUE)
  expect_match(table_one_html, "<table", fixed = TRUE)

  arsenal <- crosstable(
    data = data,
    vars = "feature",
    group = "group",
    sty = "arsenal"
  )
  finalfit <- crosstable(
    data = data,
    vars = "feature",
    group = "group",
    sty = "finalfit"
  )
  arsenal_html <- paste(as.character(arsenal$tablestyle1$content), collapse = "\n")
  finalfit_html <- paste(as.character(finalfit$tablestyle2$content), collapse = "\n")
  for (html in c(arsenal_html, finalfit_html)) {
    expect_false(grepl(payload, html, fixed = TRUE))
    expect_match(html, "&lt;img", fixed = TRUE)
    expect_match(html, "<table", fixed = TRUE)
  }
  expect_match(arsenal_html, "Pearson", fixed = TRUE)

  named_data <- data
  attr(named_data$feature, "label") <- NULL
  names(named_data)[1] <- payload
  named_arsenal <- do.call(crosstable, list(
    data = named_data,
    vars = payload,
    group = "group",
    sty = "arsenal"
  ))
  named_finalfit <- do.call(crosstable, list(
    data = named_data,
    vars = payload,
    group = "group",
    sty = "finalfit"
  ))
  for (html in c(
    paste(as.character(named_arsenal$tablestyle1$content), collapse = "\n"),
    paste(as.character(named_finalfit$tablestyle2$content), collapse = "\n")
  )) {
    expect_false(grepl(payload, html, fixed = TRUE))
    expect_match(html, "&lt;img", fixed = TRUE)
  }

  caption_data <- data
  attr(caption_data$feature, "label") <- NULL
  names(caption_data)[2] <- payload
  tangram <- do.call(crosstable, list(
    data = caption_data,
    vars = "feature",
    group = payload,
    sty = "nejm"
  ))
  tangram_html <- paste(
    as.character(tangram$tablestyle4$content),
    collapse = "\n"
  )
  expect_false(grepl(payload, tangram_html, fixed = TRUE))
  expect_match(tangram_html, "&lt;img", fixed = TRUE)
})

test_that("crosstable retains all data-quality messages when appending output", {
  data <- data.frame(
    feature = factor(c("x", "x", "y", "y", "x", "y")),
    group = factor(c("A", "A", "A", "B", "B", "B"))
  )
  result <- crosstable(
    data = data,
    vars = "feature",
    group = "group",
    sty = "gtsummary",
    p_adjust = "bonferroni"
  )
  notice <- as.character(result$dataQualityNotice$content)
  expect_match(notice, "Very small sample size", fixed = TRUE)
  expect_match(notice, "Small group detected", fixed = TRUE)
  expect_match(notice, "P-value adjustment with only 1 variable", fixed = TRUE)
})

test_that("categorize source literals round-trip special characters", {
  generate_code <- categorizeClass$private_methods$.generateRCode
  labels <- paste0("O'Brien, path", "\\", "root, line one\nline two")
  code <- generate_code(
    varname = "tumor grade",
    method = "equal",
    nbins = 3,
    breaks = "",
    sdmult = 1,
    labels = "custom",
    customlabels = labels,
    newvarname = "risk group",
    includelowest = TRUE,
    rightclosed = TRUE,
    ordered = TRUE
  )

  parsed <- parse(text = code)
  environment <- new.env(parent = baseenv())
  environment$data <- data.frame("tumor grade" = 1:9, check.names = FALSE)
  eval(parsed, envir = environment)

  expect_equal(
    environment$labels,
    c("O'Brien", "path\\root", "line one\nline two")
  )
  expect_true("risk group" %in% names(environment$data))
})

test_that("chi-square chunk sizing and generated source handle both dimensions", {
  comparison_count <- chisqposttestClass$private_methods$.pairwiseComparisonCount
  expect_equal(comparison_count(matrix(1, nrow = 2, ncol = 8)), 29)

  options <- chisqposttestOptions$new(
    rows = "tumor grade",
    cols = "response-group"
  )
  source <- chisqposttestClass$new(options = options)$asSource()
  expect_error(parse(text = source), NA)
  expect_equal(lengths(regmatches(source, gregexpr("rows =", source, fixed = TRUE))), 1)
  expect_equal(lengths(regmatches(source, gregexpr("cols =", source, fixed = TRUE))), 1)
})

test_that("MCAR diagnostics have explicit safe preconditions", {
  mcar_message <- dataqualityClass$private_methods$.mcarTestMessage
  expect_match(
    mcar_message(data.frame(x = c(1, NA))),
    "at least two numeric variables",
    fixed = TRUE
  )
  expect_match(
    mcar_message(data.frame(x = 1:3, y = 4:6)),
    "have no missing values",
    fixed = TRUE
  )
})

test_that("audited source removes obsolete and fragile patterns", {
  skip_if_not(audit_source_available, "package source tree not available")

  source_text <- function(name) {
    paste(readLines(file.path(audit_root, "R", paste0(name, ".b.R")), warn = FALSE),
          collapse = "\n")
  }
  source_lines <- function(name) {
    readLines(file.path(audit_root, "R", paste0(name, ".b.R")), warn = FALSE)
  }

  expect_match(source_text("summarydata"), "\\u{00B1}", fixed = TRUE)
  expect_false(grepl("&plusmn;", source_text("summarydata"), fixed = TRUE))
  expect_false(grepl("BaylorEdPsych", source_text("dataquality"), fixed = TRUE))
  expect_match(source_text("vartree"), "tryCatch(vtree::vtree", fixed = TRUE)
  expect_false(any(grepl("^\\s*stop\\(", source_lines("tableone"))))
  expect_false(any(grepl("^\\s*stop\\(", source_lines("agepyramid"))))

  crosstable_schema <- paste(
    readLines(file.path(audit_root, "jamovi", "crosstable.a.yaml"), warn = FALSE),
    collapse = "\n"
  )
  expect_false(grepl("# - name: stratify", crosstable_schema, fixed = TRUE))
  expect_false(grepl("# - name: posthoc_method", crosstable_schema, fixed = TRUE))
  expect_false(grepl(
    "TODO (security): zero `htmltools::htmlEscape` calls",
    source_text("venn"),
    fixed = TRUE
  ))
  expect_match(
    source_text("venn"),
    "htmlEscape(conditionMessage(e))",
    fixed = TRUE
  )
})

test_that("audited backends do not import whole third-party namespaces", {
  skip_if_not(audit_source_available, "package source tree not available")

  allowed <- c("jmvcore", "magrittr")
  for (analysis in audit_analyses) {
    lines <- readLines(
      file.path(audit_root, "R", paste0(analysis, ".b.R")),
      warn = FALSE
    )
    imports <- sub("^#' @import\\s+", "", grep("^#' @import\\s+", lines, value = TRUE))
    expect_true(
      all(imports %in% allowed),
      info = paste(analysis, "contains an unnecessary whole-package roxygen import")
    )
  }
})

test_that("audited checkbox labels use noun phrases", {
  skip_if_not(audit_source_available, "package source tree not available")

  action_verb <- "^(Show|Enable|Include|Exclude|Export|Generate|Calculate|Perform|Use)\\b"
  for (analysis in audit_analyses) {
    options <- read_definition(analysis, "a")$options
    bool_titles <- vapply(Filter(function(option) {
      identical(option$type, "Bool")
    }, options), `[[`, character(1), "title")
    expect_false(
      any(grepl(action_verb, bool_titles)),
      info = paste(analysis, "contains an action-verb checkbox title")
    )
  }
})
