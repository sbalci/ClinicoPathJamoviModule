jsurvival_audit_analyses <- c(
  "singlearm", "survival", "survivalcont", "multisurvival",
  "oddsratio", "datetimeconverter", "timeinterval", "outcomeorganizer"
)

jsurvival_audit_root <- normalizePath(
  testthat::test_path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

jsurvival_source_available <- dir.exists(file.path(jsurvival_audit_root, "jamovi")) &&
  dir.exists(file.path(jsurvival_audit_root, "R")) &&
  file.exists(file.path(jsurvival_audit_root, "DESCRIPTION"))

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

  for (analysis in jsurvival_audit_analyses) {
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

  referenced <- unique(unlist(lapply(jsurvival_audit_analyses, function(analysis) {
    collect_jsurvival_values(read_jsurvival_definition(analysis, "r"), "refs")
  })))
  defined <- names(yaml::read_yaml(
    file.path(jsurvival_audit_root, "jamovi", "00refs.yaml")
  )$refs)
  package_name <- unname(read.dcf(
    file.path(jsurvival_audit_root, "DESCRIPTION"),
    fields = "Package"
  )[1, 1])

  expect_setequal(intersect(referenced, defined), referenced)
  if (identical(package_name, "jsurvival")) {
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
  oddsratio <- getExportedValue(package_name, "oddsratio")

  empty_data <- data.frame(
    outcome = factor(character(), levels = c("no", "yes")),
    predictor = numeric()
  )
  expect_error(
    oddsratio(
      data = empty_data,
      explanatory = "predictor",
      outcome = "outcome",
      outcomeLevel = "yes",
      predictorLevel = NULL
    ),
    "No data available for analysis",
    fixed = TRUE
  )

  invalid_data <- data.frame(
    outcome = factor(rep("yes", 20)),
    predictor = seq_len(20)
  )
  expect_error(
    oddsratio(
      data = invalid_data,
      explanatory = "predictor",
      outcome = "outcome",
      outcomeLevel = "yes",
      predictorLevel = NULL
    ),
    "regression. Ensure the outcome variable",
    fixed = TRUE
  )
})

test_that("symbol entities and fragmented translation calls are absent", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  sources <- vapply(
    jsurvival_audit_analyses,
    read_jsurvival_source,
    character(1)
  )
  expect_false(any(grepl("&(mdash|times|ndash|beta|ge);", sources, perl = TRUE)))

  translation_sources <- sources[c("survival", "singlearm")]
  active_lines <- unlist(lapply(strsplit(translation_sources, "\n", fixed = TRUE), function(lines) {
    lines[!grepl("^\\s*#", lines)]
  }))
  expect_false(any(grepl("paste0?\\(\\.\\(", active_lines, perl = TRUE)))
})

test_that("audited backends use selective namespace imports", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  sources <- vapply(
    jsurvival_audit_analyses,
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
})

test_that("checkbox labels use noun phrases", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

  action_pattern <- paste0(
    "^(Show|Display|Enable|Use|Include|Perform|Calculate|Find|Extract|Test|",
    "Stratify|Add|Remove)( |$)"
  )

  for (analysis in jsurvival_audit_analyses) {
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
      label <- checkbox$label
      if (is.null(label) || !nzchar(label)) label <- option_titles[[checkbox$name]]
      expect_false(
        grepl(action_pattern, label),
        info = paste(analysis, checkbox$name, "uses an action-style checkbox label")
      )
    }
  }
})

test_that("disabled survival-tree scaffolding is not exposed in the UI", {
  skip_if_not(jsurvival_source_available, "package source tree not available")

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
  for (analysis in jsurvival_audit_analyses) {
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
})
