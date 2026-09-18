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

  # The invariant is that every manifest agrees with DESCRIPTION -- not that the
  # version is any particular literal. Pinning "1.0.0" here broke on every release.
  pkg_version <- unname(read.dcf(oncopath_file("DESCRIPTION"), fields = "Version")[[1]])
  # jamovi requires the analysis version to be x.y.z, so _updateModules.R writes the
  # first THREE components of the package version (1.0.53.01 -> 1.0.53) into .a.yaml.
  analysis_version <- paste(strsplit(pkg_version, ".", fixed = TRUE)[[1]][1:3], collapse = ".")
  for (analysis_file in analysis_files) {
    schema <- paste(readLines(analysis_file, warn = FALSE), collapse = "\n")
    expect_match(schema, paste0("version: '", analysis_version, "'"), fixed = TRUE)
  }
  expect_match(read_oncopath("jamovi", "0000.yaml"),
               paste0("version: ", pkg_version), fixed = TRUE)
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

test_that("umbrella updater keeps the production OncoPath helper boundary minimal", {
  skip_if_no_oncopath_source()
  package <- read.dcf(oncopath_file("DESCRIPTION"), fields = "Package")[[1]]
  skip_if(package == "OncoPath", "updater configuration belongs to the umbrella")
  config_path <- oncopath_file("_updateModules_config.yaml")
  plan_path <- oncopath_file("_updateModules_plan.R")
  skip_if_not(file.exists(config_path) && file.exists(plan_path), "updater unavailable")

  # Helpers are computed from what OncoPath's analyses use, not listed by hand.
  planner <- new.env(parent = globalenv())
  sys.source(plan_path, envir = planner)
  cfg <- yaml::read_yaml(config_path)
  routes <- planner$route_analyses(oncopath_root, cfg$modules, unlist(cfg$umbrella_only_suffixes))
  analyses <- routes$analysis[routes$module %in% "OncoPath"]
  expect_setequal(analyses, c("diagnosticmeta", "ihcheterogeneity", "swimmerplot", "waterfall"))
  helpers <- planner$resolve_helpers(planner$index_r_sources(oncopath_root),
                                     paste0(analyses, ".b.R"), ignore = character())
  expect_equal(helpers$errors, character(0))
  # swimmerplot needs only the follow-up estimator; the RECIST and IHC engines,
  # the survival formula and event-coding helpers must not leak into OncoPath.
  expect_setequal(helpers$files, c("utils.R", "utils-followup.R", "swimmerplot-html.R"))

  # zzz_imports.R is hand-maintained in OncoPath: never written or deleted.
  expect_true(grepl(planner$.PLAN_KEEP_R, "zzz_imports.R"))
  expect_setequal(unlist(cfg$modules$OncoPath$prune_imports, use.names = FALSE), c("cluster", "tidyr"))
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
  expect_match(source, "mada::phm(mada_data, correction = 0.5", fixed = TRUE)
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
  code <- paste(
    strsplit(source, "\n", fixed = TRUE)[[1]][
      !grepl("^\\s*#", strsplit(source, "\n", fixed = TRUE)[[1]])
    ],
    collapse = "\n"
  )
  # &lt; &gt; &amp; are REQUIRED to keep the HTML well-formed; what must not appear is
  # an entity standing in for a symbol, which is what [[GE]]/[[TIMES]] exist for.
  expect_false(grepl("&(?!lt;|gt;|amp;)[A-Za-z]+;", code, perl = TRUE))
  expect_false(grepl("&#[0-9A-Fa-fxX]+;", code))
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

  pkg_version <- unname(description[1, "Version"])
  # any version R itself accepts -- the repo ships 4-component dev versions (1.0.53.01)
  expect_false(is.na(numeric_version(pkg_version, strict = FALSE)))
  expect_match(citation, 'title: "OncoPath:', fixed = TRUE)
  expect_match(citation, paste0('version: "', pkg_version, '"'), fixed = TRUE)
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
    phm$parameter[1:2],
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

# library-audit 2026-09-16 OncoPath [MEDIUM] DONE: catalogs hold only this module's strings
#   (_updateModules build_module() runs jmvtools::i18nUpdate() after copying the umbrella catalog)
test_that("translation catalog references only this module's sources", {
  skip_if_no_oncopath_source()
  # i18nUpdate() rewrites every "#: <source>" line, so a reference to an R file or analysis
  # this package does not have means the catalog was inherited, not extracted from here.
  pot <- readLines(oncopath_file("jamovi", "i18n", "catalog.pot"), warn = FALSE)
  refs <- unlist(strsplit(sub("^#: ", "", grep("^#: ", pot, value = TRUE)), " ", fixed = TRUE))
  refs <- sub(":[0-9]+$", "", refs)
  owners <- unique(ifelse(startsWith(refs, "R/"), refs, sub("[/.].*$", "", refs)))
  analyses <- sub("\\.a\\.yaml$", "", list.files(oncopath_file("jamovi"), "\\.a\\.yaml$"))
  known <- c(file.path("R", list.files(oncopath_file("R"))), analyses, "package")
  expect_gt(length(owners), 0)
  expect_equal(setdiff(owners, known), character(0))
})

# library-audit 2026-09-16 OncoPath [LOW] DONE: requiresData: true only where the renderer, or a
#   private$ helper it calls, reads self$data (guide section 15)
test_that("images declare requiresData exactly when their renderer reads self$data", {
  skip_if_no_oncopath_source()
  # jmvcore nulls the data once .run() returns and re-reads it for a redraw or export only
  # for an Image that asks. Missing flag: the plot errors on resize/.omv reopen/export.
  # Surplus flag: the whole dataset is re-read for a plot drawn from image$state.
  images <- 0
  mismatches <- character()
  for (name in c("diagnosticmeta", "ihcheterogeneity", "swimmerplot", "waterfall")) {
    src <- sub("#.*$", "", readLines(oncopath_file("R", paste0(name, ".b.R")), warn = FALSE))
    heads <- grep("^\\s*\\.[A-Za-z_][A-Za-z0-9_.]*\\s*=\\s*function\\s*\\(", src)
    ends <- c(heads[-1] - 1, length(src))
    body <- stats::setNames(
      lapply(seq_along(heads), function(i) paste(src[heads[i]:ends[i]], collapse = "\n")),
      sub("^\\s*(\\.[A-Za-z_][A-Za-z0-9_.]*).*$", "\\1", src[heads])
    )
    walk <- function(x) {
      if (!is.list(x)) return(invisible())
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
                                               name, x[["name"]], isTRUE(x[["requiresData"]]),
                                               if (reads) "reads" else "never reads"))
      }
      for (item in x) walk(item)
    }
    walk(yaml::read_yaml(oncopath_file("jamovi", paste0(name, ".r.yaml"))))
  }
  expect_gt(images, 0)
  expect_equal(mismatches, character(0))
})

# library-audit 2026-09-16 OncoPath [LOW] PARTIAL: no .() padding anywhere, and the report-sentence builders
#   splice nothing; DEFERRED noun phrases spliced elsewhere (ihcheterogeneity, swimmerplot, .methodTitle);
#   revisit when those sentences are next rewritten or translated (guide section 9)
test_that("report sentences translate as whole sentences", {
  skip_if_no_oncopath_source()
  # Separators belong to the joining code: a translator cannot see a trailing space.
  lead <- "\\.\\(\\s*\"(?:[\\s,;:]|\\.(?!\\.\\.))"
  trail <- "\\.\\(\\s*\"[^\"\\n]*\\s\"\\s*[,)]"
  padded <- character()
  for (name in c("diagnosticmeta", "ihcheterogeneity", "swimmerplot", "waterfall")) {
    src <- readLines(oncopath_file("R", paste0(name, ".b.R")), warn = FALSE)
    hit <- !grepl("^\\s*#", src) & (grepl(lead, src, perl = TRUE) | grepl(trail, src, perl = TRUE))
    padded <- c(padded, sprintf("%s.b.R:%d", name, which(hit)))
  }
  expect_equal(padded, character(0))

  # Pseudo-translate every .() string as \u00ab...\u00bb. A translated word spliced into a
  # translated sentence then nests, padding shows inside the marks, and logic that compares
  # a translated word with English breaks (LR+ not estimable printed "Inf" in Turkish).
  skip_if_not(exists("diagnosticmetaClass") && exists("ihcheterogeneityClass"))
  literals <- character()
  collect <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name(".")) && length(e) >= 2 && is.character(e[[2]]))
      literals <<- c(literals, e[[2]])
    if (is.call(e) || is.expression(e) || is.pairlist(e) || is.list(e))
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) collect(e[[i]])
  }
  for (name in c("diagnosticmeta", "ihcheterogeneity"))
    collect(parse(oncopath_file("R", paste0(name, ".b.R")), keep.source = FALSE, encoding = "UTF-8"))
  messages <- lapply(unique(literals), function(s) list(paste0("\u00ab", s, "\u00bb")))
  names(messages) <- unique(literals)

  pkg <- diagnosticmetaOptions$new()$.__enclos_env__$private$.package
  cache <- get(".i18n", envir = asNamespace("jmvcore"))
  old_pkg <- if (pkg %in% names(cache)) cache[[pkg]] else NULL
  cache[[pkg]] <- new.env()
  cache[[pkg]][["xx"]] <- list(locale_data = list(messages = messages))
  old_lang <- Sys.getenv("LANGUAGE")
  Sys.setenv(LANGUAGE = "xx")
  on.exit({
    Sys.setenv(LANGUAGE = old_lang)
    if (is.null(old_pkg)) rm(list = pkg, envir = cache) else cache[[pkg]] <- old_pkg
  }, add = TRUE)
  # Options fix their language when constructed, so build the analyses after LANGUAGE is set.
  dm <- diagnosticmetaClass$new(options = diagnosticmetaOptions$new(), data = data.frame())
  ih <- ihcheterogeneityClass$new(options = ihcheterogeneityOptions$new(), data = data.frame())

  dp <- dm$.__enclos_env__$private
  outputs <- c(
    dp$.getInterpretationText(95, 96, 24, 0.05, c(91, 97), c(80, 99)),
    dp$.getInterpretationText(85, 92, 10.6, 0.16),
    dp$.getInterpretationText(75, 70, 3, 0.4),
    dp$.getInterpretationText(60, 100, Inf, 0.8),
    dp$.getInterpretationText(96, 50, 1.9, NaN)
  )
  ip <- ih$.__enclos_env__$private
  for (ref in c(TRUE, FALSE)) for (r in c(0.95, 0.82, 0.72, 0.4)) {
    m <- list(has_reference = ref, overall_corr = r, mean_cv = 12, bias_p = 0.2,
              n_cases = 30, n_biopsies = 4)
    outputs <- c(outputs, ip$.generateReportSentences(m, 20, 0.90))
  }
  expect_true(all(grepl("\u00ab", outputs)))                       # the pseudo-catalog was used
  expect_equal(grep("\u00ab[^\u00bb]*\u00ab", outputs, value = TRUE), character(0))   # nothing spliced
  expect_equal(grep("\\s\u00bb|\u00ab\\s", outputs, value = TRUE), character(0))     # no padding
  expect_equal(grep("\\b(Inf|NaN|NA)\\b", outputs, value = TRUE), character(0))
})

# library-audit 2026-09-16 OncoPath [LOW] DONE: NEWS.md has a section for the DESCRIPTION version;
#   _updateModules.R bumps Version: on every regeneration and never writes NEWS.md
test_that("NEWS.md has a section for the version DESCRIPTION declares", {
  skip_if_no_oncopath_source()
  package <- read.dcf(oncopath_file("DESCRIPTION"), fields = "Package")[[1]]
  skip_if(package != "OncoPath", "the umbrella keeps its own NEWS.md")
  version <- unname(read.dcf(oncopath_file("DESCRIPTION"), fields = "Version")[[1]])
  headings <- grep("^#{1,2} ", readLines(oncopath_file("NEWS.md"), warn = FALSE), value = TRUE)
  # the exact version: 1.0.82 must not satisfy 1.0.82.02, nor 1.0.8 satisfy 1.0.81
  pattern <- paste0("(^|[^0-9.])", gsub(".", "\\.", version, fixed = TRUE), "([^0-9.]|$)")
  expect_true(any(grepl(pattern, headings)),
              info = paste0("DESCRIPTION ", version, "; newest NEWS.md heading: ", headings[1]))
})

# library-audit 2026-09-16 OncoPath [INFO] DONE: no TODO comments; a .() string survives jmvcore's
#   translator intact when no catalog entry matches (" [..]" is read as a context and cut off)
test_that("translatable strings survive the translator and no TODO comments remain", {
  skip_if_no_oncopath_source()
  files <- file.path("R", c("diagnosticmeta.b.R", "ihcheterogeneity.b.R", "swimmerplot.b.R",
                            "waterfall.b.R", "swimmerplot-html.R", "utils-followup.R", "utils.R"))
  files <- files[file.exists(oncopath_file(files))]
  expect_gt(length(files), 3)

  todo <- character()
  literals <- character()
  collect <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name(".")) && length(e) >= 2 && is.character(e[[2]]))
      literals <<- c(literals, e[[2]])
    if (is.call(e) || is.expression(e) || is.pairlist(e) || is.list(e))
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) collect(e[[i]])
  }
  for (f in files) {
    src <- readLines(oncopath_file(f), warn = FALSE)
    hit <- grep("^\\s*#.*\\b(TODO|FIXME)\\b", src)
    todo <- c(todo, sprintf("%s:%d", f, hit))
    collect(parse(oncopath_file(f), keep.source = FALSE, encoding = "UTF-8"))
  }
  expect_equal(todo, character(0))

  # An Options object with no package has no catalog: the worst case for every language.
  options <- jmvcore::Options$new()
  literals <- unique(literals)
  cut_short <- literals[vapply(literals, function(s) !identical(options$translate(s), s), logical(1))]
  expect_gt(length(literals), 1000)
  expect_equal(unname(cut_short), character(0))
})

# library-audit 2026-09-16 OncoPath [INFO] REJECTED: native notice element (see .addNotice, guide section 13);
#   what HTML can do here holds: notice titles inherit, an opaque background sets its own text colour
test_that("HTML panels stay readable in jamovi's dark theme", {
  skip_if_no_oncopath_source()
  files <- file.path("R", c("diagnosticmeta.b.R", "ihcheterogeneity.b.R", "swimmerplot.b.R",
                            "waterfall.b.R", "swimmerplot-html.R", "utils-followup.R", "utils.R"))
  files <- files[file.exists(oncopath_file(files))]
  opaque <- "background(-color)?:\\s*(#[0-9A-Fa-f]{3,8}|(?!rgba|transparent|inherit|none)[a-z]+)\\b"
  own_colour <- "(^|[;'\" ])color:\\s*(?!inherit)[#a-z]"
  unreadable <- character()
  for (f in files) {
    src <- readLines(oncopath_file(f), warn = FALSE)
    for (i in grep(opaque, src, perl = TRUE)) {
      # In the dark theme inherited text is light: a pale opaque card without its own
      # text colour becomes unreadable. Translucent rgba() tints follow the theme.
      styles <- regmatches(src[i], gregexpr("style=(['\"])[^'\"]*\\1", src[i], perl = TRUE))[[1]]
      if (!length(styles)) styles <- src[i]
      if (any(grepl(opaque, styles, perl = TRUE) & !grepl(own_colour, styles, perl = TRUE)))
        unreadable <- c(unreadable, sprintf("%s:%d", f, i))
    }
    # Notice titles sit on a translucent tint: a fixed hue there fell to 2.7-2.9:1 on the
    # dark theme (#dc2626, #2563eb), so the title text must follow the pane.
    body <- paste(src, collapse = "\n")
    start <- regexpr("\\.renderNotices = function", body)
    if (start > 0) {
      renderer <- substr(body, start, start + 3000)
      if (grepl("<strong style='color: (?!inherit)", renderer, perl = TRUE) ||
          grepl("color = \"#[0-9A-Fa-f]{6}\"", renderer))
        unreadable <- c(unreadable, paste0(f, " .renderNotices title colour"))
    }
  }
  expect_equal(unreadable, character(0))
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class here): the waterfall median CI seeds its bootstrap
#   with withr::local_seed() from the "Random seed" option, so drawing the plot leaves the session's
#   random-number stream as it found it (every analysis shares one R process)
test_that("waterfall's bootstrap CI leaves the caller's RNG stream untouched", {
  skip_if_not(exists("waterfallClass"))
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  set.seed(3)
  d <- data.frame(PatientID = sprintf("PT%02d", 1:20), Response = round(stats::runif(20, -80, 40), 1))
  analysis <- waterfallClass$new(options = waterfallOptions$new(
    patientID = "PatientID", responseVar = "Response", showCI = TRUE), data = d)
  quiet(analysis$init())
  quiet(analysis$run())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(99)
  untouched <- stats::runif(3)
  set.seed(99)
  drawn <- quiet(analysis$.__enclos_env__$private$.waterfallplot(
    analysis$results$waterfallplot, ggplot2::theme_grey(), list()))
  expect_true(isTRUE(drawn))
  expect_identical(stats::runif(3), untouched)

  # the interval names the seed that drew it
  caption <- NULL
  suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                         tracer = bquote(assign("caption", plot$labels$caption, envir = .(environment())))))
  on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))), add = TRUE)
  quiet(analysis$.__enclos_env__$private$.waterfallplot(
    analysis$results$waterfallplot, ggplot2::theme_grey(), list()))
  expect_identical(caption, "Random seed: 123")
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class): tables whose row set is fixed by an option have
#   their rows before .run(). (swimmerplot's summaryData is not converted: after its five fixed metrics it
#   adds one pair of rows per response level present in the data.)
test_that("fixed-row OncoPath tables are scaffolded before .run()", {
  skip_if_not(exists("swimmerplotClass") && exists("diagnosticmetaClass"))
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  sw <- data.frame(id = sprintf("P%02d", 1:10), s = 0, e = seq(5, 50, 5), cens = rep(c(0, 1), 5))
  swim <- after_init("swimmerplot", sw, patientID = "id", startTime = "s", endTime = "e", censorVar = "cens",
                     personTimeAnalysis = TRUE)
  expect_identical(swim("advancedMetrics"), c("median_followup", "iqr", "person_time", "followup_density"))
  dm <- data.frame(study = paste("S", 1:10), tp = 20:29, fp = 5:14, fn = 3:12, tn = 40:49)
  meta <- after_init("diagnosticmeta", dm, study = "study", true_positives = "tp", false_positives = "fp",
                     false_negatives = "fn", true_negatives = "tn", bivariate_analysis = TRUE, publication_bias = TRUE)
  expect_identical(meta("publicationbias"), "deeks_test")
})
