# Design decisions are tested separately from data inspection and native results.
# These tests can run with the isolated compiled agreementintro component loaded.
guide_root <- normalizePath(testthat::test_path("..", ".."))
for (fn in c(".agreementIntroDefaults", ".agreementIntroCatalog", ".agreementIntroMissing",
  ".agreementIntroKey", ".agreementIntroProfile", ".agreementIntroRecommend"))
  assign(fn, getFromNamespace(fn, "ClinicoPath"))

guide_design <- function(...) utils::modifyList(list(purpose = "categorical",
  scale = "nominal", nRaters = 2L, panel = "same", missingRatings = "no",
  repeatedReadings = "no", analysisUnit = "independent"), list(...))
guide_decide <- function(...) .agreementIntroRecommend(guide_design(...))

# Stable IDs also appear in the method guide's Mermaid diagrams / decision index.
guide_routes <- list(
  CAT_TWO_NOMINAL = list(),
  CAT_TWO_ORDINAL = list(scale = "ordinal", categoryOrder = "confirmed", weights = "linear"),
  CAT_INCOMPLETE_ALPHA = list(nRaters = 4L, missingRatings = "yes", panel = "incomplete"),
  CAT_MULTI_ORDINAL = list(nRaters = 4L, scale = "ordinal", categoryOrder = "confirmed",
    weights = "quadratic"),
  CAT_PANEL_CONGER = list(nRaters = 3L),
  CAT_EXCHANGE_FLEISS = list(nRaters = 3L, panel = "different"),
  MEASUREMENT_PAIR = list(purpose = "measurement", scale = "continuous"),
  READER_BINARY = list(purpose = "readerAccuracy", scale = "binary", referenceType = "disease",
    positiveLabel = "yes", negativeLabel = "no", referencePositive = "disease",
    referenceNegative = "healthy"),
  READER_ORDINAL = list(purpose = "cutoff", scale = "ordinal", referenceType = "none",
    cutoff = "G2", cutoffPrespecified = "yes", categoryOrder = "confirmed"),
  READER_CONTINUOUS = list(purpose = "cutoff", scale = "continuous", referenceType = "none",
    cutoff = "10.5", cutoffPrespecified = "yes", cutoffDirection = "lt"),
  READER_MODALITY = list(purpose = "paired", comparisonGoal = "readerModality", scale = "binary",
    referenceType = "consensus", positiveLabel = "yes", negativeLabel = "no",
    referencePositive = "yes", referenceNegative = "no", repeatedReadings = "yes"),
  PAIRED_AGREEMENT = list(purpose = "paired", comparisonGoal = "agreement"),
  MODALITY_SPECIALIST = list(purpose = "paired", comparisonGoal = "specialized"),
  RANK_KENDALL = list(purpose = "ranking", scale = "rankings"),
  BIAS_MARGINAL = list(purpose = "bias"),
  FOCI_CONCORDANCE = list(purpose = "multifocal", analysisUnit = "clustered"),
  PLAN_PRECISION = list(purpose = "planning", planningGoal = "precision", nCategories = 3L),
  PLAN_POWER = list(purpose = "planning", planningGoal = "power", nCategories = 3L),
  PLAN_FIXED = list(purpose = "planning", planningGoal = "fixed", nCategories = 3L))
for (model in c("1", "2", "3")) for (unit in c("1", "K")) {
  guide_routes[[paste0("ICC_", model, "_", unit)]] <- list(purpose = "reliability",
    scale = "continuous", panel = if (model == "1") "different" else "same",
    raterSelection = if (model == "3") "fixed" else "random",
    iccDefinition = if (model == "3") "consistency" else "absolute",
    reportingUnit = if (unit == "1") "single" else "average")
}

test_that("every supported decision has a deterministic primary and at most two alternatives", {
  for (id in names(guide_routes)) {
    o <- utils::modifyList(guide_design(), guide_routes[[id]])
    d <- .agreementIntroRecommend(o)
    expect_identical(d$status, "recommended", info = id)
    expect_identical(d$path, id)
    expect_gte(length(d$recommendations), 1L)
    expect_lte(length(d$recommendations), 3L)
    expect_identical(.agreementIntroRecommend(o), d)
  }
  expect_identical(guide_decide(nRaters = 3)$recommendations[[1]]$settings$exct, TRUE)
  expect_identical(guide_decide(nRaters = 3, panel = "different")$
    recommendations[[1]]$settings$exct, FALSE)
  expect_identical(guide_decide(panel = "different")$path, "CAT_INCOMPLETE_ALPHA")
  expect_identical(guide_decide(scale = "ordinal", categoryOrder = "confirmed",
    weights = "unweighted", missingRatings = "yes")$recommendations[[1]]$settings$krippMethod,
    "nominal")
})

test_that("essential unknowns, invalid designs and target limits never receive configurations", {
  expect_identical(.agreementIntroRecommend(list())$messages, "need_purpose")
  missing <- list(scale = "unspecified", nRaters = 0L, panel = "unspecified",
    missingRatings = "unspecified", repeatedReadings = "unspecified", analysisUnit = "unspecified")
  for (name in names(missing)) {
    d <- .agreementIntroRecommend(utils::modifyList(guide_design(), missing[name]))
    expect_identical(d$status, "needs_information", info = name)
    expect_length(d$recommendations, 0L)
    expect_true(paste0("need_", name) %in% d$messages)
  }
  bad <- list(
    CAT_SCALE = list(scale = "continuous"),
    CAT_ORDER = list(scale = "ordinal", weights = "linear", categoryOrder = "uncertain"),
    CLUSTER_REVIEW = list(analysisUnit = "clustered"),
    REPEAT_REVIEW = list(repeatedReadings = "yes"),
    MEASURE_UNSUPPORTED = list(purpose = "measurement", scale = "continuous", nRaters = 3),
    ICC_SCALE = list(purpose = "reliability"),
    ICC_INCOMPLETE = list(purpose = "reliability", scale = "continuous", panel = "incomplete",
      raterSelection = "random", iccDefinition = "absolute", reportingUnit = "single"),
    ICC_ONEWAY_REVIEW = list(purpose = "reliability", scale = "continuous", panel = "different",
      raterSelection = "fixed", iccDefinition = "absolute", reportingUnit = "single"),
    ICC_LABEL_REVIEW = list(purpose = "reliability", scale = "continuous", panel = "same",
      raterSelection = "fixed", iccDefinition = "absolute", reportingUnit = "single"),
    READER_SCALE = list(purpose = "readerAccuracy"),
    PAIRED_SCALE = list(purpose = "paired", comparisonGoal = "agreement", scale = "continuous"),
    PAIRED_MULTI_REVIEW = list(purpose = "paired", comparisonGoal = "agreement", nRaters = 3),
    PAIRED_REVIEW = list(purpose = "paired", comparisonGoal = "agreement", missingRatings = "yes"),
    MODALITY_SCALE = list(purpose = "paired", comparisonGoal = "specialized", nRaters = 3),
    MODALITY_ORDER = list(purpose = "paired", comparisonGoal = "specialized", scale = "ordinal"),
    RANK_REVIEW = list(purpose = "ranking", scale = "rankings", panel = "different"),
    BIAS_REVIEW = list(purpose = "bias", nRaters = 3),
    FOCI_SCALE = list(purpose = "multifocal", scale = "continuous"),
    PLAN_UNSUPPORTED = list(purpose = "planning", planningGoal = "power", nCategories = 6))
  for (id in names(bad)) {
    d <- .agreementIntroRecommend(utils::modifyList(guide_design(), bad[[id]]))
    expect_identical(d$path, id)
    expect_false(d$status == "recommended")
    expect_length(d$recommendations, 0L)
  }
  icc <- utils::modifyList(guide_design(), guide_routes$ICC_2_1)
  for (name in c("raterSelection", "iccDefinition", "reportingUnit")) {
    o <- icc; o[[name]] <- "unspecified"
    expect_true(paste0("need_", name) %in% .agreementIntroRecommend(o)$messages)
  }
})

test_that("reader recommendations distinguish consensus from disease and preserve exact cutoffs", {
  o <- utils::modifyList(guide_design(), guide_routes$READER_BINARY)
  disease <- .agreementIntroRecommend(o)
  o$referenceType <- "consensus"
  consensus <- .agreementIntroRecommend(o)
  expect_identical(consensus$recommendations[[1]]$settings$referenceType, "consensus")
  expect_true("reference_not_truth" %in% consensus$notes)
  expect_false("reference_not_truth" %in% disease$notes)
  o$analysisUnit <- "clustered"
  expect_true(.agreementIntroRecommend(o)$recommendations[[1]]$settings$readerBootstrap)
  o$referenceType <- "none"
  expect_identical(.agreementIntroRecommend(o)$status, "needs_information")
  o <- utils::modifyList(guide_design(), guide_routes$READER_CONTINUOUS)
  for (direction in c("ge", "gt", "le", "lt")) {
    o$cutoffDirection <- direction
    expect_identical(.agreementIntroRecommend(o)$recommendations[[1]]$settings$positiveRule, direction)
  }
  for (cutoff in c("Inf", "NaN", "10; system('ignored')", "")) {
    o$cutoff <- cutoff
    expect_identical(.agreementIntroRecommend(o)$status, "needs_information")
  }
  o <- utils::modifyList(guide_design(), guide_routes$READER_MODALITY)
  o$referenceType <- "none"
  expect_identical(.agreementIntroRecommend(o)$status, "needs_information")
})

test_that("conditional information and unsupported reader branches remain explicit", {
  cases <- list(
    CAT_INPUT = guide_design(panel = "unspecified"),
    ICC_INPUT = guide_design(purpose = "reliability", scale = "continuous"),
    READER_INPUT = guide_design(purpose = "readerAccuracy", scale = "binary"),
    PAIRED_INPUT = guide_design(purpose = "paired", comparisonGoal = "agreement", panel = "unspecified"),
    RANK_INPUT = guide_design(purpose = "ranking", scale = "rankings", panel = "unspecified"),
    BIAS_INPUT = guide_design(purpose = "bias", panel = "unspecified"),
    PLAN_INPUT = guide_design(purpose = "planning"),
    READER_PANEL_REVIEW = utils::modifyList(utils::modifyList(guide_design(),
      guide_routes$READER_BINARY), list(panel = "different")),
    READER_REPEAT_REVIEW = utils::modifyList(utils::modifyList(guide_design(),
      guide_routes$READER_BINARY), list(repeatedReadings = "yes")),
    UNSUPPORTED = guide_design(purpose = "unrecognized"))
  for (id in names(cases)) {
    d <- .agreementIntroRecommend(cases[[id]])
    expect_identical(d$path, id)
    expect_length(d$recommendations, 0L)
    expect_gt(length(d$messages), 0L)
  }
})

guide_wide <- function() data.frame(case = c(1e15, 1e15 + 1, 1e15 + 2),
  patient = c(1e15, 1e15, 1e15 + 2),
  A = ordered(c("Low", "High", "Low"), levels = c("Low", "Mid", "High")),
  B = ordered(c("Low", "Mid", "High"), levels = c("Low", "Mid", "High")))
guide_long <- function(w) data.frame(case = rep(w$case, 2), patient = rep(w$patient, 2),
  reader = rep(c("A", "B"), each = nrow(w)), value = c(w$A, w$B))
guide_inspect <- function(...) utils::modifyList(guide_design(inspectData = TRUE,
  ratings = c("A", "B"), caseID = "case", patientID = "patient"), list(...))

test_that("wide and long inspection preserve IDs, full orders and clustered cases", {
  w <- guide_wide(); original <- serialize(w, NULL)
  a <- .agreementIntroProfile(w, guide_inspect())
  b <- .agreementIntroProfile(guide_long(w), guide_inspect(dataLayout = "long",
    readerID = "reader", rating = "value"))
  for (key in c("raters", "cases", "patients", "complete", "overlap", "incomplete", "clustered"))
    expect_identical(a$facts[[key]], b$facts[[key]], info = key)
  expect_identical(a$facts$cases, 3L)
  expect_identical(a$facts$patients, 2L)
  expect_true(a$facts$clustered)
  expect_identical(a$columns$A$declared, c("Low", "Mid", "High"))
  expect_identical(serialize(w, NULL), original)
  expect_length(unique(.agreementIntroKey(w, "case")), 3L)
  d <- .agreementIntroRecommend(guide_inspect(), a)
  expect_identical(d$status, "data_conflict")
  expect_true("conflict_cluster" %in% d$messages)
})

test_that("inspection exposes duplicates, absence, constants and conflicting scales without recoding", {
  w <- guide_wide(); o <- guide_inspect(patientID = NULL)
  long <- guide_long(w); lo <- guide_inspect(dataLayout = "long", readerID = "reader", rating = "value")
  expect_true("inspect_duplicates" %in% .agreementIntroProfile(rbind(long, long[1, ]), lo)$issues)
  expect_true(.agreementIntroProfile(long[-1, ], lo)$facts$incomplete)
  w$B <- ordered(w$B, levels = c("High", "Mid", "Low"))
  expect_true(.agreementIntroProfile(w, o)$facts$orderConflict)
  w <- guide_wide(); w$A[1] <- NA
  p <- .agreementIntroProfile(w, o)
  expect_equal(p$facts$missing, 1)
  expect_true("conflict_missing" %in% .agreementIntroRecommend(o, p)$messages)
  w$A <- 1; w$B <- c(1, 2, 1)
  p <- .agreementIntroProfile(w, o)
  expect_true(p$facts$numeric)
  expect_true("inspect_numeric" %in% p$notes)
  expect_true("inspect_constant" %in% p$notes)
  w$A <- c("", " ", NA)
  expect_true("inspect_no_overlap" %in% .agreementIntroProfile(w, o)$issues)
  expect_identical(.agreementIntroProfile(NULL, list())$active, FALSE)
  expect_true("inspect_empty" %in% .agreementIntroProfile(data.frame(), o)$issues)
  expect_true("inspect_roles" %in% .agreementIntroProfile(w, list(inspectData = TRUE))$issues)
  o$ratings <- c("A", "deleted")
  expect_true("inspect_columns" %in% .agreementIntroProfile(w, o)$issues)
})

test_that("inspection catches mislabeled binary classes and does not infer a reference", {
  w <- data.frame(case = 1:3, A = c("yes", "no", "yes"), B = c("no", "yes", "no"),
    reference = c("disease", "healthy", "disease"))
  o <- utils::modifyList(guide_inspect(patientID = NULL, reference = "reference"),
    guide_routes$READER_BINARY)
  d <- .agreementIntroRecommend(o, .agreementIntroProfile(w, o))
  expect_identical(d$status, "recommended")
  expect_identical(d$recommendations[[1]]$settings$vars, c("A", "B"))
  o$positiveLabel <- "Y"
  expect_true("conflict_reader_labels" %in% .agreementIntroRecommend(o,
    .agreementIntroProfile(w, o))$messages)
  o$positiveLabel <- "yes"; o$referenceType <- "unspecified"
  expect_identical(.agreementIntroRecommend(o, .agreementIntroProfile(w, o))$status,
    "needs_information")
  w$B <- c("0", "1", "0")
  expect_true("conflict_binary" %in% .agreementIntroRecommend(o,
    .agreementIntroProfile(w, o))$messages)
})

test_that("catalogue routes and every recommended enum exist in actual target schemas", {
  skip_if_not(file.exists(file.path(guide_root, "jamovi", "agreement.a.yaml")),
    "target source schemas are not included in the installed R package")
  catalog <- .agreementIntroCatalog()
  schemas <- lapply(names(catalog), function(n)
    yaml::yaml.load(paste(readLines(file.path(guide_root, "jamovi", paste0(n, ".a.yaml")),
      warn = FALSE), collapse = "\n")))
  names(schemas) <- names(catalog)
  for (n in names(catalog)) {
    a <- schemas[[n]]; c <- catalog[[n]]
    expect_identical(c$title, a$title, info = n)
    expect_identical(c$group, a$menuGroup, info = n)
    expect_identical(c$subgroup, a$menuSubgroup, info = n)
    expect_identical(c$version, a$version, info = n)
  }
  for (id in names(guide_routes)) {
    d <- .agreementIntroRecommend(utils::modifyList(guide_design(), guide_routes[[id]]))
    for (r in d$recommendations) {
      opts <- schemas[[r$target]]$options
      names(opts) <- vapply(opts, `[[`, "", "name")
      expect_true(all(names(r$settings) %in% names(opts)), info = id)
      for (key in names(r$settings)) {
        opt <- opts[[key]]; value <- r$settings[[key]]
        if (opt$type == "List") {
          values <- vapply(opt$options, function(x) if (is.list(x)) as.character(x$name) else
            as.character(x), "")
          expect_true(value %in% values, info = paste(id, key, value))
        }
        if (opt$type == "Bool") expect_type(value, "logical")
      }
    }
  }
})

test_that("native empty-data guide, all routes and optional explanations render", {
  ns <- asNamespace("ClinicoPath")
  for (o in c(list(list()), lapply(guide_routes, function(x) utils::modifyList(guide_design(), x)))) {
    a <- ns$agreementintroClass$new(options = do.call(ns$agreementintroOptions$new,
      c(o, list(showExamples = TRUE, showPitfalls = TRUE, showGlossary = TRUE,
        showCatalogue = TRUE))), data = data.frame())
    expect_no_error(a$run())
    expect_equal(nrow(a$results$catalogue$asDF), 10L)
  }
})

test_that("answer, selection, data and visibility changes equal a fresh native analysis", {
  ns <- asNamespace("ClinicoPath")
  create <- function(o, data) {
    a <- ns$agreementintroClass$new(options = do.call(ns$agreementintroOptions$new, o), data = data)
    a$run(); a
  }
  o <- guide_design(); live <- create(o, data.frame())
  snapshots <- function(a) list(tables = lapply(c("recommendations", "settings", "dataSummary",
    "columnSummary", "catalogue"), function(n) a$results[[n]]$asDF),
    html = lapply(c("status", "preparation", "inspectionNotes", "examples", "overview"),
      function(n) a$results[[n]]$content),
    visible = vapply(c("recommendations", "settings", "dataSummary", "columnSummary"),
      function(n) a$results[[n]]$visible, logical(1)))
  states <- list(list(options = list(nRaters = 3L), data = data.frame()),
    list(options = list(purpose = "unspecified"), data = data.frame()),
    list(options = guide_inspect(patientID = NULL, scale = "ordinal", categoryOrder = "confirmed",
      weights = "quadratic"), data = guide_wide()),
    list(options = list(), data = transform(guide_wide(), A = NA)),
    list(options = list(inspectData = FALSE, showRecommendation = FALSE, showOverview = FALSE),
      data = guide_wide()),
    list(options = list(inspectData = TRUE, showRecommendation = TRUE, showOverview = TRUE,
      ratings = c("A", "B")), data = guide_wide()[2:3, ]))
  for (state in states) {
    o <- utils::modifyList(o, state$options, keep.null = TRUE)
    for (n in names(state$options)) {
      option <- live$options$option(n)
      option$value <- state$options[[n]]
    }
    live$.__enclos_env__$private$.data <- state$data
    expect_no_error(live$run())
    fresh <- create(o, state$data)
    expect_equal(snapshots(live), snapshots(fresh))
    if (!nrow(live$results$recommendations$asDF) && !nrow(live$results$dataSummary$asDF))
      expect_identical(live$results$preparation$content, "<div></div>")
  }
})

test_that("guide computation leaves the RNG untouched and escapes HTML", {
  ns <- asNamespace("ClinicoPath")
  withr::local_seed(422)
  before <- .Random.seed
  a <- ns$agreementintroClass$new(options = ns$agreementintroOptions$new(), data = data.frame())
  a$run()
  expect_identical(.Random.seed, before)
  html <- a$.__enclos_env__$private$.panel("<script>alert('x')</script>&")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("unsafe target ID conversions are withheld while the inspector preserves identity", {
  w <- guide_wide()
  w$A <- c("yes", "no", "yes"); w$B <- c("no", "yes", "yes")
  w$reference <- c("disease", "healthy", "disease")
  for (layout in c("wide", "long")) {
    o <- utils::modifyList(guide_inspect(dataLayout = layout, readerID = "reader", rating = "value",
      reference = "reference", analysisUnit = "clustered"), guide_routes$READER_BINARY)
    data <- if (layout == "wide") w else {
      x <- guide_long(w); x$reference <- rep(w$reference, 2); x
    }
    p <- .agreementIntroProfile(data, o)
    expect_equal(p$facts$cases, 3L)
    expect_true(p$facts$idFormatCollision)
    expect_identical(.agreementIntroRecommend(o, p)$path, "READER_ID_REVIEW")
  }
  o <- guide_inspect(purpose = "multifocal", analysisUnit = "clustered")
  expect_identical(.agreementIntroRecommend(o, .agreementIntroProfile(w, o))$path, "FOCI_ID_REVIEW")
})

test_that("patient-specific focus labels do not invent missing ratings", {
  d <- data.frame(case = rep(c("p1", "p2"), c(2, 3)), focus = paste0("f", 1:5),
    value = c("yes", "yes", "no", "yes", "no"))
  o <- guide_inspect(purpose = "multifocal", analysisUnit = "clustered", dataLayout = "long",
    patientID = NULL, readerID = "focus", rating = "value")
  p <- .agreementIntroProfile(d, o)
  expect_false(p$facts$incomplete)
  expect_equal(p$facts$raters, 3L)
  expect_identical(.agreementIntroRecommend(o, p)$path, "FOCI_CONCORDANCE")
})

test_that("exchangeable long designs count rater slots separately from reader identities", {
  d <- data.frame(case = rep(c("c1", "c2", "c3"), each = 3), reader = paste0("r", 1:9),
    value = rep(c("yes", "no", "yes"), 3))
  o <- guide_inspect(dataLayout = "long", readerID = "reader", rating = "value",
    patientID = NULL, panel = "different", nRaters = 3L)
  p <- .agreementIntroProfile(d, o)
  expect_equal(p$facts$raters, 3L)
  expect_equal(p$facts$distinctRaters, 9L)
  expect_false(p$facts$incomplete)
  expect_identical(.agreementIntroRecommend(o, p)$path, "CAT_EXCHANGE_FLEISS")
})

test_that("all decision path identifiers have a documentation entry", {
  skip_if_not(file.exists(file.path(guide_root, "R", "agreementintro-rules.R")),
    "source decision rules are not included in the installed R package")
  doc <- paste(readLines(file.path(guide_root, "vignettes", "meddecide-agreement-method-guide.md")),
    collapse = "\n")
  rules <- paste(readLines(file.path(guide_root, "R", "agreementintro-rules.R")), collapse = "\n")
  ids <- unique(c(names(guide_routes), gsub('"', "", regmatches(rules,
    gregexpr('"[A-Z][A-Z0-9_]+"', rules))[[1]])))
  ids <- setdiff(ids, c("ICC_", "READER_", "PLAN_"))
  for (id in ids) expect_true(grepl(id, doc, fixed = TRUE), info = id)
})

# Use the real engine transport functions when running outside the desktop.
test_that("full native responses serialize and saved analyses reproduce after reopening", {
  skip_if_not_installed("RProtoBuf")
  if (is.null(getFromNamespace("RProtoBuf_new", "jmvcore"))) {
    testthat::local_mocked_bindings(RProtoBuf_new = RProtoBuf::new,
      RProtoBuf_serialize = RProtoBuf::serialize, RProtoBuf_read = RProtoBuf::read,
      .package = "jmvcore")
  }
  getFromNamespace("initProtoBuf", "jmvcore")()
  encode <- function(value) {
    item <- RProtoBuf::P("jamovi.coms.AnalysisOption")$new()
    if (is.null(value)) item$o <- 2L else if (is.list(value) || length(value) != 1L) {
      nested <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
      nested$hasNames <- !is.null(names(value))
      if (!is.null(names(value))) nested$names <- names(value)
      nested$options <- lapply(value, encode); item$c <- nested
    } else if (is.logical(value)) item$o <- as.integer(value) else if (is.integer(value))
      item$i <- value else if (is.numeric(value)) item$d <- value else item$s <- value
    item
  }
  create <- function() {
    ns <- asNamespace("ClinicoPath")
    opts <- do.call(ns$agreementintroOptions$new, c(guide_design(), list(showCatalogue = TRUE)))
    keys <- names(opts$.__enclos_env__$private$.options)
    pb <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
    pb$hasNames <- TRUE; pb$names <- keys
    pb$options <- lapply(keys, function(n) encode(opts$option(n)$value))
    opts$fromProtoBuf(pb)
    ns$agreementintroClass$new(options = opts, data = data.frame(),
      datasetId = "1", analysisId = 1L, revision = 1L)
  }
  a <- create(); a$run()
  expect_gt(length(RProtoBuf::serialize(a$asProtoBuf(final = TRUE), NULL)), 100L)
  path <- tempfile(); withr::defer(unlink(path))
  a$.setStatePathSource(function() path); a$.save()
  b <- create(); b$.setStatePathSource(function() path)
  b$init(); b$postInit(); b$.load()
  # Dynamic rows are rebuilt by .run() after the native .load() lifecycle.
  # This guide has no fitted model or random computation to cache.
  expect_equal(b$results$status$content, a$results$status$content)
  b$run()
  for (n in c("recommendations", "settings", "catalogue"))
    expect_equal(b$results[[n]]$asDF, a$results[[n]]$asDF, info = n)
  for (n in c("status", "preparation", "catalogueNotes"))
    expect_equal(b$results[[n]]$content, a$results[[n]]$content, info = n)
})
