# Regression coverage for the 2026-08-31 full audit.
lassocox_audit_data <- function() {
  withr::local_seed(818)
  d <- data.frame(x = rnorm(180), z = rnorm(180, sd = 10),
    grade = ordered(rep(c("G1", "G2", "G3"), 60)), noise = rnorm(180))
  event <- rexp(180, exp(1.2 * d$x - .05 * d$z + .7 * (d$grade == "G3")))
  censor <- rexp(180, .35)
  d$time <- pmin(event, censor)
  d$status <- as.integer(event <= censor)
  d
}

lassocox_audit_analysis <- function(data = lassocox_audit_data(), ...) {
  skip_if_not_installed("glmnet")
  args <- modifyList(list(elapsedtime = "time", outcome = "status", outcomeLevel = "1",
    censorLevel = "0", explanatory = c("x", "z", "grade", "noise"), lambda = "lambda.min",
    showEncoding = TRUE, showReproducibility = TRUE, showModelComparison = TRUE,
    showSummary = TRUE, showRCode = TRUE, showExplanations = TRUE, path_plot = TRUE), list(...))
  opts <- do.call(getFromNamespace("lassocoxOptions", "ClinicoPath")$new, args)
  getFromNamespace("lassocoxClass", "ClinicoPath")$new(options = opts, data = data,
    datasetId = "1", analysisId = 1L)
}

test_that("Cox comparison is invariant to predictor names that resemble responses", {
  d <- lassocox_audit_data()
  original <- lassocox_audit_analysis(d, explanatory = c("x", "z", "grade"))
  original$run()
  reference <- as.data.frame(original$results$modelComparison)
  for (name in c("y", ".time", ".status", "marker ` special")) {
    d[[name]] <- d$x
    renamed <- lassocox_audit_analysis(d, explanatory = c(name, "z", "grade"))
    renamed$run()
    comparison <- as.data.frame(renamed$results$modelComparison)
    expect_true(all(is.finite(comparison$cindex)))
    expect_equal(comparison, reference, tolerance = 1e-12)
    expect_false("failed" %in% names(renamed$results$modelComparison$notes))
  }
})

test_that("provenance includes constants created by complete-case filtering", {
  d <- lassocox_audit_data()
  d$constant <- 1
  d$rare <- as.numeric(seq_len(nrow(d)) <= 3)
  d$x[1:3] <- NA_real_
  a <- lassocox_audit_analysis(d, explanatory = c("x", "z", "constant", "rare"))
  a$run()
  expect_match(a$results$todo$content, "Removed constant explanatory variables: constant")
  expect_match(a$results$todo$content, "Removed design columns constant in complete cases: rare")
  expect_match(a$results$suitabilityReport$content, "3 rows excluded; 2 constant")
  expect_match(a$results$suitabilityReport$content, "candidate predictors: constant")
  expect_match(a$results$suitabilityReport$content, "complete cases: rare")
  provenance <- as.data.frame(a$results$reproducibility)
  expect_equal(provenance$value[provenance$item == "Removed constant design columns"], "rare")
  expect_equal(as.data.frame(a$results$encoding)$variable, c("x", "z"))
})

test_that("reruns clear notes and recover fixed rows and plot visibility", {
  d <- lassocox_audit_data()
  a <- lassocox_audit_analysis(d, elapsedtime = NULL)
  pr <- a$.__enclos_env__$private
  pr$.init()
  a$results$.update()
  expect_match(a$results$todo$content, "Welcome")
  expect_false(a$results$cv_plot$visible)
  op <- a$options$option("elapsedtime")
  op$value <- "time"
  values <- a$options$values()
  values$elapsedtime <- "time"
  # Public initialization reevaluates declarative visibility on a fresh lifecycle.
  a$optionsChangedHandler("elapsedtime")
  a$run()
  expect_equal(a$results$modelSummary$rowCount, 14)
  expect_equal(a$results$performance$rowCount, 1)
  # Check expression independently: a reused result's update cache is framework-managed.
  expect_true(a$options$eval("(cv_plot && length(elapsedtime) > 0 && length(outcome) > 0 && length(explanatory) > 0)"))
  expect_false(is.null(a$results$cv_plot$state))
  a$results$modelComparison$setNote("failed", "stale failure")
  a$results$coefficients$setNote("empty", "stale empty model")
  pr$.init()
  pr$.run()
  expect_false("failed" %in% names(a$results$modelComparison$notes))
  expect_false("empty" %in% names(a$results$coefficients$notes))
  pr$.clearAnalysisOutputs()
  expect_equal(a$results$modelSummary$rowCount, 0)
  expect_length(a$results$coefficients$notes, 0)
  expect_identical(a$results$rCode$content, "")
  expect_null(a$results$path_plot$state)
})

test_that("plots preserve a usable panel with thirty paths and correct effect labels", {
  a <- lassocox_audit_analysis()
  a$run()
  state <- a$results$path_plot$state
  one <- state$paths[state$paths$column == state$paths$column[1], ]
  state$paths <- do.call(rbind, lapply(seq_len(30), function(i) {
    row <- one
    row$column <- paste0("long clinical predictor number ", i)
    row$coefficient <- row$coefficient * i / 30
    row
  }))
  state$shown <- 30L
  state$total <- 501L
  a$results$path_plot$setState(state)
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 750, height = 500, res = 96)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$results$path_plot$.render())
  grid::grid.force()
  viewports <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
  panel <- viewports[grepl("^panel[.][0-9]", viewports)][1]
  expect_false(is.na(panel))
  grid::seekViewport(panel)
  height <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE) * 96
  expect_gt(height, 150)
  grid::upViewport(0)
  expect_true(a$results$coef_plot$.render())
  plot <- ggplot2::last_plot()
  labels <- plot$scales$get_scales("fill")$get_labels()
  expect_setequal(unname(labels), c("Lower fitted hazard", "Higher fitted hazard"))
  expect_match(plot$labels$subtitle, "original predictor scale")
})

test_that("required dependencies stop analysis and optional survival rendering falls back", {
  make_missing <- function(missing) {
    prototype <- lassocox_audit_analysis()
    subclass <- R6::R6Class(inherit = getFromNamespace("lassocoxClass", "ClinicoPath"),
      private = list(.hasPackage = function(package) package != missing))
    subclass$new(options = prototype$options, data = prototype$data)
  }
  a <- make_missing("glmnet")
  a$run()
  expect_equal(a$results$modelSummary$rowCount, 0)
  expect_match(a$results$todo$content, "Required packages are unavailable: glmnet")
  expect_null(a$results$cv_plot$state)

  a <- make_missing("survminer")
  a$run()
  expect_gt(a$results$modelSummary$rowCount, 0)
  expect_match(a$results$todo$content, "base-R fallback without a risk table")
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 600, height = 400)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$results$survival_plot$.render())
})

# A desktop request supplies protobuf options; R-wrapper Options$new() does not.
# Encode that request so .load() does not legitimately treat every option as changed.
lassocox_audit_options_pb <- function(options) {
  getFromNamespace("initProtoBuf", "jmvcore")()
  encode <- function(value) {
    item <- RProtoBuf::P("jamovi.coms.AnalysisOption")$new()
    if (is.null(value)) item$o <- 2L
    else if (is.list(value) || length(value) != 1L) {
      nested <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
      nested$hasNames <- !is.null(names(value))
      if (!is.null(names(value))) nested$names <- names(value)
      nested$options <- lapply(value, encode)
      item$c <- nested
    } else if (is.logical(value)) item$o <- as.integer(value)
    else if (is.integer(value)) item$i <- value
    else if (is.numeric(value)) item$d <- value
    else item$s <- value
    item
  }
  option_names <- names(options$.__enclos_env__$private$.options)
  pb <- RProtoBuf::P("jamovi.coms.AnalysisOptions")$new()
  pb$hasNames <- TRUE
  pb$names <- option_names
  pb$options <- lapply(option_names, function(name) {
    value <- if (name == "riskScore") list(value = TRUE) else options$option(name)$value
    encode(value)
  })
  options$fromProtoBuf(pb)
  stopifnot(length(options$compProtoBuf(pb)) == 0)
  invisible(pb)
}

test_that("LASSO results reload and active scores retain alignment on the protobuf wire", {
  skip_if_not_installed("RProtoBuf")
  d <- lassocox_audit_data()
  d$x[2] <- NA_real_
  make <- function(data = d, ...) {
    a <- lassocox_audit_analysis(data, ...)
    op <- a$options$option("riskScore")
    op$value <- list(value = TRUE)
    values <- a$options$values()
    values$riskScore <- TRUE
    lassocox_audit_options_pb(a$options)
    a
  }
  wire_scores <- function(analysis) {
    bytes <- RProtoBuf::serialize(analysis$asProtoBuf(final = TRUE), NULL)
    expect_type(bytes, "raw")
    response <- RProtoBuf::P("jamovi.coms.AnalysisResponse")$read(bytes)
    expect_length(analysis$options$compProtoBuf(response$options), 0)
    output <- Filter(function(item) item$name == "riskScore", response$results$group$elements)
    expect_length(output, 1)
    columns <- output[[1]]$outputs$outputs
    expect_length(columns, 1)
    expect_true(columns[[1]]$incData)
    columns[[1]]$d
  }
  file <- tempfile(fileext = ".pb")
  on.exit(unlink(file), add = TRUE)
  original <- make()
  original$.setStatePathSource(function() file)
  original$run()
  reference_scores <- original$results$riskScore$.__enclos_env__$private$.values[[1]]
  expect_length(reference_scores, nrow(d))
  expect_identical(which(is.na(reference_scores)), 2L)
  expect_true(all(is.finite(reference_scores[-2])))
  transmitted_scores <- wire_scores(original)
  expect_identical(is.na(transmitted_scores), is.na(reference_scores))
  expect_equal(transmitted_scores[-2], reference_scores[-2], tolerance = 1e-12)
  original$.save()
  expect_true(file.exists(file) && file.info(file)$size > 0)
  restored <- make()
  restored$.setStatePathSource(function() file)
  restored$init()
  restored$postInit()
  restored$.load()
  expect_equal(as.data.frame(restored$results$modelSummary), as.data.frame(original$results$modelSummary))
  expect_equal(restored$results$path_plot$state, original$results$path_plot$state)
  expect_true("apparent" %in% names(restored$results$performance$notes))
  for (name in c("cv_plot", "coef_plot", "survival_plot", "path_plot")) expect_true(restored$results[[name]]$visible)
  # Saved analysis state restores output metadata; dataset columns are transmitted
  # separately in final responses, rather than copied into the analysis state file.
  expect_true(restored$results$riskScore$enabled)
  expect_length(restored$options$compProtoBuf(original$options$asProtoBuf()), 0)
  expect_type(RProtoBuf::serialize(restored$asProtoBuf(final = TRUE), NULL), "raw")
  # A newly invalid dataset cannot retain estimates or scores from the saved model.
  bad <- d
  bad$time[1] <- 0
  failed <- make(bad)
  failed$.setStatePathSource(function() file)
  failed$init()
  failed$postInit()
  failed$.load(vChanges = "time")
  failed$run()
  expect_equal(failed$results$modelSummary$rowCount, 0)
  expect_match(failed$results$todo$content, "zero values")
  expect_null(failed$results$path_plot$state)
  expect_true(all(is.na(failed$results$riskScore$.__enclos_env__$private$.values[[1]])))
  invalid_scores <- wire_scores(failed)
  expect_length(invalid_scores, nrow(d))
  expect_true(all(is.na(invalid_scores)))
  # Recovered data repopulate the model while respecting a disabled plot option.
  recovered <- make(coef_plot = FALSE)
  recovered$.setStatePathSource(function() file)
  recovered$init()
  recovered$postInit()
  recovered$.load(vChanges = "time")
  recovered$run()
  expect_equal(recovered$results$modelSummary$rowCount, 14)
  expect_false(recovered$results$coef_plot$visible)
  expect_true(recovered$results$path_plot$visible)
  recovered_scores <- wire_scores(recovered)
  expect_identical(is.na(recovered_scores), is.na(reference_scores))
  expect_equal(recovered_scores[-2], reference_scores[-2], tolerance = 1e-12)
})

test_that("Turkish translations reach explanations, tables, and plot labels", {
  locale <- system.file("i18n/tr.json", package = "ClinicoPath")
  skip_if(!nzchar(locale), "Compiled Turkish catalog is required")
  a <- lassocox_audit_analysis(showMethodologyNotes = TRUE, includeClinicalGuidance = TRUE)
  options <- a$options
  options$.__enclos_env__$private$.translator <-
    jmvcore:::Translator$new(jsonlite::read_json(locale))
  a$run()
  expect_match(a$results$suitabilityReport$content, "Veri kalitesi", fixed = TRUE)
  expect_false(grepl("Understanding LASSO", a$results$lassoExplanation$content, fixed = TRUE))
  expect_match(a$results$methodologyNotes$content, "Breslow", fixed = TRUE)
  expect_match(a$results$clinicalGuidance$content, "doğrulama", fixed = TRUE)
  expect_equal(a$results$modelSummary$rowCount, 14)
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 600, height = 400)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$results$coef_plot$.render())
  labels <- ggplot2::last_plot()$scales$get_scales("fill")$get_labels()
  expect_setequal(unname(labels), c("Daha düşük tahmini hazard", "Daha yüksek tahmini hazard"))
})

test_that("few events receive a prominent stability notice without concealing the fit", {
  d <- lassocox_audit_data()
  d$status <- as.integer(seq_len(nrow(d)) <= 5)
  a <- lassocox_audit_analysis(d, showModelComparison = FALSE)
  a$run()
  expect_equal(a$results$modelSummary$rowCount, 14)
  expect_match(a$results$todo$content,
    "<div class='alert alert-danger'><h4>Model stability warnings", fixed = TRUE)
  expect_match(a$results$todo$content, "5 events", fixed = TRUE)
})

test_that("survival curve and risk table retain distinct readable panels", {
  skip_if_not_installed("survminer")
  a <- lassocox_audit_analysis()
  a$run()
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  grDevices::png(file, width = 600, height = 400, res = 96)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$results$survival_plot$.render())
  grid::grid.force()
  paths <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
  panels <- which(grepl("^panel[.][0-9]", paths$name))
  expect_length(panels, 2L)
  heights <- vapply(panels, function(i) {
    grid::upViewport(0)
    # Both plots use the same panel name, so include each panel's parent path.
    parts <- strsplit(paste(paths$vpPath[i], paths$name[i], sep = "::"), "::", fixed = TRUE)[[1]]
    parts <- parts[parts != "ROOT"]
    grid::downViewport(do.call(grid::vpPath, as.list(parts)))
    grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE) * 96
  }, numeric(1))
  expect_gt(min(heights), 40)
})

test_that("non-converged unpenalized comparisons are not reported numerically", {
  withr::local_seed(901)
  n <- 70
  p <- 60
  x <- matrix(rnorm(n * p), nrow = n,
    dimnames = list(NULL, paste0("x", seq_len(p))))
  linear_predictor <- .8 * x[, 1] - .6 * x[, 2]
  event_time <- rexp(n, exp(linear_predictor))
  censor_time <- rexp(n, .6)
  d <- data.frame(time = pmin(event_time, censor_time),
    status = as.integer(event_time <= censor_time), x, check.names = FALSE)
  a <- lassocox_audit_analysis(d,
    explanatory = colnames(x), showModelComparison = TRUE,
    suitabilityCheck = FALSE, cv_plot = FALSE, coef_plot = FALSE,
    survival_plot = FALSE, path_plot = FALSE, showSummary = FALSE,
    showRCode = FALSE, showEncoding = FALSE, showReproducibility = FALSE)
  a$run()
  comparison <- as.data.frame(a$results$modelComparison)
  expect_true(all(is.na(comparison[2, c("cindex", "aic", "log_likelihood")])))
  expect_true("failed" %in% names(a$results$modelComparison$notes))
  expect_match(a$results$modelComparison$notes[["failed"]]$note,
    "did not converge cleanly", fixed = TRUE)
  expect_false(grepl("Ran out of iterations", a$results$todo$content, fixed = TRUE))
})

test_that("an invalid selected Cox refit cannot produce a PH verdict", {
  prototype <- lassocox_audit_analysis(lambda = "lambda.min", showModelComparison = FALSE)
  subclass <- R6::R6Class(
    inherit = getFromNamespace("lassocoxClass", "ClinicoPath"),
    private = list(.coxRefit = function(data, columns) stop("invalid refit")))
  a <- subclass$new(options = prototype$options, data = prototype$data)
  a$run()
  expect_match(a$results$suitabilityReport$content, "Could not assess", fixed = TRUE)
  expect_false(grepl("Selected-refit test p", a$results$suitabilityReport$content, fixed = TRUE))
})

test_that("correlation details escape special predictor names exactly once", {
  withr::local_seed(902)
  d <- lassocox_audit_data()
  d[["marker & {x}"]] <- d$x
  d[["copy <z>"]] <- d$x + rnorm(nrow(d), sd = .01)
  a <- lassocox_audit_analysis(d,
    explanatory = c("marker & {x}", "copy <z>", "noise"),
    showModelComparison = FALSE)
  a$run()
  html <- a$results$suitabilityReport$content
  expect_false(grepl("&amp;amp;", html, fixed = TRUE))
  expect_match(html, "marker &amp; (x)", fixed = TRUE)
  expect_match(html, "copy &lt;z&gt;", fixed = TRUE)
  expect_match(html, "Top correlated encoded-column pairs:", fixed = TRUE)
  expect_false(grepl("Top pairs:", html, fixed = TRUE))
})

test_that("one candidate is judged by its encoded design width", {
  d <- lassocox_audit_data()
  numeric_only <- lassocox_audit_analysis(d, explanatory = "x")
  numeric_only$run()
  expect_equal(numeric_only$results$modelSummary$rowCount, 0)
  expect_match(numeric_only$results$todo$content,
    "At least two non-constant encoded predictor columns", fixed = TRUE)
  expect_false(grepl("Only one non-constant explanatory variable remains",
    numeric_only$results$todo$content, fixed = TRUE))

  factor_only <- lassocox_audit_analysis(d, explanatory = "grade")
  factor_only$run()
  expect_equal(factor_only$results$modelSummary$rowCount, 14)
  expect_equal(nrow(as.data.frame(factor_only$results$encoding)), 2L)
  expect_false(grepl("Only one non-constant explanatory variable remains",
    factor_only$results$todo$content, fixed = TRUE))
})

test_that("sample-size adequacy is displayed as undetermined", {
  a <- lassocox_audit_analysis(showModelComparison = FALSE)
  a$run()
  html <- a$results$suitabilityReport$content
  expect_match(html,
    "color: #6c757d; font-size: 18px;'>●</span></td><td style='padding: 6px;'><strong>Sample Size",
    fixed = TRUE)
  expect_match(html, "Gray = adequacy not determined by this diagnostic", fixed = TRUE)
})
