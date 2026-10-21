# ═══════════════════════════════════════════════════════════
# Regression tests: decisioncurve independent review (2026-10-13)
# ═══════════════════════════════════════════════════════════
#
# One block per finding in development-ideas/decisioncurve-independent-review-2026-10-13.md
# (IR-decisioncurve-NN). Oracles: pROC for the DeLong AUC and its variance, the rendered ggplot
# for plot geometry, and the displayed text itself for disclosures. Every wrapper call passes
# its Level arguments explicitly and goes through do.call(), because the jamovi wrapper reads a
# bare symbol passed to a variable option as a column name.

library(testthat)

ir_fixture <- function(n = 1000, seed = 101) {
  set.seed(seed)
  r <- runif(n, 0, 0.6)
  y <- rbinom(n, 1, r) == 1
  data.frame(y = factor(ifelse(y, "event", "none"), levels = c("none", "event")),
             good = round(pmin(pmax(r + rnorm(n, 0, 0.03), 0), 1), 3),
             weak = pmin(pmax(0.3 + 0.3 * (r - 0.3) + rnorm(n, 0, 0.1), 0), 1))
}

run_ir <- function(d, ...) {
  args <- list(data = d, outcome = "y", outcomePositive = "event", models = "good",
               decisionRuleVar = NULL, decisionRulePositive = NULL)
  extra <- list(...)
  for (nm in names(extra)) args[nm] <- list(extra[[nm]])
  suppressWarnings(do.call(ClinicoPath::decisioncurve, args))
}

ir_notices <- function(res) {
  h <- res$notices$content
  if (is.null(h) || !nzchar(h)) return(data.frame(type = character(), title = character()))
  chunks <- strsplit(h, "<div style='background-color: ", fixed = TRUE)[[1]][-1]
  map <- c("rgba(220, 38, 38, 0.10)" = "ERROR", "rgba(234, 88, 12, 0.10)" = "STRONG_WARNING",
           "rgba(202, 138, 4, 0.12)" = "WARNING", "rgba(37, 99, 235, 0.08)" = "INFO")
  data.frame(type = unname(map[sub(";.*$", "", chunks)]),
             title = trimws(sub("^.*?<strong> ?(.*?)</strong>.*$", "\\1", chunks, perl = TRUE)),
             stringsAsFactors = FALSE)
}

ir_has_notice <- function(res, title, type) {
  nd <- ir_notices(res)
  any(nd$title == title & nd$type == type)
}

ir_notes <- function(tbl) paste(vapply(tbl$notes, function(z) z$note, ""), collapse = " ")

ir_render <- function(res, image) {
  ggplot2::set_last_plot(NULL)
  f <- tempfile(fileext = ".png")
  res[[image]]$saveAs(f)
  ggplot2::last_plot()
}

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-01: the sample prevalence is the population prevalence; say so
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-01 the analysis summary states the sample-prevalence assumption", {
  res <- run_ir(ir_fixture(), showInterpretation = TRUE)
  notes <- res$procedureNotes$content
  expect_match(notes, "sample prevalence", fixed = TRUE)
  expect_match(notes, "case-control", fixed = TRUE)
  expect_match(res$summaryText$content, "case-control", fixed = TRUE)
})

test_that("IR-decisioncurve-01 an exact 1:1 event/non-event split warns about case-control sampling", {
  # Title renamed 2026-10-21 (DC-21-02): the notice now covers exact 1:1 to 1:4 splits, not 1:1 only.
  set.seed(3)
  bal <- data.frame(y = factor(rep(c("event", "none"), each = 150), levels = c("none", "event")),
                    good = c(runif(150, 0.2, 0.9), runif(150, 0.05, 0.6)))
  expect_true(ir_has_notice(run_ir(bal), "Outcome Split Matches a Case-Control Ratio", "WARNING"))
  unbal <- bal[-1, ]                                     # 149 events, 150 non-events
  expect_false(ir_has_notice(run_ir(unbal), "Outcome Split Matches a Case-Control Ratio", "WARNING"))
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-03: predictions inversely related to the selected positive level
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-03 DeLong AUC and SE equal pROC, for continuous and tied 0/1 predictions", {
  skip_if_not_installed("pROC")
  d <- ir_fixture(n = 400)
  ev <- d$y == "event"
  opts <- ClinicoPath:::decisioncurveOptions$new(outcome = "y", outcomePositive = "event",
                                                 models = "good", decisionRulePositive = NULL)
  a <- ClinicoPath:::decisioncurveClass$new(options = opts, data = d)
  auc_fn <- a$.__enclos_env__$private$.aucDeLong
  for (pred in list(d$good, as.numeric(d$good >= 0.3))) {
    got <- auc_fn(pred, ev)
    roc <- pROC::roc(response = ev, predictor = pred, levels = c(FALSE, TRUE), direction = "<",
                     quiet = TRUE)
    expect_equal(got$auc, as.numeric(pROC::auc(roc)), tolerance = 1e-10)
    expect_equal(got$se, sqrt(pROC::var(roc, method = "delong")), tolerance = 1e-10)
  }
})

test_that("IR-decisioncurve-03 risks of the other level are flagged; the correct level is not", {
  d <- ir_fixture()
  wrong <- run_ir(d, outcomePositive = "none")
  expect_true(ir_has_notice(wrong, "Possible Inversion: good", "STRONG_WARNING"))
  right <- run_ir(d)
  expect_false(any(grepl("Possible Inversion", ir_notices(right)$title, fixed = TRUE)))
})

test_that("IR-decisioncurve-03 an uninformative model whose AUC dips below 0.5 by chance is not flagged", {
  skip_if_not_installed("pROC")
  set.seed(11)
  n <- 200
  d <- data.frame(y = factor(ifelse(rbinom(n, 1, 0.3) == 1, "event", "none"), levels = c("none", "event")),
                  good = runif(n))
  auc <- as.numeric(pROC::auc(pROC::roc(d$y == "event", d$good, levels = c(FALSE, TRUE),
                                        direction = "<", quiet = TRUE)))
  expect_lt(auc, 0.5)                               # precondition: the point estimate is below 0.5
  expect_false(any(grepl("Possible Inversion", ir_notices(run_ir(d))$title, fixed = TRUE)))
})

test_that("IR-decisioncurve-03 sparse classes: an uninformative model with 3 events is not flagged", {
  # The rank-sum gate is exact here; a DeLong-only gate is anti-conservative in sparse classes.
  flagged <- vapply(1:40, function(k) {
    set.seed(700 + k)
    d <- data.frame(y = factor(c(rep("event", 3), rep("none", 37)), levels = c("none", "event")),
                    good = runif(40))
    any(grepl("Possible Inversion", ir_notices(run_ir(d))$title, fixed = TRUE))
  }, logical(1))
  # exact one-sided level is <= 2.5%; 40 draws allow at most a few by chance
  expect_lte(sum(flagged), 3)
})

test_that("IR-decisioncurve-03 a clinical rule that recommends treatment mostly to non-events is flagged in rule wording", {
  d <- ir_fixture(n = 400)
  # treat the LOW-risk patients: the rule points away from the event
  d$rule <- factor(ifelse(d$good < 0.25, "treat", "watch"), levels = c("watch", "treat"))
  res <- run_ir(d, clinicalDecisionRule = TRUE, decisionRuleVar = "rule",
                decisionRulePositive = "treat", decisionRuleLabel = "Low-risk rule")
  expect_true(ir_has_notice(res, "Possible Inversion: Low-risk rule", "STRONG_WARNING"))
  expect_match(res$notices$content, "recommended intervention less often", fixed = TRUE)
  expect_false(ir_has_notice(res, "Possible Inversion: good", "STRONG_WARNING"))
})

test_that("IR-decisioncurve-03 two events among heavily tied predictions are not flagged (exact p = 0.038)", {
  # Codex review: wilcox.test's normal approximation gave p = 0.0024 here; the exact probability
  # that both events fall among the 20 tied zeros is choose(20, 2) / choose(100, 2) = 0.0384.
  d <- data.frame(y = factor(c(rep("event", 2), rep("none", 98)), levels = c("none", "event")),
                  good = c(rep(0, 20), rep(1, 80)))
  expect_false(any(grepl("Possible Inversion", ir_notices(run_ir(d))$title, fixed = TRUE)))
})

test_that("IR-decisioncurve-03 the inversion notice is reproducible and names its seed", {
  d <- ir_fixture()
  a <- run_ir(d, outcomePositive = "none")$notices$content
  b <- run_ir(d, outcomePositive = "none")$notices$content
  expect_identical(a, b)
  expect_match(a, "one-sided permutation p = ", fixed = TRUE)
  expect_match(a, "seed 42", fixed = TRUE)
})

test_that("IR-decisioncurve-03 constant predictions are not flagged", {
  d <- ir_fixture(n = 300); d$good <- 0.3
  expect_false(any(grepl("Possible Inversion", ir_notices(run_ir(d))$title, fixed = TRUE)))
})

test_that("Codex-07 malformed table thresholds are reported when only the clinical impact plot uses them", {
  res <- run_ir(ir_fixture(n = 300), showTable = FALSE, calculateClinicalImpact = FALSE,
                showClinicalImpactPlot = TRUE, selectedThresholds = "bogus")
  expect_true(ir_has_notice(res, "Some thresholds ignored", "WARNING"))
  expect_true(ir_has_notice(res, "Using default thresholds", "WARNING"))
})

test_that("Codex-07 plot-only mode does not describe table rows that do not exist", {
  res <- run_ir(ir_fixture(n = 300), showTable = FALSE, calculateClinicalImpact = FALSE,
                showClinicalImpactPlot = TRUE, thresholdRange = "custom", thresholdMin = 0.1,
                thresholdMax = 0.2, selectedThresholds = "0.3")
  expect_false(ir_has_notice(res, "Table thresholds outside the analysed range", "WARNING"))
})

test_that("IR-decisioncurve-01 the 1:1 warning waits for 20 per class", {
  set.seed(4)
  small <- data.frame(y = factor(rep(c("event", "none"), each = 19), levels = c("none", "event")),
                      good = c(runif(19, 0.2, 0.9), runif(19, 0.05, 0.6)))
  expect_false(ir_has_notice(run_ir(small), "Outcome Split Matches a Case-Control Ratio", "WARNING"))
  big <- data.frame(y = factor(rep(c("event", "none"), each = 20), levels = c("none", "event")),
                    good = c(runif(20, 0.2, 0.9), runif(20, 0.05, 0.6)))
  expect_true(ir_has_notice(run_ir(big), "Outcome Split Matches a Case-Control Ratio", "WARNING"))
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-04: rows are resampled as independent patients; say so
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-04 the one-row-per-patient assumption is stated", {
  res <- run_ir(ir_fixture(n = 300), confidenceIntervals = TRUE, bootReps = 100,
                showInterpretation = TRUE)
  expect_match(res$procedureNotes$content, "one row per patient", fixed = TRUE)
  expect_match(res$summaryText$content, "one row per patient", fixed = TRUE)
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-05: the y-zoom must not cut the drawn interval off
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-05 the decision-curve y-range contains every drawn interval limit", {
  set.seed(55)
  r <- runif(80, 0, 0.3)
  d <- data.frame(y = factor(rbinom(80, 1, r), levels = 0:1, labels = c("none", "event")), good = r)
  for (band in c("pointwise", "simultaneous")) {
    res <- run_ir(d, confidenceIntervals = TRUE, bootReps = 200, ciBand = band)
    p <- ir_render(res, "dcaPlot")
    grDevices::pdf(NULL)
    yr <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
    grDevices::dev.off()
    pd <- res$dcaPlot$state$plotData
    pd <- pd[pd$model == "good", ]
    lo <- if (band == "simultaneous") pd$sim_lower else pd$ci_lower
    hi <- if (band == "simultaneous") pd$sim_upper else pd$ci_upper
    expect_lte(yr[1], min(lo))
    expect_gte(yr[2], max(hi))
  }
})

test_that("IR-decisioncurve-05 the caption separates the band description from the seed", {
  res <- run_ir(ir_fixture(n = 300), confidenceIntervals = TRUE, bootReps = 100)
  cap <- ir_render(res, "dcaPlot")$labels$caption
  expect_match(cap, "Shaded: 95% pointwise CI\nRandom seed: 42", fixed = TRUE)
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-06: the inference caution of the cited paper is shown where p is shown
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-06 both comparison tables carry the Vickers 2023 caution", {
  res <- run_ir(ir_fixture(n = 300), models = c("good", "weak"), compareModels = TRUE,
                multiModelComparison = TRUE, bootReps = 100)
  expect_match(ir_notes(res$comparisonTable), "Vickers", fixed = TRUE)
  expect_match(ir_notes(res$modelComparisonEnhanced), "Vickers", fixed = TRUE)
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-07 / -08: footnote text
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-07 the footnote names the clinical rule by the label the table uses", {
  d <- ir_fixture(n = 300)
  d$rule <- factor(ifelse(d$good >= 0.3, "treat", "watch"), levels = c("watch", "treat"))
  res <- run_ir(d, clinicalDecisionRule = TRUE, decisionRuleVar = "rule",
                decisionRulePositive = "treat", decisionRuleLabel = "", showInterpretation = TRUE)
  titles <- vapply(res$resultsTable$columns, function(c) c$title, "")
  expect_true("Clinical Rule (treat)" %in% titles)
  expect_match(res$summaryText$content, "Applied as provided in the data (Clinical Rule (treat))",
               fixed = TRUE)
})

test_that("IR-decisioncurve-08 a population of 100000 is printed in full", {
  res <- run_ir(ir_fixture(n = 300), calculateClinicalImpact = TRUE, populationSize = 100000,
                showInterpretation = TRUE)
  expect_match(res$summaryText$content, "population of 100000 patients", fixed = TRUE)
  expect_no_match(res$summaryText$content, "e+05", fixed = TRUE)
})

# ---------------------------------------------------------------------------------------------
# IR-decisioncurve-10: relative utility is attributed
# ---------------------------------------------------------------------------------------------

test_that("IR-decisioncurve-10 the relative-utility caption cites Baker et al. 2009", {
  res <- run_ir(ir_fixture(n = 300), showRelativeUtility = TRUE)
  cap <- ir_render(res, "relativeUtilityPlot")$labels$caption
  expect_match(cap, "Baker", fixed = TRUE)
  expect_match(cap, "RU = (NB - D) / (prevalence - D)", fixed = TRUE)
})

# ---------------------------------------------------------------------------------------------
# /check-function 2026-10-13: an undefined statistic is a blank cell with its reason
# ---------------------------------------------------------------------------------------------
# jmvcore sends a numeric NA as a double, which jamovi prints "NaN", and a logical NA as missing,
# printed "."; only "" is blank. PPV with nobody classified positive, NPV with nobody classified
# negative, patients screened per true positive with no true positive, and the range of a model
# that never beats both references are undefined, not failed computations.

test_that("undefined PPV, NPV, screened-per-TP and benefit-range cells are blank, never NaN or '.'", {
  d <- ir_fixture(n = 300)
  # Every prediction in (0.002, 0.02): all positive at 0.1%, none at 30%, never useful.
  d$low <- withr::with_seed(7, runif(nrow(d), 0.002, 0.02))
  res <- run_ir(d, models = c("good", "low"), selectedThresholds = "0.001, 0.30",
                showBenefitRange = TRUE, showDecisionConsequences = TRUE,
                calculateClinicalImpact = TRUE)
  cell <- function(tbl, col, row) tbl$getCell(col, rowNo = row)
  row_of <- function(tbl, model, t) {
    df <- tbl$asDF
    which(df$model == model & abs(df$threshold - t) < 1e-12)
  }
  blank_with_note <- function(tbl, col, row, note) {
    c <- cell(tbl, col, row)
    expect_identical(c$value, "")
    expect_true(any(grepl(note, unlist(c$footnotes), fixed = TRUE)))
  }

  dc <- res$decisionConsequencesTable
  blank_with_note(dc, "npv", row_of(dc, "low", 0.001), "classified negative")
  blank_with_note(dc, "ppv", row_of(dc, "low", 0.30), "classified positive")
  ci <- res$clinicalImpactTable
  blank_with_note(ci, "number_needed_to_screen", row_of(ci, "low", 0.30), "no true positive")

  br <- res$benefitRangeTable
  low_row <- which(br$asDF$model == "low")
  for (col in c("range_start", "range_end", "range_width"))
    expect_identical(cell(br, col, low_row)$value, "")
  # A blank cell still reads back as NA, so as.data.frame() callers are unaffected.
  expect_true(is.na(br$asDF$range_start[low_row]))

  # Nowhere in these columns does a raw NA reach the client.
  for (spec in list(list(dc, c("ppv", "npv")), list(ci, "number_needed_to_screen"),
                    list(br, c("range_start", "range_end", "range_width")))) {
    tbl <- spec[[1]]
    for (col in spec[[2]]) for (r in seq_len(tbl$rowCount)) {
      v <- cell(tbl, col, r)$value
      expect_false(!is.character(v) && is.na(v), info = paste(col, "row", r))
    }
  }
})

# ---------------------------------------------------------------------------------------------
# /check-function-full follow-ups (CF-decisioncurve-NN, 2026-10-13)
# ---------------------------------------------------------------------------------------------

cf_rule_fixture <- function() {
  d <- ir_fixture(n = 400)
  d$rule <- factor(ifelse(d$good >= 0.3, "treat", "no"))
  d$rule01 <- as.numeric(d$rule == "treat")
  d
}

test_that("CF-decisioncurve-01 the clinical rule is bootstrap-compared like a 0/1 predictor column", {
  d <- cf_rule_fixture()
  rule <- run_ir(d, models = c("good", "weak"), clinicalDecisionRule = TRUE,
                 decisionRuleVar = "rule", decisionRulePositive = "treat", decisionRuleLabel = "Rule",
                 compareModels = TRUE, multiModelComparison = TRUE, bootReps = 200)
  ct <- rule$comparisonTable$asDF
  expect_equal(ct$comparison, c("good vs weak", "good vs Rule", "weak vs Rule"))
  expect_equal(rule$modelComparisonEnhanced$rowCount, 3)
  expect_false(ir_has_notice(rule, "Comparisons not tested", "INFO"))

  # Oracle: the same rule entered as a numeric 0/1 model column sits at the same pair index and
  # seed substream, so every bootstrap statistic must agree exactly.
  num <- run_ir(d, models = c("good", "weak", "rule01"),
                compareModels = TRUE, multiModelComparison = TRUE, bootReps = 200)
  cn <- num$comparisonTable$asDF
  cols <- c("weighted_auc_diff", "ci_lower", "ci_upper", "p_value", "p_value_adj")
  expect_equal(ct[, cols], cn[, cols], tolerance = 1e-12, ignore_attr = TRUE)
  expect_equal(rule$modelComparisonEnhanced$asDF$p_value, num$modelComparisonEnhanced$asDF$p_value,
               tolerance = 1e-12)

  # One model plus the rule is two strategies, so the comparison runs instead of the INFO notice.
  one <- run_ir(d, models = "good", clinicalDecisionRule = TRUE, decisionRuleVar = "rule",
                decisionRulePositive = "treat", decisionRuleLabel = "Rule",
                compareModels = TRUE, multiModelComparison = TRUE, bootReps = 200)
  expect_equal(one$comparisonTable$rowCount, 1)
  expect_equal(one$modelComparisonEnhanced$rowCount, 1)
  expect_false(ir_has_notice(one, "Model comparison needs two models", "INFO"))
  # ...and a lone model still gets it.
  solo <- run_ir(d, compareModels = TRUE, bootReps = 200)
  expect_true(ir_has_notice(solo, "Model comparison needs two models", "INFO"))
})

test_that("CF-decisioncurve-02 notices are listed by severity", {
  d <- ir_fixture(n = 40, seed = 5)
  d$good[1:3] <- NA          # Complete-Case WARNING, raised before the small-n STRONG_WARNING
  nd <- ir_notices(run_ir(d))
  expect_true(all(c("STRONG_WARNING", "WARNING", "INFO") %in% nd$type))
  expect_false(is.unsorted(match(nd$type, c("ERROR", "STRONG_WARNING", "WARNING", "INFO"))))
})

test_that("CF-decisioncurve-03/05 ciLevel is not offered for the Holm-p-only table; DeLong is cited", {
  root <- testthat::test_path("..", "..", "jamovi")
  skip_if_not(file.exists(file.path(root, "decisioncurve.r.yaml")))
  ui <- yaml::read_yaml(file.path(root, "decisioncurve.u.yaml"))
  find <- function(x, nm) {
    if (is.list(x)) {
      if (identical(x$name, nm)) return(x)
      for (k in x) { hit <- find(k, nm); if (!is.null(hit)) return(hit) }
    }
    NULL
  }
  expect_false(grepl("multiModelComparison", find(ui, "ciLevel")$enable, fixed = TRUE))
  ry <- yaml::read_yaml(file.path(root, "decisioncurve.r.yaml"))
  enh <- Filter(function(i) identical(i$name, "modelComparisonEnhanced"), ry$items)[[1]]
  expect_false("ciLevel" %in% unlist(enh$clearWith))
  expect_true("DeLong1988" %in% unlist(ry$refs))
})

test_that("CF-decisioncurve-03 the enhanced comparison's Holm p does not depend on ciLevel", {
  d <- cf_rule_fixture()
  p_at <- function(level) run_ir(d, models = c("good", "weak"), multiModelComparison = TRUE,
                                 bootReps = 200, ciLevel = level)$modelComparisonEnhanced$asDF
  expect_equal(p_at(0.95)$p_value, p_at(0.80)$p_value)
})

test_that("CF-decisioncurve-04 showNetBenefitCI is gone; confidenceIntervals alone draws the band", {
  expect_false("showNetBenefitCI" %in% names(formals(ClinicoPath::decisioncurve)))
  res <- run_ir(ir_fixture(n = 300), confidenceIntervals = TRUE, bootReps = 200)
  expect_true(all(c("ci_lower", "sim_lower") %in% names(res$dcaPlot$state$plotData)))
})

test_that("CF-decisioncurve-06 hidden plots carry no state; visible ones keep theirs", {
  res <- run_ir(ir_fixture(n = 300))
  expect_false(is.null(res$dcaPlot$state))
  for (p in c("clinicalImpactPlot", "interventionsAvoidedPlot", "relativeUtilityPlot",
              "standardizedNetBenefitPlot"))
    expect_null(res[[p]]$state, label = p)
  on <- run_ir(ir_fixture(n = 300), showRelativeUtility = TRUE, showClinicalImpactPlot = TRUE)
  expect_false(is.null(on$relativeUtilityPlot$state))
  expect_false(is.null(on$clinicalImpactPlot$state))
})

test_that("CF-decisioncurve-07 the always-on optimism disclosure is a WARNING about unknown provenance", {
  res <- run_ir(ir_fixture())
  expect_true(ir_has_notice(res, "Net Benefit Is Not Optimism-Corrected", "WARNING"))
  # it must not assert the predictions are unvalidated: external-validation risks are legitimate
  expect_false(grepl("not validated", res$notices$content, fixed = TRUE))
})

test_that("CF-decisioncurve-08 a reversed highlight range warns and is not drawn", {
  d <- ir_fixture(n = 300)
  has_rect <- function(p) any(vapply(p$layers, function(l) inherits(l$geom, "GeomRect"), logical(1)))
  bad <- run_ir(d, highlightRange = TRUE, highlightMin = 0.30, highlightMax = 0.20)
  expect_true(ir_has_notice(bad, "Highlight Range Not Drawn", "WARNING"))
  expect_false(has_rect(ir_render(bad, "dcaPlot")))
  same <- run_ir(d, highlightRange = TRUE, highlightMin = 0.20, highlightMax = 0.20)
  expect_true(ir_has_notice(same, "Highlight Range Not Drawn", "WARNING"))
  expect_false(has_rect(ir_render(same, "dcaPlot")))
  ok <- run_ir(d, highlightRange = TRUE, highlightMin = 0.10, highlightMax = 0.20)
  expect_false(ir_has_notice(ok, "Highlight Range Not Drawn", "WARNING"))
  expect_true(has_rect(ir_render(ok, "dcaPlot")))
})

test_that("CF-decisioncurve-09/10 table wording and the constant Projected Tests column", {
  res <- run_ir(ir_fixture(n = 300), weightedAUC = TRUE, resourceUtilization = TRUE)
  expect_match(ir_notes(res$weightedAUCTable), "Gain vs Best Default", fixed = TRUE)
  expect_false("tests_per_1000" %in% names(res$resourceUtilizationTable$asDF))
})

test_that("CF-decisioncurve-11 the clinical impact plot renders without a ggplot2 scale message", {
  # saveAs() renders with a theme that has no fill scale, so it cannot provoke the message; the
  # jamovi global ggtheme carries discrete fill/colour scales, which scale_fill_manual replaces.
  skip_if_not(exists("getGlobalTheme", envir = asNamespace("jmvcore"), inherits = FALSE))
  o <- ClinicoPath:::decisioncurveOptions$new(
    outcome = "y", outcomePositive = "event", models = "good",
    decisionRuleVar = NULL, decisionRulePositive = NULL, showClinicalImpactPlot = TRUE)
  an <- ClinicoPath:::decisioncurveClass$new(options = o, data = ir_fixture(n = 300))
  an$init(); an$run()
  th <- jmvcore:::getGlobalTheme("default", "jmv")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_message(expect_true(an$.__enclos_env__$private$.plotClinicalImpact(
    an$results$clinicalImpactPlot, ggtheme = th$ggtheme, theme = th$theme)))
})

# ═══════════════════════════════════════════════════════════════════════════════════════════════
# RF-decisioncurve-NN: fixes from /review-function decisioncurve (2026-10-13 night). Each block
# fails on the pre-fix code except RF-01 and the migration half of RF-03, which guard that the
# rewrite changed no number and broke no saved file.
# ═══════════════════════════════════════════════════════════════════════════════════════════════

rf_class <- function(d, ...) {
  o <- ClinicoPath:::decisioncurveOptions$new(
    outcome = "y", outcomePositive = "event", models = "good",
    decisionRuleVar = NULL, decisionRulePositive = NULL, ...)
  ClinicoPath:::decisioncurveClass$new(options = o, data = d)
}

rf_all_plots <- list(calculateClinicalImpact = TRUE, showClinicalImpactPlot = TRUE,
                     showInterventionAvoided = TRUE, showRelativeUtility = TRUE,
                     showStandardizedNetBenefit = TRUE)

test_that("RF-decisioncurve-01 the sorted net-benefit sweep equals the per-threshold definition", {
  set.seed(3); n <- 400
  th <- round(seq(0.05, 0.5, by = 0.01), 10)
  y <- factor(sample(c("none", "event"), n, TRUE, prob = c(.7, .3)), levels = c("none", "event"))
  pv <- rf_class(data.frame(y = y, good = runif(n)))$.__enclos_env__$private
  for (p in list(sample(c(0, 1, th[c(1, 11, 26, 46)], 0.123), n, TRUE),   # ties AT grid points
                 as.numeric(runif(n) < 0.4),                              # a 0/1 rule
                 runif(n))) {
    ref <- vapply(th, function(t) pv$.calculateNetBenefit(p, y, t, "event")$net_benefit, numeric(1))
    expect_identical(pv$.calculateNetBenefitsVectorized(p, y, th, "event"), ref)
    expect_identical(rev(pv$.calculateNetBenefitsVectorized(p, y, rev(th), "event")), ref)
  }
  # The bootstrap draws are the old per-threshold loop's, replicate for replicate.
  p <- sample(c(th[c(1, 11, 26)], runif(50)), n, TRUE)
  ref_lower <- withr::with_seed(11, {
    m <- matrix(NA_real_, 200, length(th))
    for (i in 1:200) {
      idx <- sample(n, n, TRUE)
      for (j in seq_along(th))
        m[i, j] <- pv$.calculateNetBenefit(p[idx], y[idx], th[j], "event")$net_benefit
    }
    apply(m, 2, stats::quantile, probs = (1 - 0.95) / 2)
  })
  got <- withr::with_seed(11, pv$.calculateBootstrapCI(p, y, th, "event", 200))
  expect_identical(unname(got$lower), unname(ref_lower))
})

test_that("RF-decisioncurve-02 the clinical impact bars stay distinct with red-green colour blindness", {
  skip_if_not(exists("getGlobalTheme", envir = asNamespace("jmvcore"), inherits = FALSE))
  an <- rf_class(ir_fixture(n = 300), showClinicalImpactPlot = TRUE)
  an$init(); an$run()
  th <- jmvcore:::getGlobalTheme("default", "jmv")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  ggplot2::set_last_plot(NULL)
  an$.__enclos_env__$private$.plotClinicalImpact(an$results$clinicalImpactPlot,
                                                  ggtheme = th$ggtheme, theme = th$theme)
  fills <- unique(ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]$fill)
  expect_setequal(toupper(fills), c("#0072B2", "#D55E00"))   # Okabe-Ito, not darkgreen/darkred
})

test_that("RF-decisioncurve-03 plot state holds no per-patient vector and does not grow with n", {
  plots <- c("dcaPlot", "clinicalImpactPlot", "interventionsAvoidedPlot",
             "relativeUtilityPlot", "standardizedNetBenefitPlot")
  sizes <- function(n) {
    res <- do.call(run_ir, c(list(ir_fixture(n = n)), rf_all_plots))
    vapply(plots, function(p) {
      st <- res[[p]]$state
      expect_true(all(names(st) %in% c("plotData", "prevalence", "impactData")), label = p)
      length(serialize(st, NULL))
    }, numeric(1))
  }
  expect_equal(sizes(300), sizes(3000))
})

test_that("RF-decisioncurve-03 state saved by the 2026-10-08 build (per-patient slices) still renders", {
  d <- ir_fixture(n = 300)
  an <- do.call(rf_class, c(list(d), rf_all_plots))
  pv <- an$.__enclos_env__$private
  pv$.run()
  old <- list(
    clinicalImpactPlot = list(dcaResults = pv$.dcaResults, analysisOutcomes = pv$.analysisOutcomes,
                              outcomePositive = "event"),
    interventionsAvoidedPlot = list(dcaResults = pv$.dcaResults, treatAllNB = pv$.treatAllNB),
    relativeUtilityPlot = list(dcaResults = pv$.dcaResults, plotData = pv$.plotData,
                               analysisOutcomes = pv$.analysisOutcomes, outcomePositive = "event"),
    standardizedNetBenefitPlot = list(dcaResults = pv$.dcaResults, plotData = pv$.plotData,
                                      analysisOutcomes = pv$.analysisOutcomes, outcomePositive = "event"))
  renderer <- c(clinicalImpactPlot = ".plotClinicalImpact",
                interventionsAvoidedPlot = ".plotInterventionsAvoided",
                relativeUtilityPlot = ".plotRelativeUtility",
                standardizedNetBenefitPlot = ".plotStandardizedNetBenefit")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  for (p in names(old)) {
    b <- do.call(rf_class, c(list(d), rf_all_plots))
    b$results[[p]]$setState(old[[p]])
    bp <- b$.__enclos_env__$private
    expect_true(bp[[renderer[[p]]]](b$results[[p]], ggtheme = ggplot2::theme_bw(), theme = list()),
                label = paste(p, "renders from an old-format slice"))
    if (p == "clinicalImpactPlot") expect_equal(bp$.impactData, pv$.impactData)
    if (p != "interventionsAvoidedPlot") expect_equal(bp$.prevalence, pv$.prevalence)
  }
})

test_that("RF-decisioncurve-04 the enhanced comparison prints no p < 0.05 verdict column", {
  res <- run_ir(ir_fixture(n = 300), models = c("good", "weak"), multiModelComparison = TRUE,
                bootReps = 100)
  cols <- vapply(res$modelComparisonEnhanced$columns, function(c) c$name, "")
  expect_false("conclusion" %in% cols)
  expect_equal(res$modelComparisonEnhanced$rowCount, 1)
})

test_that("RF-decisioncurve-05 unticking the decision curve plot hides it and stops the intervals", {
  off <- run_ir(ir_fixture(n = 300), showPlot = FALSE, confidenceIntervals = TRUE, bootReps = 100,
                showInterpretation = TRUE)
  expect_false(isTRUE(off$dcaPlot$visible))
  expect_null(off$dcaPlot$state)
  expect_no_match(off$summaryText$content, "Unit of Resampling", fixed = TRUE)
  expect_no_match(off$notices$content, "Few Positive Classifications", fixed = TRUE)
  on <- run_ir(ir_fixture(n = 300), confidenceIntervals = TRUE, bootReps = 100,
               showInterpretation = TRUE)
  expect_true("ci_lower" %in% names(on$dcaPlot$state$plotData))
  expect_match(on$summaryText$content, "Unit of Resampling", fixed = TRUE)

  root <- testthat::test_path("..", "..", "jamovi")
  skip_if_not(file.exists(file.path(root, "decisioncurve.u.yaml")))
  ui <- yaml::read_yaml(file.path(root, "decisioncurve.u.yaml"))
  find <- function(x, nm) {
    if (is.list(x)) {
      if (identical(x$name, nm)) return(x)
      for (k in x) { hit <- find(k, nm); if (!is.null(hit)) return(hit) }
    }
    NULL
  }
  expect_identical(find(ui, "confidenceIntervals")$enable, "(showPlot)")
  expect_identical(find(ui, "ciBand")$enable, "(confidenceIntervals && showPlot)")
  expect_match(find(ui, "bootReps")$enable, "(confidenceIntervals && showPlot)", fixed = TRUE)
  expect_match(find(ui, "ciLevel")$enable, "(confidenceIntervals && showPlot)", fixed = TRUE)
})

test_that("RF-decisioncurve-06/07 whole-sentence i18n units; payoff labels say monetary", {
  src_path <- testthat::test_path("..", "..", "R", "decisioncurve.b.R")
  ui_path <- testthat::test_path("..", "..", "jamovi", "decisioncurve.u.yaml")
  skip_if_not(file.exists(src_path) && file.exists(ui_path))   # parked analyses ship no source
  src <- readLines(src_path)
  expect_false(any(grepl('.("(tied)")', src, fixed = TRUE)))
  expect_false(any(grepl('.("none.")', src, fixed = TRUE)))
  ui <- paste(readLines(ui_path), collapse = "\n")
  expect_match(ui, "label: Monetary value per true positive", fixed = TRUE)
  expect_match(ui, "label: Monetary harm per false positive", fixed = TRUE)
  expect_match(ui, "label: Bootstrap and Resampling", fixed = TRUE)
})

test_that("RF-decisioncurve-08a the clinical interpretation is opt-in; the caveats are not", {
  res <- run_ir(ir_fixture(n = 300))
  expect_false(isTRUE(res$summaryText$visible))
  # "<div></div>" is the blank (an Html "" is skipped by the client), so test for visible text.
  expect_false(isTRUE(nzchar(gsub("<[^>]+>", "", res$summaryText$content))))
  expect_match(res$procedureNotes$content, "one row per patient", fixed = TRUE)
  on <- run_ir(ir_fixture(n = 300), showInterpretation = TRUE)
  expect_true(isTRUE(on$summaryText$visible))
  expect_match(on$summaryText$content, "Interpretation Guidelines", fixed = TRUE)
})

test_that("RF-decisioncurve-08b the interventions-avoided plot zooms to -100 and names the dive", {
  set.seed(7)
  y <- factor(c(rep("event", 60), rep("none", 140)), levels = c("none", "event"))
  # 20 events scored 0.5%: missed at every threshold above that, each costing 1/odds = 99 at 1%.
  d <- data.frame(y = y, good = c(rep(0.005, 20), runif(40, 0.3, 0.9), runif(140, 0.02, 0.5)))
  res <- run_ir(d, thresholdRange = "auto", showInterventionAvoided = TRUE)
  p <- ir_render(res, "interventionsAvoidedPlot")
  grDevices::pdf(NULL); b <- ggplot2::ggplot_build(p); grDevices::dev.off()
  nia <- b$data[[1]]$y
  expect_lt(min(nia), -100)
  yr <- b$layout$panel_params[[1]]$y.range
  expect_gt(yr[1], min(nia))          # zoomed, not drawn to the bottom of the dive
  expect_lte(yr[1], -100)
  expect_gte(yr[2], max(nia))         # nothing cut off at the top
  expect_match(p$labels$caption, "good falls below -100 per 100 patients", fixed = TRUE)
  # no dive, no zoom and no extra caption line
  calm <- ir_render(run_ir(ir_fixture(n = 300), showInterventionAvoided = TRUE),
                    "interventionsAvoidedPlot")
  expect_no_match(calm$labels$caption, "falls below", fixed = TRUE)
})

test_that("RF-decisioncurve-08c table rows are built in .init() and filled, not appended, by .run()", {
  d <- cf_rule_fixture()
  o <- ClinicoPath:::decisioncurveOptions$new(
    outcome = "y", outcomePositive = "event", models = c("good", "weak"),
    clinicalDecisionRule = TRUE, decisionRuleVar = "rule", decisionRulePositive = "treat",
    decisionRuleLabel = "Rule", showBenefitRange = TRUE, weightedAUC = TRUE, compareModels = TRUE,
    multiModelComparison = TRUE, calculateClinicalImpact = TRUE, costBenefitAnalysis = TRUE,
    showDecisionConsequences = TRUE, resourceUtilization = TRUE, bootReps = 100)
  # 6 default table thresholds, 3 strategies (2 models + the rule), 3 pairs
  want <- c(resultsTable = 6, benefitRangeTable = 3, weightedAUCTable = 3, comparisonTable = 3,
            modelComparisonEnhanced = 3, clinicalImpactTable = 18, costBenefitTable = 18,
            decisionConsequencesTable = 18, resourceUtilizationTable = 18)
  counts <- function(an) vapply(names(want), function(t) as.numeric(an$results[[t]]$rowCount), 0)

  an <- ClinicoPath:::decisioncurveClass$new(options = o, data = d)
  an$init()
  expect_equal(counts(an), want)                                  # structure before any computation
  expect_equal(an$results$resultsTable$getCell(rowNo = 1, "threshold")$value, 0.05)
  expect_equal(an$results$comparisonTable$getCell(rowNo = 3, "comparison")$value, "weak vs Rule")
  an$run()
  expect_equal(counts(an), want)
  expect_true(all(is.finite(an$results$resultsTable$asDF$treat_all)))
  an$.__enclos_env__$private$.run()                               # a second cycle on the same object
  expect_equal(counts(an), want)

  direct <- ClinicoPath:::decisioncurveClass$new(options = o, data = d)
  direct$.__enclos_env__$private$.run()                           # no .init(): rows built on demand
  expect_equal(counts(direct), want)

  # A lone model with both comparisons ticked has no pair, and no phantom "pair_" row.
  solo <- run_ir(d, compareModels = TRUE, multiModelComparison = TRUE, bootReps = 100)
  expect_equal(solo$comparisonTable$rowCount, 0)
  expect_equal(solo$modelComparisonEnhanced$rowCount, 0)
})
