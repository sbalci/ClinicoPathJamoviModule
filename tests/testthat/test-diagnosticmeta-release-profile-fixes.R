# Regression tests for the diagnosticmeta findings of the 2026-10-18 release-profile
# check (quality-reports/ONCOPATH_RELEASE_PROFILE_2026-10-18-night.md). Each test names
# the finding it guards and fails on the backend as it stood before the 2026-10-19 fix.

notes_of <- function(tbl) vapply(tbl$notes, function(x) paste(x$note, collapse = " "), "")
notice_text <- function(res) gsub("\\s+", " ", gsub("<[^>]+>", " ",
                                                    paste(res$notices$content, collapse = " ")))
count_of <- function(txt, pattern) lengths(regmatches(txt, gregexpr(pattern, txt, fixed = TRUE)))

dm_data <- function() data.frame(
  study = paste0("S", 1:12),
  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
  fp = c(10,  8, 15,  5, 20,  4, 12,  9, 18,  6, 11,  7),
  fn = c( 5,  7, 10,  3, 15,  2,  8,  6, 12,  4,  9,  5),
  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
  stringsAsFactors = FALSE)

dm_run <- function(d, ...) ClinicoPath::diagnosticmeta(
  data = d, study = "study", true_positives = "tp", false_positives = "fp",
  false_negatives = "fn", true_negatives = "tn", ...)

cells <- c("tp", "fp", "fn", "tn")

# Through the class, as jamovi runs it: a reject() leaves the notices in place,
# and each notice keeps its severity.
dm_class <- function(d, ...) {
  a <- ClinicoPath:::diagnosticmetaClass$new(options = ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp", false_negatives = "fn",
    true_negatives = "tn", ...), data = d)
  a$init(noThrow = TRUE)
  if (!a$errored) a$run(noThrow = TRUE)
  a
}
notice_types <- function(a) {
  nl <- a$.__enclos_env__$private$.noticeList
  stats::setNames(vapply(nl, function(x) x$type, ""), vapply(nl, function(x) x$title, ""))
}


test_that("Deeks' test uses the raw counts whatever zero-cell correction is chosen (R01)", {
  # Was: under 'constant', 'zero_cells' and 'reciprocal_n' the test received the
  # corrected counts, found no zero cell, and always gave a verdict - on a
  # per-study correction that builds a size-related trend into its own outcome
  # (false asymmetry in 55-73% of null meta-analyses).
  d <- dm_data(); d$fn[c(2, 4, 6, 8)] <- 0           # 4 of 12 studies: more than a quarter
  cd <- d; cd[cells] <- cd[cells] + 0.5              # oracle: +0.5 to EVERY study, raw counts
  n1 <- d$tp + d$fn; n0 <- d$fp + d$tn
  ess <- 4 * n1 * n0 / (n1 + n0)
  ldor <- log((cd$tp * cd$tn) / (cd$fp * cd$fn))
  oracle <- summary(stats::lm(ldor ~ I(1 / sqrt(ess)), weights = ess))$coefficients[2, 3]

  for (corr in c("none", "constant", "zero_cells", "reciprocal_n")) {
    r <- dm_run(d, bivariate_analysis = FALSE, publication_bias = TRUE, funnel_plot = TRUE,
                zero_cell_correction = corr)
    pb <- r$publicationbias$asDF
    expect_equal(pb$statistic[1], oracle, tolerance = 1e-6, label = corr)
    expect_match(pb$interpretation[1], "Not interpretable", info = corr)
    expect_true(any(r$funnelplot$state$fn == 0), info = corr)   # the funnel carries the same raw counts
    expect_true(any(grepl("withheld as a precaution", notes_of(r$publicationbias), fixed = TRUE)), info = corr)
    if (corr != "none") {
      expect_true(any(grepl("The test uses the raw counts", notes_of(r$publicationbias), fixed = TRUE)),
                  info = corr)
      expect_match(notice_text(r), "Deeks' test and the funnel plot use the raw counts with their own 0.5 correction",
                   fixed = TRUE, info = corr)
    }
  }
})


test_that("under the default setting zero-cell studies stay in every univariate model (R02/R22)", {
  # Was: the heterogeneity and meta-regression models dropped every study with a
  # zero cell - exactly the 100% sensitivity or specificity studies - while the
  # bivariate model kept them with +0.5, and the notice said only "fewer studies".
  d <- dm_data(); d$grp <- factor(rep(c("A", "B"), each = 6))
  d$fn[1:3] <- 0                                     # three perfect-sensitivity studies, all in A
  r <- dm_run(d, covariate = "grp", meta_regression = TRUE, heterogeneity_analysis = TRUE)

  cd <- d; z <- rowSums(cd[cells] == 0) > 0
  cd[z, cells] <- cd[z, cells] + 0.5                 # mada's correction.control = "single"
  yi <- stats::qlogis(cd$tp / (cd$tp + cd$fn)); vi <- 1 / cd$tp + 1 / cd$fn
  ref <- metafor::rma(yi, vi, mods = ~ grp, data = cd, method = "REML", test = "knha")
  t <- r$metaregression$asDF
  expect_equal(t$estimate[t$measure == "Sensitivity"], as.numeric(ref$beta), tolerance = 1e-6)
  expect_equal(r$heterogeneity$asDF$q_statistic[1],
               metafor::rma(yi, vi, method = "REML")$QE, tolerance = 1e-6)
  expect_true(any(grepl("3 study/studies with a zero cell were kept", notes_of(r$metaregression), fixed = TRUE)))
  expect_true(any(grepl("3 study/studies with a zero cell were kept", notes_of(r$heterogeneity), fixed = TRUE)))
  expect_match(notice_text(r),
               "every model (bivariate, proportional-hazards SROC, heterogeneity and meta-regression) adds 0.5",
               fixed = TRUE)
})


test_that("repeated identifiers are a STRONG_WARNING and blank ones are counted once (R03/R18)", {
  # Was: one WARNING counted each blank identifier twice and listed
  # "Unnamed study, Unnamed study", and called relabelling the remedy for rows
  # that share participants.
  d <- dm_data(); d$study[2] <- "S1"; d$study[c(7, 9)] <- NA
  r <- dm_run(d, show_analysis_summary = TRUE)
  txt <- notice_text(r)
  types <- notice_types(dm_class(d))
  expect_equal(unname(types["Repeated study identifiers"]), "STRONG_WARNING")
  expect_equal(unname(types["Missing study identifiers"]), "WARNING")
  expect_match(r$summary$content, "pooled as separate studies, so some participants are counted more than once",
               fixed = TRUE)
  expect_match(txt, "Repeated study identifiers", fixed = TRUE)
  expect_match(txt, "2 rows share 1 study identifier(s) (S1)", fixed = TRUE)
  expect_match(txt, "counts those participants more than once", fixed = TRUE)
  expect_match(txt, "2 study/studies have no identifier", fixed = TRUE)
  expect_false(grepl("Unnamed study, Unnamed study", txt, fixed = TRUE))
  expect_true(any(grepl("participants they share are counted more than once",
                        notes_of(r$bivariateresults), fixed = TRUE)))
  # no repeat, no qualifier
  expect_false(any(grepl("counted more than once", notes_of(dm_run(dm_data())$bivariateresults), fixed = TRUE)))
})


test_that("the scope note counts participants from the raw counts (R04)", {
  # Was: summed from the corrected counts, so 'constant' added one person per
  # corrected study and +1/N printed a fractional participant total.
  d <- dm_data(); d$fn[2] <- 0; d$fp[5] <- 0
  total <- base::format(sum(d$tp + d$fp + d$fn + d$tn), big.mark = ",")
  for (corr in c("constant", "zero_cells", "reciprocal_n")) {
    n <- notes_of(dm_run(d, zero_cell_correction = corr)$bivariateresults)
    scope <- n[grepl("Pooled from", n, fixed = TRUE)]
    expect_match(scope, sprintf("comprising %s participants", total), fixed = TRUE, info = corr)
  }
})


test_that("the I-squared note names the between-study correlation (R05)", {
  # Was: "ignores the within-study correlation" - sensitivity and specificity
  # come from disjoint groups, so they have none.
  n <- notes_of(dm_run(dm_data())$bivariateresults)
  expect_true(any(grepl("between-study correlation between sensitivity and specificity (the threshold effect)",
                        n, fixed = TRUE)))
  expect_false(any(grepl("within-study correlation", n, fixed = TRUE)))
})


test_that("the summary carries the LR intervals and qualifies a grade they do not support (R06)", {
  # Was: the likelihood ratios were graded, and pasted into the copy text, from
  # the point estimates alone.
  r <- dm_run(dm_data()[1:5, ], show_analysis_summary = TRUE)
  b <- r$bivariateresults$asDF
  html <- r$summary$content
  expect_match(html, sprintf("Positive LR %.2f (%.2f-%.2f) and negative LR %.2f (%.2f-%.2f).",
                             b$estimate[3], b$ci_lower[3], b$ci_upper[3],
                             b$estimate[4], b$ci_lower[4], b$ci_upper[4]), fixed = TRUE)
  # here LR+ 6.2-10.2 crosses the cut-off at 10 and LR- 0.12-0.22 the one at 0.2
  expect_lt(b$ci_lower[3], 10); expect_gt(b$ci_upper[3], 10)
  expect_lt(b$ci_lower[4], 0.2); expect_gt(b$ci_upper[4], 0.2)
  expect_equal(count_of(html, "spans more than one evidence category"), 2L)
  # intervals inside one category: no qualifier
  html12 <- dm_run(dm_data(), show_analysis_summary = TRUE)$summary$content
  expect_equal(count_of(html12, "spans more than one evidence category"), 0L)
})


test_that("the static panels make no fitness-for-use or threshold claims (R07, R01 text)", {
  r <- dm_run(dm_data(), show_interpretation = TRUE, sroc_plot = TRUE, show_plot_explanations = TRUE)
  interp <- r$interpretation$content
  expect_false(grepl("acceptable miss rate for screening", interp, fixed = TRUE))
  expect_match(interp, "about 11-20 of every 100 diseased cases missed", fixed = TRUE)
  expect_false(grepl("so a verdict is always given", interp, fixed = TRUE))
  expect_match(interp, "The test always uses the raw counts", fixed = TRUE)
  expect_false(grepl("Prioritize high sensitivity", interp, fixed = TRUE))
  expect_false(grepl("Prioritize high specificity", interp, fixed = TRUE))
  expect_match(interp, "the level needed depends on prevalence and on the cost of a missed case", fixed = TRUE)
  sroc_txt <- r$srocplot_explanation$content
  expect_false(grepl("identify optimal operating points", sroc_txt, fixed = TRUE))
  expect_match(sroc_txt, "does not identify a cut-off", fixed = TRUE)
})


test_that("one exclusion notice gives the final count, and no set-level power notice (R10/R14/R21)", {
  # Was: the exclusion notice was written before the fractional-count step, so
  # it said "remaining 8" for 7 analysed studies; a set-level power WARNING
  # quoted counts no fitted model used.
  d <- dm_data()[1:9, ]; d$tp[3] <- NA; d$tp[5] <- d$tp[5] + 0.5
  d$year <- 2001:2009
  r <- dm_run(d, covariate = "year", meta_regression = TRUE)
  txt <- notice_text(r)
  expect_equal(count_of(txt, "were excluded before analysis"), 1L)
  # 2026-10-19 audit: the reasons are whole sentences after the count, not "(%s)" fragments
  expect_match(txt, paste("2 of 9 studies were excluded before analysis. All results below are based on",
                          "the remaining 7 studies. Missing counts: 1 study/studies. Fractional counts: 1 study/studies."),
               fixed = TRUE)
  expect_true(any(grepl("Pooled from 7 studies", notes_of(r$bivariateresults), fixed = TRUE)))
  expect_false(grepl("Meta-regression power caution", txt, fixed = TRUE))
  expect_true(any(grepl("Sensitivity meta-regression on 7 studies", notes_of(r$metaregression), fixed = TRUE)))
  expect_match(txt, "1 of 9 studies have fractional counts", fixed = TRUE)   # the same denominator as above

  # too few left: the notice says so instead of promising results, beside the error
  few <- dm_data()[1:4, ]; few$tp[1] <- NA; few$tp[2] <- few$tp[2] + 0.5
  a <- dm_class(few)
  expect_true(a$errored)
  ftxt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", a$results$notices$content))
  expect_match(ftxt, "2 of 4 studies were excluded before analysis, leaving 2 - too few to pool. Missing counts: 1 study/studies. Fractional counts: 1 study/studies.",
               fixed = TRUE)
  expect_false(grepl("All results below", ftxt, fixed = TRUE))
})


test_that("the prediction note compares interval and region at the chosen level (R11)", {
  # Was: "below about eight studies the printed interval is the wider" - true
  # only at 95%; at 99% the crossover is 15 studies.
  n99 <- notes_of(dm_run(dm_data(), sroc_plot = TRUE, confidence_level = 99)$bivariateresults)
  p99 <- n99[grepl("Prediction interval for a future study", n99, fixed = TRUE)]
  expect_match(p99, "t on 10 df (3.169)", fixed = TRUE)
  expect_match(p99, "the printed interval is therefore the wider of the two", fixed = TRUE)
  n95 <- notes_of(dm_run(dm_data(), sroc_plot = TRUE)$bivariateresults)
  p95 <- n95[grepl("Prediction interval for a future study", n95, fixed = TRUE)]
  expect_match(p95, "radius mada uses (2.448)", fixed = TRUE)
  expect_match(p95, "the region therefore extends further than the printed interval", fixed = TRUE)
})


test_that("bivariate non-convergence is retried, and withheld when it persists (R13)", {
  # Was: mvmeta's "convergence not reached" warning was the only signal and
  # never reached the user, so an unfinished fit was reported as final.
  skip_if_not_installed("mada")
  orig <- mada::reitsma

  # the default limit is reached, the 5000-iteration retry converges
  r <- with_mocked_bindings(
    dm_run(dm_data()),
    reitsma = function(data, ..., control = list()) {
      m <- orig(data, ..., control = control)
      if (is.null(control$maxiter)) warning("convergence not reached after maximum number of iterations")
      m
    },
    .package = "mada")
  expect_true(any(grepl("refitted with a limit of 5000 and converged", notes_of(r$bivariateresults), fixed = TRUE)))
  expect_false(is.na(r$bivariateresults$asDF$estimate[1]))

  # converged == FALSE with no warning is caught too, and a refit that errors
  # is a failed fit, not "did not converge within 5000 iterations"
  r <- with_mocked_bindings(
    dm_run(dm_data()[1:4, ]),
    reitsma = function(data, ..., control = list()) { m <- orig(data, ..., control = control); m$converged <- FALSE; m },
    .package = "mada")
  expect_match(notice_text(r), "Bivariate model did not converge", fixed = TRUE)
  expect_false(grepl("Very few studies", notice_text(r), fixed = TRUE))   # no caution about a pair not shown
  r <- with_mocked_bindings(
    dm_run(dm_data()),
    reitsma = function(data, ..., control = list()) {
      if (!is.null(control$maxiter)) stop("refit exploded")
      m <- orig(data, ..., control = control); m$converged <- FALSE; m
    },
    .package = "mada")
  expect_match(notice_text(r), "Bivariate model failed", fixed = TRUE)
  expect_match(notice_text(r), "refit exploded", fixed = TRUE)
  expect_false(grepl("did not converge", notice_text(r), fixed = TRUE))

  # it never converges
  local_mocked_bindings(
    reitsma = function(data, ..., control = list()) {
      m <- orig(data, ..., control = control)
      warning("convergence not reached after maximum number of iterations")
      m
    },
    .package = "mada")
  r <- dm_run(dm_data(), sroc_plot = TRUE, show_analysis_summary = TRUE)
  txt <- notice_text(r)
  expect_match(txt, "Bivariate model did not converge", fixed = TRUE)
  expect_false(grepl("Substantial between-study heterogeneity", txt, fixed = TRUE))
  expect_true(all(is.na(r$bivariateresults$asDF$estimate)))
  bn <- notes_of(r$bivariateresults)
  expect_true(any(grepl("did NOT converge within 5000 iterations", bn, fixed = TRUE)))
  expect_false(any(grepl("Prediction interval for a future study", bn, fixed = TRUE)))
  expect_false(any(grepl("Between-study variance components", bn, fixed = TRUE)))
  expect_null(r$srocplot$state$pooled_sens)
  expect_match(r$srocplot$state$curve_note, "did not converge", fixed = TRUE)
  expect_match(r$summary$content, "did not converge", fixed = TRUE)
})


test_that("the SROC plot draws the studies and says why when there is no bivariate model (R15/R20)", {
  # Was: a one-line text panel, cut off at both ends, asking the user to
  # "Enable 'Bivariate Analysis'" - even when it was on and the fit had failed.
  skip_if_not_installed("mada")
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", bivariate_analysis = FALSE, sroc_plot = TRUE)
  a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = dm_data()); a$run()
  off <- a$results$srocplot$state
  expect_null(off$message)
  expect_equal(nrow(off$data), 12)
  expect_match(off$curve_note, "'Bivariate random-effects model'", fixed = TRUE)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(a$.__enclos_env__$private$.srocplot(a$results$srocplot,
                                                   ggtheme = ggplot2::theme_grey(), theme = NULL))

  local_mocked_bindings(reitsma = function(...) stop("simulated failure"), .package = "mada")
  r <- dm_run(dm_data(), sroc_plot = TRUE)
  expect_match(notice_text(r), "Bivariate model failed", fixed = TRUE)
  st <- r$srocplot$state
  expect_equal(nrow(st$data), 12)
  expect_null(st$pooled_sens)
  expect_match(st$curve_note, "could not be fitted", fixed = TRUE)
  expect_false(grepl("switched off", st$curve_note, fixed = TRUE))
})


test_that("an error after the bivariate model is stored leaves no model for the SROC plot (R15)", {
  # The model is stored before the summary is written; an error there used to
  # leave it behind, so the SROC plot drew a pooled point the ERROR notice
  # called unavailable. The summary's escape of the corrected study names is
  # made to fail for one study, after the model is stored.
  skip_if_not_installed("htmltools")
  d <- dm_data(); d$study[3] <- "BOOM"; d$fn[3] <- 0
  orig_esc <- htmltools::htmlEscape
  local_mocked_bindings(htmlEscape = function(text, ...) {
    if (identical(as.character(text), "BOOM")) stop("boom")
    orig_esc(text, ...)
  }, .package = "htmltools")
  r <- dm_run(d, zero_cell_correction = "constant", show_analysis_summary = TRUE, sroc_plot = TRUE)
  # 2026-10-19 audit: the summary now runs outside the bivariate tryCatch, so
  # a summary error is reported as a summary failure and the fitted model -
  # whose rows are filled - stays for the SROC plot. (The no-model-left-behind
  # guarantee for an error inside the fit is in the error handler, and the
  # missing-coefficients case is covered in test-diagnosticmeta-audit-20261019.R.)
  txt <- notice_text(r)
  expect_false(grepl("Bivariate model failed", txt, fixed = TRUE))
  expect_match(txt, "Analysis summary unavailable", fixed = TRUE)
  expect_false(is.null(r$srocplot$state$pooled_sens))
})


test_that("the analysis summary names the control when the bivariate model is off", {
  html <- dm_run(dm_data(), bivariate_analysis = FALSE, show_analysis_summary = TRUE)$summary$content
  expect_match(html, "Tick 'Bivariate random-effects model' for pooled estimates.", fixed = TRUE)
})


test_that("an unconverged PHM fit fills no cell and raises no AUC verdict (R16/R19)", {
  # Was: the last iterate filled theta, tau-squared and the AUC, and drove
  # 'Low SROC AUC' / 'does not fit' notices beside a notice calling it untrustworthy.
  skip_if_not_installed("mada")
  d <- data.frame(study = paste0("S", 1:6),
                  tp = c(45, 2, 60, 22, 80, 5), fp = c(10, 30, 30, 5, 2, 40),
                  fn = c(5, 30, 2, 6, 25, 20),  tn = c(90, 10, 60, 60, 150, 20))
  # the converged fit has AUC 0.39; with the bivariate model off, the verdict
  # must not refer to a pooled pair "above"
  off <- notice_text(dm_run(d, bivariate_analysis = FALSE, hsroc_analysis = TRUE))
  expect_match(off, "Proportional-hazards SROC model does not fit", fixed = TRUE)
  expect_false(grepl("pooled sensitivity and specificity above", off, fixed = TRUE))
  expect_match(off, "Tick 'Bivariate random-effects model' for the bivariate SROC curve instead", fixed = TRUE)
  pooled <- notice_text(dm_run(d, hsroc_analysis = TRUE))
  expect_match(pooled, "The pooled sensitivity and specificity above are unaffected", fixed = TRUE)
  failed <- with_mocked_bindings(notice_text(dm_run(d, hsroc_analysis = TRUE)),
                                 reitsma = function(...) stop("simulated failure"), .package = "mada")
  expect_match(failed, "The bivariate model gives no usable curve here either", fixed = TRUE)
  expect_false(grepl("Use the bivariate model", failed, fixed = TRUE))

  orig <- mada::phm
  local_mocked_bindings(phm = function(...) {
    m <- orig(...)
    warning("Reached maximum number of iterations!")
    m
  }, .package = "mada")
  r <- dm_run(d, hsroc_analysis = TRUE)
  expect_true(all(is.na(r$hsrocresults$asDF$estimate)))
  txt <- notice_text(r)
  expect_match(txt, "Proportional-hazards SROC model did not converge", fixed = TRUE)
  expect_false(grepl("(HSROC)", txt, fixed = TRUE))
  expect_false(grepl("Low SROC AUC", txt, fixed = TRUE))
  expect_false(grepl("does not fit", txt, fixed = TRUE))
  expect_match(txt, "Use the bivariate model and its SROC curve instead", fixed = TRUE)
})


test_that("the few-studies warning describes the fixed-effect model when it is chosen (R17)", {
  d <- dm_data()[1:4, ]
  fixed <- notice_text(suppressWarnings(dm_run(d, method = "fixed")))
  expect_match(fixed, "The fixed-effect model estimates two pooled means", fixed = TRUE)
  expect_false(grepl("five parameters", fixed, fixed = TRUE))
  expect_match(notice_text(suppressWarnings(dm_run(d))), "five parameters", fixed = TRUE)
})


test_that("option titles use sentence case and one symbol for study size (R23)", {
  path <- testthat::test_path("..", "..", "jamovi", "diagnosticmeta.a.yaml")
  skip_if_not(file.exists(path), "jamovi yaml not available in the installed test context")
  opts <- yaml::read_yaml(path)$options
  titles <- unlist(lapply(opts, function(o)
    vapply(o$options, function(x) if (is.null(x$title)) "" else x$title, "")))
  expect_true("Color-blind safe" %in% titles)
  expect_true("+1/N to all cells (N = study size)" %in% titles)
  expect_false(any(c("Color-Blind safe", "+1/N to all cells (n = study size)") %in% titles))
})


test_that("a meta-regression its studies fit exactly reports no inference (S08)", {
  # Was: Knapp-Hartung scales by a residual variance of zero, so every SE was 0,
  # z = Inf and p = 0 - read as overwhelming evidence.
  d <- data.frame(study = paste0("S", 1:6),
                  tp = rep(c(40, 30), each = 3), fn = rep(c(10, 20), each = 3),
                  fp = rep(c(5, 8), each = 3),   tn = rep(c(45, 42), each = 3),
                  grp = factor(rep(c("A", "B"), each = 3)))
  r <- dm_run(d, bivariate_analysis = FALSE, covariate = "grp", meta_regression = TRUE)
  t <- r$metaregression$asDF
  expect_equal(nrow(t), 4)
  expect_true(all(is.finite(t$estimate)))
  expect_true(all(is.na(t$std_error) & is.na(t$ci_lower) & is.na(t$ci_upper) &
                  is.na(t$z_value) & is.na(t$p_value)))
  expect_true(any(grepl("fit the covariate exactly", notes_of(r$metaregression), fixed = TRUE)))
  expect_equal(t$estimate[t$measure == "Sensitivity"],
               c(stats::qlogis(0.8), stats::qlogis(0.6) - stats::qlogis(0.8)), tolerance = 1e-8)
})


test_that("an empty dataset with a partial selection does not promise a result (S01)", {
  r <- ClinicoPath::diagnosticmeta(data = dm_data()[0, ], study = "study", true_positives = "tp")
  txt <- notice_text(r)
  expect_match(txt, "Data contains no rows", fixed = TRUE)
  expect_false(grepl("the meta-analysis will appear", txt, fixed = TRUE))
})
