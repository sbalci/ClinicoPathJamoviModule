# Regression tests for the 2026-09-20 /check-function-full audit fixes.
#
# Each test fails on the backend as it stood before that pass; the comment on
# each one names the defect it guards.

af_notes  <- function(t) vapply(t$notes, function(x) paste(x$note, collapse = " "), "")
af_notice <- function(r) gsub("\\s+", " ", gsub("<[^>]+>", " ",
                                                paste(r$notices$content, collapse = " ")))

af_data <- function() data.frame(
  study = paste0("S", 1:12),
  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
  fp = c(10,  8, 15,  5, 20,  4, 12,  9, 18,  6, 11,  7),
  fn = c( 5,  7, 10,  3, 15,  2,  8,  6, 12,  4,  9,  5),
  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
  stringsAsFactors = FALSE)

af_run <- function(d = af_data(), ...) suppressWarnings(ClinicoPath::diagnosticmeta(
  data = d, study = "study", true_positives = "tp", false_positives = "fp",
  false_negatives = "fn", true_negatives = "tn", ...))


test_that("switching everything off explains itself instead of showing a blank pane", {
  # Was: with `bivariate_analysis` unticked and the other options at their
  # defaults, every result item was hidden and the instructions panel had
  # already been hidden the moment the variables were assigned - a valid
  # dataset produced an entirely empty results pane with no explanation.
  r <- af_run(bivariate_analysis = FALSE)
  expect_match(af_notice(r), "Nothing selected to display", fixed = TRUE)
  expect_match(af_notice(r), "12 studies were read successfully", fixed = TRUE)

  # and it must not fire on an ordinary run
  expect_false(grepl("Nothing selected", af_notice(af_run()), fixed = TRUE))
})


test_that("the Notices panel is hidden when there is nothing to say", {
  # Was: an empty titled "Notices" box rendered on every clean run.
  clean <- af_run()
  expect_equal(nchar(paste(clean$notices$content, collapse = "")), 0)
  expect_false(isTRUE(clean$notices$visible))

  noisy <- af_run(bivariate_analysis = FALSE)
  expect_true(isTRUE(noisy$notices$visible))
})


test_that("three or four studies are not pooled without a caution", {
  # Was: k = 3 cleared the hard floor and was then pooled silently from a model
  # that estimates five parameters, printing 85.7% (79.2-90.4) with no warning.
  d <- af_data()
  for (k in c(3, 4)) {
    r <- af_run(d[seq_len(k), ])
    expect_match(af_notice(r), "Very few studies", fixed = TRUE,
                 info = paste("k =", k))
    expect_match(af_notice(r), "five parameters", fixed = TRUE)
  }
  for (k in c(5, 12)) {
    expect_false(grepl("Very few studies", af_notice(af_run(d[seq_len(k), ])), fixed = TRUE),
                 info = paste("k =", k))
  }
})


test_that("hidden explanation panels are not built at all", {
  # Was: about + interpretation + the three plot explanations + the analysis
  # summary were generated on every run and written into every saved .omv even
  # with all six checkboxes off - roughly 31 KB of HTML nobody could see. The
  # four show_* options appeared nowhere in the backend.
  panels <- c("about", "interpretation", "forestplot_explanation",
              "srocplot_explanation", "funnelplot_explanation", "summary")
  chars <- function(r) vapply(panels, function(n) nchar(paste(r[[n]]$content, collapse = "")), 0)

  off <- af_run()
  expect_equal(unname(sum(chars(off))), 0)

  on <- af_run(show_methodology = TRUE, show_interpretation = TRUE,
               show_analysis_summary = TRUE, show_plot_explanations = TRUE)
  expect_gt(sum(chars(on)), 20000)
  # each one individually, so a single panel silently going missing is caught
  for (nm in panels) expect_gt(unname(chars(on)[nm]), 0, label = nm)
})


test_that("the zero-cell table note survives with the summary panel switched off", {
  # The note used to be written by .generateSummary(); gating that generator on
  # show_analysis_summary would have taken a disclosure with it, so it moved
  # into .run().
  d <- af_data(); d$fn[2] <- 0
  r <- af_run(d, zero_cell_correction = "constant", show_analysis_summary = FALSE)
  expect_true(any(grepl("Zero-cell correction applied",
                        af_notes(r$bivariateresults), fixed = TRUE)))
})


test_that("a failed meta-regression would be explained, not just echoed", {
  # Was: the bare metafor message was the entire notice content ("Number of
  # parameters to be estimated is larger than the number of observations"),
  # with no statement of what is missing or what to do, and library messages
  # can carry newlines that a single-line notice must not.
  #
  # Asserted against the source, because the per-margin degrees-of-freedom
  # guard added in the same release now catches every input that used to reach
  # metafor's error - five candidate pathological datasets (k=3 with a 2-level
  # factor, k=4 with a 3-level factor, a NaN covariate, a collinear covariate,
  # k=5 with a 3-level factor) are all stopped earlier with a specific note.
  # The handler is defensive now, so a runtime test of it would be vacuous.
  #
  # This asserts CONTENT, not call shape: an earlier version pinned the exact
  # `sprintf(.("%s meta-regression failed"), measure)` spelling and broke when
  # that title became a two-branch if/else for translation reasons.
  src <- paste(readLines(testthat::test_path("..", "..", "R", "diagnosticmeta.b.R"),
                         warn = FALSE), collapse = "\n")

  # the framing sentence exists and names both what is lost and what is not
  expect_match(src, "meta-regression model could not be fitted", fixed = TRUE)
  expect_match(src, "the pooled estimates are unaffected", fixed = TRUE)

  # the library message is newline-stripped before it becomes a single-line notice
  expect_match(src, 'gsub("[\\r\\n]+", " ", trimws(conditionMessage(e)))', fixed = TRUE)

  # and the raw message is never the whole notice body on its own
  expect_false(grepl("meta-regression failed\"),\n                                e$message)", src, fixed = TRUE))
})


test_that("the bivariate table is cleared when its own option changes", {
  # Was: `bivariate_analysis` was missing from bivariateresults' clearWith.
  # jmvcore's Table$fromProtoBuf returns early - restoring nothing - only when
  # an option in clearWith changed; otherwise it restores the previous run's
  # CELLS. So unticking and reticking the box left the table holding the
  # previous fit's pooled estimates. (An earlier version of this comment blamed
  # a stale "disabled" note; that cannot happen - setNote() defaults to
  # init = TRUE and fromProtoBuf restores a note only `if (!note$init)`.)
  # Asserted against the .r.yaml source because the behaviour lives in
  # jmvcore/the client, not in this R code.
  ry <- readLines(testthat::test_path("..", "..", "jamovi", "diagnosticmeta.r.yaml"), warn = FALSE)
  start <- grep("^    - name: bivariateresults\\s*$", ry)
  expect_length(start, 1)
  nxt <- grep("^    - name: ", ry)
  stop_at <- min(nxt[nxt > start], length(ry))
  block <- ry[start:stop_at]
  expect_true(any(grepl("^\\s*- bivariate_analysis\\s*$", block)),
              info = "bivariate_analysis must appear in bivariateresults clearWith")
})


test_that("table note keys are stable ASCII, not translated labels", {
  # Was: paste0("small_sample_", tolower(measure)) with measure = .("Sensitivity"),
  # so in a Turkish session the note key became "small_sample_duyarlilik" -
  # note identity depending on the catalogue.
  d <- af_data()[1:7, ]
  d$fn[c(2, 3, 5, 6)] <- 0
  d$grp <- c("A", "A", "B", "B", "C", "C", "A")
  keys <- names(af_run(d, covariate = "grp", meta_regression = TRUE)$metaregression$notes)
  expect_true(all(grepl("^[a-z0-9_]+$", keys)), info = paste(keys, collapse = ", "))
  expect_true(any(keys %in% c("small_sample_sens", "overparameterised_sens",
                              "constant_sens", "insufficient_sens")))
})


test_that("an out-of-range method is refused before the backend sees it", {
  # The backend's own fallback for this was removed as unreachable; this test
  # is what makes that claim true rather than assumed.
  expect_error(af_run(method = "bogus"), "must be one of")
})


test_that(".run() yields to jamovi between the expensive fits", {
  # Was: no checkpoint anywhere in 3800 lines - 200 studies froze the UI for
  # about four seconds with no way to cancel. The calls must sit OUTSIDE the
  # dispatch tryCatch blocks: .checkpoint()'s restart is error-class, so a
  # tryCatch(error=) around one swallows it.
  src <- readLines(testthat::test_path("..", "..", "R", "diagnosticmeta.b.R"), warn = FALSE)
  hits <- grep("private$.checkpoint()", src, fixed = TRUE)
  expect_gte(length(hits), 6)
  # the line after each checkpoint must be blank or a comment starting a
  # dispatch block, never inside a tryCatch body
  expect_true(all(grepl("^\\s*$|^\\s*#", src[hits + 1])))
})


test_that("removing the dead scaffolding did not change any number", {
  # .validateStudyData, the unreachable method fallback and the inert cache
  # layer were deleted; the fitted results must be bit-identical to mada.
  skip_if_not_installed("mada")
  d <- af_data()
  md <- data.frame(TP = d$tp, FP = d$fp, FN = d$fn, TN = d$tn)
  up <- summary(mada::reitsma(md, method = "reml", correction = 0.5,
                              correction.control = "single"))$coefficients
  b <- af_run()$bivariateresults$asDF
  expect_equal(b$estimate[1] / 100, unname(up["sensitivity", "Estimate"]), tolerance = 1e-10)
  expect_equal(b$estimate[2] / 100, 1 - unname(up["false pos. rate", "Estimate"]),
               tolerance = 1e-10)
})


# --- findings from the six-lens adversarial review of the audit fixes -------

test_that("the funnel renderer survives an aliased slope", {
  # Was: every study the same effective sample size makes inv_root_ess constant,
  # so summary(lm)$coefficients is one row and pcf[2, 3] threw "subscript out of
  # bounds". The test path guarded it, the renderer did not - and the renderer
  # has no tryCatch, so the whole plot died.
  d <- data.frame(study = paste0("A", 1:6), tp = c(45, 44, 47, 43, 46, 42),
                  fn = 50 - c(45, 44, 47, 43, 46, 42),
                  fp = c(8, 9, 7, 10, 6, 11), tn = 100 - c(8, 9, 7, 10, 6, 11))
  a <- ClinicoPath:::diagnosticmetaClass$new(
    options = ClinicoPath:::diagnosticmetaOptions$new(
      study = "study", true_positives = "tp", false_positives = "fp",
      false_negatives = "fn", true_negatives = "tn",
      publication_bias = TRUE, funnel_plot = TRUE), data = d)
  suppressWarnings(a$run())
  fn <- ClinicoPath:::diagnosticmetaClass$private_methods$.funnelplot
  environment(fn) <- list2env(list(private = a$.__enclos_env__$private, self = a),
                              parent = environment(fn))
  f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
  on.exit({grDevices::dev.off(); unlink(f)}, add = TRUE)
  expect_true(fn(a$results$funnelplot, ggplot2::theme_grey(), NULL))
})


test_that("one zero variance component is not reported as two", {
  # Was: estimable <- all(diag(Psi) > 1e-4) is FALSE if EITHER is small, but the
  # else branch said "Both are effectively zero, so the studies are consistent
  # with a single common sensitivity and specificity" - printed directly under
  # its own "tau-squared (logit sensitivity) = 1.207". The mixed case is the
  # routine one in DTA: specificity pinned near 1, sensitivity heterogeneous.
  set.seed(5); k <- 14
  n1 <- sample(60:220, k, TRUE)
  se <- stats::plogis(stats::rnorm(k, stats::qlogis(0.80), 1.1))
  d <- data.frame(study = paste0("S", seq_len(k)), tp = stats::rbinom(k, n1, se),
                  fp = 5, tn = 495)
  d$fn <- n1 - d$tp
  vc <- af_notes(af_run(d)$bivariateresults)
  vc <- vc[grepl("variance components", vc, fixed = TRUE)]
  expect_length(vc, 1)
  expect_match(vc, "One of the two is effectively zero", fixed = TRUE)
  expect_false(grepl("Both are effectively zero", vc, fixed = TRUE))
  # and the correlation stays withheld, because it is still unidentifiable
  expect_false(grepl("Correlation between", vc, fixed = TRUE))
})


test_that("a below-chance PHM area is called model misfit, not a swapped column", {
  # Was: an ERROR notice reading "the pooled test performs worse than chance.
  # Check the TP/FP/FN/TN column assignment." theta > 1 puts the Lehmann curve
  # below the diagonal - that is this model failing, not a data-entry mistake -
  # and the converged-fit retry routes ordinary data here routinely.
  d <- data.frame(study = paste0("S", 1:6), tp = c(45, 2, 60, 22, 80, 5),
                  fp = c(10, 30, 30, 5, 2, 40), fn = c(5, 30, 2, 6, 25, 20),
                  tn = c(90, 10, 60, 60, 150, 20))
  txt <- af_notice(af_run(d, hsroc_analysis = TRUE))
  expect_match(txt, "does not describe these studies", fixed = TRUE)
  expect_match(txt, "pooled sensitivity and specificity above are unaffected", fixed = TRUE)
})


test_that("funnel_plot alone does not count as something being displayed", {
  # Was: shows_something listed the bare funnel_plot flag, but the item is
  # visible: (funnel_plot && publication_bias), so this state showed an empty
  # pane with no notice - the exact case the notice exists for.
  r <- af_run(bivariate_analysis = FALSE, funnel_plot = TRUE, publication_bias = FALSE)
  expect_match(af_notice(r), "Nothing selected to display", fixed = TRUE)
})


test_that("unticking a panel clears its content, not just its visibility", {
  # NO-REGRESSION GUARD, not a fail-before test: from R each run starts with a
  # fresh results object, so the defect this guards - jmvcore's
  # Html$fromProtoBuf restoring content unconditionally, leaving a
  # once-ticked panel's HTML in the results and in every later save - is only
  # reachable across runs inside jamovi. What this asserts is that the panels
  # are empty when their boxes are off, which the setContent("") else-branches
  # keep true.
  panels <- c("about", "interpretation", "summary", "forestplot_explanation",
              "srocplot_explanation", "funnelplot_explanation")
  off <- af_run()
  for (nm in panels)
    expect_equal(nchar(paste(off[[nm]]$content, collapse = "")), 0, label = nm)
})


# --- finding from the internal security review -----------------------------

test_that("study labels cannot inject formatting into the zero-cell table note", {
  # Was: table notes are NOT a plain-text sink - jamovi renders a small HTML
  # allow-list in them (i, em, b, strong, sub, sup) - and this note interpolated
  # study labels from the user's data raw. A study named "<sup>MARKER</sup>"
  # rendered as a superscript inside a caveat printed beside the pooled
  # estimates. Not XSS (script/img/handlers are outside the allow-list), but it
  # let crafted data reshape a clinical disclosure.
  lab <- c("<sup>MARKER</sup>", "<B >BOLD</B >", "<StRoNg>S</StRoNg>")
  d <- data.frame(study = c(lab, paste0("S", 4:12)),
                  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
                  fp = c(10,  8, 15,  5, 20,  4, 12,  9, 18,  6, 11,  7),
                  fn = c( 0,  0,  0,  3, 15,  2,  8,  6, 12,  4,  9,  5),
                  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
                  stringsAsFactors = FALSE)
  note <- af_notes(af_run(d, zero_cell_correction = "constant")$bivariateresults)
  note <- note[grepl("Zero-cell correction applied", note, fixed = TRUE)]
  expect_length(note, 1)

  # none of the six honoured tags survives, in any casing or with attributes
  expect_false(grepl("<[/[:space:]]*(i|em|b|strong|sub|sup)([[:space:]][^>]*)?>",
                     note, ignore.case = TRUE))
  # the label text itself is kept - this strips markup, it does not censor
  expect_match(note, "MARKER", fixed = TRUE)

  # and legitimate punctuation is NOT damaged
  d2 <- d; d2$study[1:3] <- c("Smith 2019 (a<b)", "Jones & Lee", "O'Brien \"2020\"")
  note2 <- af_notes(af_run(d2, zero_cell_correction = "constant")$bivariateresults)
  note2 <- note2[grepl("Zero-cell correction applied", note2, fixed = TRUE)]
  for (txt in c("a<b", "Jones & Lee", "O'Brien"))
    expect_match(note2, txt, fixed = TRUE)
})


# --- /fix-function pass: the 8 findings left open by the release-profile run ---

test_that("the copy-ready summary carries uncertainty, not bare point estimates", {
  # Was (D16): the one block a user pastes into a manuscript read "pooled
  # sensitivity of 81.7% and specificity of 89.2%" with no interval, no
  # confidence level and no prediction interval - while a STRONG_WARNING about
  # a 55-94% prediction interval sat in the notices panel.
  r <- af_run(show_analysis_summary = TRUE)
  raw <- paste(r$summary$content, collapse = " ")
  copy <- regmatches(raw, regexpr("data-text='[^']*'", raw, perl = TRUE))
  expect_length(copy, 1)
  expect_match(copy, "confidence level", fixed = TRUE)
  expect_match(copy, "%-", fixed = TRUE)                     # a CI range
  expect_match(copy, "predicted to fall", fixed = TRUE)      # the PI
})


test_that("accuracy bands are labelled descriptive, not fitness-for-use", {
  # Was (D17): "is classified as good for screening purposes" - a clinical
  # clearance no meta-analysis of accuracy alone can give, from bands with no
  # cited source.
  txt <- paste(af_run(show_analysis_summary = TRUE)$summary$content, collapse = " ")
  expect_false(grepl("for screening purposes", txt, fixed = TRUE))
  expect_false(grepl("for confirmatory testing", txt, fixed = TRUE))
  expect_match(txt, "band on the conventional 90/80/70 scale", fixed = TRUE)
  expect_match(txt, "not a fitness-for-use judgement", fixed = TRUE)
})


test_that("sparse data raises the normal-approximation caveat, ordinary data does not", {
  # Was (D22): the bivariate model is a normal approximation to the logit pair
  # and is biased with tiny cells (published gap vs a binomial GLMM: several
  # percentage points), with no caveat anywhere.
  sparse <- data.frame(study = paste0("S", 1:8), tp = c(9, 8, 7, 6, 9, 8, 7, 6),
                       fp = c(1, 0, 1, 1, 0, 1, 0, 1), fn = c(1, 0, 1, 0, 1, 0, 1, 0),
                       tn = c(12, 11, 10, 13, 12, 11, 10, 13))
  expect_match(af_notice(af_run(sparse)), "Sparse data", fixed = TRUE)
  # must not fire on ordinary data - cells of 2-4 are common and tolerable
  expect_false(grepl("Sparse data", af_notice(af_run()), fixed = TRUE))
})


test_that("the meta-regression table states its scale and reference level", {
  # Was (D28): coefficients on the logit scale with no label ("Intercept 95.26"
  # read like a percentage) and, for a factor covariate, no named reference.
  d <- af_data(); d$grade <- factor(rep(c("CT", "MRI", "US"), 4))
  n <- af_notes(af_run(d, covariate = "grade", meta_regression = TRUE)$metaregression)
  scale_note <- n[grepl("LOGIT scale", n, fixed = TRUE)]
  expect_length(scale_note, 1)
  expect_match(scale_note, "reference level 'CT'", fixed = TRUE)

  # a continuous covariate gets the per-unit wording instead
  d2 <- af_data(); d2$year <- 2001:2012
  n2 <- af_notes(af_run(d2, covariate = "year", meta_regression = TRUE)$metaregression)
  expect_true(any(grepl("per one-unit increase", n2, fixed = TRUE)))
})


test_that("the pooled table states its confidence level, k and N", {
  # Was (D29): five estimates with intervals and no statement of the level,
  # the number of studies, or the number of participants behind them.
  n <- af_notes(af_run(confidence_level = 90)$bivariateresults)
  scope <- n[grepl("Pooled from", n, fixed = TRUE)]
  expect_length(scope, 1)
  expect_match(scope, "12 studies", fixed = TRUE)
  expect_match(scope, "90% confidence intervals", fixed = TRUE)
  expect_match(scope, "1,789 participants", fixed = TRUE)
})


test_that("heterogeneity and PHM rows are scaffolded, not rebuilt each run", {
  # Was (D30): deleteRows() + addRow() in .run(), so the tables appeared empty
  # and visibly restructured on every run.
  r <- af_run(heterogeneity_analysis = TRUE, hsroc_analysis = TRUE)
  expect_equal(nrow(r$heterogeneity$asDF), 2)
  expect_equal(r$hsrocresults$asDF$parameter[3],
               "Area under the SROC curve (AUC = 1/(1 + theta))")

  # a margin that cannot be fitted leaves BLANK cells, not stale ones or a
  # missing row
  d <- af_data(); d$fn <- 0
  h <- af_run(d, heterogeneity_analysis = TRUE)$heterogeneity$asDF
  expect_equal(nrow(h), 2)
  expect_true(is.na(h$q_statistic[1]))
  expect_false(is.na(h$q_statistic[2]))
})
