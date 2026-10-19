# Regression tests for the diagnosticmeta findings of the 2026-10-19 module audit
# (OncoPath docs/audit/MODULE_AUDIT_REPORT_20261019-1535.md, "### diagnosticmeta").
# Each test fails on the backend as it stood before the fix.

au_data <- function() data.frame(
  study = paste0("S", 1:12),
  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
  fp = c(10,  8, 15,  5, 20,  4, 12,  9, 18,  6, 11,  7),
  fn = c( 5,  7, 10,  3, 15,  2,  8,  6, 12,  4,  9,  5),
  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
  stringsAsFactors = FALSE)

au_class <- function(d, ...) {
  a <- diagnosticmetaClass$new(options = diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", ...), data = d)
  a$init(noThrow = TRUE)
  if (!a$errored) a$run(noThrow = TRUE)
  a
}
au_notes <- function(tbl) vapply(tbl$notes, function(x) paste(x$note, collapse = " "), "")
au_notice_text <- function(a) gsub("\\s+", " ", gsub("<[^>]+>", " ",
                                                    paste(a$results$notices$content, collapse = " ")))
au_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
au_private <- function(a) a$.__enclos_env__$private


test_that("a bracketed study name keeps the whole zero-cell note (MEDIUM)", {
  # Was: setNote() translates its text and a catalog miss reads " [..." as a
  # msgctxt, so the note stopped at "(Smith " and lost the caution sentence.
  d <- au_data(); d$study[1] <- "Smith [2020]"; d$fn[1] <- 0
  note <- au_notes(au_class(d, zero_cell_correction = "constant")$results$bivariateresults)
  note <- note[grepl("Zero-cell correction applied", note, fixed = TRUE)]
  expect_length(note, 1)
  expect_match(note, "Smith (2020)", fixed = TRUE)
  expect_match(note, "Results should be interpreted with caution", fixed = TRUE)
})


test_that("covariate levels with brackets or '<' survive the meta-regression note and cells (MEDIUM/LOW)", {
  d <- au_data()
  # reference level "Grade [1]": the scale note used to end "... level 'Grade"
  d$grp <- factor(rep(c("Grade [1]", "Grade 2"), 6), levels = c("Grade [1]", "Grade 2"))
  a <- au_class(d, covariate = "grp", meta_regression = TRUE)
  scale <- au_notes(a$results$metaregression)
  scale <- scale[grepl("LOGIT scale", scale, fixed = TRUE)]
  expect_length(scale, 1)
  expect_match(scale, "against the reference level 'Grade (1)'", fixed = TRUE)
  expect_match(scale, "Specificity: each coefficient", fixed = TRUE)

  # a level "<Negative>" in the parameter cell used to render blank
  d$grp <- factor(rep(c("Positive", "<Negative>"), 6), levels = c("Positive", "<Negative>"))
  a <- au_class(d, covariate = "grp", meta_regression = TRUE)
  params <- a$results$metaregression$asDF$parameter
  expect_true(any(grepl("grp < Negative>", params, fixed = TRUE)))
  expect_false(any(grepl("<N", params, fixed = TRUE)))
})


test_that("a study name with '<' keeps its text in the individual-study cell (LOW)", {
  d <- au_data(); d$study[2] <- "Kim <LOD>"
  a <- au_class(d, show_individual_studies = TRUE)
  studies <- a$results$individualstudies$asDF$study
  expect_true("Kim < LOD>" %in% studies)
})


test_that("each exclusion reason is a whole sentence (LOW i18n-fragment)", {
  d <- au_data(); d$tp[3] <- NA; d$tp[5] <- d$tp[5] + 0.5
  txt <- au_notice_text(au_class(d))
  expect_match(txt, paste("2 of 12 studies were excluded before analysis. All results below are based",
                          "on the remaining 10 studies. Missing counts: 1 study/studies.",
                          "Fractional counts: 1 study/studies."), fixed = TRUE)
  expect_false(grepl("with missing counts", txt, fixed = TRUE))
})


test_that("the meta-regression 'not run' notes are whole sentences per margin (LOW i18n-fragment)", {
  d <- au_data()[1:4, ]
  d$grp <- factor(c("a", "b", "c", "d"))
  notes <- au_notes(suppressWarnings(au_class(d, covariate = "grp", meta_regression = TRUE))$results$metaregression)
  expect_true(any(startsWith(notes, "Sensitivity meta-regression not run: the 4 studies")))
  expect_true(any(startsWith(notes, "Specificity meta-regression not run: the 4 studies")))
})


test_that("the basic summary states its reason once and points to the table only when it is on (LOW)", {
  off <- au_class(au_data(), bivariate_analysis = FALSE, show_analysis_summary = TRUE,
                  show_individual_studies = FALSE)
  txt <- au_text(off$results$summary$content)
  reason <- "The bivariate model is switched off."
  expect_equal(lengths(regmatches(txt, gregexpr(reason, txt, fixed = TRUE))), 1L)
  expect_false(grepl("table below", txt, fixed = TRUE))

  on <- au_class(au_data(), bivariate_analysis = FALSE, show_analysis_summary = TRUE,
                 show_individual_studies = TRUE)
  expect_match(au_text(on$results$summary$content),
               "Individual study results are available in the table below", fixed = TRUE)
})


test_that("the copy-to-clipboard text is escaped for its single-quoted attribute (LOW)", {
  a <- au_class(au_data(), show_analysis_summary = TRUE)
  html <- a$results$summary$content
  attr_val <- regmatches(html, regexpr("data-text='[^']*'", html))
  expect_length(attr_val, 1)
  expect_match(attr_val, "pooled sensitivity", fixed = TRUE)
  # the whole sentence sits inside the quotes: nothing escaped the attribute
  expect_match(attr_val, "confidence level.", fixed = TRUE)
  expect_match(html, "Copy Summary to Clipboard", fixed = TRUE)
})


test_that("Deeks' funnel caption shows p < 0.001 and whole-sentence qualifiers (LOW)", {
  priv <- au_private(au_class(au_data()))
  expect_equal(priv$.deeksFitLabel(-5.1, 18, 1e-6, k = 20, zero_share = 0),
               "Deeks' asymmetry test: t = -5.10 on 18 df, p < 0.001")
  expect_equal(priv$.deeksFitLabel(-0.49, 18, 0.6286, k = 20, zero_share = 0),
               "Deeks' asymmetry test: t = -0.49 on 18 df, p = 0.629")
  expect_match(priv$.deeksFitLabel(1, 6, 0.2, k = 8, zero_share = 0),
               "p = 0.200. Descriptive only: k = 8 < 10 gives low statistical power.", fixed = TRUE)
  expect_match(priv$.deeksFitLabel(1, 18, 0.2, k = 20, zero_share = 0.5),
               "Descriptive only: too many zero cells", fixed = TRUE)
})


test_that("a summary error is not reported as a failed bivariate model (LOW robustness)", {
  # Was: .generateSummary() ran inside the bivariate tryCatch, so its error
  # became "Bivariate model failed" and nulled the fitted model.
  d <- au_data()
  a <- diagnosticmetaClass$new(options = diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", show_analysis_summary = TRUE), data = d)
  a$init()
  priv <- au_private(a)
  unlockBinding(".generateSummary", priv)
  priv$.generateSummary <- function(...) stop("summary rendering failed")
  a$run(noThrow = TRUE)
  expect_false(a$errored)
  titles <- vapply(priv$.noticeList, function(x) x$title, "")
  expect_false("Bivariate model failed" %in% titles)
  expect_true("Analysis summary unavailable" %in% titles)
  expect_false(is.null(priv$.biv_model))
  expect_true(any(!is.na(a$results$bivariateresults$asDF$estimate)))
})


test_that("no model is left for the SROC plot when the coefficients are missing (LOW robustness)", {
  # Force summary() of the fitted Reitsma model to fail. The generic is traced:
  # tracing mada's S3 method is not seen once that method has been dispatched.
  suppressMessages(trace("summary", where = baseenv(), print = FALSE,
                         tracer = quote(if (inherits(object, "reitsma")) stop("forced"))))
  on.exit(suppressMessages(untrace("summary", where = baseenv())), add = TRUE)
  a <- au_class(au_data())
  expect_true(any(grepl("coefficient matrix missing", au_notes(a$results$bivariateresults), fixed = TRUE)))
  expect_null(au_private(a)$.biv_model)
})


test_that("the funnel y axis label is a translatable string, not an expression (LOW i18n)", {
  src <- paste(deparse(body(au_private(au_class(au_data()))$.funnelplot)), collapse = "\n")
  expect_false(grepl("expression(1/sqrt(", src, fixed = TRUE))
  expect_match(src, "1 / sqrt(effective sample size)", fixed = TRUE)
})
