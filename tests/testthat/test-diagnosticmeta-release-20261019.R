# Regression tests for the diagnosticmeta pre-release review of 2026-10-19
# (DM-1..DM-10; evidence: repro_DM-*.R and sim_*.R of that review, and the gate
# calibration gate_calib.R). Each test fails on the backend as it stood before
# the fix. The estimators were checked against mada/metafor and are unchanged;
# these are disclosure and wording fixes.

rr_data <- function() data.frame(
  study = paste0("S", 1:12),
  tp = c(45, 30, 60, 22, 80, 15, 55, 38, 70, 25, 50, 33),
  fp = c(10,  8, 15,  5, 20,  6, 12,  9, 18,  6, 11,  7),
  fn = c( 5,  7, 10,  6, 15,  5,  8,  6, 12,  5,  9,  5),
  tn = c(90, 70,120, 45,160, 35,100, 80,140, 55, 95, 65),
  stringsAsFactors = FALSE)

rr_run <- function(d = rr_data(), ...) {
  a <- diagnosticmetaClass$new(options = diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn", ...), data = d)
  a$init(noThrow = TRUE)
  if (!a$errored) a$run(noThrow = TRUE)
  a
}
rr_notes <- function(a) vapply(a$results$bivariateresults$notes,
                               function(x) paste(x$note, collapse = " "), "")
rr_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
rr_notices <- function(a) rr_text(a$results$notices$content)
rr_summary <- function(a) rr_text(a$results$summary$content)


test_that("DM-1: cells of 2-4 in a quarter of the studies raise the sparse-data warning", {
  # Was: the gate needed a cell of 0 or 1, so a high-sensitivity set with 2-4
  # false negatives per study - where the nominal 95% CI covered about 80% in
  # simulation - showed no caution.
  d <- rr_data(); d$fn[1:4] <- c(2, 3, 4, 3)
  txt <- rr_notices(rr_run(d))
  expect_match(txt, "Sparse data", fixed = TRUE)
  expect_match(txt, "4 of 12 studies have a cell smaller than 5", fixed = TRUE)
  expect_match(txt, "about 80%", fixed = TRUE)
  # the 0-or-1 tier keeps its own wording and now quantifies coverage too
  d$fn[1:4] <- c(0, 1, 1, 0)
  txt <- rr_notices(rr_run(d))
  expect_match(txt, "4 of 12 studies have a cell of 0 or 1", fixed = TRUE)
  expect_match(txt, "about 70%", fixed = TRUE)
  # all cells of 5 or more: no warning
  expect_false(grepl("Sparse data", rr_notices(rr_run()), fixed = TRUE))
})


test_that("DM-2: no false positives in any study is disclosed and LR+ is not graded", {
  # Was: specificity 99.5% (99.0-99.7) and "LR+ = ... strong evidence" with
  # nothing saying both were set by the +0.5 correction.
  d <- rr_data(); d$fp <- 0
  a <- rr_run(d, show_analysis_summary = TRUE)
  txt <- rr_notices(a)
  expect_match(txt, "Specificity set by the continuity correction", fixed = TRUE)
  expect_match(txt, "12 of 12 studies have no false positives", fixed = TRUE)
  bound <- sprintf("%.2f%%", 100 * (1 - stats::qbeta(0.975, 1, sum(d$tn))))
  expect_match(txt, bound, fixed = TRUE)
  expect_true(any(grepl("rest on the continuity correction", rr_notes(a), fixed = TRUE)))
  s <- rr_summary(a)
  expect_false(grepl("provides strong evidence FOR disease", s, fixed = TRUE))
  expect_false(grepl("(the SpPin pattern)", s, fixed = TRUE))
  expect_match(s, "so it is not graded", fixed = TRUE)
  # the mirror case: no false negatives
  d <- rr_data(); d$fn <- 0
  a <- rr_run(d, show_analysis_summary = TRUE)
  expect_match(rr_notices(a), "Sensitivity set by the continuity correction", fixed = TRUE)
  expect_false(grepl("(the SnNout pattern)", rr_summary(a), fixed = TRUE))
  # ordinary data: neither
  txt <- rr_notices(rr_run())
  expect_false(grepl("set by the continuity correction", txt, fixed = TRUE))
})


test_that("DM-3: '+1/N to all cells' warns that it down-weights zero-cell studies", {
  # Was: the only notice said a correction was applied; in simulation its CI
  # covered 31-55% and it biased the pooled accuracy downward.
  d <- rr_data(); d$fn[1:2] <- 0
  txt <- rr_notices(rr_run(d, zero_cell_correction = "reciprocal_n"))
  expect_match(txt, "almost no weight", fixed = TRUE)
  expect_match(txt, "31-55%", fixed = TRUE)
  expect_false(grepl("almost no weight", rr_notices(rr_run(d)), fixed = TRUE))
  path <- testthat::test_path("..", "..", "jamovi", "diagnosticmeta.a.yaml")
  skip_if_not(file.exists(path), "jamovi yaml not available in the installed test context")
  opts <- yaml::read_yaml(path)$options
  desc <- opts[[which(vapply(opts, function(o) identical(o$name, "zero_cell_correction"), TRUE))]]$description$R
  expect_match(desc, "almost no weight", fixed = TRUE)
  expect_match(desc, "fit the same models", fixed = TRUE)
})


# The review's own case (repro_DM-2.R): the bundled set with no false
# positives anywhere, specificity 99.47%.
rr_bundled_fp0 <- function() {
  e <- new.env()
  utils::data("diagnosticmeta_test", package = "ClinicoPath", envir = e)
  d <- as.data.frame(e$diagnosticmeta_test)
  names(d)[1:5] <- c("study", "tp", "fp", "fn", "tn")
  d$fp <- 0
  d
}


test_that("DM-4: a specificity above 99% is not reported as 0 false alarms per 100", {
  # Was: "identifies 100 out of 100 healthy individuals" and "only 0 out of 100
  # ... will test positive" beside a table showing 99.5%.
  s <- rr_summary(rr_run(rr_bundled_fp0(), show_analysis_summary = TRUE))
  expect_match(s, "Specificity: 99.5%", fixed = TRUE)
  expect_false(grepl("only 0 out of 100", s, fixed = TRUE))
  expect_false(grepl("identifies 100 out of 100 healthy", s, fixed = TRUE))
  expect_match(s, "only 5 out of 1,000 healthy individuals", fixed = TRUE)
  expect_match(s, "identifies 995 out of 1,000 healthy individuals", fixed = TRUE)
})


test_that("DM-5: a near-zero variance is not shown as 0.000 beside a correlation of 1.00", {
  # Was: "tau-squared (logit false-positive rate) = 0.000" followed by
  # "Correlation ... = 1.00 ... which with few studies ..." at k = 20.
  note <- rr_notes(rr_run(rr_bundled_fp0()))
  note <- note[grepl("Between-study variance", note, fixed = TRUE)]
  expect_length(note, 1)
  expect_false(grepl("= 0\\.000[,.]", note))
  expect_match(note, "false-positive rate) = 0.000335", fixed = TRUE)
  expect_false(grepl("Correlation between logit sensitivity", note, fixed = TRUE))
  expect_match(note, "One of the two is effectively zero", fixed = TRUE)
  expect_false(grepl("with few studies", note, fixed = TRUE))
})


test_that("DM-6: the zero-cell note shows that more studies were corrected than it names", {
  d <- rr_data(); d$fn[1:5] <- 0
  note <- rr_notes(rr_run(d, zero_cell_correction = "constant"))
  note <- note[grepl("Zero-cell correction applied", note, fixed = TRUE)]
  expect_length(note, 1)
  expect_match(note, "to 5 of 12 studies (S1, S2, S3, ...)", fixed = TRUE)
})


test_that("DM-10: the default and '+0.5 to all cells' are said to fit the same models", {
  d <- rr_data(); d$fn[1:2] <- 0
  expect_true(any(grepl("+0.5 to the zero cells only", rr_notes(rr_run(d)), fixed = TRUE)))
  note <- rr_notes(rr_run(d, zero_cell_correction = "constant"))
  expect_true(any(grepl("same models as the default", note, fixed = TRUE)))
})
