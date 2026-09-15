# Interpretation safeguards found in the release review (2026-09-15).
#
# Within-subgroup HRs and per-covariate likelihood-ratio tests are unadjusted,
# and subgroup p-values invite "significant here, not there, so the effect
# differs". The clinical summary also labelled the complete-case analysis set
# "Total patients" next to a larger dataset.

.in_quiet <- function(expr) {
  f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}
.in_notes <- function(tbl) vapply(tbl$notes, function(n) as.character(n$note), character(1))
.in_data <- function() {
  data(multisurvival_test, package = "ClinicoPath", envir = environment())
  d <- multisurvival_test
  d$treatment <- factor(d$treatment)
  d$stage <- factor(d$stage)
  d
}

test_that("within-subgroup hazard ratios are labelled exploratory and unadjusted", {
  res <- .in_quiet(.run_multisurvival(
    data = .in_data(), elapsedtime = "elapsedtime", outcome = "outcome",
    explanatory = "treatment", contexpl = "age", interactions = list(c("treatment", "age"))))
  expect_gt(res$subgroupHR$rowCount, 0)
  expect_true(any(grepl("not adjusted for multiple comparisons", .in_notes(res$subgroupHR), fixed = TRUE)))
  expect_true(any(grepl("interaction test", .in_notes(res$subgroupHR), fixed = TRUE)))
})

test_that("covariate-contribution rows are flagged as separate unadjusted tests", {
  res <- .in_quiet(.run_multisurvival(
    data = .in_data(), elapsedtime = "elapsedtime", outcome = "outcome",
    explanatory = c("treatment", "stage"), contexpl = "age", compare_models = TRUE))
  expect_gt(res$modelContributionTable$rowCount, 0)
  expect_true(any(grepl("not adjusted for multiple comparisons", .in_notes(res$modelContributionTable), fixed = TRUE)))
})

test_that("the clinical summary names the analysed count, not the dataset size", {
  d <- .in_data()
  set.seed(5)
  d$age[sample(nrow(d), 60)] <- NA
  res <- .in_quiet(.run_multisurvival(
    data = d, elapsedtime = "elapsedtime", outcome = "outcome",
    explanatory = "treatment", contexpl = "age"))
  txt <- as.character(res$text$content)
  expect_match(txt, "Patients in the model: 140", fixed = TRUE)
  expect_false(grepl("Total patients:", txt, fixed = TRUE))
})
