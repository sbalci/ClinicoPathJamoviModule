# Release review of statsplot2.
#
# The arithmetic is delegated to ggstatsplot and is sound. The risk was in what
# the analysis reported about its own inputs: an inflated N, a "successful"
# one-group comparison, a silently reclassified constant outcome, and - worst -
# statistics computed on a random subsample that the panel never mentioned.

sp_notices <- function(res)
  # Do NOT strip "<...>" here: the notices output is Preformatted PLAIN TEXT, and
  # a tag-stripping regex eats everything between a "<" and the next ">" - e.g.
  # "recommended for n<30 ... Required: >=2 valid values" collapses into
  # "recommended for n=2 valid values", merging two notices.
  gsub("[[:space:]]+", " ", paste(as.character(res$notices$content), collapse = " "))

sp_big <- function(n = 30000, seed = 1) {
  set.seed(seed)
  data.frame(y = rnorm(n), g = factor(sample(c("A", "B"), n, TRUE)))
}


# ---- the subsample must announce itself ------------------------------------

test_that("random subsampling is disclosed, not left looking like missing data", {
  # The explanation went to message(), i.e. the R console, which a jamovi user
  # never sees. The panel said only "Observations used: 5,000 of 30,000", which
  # reads as NA exclusion. It matters because every statistic below is computed
  # on the subsample: measured over 300 replicates at d = 0.05 with n = 30,000,
  # full-data power is 99.7% (median p ~ 0.0000) against 45.3% on the 5,000-row
  # draw (median p = 0.0716) - a near-certain detection becomes a coin flip.
  n <- sp_notices(statsplot2(data = sp_big(), dep = "y", group = "g", sampleLarge = TRUE))
  expect_match(n, "random subsample", ignore.case = TRUE)
  expect_match(n, "drawn at RANDOM")
  expect_match(n, "lowers power")
  expect_match(n, "RANDOM SUBSAMPLE - see warning above")
})

test_that("no subsample warning when the full data is used", {
  n <- sp_notices(statsplot2(data = sp_big(), dep = "y", group = "g", sampleLarge = FALSE))
  expect_false(grepl("random subsample", n, ignore.case = TRUE))
  expect_match(n, "Observations used: 30,000 of 30,000")
})

test_that("the sampling threshold and size are under user control", {
  # Both were hard-coded at 10,000 / 5,000, so a user who wanted to keep 20,000
  # rows had only the all-or-nothing switch.
  skip_if_not(all(c("sampleThreshold", "sampleSize") %in% names(formals(statsplot2))),
              "new options not compiled yet - run jmvtools::prepare()")
  d <- sp_big()
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE)),
               "Observations used: 5,000")                       # defaults unchanged
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleSize = 20000)),
               "Observations used: 20,000")
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleThreshold = 50000)),
               "Observations used: 30,000 of 30,000")
  # asking to keep more rows than exist must not error or over-report
  expect_match(sp_notices(statsplot2(data = d, dep = "y", group = "g",
                                     sampleLarge = TRUE, sampleSize = 999999)),
               "Observations used: 30,000 of 30,000")
})


# ---- the reported N ---------------------------------------------------------

test_that("N counts usable observations, not rows", {
  # With `Exclude missing values` off (the default) missing values stay in the
  # frame and ggstatsplot drops them, so nrow() over-reported: 180 rows with 155
  # usable outcomes was announced as "Observations used: 180 of 180".
  data(statsplot2_test)
  d <- statsplot2_test
  d$tumor_reduction[1:20] <- NA
  usable <- sum(stats::complete.cases(d[, c("tumor_reduction", "treatment")]))
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, sprintf("Observations used: %d of %d", usable, nrow(d)))
  expect_match(n, "omitted from the statistics")
})


# ---- setups that cannot mean anything ---------------------------------------

test_that("a one-group comparison is rejected, not called successful", {
  data(statsplot2_test)
  d <- statsplot2_test
  d$treatment <- "Placebo"
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, "Only one group to compare")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("a constant outcome is flagged, including the analysis-type switch", {
  # A constant numeric has one unique value, so the automatic plot selection
  # reads it as a FACTOR: the analysis silently changes from
  # independent_factor_continuous to independent_factor_factor.
  data(statsplot2_test)
  d <- statsplot2_test
  d$tumor_reduction <- 50
  n <- sp_notices(statsplot2(data = d, dep = "tumor_reduction", group = "treatment"))
  expect_match(n, "Outcome has no variation")
  expect_match(n, "changes the analysis type")
  expect_false(grepl("completed successfully", n, fixed = TRUE))
})

test_that("an ordinary analysis still reports success", {
  data(statsplot2_test)
  n <- sp_notices(statsplot2(data = statsplot2_test, dep = "tumor_reduction",
                             group = "treatment"))
  expect_match(n, "completed successfully")
  expect_false(grepl("Only one group", n, fixed = TRUE))
  expect_false(grepl("no variation", n, fixed = TRUE))
})


# ---- shipped data must load by its own name ---------------------------------

test_that("statsplot2 datasets load under the names they are documented by", {
  # data(foo) loads data/foo.rda and creates whatever objects are inside it. When
  # those differ, data(foo) succeeds and `foo` still does not exist:
  # statsplot2_repeated.rda held `repeated_measures_data` and
  # statsplot2_clinical.rda held `clinical_trial_data`.
  for (n in c("statsplot2_test", "statsplot2_repeated", "statsplot2_clinical",
              "statsplot2_skewed", "statsplot2_outliers")) {
    e <- new.env()
    data(list = n, package = "ClinicoPath", envir = e)
    expect_true(exists(n, envir = e), info = n)
  }
})
