# End-to-end variable-name safety for multisurvival (/check-function, 2026-09-15).
#
# The formula-level helpers already have unit tests for backtick escaping; this
# runs the whole analysis on column names with spaces, punctuation and a
# non-ASCII letter, which is what jamovi users actually import. Before the
# .executeAnalysis() catch-all was removed, a failure here would have been hidden
# in the todo panel; now it raises and the test fails.

test_that("multisurvival runs on spaced, punctuated and non-ASCII variable names", {
  set.seed(3)
  n <- 160
  d <- data.frame(
    fu     = round(stats::rexp(n, 0.05), 1) + 0.1,
    status = factor(sample(c("Alive", "Dead"), n, TRUE, prob = c(0.4, 0.6))),
    grade  = factor(sample(c("G1", "G2", "G3"), n, TRUE)),
    age    = round(stats::rnorm(n, 60, 10)),
    stringsAsFactors = FALSE
  )
  names(d) <- c("Follow-up (months)", "Vital Status", "Tumor Grade", "Ya\u015f")

  res <- .run_multisurvival(
    data = d,
    elapsedtime = "Follow-up (months)",
    outcome = "Vital Status",
    outcomeLevel = "Dead",
    explanatory = "Tumor Grade",
    contexpl = "Ya\u015f"
  )

  expect_s3_class(res, "multisurvivalResults")
  cox_text <- as.character(res$text$content)
  expect_false(grepl("Error generating Cox regression table", cox_text, fixed = TRUE))
  expect_match(cox_text, "G3", fixed = TRUE)
})

test_that("a one-valued explanatory variable is rejected in plain language", {
  set.seed(4)
  n <- 80
  d <- data.frame(
    fu = round(stats::rexp(n, 0.05), 1) + 0.1,
    status = factor(sample(c("Alive", "Dead"), n, TRUE)),
    arm = factor(rep("Same", n))
  )
  expect_error(
    .run_multisurvival(data = d, elapsedtime = "fu", outcome = "status",
                       outcomeLevel = "Dead", explanatory = "arm"),
    "has only one value in the analysed rows"
  )
})
