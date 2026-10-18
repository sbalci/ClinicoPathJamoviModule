# 'Always Fisher's exact' replaced the p-value in the omnibus row, but the post-hoc
# gate, the clinical summary, the copy-ready sentences, the export and the analysis
# summary still read the Pearson p-value. The two fixtures below put Pearson and
# Fisher on opposite sides of alpha = 0.05 (3 x 2 tables, n = 20), so every panel
# must follow the p-value the table shows.

library(testthat)

fisher_cq_data <- function(counts) {
  cells <- expand.grid(cols = c("C1", "C2"), rows = c("R1", "R2", "R3"))
  data.frame(rows = factor(rep(cells$rows, counts)),
             cols = factor(rep(cells$cols, counts)))
}

fisher_cq <- function(d, ...) chisqposttest(data = d, rows = "rows", cols = "cols",
                                            counts = NULL, posthoc = "bonferroni",
                                            showClinicalSummary = TRUE,
                                            copyReadySentences = TRUE,
                                            exportResults = TRUE, ...)

test_that("Fisher not significant, Pearson significant: the gate stays closed everywhere", {
  d <- fisher_cq_data(c(0, 3, 3, 4, 0, 10))
  tab <- table(d$rows, d$cols)
  pearson_p <- suppressWarnings(chisq.test(tab, correct = FALSE))$p.value
  fisher_p <- fisher.test(tab)$p.value
  expect_lt(pearson_p, 0.05)
  expect_gt(fisher_p, 0.05)

  res <- fisher_cq(d, testSelection = "fisher")
  expect_equal(res$chisqTable$asDF$p, fisher_p, tolerance = 1e-10)
  expect_equal(nrow(res$posthocTable$asDF), 0L)
  expect_match(as.character(res$multipleTestingInfo$content),
               "overall Fisher(&#39;|')s exact test is not significant")

  summary_html <- as.character(res$clinicalSummary$content)
  expect_match(summary_html, "not statistically significant", fixed = TRUE)
  expect_match(summary_html, "Fisher(&#39;|')s exact test, p = 0.076")

  sentences <- as.character(res$reportSentences$content)
  expect_match(sentences, "could not be rejected", fixed = TRUE)
  expect_match(sentences, "Fisher(&#39;|')s exact test was performed")
  expect_false(grepl("A chi-square test of independence", sentences, fixed = TRUE))

  export <- res$exportTable$asDF
  p_row <- export[export$measure == "P-value (Fisher's exact test)", ]
  expect_equal(nrow(p_row), 1L)
  expect_match(p_row$interpretation, "No statistically significant association", fixed = TRUE)

  notices <- res$notices$content
  expect_match(notices, "Fisher's exact test, p = 0.076 at alpha = 0.05; no pairwise comparisons (overall test not significant)", fixed = TRUE)
  # the sparse-cell warnings must not call the Fisher p-value unreliable or ask
  # the user to select the test already in use
  expect_false(grepl("too low for the chi-square approximation", notices, fixed = TRUE))
  expect_false(grepl("Some expected counts are below 5", notices, fixed = TRUE))
  expect_match(notices, "The overall p-value is from Fisher's exact test", fixed = TRUE)
})

test_that("Fisher significant, Pearson not significant: the gate opens with Fisher only", {
  d <- fisher_cq_data(c(1, 2, 3, 4, 0, 10))
  tab <- table(d$rows, d$cols)
  pearson_p <- suppressWarnings(chisq.test(tab, correct = FALSE))$p.value
  fisher_p <- fisher.test(tab)$p.value
  expect_gt(pearson_p, 0.05)
  expect_lt(fisher_p, 0.05)

  fis <- fisher_cq(d, testSelection = "fisher")
  expect_gt(nrow(fis$posthocTable$asDF), 0L)
  expect_match(as.character(fis$clinicalSummary$content), "was statistically significant", fixed = TRUE)
  expect_match(as.character(fis$reportSentences$content), "was rejected", fixed = TRUE)
  expect_match(fis$notices$content, "Fisher's exact test, p = 0.045 at alpha = 0.05", fixed = TRUE)

  # the same data under the chi-square selection keeps the Pearson decision
  chi <- fisher_cq(d, testSelection = "chisquare")
  expect_equal(chi$chisqTable$asDF$p, pearson_p, tolerance = 1e-10)
  expect_equal(nrow(chi$posthocTable$asDF), 0L)
  expect_match(chi$notices$content, "Pearson chi-square without continuity correction, p = 0.077", fixed = TRUE)
})

test_that("a 2x2 table under a forced Fisher test carries no continuity-correction note", {
  two <- data.frame(rows = factor(rep(c("A", "B"), each = 40)),
                    cols = factor(c(rep(c("X", "Y"), c(30, 10)), rep(c("X", "Y"), c(10, 30)))))
  has_cc_note <- function(res) !is.null(res$chisqTable$notes[["continuity"]]$note)
  expect_true(has_cc_note(chisqposttest(data = two, rows = "rows", cols = "cols")))
  expect_false(has_cc_note(chisqposttest(data = two, rows = "rows", cols = "cols",
                                         testSelection = "fisher")))
})
