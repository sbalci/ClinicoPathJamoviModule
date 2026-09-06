# Regression cover for the 2026-09 audit findings on reportcat.
# Each block fails against the pre-audit backend.

library(testthat)

rc_private <- function(data, vars) {
  a <- ClinicoPath:::reportcatClass$new(
    options = ClinicoPath:::reportcatOptions$new(vars = vars),
    data = data)
  a$.__enclos_env__$private
}

test_that("variable names and levels are escaped in the gtExtras summary table", {
  # gtExtras::gt_plt_summary() injects column names and factor levels into the
  # overview as raw HTML, so gt's body-cell escaping did not apply: a column
  # named <img src=x onerror=alert(1)> was emitted verbatim (verified live).
  d <- data.frame(check.names = FALSE,
                  `<img src=x onerror=alert(1)>` = factor(c("<b>lvl</b>", "a", "a")))
  res <- ClinicoPath::reportcat(data = d, vars = "<img src=x onerror=alert(1)>")

  for (item in c("text1", "text", "clinicalSummary", "reportSentences", "assumptions")) {
    html <- as.character(res[[item]]$content)
    expect_false(grepl("<img", html, fixed = TRUE), label = item)
    expect_false(grepl("<b>lvl", html, fixed = TRUE), label = item)
  }
  html <- as.character(res$text1$content)
  expect_true(grepl("&lt;img", html, fixed = TRUE))
  expect_true(grepl("&lt;b&gt;lvl", html, fixed = TRUE))
  expect_false(grepl("&amp;lt;", html, fixed = TRUE))   # escaped once, not twice
})

test_that("the summary table keeps the original variable name", {
  # as.data.frame(lapply()) used check.names = TRUE, so the Summary Table read
  # "Tumor.Grade" under a Variable Summaries panel reading "Tumor Grade".
  d <- data.frame(check.names = FALSE,
                  `Tumor Grade` = factor(c("G1", "G2", "G3", "G2")),
                  `ER & PR` = c("pos", "neg", "pos", "pos"))
  res <- ClinicoPath::reportcat(data = d, vars = c("Tumor Grade", "ER & PR"))
  html <- as.character(res$text1$content)

  expect_true(grepl("Tumor Grade", html, fixed = TRUE))
  expect_false(grepl("Tumor.Grade", html, fixed = TRUE))
  expect_true(grepl("ER &amp; PR", html, fixed = TRUE))
  expect_false(grepl("ER &amp;amp; PR", html, fixed = TRUE))
  # summaries still use the raw name for the data[[ ]] lookup
  expect_match(as.character(res$text$content), "G2: n = 2")
})

test_that("the gt fallback table escapes names and level labels exactly once", {
  # Reachable only when gt_plt_summary() errors, so it is driven directly.
  d <- data.frame(check.names = FALSE,
                  `<b>v</b> & w` = factor(c("<i>x</i>", "<i>x</i>", "y")))
  p <- rc_private(d, "<b>v</b> & w")
  html <- as.character(p$.gtExtras_style_fallback_cat(d, "<b>v</b> & w", "boom"))

  expect_false(grepl("<b>v</b>", html, fixed = TRUE))
  expect_false(grepl("<i>x</i>", html, fixed = TRUE))
  expect_true(grepl("&lt;b&gt;v&lt;/b&gt; &amp; w", html, fixed = TRUE))
  expect_true(grepl("&lt;i&gt;x&lt;/i&gt;", html, fixed = TRUE))
  expect_false(grepl("&amp;lt;", html, fixed = TRUE))
  # the reason the visual summary was replaced is now shown
  expect_match(html, "could not be produced")
  expect_match(html, "Reason: boom")

  html2 <- as.character(p$.create_simple_cat_summary_table(d, "<b>v</b> & w", "<script>"))
  expect_false(grepl("<b>v</b>", html2, fixed = TRUE))
  expect_false(grepl("<script>", html2, fixed = TRUE))
  expect_match(html2, "Reason: &lt;script&gt;")
})

test_that("fatal validation rejects instead of hiding the results", {
  # .resetOutputs() used to setVisible(FALSE) every panel before the checks, so
  # a fatal condition collapsed the pane to a lone error panel. reject() shows
  # the message and leaves the previous results in place.
  d <- data.frame(allna = factor(rep(NA_character_, 3), levels = c("X", "Y")),
                  g = factor(c("A", "B", "B")))
  expect_error(ClinicoPath::reportcat(data = d[0, , drop = FALSE], vars = "g"), "no rows")
  expect_error(ClinicoPath::reportcat(data = d, vars = "allna"), "allna")

  # not fatal: one usable variable remains, the empty one is named
  res <- ClinicoPath::reportcat(data = d, vars = c("allna", "g"))
  expect_true(res$dataWarnings$visible)
  expect_match(as.character(res$dataWarnings$content), "allna")
  expect_true(res$text1$visible)
})
