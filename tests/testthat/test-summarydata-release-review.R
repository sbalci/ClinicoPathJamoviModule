# Regression cover for the defects found during the summarydata release review.
# Each block fails against the pre-review backend.

library(testthat)

sd_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

test_that("the two panels never disagree about normality", {
  # The diagnostics text tested round(p, 3) > 0.05 while the copy-ready sentence
  # tested the exact p. A variable with p = 0.0501 was therefore reported as
  # "not consistent with a normal distribution" in one panel and "showed normal
  # distribution" in the other - both printing "p = 0.05". The copy-ready panel
  # is the one the user is invited to paste into a manuscript.
  #
  # seed 1676 gives shapiro p = 0.05010 on rnorm(40): just above 0.05, but 0.050
  # once rounded, so the two comparisons used to fall on opposite sides.
  set.seed(1676)
  d <- data.frame(marker = rnorm(40))
  p <- shapiro.test(d$marker)$p.value
  expect_gt(p, 0.05)
  expect_equal(round(p, 3), 0.05)

  res <- summarydata(data = d, vars = "marker", distr = TRUE, report_sentences = TRUE)
  diag <- sd_txt(res$text$content)
  sent <- sd_txt(res$reportSentences$content)

  # p is above 0.05, so neither panel may assert a departure from normality
  expect_match(diag, "consistent with a normal distribution")
  expect_false(grepl("are not consistent with a normal distribution", diag))
  expect_match(sent, "no evidence of departure from normality")
  expect_false(grepl("evidence of departure from normality \\(", sent) &&
               !grepl("no evidence", sent))
})

test_that("the copy-ready sentence does not claim normality was established", {
  # "Data showed normal distribution" overstates a non-significant test, and this
  # is text the user is told to paste into a manuscript.
  set.seed(11)
  d <- data.frame(marker = rnorm(80))
  sent <- sd_txt(summarydata(data = d, vars = "marker", distr = TRUE,
                             report_sentences = TRUE)$reportSentences$content)

  expect_false(grepl("showed normal distribution", sent))
  expect_match(sent, "no evidence of departure from normality")
})

test_that("a very small p-value is not reported as zero", {
  # round(2.8e-12, 3) is 0, and "Shapiro-Wilk p-value = 0" went into the
  # copy-ready sentence. A p-value is never zero.
  set.seed(3)
  d <- data.frame(marker = rexp(200, 0.1))
  expect_lt(shapiro.test(d$marker)$p.value, 0.001)

  res <- summarydata(data = d, vars = "marker", distr = TRUE, report_sentences = TRUE)
  raw_diag <- as.character(res$text$content)
  raw_sent <- as.character(res$reportSentences$content)

  expect_false(grepl("p-value = 0;", raw_diag, fixed = TRUE))
  expect_false(grepl("p = 0)", raw_sent, fixed = TRUE))

  # and the "<" is emitted as an entity. A literal "< 0.001" would also render
  # (a tag only opens when "<" is followed by a letter), so this guards the
  # encoding convention rather than a rendering bug.
  expect_match(raw_diag, "&lt; 0.001", fixed = TRUE)
  expect_match(raw_sent, "&lt; 0.001", fixed = TRUE)
  expect_false(grepl("= < 0.001", raw_diag, fixed = TRUE))
})

test_that("decimal_places applies to every panel, not just the summary", {
  # The summary honoured the option; the report sentences and the outlier report
  # were hard-coded to 2 and 3 decimals, so a user asking for 4 got three
  # different precisions across one output.
  set.seed(7)
  d <- data.frame(marker = c(rnorm(60), 12, -9))

  res <- summarydata(data = d, vars = "marker", decimal_places = 4,
                     outliers = TRUE, report_sentences = TRUE)
  mean4 <- formatC(mean(d$marker), format = "f", digits = 4)

  expect_match(sd_txt(res$text$content), mean4, fixed = TRUE)
  expect_match(sd_txt(res$reportSentences$content), mean4, fixed = TRUE)

  # outlier bounds at 4 dp too
  q <- quantile(d$marker, c(0.25, 0.75))
  expect_match(sd_txt(res$outlierReport$content),
               formatC(q[[1]] - 1.5 * diff(q), format = "f", digits = 4), fixed = TRUE)
})

test_that("precision is uniform within a line", {
  # round() drops trailing zeros, so "Mean 0.2574 +/- 2.146" showed 4 dp and 3 dp
  # in the same sentence.
  set.seed(7)
  d <- data.frame(marker = c(rnorm(60), 12, -9))
  out <- sd_txt(summarydata(data = d, vars = "marker", decimal_places = 4)$text$content)

  nums <- regmatches(out, gregexpr("[-0-9]+\\.[0-9]+", out))[[1]]
  decimals <- nchar(sub("^[-0-9]+\\.", "", nums))
  expect_true(all(decimals == 4),
              info = paste("decimal counts:", paste(unique(decimals), collapse = ", ")))
})

test_that("a single observation reports no SD rather than 'NA'", {
  res <- summarydata(data = data.frame(v = 5), vars = "v", report_sentences = TRUE)

  expect_match(sd_txt(res$text$content), "SD not defined")
  expect_false(grepl("± NA", sd_txt(res$text$content)))
  expect_false(grepl("NA", sd_txt(res$reportSentences$content)))
  # and no "1 observations"
  expect_false(grepl("1 observations", sd_txt(res$reportSentences$content)))
})

test_that("skewness and kurtosis match the moments package convention", {
  # moments reports g1 and b2, where a normal distribution has kurtosis 3 (not 0).
  # The glossary states "3 = normal", so the two must not drift apart.
  set.seed(3)
  d <- data.frame(marker = rexp(200, 0.1))
  out <- sd_txt(summarydata(data = d, vars = "marker", distr = TRUE)$text$content)

  x <- d$marker; n <- length(x); m <- mean(x)
  g1 <- (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5
  b2 <- (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2

  expect_match(out, sprintf("skewness = %s", round(g1, 2)), fixed = TRUE)
  expect_match(out, sprintf("kurtosis = %s", round(b2, 2)), fixed = TRUE)
  expect_match(sd_txt(summarydata(data = d, vars = "marker",
                                  distr = TRUE)$glossary$content),
               "3 = normal")
})

test_that("outlier bounds match a hand-computed IQR fence", {
  d <- data.frame(v = c(1:20, 100))
  out <- sd_txt(summarydata(data = d, vars = "v", outliers = TRUE)$outlierReport$content)

  q <- quantile(d$v, c(0.25, 0.75))
  lo <- q[[1]] - 1.5 * diff(q)
  hi <- q[[2]] + 1.5 * diff(q)

  expect_match(out, formatC(lo, format = "f", digits = 2), fixed = TRUE)
  expect_match(out, formatC(hi, format = "f", digits = 2), fixed = TRUE)
  expect_match(out, "1 outlier(s) detected", fixed = TRUE)
  expect_match(out, formatC(100, format = "f", digits = 2), fixed = TRUE)
})
