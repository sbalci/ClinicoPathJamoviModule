# Regression cover for the defects found during the summarydata release review.
# Each block fails against the pre-review backend.

library(testthat)

sd_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

test_that("the two panels never disagree about normality", {
  # The diagnostics text tested round(p, 3) > 0.05 while the draft sentence
  # tested the exact p. A variable with p = 0.0501 was therefore reported as
  # "not consistent with a normal distribution" in one panel and "showed normal
  # distribution" in the other - both printing "p = 0.05". The draft panel is
  # intended to help users prepare manuscript text.
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

  # The printed value must not round back onto the 0.050 decision boundary.
  expect_match(diag, "p-value = 0.0501", fixed = TRUE)
  expect_match(sent, "p = 0.0501", fixed = TRUE)
  expect_false(grepl("p-value = 0.050;", diag, fixed = TRUE))
})

test_that("the draft sentence does not claim normality was established", {
  # "Data showed normal distribution" overstates a non-significant test.
  set.seed(11)
  d <- data.frame(marker = rnorm(80))
  sent <- sd_txt(summarydata(data = d, vars = "marker", distr = TRUE,
                             report_sentences = TRUE)$reportSentences$content)

  expect_false(grepl("showed normal distribution", sent))
  expect_match(sent, "no evidence of departure from normality")
})

test_that("a very small p-value is not reported as zero", {
  # round(2.8e-12, 3) is 0, and "Shapiro-Wilk p-value = 0" went into the
  # draft sentence. A p-value is never zero.
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
  expect_match(sd_txt(res$text1$content), mean4, fixed = TRUE)
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
                                  distr = TRUE,
                                  show_guidance = TRUE)$glossary$content),
               "3 = normal")
})

test_that("large samples retain moments when Shapiro-Wilk is unavailable", {
  d <- data.frame(marker = (seq_len(5001) / 5001)^3)
  out <- sd_txt(summarydata(data = d, vars = "marker", distr = TRUE)$text$content)

  expect_match(out, "between 3 and 5000", fixed = TRUE)
  expect_match(out, "skewness = 1.06", fixed = TRUE)
  expect_match(out, "kurtosis = 2.91", fixed = TRUE)
  expect_false(grepl("Shapiro-Wilk p-value", out, fixed = TRUE))
})

test_that("outlier bounds match a hand-computed IQR fence", {
  d <- data.frame(v = c(1:20, 100))
  out <- sd_txt(summarydata(data = d, vars = "v", outliers = TRUE)$outlierReport$content)

  q <- quantile(d$v, c(0.25, 0.75))
  lo <- q[[1]] - 1.5 * diff(q)
  hi <- q[[2]] + 1.5 * diff(q)

  expect_match(out, formatC(lo, format = "f", digits = 2), fixed = TRUE)
  expect_match(out, formatC(hi, format = "f", digits = 2), fixed = TRUE)
  expect_match(out, "1 observation(s) flagged", fixed = TRUE)
  expect_match(out, formatC(100, format = "f", digits = 2), fixed = TRUE)
})

test_that("IQR screening is not presented as an expected or reference range", {
  d <- data.frame(v = c(1:20, 100))
  res <- summarydata(data = d, vars = "v", outliers = TRUE,
                     show_guidance = TRUE)
  report <- sd_txt(res$outlierReport$content)
  clinical <- sd_txt(res$clinicalInterpretation$content)

  expect_match(report, "IQR fences", fixed = TRUE)
  expect_false(grepl("expected range", report, ignore.case = TRUE))
  expect_false(grepl("reference range validation", clinical, ignore.case = TRUE))
  expect_false(grepl("statistical assumption verification", clinical,
                     ignore.case = TRUE))
  expect_match(clinical,
               "does not establish or verify clinical reference intervals",
               fixed = TRUE)
})

test_that("educational guidance is opt-in", {
  d <- data.frame(marker = 1:10)
  default <- summarydata(data = d, vars = "marker")
  guided <- summarydata(data = d, vars = "marker", show_guidance = TRUE)

  expect_identical(as.character(default$clinicalInterpretation$content), "")
  expect_identical(as.character(default$aboutAnalysis$content), "")
  expect_identical(as.character(default$glossary$content), "")

  expect_match(sd_txt(guided$clinicalInterpretation$content),
               "Clinical Interpretation Guide", fixed = TRUE)
  expect_match(sd_txt(guided$aboutAnalysis$content),
               "What this analysis provides", fixed = TRUE)
  expect_match(sd_txt(guided$glossary$content),
               "Statistical Terminology", fixed = TRUE)
})

test_that("draft sentences disclose missingness and required reporting context", {
  d <- data.frame(marker = c(1, 2, 3, NA, NA))
  out <- sd_txt(summarydata(data = d, vars = "marker",
                            report_sentences = TRUE)$reportSentences$content)

  expect_match(out, "3 of 5 records", fixed = TRUE)
  expect_match(out, "2 missing", fixed = TRUE)
  expect_match(out, "Draft Statistical Summary", fixed = TRUE)
  expect_match(out, "measurement units", fixed = TRUE)
  expect_match(out, "study population", fixed = TRUE)
  expect_match(out, "missing-data handling", fixed = TRUE)
  expect_false(grepl("copy-ready", out, ignore.case = TRUE))
})

test_that("high missingness and very small samples produce strong notices", {
  high <- summarydata(data = data.frame(marker = c(1:7, NA, NA, NA)),
                      vars = "marker")
  high_notice <- as.character(high$notices$content)

  expect_match(high_notice, "STRONG WARNING: High missingness", fixed = TRUE)
  expect_match(high_notice, "3 missing values among 10 records (30.0%)",
               fixed = TRUE)
  expect_match(high_notice, "7 available observations", fixed = TRUE)

  small <- summarydata(data = data.frame(marker = c(1, 2, NA)), vars = "marker")
  small_notice <- as.character(small$notices$content)
  expect_match(small_notice, "STRONG WARNING: Very small sample", fixed = TRUE)
  expect_match(small_notice, "only 2 non-missing observation(s)", fixed = TRUE)
})

test_that("a simplified visual summary is disclosed", {
  testthat::local_mocked_bindings(
    gt_plt_summary = function(...) stop("forced visual failure"),
    .package = "gtExtras"
  )

  res <- summarydata(data = data.frame(marker = 1:5), vars = "marker")
  notice <- as.character(res$notices$content)
  visual_raw <- as.character(res$text1$content)
  visual <- sd_txt(visual_raw)

  expect_match(notice, "WARNING: Visual summary simplified", fixed = TRUE)
  expect_match(notice, "inline distribution plots could not be rendered",
               fixed = TRUE)
  expect_match(visual, "Dataset Summary", fixed = TRUE)
  expect_false(grepl("background-color: #FFFFFF", visual_raw,
                     ignore.case = TRUE))
  expect_false(grepl("background-color: #F8F9FA", visual_raw,
                     ignore.case = TRUE))
  expect_false(grepl("color: #333333", visual_raw,
                     ignore.case = TRUE))
  expect_match(visual_raw, "background-color: transparent", fixed = TRUE)
})

test_that("the normal visual summary follows the jamovi colour theme", {
  visual <- as.character(summarydata(
    data = data.frame(marker = 1:5), vars = "marker"
  )$text1$content)

  expect_false(grepl("background-color: #FFFFFF", visual,
                     ignore.case = TRUE))
  expect_false(grepl("color: #333333", visual,
                     ignore.case = TRUE))
  expect_match(visual, "background-color: transparent", fixed = TRUE)
  expect_match(visual, "color: inherit", fixed = TRUE)
})

test_that("multiple Shapiro-Wilk tests are labelled exploratory and unadjusted", {
  d <- data.frame(a = 1:10, b = 11:20)
  res <- summarydata(data = d, vars = c("a", "b"), distr = TRUE)
  notice <- as.character(res$notices$content)

  expect_match(notice, "NOTE: Multiple distribution diagnostics", fixed = TRUE)
  expect_match(notice, "without multiplicity adjustment", fixed = TRUE)
  expect_match(notice, "not confirmatory hypothesis tests", fixed = TRUE)
})

test_that("the source schema requires vars in R and labels draft output honestly", {
  schema_path <- testthat::test_path("..", "..", "jamovi", "summarydata.a.yaml")
  results_path <- testthat::test_path("..", "..", "jamovi", "summarydata.r.yaml")
  parsed_schema <- yaml::read_yaml(schema_path)
  vars_option <- parsed_schema$options[[which(vapply(
    parsed_schema$options,
    function(option) identical(option$name, "vars"),
    logical(1)
  ))]]
  schema <- paste(readLines(schema_path, warn = FALSE), collapse = "\n")
  results <- paste(readLines(results_path, warn = FALSE), collapse = "\n")

  expect_false("default" %in% names(vars_option))
  expect_match(schema, "title: \"Draft report sentences\"", fixed = TRUE)
  expect_match(results, "title: 'Draft Statistical Summary'", fixed = TRUE)
  expect_match(schema, "name: show_guidance", fixed = TRUE)
  expect_true(package_version(parsed_schema$version) >= package_version("1.0.8"))
  expect_match(results, "visible: (length(vars) > 0 && show_guidance)",
               fixed = TRUE)
})
