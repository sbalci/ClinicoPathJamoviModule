# Regression cover for the 2026-09 module audit findings on summarydata.
# Each block fails against the pre-audit backend.

library(testthat)

test_that("variable names are escaped in the gtExtras visual summary (XSS)", {
  # gt_plt_summary() renders its name column as raw HTML, so a column header
  # containing markup was emitted verbatim into the text1 Html item.
  set.seed(1)
  d <- data.frame(rnorm(30), rnorm(30), check.names = FALSE)
  names(d) <- c("Age <img src=x onerror=alert(1)>", "marker")

  # The wrapper resolves `vars` by NSE, so the names must be literal strings
  # (a variable holding them would be read as a column called by its own name).
  res <- summarydata(data = d, vars = c("Age <img src=x onerror=alert(1)>", "marker"))

  visual <- as.character(res$text1$content)
  expect_false(grepl("<img", visual, fixed = TRUE))
  expect_match(visual, "&lt;img", fixed = TRUE)
  # No double escape either (would display "&lt;img" literally).
  expect_false(grepl("&amp;lt;", visual, fixed = TRUE))

  # The text panel already escaped; keep it that way.
  txt <- as.character(res$text$content)
  expect_false(grepl("<img", txt, fixed = TRUE))
  expect_match(txt, "&lt;img", fixed = TRUE)
})

test_that("Preformatted notices show raw variable names, not HTML entities", {
  # The notices item is type Preformatted: jamovi renders it as text, so an
  # escaped "Ki67 &amp; p53" was displayed literally.
  set.seed(2)
  d <- data.frame(rnorm(20), rnorm(20), check.names = FALSE)
  names(d) <- c("Ki67 & p53", "CD3 & CD8")
  d[["Ki67 & p53"]][1:10] <- NA   # 50 % missing  -> High missingness
  d[["CD3 & CD8"]][1:18] <- NA    # n = 2         -> Very small sample

  res <- summarydata(data = d, vars = c("Ki67 & p53", "CD3 & CD8"))
  notices <- as.character(res$notices$content)

  expect_match(notices, "High missingness", fixed = TRUE)
  expect_match(notices, "Very small sample", fixed = TRUE)
  expect_match(notices, "Ki67 & p53", fixed = TRUE)
  expect_match(notices, "CD3 & CD8", fixed = TRUE)
  expect_false(grepl("&amp;", notices, fixed = TRUE))
})

test_that("vars is an optional argument of the R wrapper", {
  # Without `default: NULL` the generated wrapper made `vars` required and
  # summarydata(data = df) failed with 'argument "vars" is missing'.
  expect_true(is.null(formals(summarydata)[["vars"]]))
})

test_that("a single observation is reported as a sample-size problem, not constant data", {
  d <- data.frame(marker = c(5, NA, NA, NA, NA))
  res <- summarydata(data = d, vars = "marker", distr = TRUE)
  txt <- as.character(res$text$content)

  expect_match(txt, "between 3 and 5000", fixed = TRUE)
  expect_false(grepl("constant and have no variance", txt, fixed = TRUE))
})

test_that("the IQR-fence sentences render the multiplication sign, not a placeholder", {
  # The sign now travels through a {times} placeholder so the catalog key is
  # the runtime string; make sure jmvcore::format actually substituted it.
  set.seed(3)
  d <- data.frame(marker = c(rnorm(30), 50))
  res <- summarydata(data = d, vars = "marker", outliers = TRUE, show_guidance = TRUE)

  outlier <- as.character(res$outlierReport$content)
  glossary <- as.character(res$glossary$content)
  expect_match(outlier, "Q1-1.5\u{D7}IQR", fixed = TRUE)
  expect_match(glossary, "Q1-1.5\u{D7}IQR", fixed = TRUE)
  expect_false(grepl("{times}", outlier, fixed = TRUE))
  expect_false(grepl("{times}", glossary, fixed = TRUE))
})
