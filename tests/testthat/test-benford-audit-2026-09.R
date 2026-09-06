# Regression cover for the 2026-09 module audit fixes to benford.
# Each block fails against the pre-audit backend.

library(testthat)

bf_benford_data <- function(n, seed) { set.seed(seed); 10^runif(n, 0, 4) }

test_that("NaN is counted by the non-finite notice, not folded into missing", {
  # is.na(NaN) is TRUE, so `!is.na & !is.finite` counted only +/-Inf: one NaN
  # and one Inf reported "1 value(s)" under a notice whose text names NaN.
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = c(bf_benford_data(200, 11), NaN, Inf)),
                 var = "v", digits = 1)
  txt <- as.character(res$notices$content)
  expect_match(txt, "Non-finite values excluded", fixed = TRUE)
  expect_match(txt, "2 value(s)", fixed = TRUE)
})

test_that("repaired number splices render as whole English sentences", {
  # The fragments were folded into single {n} templates; the rendered text
  # must be unchanged and carry no jmvcore miss marker.
  skip_if_not_installed("benford.analysis")

  few <- benford(data = data.frame(v = bf_benford_data(20, 12)), var = "v", digits = 1)
  html <- as.character(few$dataWarning$content)
  expect_match(html, "Insufficient Data", fixed = TRUE)
  expect_match(html, "at least <strong>30</strong> valid observations", fixed = TRUE)
  expect_match(html, "Current data: 20 valid observations", fixed = TRUE)

  bad <- benford(data = data.frame(v = c(bf_benford_data(40, 13), 0, 0, 0, -1, -2)),
                 var = "v", digits = 1)
  html <- as.character(bad$dataWarning$content)
  expect_match(html, "Invalid Values Detected", fixed = TRUE)
  expect_match(html, "<li>3 zero values</li>", fixed = TRUE)
  expect_match(html, "<li>2 negative values</li>", fixed = TRUE)

  ok <- benford(data = data.frame(v = bf_benford_data(400, 14)), var = "v", digits = 1)
  for (item in c("explanation", "todo", "reportSentence", "text", "text2"))
    expect_false(grepl("\u{2026}", as.character(ok[[item]]$content), fixed = TRUE),
                 info = item)
  expect_match(as.character(ok$todo$content),
               "For technical details, see <a href=", fixed = TRUE)
  expect_match(as.character(ok$text$content), "Digit    | Expected %", fixed = TRUE)
  expect_match(as.character(ok$text$content), "Mantissa Arc Test: L\u{00B2} = ", fixed = TRUE)

  df <- ok$summary$asDF
  expect_match(df$value[df$statistic == "Chi-square Test"],
               "^X\u{00B2} = [0-9.]+, df = 8$")
  expect_match(df$value[df$statistic == "Mantissa Arc Test"],
               "^2nL\u{00B2} = [0-9.]+, df = 2$")
})

test_that("no .() msgid carries a non-ASCII character or a \\u escape", {
  # A non-ASCII character inside a .() literal is catalogued as the literal
  # "\u{..}" escape (see catalog.pot), so the runtime key never matches and the
  # string is silently untranslatable. Symbols go through placeholders.
  src <- readLines("../../R/benford.b.R", warn = FALSE)
  code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
  lits <- regmatches(code, gregexpr('\\.\\("[^"]*"\\)', code))
  lits <- unlist(lits)
  expect_gt(length(lits), 100)
  expect_length(grep("\\\\u\\{", lits), 0)
  expect_length(grep("[^ -~]", lits), 0)
  # and no fragment convention the audit flagged: markup inside a msgid
  expect_length(grep("<strong>", lits, fixed = TRUE), 0)
})
