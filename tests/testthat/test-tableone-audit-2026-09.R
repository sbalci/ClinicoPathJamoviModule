# Regressions for the 2026-09 module audit of tableone.
tableone_audit_2026_analysis <- function(data, ...) {
  ns <- environment(tableone)
  get("tableoneClass", ns)$new(data = data,
    options = get("tableoneOptions", ns)$new(...))
}

test_that("a catalogue placeholder typo degrades the sentence instead of aborting", {
  analysis <- tableone_audit_2026_analysis(data.frame(x = 1:5), vars = "x")
  format_text <- analysis$.__enclos_env__$private$.formatText
  # msgstr renamed {n} to {N}: the unknown token stays literal, the run survives
  expect_identical(format_text("Each of the {N} cases; {n} kept", n = 7L),
                   "Each of the {N} cases; 7 kept")
  expect_identical(format_text("Variables: {variables}", variables = c("a", "b")),
                   "Variables: a, b")
  expect_identical(format_text("No tokens here"), "No tokens here")
})

test_that("assumptions panel is never a visible empty heading", {
  skipped <- "Data quality check not performed"
  # no rows
  expect_match(tableone(data.frame(x = numeric()), "x")$assumptions$content, "no rows")
  # nothing summarisable
  expect_match(tableone(data.frame(x = rep(NA_real_, 40)), "x")$assumptions$content,
               "could be summarised")
  # listwise deletion emptied the frame
  expect_match(tableone(data.frame(x = c(NA, 2), y = c(1, NA)), c("x", "y"),
                        excl = TRUE)$assumptions$content, "left no cases")
  # janitor: nothing tabulable
  expect_match(tableone(data.frame(x = 1:40), "x", sty = "t4")$assumptions$content,
               "could be tabulated")
  for (content in list(
    tableone(data.frame(x = numeric()), "x")$assumptions$content,
    tableone(data.frame(x = 1:40), "x", sty = "t4")$assumptions$content)) {
    expect_match(content, skipped)
  }
  # success path still produces the real check
  ok <- tableone(data.frame(x = 1:40), "x")$assumptions$content
  expect_match(ok, "Data Quality Check")
  expect_false(grepl(skipped, ok, fixed = TRUE))
  # no selection: hidden by visible: (vars), body stays empty
  expect_identical(tableone(data.frame(x = 1:40))$assumptions$content, "")
})

test_that("a failed janitor table withholds the data quality check with a reason", {
  skip_if_not_installed("janitor")
  testthat::local_mocked_bindings(
    adorn_pct_formatting = function(...) stop("percentage formatter unavailable"),
    .package = "janitor"
  )
  result <- tableone(data.frame(Group = factor(c("A", "A", "B"))), "Group", sty = "t4")
  expect_match(result$assumptions$content, "withheld")
  expect_match(result$assumptions$content, "Data quality check not performed")
})

test_that("engine row labels are still the English defaults in the default locale", {
  d <- data.frame(g = factor(c("A", "B", NA)))
  expect_match(gsub("<[^>]*>", " ", tableone(d, "g", sty = "t2")$tablestyle2$content),
               "Unknown")
  expect_match(gsub("<[^>]*>", " ", tableone(d, "g", sty = "t3")$tablestyle3$content),
               "N-Miss")
  plain <- gsub("<[^>]*>", " ", tableone(d, "g", sty = "t4")$tablestyle4$content)
  expect_match(plain, "Total")
  expect_match(plain, "\\bN\\b")
})
