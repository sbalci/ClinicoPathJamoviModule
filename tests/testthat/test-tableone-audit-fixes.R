# Regressions for the full audit. Exercise real classes and rendered output.
tableone_audit_analysis <- function(data, ...) {
  ns <- environment(tableone)
  options <- get("tableoneOptions", ns)$new(...)
  get("tableoneClass", ns)$new(options = options, data = data,
                              datasetId = "1", analysisId = 1L)
}

test_that("empty R selections reach onboarding without inventing a dataset", {
  d <- data.frame(x = 1:40)
  for (selection in list(NULL, character())) {
    result <- tableone(d, vars = !!selection, showAbout = TRUE)
    expect_match(result$todo$content, "Welcome")
    expect_match(result$about$content, "About Table One")
    expect_identical(result$tablestyle1$content, "")
    expect_identical(result$summary$content, "")
  }
  expect_match(tableone(d)$todo$content, "Welcome")
  expect_match(tableone(d[FALSE, , drop = FALSE])$todo$content, "No Data Available")
  expect_identical(tableone(d)$about$content, "")
  expect_error(tableone(1:5), "data frame")
})

test_that("unsupported classes are omitted before exclusion and reporting", {
  d <- data.frame(Age = 1:40, Group = factor(rep(c("A", "B"), 20)),
                  VisitDate = as.Date("2026-01-01") + 1:40,
                  Duration = as.difftime(1:40, units = "days"))
  d$VisitDate[1:10] <- NA
  d$Duration[11:20] <- NA
  for (style in c("t1", "t2", "t3", "t4")) {
    result <- tableone(d, c("Age", "Group", "VisitDate", "Duration"),
                       sty = style, excl = TRUE, showSummary = TRUE,
                       showReportSentence = TRUE)
    expect_match(result$todo$content, "VisitDate (Date)", fixed = TRUE)
    expect_match(result$todo$content, "Duration (difftime)", fixed = TRUE)
    expect_match(result$summary$content, "40 cases (no exclusions applied)", fixed = TRUE)
    expect_false(grepl("VisitDate|Duration", result$reportSentence$content))
    if (style == "t1") {
      expect_match(result$tablestyle1$content, "20.50 (11.69)", fixed = TRUE)
    }
  }
  result <- tableone(d, "VisitDate", showSummary = TRUE, showReportSentence = TRUE)
  expect_match(result$todo$content, "Nothing to summarise")
  expect_identical(result$reportSentence$content, "")
  expect_identical(result$summary$content, "")
  expect_match(result$assumptions$content, "Data quality check not performed")
})

test_that("real protobuf style restoration cannot retain obsolete reports", {
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  d <- data.frame(x = 1:40)
  origin <- tableone_audit_analysis(d, vars = "x", sty = "t1",
                                    showSummary = TRUE, showReportSentence = TRUE)
  origin$run()
  saved <- origin$results$asProtoBuf()
  expect_true(nzchar(origin$results$reportSentence$content))
  for (style in c("t2", "t3", "t4")) {
    restored <- tableone_audit_analysis(d, vars = "x", sty = style,
                                       showSummary = TRUE, showReportSentence = TRUE)
    restored$init()
    restored$postInit()
    restored$results$fromProtoBuf(saved, "sty", character())
    for (name in c("summary", "reportSentence", "assumptions")) {
      expect_true(restored$results[[name]]$.__enclos_env__$private$.stale, info = name)
    }
    restored$run()
    expect_identical(restored$results$tablestyle1$content, "")
    if (style == "t4") {
      expect_match(restored$results$tablestyle4$content, "Not tabulated")
      for (name in c("summary", "reportSentence")) {
        expect_identical(restored$results[[name]]$content, "", info = name)
      }
      expect_match(restored$results$assumptions$content, "Data quality check not performed")
    } else {
      expect_match(restored$results$reportSentence$content, "40 cases")
    }
    expect_type(RProtoBuf::serialize(restored$results$asProtoBuf(), NULL), "raw")
  }
})

test_that("all no-table and failed-engine paths clear restored outputs", {
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  origin <- tableone_audit_analysis(data.frame(x = 1:40), vars = "x",
                                    showSummary = TRUE, showReportSentence = TRUE,
                                    showAbout = TRUE)
  origin$run()
  saved <- origin$results$asProtoBuf()
  fixtures <- list(empty = data.frame(x = numeric()),
                   missing = data.frame(x = rep(NA_real_, 40)),
                   unsupported = data.frame(x = as.Date("2026-01-01") + 1:40),
                   no_complete = data.frame(x = c(NA, 2), y = c(1, NA)),
                   unselected = data.frame(x = 1:40))
  for (label in names(fixtures)) {
    d <- fixtures[[label]]
    selection <- if (label == "unselected") NULL else names(d)
    restored <- tableone_audit_analysis(d, vars = selection, excl = TRUE,
                                       showSummary = TRUE, showReportSentence = TRUE)
    restored$init()
    restored$postInit()
    restored$results$fromProtoBuf(saved, c("vars", "excl", "showAbout"), character())
    restored$run()
    for (name in c("tablestyle1", "summary", "reportSentence", "about")) {
      expect_identical(restored$results[[name]]$content, "", info = paste(label, name))
    }
    # The panel is visible whenever variables are selected, so it must carry
    # a body on every early return; without a selection it stays hidden/empty.
    if (label == "unselected") {
      expect_identical(restored$results$assumptions$content, "", info = label)
    } else {
      expect_match(restored$results$assumptions$content,
                   "Data quality check not performed", info = label)
    }
  }
  ns <- environment(tableone)
  failing_class <- R6::R6Class(inherit = get("tableoneClass", ns), private = list(
    .renderTable = function(...) stop("engine unavailable")
  ))
  failed <- failing_class$new(data = data.frame(x = 1:40),
    options = get("tableoneOptions", ns)$new(vars = "x",
      showSummary = TRUE, showReportSentence = TRUE))
  failed$init()
  failed$postInit()
  failed$results$fromProtoBuf(saved, character(), character())
  # The subclass forces an engine error without changing dependency namespaces.
  expect_error(failed$run(), "engine unavailable")
  expect_identical(failed$results$summary$content, "")
  expect_identical(failed$results$reportSentence$content, "")
})

test_that("arsenal output exports whitespace without decoding user markup", {
  d <- data.frame(Age = 1:40,
                  Group = factor(rep(c("<20%", "&nbsp; <script>alert(1)</script>"), 20)))
  result <- tableone(d, c("Age", "Group"), sty = "t3")
  html <- as.character(result$tablestyle3$content)
  text <- result$tablestyle3$asString()
  expect_false(grepl("&nbsp;", html, fixed = TRUE))
  expect_false(grepl("<(td|th)\\s", html, perl = TRUE))
  expect_match(html, "&lt;20%", fixed = TRUE)
  expect_match(html, "&amp;nbsp;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("<(td|th)(>|\\s)", text, perl = TRUE))
  # &nbsp; typed by the user may appear literally in text; renderer indentation
  # must not leave entity text before Mean or categorical labels.
  expect_false(grepl("&nbsp;.*(Mean|&lt;20)", text))
  expect_match(html, sprintf("%.1f", mean(d$Age)), fixed = TRUE)
})

test_that("warning thresholds compare counts rather than rounded percentages", {
  inspect <- function(n_missing, exclude = FALSE) {
    d <- data.frame(x = seq_len(10000))
    d$x[seq_len(n_missing)] <- NA
    tableone(d, "x", excl = exclude)$assumptions$content
  }
  expect_false(grepl("Moderate missing", inspect(2000), fixed = TRUE))
  expect_match(inspect(2001), "Moderate missing")
  expect_false(grepl("High missing", inspect(5000), fixed = TRUE))
  expect_match(inspect(5001), "High missing")
  expect_match(inspect(5001), "5001/10000", fixed = TRUE)
  expect_false(grepl("Notable case loss", inspect(1000, TRUE), fixed = TRUE))
  expect_match(inspect(1001, TRUE), "Notable case loss")
  expect_false(grepl("Large case loss", inspect(3000, TRUE), fixed = TRUE))
  expect_match(inspect(3001, TRUE), "Large case loss")
  expect_match(inspect(3001, TRUE), "not validated clinical cutoffs", fixed = TRUE)
})

test_that("guidance and report wording respect statistics and row units", {
  result <- tableone(data.frame(score = rep(1:5, 8)), "score", nonnormal = TRUE,
                     showReportSentence = TRUE)
  expect_match(result$assumptions$content, "median option is enabled", fixed = TRUE)
  expect_match(result$assumptions$content, "median (Q1, Q3)", fixed = TRUE)
  expect_match(result$reportSentence$content, "40 cases", fixed = TRUE)
  expect_match(result$reportSentence$content, "not verified unique patients", fixed = TRUE)
  expect_match(result$reportSentence$content, "not deduplicated", fixed = TRUE)
})

test_that("formatting failure never labels unformatted fractions as Percent", {
  skip_if_not_installed("janitor")
  testthat::local_mocked_bindings(
    adorn_pct_formatting = function(...) stop("percentage formatter unavailable"),
    .package = "janitor"
  )
  result <- tableone(data.frame(Group = factor(c("A", "A", "B"))), "Group",
                     sty = "t4", showSummary = TRUE, showReportSentence = TRUE)
  expect_match(result$tablestyle4$content, "percentage formatter unavailable")
  expect_match(result$todo$content, "output is incomplete")
  expect_identical(result$summary$content, "")
  expect_identical(result$reportSentence$content, "")
  expect_false(grepl("66.7|0.666", result$tablestyle4$content))
})

test_that("table statistics and recorded denominators agree with base R", {
  d <- data.frame(x = c(1:19, 100),
                  g = factor(c(rep("A", 10), rep("B", 8), NA, NA)))
  standard <- tableone(d, c("x", "g"))
  expect_match(standard$tablestyle1$content,
               sprintf("%.2f (%.2f)", mean(d$x), sd(d$x)), fixed = TRUE)
  expect_match(standard$tablestyle1$content, "8 (44.4)", fixed = TRUE)
  q <- quantile(d$x, c(.25, .5, .75))
  median <- tableone(d, c("x", "g"), nonnormal = TRUE)
  expect_match(median$tablestyle1$content,
               sprintf("%.2f [%.2f, %.2f]", q[2], q[1], q[3]), fixed = TRUE)
  complete <- tableone(d, c("x", "g"), excl = TRUE)
  expect_match(complete$tablestyle1$content, "n\\s+18")
  expect_match(complete$tablestyle1$content,
               sprintf("%.2f (%.2f)", mean(d$x[1:18]), sd(d$x[1:18])), fixed = TRUE)
  frequency <- tableone(d, "g", sty = "t4")
  plain <- gsub("<[^>]*>", " ", frequency$tablestyle4$content)
  expect_match(plain, "B\\s+8\\s+40.0%\\s+44.4%")
})

test_that("installed analysis serializes full responses and reruns exported syntax", {
  skip_if_not_installed("ClinicoPath")
  skip_if_not_installed("RProtoBuf")
  d <- setNames(data.frame(x = 1:40, g = factor(rep(c("A", "B"), 20))),
                c("Ki67 {%}", "a`b"))
  for (style in c("t1", "t2", "t3", "t4")) {
    analysis <- tableone_audit_analysis(d, vars = names(d), sty = style)
    analysis$run()
    response <- analysis$asProtoBuf(final = TRUE)
    expect_length(response$references, 6L)
    expect_gt(length(RProtoBuf::serialize(response, NULL)), 0L)
    code <- analysis$asSource()
    restored <- eval(parse(text = code), envir = list(data = d))
    item <- paste0("tablestyle", substring(style, 2))
    expect_identical(as.character(restored[[item]]$content),
                      as.character(analysis$results[[item]]$content))
  }
})
