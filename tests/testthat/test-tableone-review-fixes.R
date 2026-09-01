# Regression cases from the 2026-08-31 review; exercise the real analysis/engines.
tableone_review_analysis <- function(data, ...) {
  ns <- environment(tableone)
  get("tableoneClass", ns)$new(
    options = get("tableoneOptions", ns)$new(...), data = data,
    datasetId = "1", analysisId = 1L
  )
}

test_that("NA factor levels share canonical missingness across all outputs", {
  for (ordered in c(FALSE, TRUE)) {
    d <- data.frame(x = 1:40, g = factor(
      c(rep("A", 20), rep("B", 10), rep(NA, 10)),
      levels = c("A", "B", "Unused", NA), exclude = NULL, ordered = ordered
    ))
    d$x[1] <- NA
    attr(d$g, "label") <- "Group label"
    before <- d
    canonical <- d
    canonical$g <- factor(as.character(d$g),
                          levels = levels(d$g)[!is.na(levels(d$g))], ordered = ordered)
    attr(canonical$g, "label") <- "Group label"
    for (style in c("t1", "t2", "t3", "t4")) {
      for (exclude in c(FALSE, TRUE)) {
        actual <- tableone(d, c("x", "g"), sty = style, excl = exclude,
                          showSummary = TRUE, showReportSentence = TRUE)
        expected <- tableone(canonical, c("x", "g"), sty = style, excl = exclude,
                            showSummary = TRUE, showReportSentence = TRUE)
        for (item in c(paste0("tablestyle", substring(style, 2)),
                       "summary", "reportSentence", "assumptions")) {
          expect_identical(as.character(actual[[item]]$content),
                           as.character(expected[[item]]$content),
                           info = paste(ordered, style, exclude, item))
        }
        expect_match(actual$todo$content, "Actual NA factor levels")
        if (exclude) {
          n <- if (style == "t4") 30L else 29L
          expect_match(actual$reportSentence$content,
                       paste(n, "cases with complete data"), fixed = TRUE)
        }
        expect_identical(d, before)
      }
    }
  }
})

test_that("actual NA levels do not recode literal categories or all-missing columns", {
  d <- data.frame(g = factor(c("NA", "Unknown", "<20%", NA), exclude = NULL))
  for (style in c("t1", "t2", "t3", "t4")) {
    result <- tableone(d, "g", sty = style, excl = TRUE, showReportSentence = TRUE)
    expect_match(result$reportSentence$content, "3 cases with complete data")
    rendered <- result[[paste0("tablestyle", substring(style, 2))]]$content
    expect_match(rendered, "Unknown")
    expect_match(rendered, "NA")
    if (style != "t1") expect_match(rendered, "&lt;20%", fixed = TRUE)
    empty <- data.frame(g = factor(rep(NA, 40), exclude = NULL), x = 1:40)
    result <- tableone(empty, c("g", "x"), sty = style, excl = TRUE,
                      showReportSentence = TRUE)
    expect_match(result$todo$content, "Every value of this variable is missing")
    if (style != "t4") expect_match(result$reportSentence$content, "40 cases")
  }
})

test_that("original non-scalar columns are rejected before framework flattening", {
  fixtures <- list(matrix(1:80, 40), matrix(1:40, 40), array(1:80, c(40, 1, 2)),
                   as.list(1:40), rep(list(NA), 40), rep(list(list(1, 2)), 40))
  for (value in fixtures) {
    d <- data.frame(x = 1:40)
    d$m <- value
    before <- d
    for (vars in list("m", c("x", "m"))) {
      for (style in c("t1", "t2", "t3", "t4")) {
        expect_error(tableone(d, vars = !!vars, sty = style),
                     "Unsupported non-scalar columns: m")
      }
    }
    expect_identical(d, before)
    # Unselected columns must not affect valid analyses or onboarding.
    expect_match(tableone(d, "x")$tablestyle1$content, "20.50 (11.69)", fixed = TRUE)
    expect_match(tableone(d)$todo$content, "Welcome")
  }
  analysis <- tableone_review_analysis(d, vars = "m")
  expect_error(analysis$init(noThrow = TRUE), NA)
  expect_identical(analysis$status, "error")
  expect_identical(analysis$results$summary$content, "")
})

test_that("escaping cannot change factor codes or missingness", {
  analysis <- tableone_review_analysis(data.frame(x = 1:40), vars = "x")
  private <- analysis$.__enclos_env__$private
  d <- data.frame(g = factor(c("<20%", "&amp;", NA), exclude = NULL),
                  text = c("<tag>", NA, "a&b"))
  escaped <- private$.htmlSafeTableData(d)
  expect_identical(is.na(escaped), is.na(d))
  expect_identical(as.integer(escaped$g), as.integer(d$g))
  expect_identical(is.na(levels(escaped$g)), is.na(levels(d$g)))
  expect_identical(as.character(escaped$text), c("&lt;tag&gt;", NA, "a&amp;b"))
  expect_identical(private$.formatText("{value}: {n}", value = "a{other}\\1", n = 7),
                   "a{other}\\1: 7")
})

test_that("frequency rendering is bounded by recorded categories", {
  d <- data.frame(g = factor(c(rep(c("L1", "L2"), 20), NA),
                             levels = paste0("L", 1:1000)))
  r <- tableone(d, "g", sty = "t4")
  html <- as.character(r$tablestyle4$content)
  expect_false(grepl("L1000", html, fixed = TRUE))
  expect_length(regmatches(html, gregexpr("<tr", html, fixed = TRUE))[[1]], 5L)
  expect_match(html, "Valid Percent")
  expect_match(html, "50.0%", fixed = TRUE)
  expect_lt(nchar(html), 5000L)
  d <- data.frame(g = factor(paste0("L", 1:21), levels = paste0("L", 1:1000)))
  r <- tableone(d, "g", sty = "t4", showReportSentence = TRUE)
  expect_match(r$tablestyle4$content, "maximum 20 categories")
  expect_identical(r$reportSentence$content, "")
})

test_that("dichotomous rows identify the counted level", {
  d <- data.frame(flag = rep(FALSE, 40), yes = rep(c("yes", "no"), 20),
                  code = rep(0:1, 20))
  attr(d$flag, "label") <- "Flag <flag>"
  r <- tableone(d, names(d), sty = "t2")
  html <- as.character(r$tablestyle2$content)
  expect_match(html, "Flag &lt;flag&gt; = TRUE", fixed = TRUE)
  expect_match(html, "yes = yes", fixed = TRUE)
  expect_match(html, "code = 1", fixed = TRUE)
  expect_match(html, "0 (0%)", fixed = TRUE)
  expect_false(grepl("<flag>", html, fixed = TRUE))
})

test_that("canonical factor results survive protobuf and style restoration", {
  skip_if_not_installed("RProtoBuf")
  jmvcore:::initProtoBuf()
  d <- data.frame(g = factor(c(rep("A", 20), rep("B", 10), rep(NA, 10)),
                            exclude = NULL))
  origin <- tableone_review_analysis(d, vars = "g", sty = "t3", excl = TRUE,
                                     showSummary = TRUE, showReportSentence = TRUE)
  origin$run()
  saved <- origin$results$asProtoBuf()
  for (style in c("t1", "t2", "t3", "t4")) {
    restored <- tableone_review_analysis(d, vars = "g", sty = style, excl = TRUE,
                                         showSummary = TRUE, showReportSentence = TRUE)
    restored$init()
    restored$postInit()
    restored$results$fromProtoBuf(saved, "sty", character())
    restored$run()
    expect_match(restored$results$reportSentence$content, "30 cases with complete data")
    expect_match(restored$results$todo$content, "Actual NA factor levels")
    expect_gt(length(RProtoBuf::serialize(restored$asProtoBuf(final = TRUE), NULL)), 0)
  }
})

test_that("Turkish runtime prose retains counts, placeholders and user labels", {
  locale_file <- system.file("i18n/tr.json", package = "ClinicoPath")
  skip_if(!nzchar(locale_file), "Compiled Turkish catalog is required")
  lang <- jsonlite::read_json(locale_file)
  translator <- jmvcore:::Translator$new(lang)
  d <- data.frame(g = factor(c(rep("A", 20), rep("B", 10), rep(NA, 10)),
                            exclude = NULL))
  names(d) <- "Group {user} <tag>"
  analysis <- tableone_review_analysis(d, vars = names(d), sty = "t4", excl = TRUE,
                                       showSummary = TRUE, showAbout = TRUE,
                                       showReportSentence = TRUE)
  options <- analysis$options
  options$.__enclos_env__$private$.translator <- translator
  analysis$run()
  expect_match(analysis$results$reportSentence$content, "30 olgunun", fixed = TRUE)
  expect_match(analysis$results$reportSentence$content, "Group {user} &lt;tag&gt;", fixed = TRUE)
  expect_match(analysis$results$summary$content, "Analiz \u00d6zeti", fixed = TRUE)
  expect_match(analysis$results$about$content, "Tablo Bir Hakk\u0131nda", fixed = TRUE)
  expect_match(analysis$results$tablestyle4$content, "Y\u00fczde", fixed = TRUE)
  expect_false(grepl("\\{(n|screened|variables|percent|missing)\\}",
                    analysis$results$reportSentence$content))
  # The installed locale must cover every backend translation marker.
  path <- testthat::test_path("..", "..", "R", "tableone.b.R")
  parsed <- utils::getParseData(parse(path, keep.source = TRUE))
  calls <- parsed$parent[parsed$token == "SYMBOL_FUNCTION_CALL" & parsed$text == "."]
  calls <- parsed$parent[match(calls, parsed$id)]
  constants <- parsed[parsed$token == "STR_CONST", ]
  parents <- parsed$parent[match(constants$parent, parsed$id)]
  strings <- vapply(constants$text[parents %in% calls],
                    function(text) eval(parse(text = text)), character(1))
  expect_gt(length(strings), 80L)
  messages <- lang$locale_data$messages
  for (id in unique(strings)) {
    expect_true(nzchar(messages[[id]][[1]]), info = id)
    tokens <- function(x) sort(regmatches(x, gregexpr("\\{[A-Za-z][A-Za-z0-9]*\\}", x))[[1]])
    expect_identical(tokens(id), tokens(messages[[id]][[1]]), info = id)
  }
})

test_that("one-case report uses a complete singular sentence", {
  r <- tableone(data.frame(x = 1), "x", showReportSentence = TRUE)
  expect_match(r$reportSentence$content, "1 case. Variables", fixed = TRUE)
  expect_false(grepl("1 cases", r$reportSentence$content, fixed = TRUE))
})
