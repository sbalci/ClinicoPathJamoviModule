# Regression coverage for the 2026-10-20 module audit repairs.
test_that("diagnostic syntax preserves column and level literals", {
  ns <- environment(agreement)
  labels <- c('positive "quoted" \\ class', 'negative')
  columns <- c('gold with spaces', 'test `quoted` \\ name', 'second test', 'score with spaces')
  d <- data.frame(y = factor(rep(labels, each = 20)),
    a = factor(rep(labels, 20)), b = factor(rep(labels, each = 10, times = 2)),
    score = seq_len(40) / 40)
  names(d) <- columns
  configs <- list(
    agreement = list(vars = columns[2:3]),
    decision = list(gold = columns[1], goldPositive = labels[1], goldNegative = labels[2],
      newtest = columns[2], testPositive = labels[1], testNegative = labels[2]),
    decisioncurve = list(outcome = columns[1], outcomePositive = labels[1], models = columns[4]),
    lassologistic = list(outcome = columns[1], outcomeLevel = labels[1], explanatory = columns[4]))
  for (name in "agreement") {
    opts <- do.call(get(paste0(name, "Options"), ns)$new, configs[[name]])
    a <- get(paste0(name, "Class"), ns)$new(data = d, options = opts)
    a$init()
    code <- a$asSource()
    parsed <- parse(text = code)[[1]]
    expect_length(parse(text = code), 1L)
    for (key in names(configs[[name]])) {
      # Only evaluate the literal argument node, never the exported analysis call.
      expect_identical(eval(parsed[[key]], envir = baseenv()), configs[[name]][[key]],
        info = paste(name, key))
    }
    expect_identical(parsed$data, quote(data))
    expect_false("results/" %in% names(as.list(parsed)))
    expect_false("confLevel" %in% names(as.list(parsed)))
  }
})


test_that("limits-of-agreement coverage rejects invalid input without substitution", {
  ns <- environment(agreement)
  d <- data.frame(a = seq_len(60) / 10, b = seq_len(60) / 10 + sin(seq_len(60)))
  make <- function(coverage) {
    a <- ns$agreementClass$new(data = d,
      options = ns$agreementOptions$new(vars = c("a", "b"),
        blandAltmanPlot = TRUE, baConfidenceLevel = coverage))
    suppressWarnings(a$init(noThrow = TRUE))
    if (!a$errored) suppressWarnings(a$run(noThrow = TRUE))
    a
  }
  for (coverage in c(-1, .2, 1, 95, Inf, NaN)) {
    a <- make(coverage)
    expect_identical(a$options$baConfidenceLevel, coverage)
    expect_true(a$errored, info = as.character(coverage))
    expect_false(a$complete)
  }
  for (coverage in c(.5, .95, .99)) {
    a <- make(coverage)
    expect_false(a$errored)
    expect_true(a$complete)
    expect_identical(a$options$baConfidenceLevel, coverage)
  }
})

test_that("agreement guides use translation while preserving markup", {
  ns <- environment(agreement)
  opts <- ns$agreementOptions$new()
  a <- ns$agreementClass$new(data = data.frame(), options = opts)
  a$init()
  private <- a$.__enclos_env__$private
  # Options translation is the actual hook used by jmvcore::. inside R6 methods.
  opts$.__enclos_env__$private$.translator <- list(translate = function(text, n = 1) {
    if (text == "About This Analysis") return("Bu analiz hakkında")
    if (text == "What is Gwet's AC Coefficient?") return("Gwet AC katsayısı nedir?")
    text
  })
  about <- private$.createAboutPanel()
  expect_match(about, "Bu analiz hakkında", fixed = TRUE)
  expect_match(about, "<h3", fixed = TRUE)
  private$.populateGwetExplanation()
  expect_match(a$results$gwetExplanation$content, "Gwet AC katsayısı nedir?", fixed = TRUE)
  expect_false(grepl("\\{tag[0-9]+\\}", about))
})

test_that("agreement guide headings use the packaged Turkish catalog", {
  ns <- environment(agreement)
  opts <- ns$agreementOptions$new()
  opts$.__enclos_env__$private$.lang <- "tr"
  a <- ns$agreementClass$new(data = data.frame(), options = opts)
  a$init()
  private <- a$.__enclos_env__$private
  private$.populateGwetExplanation()
  expect_match(a$results$gwetExplanation$content, "Gwet AC katsayısı nedir?", fixed = TRUE)
  expect_match(private$.createAboutPanel(), "Bu [Aa]naliz [Hh]akkında")
  expect_false(grepl("\\{tag[0-9]+\\}", a$results$gwetExplanation$content))
})
