# Regression coverage for the 2026-10-20 module audit repairs.
test_that("diagnostic syntax preserves column and level literals", {
  ns <- environment(decisioncurve)
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
  for (name in "decisioncurve") {
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

