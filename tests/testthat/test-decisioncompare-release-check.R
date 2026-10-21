# /check-function decisioncompare --profile release (2026-10-04): regressions for the
# reference-interpretation pass. Each test fails on the source before that check.

rc_data <- function() {
  data.frame(
    gold = factor(rep(c("P", "N"), c(40, 60))),
    a = factor(c(rep("P", 34), rep("N", 6), rep("P", 9), rep("N", 51))),
    b = factor(c(rep("P", 30), rep("N", 10), rep("P", 4), rep("N", 56))))
}

rc_analysis <- function(..., data = rc_data()) {
  args <- utils::modifyList(list(gold = "gold", goldPositive = "P", goldNegative = "N",
    test1 = "a", test1Positive = "P", test1Negative = "N",
    test2 = "b", test2Positive = "P", test2Negative = "N"), list(...), keep.null = TRUE)
  options <- do.call(ClinicoPath:::decisioncompareOptions$new, args)
  ClinicoPath:::decisioncompareClass$new(options = options, data = data)
}

rc_run <- function(..., data = rc_data()) {
  analysis <- rc_analysis(..., data = data)
  analysis$init()
  # A validation stop re-raises after the notices are rendered; the tests read the notices.
  try(analysis$run(), silent = TRUE)
  analysis
}

# The jamovi client evaluates .u.yaml `enable:` with no operator precedence: operands fold
# strictly left to right. Operands: name, name:level, !operand, (group).
rc_enable <- function(expr, values) {
  s <- gsub("\\s+", "", expr)
  stopifnot(startsWith(s, "("), endsWith(s, ")"))
  pos <- 1L
  group <- function() {
    pos <<- pos + 1L
    acc <- NULL
    op <- NULL
    repeat {
      negate <- substr(s, pos, pos) == "!"
      if (negate) pos <<- pos + 1L
      if (substr(s, pos, pos) == "(") {
        value <- group()
      } else {
        rest <- substring(s, pos)
        token <- regmatches(rest, regexpr("^[A-Za-z0-9_]+(:[A-Za-z0-9_]+)?", rest))
        pos <<- pos + nchar(token)
        parts <- strsplit(token, ":", fixed = TRUE)[[1]]
        value <- if (length(parts) == 2L) identical(values[[parts[1]]], parts[2])
                 else isTRUE(values[[parts[1]]])
      }
      if (negate) value <- !value
      acc <- if (is.null(op)) value else if (op == "&&") acc && value else acc || value
      following <- substr(s, pos, pos + 1L)
      if (following %in% c("&&", "||")) {
        op <- following
        pos <<- pos + 2L
      } else {
        pos <<- pos + 1L
        return(acc)
      }
    }
  }
  group()
}

rc_controls <- function() {
  find <- function(items) {
    out <- list()
    for (item in items) {
      if (!is.null(item$name)) out[[item$name]] <- item
      if (!is.null(item$children)) out <- c(out, find(item$children))
    }
    out
  }
  find(yaml::read_yaml(test_path("..", "..", "jamovi", "decisioncompare.u.yaml"))$children)
}

test_that("consensus and method modes ignore a prevalence setting left from disease mode", {
  for (mode in c("consensus", "method")) {
    # pp keeps its value while its checkbox is disabled; it must not add a PPV/NPV notice
    a <- rc_run(referenceType = mode, pp = TRUE, pprob = 0.2)
    notices <- a$results$notices$content
    expect_false(grepl("Prevalence Source", notices, fixed = TRUE))
    expect_equal(a$results$comparatorMetrics$rowCount, 14)

    # nor block the comparator's own confidence intervals
    a <- rc_run(referenceType = mode, pp = TRUE, pprob = 0.2, ci = TRUE)
    expect_false(grepl("Conflicting Options", a$results$notices$content, fixed = TRUE))
    metrics <- a$results$comparatorMetrics$asDF
    expect_equal(metrics$lower[1], stats::binom.test(34, 40)$conf.int[1])
  }
  # The disease path still rejects the pair and still reports the prevalence source
  for (mode in c("legacy", "disease")) {
    a <- rc_run(referenceType = mode, pp = TRUE, pprob = 0.2, ci = TRUE)
    expect_true(grepl("Conflicting Options", a$results$notices$content, fixed = TRUE))
    a <- rc_run(referenceType = mode, pp = TRUE, pprob = 0.2)
    expect_true(grepl("Prevalence Source", a$results$notices$content, fixed = TRUE))
  }
})

test_that("a ticked CI or prevalence checkbox can always be unticked where it is used", {
  controls <- rc_controls()
  ci <- controls$ci$enable
  pp <- controls$pp$enable
  for (mode in c("legacy", "disease", "consensus", "method")) {
    disease_path <- mode %in% c("legacy", "disease")
    for (ci_value in c(FALSE, TRUE)) for (pp_value in c(FALSE, TRUE)) {
      values <- list(referenceType = mode, ci = ci_value, pp = pp_value)
      label <- paste(mode, "ci", ci_value, "pp", pp_value)
      # ci is used in every mode, pp only on the disease path
      if (ci_value) expect_true(rc_enable(ci, values), info = label)
      if (pp_value && disease_path) expect_true(rc_enable(pp, values), info = label)
      # mutual exclusion is kept where both apply
      if (disease_path && pp_value && !ci_value) expect_false(rc_enable(ci, values), info = label)
      if (disease_path && ci_value && !pp_value) expect_false(rc_enable(pp, values), info = label)
      # comparator modes offer intervals whatever pp holds
      if (!disease_path) expect_true(rc_enable(ci, values), info = label)
    }
  }
})

test_that("paired comparator p-values carry the p-value column format", {
  # R's yaml reads YAML 1.1, where the column name `n` is boolean FALSE; keep scalars as text
  keep <- function(x) x
  results <- yaml::read_yaml(test_path("..", "..", "jamovi", "decisioncompare.r.yaml"),
    handlers = list("bool#yes" = keep, "bool#no" = keep))$items
  table <- Filter(function(item) identical(item$name, "comparatorDifferences"), results)[[1]]
  formats <- vapply(table$columns, function(column)
    if (is.null(column$format)) "" else column$format, character(1))
  names(formats) <- vapply(table$columns, `[[`, character(1), "name")
  expect_identical(unname(formats[c("p", "adjusted")]), c("zto,pvalue", "zto,pvalue"))
})

# ---- Second release pass (2026-10-04, after the validation-provenance work) ----

rc_factor <- function(x, levels = c("N", "P")) factor(x, levels = levels)

test_that("a supplied prevalence below 0.05% is not printed as 0.0%", {
  a <- rc_run(pp = TRUE, pprob = 2e-4, showDescriptiveReport = TRUE)
  report <- a$results$clinicalReport$content
  expect_match(report, "0.020% population prevalence", fixed = TRUE)
  expect_false(grepl("At the 0.0% population", report, fixed = TRUE))
  a <- rc_run(pp = TRUE, pprob = 0.3, showDescriptiveReport = TRUE)
  expect_match(a$results$clinicalReport$content, "30.0% population prevalence", fixed = TRUE)
})

test_that("comparator Holm family leaves out tests without discordant pairs", {
  # PPA: identical calls among reference positives. NPA and OPA: discordant 1 and 8.
  d <- data.frame(gold = rc_factor(rep(c("P", "N"), c(20, 30))),
    a = rc_factor(c(rep("P", 15), rep("N", 5), rep("P", 8), "N", rep("N", 21))),
    b = rc_factor(c(rep("P", 15), rep("N", 5), rep("N", 8), "P", rep("N", 21))))
  dif <- rc_run(referenceType = "consensus", statComp = TRUE, data = d)$results$comparatorDifferences$asDF
  p <- 11 / 512  # mid-p for 1 versus 8 discordant pairs
  expect_equal(dif$p, c(1, p, p))
  expect_true(is.na(dif$adjusted[1]))
  expect_equal(dif$adjusted[2:3], stats::p.adjust(c(p, p), "holm"))
})

test_that("comparator mode warns about sparse classes and splits exclusions", {
  d <- rc_data()
  d$a <- factor(as.character(d$a), levels = c("N", "P", "ND"))
  d$a[c(1, 2, 3)] <- NA
  d$a[c(41, 42, 43, 44)] <- "ND"
  d$gold[c(50, 51)] <- NA
  a <- rc_run(referenceType = "method", excludeIndeterminate = TRUE, data = d)
  pop <- a$results$comparatorPopulation$asDF
  expect_equal(pop$excluded, c(9, 2))
  expect_equal(pop$missing, c(5, 2))
  expect_equal(pop$other, c(4, 0))
  small <- d[c(1:20, 41:60), ]
  a <- rc_run(referenceType = "method", excludeIndeterminate = TRUE, data = small)
  expect_match(a$results$notices$content, "Sparse Comparator Class", fixed = TRUE)
})

test_that("comparator mode stops when a test has no determinate pair", {
  d <- rc_data()
  d$b <- factor(rep("ND", nrow(d)), levels = c("N", "P", "ND"))
  a <- rc_run(referenceType = "consensus", excludeIndeterminate = TRUE, data = d)
  expect_match(a$results$notices$content, "No Determinate Cases", fixed = TRUE)
  expect_equal(a$results$comparatorMetrics$rowCount, 0)
})

test_that("comparator strata follow the factor level order and count missing strata", {
  d <- rc_data()
  d$grade <- factor(rep(c("G3", "G1", "G2"), length.out = nrow(d)), levels = c("G1", "G2", "G3"))
  d$grade[c(5, 6)] <- NA
  a <- rc_run(referenceType = "consensus", stratify = "grade", data = d)
  expect_identical(unique(a$results$comparatorStrata$asDF$stratum), c("G1", "G2", "G3"))
  expect_match(a$results$notices$content, "Missing Stratifier Values", fixed = TRUE)
})

test_that("the reference or a compared test cannot be the stratifier", {
  for (s in c("gold", "a")) {
    a <- rc_run(referenceType = "consensus", stratify = s)
    expect_match(a$results$notices$content, "Stratifier Reused", fixed = TRUE)
    expect_equal(a$results$comparatorMetrics$rowCount, 0)
  }
})

test_that("a reference with a single test explains that a second test is needed", {
  a <- rc_run(test2 = NULL, test2Positive = NULL, test2Negative = NULL)
  expect_match(a$results$notices$content, "Second Test Required", fixed = TRUE)
})

test_that("a level-mapping failure has a title that fits every cause", {
  a <- rc_run(test2Negative = "P")
  expect_match(a$results$notices$content, "Level Mapping Problem", fixed = TRUE)
})

test_that("labels starting with < stay visible in the ledger tables", {
  d <- rc_data()
  d$a <- factor(as.character(d$a), levels = c("N", "P", "<LOD"))
  d$a[c(41, 42)] <- "<LOD"
  a <- rc_run(excludeIndeterminate = TRUE, data = d)
  expect_match(a$results$levelMapping$asDF$otherLevels[2], "< LOD (n=2)", fixed = TRUE)
})

test_that("option-only tables are filled in .init() with readable values", {
  a <- rc_analysis(showValidationContext = TRUE, referenceType = "method",
    cohortRole = "<external cohort")
  a$init()
  v <- a$results$validationContext$asDF
  expect_equal(nrow(v), 7)
  expect_equal(v$value[v$setting == "Reference interpretation"], "Comparative method")
  expect_equal(v$value[v$setting == "Unit of observation"], "Not specified")
  expect_equal(v$value[v$setting == "Cohort role"], "< external cohort")
  expect_equal(a$results$referenceDefinition$rowCount, 6)
})

test_that("free-text options export as parseable R syntax", {
  a <- rc_run(referenceDescription = 'Consensus of 3 "expert" \\ pathologists',
    modelVersion = "C:\\models\\v2")
  src <- a$asSource()
  expect_no_error(parse(text = src))
  expect_match(src, "referenceDescription = ", fixed = TRUE)
})

test_that("the radar closes on its first spoke and gaps are captioned", {
  d <- data.frame(test = rep(c("A", "B"), each = 3), metric = rep(c("PPA", "NPA", "OPA"), 2),
    estimate = c(.9, .7, .78, .6, NA, .81))
  radar <- ClinicoPath:::.diagComparatorPlotObject(d, "radar")
  expect_s3_class(radar$layers[[1]]$geom, "GeomPath")
  expect_false(any(vapply(radar$layers, function(l) inherits(l$geom, "GeomPolygon"), logical(1))))
  ring <- radar$data
  expect_equal(ring$estimate[ring$test == "A" & ring$x == 4], .9)
  caption <- function(p) if (exists("get_labs", asNamespace("ggplot2")))
    ggplot2::get_labs(p)$caption else p$labels$caption
  expect_match(caption(radar), "B NPA", fixed = TRUE)
  bar <- ClinicoPath:::.diagComparatorPlotObject(d, "bar")
  expect_match(caption(bar), "Not estimable", fixed = TRUE)
})

test_that("the comparator report rounds half-up like the tables", {
  m <- ClinicoPath:::.diagComparatorMetrics(c(rep(TRUE, 9), rep(FALSE, 7)), rep(TRUE, 16), FALSE)
  expect_match(ClinicoPath:::.diagComparatorReport(list(A = m)), "PPA 56.3% (9/16)", fixed = TRUE)
})

test_that("new options are documented and level selectors start empty", {
  options <- yaml::read_yaml(test_path("..", "..", "jamovi", "decisioncompare.a.yaml"))$options
  undocumented <- vapply(Filter(function(o) o$name != "data", options),
    function(o) if (is.null(o$description)) o$name else "", character(1))
  expect_identical(undocumented[nzchar(undocumented)], character(0))
  levels <- Filter(function(o) identical(o$type, "Level"), options)
  expect_true(all(vapply(levels, function(o) isTRUE(o$allowNone), logical(1))))
})

# ---- Fixes from the adversarial verification of the second pass ----

test_that("PPV and NPV at a tiny supplied prevalence are not printed as 0.0% or 100.0%", {
  report <- rc_run(pp = TRUE, pprob = 1e-5, showDescriptiveReport = TRUE)$results$clinicalReport$content
  expect_match(report, "0.0010% population prevalence", fixed = TRUE)
  expect_false(grepl("value was 0.0%", report, fixed = TRUE))
  expect_false(grepl("value was 100.0%", report, fixed = TRUE))
})

test_that("the second-test notice names no slot and fires for any single test slot", {
  a <- rc_run(test1 = NULL, test1Positive = NULL, test1Negative = NULL)
  notices <- a$results$notices$content
  expect_match(notices, "Second Test Required", fixed = TRUE)
  expect_match(notices, "another test slot", fixed = TRUE)
})

test_that("every mapping problem is reported in one notice, reference included", {
  # Two real mapping errors (an empty box is no longer one: IR2-decisioncompare-04).
  a <- rc_run(test1Negative = "P", test2Negative = "absent")
  notices <- a$results$notices$content
  expect_match(notices, "a: Choose different", fixed = TRUE)
  expect_match(notices, "b: The negative level", fixed = TRUE)
  a <- rc_run(goldPositive = NULL, test2Negative = NULL)
  expect_match(a$results$notices$content, "Levels Not Yet Selected", fixed = TRUE)
  expect_match(a$results$notices$content, "Reference positive level, Test 2 negative level", fixed = TRUE)
})

test_that("the recorded rule keeps level labels with braces exactly", {
  d <- rc_data()
  d$a <- factor(ifelse(as.character(d$a) == "P", "{1+}", "N"), levels = c("N", "{1+}"))
  a <- rc_run(test1Positive = "{1+}", data = d)
  rules <- a$results$referenceDefinition$asDF$value[4]
  expect_match(rules, "a: positive = {1+}; negative = N", fixed = TRUE)
})

test_that("shared helpers keep their English defaults for other callers", {
  m <- ClinicoPath:::.diagComparatorMetrics(c(TRUE, FALSE), c(TRUE, TRUE), TRUE)$metrics
  expect_identical(m$metric[1], "Positive percent agreement (PPA)")
  expect_identical(m$method[1], "Exact binomial (independent cases)")
  expect_error(ClinicoPath:::.diagComparatorBinary(factor(c("+", "-")), "+", NULL),
    "Select an explicit negative level", fixed = TRUE)
  t <- utils::modifyList(ClinicoPath:::.diagText, list(metric_ppa = "X"))
  expect_identical(ClinicoPath:::.diagComparatorMetrics(TRUE, TRUE, FALSE, t)$metrics$metric[1], "X")
})
