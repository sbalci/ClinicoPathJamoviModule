# Regression cover for the defects found during the benford release review.
# Each block fails against the pre-review backend.

library(testthat)

bf_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

# The narrative verdict lives in the summary table's Assessment row; the
# reportSentence panel carries only the headline plus the recommendation.
bf_assessment <- function(res) {
  df <- res$summary$asDF
  df$interpretation[df$statistic == "Assessment"]
}
bf_finding <- function(res) {
  df <- res$summary$asDF
  df$value[df$statistic == "Assessment"]
}
bf_madrow <- function(res) {
  df <- res$summary$asDF
  df$interpretation[df$statistic == "MAD (Mean Absolute Deviation)"]
}

# Exactly-Benford data: the first digits of 10^U are Benford-distributed by
# construction, so anything the analysis flags here is a false positive.
benford_data <- function(n, seed) { set.seed(seed); 10^runif(n, 0, 4) }

# Private methods must be reached through a live instance: pulling them off
# R6ClassGenerator$private_methods gives an unbound closure with no `private`.
bf_private <- function() {
  ns <- asNamespace("ClinicoPath")
  obj <- get("benfordClass", ns)$new(
    options = get("benfordOptions", ns)$new(var = "v", digits = 2),
    data = data.frame(v = benford_data(200, 1)))
  obj$.__enclos_env__$private
}

test_that("clean Benford data is never accused of manipulation", {
  # With the default 2-digit setting, Nigrini's MAD cut-offs sit BELOW the
  # sampling-noise floor until n > 1301, so exactly-Benford data was labelled
  # "Nonconformity" in 20/20 simulated runs at n = 100, 300 and 1000 - and the
  # analysis then reported "potential manipulation" and "IMMEDIATE REVIEW
  # REQUIRED". These are precisely the sample sizes the module's own guidance
  # recommends ("100-1000 observations").
  skip_if_not_installed("benford.analysis")

  # digits = 1 / n = 300 / seed 7 is the worst case found: MAD = 0.0173 lands in
  # Nigrini's "Nonconformity" band on data that is Benford by construction, while
  # the chi-square test on the same data reports p = 0.107. The original gate
  # cleared the label here (0.015 > the 0.0136 noise floor) and the analysis
  # printed the manipulation text over a non-significant test.
  cells <- list(list(n = 150, d = 2, s = 250), list(n = 500,  d = 2, s = 600),
                list(n = 1000, d = 2, s = 1100), list(n = 300, d = 1, s = 7),
                list(n = 500, d = 1, s = 8),     list(n = 2000, d = 2, s = 12))

  for (cell in cells) {
    lbl <- paste0("n = ", cell$n, ", digits = ", cell$d)
    res <- benford(data = data.frame(v = benford_data(cell$n, cell$s)),
                   var = "v", digits = cell$d)
    txt <- paste(bf_txt(res$reportSentence$content), bf_assessment(res))

    expect_false(grepl("manipulation", txt), info = lbl)
    expect_false(grepl("IMMEDIATE REVIEW", txt), info = lbl)
    expect_false(grepl("does NOT conform", txt), info = lbl)
    # and the output must say why the conformity label is being set aside
    expect_match(bf_assessment(res), "sampling noise", info = lbl)
    expect_match(bf_madrow(res), "not reliable", info = lbl)
    # NOT expect_identical(bf_finding(res), "No departure detected"): the
    # chi-square test is correctly sized here (measured 4.8% over 400 seeds at
    # nominal 5%), so demanding that no seed ever lands in its own 5% tail tests
    # randomness, not behaviour - seed 1100 at n=1000 sits at the 0.5th
    # percentile and legitimately reports a departure. What IS deterministic:
    # every cell spans 4 decades with n >= 100, so neither gate may fire.
    expect_true(bf_finding(res) %in% c("Departure detected", "No departure detected"),
                info = paste(lbl, "- finding =", bf_finding(res)))
  }
})

test_that("a genuine departure from Benford's Law is still detected", {
  # Uniform mantissa scaled by a power of ten: leading digits uniform (as far
  # from Benford as it gets) while spanning 4 decades, so the range precondition
  # is met. NOT runif(500,100,999) - that spans one decade, where Benford's Law
  # does not apply. And NOT d * 10^U - that is log-uniform, i.e. Benford itself.
  skip_if_not_installed("benford.analysis")

  set.seed(7)
  res <- benford(data = data.frame(v = runif(500, 1, 10) * 10^sample(0:3, 500, TRUE)),
                 var = "v", digits = 2)
  expect_identical(bf_finding(res), "Departure detected")
  expect_match(bf_assessment(res), "departs from Benford|departure from Benford")
  # the finding must show its basis: the test used, and the size relative to noise
  expect_match(bf_assessment(res), "chi-square|Chi-square")
})

test_that("the finding never attributes a cause or grades concern", {
  # The module reports what it measured; whether that is a problem depends on how
  # the variable was collected and is the reader's call. It must never tell a
  # clinician their data shows manipulation, nor grade it High/Moderate/Low.
  skip_if_not_installed("benford.analysis")

  set.seed(11)
  cases <- list(
    data.frame(v = runif(600, 1, 10) * 10^sample(0:3, 600, TRUE)),  # departure
    data.frame(v = benford_data(600, 21)),                          # conforming
    data.frame(v = runif(400, 100, 999)),                           # narrow range
    data.frame(v = benford_data(60, 22)))                           # tiny sample

  for (i in seq_along(cases)) {
    res <- benford(data = cases[[i]], var = "v", digits = 1)
    all_text <- paste(bf_txt(res$reportSentence$content), bf_assessment(res),
                      bf_txt(res$explanation$content), bf_txt(res$todo$content),
                      as.character(res$notices$content))
    for (word in c("manipulation", "IMMEDIATE REVIEW", "fraud", "does NOT conform"))
      expect_false(grepl(word, all_text, fixed = TRUE),
                   info = paste("case", i, "contains:", word))
    expect_false(grepl("(High|Moderate|Low) concern", all_text),
                 info = paste("case", i, "grades concern"))
    expect_true(bf_finding(res) %in% c("Departure detected", "No departure detected",
                                       "Not assessable", "Limited evidence"),
                info = paste("case", i, "finding =", bf_finding(res)))
  }
})

test_that("a variable spanning less than two decades is not assessed", {
  # Benford's Law describes data spanning several orders of magnitude. Below two
  # decades a departure is arithmetic, not evidence about recording. Measured on
  # ordinary correctly-recorded clinical variables at N=400, the pre-fix code
  # returned its largest departure for platelet counts (1.14 decades) and serum
  # creatinine (1.61) while simultaneously warning the result was meaningless.
  skip_if_not_installed("benford.analysis")

  set.seed(3)
  narrow <- list(runif(400, 100, 999),                       # 1.00 decades
                 pmax(10, round(rnorm(400, 250, 90))),       # ~1.1, platelet-like
                 pmax(1, pmin(100, round(rbeta(400, 2, 3) * 100))))  # ~1.6, Ki67-like
  for (i in seq_along(narrow)) for (dg in 1:2) {
    res <- benford(data = data.frame(v = narrow[[i]]), var = "v", digits = dg)
    expect_identical(bf_finding(res), "Not assessable",
                     info = paste("narrow case", i, "digits", dg))
    expect_match(bf_assessment(res), "orders of magnitude", info = paste("case", i))
  }

  # ... and a wide-range variable is still assessed
  res <- benford(data = data.frame(v = benford_data(600, 31)), var = "v", digits = 1)
  expect_identical(bf_finding(res), "No departure detected")
})

test_that("the noise floor matches simulation", {
  # E|p_hat - p| = sqrt(2 p (1-p) / (pi n)) per bin, averaged over bins.
  skip_if_not_installed("benford.analysis")

  emad <- bf_private()$.expectedMadUnderNull

  for (dg in c(1, 2, 3)) {
    for (n in c(1000, 5000)) {
      set.seed(9)
      sim <- median(replicate(15, benford.analysis::benford(
        10^runif(n, 0, 4), number.of.digits = dg)$MAD))
      analytic <- emad(n, dg)
      # The per-bin normal approximation is loosest for 1-digit analysis, where
      # there are only 9 bins and p is large; 25% is comfortably tight enough to
      # catch a wrong formula while tolerating that.
      expect_lt(abs(analytic - sim) / sim, 0.25,
                label = sprintf("digits=%d n=%d analytic=%.6f sim=%.6f",
                                dg, n, analytic, sim))
    }
  }
})

test_that("the minimum n for a trustworthy MAD label is digit-specific", {
  priv <- bf_private()
  minN <- priv$.minNForMadLabel
  reliable <- priv$.madLabelIsReliable

  # The gate compares Nigrini's cut-off (0.015 / 0.0022 / 0.0005) to the noise
  # floor and demands a factor of 2 of headroom. A bare `cutoff > floor` is not
  # enough: it tests the MEAN noise MAD against the TOP cut-off only, while MAD
  # has spread around that mean and three lower cut-offs (Close / Acceptable /
  # Marginally acceptable) also feed the verdict. Measured on exactly-Benford
  # 10^U data (200 reps), share of runs given a verdict above "Low" under the
  # bare gate: 1 digit n = 300 -> 64% (30% of them the full "High" / "does NOT
  # conform ... or manipulation" text), n = 500 -> 25%; 2 digits n = 2000 ->
  # 40%. With the headroom every one of those cells sits at the 2-5% baseline
  # the chi-square branch produces.
  expect_equal(ceiling(minN(1)), 981)
  expect_equal(ceiling(minN(2)), 5204)
  expect_equal(ceiling(minN(3)), 10200)

  # the three cells that produced the false alarms must NOT use the MAD label
  expect_false(reliable(300, 1))
  expect_false(reliable(500, 1))
  expect_false(reliable(2000, 2))
  # 2-digit analysis (the default) is unreliable at every size the module suggests
  expect_false(reliable(1000, 2))

  # minN and the gate must agree exactly: every user-facing "needs N > ..."
  # message is generated from minN.
  for (d in 1:3) {
    n_min <- ceiling(minN(d))
    expect_false(reliable(n_min - 1, d), info = paste("digits =", d))
    expect_true(reliable(n_min + 1, d), info = paste("digits =", d))
  }

  # 1-digit analysis still needs far fewer observations than 2-digit, and a
  # large enough sample does get the label
  expect_true(reliable(2000, 1))
  expect_true(reliable(20000, 2))
})

test_that("the suspicious-values panel lists the flagged rows, not the first rows", {
  # suspect_indices came from as.numeric(rownames(getSuspects(...))), but
  # getSuspects returns a data.table whose rownames are reset to 1..nrow, so the
  # indices were always 1, 2, 3, ... and the panel listed the FIRST n rows of the
  # dataset under the heading "Suspicious Data Points".
  skip_if_not_installed("benford.analysis")

  x <- benford_data(2000, 11)
  res <- benford(data = data.frame(v = x), var = "v", digits = 2)
  panel <- as.character(res$text2$content)

  b <- benford.analysis::benford(x, number.of.digits = 2)
  flagged <- as.numeric(benford.analysis::getSuspects(
    bfd = b, data = data.frame(value = x))$value)
  expect_gt(length(flagged), 0)

  # The panel prints "row  value" pairs. Parse them and check the invariant that
  # was broken: the printed row number must actually hold the printed value.
  pairs <- regmatches(panel, gregexpr("(?m)^\\s*([0-9]+)\\s+([0-9.]+)\\s*$", panel, perl = TRUE))[[1]]
  expect_gt(length(pairs), 5)

  rows <- as.integer(sub("^\\s*([0-9]+).*", "\\1", pairs))
  vals <- as.numeric(sub("^\\s*[0-9]+\\s+", "", pairs))

  expect_true(all(abs(x[rows] - vals) < 1e-4))          # row really holds value
  expect_true(all(vapply(vals, function(v)              # and value really flagged
    any(abs(flagged - v) < 1e-6), logical(1))))
  expect_false(identical(rows, seq_along(rows)))        # not just 1, 2, 3, ...
})

test_that("the MAD matches a hand-computed first-two-digit deviation", {
  skip_if_not_installed("benford.analysis")

  x <- benford_data(2000, 11)
  b <- benford.analysis::benford(x, number.of.digits = 2)

  d2 <- as.numeric(substr(gsub("\\.", "", formatC(x, format = "e", digits = 10)), 1, 2))
  obs <- as.numeric(table(factor(d2, levels = 10:99))) / length(x)
  expected <- log10(1 + 1 / (10:99))

  expect_equal(b$MAD, mean(abs(obs - expected)), tolerance = 1e-9)
})

test_that("the welcome panel is hidden once a variable is selected", {
  # `visible: (!var)` never worked: jmvcore only treats a visible string as an
  # expression when it starts with "(" plus a letter, so a leading "!" was
  # returned as a raw truthy string and the (empty) "Getting Started" box stayed
  # on screen for every run.
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = benford_data(300, 1)), var = "v", digits = 2)
  expect_false(res$welcome$visible)
})

test_that("no Html panel is truncated by a raw '<'", {
  # A raw "<" in a string bound for an Html item opens what the renderer reads
  # as a tag and swallows everything up to the next ">". "(N<100)" silently ate
  # the whole small-sample report sentence after "observations (N"; .fmtP's
  # "< 0.0001" had the same trap and is escaped to &lt;. Assert the sentences
  # survive rendering rather than trusting that every author remembers.
  skip_if_not_installed("benford.analysis")

  strip <- function(x) gsub("<[^>]*>", " ", as.character(x))   # what the renderer keeps
  cases <- list(list(v = benford_data(60, 41),  d = 1, tail = "does not establish"),
                list(v = benford_data(600, 42), d = 1, tail = "observations using"))
  for (i in seq_along(cases)) {
    res <- benford(data = data.frame(v = cases[[i]]$v), var = "v", digits = cases[[i]]$d)
    for (item in c("reportSentence", "explanation", "dataWarning", "todo"))
      expect_false(grepl("<[0-9 =]", as.character(res[[item]]$content)),
                   info = paste("case", i, item, "has a raw comparison '<'"))
    expect_match(strip(res$reportSentence$content), cases[[i]]$tail, info = paste("case", i))
  }
})

test_that("the bin listing shows the user's own variable name, unescaped", {
  # .escapeVar() rendered a column named 'Serum Na+ (mmol/L) <lab>' back to the
  # user as 'Serum_Na_mmol_L_lab_'. text2 is a Preformatted item, which the
  # jamovi client renders with innerText (not an HTML sink), and
  # print.data.frame reproduces any name faithfully, so nothing needed escaping.
  # do.call passes the name as a LITERAL: jmvcore::resolveQuo() on a bare symbol
  # returns the SYMBOL'S OWN NAME, so benford(var = nm) would ask for a column
  # called "nm" and die in select() before reaching any module code.
  skip_if_not_installed("benford.analysis")

  nm <- 'Serum Na+ (mmol/L) <lab> & "co"'
  d <- data.frame(benford_data(400, 51)); names(d) <- nm
  res <- do.call(benford, list(data = d, var = nm, digits = 1))

  expect_match(as.character(res$text2$content), nm, fixed = TRUE)
  expect_false(grepl("Serum_Na_mmol_L", as.character(res$text2$content), fixed = TRUE))
})

test_that("notice levels render distinctly and most severe first", {
  # Both STRONG_WARNING and WARNING used to render the literal "WARNING: ", so
  # the level the code computed was invisible. Three levels are now used and each
  # renders distinctly. Notices are also appended in whatever order the code
  # reaches them, so an analysis-stopping ERROR could sit below an advisory one:
  # Inf values AND fewer than 30 valid observations adds the non-finite WARNING
  # first and the stop ERROR second.
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = c(benford_data(20, 61), Inf, -Inf, NaN)),
                 var = "v", digits = 1)
  txt <- as.character(res$notices$content)
  expect_match(txt, "ERROR: ", fixed = TRUE)
  expect_match(txt, "WARNING: ", fixed = TRUE)
  expect_lt(regexpr("ERROR: ", txt, fixed = TRUE),      # ERROR precedes WARNING
            regexpr("WARNING: ", txt, fixed = TRUE))
  # the retired level must not leak its internal code into the rendered text
  expect_false(grepl("STRONG_WARNING", txt, fixed = TRUE))

  # INFO renders as its own prefix, not as a warning
  ok <- benford(data = data.frame(v = benford_data(400, 62)), var = "v", digits = 2)
  expect_match(as.character(ok$notices$content), "NOTE: ", fixed = TRUE)
})

test_that("the plot state carries no per-observation table", {
  # $data and $s.o.data hold one row PER OBSERVATION and pushed the serialized
  # state past jmvcore's 500 KB warning at n ~ 25000 (measured 513,158 bytes;
  # 7,728 after the trim). No panel the renderer draws reads either: the sole
  # reader is the mantissa panel, excluded by the `except` pinned in .plot().
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = benford_data(2000, 71)), var = "v", digits = 2)
  st <- res$plot$state
  expect_s3_class(st, "Benford")          # class must survive the trim
  expect_null(st$data)
  expect_null(st$s.o.data)
  expect_equal(nrow(st$bfd), 90L)         # the aggregated table the plot needs
})

test_that("every translated template is a jmvcore::format placeholder string", {
  # sprintf()/glue() with a .() template crash .run() (sprintf) or EVAL the
  # brace contents (glue) if a translator drops, reorders or mistypes a
  # specifier. jmvcore::format does plain substitution and degrades to an
  # ellipsis. This asserts the migration stays migrated.
  src <- readLines("../../R/benford.b.R", warn = FALSE)
  code <- grep("^\\s*#", src, invert = TRUE, value = TRUE)
  # a .() literal opening on the same line as sprintf( / glue::glue(
  expect_length(grep('sprintf\\(\\s*\\.\\("', code), 0)
  expect_length(grep('glue::glue\\(\\s*\\.\\("', code), 0)
  # ... or on the line directly after it
  opens <- grep("(sprintf|glue::glue)\\($", code)
  if (length(opens))
    expect_false(any(grepl('^\\s*\\.\\("', code[opens + 1])))
})

test_that("no jmvcore::format placeholder can swallow its own template", {
  # jmvcore::format(str, ..., context). `str` sits BEFORE `...`, so R partial
  # matching binds an argument named s/st/str to the TEMPLATE slot: the real
  # template slides into `...` and the call returns only that one value, with
  # the whole sentence gone and no error raised. Underscored placeholders ship
  # as literal braces. Both are silent, so they are asserted here rather than
  # left to review.
  exprs <- parse("../../R/benford.b.R")
  problems <- character(0)
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- e[[1]]
    if (is.call(fn) && length(fn) == 3 &&
        identical(as.character(fn[[1]]), "::") &&
        identical(as.character(fn[[3]]), "format") &&
        identical(as.character(fn[[2]]), "jmvcore")) {
      args <- as.list(e)[-1]
      tmpl <- args[[1]]
      lit <- if (is.call(tmpl) && identical(as.character(tmpl[[1]]), "."))
        tmpl[[2]] else tmpl
      nms <- names(args)
      if (is.null(nms)) nms <- rep("", length(args))
      nms[1] <- ""
      supplied <- nms[nzchar(nms)]
      if (is.character(lit) && length(lit) == 1) {
        ph <- gsub("[{}]", "", regmatches(lit, gregexpr("\\{[^}]*\\}", lit))[[1]])
        tag <- substr(lit, 1, 40)
        if (length(setdiff(ph, supplied)))
          problems <<- c(problems, paste("placeholder without argument:", tag))
        if (length(setdiff(supplied, ph)))
          problems <<- c(problems, paste("argument without placeholder:", tag))
        if (length(grep("_", ph)))
          problems <<- c(problems, paste("underscored placeholder:", tag))
        if (length(intersect(supplied, c("s", "st", "str", "context"))))
          problems <<- c(problems, paste("SWALLOWS TEMPLATE:", tag))
      }
    }
    for (i in seq_along(e)) {
      x <- tryCatch(e[[i]], error = function(err) NULL)
      if (!is.null(x)) walk(x)
    }
  }
  for (e in exprs) walk(e)
  expect_equal(problems, character(0))
})

test_that("the bin listing caps at 100 rows and says how many it withheld", {
  # Membership scales with the data (1820 rows at n=5000, 1 digit), and every
  # listed row is serialized into the saved .omv on each run. The cap line is
  # itself a translated template, so exercise it rather than trusting the count.
  skip_if_not_installed("benford.analysis")

  res <- benford(data = data.frame(v = benford_data(6000, 83)), var = "v", digits = 1)
  panel <- as.character(res$text2$content)

  n_flagged <- as.integer(sub(".*bins: ([0-9]+) /.*", "\\1",
                              regmatches(panel, regexpr("bins: [0-9]+ /", panel))))
  expect_gt(n_flagged, 100)

  # the withheld-count sentence rendered, with all three numbers substituted
  expect_match(panel, sprintf("and %d more \\(showing the first 100 of %d\\)",
                              n_flagged - 100, n_flagged))
  expect_false(grepl("{", panel, fixed = TRUE))   # no unsubstituted placeholder
  expect_false(grepl("\u{2026}", panel, fixed = TRUE))  # no jmvcore miss marker

  # exactly 100 data rows are printed
  pairs <- regmatches(panel, gregexpr("(?m)^\\s*[0-9]+\\s+[0-9.]+\\s*$",
                                      panel, perl = TRUE))[[1]]
  expect_equal(length(pairs), 100L)
})
