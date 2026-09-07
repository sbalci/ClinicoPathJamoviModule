# .escapeVariableNames() -- R/formula_utils.R
#
# This helper was deleted by accident on 2026-09-06 (commit 0247c20f), which
# extracted the formula helpers from R/utils.R into R/formula_utils.R. Its
# immediate neighbour .stripBackticks() made the move; this one was dropped with
# the other 318 removed lines. Nothing caught it: the callers are inside R6
# methods, where lintr's object_usage linter is blind, and the package still
# builds -- every Cox model routed through .buildSurvivalFormula() simply died
# at RUNTIME with "could not find function '.escapeVariableNames'".
#
# The first test below is the one that would have caught it, and it is
# deliberately a bare existence + call check rather than a behavioural one.

if (!exists(".escapeVariableNames"))
  source(file.path("..", "..", "R", "formula_utils.R"))

test_that("the helper exists and every caller can reach it", {
  expect_true(is.function(.escapeVariableNames))

  # Guard the distribution invariant too: each umbrella file that CALLS the
  # helper must be shipped alongside a file that DEFINES it. formula_utils.R is
  # in the r_files of every module that ships one of these callers.
  root <- file.path("..", "..")
  callers <- c("R/survival_utils.R", "R/multisurvival.b.R",
               "R/multisurvival-interactions.R")
  for (f in callers) {
    p <- file.path(root, f)
    skip_if_not(file.exists(p), paste(f, "not present"))
    txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
    expect_true(grepl(".escapeVariableNames(", txt, fixed = TRUE), info = f)
  }
  # ... and the definition lives in exactly one umbrella file, so no Collate
  # ordering can shadow it with a second copy.
  r_files <- list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  defs <- vapply(r_files, function(f)
    any(grepl("^\\.escapeVariableNames\\s*<-\\s*function",
              readLines(f, warn = FALSE))), logical(1))
  expect_equal(sum(defs), 1L)
})

test_that("names that R cannot parse bare are quoted", {
  skip_if_not_installed("jmvcore")
  # Every one of these produced an unparseable -- or silently WRONG -- formula
  # under the hand-rolled grepl("[^a-zA-Z0-9._]") rule this replaced.
  tricky <- c("1stGrade", "if", "for", "function", "a`b", "TRUE", "NA")
  for (nm in tricky) {
    f <- try(stats::as.formula(paste0("y ~ ", .escapeVariableNames(nm))), silent = TRUE)
    expect_false(inherits(f, "try-error"), info = nm)
    # It must resolve to the VARIABLE, never to a literal or a keyword.
    expect_equal(all.vars(f)[2], nm, info = nm)
  }
})

test_that("ordinary and punctuated names behave as before", {
  skip_if_not_installed("jmvcore")
  expect_equal(.escapeVariableNames("age"), "age")
  expect_equal(.escapeVariableNames("p.value"), "p.value")
  expect_equal(.escapeVariableNames("x_1"), "x_1")
  expect_equal(.escapeVariableNames("Tumor Grade"), "`Tumor Grade`")
  expect_equal(.escapeVariableNames("Ki-67 (%)"), "`Ki-67 (%)`")
})

test_that("vector input keeps length and order, and drops names", {
  skip_if_not_installed("jmvcore")
  # Callers paste() the result against parallel vectors (factor levels), so a
  # dropped or reordered element silently mislabels a coefficient.
  v <- c("age", "Tumor Grade", "x_1", "Ki-67 (%)")
  out <- .escapeVariableNames(v)
  expect_length(out, 4L)
  expect_equal(out[c(1, 3)], c("age", "x_1"))
  expect_null(names(out))
  expect_equal(.escapeVariableNames(character(0)), character(0))
})

test_that("escaped names match the coefficient names coxph actually produces", {
  skip_if_not_installed("survival")
  skip_if_not_installed("jmvcore")
  # multisurvival-interactions.R rebuilds coefficient names as
  # paste0(.escapeVariableNames(focal), level) and matches them against
  # names(coef(fit)). If the escaper and coxph's deparsing ever disagree, the
  # interaction terms silently fail to match and report nothing.
  set.seed(1)
  n <- 200
  for (vn in c("grade", "Tumor Grade", "Ki-67 (%)", "1stGrade")) {
    d <- data.frame(t = stats::rexp(n, 0.1), s = stats::rbinom(n, 1, 0.6))
    d[[vn]] <- factor(sample(c("Low", "High"), n, TRUE), levels = c("Low", "High"))
    fml <- stats::as.formula(sprintf("survival::Surv(t, s) ~ %s",
                                     .escapeVariableNames(vn)))
    fit <- survival::coxph(fml, data = d)
    expect_equal(names(stats::coef(fit))[1],
                 paste0(.escapeVariableNames(vn), "High"), info = vn)
  }
})
