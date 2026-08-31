library(testthat)

# Numerical verification for `oddsratio()`.
#
# These tests compare what the analysis RENDERS against values computed
# independently with stats::glm, logistf and epiR. The previous version of this
# file built reference fits and then asserted only `!is.null(res$text)`, which
# passes just as happily when every number on screen is wrong.

# The odds-ratio table is rendered as HTML by knitr::kable; recover the cells.
or_text <- function(res) {
    gsub("[[:space:]]+", " ",
         gsub("<[^>]*>", " ", paste(as.character(res$text$content), collapse = " ")))
}
# finalfit's p-value convention is "p<0.001" below the threshold, and kable
# HTML-escapes the "<" to "&lt;". Matching only "p=" silently DROPPED every
# strongly-significant row, which is exactly the row a reader cares about most.
OR_CELL_RE <- "[0-9]+\\.[0-9]{2} \\([0-9.]+-[0-9.]+, p(=[0-9.]+|(&lt;|<)[0-9.]+)\\)"
or_cells <- function(res) {
    regmatches(or_text(res), gregexpr(OR_CELL_RE, or_text(res)))[[1]]
}
# Cells of one named OR column only. The table is rendered row-major, so the
# univariable and multivariable cells of a row alternate; `which` picks the
# 1st or 2nd of each pair.
or_cells_col <- function(res, which = c("univariable", "multivariable")) {
    which <- match.arg(which)
    cells <- or_cells(res)
    if (length(cells) == 0) return(cells)
    cells[seq(if (which == "univariable") 1 else 2, length(cells), by = 2)]
}
or_nums <- function(cell) as.numeric(regmatches(cell, gregexpr("[0-9]+\\.[0-9]+", cell))[[1]])
diag_text <- function(res) {
    gsub("[[:space:]]+", " ",
         gsub("<[^>]*>", " ", paste(as.character(res$diagnosticMetrics$content), collapse = " ")))
}
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

or_df <- function(n = 240, seed = 11) {
    set.seed(seed)
    d <- data.frame(
        grade = factor(sample(c("Low", "High"), n, TRUE)),
        stage = factor(sample(c("I", "II", "III"), n, TRUE)),
        age   = round(rnorm(n, 60, 10), 1))
    lp <- -1 + 0.9 * (d$grade == "High") + 0.5 * (d$stage == "III") + 0.02 * (d$age - 60)
    d$dead <- factor(ifelse(rbinom(n, 1, plogis(lp)) == 1, "Dead", "Alive"),
                     levels = c("Alive", "Dead"))
    d
}

test_that("multivariable odds ratios, CIs and p-values match stats::glm", {
    df <- or_df()
    res <- quietly(oddsratio(data = df, explanatory = c("grade", "stage"),
                             outcome = "dead", outcomeLevel = "Dead",
                             predictorLevel = NULL))

    ref <- stats::glm(dead ~ grade + stage, data = df, family = stats::binomial)
    est <- exp(stats::coef(ref))
    ci  <- exp(suppressMessages(stats::confint(ref)))   # profile likelihood, as finalfit uses
    p   <- summary(ref)$coefficients[, 4]

    cells <- or_cells(res)
    expect_gte(length(cells), 3)
    got <- do.call(rbind, lapply(cells, or_nums))

    for (term in c("gradeLow", "stageII", "stageIII")) {
        want <- c(est[[term]], ci[term, 1], ci[term, 2], p[[term]])
        hit <- which(abs(got[, 1] - want[1]) < 0.006 &
                     abs(got[, 2] - want[2]) < 0.006 &
                     abs(got[, 3] - want[3]) < 0.006)
        expect_gt(length(hit), 0, label = paste("odds ratio row", term))
    }
})

test_that("choosing the other outcome level inverts the odds ratios", {
    # The single most consequential option in this analysis: if the positive
    # level is applied to the wrong side, every odds ratio is reported upside
    # down while still looking entirely plausible.
    df <- or_df()
    a <- quietly(oddsratio(data = df, explanatory = "grade", outcome = "dead",
                           outcomeLevel = "Dead", predictorLevel = NULL))
    b <- quietly(oddsratio(data = df, explanatory = "grade", outcome = "dead",
                           outcomeLevel = "Alive", predictorLevel = NULL))

    or_a <- or_nums(or_cells(a)[1])[1]
    or_b <- or_nums(or_cells(b)[1])[1]
    expect_equal(or_b, 1 / or_a, tolerance = 0.01)

    ref <- exp(stats::coef(stats::glm(dead ~ grade, data = df, family = stats::binomial)))[["gradeLow"]]
    expect_equal(or_a, unname(round(ref, 2)), tolerance = 0.006)
})

test_that("Firth penalized odds ratios match logistf", {
    skip_if_not_installed("logistf")
    df <- or_df()
    res <- quietly(oddsratio(data = df, explanatory = c("grade", "stage"),
                             outcome = "dead", outcomeLevel = "Dead",
                             predictorLevel = NULL, usePenalized = TRUE))

    # The penalized table mirrors the maximum-likelihood table's structure: a
    # reference row per factor (rendered "-", so not an OR cell) plus BOTH a
    # univariable and a multivariable Firth column. Each column is checked
    # against its own logistf fit -- a multivariable-only check would let a
    # wrong univariable column through unnoticed.
    fit_nums <- function(fit) {
        keep <- names(stats::coef(fit)) != "(Intercept)"
        data.frame(or = exp(stats::coef(fit))[keep],
                   lo = exp(fit$ci.lower)[keep],
                   hi = exp(fit$ci.upper)[keep])
    }

    want_multi <- fit_nums(logistf::logistf(dead ~ grade + stage, data = df))
    want_uni   <- do.call(rbind, lapply(c("grade", "stage"), function(v)
        fit_nums(logistf::logistf(stats::as.formula(paste("dead ~", v)), data = df))))

    for (col in c("univariable", "multivariable")) {
        want <- if (col == "univariable") want_uni else want_multi
        got  <- do.call(rbind, lapply(or_cells_col(res, col), or_nums))
        expect_equal(nrow(got), nrow(want), label = paste("Firth", col, "cell count"))
        for (i in seq_len(nrow(want))) {
            hit <- which(abs(got[, 1] - want$or[i]) < 0.006 &
                         abs(got[, 2] - want$lo[i]) < 0.006 &
                         abs(got[, 3] - want$hi[i]) < 0.006)
            expect_gt(length(hit), 0,
                      label = paste("Firth", col, "row", rownames(want)[i]))
        }
    }

    # Reference rows must be present and rendered as "-", not as a spurious OR.
    expect_true(grepl("High", or_text(res)))
    expect_true(grepl("\\bI\\b", or_text(res)))
})

test_that("diagnostic metrics and their CIs match epiR::epi.tests", {
    skip_if_not_installed("epiR")
    df <- or_df()
    res <- quietly(oddsratio(data = df, explanatory = "grade", outcome = "dead",
                             outcomeLevel = "Dead", diagnosticPredictor = "grade",
                             predictorLevel = "High", showNomogram = TRUE))

    tb <- table(factor(df$grade, levels = c("High", "Low")),
                factor(df$dead,  levels = c("Dead", "Alive")))
    det <- as.data.frame(epiR::epi.tests(
        matrix(as.vector(t(tb)), nrow = 2, byrow = TRUE))$detail)
    ref <- function(s) as.numeric(det[det$statistic == s, c("est", "lower", "upper")][1, ])

    txt <- diag_text(res)
    se <- ref("se"); sp <- ref("sp"); lp <- ref("lr.pos"); ln <- ref("lr.neg")

    expect_match(txt, sprintf("Sensitivity: %.1f%% \\(95%% CI %.1f-%.1f%%\\)",
                              se[1] * 100, se[2] * 100, se[3] * 100))
    expect_match(txt, sprintf("Specificity: %.1f%% \\(95%% CI %.1f-%.1f%%\\)",
                              sp[1] * 100, sp[2] * 100, sp[3] * 100))
    expect_match(txt, sprintf("Positive LR: %.2f \\(95%% CI %.2f-%.2f\\)", lp[1], lp[2], lp[3]))
    expect_match(txt, sprintf("Negative LR: %.2f \\(95%% CI %.2f-%.2f\\)", ln[1], ln[2], ln[3]))
})

# ---------------------------------------------------------------- regressions

test_that("a separated fit reports 'not estimable' instead of an astronomical OR", {
    # Regression: with a perfectly separated 2x2, glm's IRLS stops at whatever
    # coefficient the iteration limit allows and finalfit rendered exp() of it as
    #   "118848049086800030859264.00 (0.00-Inf, p=1.000)"
    # -- an odds ratio of 1.19e23 printed to two decimal places as if estimated.
    sep <- data.frame(x = factor(rep(c("A", "B"), each = 30)),
                      y = factor(c(rep("No", 30), rep("Yes", 30)), levels = c("No", "Yes")))
    res <- quietly(oddsratio(data = sep, explanatory = "x", outcome = "y",
                             outcomeLevel = "Yes", predictorLevel = NULL))

    txt <- or_text(res)
    expect_match(txt, "not estimable")
    expect_false(grepl("118848049086800030859264", txt, fixed = TRUE))
    # no giant number of any kind survives in the table
    expect_length(regmatches(txt, gregexpr("[0-9]{12,}", txt))[[1]], 0L)
    # and the reason is stated, naming the variable
    warn <- paste(as.character(res$strongWarnings$content), collapse = " ")
    expect_match(warn, "could not be estimated")
    expect_match(warn, "\\bx\\b")
})

test_that("near-separation is caught too", {
    near <- data.frame(x = factor(rep(c("A", "B"), each = 30)),
                       y = factor(c(rep("No", 29), "Yes", rep("Yes", 30)),
                                  levels = c("No", "Yes")))
    res <- quietly(oddsratio(data = near, explanatory = "x", outcome = "y",
                             outcomeLevel = "Yes", predictorLevel = NULL))
    expect_match(or_text(res), "not estimable")
})

test_that("an ordinary analysis is untouched by the non-estimable guard", {
    # The guard keys on an unbounded confidence interval, so it must never fire
    # on a normal fit -- including one with a small p-value or a wide interval.
    df <- or_df()
    res <- quietly(oddsratio(data = df, explanatory = c("grade", "stage"),
                             outcome = "dead", outcomeLevel = "Dead",
                             predictorLevel = NULL))
    expect_false(grepl("not estimable", or_text(res)))
    expect_false(nzchar(trimws(gsub("<[^>]*>", "",
        paste(as.character(res$strongWarnings$content), collapse = "")))))
})

test_that("a large but genuinely estimable Firth odds ratio is preserved", {
    # Firth on the same separated data yields a finite, bounded estimate. That is
    # a real result and must survive the guard.
    skip_if_not_installed("logistf")
    sep <- data.frame(x = factor(rep(c("A", "B"), each = 30)),
                      y = factor(c(rep("No", 30), rep("Yes", 30)), levels = c("No", "Yes")))
    res <- quietly(oddsratio(data = sep, explanatory = "x", outcome = "y",
                             outcomeLevel = "Yes", predictorLevel = NULL,
                             usePenalized = TRUE))
    txt <- or_text(res)
    expect_false(grepl("not estimable", txt))

    fr <- logistf::logistf(y ~ x, data = sep)
    expect_match(txt, sprintf("%.2f", exp(stats::coef(fr))[["xB"]]), fixed = TRUE)
})

test_that("a degenerate 2x2 yields metrics without CIs rather than an error", {
    skip_if_not_installed("epiR")
    z <- data.frame(p = factor(c(rep("Pos", 20), rep("Neg", 20))),
                    o = factor(c(rep("D", 20), rep("H", 20)), levels = c("H", "D")))
    expect_no_error({
        res <- quietly(oddsratio(data = z, explanatory = "p", outcome = "o",
                                 outcomeLevel = "D", diagnosticPredictor = "p",
                                 predictorLevel = "Pos", showNomogram = TRUE))
    })
    txt <- diag_text(res)
    # A perfect test: LR+ diverges (not a number), LR- is exactly 0.
    expect_match(txt, "Positive LR: infinite")
    expect_match(txt, "Sensitivity: 100.0%")
})
