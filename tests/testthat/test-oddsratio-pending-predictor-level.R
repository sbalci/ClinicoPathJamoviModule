library(testthat)

# An empty "Predictor Positive Level" must make the diagnostic metrics WAIT for the
# user's choice. The positive level used to be guessed (keyword match on
# "Positive"/"Yes"/"High"/..., else the second factor level), and a wrong guess
# silently swaps sensitivity with 1 - specificity. With `allowNone: true` the jamovi
# client now sends NULL for an empty box, so NULL is the path a GUI user takes.

strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
diag_txt <- function(res) strip(res$diagnosticMetrics$content)
info_txt <- function(res) strip(res$infoMessages$content)
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

or_df <- function(n = 240, seed = 11) {
    set.seed(seed)
    d <- data.frame(grade = factor(sample(c("Low", "High"), n, TRUE)),
                    age = round(rnorm(n, 60, 10), 1))
    lp <- -1 + 0.9 * (d$grade == "High") + 0.02 * (d$age - 60)
    d$dead <- factor(ifelse(rbinom(n, 1, plogis(lp)) == 1, "Dead", "Alive"),
                     levels = c("Alive", "Dead"))
    d
}
level_counts <- function(x) {
    tb <- table(x)
    paste(sprintf("%s (n = %d)", names(tb), as.integer(tb)), collapse = ", ")
}

test_that("an empty predictor level leaves the diagnostic metrics waiting; the model still runs", {
    d <- or_df()
    counts <- level_counts(d$grade)
    expect_identical(counts, "High (n = 123), Low (n = 117)")

    for (empty in list(NULL, NA_character_, "", c("High", "Low"))) {
        res <- quietly(oddsratio(data = d, explanatory = c("grade", "age"), outcome = "dead",
                                 outcomeLevel = "Dead", diagnosticPredictor = "grade",
                                 predictorLevel = empty, showNomogram = TRUE))
        label <- paste("predictorLevel =", deparse(empty))

        dtxt <- diag_txt(res)
        expect_match(dtxt, "waiting", fixed = TRUE, label = label)
        expect_match(dtxt, "Predictor Positive Level", fixed = TRUE, label = label)
        expect_match(dtxt, counts, fixed = TRUE, label = label)
        # No metric is computed from a guessed level.
        for (metric in c("Sensitivity:", "Specificity:", "Positive LR:", "Negative LR:",
                         "automatically detected"))
            expect_false(grepl(metric, dtxt, fixed = TRUE), label = paste(label, metric))

        # INFO notice, not an error or a warning.
        itxt <- info_txt(res)
        expect_match(itxt, "Predictor Positive Level", fixed = TRUE, label = label)
        expect_match(itxt, "'grade'", fixed = TRUE, label = label)
        expect_match(itxt, counts, fixed = TRUE, label = label)
        expect_false(grepl("Predictor Positive Level", strip(res$errors$content), fixed = TRUE))
        expect_false(grepl("Predictor Positive Level", strip(res$warnings$content), fixed = TRUE))

        # The logistic model, odds ratios and nomogram are unaffected.
        or_html <- strip(res$text$content)
        expect_match(or_html, "[0-9]+\\.[0-9]{2} \\([0-9.]+-[0-9.]+, p", label = label)
        expect_match(itxt, "Odds ratio analysis completed successfully.", fixed = TRUE)
        expect_false(is.null(res$plot_nomogram$state), label = label)
    }
})

test_that("with no Diagnostic Predictor chosen, the message says where the level list lives", {
    # predictorLevel is a LevelSelector bound to the Diagnostic Predictor box, so
    # when the first explanatory variable is used by default the GUI cannot offer
    # its levels until that variable is placed in the box.
    d <- or_df()
    res <- quietly(oddsratio(data = d, explanatory = "grade", outcome = "dead",
                             outcomeLevel = "Dead", predictorLevel = NULL,
                             showNomogram = TRUE))
    dtxt <- diag_txt(res)
    expect_match(dtxt, "waiting", fixed = TRUE)
    expect_match(dtxt, level_counts(d$grade), fixed = TRUE)
    expect_match(dtxt, "Diagnostic Predictor", fixed = TRUE)
    expect_false(grepl("Sensitivity:", dtxt, fixed = TRUE))
})

test_that("a chosen predictor level still gives the epiR::epi.tests() metrics", {
    skip_if_not_installed("epiR")
    d <- or_df()
    for (pos in c("High", "Low")) {
        res <- quietly(oddsratio(data = d, explanatory = "grade", outcome = "dead",
                                 outcomeLevel = "Dead", diagnosticPredictor = "grade",
                                 predictorLevel = pos, showNomogram = TRUE))
        neg <- setdiff(c("High", "Low"), pos)
        tb <- table(factor(d$grade, levels = c(pos, neg)),
                    factor(d$dead, levels = c("Dead", "Alive")))
        det <- as.data.frame(epiR::epi.tests(
            matrix(as.vector(t(tb)), nrow = 2, byrow = TRUE))$detail)
        ref <- function(s) as.numeric(det[det$statistic == s, c("est", "lower", "upper")][1, ])
        se <- ref("se"); sp <- ref("sp"); lp <- ref("lr.pos"); ln <- ref("lr.neg")

        txt <- diag_txt(res)
        expect_match(txt, sprintf("Sensitivity: %.1f%% \\(95%% CI %.1f-%.1f%%\\)",
                                  se[1] * 100, se[2] * 100, se[3] * 100))
        expect_match(txt, sprintf("Specificity: %.1f%% \\(95%% CI %.1f-%.1f%%\\)",
                                  sp[1] * 100, sp[2] * 100, sp[3] * 100))
        expect_match(txt, sprintf("Positive LR: %.2f \\(95%% CI %.2f-%.2f\\)", lp[1], lp[2], lp[3]))
        expect_match(txt, sprintf("Negative LR: %.2f \\(95%% CI %.2f-%.2f\\)", ln[1], ln[2], ln[3]))
        expect_match(txt, sprintf("used as the positive/exposure category as specified"), fixed = TRUE)
        expect_false(grepl("waiting", txt, fixed = TRUE))
    }
})

test_that("a predictor level that is not a level of the predictor keeps its error message", {
    d <- or_df()
    res <- quietly(oddsratio(data = d, explanatory = "grade", outcome = "dead",
                             outcomeLevel = "Dead", diagnosticPredictor = "grade",
                             predictorLevel = "Medium", showNomogram = TRUE))
    msg <- "The selected positive predictor level 'Medium' is not present among paired complete observations. Available levels: High, Low."
    expect_match(diag_txt(res), msg, fixed = TRUE)
    expect_match(strip(res$warnings$content), msg, fixed = TRUE)
    expect_false(grepl("Sensitivity:", diag_txt(res), fixed = TRUE))
    expect_false(grepl("waiting", diag_txt(res), fixed = TRUE))
})
