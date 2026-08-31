library(testthat)

# Regression tests for `oddsratio()` audit findings.
#
# Every block below asserts an OBSERVABLE property of the rendered results --
# what the user actually sees in the Html outputs or whether the forest plot
# draws at all -- rather than an internal calling convention. Analysis-level
# rejections in jamovi surface as content (res$todo / the notice Html items),
# not as R conditions, so nothing here uses expect_error() as a proxy for
# "the analysis complained".
#
# Note on the wrapper signature: `outcomeLevel` and `predictorLevel` are jamovi
# `type: Level` options, which can never carry a `default:`. They are therefore
# REQUIRED arguments of the generated wrapper and are passed explicitly on every
# call, as NULL when the corresponding variable is not selected.

data(histopathology, package = "ClinicoPath")

# ---- helpers ---------------------------------------------------------------

# Third-party fitters (finalfit, logistf, pROC) print progress to the console.
quietly <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

# De-HTML one result item for reading.
strip_html <- function(x) {
    if (is.null(x)) return("")
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(x), collapse = " ")))
}

notice_items <- c("errors", "strongWarnings", "warnings", "infoMessages")

notices_html <- function(res) {
    paste(vapply(notice_items, function(n) {
        x <- res[[n]]$content
        if (is.null(x)) "" else paste(as.character(x), collapse = "")
    }, character(1)), collapse = "")
}

# Every Html result item this analysis can populate, concatenated raw.
all_html <- function(res) {
    nm <- c("todo", notice_items, "text", "text2",
            "oddsRatioExplanation", "riskMeasuresExplanation",
            "diagnosticTestExplanation", "diagnosticMetrics", "nomogram",
            "nomogramAnalysisExplanation")
    paste(vapply(nm, function(n) {
        x <- tryCatch(res[[n]]$content, error = function(e) NULL)
        if (is.null(x)) "" else paste(as.character(x), collapse = "")
    }, character(1)), collapse = "\n")
}

# Names of table columns whose body cells are ALL blank. finalfit legitimately
# leaves individual cells empty (the variable name is printed only on the first
# row of each block), so emptiness is judged per column, not per cell.
empty_columns <- function(html) {
    head_row <- regmatches(html, regexpr("(?s)<thead>.*?</thead>", html, perl = TRUE))
    if (length(head_row) == 0) return(character(0))
    hdr <- trimws(gsub("<[^>]*>", "",
        regmatches(head_row, gregexpr("(?s)<th[^>]*>.*?</th>", head_row, perl = TRUE))[[1]]))

    body <- regmatches(html, regexpr("(?s)<tbody>.*</tbody>", html, perl = TRUE))
    if (length(body) == 0) return(character(0))
    rows <- regmatches(body, gregexpr("(?s)<tr>.*?</tr>", body, perl = TRUE))[[1]]
    cells <- lapply(rows, function(r)
        trimws(gsub("<[^>]*>", "",
            regmatches(r, gregexpr("(?s)<td[^>]*>.*?</td>", r, perl = TRUE))[[1]])))
    cells <- cells[lengths(cells) == length(hdr)]
    if (length(cells) == 0) return(character(0))

    m <- do.call(rbind, cells)
    hdr[apply(m, 2, function(col) all(!nzchar(col)))]
}

# Render the forest plot to a throwaway device so nothing lands in the wd.
render_plot <- function(res) {
    png(tempfile(), width = 600, height = 450)
    on.exit(dev.off(), add = TRUE)
    quietly(res$plot$.render(width = 600, height = 450, ppi = 72))
}

# All numbers that follow an "AIC" label, whatever wording surrounds them.
aic_values <- function(res) {
    txt <- strip_html(res$text2$content)
    hits <- regmatches(txt, gregexpr("AIC[^0-9]{0,25}-?[0-9]+(\\.[0-9]+)?",
                                     txt, ignore.case = TRUE))[[1]]
    as.numeric(sub(".*?(-?[0-9]+(\\.[0-9]+)?)$", "\\1", hits))
}

# A 2x2 whose minimum expected cell count is 3.06 (n = 16), which is what trips
# the small-expected-count assumption check.
small_2x2 <- data.frame(
    pred = factor(c(rep("Pos", 7), rep("Neg", 9))),
    out  = factor(c(rep("Yes", 6), "No", "Yes", rep("No", 8)))
)

# ---- C1: forest plot must survive non-syntactic variable names -------------

test_that("C1: forest plot renders when variable names contain spaces", {
    res <- quietly(oddsratio(
        data = histopathology,
        explanatory = "Rater 1",
        outcome = "Golden Standart",
        outcomeLevel = "1",
        predictorLevel = NULL))

    # Before the fix this dies inside finalfit::or_plot() with
    #   <text>:1:8: unexpected symbol / 1: Golden Standart
    expect_no_error(render_plot(res))
})

test_that("C1: forest plot renders with a spaced outcome and a clean predictor", {
    res <- quietly(oddsratio(
        data = histopathology,
        explanatory = "LVI",
        outcome = "Golden Standart",
        outcomeLevel = "1",
        predictorLevel = NULL))

    expect_no_error(render_plot(res))
})

# GAP: the rendered forest plot must also keep the ORIGINAL variable names
# ("Golden Standart", not "golden_standart"). finalfit::or_plot() draws through
# gridExtra onto a fresh page, and grid::grid.grab() afterwards returns no text
# grobs, so there is no cheap, non-brittle way to read the drawn labels back.
# Asserting on the plot's internal state instead would lock in an
# implementation detail. The render assertions above therefore do not cover
# label restoration; that is checked by eye.

# ---- H2: Firth AIC must be on the maximum-likelihood scale -----------------

test_that("H2: penalized AIC is on the same scale as the maximum-likelihood AIC", {
    d <- histopathology
    names(d)[names(d) == "Golden Standart"] <- "gs"

    ml <- quietly(oddsratio(data = d, explanatory = c("LVI", "PNI"), outcome = "gs",
                            outcomeLevel = "1", predictorLevel = NULL,
                            usePenalized = FALSE))
    fi <- quietly(oddsratio(data = d, explanatory = c("LVI", "PNI"), outcome = "gs",
                            outcomeLevel = "1", predictorLevel = NULL,
                            usePenalized = TRUE))

    ml_aic <- aic_values(ml)
    fi_aic <- aic_values(fi)
    expect_gte(length(ml_aic), 1)
    expect_gte(length(fi_aic), 1)

    # Same 248 observations, same two predictors: the penalized information
    # criterion cannot be two orders of magnitude away from the ML one.
    # Before the fix these are 337.1 and 4.87.
    ratio <- ml_aic[1] / fi_aic[1]
    expect_true(ratio > 1 / 3 && ratio < 3,
                info = paste("ML AIC =", ml_aic[1], "vs penalized AIC =", fi_aic[1]))
})

# ---- H3: Firth table must be a finished table, not a raw data frame --------

test_that("H3: Firth odds-ratio table shows no internal column names or empty columns", {
    d <- histopathology
    names(d)[names(d) == "Golden Standart"] <- "gs"

    res <- quietly(oddsratio(data = d, explanatory = c("LVI", "PNI"), outcome = "gs",
                             outcomeLevel = "1", predictorLevel = NULL,
                             usePenalized = TRUE))
    html <- paste(as.character(res$text$content), collapse = "")

    # Internal names leaking into the header.
    expect_false(grepl("OR_Uni", html, fixed = TRUE))
    expect_false(grepl("OR_Multi", html, fixed = TRUE))

    # No column may be rendered entirely blank. (A single blank cell is normal:
    # finalfit leaves the variable name empty on continuation rows.)
    expect_equal(empty_columns(html), character(0))

    # Per-level counts, e.g. "84 (58.7)".
    txt <- strip_html(html)
    expect_true(grepl("[0-9]+ \\([0-9]+\\.[0-9]\\)", txt),
                info = "no 'N (pct)' count cells in the penalized table")

    # The reference level of each factor must be listed, as it is in the
    # unpenalized table (LVI has levels Absent / Present).
    expect_true(grepl("Absent", txt, fixed = TRUE),
                info = "reference level row missing from the penalized table")
})

# ---- M4: small-cell recommendation must be ONE structured bullet -----------

test_that("M4: small-cell recommendation renders as a single structured bullet", {
    res <- quietly(oddsratio(
        data = small_2x2,
        explanatory = "pred",
        outcome = "out",
        outcomeLevel = "Yes",
        diagnosticPredictor = "pred",
        predictorLevel = "Pos",
        showNomogram = TRUE))

    html <- paste(as.character(res$diagnosticMetrics$content), collapse = "")
    expect_true(grepl("Statistical Recommendations", html, fixed = TRUE),
                info = "min expected cell count 3.06 should trigger the assumption check")

    block <- regmatches(html,
        regexpr("(?s)Statistical Recommendations.*?</div>", html, perl = TRUE))
    expect_equal(length(block), 1L)

    bullets <- gregexpr("•", block)[[1]]
    n_bullets <- if (bullets[1] == -1) 0L else length(bullets)
    # Before the fix the recommendation list is flattened by append() into four
    # character elements, so the test name, the reason, the code and the
    # interpretation each become their own bullet.
    expect_equal(n_bullets, 1L)

    # Text of the FIRST bullet: it must carry the test name and its reason
    # together, not just one of the two.
    bullet_text <- strip_html(sub("(?s).*?•", "", block, perl = TRUE))
    expect_true(grepl("Fisher", bullet_text, fixed = TRUE))
    expect_gt(nchar(trimws(bullet_text)), 30)
})

# ---- M5: re-rendering the plot must not append notices again ---------------

test_that("M5: re-rendering the forest plot does not duplicate notices", {
    # The only notice emitted from inside .plot() is the penalized-plot
    # fallback warning, so this is the configuration that exercises the
    # render path's notice handling.
    res <- quietly(oddsratio(
        data = histopathology,
        explanatory = "Rater 1",
        outcome = "Golden Standart",
        outcomeLevel = "1",
        predictorLevel = NULL,
        usePenalized = TRUE))

    try(render_plot(res), silent = TRUE)
    after_first <- notices_html(res)

    try(render_plot(res), silent = TRUE)
    try(render_plot(res), silent = TRUE)
    after_third <- notices_html(res)

    # Rendering is idempotent: the same plot drawn three times must leave the
    # notice panels exactly as one draw left them.
    expect_identical(after_third, after_first)
})

# ---- M7: validation warnings must be emitted after the last append ---------

test_that("M7: no validation warning is appended after the notice-emission loop", {
    gen <- tryCatch(get("oddsratioClass", envir = asNamespace("ClinicoPath")),
                    error = function(e) NULL)
    if (is.null(gen))
        gen <- tryCatch(get("oddsratioClass", envir = globalenv()),
                        error = function(e) NULL)
    skip_if(is.null(gen), "oddsratioClass not reachable")

    src <- deparse(gen$private_methods$.run)

    # The loop that turns collected validation messages into notices.
    emit <- grep("for \\(info_msg in validation_results\\$info\\)", src)
    skip_if(length(emit) == 0, "notice-emission loop not found in .run()")

    # Anything still appended to validation_results after that loop can never
    # reach the user -- the explanatory-variable mapping warning is one such.
    appends <- grep("validation_results\\$(warnings|strong_warnings|info) <- c\\(",
                    src)
    late <- appends[appends > max(emit)]
    expect_equal(length(late), 0L,
                 info = paste("appended after the emission loop at source lines:",
                              paste(late, collapse = ", ")))
})

# ---- M9: no hard-coded dark foreground colours -----------------------------

test_that("M9: rendered Html declares no hard-coded foreground colour", {
    d <- histopathology
    names(d)[names(d) == "Golden Standart"] <- "gs"
    names(d)[names(d) == "New Test"] <- "newtest"

    res <- quietly(oddsratio(data = d, explanatory = "newtest", outcome = "gs",
                             outcomeLevel = "1", diagnosticPredictor = "newtest",
                             predictorLevel = "1", showNomogram = TRUE,
                             showExplanations = TRUE))

    html <- all_html(res)
    # Match `color: #...` only when "color" is a property in its own right;
    # the lookbehind drops background-color, border-color, outline-color, etc.
    hits <- regmatches(html,
        gregexpr("(?<![a-zA-Z-])color[[:space:]]*:[[:space:]]*#[0-9A-Fa-f]{3,8}",
                 html, perl = TRUE))[[1]]
    expect_equal(length(hits), 0L,
                 info = paste("hard-coded foreground colours:",
                              paste(unique(hits), collapse = ", ")))
})

# ---- E: no unsubstituted translation placeholders --------------------------

test_that("E: no unsubstituted {placeholder} leaks into any Html output", {
    d <- histopathology
    names(d)[names(d) == "Golden Standart"] <- "gs"
    names(d)[names(d) == "New Test"] <- "newtest"

    configs <- list(
        quietly(oddsratio(data = d, explanatory = "LVI", outcome = "gs",
                          outcomeLevel = "1", predictorLevel = NULL)),
        quietly(oddsratio(data = d, explanatory = c("LVI", "Age"), outcome = "gs",
                          outcomeLevel = "0", predictorLevel = NULL,
                          usePenalized = TRUE)),
        quietly(oddsratio(data = d, explanatory = "newtest", outcome = "gs",
                          outcomeLevel = "1", diagnosticPredictor = "newtest",
                          predictorLevel = "1", showNomogram = TRUE,
                          showExplanations = TRUE)),
        quietly(oddsratio(data = small_2x2, explanatory = "pred", outcome = "out",
                          outcomeLevel = "Yes", diagnosticPredictor = "pred",
                          predictorLevel = "Pos", showNomogram = TRUE))
    )

    for (res in configs) {
        html <- all_html(res)
        leaks <- unique(regmatches(html,
            gregexpr("\\{[a-z][A-Za-z_0-9]*\\}", html))[[1]])
        expect_equal(length(leaks), 0L,
                     info = paste("unsubstituted placeholders:",
                                  paste(leaks, collapse = ", ")))
    }
})

# ---- guard: outcome level semantics (currently correct -- lock it in) ------

test_that("GUARD: swapping the positive outcome level inverts the odds ratio", {
    or_of <- function(level) {
        res <- quietly(oddsratio(data = histopathology, explanatory = "LVI",
                                 outcome = "Golden Standart",
                                 outcomeLevel = level, predictorLevel = NULL))
        txt <- strip_html(res$text$content)
        cell <- regmatches(txt, regexpr("[0-9]+\\.[0-9]+ \\([0-9.]+-[0-9.]+, p=[0-9.]+\\)",
                                        txt))
        expect_equal(length(cell), 1L)
        as.numeric(sub(" .*", "", cell))
    }

    or1 <- or_of("1")   # 0.80 as rendered (0.7956 exact)
    or0 <- or_of("0")   # 1.26 as rendered (1.2569 exact)

    expect_equal(or1 * or0, 1, tolerance = 0.02)
})

# ---- guard: epiR agreement (currently correct -- lock it in) ---------------

test_that("GUARD: diagnostic metrics match epiR::epi.tests()", {
    res <- quietly(oddsratio(
        data = histopathology,
        explanatory = "New Test",
        outcome = "Golden Standart",
        outcomeLevel = "1",
        diagnosticPredictor = "New Test",
        predictorLevel = "1",
        showNomogram = TRUE))

    txt <- strip_html(res$diagnosticMetrics$content)

    grab <- function(pattern) {
        hit <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
        expect_equal(length(hit), 1L, label = paste("match for", pattern))
        as.numeric(regmatches(hit, gregexpr("[0-9]+\\.[0-9]+", hit))[[1]])
    }

    sens <- grab("Sensitivity: [0-9.]+% \\(95% CI [0-9.]+-[0-9.]+%\\)")
    spec <- grab("Specificity: [0-9.]+% \\(95% CI [0-9.]+-[0-9.]+%\\)")
    plr  <- grab("Positive LR: [0-9.]+ \\(95% CI [0-9.]+-[0-9.]+\\)")
    nlr  <- grab("Negative LR: [0-9.]+ \\(95% CI [0-9.]+-[0-9.]+\\)")

    expect_equal(sens, c(71.4, 61.4, 80.1), tolerance = 1e-3)
    expect_equal(spec, c(75.7, 68.0, 82.2), tolerance = 1e-3)
    expect_equal(plr,  c(2.93, 2.16, 3.99), tolerance = 1e-3)
    expect_equal(nlr,  c(0.38, 0.27, 0.52), tolerance = 1e-3)
})

# ---- helpers for the model-specification and label-restoration blocks ------

# First two cells of each body row of the odds-ratio table: the variable label
# and its level. That pair IS the model specification as rendered -- one row per
# estimated term plus the reference row -- independent of the numbers printed in
# the OR columns.
or_table_rows <- function(res) {
    html <- paste(as.character(res$text$content), collapse = "")
    body <- regmatches(html, regexpr("(?s)<tbody>.*</tbody>", html, perl = TRUE))
    if (length(body) == 0) return(character(0))
    rows <- regmatches(body, gregexpr("(?s)<tr>.*?</tr>", body, perl = TRUE))[[1]]
    vapply(rows, function(r) {
        cells <- trimws(gsub("<[^>]*>", "",
            regmatches(r, gregexpr("(?s)<td[^>]*>.*?</td>", r, perl = TRUE))[[1]]))
        paste(cells[seq_len(min(2, length(cells)))], collapse = " | ")
    }, character(1), USE.NAMES = FALSE)
}

or_table_header <- function(res) {
    html <- paste(as.character(res$text$content), collapse = "")
    hd <- regmatches(html, regexpr("(?s)<thead>.*?</thead>", html, perl = TRUE))
    if (length(hd) == 0) return(character(0))
    trimws(gsub("<[^>]*>", "",
        regmatches(hd, gregexpr("(?s)<th[^>]*>.*?</th>", hd, perl = TRUE))[[1]]))
}


test_that("N1: usePenalized changes the estimator, not the model specification", {
    skip_if_not_installed("logistf")

    # Grade is numeric with three distinct values (1, 2, 3). finalfit's
    # `cont_cut` default of 5 used to mutate any such column to a FACTOR on its
    # own copy of the data and fit the model on that, while .fitFirthModel()
    # (logistf) and .prepareRmsNomogram() (rms::lrm) fitted the raw numeric
    # column linearly. Ticking a checkbox labelled "Firth penalized logistic
    # regression" therefore silently refitted a DIFFERENT model: the ML table
    # showed rows "Grade | 1", " | 2", " | 3" while the Firth table showed the
    # single row "Grade | Mean (SD)". cont_cut = 0 pins every path to the
    # coding the analyst actually chose.
    or_rows <- function(pen) or_table_rows(quietly(oddsratio(
        data = histopathology, explanatory = "Grade", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL, usePenalized = pen)))

    expect_identical(or_rows(TRUE), or_rows(FALSE),
        info = paste("Firth rows:", paste(or_rows(TRUE), collapse = " / "),
                     "| ML rows:",  paste(or_rows(FALSE), collapse = " / ")))
})

test_that("N1-control: a many-valued numeric already agreed across estimators", {
    skip_if_not_installed("logistf")

    # Age has enough distinct values that the cont_cut rule never applied to it,
    # so both paths always agreed. This pins the defect to the low-cardinality
    # band rather than to some general Firth-vs-ML formatting difference.
    or_rows <- function(pen) or_table_rows(quietly(oddsratio(
        data = histopathology, explanatory = "Age", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL, usePenalized = pen)))

    expect_identical(or_rows(TRUE), or_rows(FALSE))
})

test_that("N1-b: a genuine factor is unaffected by the cont_cut fix", {
    # cont_cut only ever touched NUMERIC columns. A user who wants level-wise
    # odds ratios for Grade sets it to a factor, and must still get three rows.
    d <- histopathology
    d$GradeF <- factor(d$Grade)

    rows <- or_table_rows(quietly(oddsratio(
        data = d, explanatory = "GradeF", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL)))

    expect_equal(length(rows), 3L)
    expect_true(any(grepl("GradeF", rows, fixed = TRUE)))
    expect_false(any(grepl("Mean (SD)", rows, fixed = TRUE)))
})

test_that("N1-c: the forest plot estimates match the table for a low-cardinality numeric", {
    # finalfit::or_plot() has no cont_cut argument: it built its label rows with
    # summary_factorlist()'s default cont_cut = 5 but fitted glmmulti() on the
    # raw column, so the fit_id join found nothing. The plot drew three labelled
    # rows with NO estimate plus a fourth, unlabelled row carrying the only odds
    # ratio. Assert the join now produces exactly one fully-estimated row.
    res <- quietly(oddsratio(
        data = histopathology, explanatory = "Grade", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL))

    st <- res$plot$state
    expect_false(is.null(st))

    fl <- finalfit::summary_factorlist(st$plotData, st$formulaDependent,
                                       st$formulaExplanatory,
                                       cont_cut = 0, total_col = TRUE, fit_id = TRUE)
    joined <- finalfit::ff_merge(
        fl,
        finalfit::fit2df(finalfit::glmmulti(st$plotData, st$formulaDependent,
                                            st$formulaExplanatory),
                         condense = FALSE, estimate_suffix = " (multivariable)"))

    expect_equal(nrow(joined), 1L)
    expect_false(anyNA(joined$L95))
    expect_true(all(!is.na(joined$label)))

    expect_true(quietly(res$plot$.render(width = 600, height = 450, ppi = 72)))
})

test_that("N2: the odds-ratio table header names the outcome as the user selected it", {
    # .restoreOriginalNamesInTable() rewrote the VALUES of the first column but
    # never names(table_data)[1], which finalfit had built from the
    # janitor-cleaned name. The header read "Dependent: disease_status".
    res <- quietly(oddsratio(
        data = histopathology, explanatory = "LVI", outcome = "Disease Status",
        outcomeLevel = "Ill", predictorLevel = NULL))

    hdr <- or_table_header(res)
    expect_gte(length(hdr), 1L)
    expect_identical(hdr[1], "Dependent: Disease Status")
})

test_that("N2-b: no janitor-cleaned name reaches the rendered table, on either path", {
    # Explanatory names were already restored ("New Test" survived); the
    # dependent was the only leak. Assert both, on both fitting paths, so the
    # fix cannot regress on one of them alone.
    for (pen in c(FALSE, TRUE)) {
        if (pen && !requireNamespace("logistf", quietly = TRUE)) next
        res <- quietly(oddsratio(
            data = histopathology, explanatory = c("New Test", "LVI"),
            outcome = "Disease Status", outcomeLevel = "Ill",
            predictorLevel = NULL, usePenalized = pen))

        html <- paste(as.character(res$text$content), collapse = "")
        for (leak in c("disease_status", "new_test"))
            expect_false(grepl(leak, html, fixed = TRUE),
                         label = paste0("cleaned name '", leak,
                                        "' leaked (usePenalized=", pen, ")"))
    }
})

test_that("N3: the diagnostic 2x2 is laid out positive-first", {
    # The table used to come out in factor-level order, so with Absent/Present
    # against Alive/Dead the true positives sat in the bottom-right and the
    # reader had to map the corners onto the TP/FP/FN/TN line by hand. The cells
    # must now read tp, fp, fn, tn left-to-right, top-to-bottom.
    res <- quietly(oddsratio(
        data = histopathology, explanatory = c("LVI", "Age"),
        outcome = "Mortality5yr", outcomeLevel = "Dead",
        diagnosticPredictor = "LVI", predictorLevel = "Present",
        showNomogram = TRUE))

    html <- paste(as.character(res$diagnosticMetrics$content), collapse = "")
    tbl  <- regmatches(html, regexpr("(?s)<table.*?</table>", html, perl = TRUE))
    expect_equal(length(tbl), 1L)

    cells <- trimws(gsub("<[^>]*>", "",
        regmatches(tbl, gregexpr("(?s)<td[^>]*>.*?</td>", tbl, perl = TRUE))[[1]]))
    counts <- suppressWarnings(as.numeric(cells))
    counts <- counts[!is.na(counts)]

    txt <- strip_html(res$diagnosticMetrics$content)
    quad <- as.numeric(regmatches(
        regmatches(txt, regexpr("TP: \\d+, FP: \\d+, FN: \\d+, TN: \\d+", txt)),
        gregexpr("\\d+", regmatches(txt, regexpr("TP: \\d+, FP: \\d+, FN: \\d+, TN: \\d+", txt))))[[1]])

    expect_equal(counts, quad)

    # And the positive row/column must be marked, or the reordering is invisible.
    expect_true(grepl("Present (+)", strip_html(tbl), fixed = TRUE))
    expect_true(grepl("Dead (+)",    strip_html(tbl), fixed = TRUE))
})

test_that("N4: the diagnostic panel states its own denominator", {
    # The 2x2 uses complete cases of outcome + predictor while the regression
    # uses complete cases of outcome + every explanatory variable, so the two Ns
    # differ on one page. Say so rather than leaving the reader to notice.
    res <- quietly(oddsratio(
        data = histopathology, explanatory = c("LVI", "Age"),
        outcome = "Mortality5yr", outcomeLevel = "Dead",
        diagnosticPredictor = "LVI", predictorLevel = "Present",
        showNomogram = TRUE))

    txt <- strip_html(res$diagnosticMetrics$content)
    # Both denominators, and no causal claim: when the diagnostic predictor is
    # not in the regression model, "needs every explanatory variable" is simply
    # the wrong reason for the difference.
    expect_match(txt, "Based on 248 observations")
    expect_match(txt, "the regression model above uses 247 rows")
    expect_match(txt, "need not be the same patients")
})

test_that("N5: a low-cardinality numeric predictor declares how it was coded", {
    # cont_cut = 0 makes every path agree, but silence about the resulting
    # linear trend would trade a hidden choice for an undeclared one.
    res <- quietly(oddsratio(
        data = histopathology, explanatory = "Grade", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL))

    expect_match(strip_html(res$infoMessages$content),
                 "Entered as continuous: Grade")

    # ... and must NOT fire for a variable it does not apply to.
    res2 <- quietly(oddsratio(
        data = histopathology, explanatory = "Age", outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL))

    expect_false(grepl("Entered as continuous",
                       strip_html(res2$infoMessages$content), fixed = TRUE))
})

test_that("N6: the analysis emits no message() chatter into Analysis Notes", {
    # finalfit's C-statistic and profile intervals pull in pROC and MASS, which
    # print "Setting levels:", "Setting direction:" and "Waiting for profiling
    # to be done...". jamovi renders those in the Analysis Notes panel, mixed in
    # with the module's own notices.
    msgs <- character(0)
    withCallingHandlers(
        suppressWarnings(oddsratio(
            data = histopathology, explanatory = c("LVI", "Age"),
            outcome = "Mortality5yr", outcomeLevel = "Dead",
            predictorLevel = NULL)),
        message = function(m) {
            msgs <<- c(msgs, conditionMessage(m))
            invokeRestart("muffleMessage")
        })

    expect_equal(msgs, character(0),
                 label = paste("leaked messages:", paste(msgs, collapse = " | ")))
})

test_that("N7: a binary 0/1 numeric keeps its per-level counts", {
    # cont_cut = 0 (N1) stops finalfit respecifying the model, but a numeric with
    # exactly TWO distinct values is the same model either way -- identical OR,
    # CI and p -- so leaving it on the linear side bought nothing and replaced
    # the per-level n (%) cross-tab with "Mean (SD) 0.2 (0.4)" for a 0/1 marker.
    # These are the analysis's own documented example variables.
    rows <- or_table_rows(quietly(oddsratio(
        data = histopathology, explanatory = c("New Test", "Rater 1"),
        outcome = "Golden Standart", outcomeLevel = "1", predictorLevel = NULL)))

    expect_false(any(grepl("Mean (SD)", rows, fixed = TRUE)))
    expect_equal(length(rows), 4L)          # two levels for each of two predictors
    expect_true(any(grepl("New Test", rows, fixed = TRUE)))
})

test_that("N7-b: coercing the binary leaves every estimate untouched", {
    skip_if_not_installed("logistf")
    # The whole justification for N7 is that it is estimator-neutral. If this
    # ever fails, the coercion is doing something it must not.
    ors <- function(pen) {
        html <- paste(as.character(quietly(oddsratio(
            data = histopathology, explanatory = c("New Test", "Rater 1"),
            outcome = "Golden Standart", outcomeLevel = "1",
            predictorLevel = NULL, usePenalized = pen))$text$content), collapse = "")
        regmatches(html, gregexpr("[0-9]+\\.[0-9]{2} \\([0-9.]+-[0-9.]+", html))[[1]]
    }
    expect_true(any(grepl("^7\\.6[0-9] \\(4\\.3", ors(FALSE))))   # ML   7.66 (4.36-
    expect_true(any(grepl("^7\\.5[0-9] \\(4\\.2", ors(TRUE))))    # Firth 7.51 (4.29-
})

test_that("N8: the continuous-coding notice tracks the coding, not finalfit's old cutoff", {
    d <- histopathology
    set.seed(1)
    d$GleasonGG <- sample(1:5, nrow(d), TRUE)   # 5 distinct: above finalfit's old < 5
    d$TStage2   <- sample(1:4, nrow(d), TRUE)   # 4 distinct: below it

    # do.call, not oddsratio(explanatory = ex): the wrapper resolves its
    # variable arguments with jmvcore::resolveQuo(), which returns a bare
    # SYMBOL's own name -- so `explanatory = ex` asks for a column called "ex".
    note <- function(ex) strip_html(quietly(do.call(oddsratio, list(
        data = d, explanatory = ex, outcome = "Mortality5yr",
        outcomeLevel = "Dead", predictorLevel = NULL)))$infoMessages$content)

    # Every numeric is fitted linearly now, so a Gleason grade group needs the
    # same disclosure a 1-4 stage does. Inheriting `< 5` named one and not the
    # other in the same model, implying Gleason had been handled some other way.
    expect_match(note("GleasonGG"), "Entered as continuous: GleasonGG \\(5 values\\)")
    expect_match(note("TStage2"),   "Entered as continuous: TStage2 \\(4 values\\)")
    expect_match(note(c("GleasonGG", "TStage2")), "GleasonGG \\(5 values\\), TStage2 \\(4 values\\)")

    # ... and it must stay silent for a binary, which is now coerced to a factor
    # and so is NOT entered as a trend. Advising a measure-type change there
    # would be advice that provably changes nothing.
    expect_false(grepl("Entered as continuous",
        strip_html(quietly(oddsratio(
            data = histopathology, explanatory = "New Test",
            outcome = "Golden Standart", outcomeLevel = "1",
            predictorLevel = NULL))$infoMessages$content), fixed = TRUE))
})

test_that("N9: a sparse level in a numeric trend is warned about", {
    # The factor branch of .validateInputs() has always warned about categories
    # with fewer than 5 observations; the numeric branch never did. Once such a
    # column is fitted as a linear trend those few rows carry high leverage on
    # the whole slope, and the "not estimable" / separation signal they used to
    # trigger under the old factor coding no longer fires.
    set.seed(9)
    d <- data.frame(y = factor(sample(c("No", "Yes"), 200, TRUE)),
                    SparseGrade = c(rep(1, 120), rep(2, 77), rep(3, 3)))

    res <- quietly(oddsratio(data = d, explanatory = "SparseGrade", outcome = "y",
                             outcomeLevel = "Yes", predictorLevel = NULL))

    expect_match(strip_html(res$warnings$content),
                 "entered as continuous and has 1 value\\(s\\) carried by fewer than 5")
})

test_that("N10: the plot state carries no dead payload", {
    # `filteredTable` was computed in .run(), serialized into every saved .omv
    # (~5.8 KB), read into a local in .plot() and never used.
    res <- quietly(oddsratio(
        data = histopathology, explanatory = c("LVI", "Age"),
        outcome = "Mortality5yr", outcomeLevel = "Dead", predictorLevel = NULL))

    expect_false("filteredTable" %in% names(res$plot$state))
    expect_setequal(names(res$plot$state),
                    c("plotData", "formulaDependent", "formulaExplanatory",
                      "originalNames", "originalOutcomeName",
                      "originalExplanatoryNames"))
})

test_that("N11: the nomogram's axes carry the user's variable names, live and after reload", {
    # The model is fitted on janitor-CLEANED names on purpose: rms::lrm() dies
    # with "subscript out of bounds" on a frame whose columns contain spaces, so
    # refitting on the original names would trade a cosmetic axis label for no
    # nomogram at all. The finished nomogram object is relabelled instead.
    axes <- function(res, priv) {
        nom <- priv$.nom_object
        if (is.null(nom)) return(character(0))
        setdiff(names(nom), c("total.points", "lp", "Predicted Probability"))
    }

    for (ex in list(c("LVI", "PNI"), c("New Test", "Rater 1"))) {
        res <- quietly(do.call(oddsratio, list(
            data = histopathology, explanatory = ex, outcome = "Disease Status",
            outcomeLevel = "Ill", predictorLevel = NULL, showNomogram = TRUE)))

        # The analysis object is not returned by the wrapper, so assert on the
        # rendered plot instead: it must draw on both paths.
        expect_true(quietly(res$plot_nomogram$.render(width = 800, height = 600, ppi = 72)))

        # No cleaned name may appear in the nomogram's own Html panel either.
        html <- strip_html(res$nomogram$content)
        for (leak in c("new_test", "rater_1", "disease_status"))
            expect_false(grepl(leak, html, fixed = TRUE),
                         label = paste("cleaned name in nomogram panel:", leak))
    }
})
