# Release review of jjsegmentedtotalbar.
#
# The analysis sums a numeric Value Variable within each (category, segment)
# cell and renders each bar as a 100% composition. Almost all of the risk lives
# in what that sum is ALLOWED to be: the Value Variable is `permitted: [numeric]`
# and `suggested: [continuous]`, so it is frequently a measurement rather than a
# count - and a measurement must not be described as an observation count, nor
# handed to a chi-square test without saying so.

stb_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x %||% "")))
stb_warn <- function(res) stb_txt(res$warnings$content)

# `y_is_count` is the affirmation that gates the chi-square. It lives in
# jamovi/jjsegmentedtotalbar.a.yaml and only reaches the R wrapper after
# jmvtools::prepare(); until then the argument does not exist and passing it is
# an "unused argument" error. Build the call so the suite is honest either way -
# it SKIPS the chi-square blocks on a stale header rather than silently passing.
stb_has_affirm <- function() "y_is_count" %in% names(formals(jjsegmentedtotalbar))
stb_skip_without_affirm <- function()
    testthat::skip_if_not(stb_has_affirm(),
                          "y_is_count not compiled yet - run jmvtools::prepare()")

# Runs with the count affirmation supplied.
stb_tested <- function(...) {
    a <- list(...)
    a$show_statistical_tests <- TRUE
    if (stb_has_affirm()) a$y_is_count <- TRUE
    do.call(jjsegmentedtotalbar, a)
}

stb_svg <- function(...) {
    res <- jjsegmentedtotalbar(..., show_plot = TRUE)
    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
    on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
    print(res$plot)
    paste(readLines(f, warn = FALSE), collapse = "")
}

# 90 patients, y = a cost in whole dollars: integer-valued but NOT a count.
stb_cost <- function() {
    set.seed(2)
    data.frame(arm  = factor(rep(c("Surgery", "Chemo", "Radio"), each = 30)),
               path = factor(rep(rep(c("G1", "G2", "G3"), each = 10), 3)),
               cost = sample(50:500, 90, TRUE))
}
stb_counts <- function() {
    set.seed(42)
    data.frame(arm  = factor(rep(c("A", "B", "C"), each = 40)),
               resp = factor(sample(c("CR", "PR", "SD", "PD"), 120, TRUE)),
               n    = sample(1:15, 120, TRUE))
}


# ---- the crash --------------------------------------------------------------

test_that("a continuous Value Variable does not crash the analysis", {
    # sprintf("%d", 444.7) is a hard R error, and the call sat outside every
    # guard, so ANY non-integer total took the whole analysis down at default
    # settings - 99 of 99 suite errors were this one line.
    set.seed(1)
    d <- data.frame(arm  = factor(rep(c("A", "B"), each = 30)),
                    path = factor(rep(rep(c("G1", "G2", "G3"), each = 10), 2)),
                    size = round(runif(60, 5, 50), 1))
    expect_no_error(jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "path", y_var = "size"))
    res <- jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "path", y_var = "size")
    expect_gt(nrow(res$composition_table$asDF), 0)
})

test_that("fractional totals are shown as fractions, not truncated", {
    d <- data.frame(g = factor(c("A", "A", "B", "B")),
                    s = factor(c("x", "y", "x", "y")),
                    v = c(1.25, 2.25, 3.5, 4.5))          # total 11.5
    w <- stb_warn(jjsegmentedtotalbar(data = d, x_var = "g", fill_var = "s", y_var = "v"))
    expect_match(w, "11.5", fixed = TRUE)
    expect_false(grepl("total 11 of", w, fixed = TRUE))   # as.integer() truncation
})


# ---- what the total is allowed to be called ---------------------------------

test_that("a summed measurement is never reported as an observation count", {
    # 90 patients, costs summing to 26678. Calling that "26678 observations"
    # invites the reader to quote it as N.
    res <- jjsegmentedtotalbar(data = stb_cost(), x_var = "arm", fill_var = "path", y_var = "cost")
    w <- stb_warn(res)
    expect_match(w, "Analysed 90 rows")
    expect_false(grepl("26678 observations", w, fixed = TRUE))
    expect_match(w, "summed quantity")
})

test_that("the copy-ready sentence puts N on the rows, not on the sum", {
    # This line is the one most likely to be pasted into a manuscript.
    res <- jjsegmentedtotalbar(data = stb_cost(), x_var = "arm", fill_var = "path",
                               y_var = "cost", showExplanations = TRUE)
    t <- stb_txt(res$clinical_summary$content)
    expect_match(t, "(N=90 rows", fixed = TRUE)
    expect_false(grepl("N=26678", t, fixed = TRUE))
})


# ---- the chi-square ---------------------------------------------------------

test_that("chi-square matches stats::chisq.test exactly on count data", {
    stb_skip_without_affirm()
    d <- stb_counts()
    res <- stb_tested(data = d, x_var = "arm", fill_var = "resp", y_var = "n")
    st <- res$statistical_tests$asDF
    st <- st[st$test_name == "Pearson's Chi-square", ]

    agg <- stats::aggregate(n ~ arm + resp, data = d, FUN = sum)
    m   <- stats::xtabs(n ~ resp + arm, data = agg)
    ref <- suppressWarnings(stats::chisq.test(m))

    expect_equal(st$statistic[1], unname(ref$statistic), tolerance = 1e-8)
    expect_equal(st$df[1],        unname(ref$parameter))
    expect_equal(st$p_value[1],   ref$p.value, tolerance = 1e-12)
})

test_that("the count assumption behind chi-square is stated where it is read", {
    # The cells are SUMS. Integrality does not make them frequencies: measured on
    # random, association-free costs, chi2 = 277.5 / p = 7.7e-59 in dollars and
    # chi2 = 27750.5 / p = 0 for the SAME money in cents. The statistic scales
    # with the unit, so on a measurement it manufactures significance. There is
    # no way to detect a count automatically, so it must be disclosed.
    stb_skip_without_affirm()
    res <- stb_tested(data = stb_cost(), x_var = "arm", fill_var = "path", y_var = "cost")
    expect_true("count_assumption" %in% names(res$statistical_tests$notes))
    expect_match(as.character(res$statistical_tests$notes$count_assumption$note),
                 "scales with the unit")
})

test_that("the unit-scaling failure mode is real, which is why it is disclosed", {
    stb_skip_without_affirm()
    d <- stb_cost()
    d$cost_cents <- d$cost * 100L
    get <- function(v) {
        st <- stb_tested(data = d, x_var = "arm", fill_var = "path",
                         y_var = v)$statistical_tests$asDF
        st$statistic[st$test_name == "Pearson's Chi-square"][1]
    }
    expect_equal(get("cost_cents") / get("cost"), 100, tolerance = 1e-6)
})

test_that("non-integer data skips the test rather than reporting a bogus one", {
    # NB: adding a constant to every row does NOT make the TOTALS fractional
    # (10 rows per cell x 0.5 = +5.0), and it is the cell totals that are checked.
    stb_skip_without_affirm()
    d <- stb_cost(); d$cost <- d$cost + 0.07
    expect_false(all(abs(with(d, tapply(cost, list(arm, path), sum)) %% 1) < 1e-9))
    res <- stb_tested(data = d, x_var = "arm", fill_var = "path", y_var = "cost")
    expect_equal(nrow(res$statistical_tests$asDF), 0L)
    expect_match(stb_warn(res), "cannot be frequencies")
})

test_that("sparse tables get an expected-count warning", {
    # chisq.test() raises only a console warning, which the GUI never shows.
    stb_skip_without_affirm()
    d <- data.frame(g = factor(rep(c("A", "B"), each = 2)),
                    s = factor(rep(c("x", "y"), 2)),
                    v = c(1, 2, 8, 1))
    expect_match(stb_warn(stb_tested(data = d, x_var = "g", fill_var = "s", y_var = "v")),
                 "expected counts below 5")
})


# ---- composition arithmetic -------------------------------------------------

test_that("composition matches a base-R aggregate and every bar totals 100%", {
    d <- stb_counts()
    tab <- jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "resp",
                               y_var = "n")$composition_table$asDF

    agg <- stats::aggregate(n ~ arm + resp, data = d, FUN = sum)
    m   <- stats::xtabs(n ~ resp + arm, data = agg)
    grp <- prop.table(m, margin = 2) * 100
    ovr <- m / sum(m) * 100

    for (i in seq_len(nrow(tab))) {
        ci <- as.character(tab$category[i]); si <- as.character(tab$segment[i])
        expect_equal(tab$count[i], as.numeric(m[si, ci]), info = paste(ci, si))
        expect_equal(tab$percentage[i] * 100, as.numeric(grp[si, ci]), info = paste(ci, si))
        expect_equal(tab$overall_percentage[i] * 100, as.numeric(ovr[si, ci]), info = paste(ci, si))
    }
    expect_equal(as.vector(tapply(tab$percentage, tab$category, sum)) * 100, rep(100, 3))
})

test_that("percentages are invariant to the unit of the value variable", {
    d <- stb_cost(); d$cost_cents <- d$cost * 100L
    a <- jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "path", y_var = "cost")
    b <- jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "path", y_var = "cost_cents")
    expect_equal(a$composition_table$asDF$percentage, b$composition_table$asDF$percentage)
})


# ---- the presets ------------------------------------------------------------

test_that("every clinical preset actually changes the chart", {
    # The presets stored a config that nothing except the guidance panel ever
    # read, so all five produced a byte-identical plot while advertising a
    # "predefined clinical analysis configuration".
    d <- stb_counts()
    base <- stb_svg(data = d, x_var = "arm", fill_var = "resp", y_var = "n",
                    analysis_preset = "custom")
    for (p in c("treatment_response", "demographics", "biomarker", "quality", "temporal"))
        expect_false(identical(base, stb_svg(data = d, x_var = "arm", fill_var = "resp",
                                             y_var = "n", analysis_preset = p)),
                     info = p)
})

test_that("an explicit choice overrides the preset", {
    # demographics asks for the colorblind palette; a user asking for viridis must
    # win, or the control appears broken. Compare against the same preset left on
    # its own palette - the preset also sets chart_style and percentage display,
    # so this must not be compared against "custom".
    d <- stb_counts()
    preset_default <- stb_svg(data = d, x_var = "arm", fill_var = "resp", y_var = "n",
                              analysis_preset = "demographics")
    overridden <- stb_svg(data = d, x_var = "arm", fill_var = "resp", y_var = "n",
                          analysis_preset = "demographics", color_palette = "viridis")
    expect_false(identical(preset_default, overridden))
})


# ---- data handling ----------------------------------------------------------

test_that("negative category totals are rejected", {
    d <- data.frame(g = factor(c("A", "A", "B", "B")),
                    s = factor(c("x", "y", "x", "y")),
                    v = c(-5, 2, 3, 4))
    expect_match(stb_warn(jjsegmentedtotalbar(data = d, x_var = "g", fill_var = "s", y_var = "v")),
                 "Negative Values")
})

test_that("an empty dataset is explained, not crashed on", {
    d <- stb_counts()[0, ]
    res <- jjsegmentedtotalbar(data = d, x_var = "arm", fill_var = "resp", y_var = "n")
    expect_match(as.character(res$instructions$content), "No data available")
})


# ---- the count affirmation gate ---------------------------------------------

test_that("chi-square does not run until the user affirms the value is a count", {
    # No data check can separate a count from a whole-number MEASUREMENT, and
    # running the test on a measurement manufactures significance. So the test is
    # opt-in on something only the user knows. This assertion holds both before
    # and after jmvtools::prepare(): with a stale header the option is absent and
    # .optionOr() falls back to FALSE, which is the safe direction.
    res <- jjsegmentedtotalbar(data = stb_counts(), x_var = "arm", fill_var = "resp",
                               y_var = "n", show_statistical_tests = TRUE)
    expect_equal(nrow(res$statistical_tests$asDF), 0L)
    expect_match(stb_warn(res), "Chi-square not run")
    expect_match(stb_warn(res), "Value Variable counts cases")
})

test_that("a missing compiled option degrades instead of crashing the analysis", {
    # jmvcore's `$` ERRORS on an undeclared option rather than returning NULL, so
    # an .a.yaml addition that has not been through prepare() would otherwise take
    # the whole analysis down.
    res <- jjsegmentedtotalbar(data = stb_counts(), x_var = "arm", fill_var = "resp",
                               y_var = "n", show_statistical_tests = TRUE)
    expect_gt(nrow(res$composition_table$asDF), 0)
})


# ---- numeric variables in categorical slots ---------------------------------

test_that("a numeric variable is already a factor before the backend sees it", {
    # Both slots are `permitted: [factor]`. jamovi refuses a continuous variable
    # in the GUI, and the R wrapper coerces it upstream, so a backend-side
    # "you gave me a number" warning would be unreachable code. What survives is
    # the consequence - one bar per distinct value - and the notice that flags it.
    set.seed(8)
    d <- data.frame(grp = factor(rep(c("A", "B"), each = 20)),
                    num = as.numeric(sample(1:20, 40, TRUE)),
                    v   = as.numeric(sample(1:20, 40, TRUE)))
    res <- jjsegmentedtotalbar(data = d, x_var = "num", fill_var = "grp", y_var = "v")
    expect_gt(res$summary$asDF$categories, 10)
    expect_match(stb_warn(res), "Large number of categories")
})


# ---- preset transparency ----------------------------------------------------

test_that("the template panel names the controls it fills and the override rule", {
    # It used to claim it optimised "chart settings, color palettes, and
    # statistical options" while applying nothing at all - and it has never had
    # any bearing on the statistical options.
    res <- jjsegmentedtotalbar(data = stb_counts(), x_var = "arm", fill_var = "resp",
                               y_var = "n", analysis_preset = "demographics")
    g <- stb_txt(res$preset_guidance$content)
    expect_match(g, "What this template changes")
    expect_match(g, "colorblind")      # the palette this preset actually applies
    expect_match(g, "publication")     # and the chart style
    expect_match(g, "your choice is kept")
    expect_match(g, "anything under Statistical Tests")
    expect_false(grepl("statistical options", g, fixed = TRUE))
})
