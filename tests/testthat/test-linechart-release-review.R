# Release review of linechart.
#
# The correlation and regression arithmetic is correct and matches base R. The
# defects were around it: values printed at 15 significant digits, non-finite
# inputs aborting the run with an internal R message, and degenerate inputs
# producing a blank panel with no explanation at all.

lc_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x %||% "")))
lc_todo <- function(res) lc_txt(res$todo$content)
lc_cell <- function(res, stat) {
    df <- res$summary$asDF
    v <- df$value[df$statistic == stat]
    if (!length(v)) NA_character_ else v[1]
}
lc_interp <- function(res, measure) {
    df <- res$correlation$asDF
    v <- df$interpretation[df$measure == measure]
    if (!length(v)) NA_character_ else v[1]
}

lc_trend <- function(n = 40, seed = 21) {
    set.seed(seed)
    data.frame(t = seq_len(n), y = 3 + 0.8 * seq_len(n) + rnorm(n, 0, 4))
}


# ---- numbers must be readable ------------------------------------------------

test_that("reported values are rounded, not printed at full precision", {
    # base::format() is MASKED here: `@import jmvcore` brings jmvcore's own
    # format() into the namespace, and that one is a string-template helper which
    # ignores `digits`. So every format(x, digits = 3) silently stringified at
    # full precision - "Y Mean 19.8349757678086" in a clinical summary table.
    res <- linechart(data = lc_trend(), xvar = "t", yvar = "y", trendline = TRUE)

    for (stat in c("Y Mean", "Y Median", "Y Standard Deviation")) {
        v <- lc_cell(res, stat)
        expect_lt(nchar(v), 8L)                       # 15-16 chars before the fix
        expect_match(v, "^-?[0-9.]+$", info = stat)
    }
    expect_match(lc_cell(res, "Y Range"), "^-?[0-9.]+ - -?[0-9.]+$")

    # the slope sentence is copy-ready text; it read "0.829075514952931 unit"
    slope_txt <- lc_interp(res, "Regression Slope")
    expect_match(slope_txt, "0.829 unit", fixed = TRUE)
    expect_false(grepl("0.829075", slope_txt, fixed = TRUE))
})

test_that("base::format is genuinely masked, which is why the helper exists", {
    # If a future jmvcore stops exporting format(), this test fails and the
    # .fmtNum() helper can be reconsidered - it should not be removed on a hunch.
    skip_if_not(requireNamespace("jmvcore", quietly = TRUE))
    expect_true("format" %in% getNamespaceExports("jmvcore"))
    f <- get("format", envir = asNamespace("ClinicoPath"))
    expect_equal(environmentName(environment(f)), "jmvcore")
    expect_equal(base::format(19.8349757678086, digits = 3), "19.8")
})


# ---- inputs that used to abort or vanish -------------------------------------

test_that("infinite values are excluded and explained, not crashed on", {
    # complete.cases() follows is.na(), which is FALSE for Inf, so infinities
    # survived the filter and reached `var(y) == 0`. var() returns NaN there and
    # `NaN == 0` is NA, so `if (NA)` aborted with "missing value where TRUE/FALSE
    # needed" - under a heading that then advised removing MISSING values.
    d <- lc_trend(); d$y[c(3, 7)] <- c(Inf, -Inf)
    t <- lc_todo(linechart(data = d, xvar = "t", yvar = "y"))
    expect_match(t, "infinite values")
    expect_match(t, "2 row")
    expect_match(t, "38 rows were analysed")
    expect_false(grepl("missing value where TRUE/FALSE needed", t, fixed = TRUE))
})

test_that("rows dropped for missing values are disclosed", {
    # warning() alone reaches only the R console, which a jamovi user never sees.
    d <- lc_trend(); d$y[1:4] <- NA
    t <- lc_todo(linechart(data = d, xvar = "t", yvar = "y"))
    expect_match(t, "4 row")
    expect_match(t, "excluded because of missing values")
    expect_match(t, "36 rows were analysed")
})

test_that("complete data raises no exclusion notice", {
    t <- lc_todo(linechart(data = lc_trend(), xvar = "t", yvar = "y"))
    expect_false(grepl("were excluded", t, fixed = TRUE))
    expect_false(grepl("infinite values", t, fixed = TRUE))
})

test_that("a single distinct X value is called out", {
    # 10 observations all at one time point rendered a plot and 7 summary rows
    # with no comment. That is not a line, and no trend exists to describe.
    d <- data.frame(t = rep(1, 10), y = rnorm(10))
    expect_match(lc_todo(linechart(data = d, xvar = "t", yvar = "y")),
                 "single X value")
})

test_that("an empty dataset explains itself instead of going blank", {
    # reject() signals to the user why the analysis cannot proceed on an empty dataset
    d <- data.frame(t = numeric(0), y = numeric(0))
    expect_error(linechart(data = d, xvar = "t", yvar = "y"), "The dataset has no rows")
})

test_that("existing guards still fire", {
    expect_match(lc_todo(linechart(data = data.frame(t = 1:2, y = c(1, 2)),
                                   xvar = "t", yvar = "y")),
                 "At least 3 complete observations")
    expect_match(lc_todo(linechart(data = data.frame(t = 1:10, y = rep(5, 10)),
                                   xvar = "t", yvar = "y")),
                 "no variation")
})


# ---- the reference line ------------------------------------------------------

lc_plot_has <- function(label, ...) {
    res <- linechart(..., reflineLabel = label)
    f <- tempfile(fileext = ".svg"); svglite::svglite(f, 8, 6)
    on.exit(unlink(f), add = TRUE)
    print(res$plot)
    grDevices::dev.off()          # must close before reading: svglite buffers
    any(grepl(label, readLines(f, warn = FALSE), fixed = TRUE))
}

test_that("a reference line at zero is reachable", {
    # 0 was the "no line" sentinel, which made the most common clinical reference
    # impossible to draw: change from baseline, a difference, and a log fold
    # change all sit at zero. showRefline is now the sole gate.
    d <- lc_trend()
    expect_true(lc_plot_has("Threshold", data = d, xvar = "t", yvar = "y",
                            showRefline = TRUE, refline = 0))
    expect_false(lc_plot_has("Threshold", data = d, xvar = "t", yvar = "y",
                             showRefline = FALSE, refline = 0))
})

# ---- statistics --------------------------------------------------------------

test_that("correlation and regression match base R exactly", {
    d <- lc_trend()
    res <- linechart(data = d, xvar = "t", yvar = "y", trendline = TRUE)
    df <- res$correlation$asDF

    pe  <- stats::cor.test(d$t, d$y, method = "pearson")
    sp  <- suppressWarnings(stats::cor.test(d$t, d$y, method = "spearman"))
    fit <- stats::lm(y ~ t, data = d)

    expect_equal(df$value[df$measure == "Pearson Correlation"],
                 unname(pe$estimate), tolerance = 1e-10)
    expect_equal(df$value[df$measure == "Spearman Correlation (Rank-based)"],
                 unname(sp$estimate), tolerance = 1e-10)
    expect_equal(df$value[df$measure == "Regression Slope"],
                 unname(coef(fit)[2]), tolerance = 1e-10)
    expect_equal(df$value[df$measure == "R-squared (Effect Size)"],
                 summary(fit)$r.squared, tolerance = 1e-10)
})

test_that("the summary table matches base R", {
    d <- lc_trend()
    res <- linechart(data = d, xvar = "t", yvar = "y")
    expect_equal(as.numeric(lc_cell(res, "Number of Observations")), nrow(d))
    expect_equal(as.numeric(lc_cell(res, "Y Mean")), mean(d$y), tolerance = 5e-3)
    expect_equal(as.numeric(lc_cell(res, "Y Median")), stats::median(d$y), tolerance = 5e-3)
    expect_equal(as.numeric(lc_cell(res, "Y Standard Deviation")), stats::sd(d$y), tolerance = 5e-3)
})

test_that("the repeated-measures independence limitation is stated", {
    # Several observations per time point means the correlation and regression
    # p-values assume independence they do not have.
    set.seed(4)
    d <- data.frame(t = rep(1:5, each = 6), y = rnorm(30))
    res <- linechart(data = d, xvar = "t", yvar = "y", trendline = TRUE)
    expect_match(lc_txt(res$assumptions$content), "independen", ignore.case = TRUE)
})


# ---- what the confidence band actually is ------------------------------------

test_that("the confidence band is disclosed as a model fit, not point uncertainty", {
    # geom_smooth() fits its OWN model - a straight line by default, loess when
    # Smooth is on - while the visible line is geom_line() through the observed
    # values. A reader sees a ribbon hugging a wiggly line and assumes it is that
    # line's uncertainty. Verified against predict(lm, interval = "confidence"):
    # the band matches to 0 difference, i.e. it is exactly a linear-fit CI.
    d <- lc_trend()

    linear <- lc_todo(linechart(data = d, xvar = "t", yvar = "y", confidence = TRUE))
    expect_match(linear, "straight-line")
    expect_match(linear, "connects the observed values")

    loess <- lc_todo(linechart(data = d, xvar = "t", yvar = "y",
                               confidence = TRUE, smooth = TRUE))
    expect_match(loess, "LOESS fit")

    expect_false(grepl("shaded band", lc_todo(linechart(data = d, xvar = "t", yvar = "y")),
                       fixed = TRUE))
})

test_that("the band really is a linear-fit confidence interval", {
    # Pins the claim the wording above makes. If ggplot2 ever changes what
    # geom_smooth draws, this fails and the wording must be revisited.
    skip_if_not_installed("ggplot2")
    d <- lc_trend()
    gg <- ggplot2::ggplot(d, ggplot2::aes(t, y)) +
        ggplot2::geom_smooth(method = "lm", se = TRUE, formula = y ~ x)
    bd <- ggplot2::ggplot_build(gg)$data[[1]]
    pr <- stats::predict(stats::lm(y ~ t, data = d),
                         newdata = data.frame(t = bd$x),
                         interval = "confidence", level = 0.95)
    expect_equal(bd$ymin, unname(pr[, "lwr"]), tolerance = 1e-9)
    expect_equal(bd$ymax, unname(pr[, "upr"]), tolerance = 1e-9)
})


# ---- base::format masking (module-wide) --------------------------------------

test_that("no source file calls a bare format() with base-format arguments", {
    # jmvcore::format(str, ..., context) is a string-template interpolator: it
    # substitutes {} placeholders and returns its input untouched otherwise, so
    # digits/big.mark/nsmall are silently dropped. `@import jmvcore` puts it ahead
    # of base::format for the WHOLE package, so every unqualified call was
    # affected - 97 sites across 20 files, e.g. "Cost Analysis (Per 10000
    # Patients)" instead of "10,000".
    # No skip_on_cran() here: this is a source-code guard, not a slow or
    # environment-dependent check, and skipping it is how the regression would
    # creep back in unnoticed.
    r_dir <- testthat::test_path("..", "..", "R")
    skip_if_not(dir.exists(r_dir), "package sources not available")

    args_re <- "digits|nsmall|big\\.mark|scientific|width|justify|trim|small\\.mark|decimal\\.mark"
    files <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
    files <- files[!grepl("[.]h[.]R$", files)]
    offenders <- character()
    for (f in files) {
        L <- readLines(f, warn = FALSE)
        hit <- grep(paste0("(^|[^.:$_[:alnum:]])format\\([^)]*(", args_re, ")\\s*="), L)
        hit <- hit[!grepl("^\\s*#", L[hit])]
        if (length(hit)) offenders <- c(offenders, sprintf("%s:%s", basename(f), paste(hit, collapse = ",")))
    }
    expect_equal(offenders, character(0))
})


# ---- assumption checks -------------------------------------------------------

test_that("the assumptions panel checks constant variance, not just claims it", {
    # The panel listed Homoscedasticity as a key assumption but only ever tested
    # linearity and normality, so it asserted something it had not checked.
    lc_assum <- function(...) lc_txt(linechart(...)$assumptions$content)

    # Fan-shaped residuals: spread grows with x, so the screen must fire.
    set.seed(7)
    n <- 80
    x <- seq(1, 40, length.out = n)
    hetero <- data.frame(t = x, y = 5 + 0.5 * x + rnorm(n, 0, x / 4))
    expect_match(lc_assum(data = hetero, xvar = "t", yvar = "y", trendline = TRUE),
                 "Residual spread changes with the fitted value")

    # Constant-variance residuals: the screen must NOT fire.
    set.seed(8)
    homo <- data.frame(t = x, y = 5 + 0.5 * x + rnorm(n, 0, 2))
    expect_match(lc_assum(data = homo, xvar = "t", yvar = "y", trendline = TRUE),
                 "Residual spread looks constant")
})

test_that("the dependency gate lists no package the analysis never calls", {
    # dplyr sat in .checkDependencies() but is called nowhere in linechart, so a
    # missing dplyr aborted the run for a package it does not use. Source guard:
    # a relative path would resolve against tests/testthat, so use test_path().
    f <- testthat::test_path("..", "..", "R", "linechart.b.R")
    skip_if_not(file.exists(f), "package sources not available")
    L <- readLines(f, warn = FALSE)
    code <- L[!grepl("^\\s*#", L)]
    expect_false(any(grepl("dplyr", code, fixed = TRUE)))
})


# ---- reported statistics --------------------------------------------------

test_that("every statistic the module computes is actually reported", {
    # The CI, the slope p and the ANOVA df were all computed and then discarded,
    # so the table showed r and an asterisk code and nothing a clinician could
    # put in a report. Each new cell is asserted against base R.
    set.seed(3)
    d <- data.frame(t = 1:40, y = 2 + 0.4 * (1:40) + rnorm(40, 0, 3))
    df <- linechart(data = d, xvar = "t", yvar = "y", trendline = TRUE)$correlation$asDF

    ct  <- stats::cor.test(d$t, d$y, method = "pearson")
    sp  <- suppressWarnings(stats::cor.test(d$t, d$y, method = "spearman"))
    fit <- stats::lm(y ~ t, data = d)

    pe <- df[df$measure == "Pearson Correlation", ]
    expect_equal(pe$ci_lower, ct$conf.int[1], tolerance = 1e-10)
    expect_equal(pe$ci_upper, ct$conf.int[2], tolerance = 1e-10)
    expect_equal(pe$pvalue,   ct$p.value,     tolerance = 1e-10)

    expect_equal(df$pvalue[df$measure == "Regression Slope"],
                 unname(summary(fit)$coefficients[2, 4]), tolerance = 1e-10)
    expect_equal(df$pvalue[df$measure == "Spearman Correlation (Rank-based)"],
                 unname(sp$p.value), tolerance = 1e-10)
})

test_that("the ANOVA row carries its degrees of freedom and p-value", {
    # "F = 146.9" with no df cannot be interpreted or reported.
    set.seed(4)
    dc <- data.frame(v = factor(rep(c("Base", "W4", "W8"), each = 10)),
                     y = c(rnorm(10, 10), rnorm(10, 13), rnorm(10, 16)))
    df <- linechart(data = dc, xvar = "v", yvar = "y", trendline = TRUE)$correlation$asDF
    a  <- anova(lm(y ~ v, data = dc))

    row <- df[grepl("ANOVA", df$measure), ]
    expect_equal(nrow(row), 1L)
    expect_match(row$measure, "df 2, 27", fixed = TRUE)
    expect_equal(row$value,  a$`F value`[1], tolerance = 1e-10)
    expect_equal(row$pvalue, a$`Pr(>F)`[1],  tolerance = 1e-10)
})

test_that("significance is stated in words, not as an asterisk code", {
    set.seed(3)
    d <- data.frame(t = 1:40, y = 2 + 0.4 * (1:40) + rnorm(40, 0, 3))
    txt <- linechart(data = d, xvar = "t", yvar = "y",
                     trendline = TRUE)$correlation$asDF
    interp <- txt$interpretation[txt$measure == "Pearson Correlation"]
    expect_match(interp, "statistically significant", fixed = TRUE)
    expect_false(grepl("(***)", interp, fixed = TRUE))
})
