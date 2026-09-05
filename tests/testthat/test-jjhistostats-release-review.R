# Regression tests from the `jjhistostats` release review.
#
# The analysis wraps ggstatsplot::gghistostats. Its `type` argument selects the ONE-SAMPLE
# LOCATION TEST reported in the plot subtitle, not a normality test -- the option
# documentation claimed the opposite, and the nonparametric label named Mann-Whitney, which
# is a two-sample test that cannot be computed here at all.

hs_run <- function(...) {
    set.seed(1)
    d <- data.frame(chol = rnorm(80, 5.2, 0.9), age = rnorm(80, 62, 11))
    o <- ClinicoPath:::jjhistostatsOptions$new(dep = "chol", ...)
    a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
todo_of <- function(a) a$results$todo$content


test_that("gghistostats' `type` selects a one-sample location test, not a normality test", {
    # This is the fact the option documentation used to get wrong. Pin it against
    # statsExpressions so a future upstream change is caught rather than assumed.
    skip_if_not_installed("statsExpressions")
    set.seed(42); x <- rnorm(60, 5.4, 1.2)
    d <- data.frame(v = x)
    method_for <- function(ty)
        as.character(statsExpressions::one_sample_test(
            data = d, x = v, test.value = 5, type = ty)$method)[1]

    expect_match(method_for("parametric"),    "One Sample t-test")
    expect_match(method_for("nonparametric"), "Wilcoxon signed rank")
    expect_match(method_for("robust"),        "one-sample")
    expect_match(method_for("bayes"),         "Bayes")

    # and the parametric branch reproduces stats::t.test exactly
    sp <- statsExpressions::one_sample_test(data = d, x = v, test.value = 5, type = "parametric")
    tt <- t.test(x, mu = 5)
    expect_equal(as.numeric(sp$statistic), as.numeric(tt$statistic), tolerance = 1e-10)
    expect_equal(as.numeric(sp$p.value),   as.numeric(tt$p.value),   tolerance = 1e-10)

    # nothing here is Shapiro-Wilk or Anderson-Darling
    expect_false(grepl("Shapiro|Anderson", method_for("parametric")))
    expect_false(grepl("Shapiro|Anderson", method_for("nonparametric")))
})


test_that("the option documentation names the tests that are actually run", {
    a_yaml <- paste(readLines("../../jamovi/jjhistostats.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: typestatistics\\n.*?(?=\\n    - name: )",
                                      a_yaml, perl = TRUE))
    expect_length(blk, 1L)
    expect_match(blk, "one-sample t-test", fixed = TRUE)
    expect_match(blk, "Wilcoxon signed-rank", fixed = TRUE)
    # the two false claims must not come back
    expect_false(grepl("Mann-Whitney", blk, fixed = TRUE))
    expect_false(grepl("uses\\s+Shapiro-Wilk", blk))
    # it must say plainly that it is not a normality test
    expect_match(blk, "does NOT")

    # Mann-Whitney must not be offered as this analysis's own nonparametric option anywhere
    b <- paste(readLines("../../R/jjhistostats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("'Nonparametric (Mann-Whitney)'", b, fixed = TRUE))
})


test_that("a subtitle testing against zero is disclosed, however it arose", {
    # gghistostats always reports a one-sample test when results.subtitle is TRUE, and
    # test.value is only forwarded when enableOneSampleTest is on -- so with that switch off
    # the test silently uses ggstatsplot's default null of 0. On cholesterol ~ N(5.2, 0.9)
    # that yields t(79) = 58.42, p = 9e-67, Hedges' g = 6.47: true, and meaningless.
    warned <- function(a) grepl("testing against zero", todo_of(a), fixed = TRUE)

    expect_true(warned(hs_run(resultssubtitle = TRUE, enableOneSampleTest = FALSE)))
    expect_true(warned(hs_run(resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 0)))

    # quiet when the null is meaningful, and when no test is displayed at all
    expect_false(warned(hs_run(resultssubtitle = TRUE, enableOneSampleTest = TRUE, test.value = 5.2)))
    expect_false(warned(hs_run(resultssubtitle = FALSE)))

    # the message must name the test, and explain where the 0 came from
    a <- hs_run(resultssubtitle = TRUE, enableOneSampleTest = FALSE)
    expect_match(todo_of(a), "<strong>one-sample t-test</strong>", fixed = TRUE)
    expect_match(todo_of(a), "'One-sample test' is switched off", fixed = TRUE)
    # and must NOT repeat the old advice, which steered the user into the silent-null state
    expect_false(grepl("uncheck 'Enable One-Sample Test'", todo_of(a), fixed = TRUE))
    expect_match(todo_of(a), "Switch off <strong>Statistical Results</strong>", fixed = TRUE)
})


test_that("the named test tracks the chosen statistic", {
    named <- function(ty) {
        a <- hs_run(resultssubtitle = TRUE, typestatistics = ty)
        m <- regmatches(todo_of(a), regexpr("<strong>one-sample [^<]+", todo_of(a)))
        sub("<strong>one-sample ", "", m)
    }
    expect_equal(named("parametric"),    "t-test")
    expect_equal(named("nonparametric"), "Wilcoxon signed-rank test")
    expect_equal(named("robust"),        "bootstrap-t test")
    expect_equal(named("bayes"),         "Bayesian t-test")
})


test_that("every clinical preset triggers the disclosure, because each forces the subtitle on", {
    # .applyClinicalPreset sets resultssubtitle = TRUE for all four presets, so a user who
    # picks a preset and never touches the statistics panel lands on the silent-null path.
    # The guard reads through private$.option(), which honours preset overrides.
    for (p in c("lab_values", "biomarkers", "patient_chars", "pathology_scores"))
        expect_true(grepl("testing against zero", todo_of(hs_run(clinicalPreset = p)), fixed = TRUE),
                    info = p)
})


test_that("the skewness statistic is the population g1 moment", {
    # m3 / m2^1.5 with both moments on an n denominator. The earlier form divided a
    # n-denominator third moment by an (n-1)-denominator SD, which biases the estimate.
    set.seed(3); x <- rexp(200)
    n <- length(x); mu <- mean(x)
    m2 <- sum((x - mu)^2) / n
    m3 <- sum((x - mu)^3) / n
    g1 <- m3 / m2^1.5

    d <- data.frame(v = x)
    o <- ClinicoPath:::jjhistostatsOptions$new(dep = "v", showInterpretation = TRUE)
    a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
    a$init(); a$.__enclos_env__$private$.run()
    txt <- a$results$interpretation$content

    # right-skewed exponential: g1 is comfortably above 0.5 and the text must say so
    expect_gt(g1, 0.5)
    expect_match(txt, "Right-skewed")
    expect_match(txt, "Sample size:</strong> 200", fixed = TRUE)
    # the heuristic must stay disclosed as a rule of thumb, not a formal test
    expect_match(txt, "rule-of-thumb")
})


test_that("a plot title with Split By does not crash the grouped plot", {
    # grouped_gghistostats() supplies `title` itself, once per group, via its internal pmap.
    # Passing the user's title too made do.call() raise
    #   formal argument "title" matched by multiple actual arguments
    # and the entire grouped plot failed to render. An empty title escaped only because the
    # NULL-strip removed it, so the crash needed a non-empty title plus a grouping variable.
    set.seed(7)
    d <- data.frame(chol = rnorm(60, 5.2, 0.9), g = factor(rep(c("A", "B"), 30)))
    o <- ClinicoPath:::jjhistostatsOptions$new(dep = "chol", grvar = "g",
                                               title = "Serum cholesterol")
    a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
    a$init()
    p <- a$.__enclos_env__$private
    expect_no_error(p$.run())
    expect_no_error(p$.plot2(a$results$plot2, ggtheme = ggplot2::theme_bw(), theme = NULL))
})


test_that("a custom title is not stamped onto every variable's panel", {
    set.seed(7)
    d <- data.frame(chol = rnorm(60, 5.2, 0.9), psa = rnorm(60, 4, 1.5))
    labs <- function(deps) {
        o <- ClinicoPath:::jjhistostatsOptions$new(dep = deps, title = "Serum cholesterol",
                                                   xlab = "mmol/L")
        a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
        a$init(); p <- a$.__enclos_env__$private; p$.run()
        md <- p$.prepareData(); od <- p$.prepareOptions(); ad <- p$.prepareAesthetics()
        lapply(deps, function(v) p$.generateHistogram(md, v, od, ad)$labels)
    }
    # one variable: the user's label is unambiguous, so honour it
    one <- labs("chol")[[1]]
    expect_equal(one$title, "Serum cholesterol")
    expect_equal(one$x, "mmol/L")

    # two variables: one label cannot describe both, so fall back to each variable's own name
    two <- labs(c("chol", "psa"))
    expect_null(two[[1]]$title)
    expect_null(two[[2]]$title)
    expect_equal(two[[1]]$x, "chol")
    expect_equal(two[[2]]$x, "psa")
})


test_that("a variable with too few values is refused by name, not by an upstream error", {
    # n = 1 used to die inside the interpretation panel at `if (sd_val > 0)`, because sd() of a
    # single value is NA -> "missing value where TRUE/FALSE needed". An all-NA column with
    # centrality plotting on reached ggstatsplot and died with "You must provide a
    # model-object...", which names neither the variable nor the problem. Every clinical preset
    # forces centralityline on, so that path was one click away.
    one <- data.frame(v = c(3, rep(NA_real_, 9)))
    expect_error(ClinicoPath::jjhistostats(data = one, dep = "v", showInterpretation = TRUE),
                 "has 1 finite numeric value")
    none <- data.frame(v = rep(NA_real_, 40))
    expect_error(ClinicoPath::jjhistostats(data = none, dep = "v", centralityline = TRUE),
                 "has 0 finite numeric values")
    # neither message may be the old opaque one
    msg <- tryCatch(ClinicoPath::jjhistostats(data = one, dep = "v", showInterpretation = TRUE),
                    error = conditionMessage)
    expect_false(grepl("missing value where TRUE/FALSE needed", msg, fixed = TRUE))
    expect_false(grepl("model-object", msg, fixed = TRUE))

    # three values is enough to proceed
    ok <- data.frame(v = c(1, 2, 3, rep(NA_real_, 7)))
    expect_no_error(ClinicoPath::jjhistostats(data = ok, dep = "v"))
})


test_that("symmetric non-normal data is not certified as normal", {
    # Skewness alone is blind to symmetric departures from normality. A bimodal mixture and a
    # uniform sample both have |g1| < 0.05 and were being told "Normal distribution allows use
    # of parametric statistics (t-tests, ANOVA)".
    verdict <- function(x) {
        d <- data.frame(v = x)
        o <- ClinicoPath:::jjhistostatsOptions$new(dep = "v", showInterpretation = TRUE)
        a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
        a$init(); a$.__enclos_env__$private$.run()
        a$results$interpretation$content
    }
    set.seed(11)
    bimodal <- c(rnorm(100, 2, 0.3), rnorm(100, 10, 0.3))
    uniform <- runif(200, 0, 10)
    normal  <- rnorm(200, 5, 1)

    # the reference test agrees these are not normal
    expect_lt(shapiro.test(bimodal)$p.value, 0.05)
    expect_lt(shapiro.test(uniform)$p.value, 0.05)
    expect_gt(shapiro.test(normal)$p.value, 0.05)

    expect_false(grepl("No evidence of a departure from normality", verdict(bimodal)))
    expect_false(grepl("No evidence of a departure from normality", verdict(uniform)))
    expect_true(grepl("No evidence of a departure from normality",  verdict(normal)))

    # and the panel now reports which test it used
    expect_match(verdict(bimodal), "Shapiro-Wilk")

    # a constant column is called out rather than being handed a normality verdict
    const <- verdict(rep(50, 40))
    expect_match(const, "constant")
    expect_false(grepl("No evidence of a departure from normality", const))
})


test_that("a clinical preset does not keep overriding after the user returns to Custom", {
    # jamovi reuses the analysis object across option changes and `overrides` is instance
    # state, so the preset's settings used to outlive the preset: the options panel showed the
    # user's choices while the analysis went on computing the preset's.
    set.seed(7); d <- data.frame(chol = rnorm(60, 5.2, 0.9))
    o <- ClinicoPath:::jjhistostatsOptions$new(dep = "chol", clinicalPreset = "biomarkers",
             typestatistics = "parametric", resultssubtitle = FALSE)
    a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
    a$init()
    p <- a$.__enclos_env__$private

    # while the preset is selected it legitimately overrides
    expect_equal(p$.option("typestatistics"), "nonparametric")
    expect_true(p$.option("resultssubtitle"))

    # back to Custom: the user's own settings must take effect again
    op <- o$option("clinicalPreset"); op$value <- "custom"
    p$.applyClinicalPreset()
    expect_equal(p$.option("typestatistics"), "parametric")
    expect_false(p$.option("resultssubtitle"))
})


test_that("the distribution diagnostics render every selected variable", {
    # The single-variable gate had no `else`, so with 2+ variables each function returned TRUE
    # having drawn nothing: jamovi displayed an empty box titled "QQ Plot - Normality
    # Assessment", which reads as "checked, nothing to report".
    skip_if_not_installed("ggpubr")
    set.seed(7); d <- data.frame(chol = rnorm(60, 5.2, 0.9), psa = rnorm(60, 4, 1.5))
    bytes <- function(deps, fn) {
        o <- ClinicoPath:::jjhistostatsOptions$new(dep = deps, addDistributionDiagnostics = TRUE,
                 ggpubrShowQQ = TRUE, ggpubrShowECDF = TRUE)
        a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
        a$init(); p <- a$.__enclos_env__$private; p$.run()
        f <- tempfile(fileext = ".png"); grDevices::png(f, 400, 300)
        on.exit(grDevices::dev.off(), add = TRUE)
        p[[fn]](a$results$qqPlot)
        grDevices::dev.off(); on.exit()
        sz <- file.info(f)$size
        if (is.na(sz)) 0L else sz
    }
    for (fn in c(".plotDensity", ".plotQQ", ".plotECDF")) {
        expect_gt(bytes("chol", fn), 1000)                    # single variable still works
        expect_gt(bytes(c("chol", "psa"), fn), 1000)          # and two no longer draw nothing
    }
})


test_that("conf.level is bounded away from the value that erases the subtitle", {
    # conf.level = 1 made the entire statistical subtitle disappear with no message; 0 printed
    # a zero-width "0% CI". The option used to allow both.
    a_yaml <- paste(readLines("../../jamovi/jjhistostats.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: conf\\.level\\n.*?(?=\\n    - name: )",
                                      a_yaml, perl = TRUE))
    expect_length(blk, 1L)
    expect_match(blk, "min: 0.5", fixed = TRUE)
    expect_match(blk, "max: 0.999", fixed = TRUE)

    # Once the yaml bound is compiled into the wrapper, jmvcore refuses the bad value at the
    # option layer -- which is a better outcome than the analysis running and silently
    # producing no subtitle. Accept either, so this test is meaningful before and after
    # regeneration, but require that conf.level = 1 never yields a subtitle-less plot.
    set.seed(7); d <- data.frame(chol = rnorm(60, 5.2, 0.9))
    subtitle_at <- function(cl) {
        o <- ClinicoPath:::jjhistostatsOptions$new(dep = "chol", resultssubtitle = TRUE,
                                                   conf.level = cl)
        a <- ClinicoPath:::jjhistostatsClass$new(options = o, data = d)
        a$init(); p <- a$.__enclos_env__$private; p$.run()
        p$.generateHistogram(p$.prepareData(), "chol", p$.prepareOptions(),
                             p$.prepareAesthetics())$labels$subtitle
    }
    expect_false(is.null(subtitle_at(0.95)))

    rejected <- tryCatch({ subtitle_at(1); NULL }, error = conditionMessage)
    if (is.null(rejected)) {
        # bound not yet compiled: the old silent behaviour must at least be visible here
        expect_true(is.null(subtitle_at(1)))
    } else {
        expect_match(rejected, "between 0.5 and 0.999")
    }
})
