# Release review of jjdotplotstats.
#
# Every block reproduces a user-visible symptom rather than a code path, and the
# numeric cases are checked against stats/effectsize rather than against the
# module's own arithmetic.

# ---- helpers ---------------------------------------------------------------

dp_data3 <- function(seed = 42) {
    set.seed(seed)
    data.frame(
        v = c(rnorm(40, 10, 2), rnorm(40, 13, 2), rnorm(40, 11, 2)),
        g = factor(rep(c("A", "B", "C"), each = 40)),
        s = factor(rep(c("M", "F"), 60))
    )
}
dp_data2 <- function(seed = 42) {
    set.seed(seed)
    data.frame(v = c(rnorm(40, 10, 2), rnorm(40, 13, 2)),
               g = factor(rep(c("A", "B"), each = 40)))
}

# Render an Image result item and return the text actually drawn.
dp_render <- function(res, item = "plot", width = 10, height = 6) {
    f <- tempfile(fileext = ".svg")
    svglite::svglite(f, width = width, height = height)
    ok <- tryCatch({ print(res[[item]]); TRUE },
                   error = function(e) conditionMessage(e))
    grDevices::dev.off()
    x <- readLines(f, warn = FALSE); unlink(f)
    list(ok = ok,
         svg = paste(x, collapse = ""),
         txt = paste(gsub("^>|</text>$", "",
                          regmatches(x, regexpr(">[^<]*</text>", x))), collapse = ""))
}
dp_notices <- function(res) gsub("<[^>]*>", "", res$notices$content %||% "")
`%||%` <- function(a, b) if (is.null(a)) b else a

# Loading ClinicoPath pulls in formula.tools (via logistf), which breaks
# stats::oneway.test for the whole session - the very defect these tests cover.
# The reference values therefore have to be computed with base behaviour
# restored, exactly as the module does internally.
dp_base_formula <- function(expr) {
    tbl <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
    if (exists("as.character.formula", envir = tbl, inherits = FALSE)) {
        old <- get("as.character.formula", envir = tbl, inherits = FALSE)
        assign("as.character.formula", function(x, ...) as.character(unclass(x)), envir = tbl)
        on.exit(assign("as.character.formula", old, envir = tbl), add = TRUE)
    }
    force(expr)
}


# ---- statistical validity --------------------------------------------------

test_that("the reported Welch ANOVA matches stats::oneway.test", {
    d <- dp_data3()
    r <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                  resultssubtitle = TRUE, k = 4))
    expect_true(isTRUE(r$ok))

    a <- dp_base_formula(stats::oneway.test(v ~ g, data = d, var.equal = FALSE))
    expect_match(r$txt, sprintf("FWelch(2, %.4f)=%.4f",
                                a$parameter[2], a$statistic), fixed = TRUE)
})

test_that("partial eta-squared equals df1*F/(df1*F+df2) for the Welch model", {
    d <- dp_data3()
    a <- dp_base_formula(stats::oneway.test(v ~ g, data = d, var.equal = FALSE))
    eta <- unname(a$parameter[1] * a$statistic /
                  (a$parameter[1] * a$statistic + a$parameter[2]))

    r <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                  resultssubtitle = TRUE, effsizetype = "eta", k = 4))
    expect_match(r$txt, sprintf("=%.4f", eta), fixed = TRUE)
})

test_that("two-group Cohen's d and Hedges' g match effectsize", {
    skip_if_not_installed("effectsize")
    d <- dp_data2()
    # The reported t is Welch's, so the effect size must use the UNPOOLED SD to
    # stay consistent with it - pooled would be a different estimator
    # (-1.4848 vs -1.4836 on this data).
    dd <- effectsize::cohens_d(v ~ g, data = d, pooled_sd = FALSE)$Cohens_d
    gg <- effectsize::hedges_g(v ~ g, data = d, pooled_sd = FALSE)$Hedges_g

    rd <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                   resultssubtitle = TRUE, effsizetype = "biased", k = 4))
    rg <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                   resultssubtitle = TRUE, effsizetype = "unbiased", k = 4))
    expect_match(rd$txt, sprintf("dCohen=%.4f", dd), fixed = TRUE)
    expect_match(rg$txt, sprintf("Hedges=%.4f", gg), fixed = TRUE)
})

test_that("the nonparametric branch reports Kruskal-Wallis", {
    d <- dp_data3()
    kw <- dp_base_formula(stats::kruskal.test(v ~ g, data = d))
    r <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                  resultssubtitle = TRUE,
                                  typestatistics = "nonparametric", k = 4))
    expect_match(r$txt, sprintf("=%.4f", unname(kw$statistic)), fixed = TRUE)
})


# ---- regressions fixed in this review --------------------------------------

test_that("a three-group comparison actually shows its statistics", {
    # formula.tools (loaded transitively via logistf, an Imports of this package)
    # replaces as.character.formula with one returning a single string, which
    # makes stats::oneway.test reject every formula with "a two-sided formula is
    # required". ggstatsplot swallowed that and returned subtitle = NULL, so the
    # user ticked "Statistical results in plot" and got a figure with none.
    skip_if_not(requireNamespace("formula.tools", quietly = TRUE))
    loadNamespace("formula.tools")

    r <- dp_render(jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                                  resultssubtitle = TRUE))
    expect_true(isTRUE(r$ok))
    expect_match(r$txt, "FWelch", fixed = TRUE)
})

test_that("the effect size selector changes the reported effect size", {
    d <- dp_data3()
    eta   <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                      resultssubtitle = TRUE, effsizetype = "eta"))$txt
    omega <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                      resultssubtitle = TRUE, effsizetype = "omega"))$txt
    expect_false(identical(eta, omega))
    expect_match(eta,   "η", fixed = TRUE)   # eta
    expect_match(omega, "ω", fixed = TRUE)   # omega
})

test_that("the Split By figure renders instead of failing on a title collision", {
    # grouped_ggbetweenstats titles each panel with its level name, so passing
    # `title` through ... threw 'formal argument "title" matched by multiple
    # actual arguments' for every Split By analysis. The error was raised at
    # render time, where notices are discarded, so the panel was simply empty.
    r <- dp_render(jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                                  grvar = "s", resultssubtitle = TRUE), item = "plot2")
    expect_true(isTRUE(r$ok))
    expect_match(r$txt, "FWelch", fixed = TRUE)
    expect_false(grepl("could not be drawn", r$txt, fixed = TRUE))
})

test_that("a Split By figure still renders when a plot title is set", {
    r <- dp_render(jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                                  grvar = "s", resultssubtitle = TRUE,
                                  mytitle = "Biomarker by sex"), item = "plot2")
    expect_true(isTRUE(r$ok))
    expect_match(r$txt, "Biomarker by sex", fixed = TRUE)
})

test_that("an infinite value is excluded and disclosed rather than blanking the plot", {
    d <- dp_data3(); d$v[3] <- Inf
    res <- jjdotplotstats(data = d, dep = "v", group = "g", resultssubtitle = TRUE)

    expect_match(res$todo$content, "infinite value")
    r <- dp_render(res)
    expect_true(isTRUE(r$ok))
    expect_match(r$txt, "FWelch", fixed = TRUE)      # the figure is not empty
    expect_match(r$txt, "(n = 39)", fixed = TRUE)    # the Inf row is gone
})

test_that("a constant dependent variable is rejected in .run(), where notices render", {
    d <- data.frame(v = rep(5, 60), g = factor(rep(c("A", "B", "C"), each = 20)))
    res <- jjdotplotstats(data = d, dep = "v", group = "g")
    expect_match(dp_notices(res), "no variation to compare")
    expect_false(grepl("completed successfully", dp_notices(res), fixed = TRUE))
})

test_that("nothing claims success before the figure exists", {
    res <- jjdotplotstats(data = dp_data3(), dep = "v", group = "g")
    n <- dp_notices(res)
    expect_false(grepl("completed successfully", n, fixed = TRUE))
    expect_match(n, "Comparing 3 groups")
})

test_that("contradictory centrality settings are reported, not silently resolved", {
    res <- jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                          centralityplotting = TRUE,
                          centralityparameter = "mean",
                          centralitytype = "nonparametric")
    expect_match(dp_notices(res), "disagree")
})

test_that("agreeing centrality settings raise no warning", {
    res <- jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                          centralityplotting = TRUE,
                          centralityparameter = "mean",
                          centralitytype = "parametric")
    expect_false(grepl("disagree", dp_notices(res), fixed = TRUE))
})

test_that("the retired centrality precision option says so", {
    res <- jjdotplotstats(data = dp_data3(), dep = "v", group = "g", centralityk = 4)
    expect_match(dp_notices(res), "no longer has any effect")
})

test_that("exclusion messages survive an option-only change (cache hit)", {
    # .prepareData is keyed on the variables and data dimensions, so changing an
    # OPTION is a cache hit. The disclosure used to vanish while the analysis
    # still excluded the rows.
    d <- dp_data3(); d$v[1:5] <- NA
    res1 <- jjdotplotstats(data = d, dep = "v", group = "g", conflevel = 0.95)
    res2 <- jjdotplotstats(data = d, dep = "v", group = "g", conflevel = 0.99)
    expect_match(res1$todo$content, "rows excluded")
    expect_match(res2$todo$content, "rows excluded")
})

test_that("the formula shield is restored after every run", {
    skip_if_not(requireNamespace("formula.tools", quietly = TRUE))
    loadNamespace("formula.tools")
    before <- getS3method("as.character", "formula")
    invisible(dp_render(jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                                       resultssubtitle = TRUE)))
    expect_identical(getS3method("as.character", "formula"), before)

    # ...including when the plot fails
    dbad <- data.frame(v = rep(1, 10), g = factor(rep(c("A", "B"), 5)))
    invisible(try(dp_render(jjdotplotstats(data = dbad, dep = "v", group = "g")), silent = TRUE))
    expect_identical(getS3method("as.character", "formula"), before)
})


# ---- options actually reaching the figure ----------------------------------

test_that("the reference line is drawn only when asked for", {
    d <- dp_data3()
    on  <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                    testvalueline = TRUE,  testvalue = 12))$svg
    off <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                    testvalueline = FALSE, testvalue = 12))$svg
    n <- function(s) length(regmatches(s, gregexpr("stroke-dasharray", s))[[1]])
    expect_gt(n(on), n(off))
})

test_that("decimal places reach the rendered statistics", {
    d <- dp_data3()
    k0 <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                   resultssubtitle = TRUE, k = 0))$txt
    k4 <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                   resultssubtitle = TRUE, k = 4))$txt
    expect_false(identical(k0, k4))
})

test_that("every statistical test type renders", {
    d <- dp_data3()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        r <- dp_render(jjdotplotstats(data = d, dep = "v", group = "g",
                                      resultssubtitle = TRUE, typestatistics = ty))
        expect_true(isTRUE(r$ok), info = ty)
        expect_false(grepl("could not be drawn", r$txt, fixed = TRUE))
    }
})

test_that("axis labels follow the flipped orientation", {
    r <- dp_render(jjdotplotstats(data = dp_data3(), dep = "v", group = "g",
                                  xtitle = "Marker (ng/mL)", ytitle = "Tumour grade"))
    expect_match(r$txt, "Marker (ng/mL)", fixed = TRUE)
    expect_match(r$txt, "Tumour grade", fixed = TRUE)
})


# ---- validation and edge cases ---------------------------------------------

test_that("a single group is rejected with an actionable message", {
    d <- data.frame(v = rnorm(30), g = factor(rep("A", 30)))
    expect_match(dp_notices(jjdotplotstats(data = d, dep = "v", group = "g")),
                 "At least two groups")
})

test_that("an out-of-range confidence level is rejected", {
    expect_match(dp_notices(jjdotplotstats(data = dp_data3(), dep = "v",
                                           group = "g", conflevel = 1)),
                 "greater than 0 and less than 1")
})

test_that("small samples and small groups are flagged", {
    d <- data.frame(v = rnorm(12), g = factor(rep(c("A", "B"), each = 6)))
    n <- dp_notices(jjdotplotstats(data = d, dep = "v", group = "g"))
    expect_match(n, "Small total sample size")
    expect_match(n, "Very small group sizes")
})

test_that("an incomplete variable selection shows the welcome text, not an error", {
    # Only `dep` chosen: .run() takes the same welcome branch as "nothing
    # selected", but at least one variable exists so jmvcore::select() has
    # something to build a frame from. (With NO variable at all, select() makes a
    # zero-column frame and dies in row.names<- before any module code runs -
    # a jmvcore-wide artefact of the R entry point that the GUI never reaches.)
    res <- jjdotplotstats(data = dp_data3(), dep = "v", group = NULL)
    expect_match(res$todo$content, "Welcome")
    expect_equal(dp_notices(res), "")
})

test_that("the syntax generator quotes variable names that need it", {
    d <- dp_data3(); names(d)[1:2] <- c("Marker Level", "Tumour Grade")
    res <- jjdotplotstats(data = d, dep = "Marker Level", group = "Tumour Grade")
    src <- res$analysis$asSource()
    expect_match(src, '"Marker Level"', fixed = TRUE)
    expect_match(src, '"Tumour Grade"', fixed = TRUE)
    expect_equal(length(gregexpr("Marker Level", src)[[1]]), 1L)  # not duplicated
    expect_silent(parse(text = src))
})
