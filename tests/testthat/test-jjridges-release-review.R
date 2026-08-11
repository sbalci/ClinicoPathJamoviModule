# Regression tests from the `jjridges` release review.
#
# jjridges is not only a plot: it runs k(k-1)/2 pairwise tests, computes effect sizes with
# confidence intervals, and adjusts p-values. Every estimator below was checked against an
# independent implementation before these expectations were written.

rg_run <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jjridgesOptions$new,
                 utils::modifyList(list(x_var = "v", y_var = "g"), list(...)))
    a <- ClinicoPath:::jjridgesClass$new(options = o, data = dat)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
five_groups <- function(seed = 9) {
    set.seed(seed)
    data.frame(v = rnorm(150, 10, 2), g = factor(rep(paste0("Stage", 1:5), each = 30)))
}


test_that("effect-size point estimates match independent implementations", {
    skip_if_not_installed("effectsize")
    set.seed(42); a <- rnorm(40, 10, 2); b <- rnorm(35, 11.5, 2.2)
    est <- function(ty) {
        o <- ClinicoPath:::jjridgesOptions$new(x_var = "v", y_var = "g", effsize_type = ty)
        cls <- ClinicoPath:::jjridgesClass$new(options = o,
                 data = data.frame(v = c(a, b), g = factor(rep(c("A", "B"), c(40, 35)))))
        cls$.__enclos_env__$private$.calculateEffectSizeWithCI(a, b)
    }
    # Cohen's d against effectsize AND against the hand-computed pooled-SD formula
    sp <- sqrt(((40 - 1) * var(a) + (35 - 1) * var(b)) / (40 + 35 - 2))
    expect_equal(est("d")$effect_size, (mean(a) - mean(b)) / sp, tolerance = 1e-10)
    expect_equal(est("d")$effect_size,
                 as.numeric(effectsize::cohens_d(a, b, pooled_sd = TRUE)$Cohens_d),
                 tolerance = 1e-10)
    expect_equal(est("g")$effect_size,
                 as.numeric(effectsize::hedges_g(a, b, pooled_sd = TRUE)$Hedges_g),
                 tolerance = 1e-10)
    # Hodges-Lehmann is the median of pairwise differences, which is what wilcox.test estimates
    expect_equal(est("hodges_lehmann")$effect_size,
                 median(as.vector(outer(a, b, "-"))), tolerance = 1e-10)
    # Cliff's delta against its definition
    expect_equal(est("cliff_delta")$effect_size,
                 mean(outer(a, b, ">")) - mean(outer(a, b, "<")), tolerance = 1e-10)
})


test_that("every effect size reports a TWO-sided confidence interval", {
    # effectsize defaults to alternative = "greater" for variance-explained measures, which
    # returns a ONE-sided interval whose upper bound is always exactly 1. That was being
    # printed under columns headed "Effect CI Lower/Upper" next to Cohen's d, Hedges' g,
    # Cliff's delta and Hodges-Lehmann, all of which are two-sided -- so one pair of columns
    # silently mixed two conventions depending on which estimator the user picked.
    skip_if_not_installed("effectsize")
    set.seed(42); a <- rnorm(40, 10, 2); b <- rnorm(35, 11.5, 2.2)
    est <- function(ty) {
        o <- ClinicoPath:::jjridgesOptions$new(x_var = "v", y_var = "g", effsize_type = ty)
        cls <- ClinicoPath:::jjridgesClass$new(options = o,
                 data = data.frame(v = c(a, b), g = factor(rep(c("A", "B"), c(40, 35)))))
        cls$.__enclos_env__$private$.calculateEffectSizeWithCI(a, b)
    }
    for (ty in c("eta", "omega")) {
        ci <- est(ty)
        expect_lt(ci$ci_upper, 1)          # the one-sided artefact was exactly 1
        expect_lt(ci$ci_lower, ci$effect_size)
        expect_gt(ci$ci_upper, ci$effect_size)
    }
    # and they agree with effectsize asked for two.sided explicitly
    df <- data.frame(value = c(a, b), group = factor(rep(c("g1", "g2"), c(40, 35))))
    m <- aov(value ~ group, data = df)
    ref <- effectsize::eta_squared(m, ci = 0.95, alternative = "two.sided")
    expect_equal(est("eta")$ci_lower, ref$CI_low,  tolerance = 1e-8)
    expect_equal(est("eta")$ci_upper, ref$CI_high, tolerance = 1e-8)
})


test_that("p-value adjustment matches stats::p.adjust and 'none' is a no-op", {
    d <- five_groups()
    none <- rg_run(d, show_stats = TRUE, p_adjust_method = "none")$results$tests$asDF
    expect_equal(nrow(none), 10L)                      # 5 groups -> 5*4/2 pairwise tests
    expect_equal(none$p_adjusted, none$p_value)        # 'none' must not alter anything

    for (m in c("holm", "bonferroni", "fdr")) {
        got <- rg_run(d, show_stats = TRUE, p_adjust_method = m)$results$tests$asDF
        expect_equal(got$p_value, none$p_value, tolerance = 1e-12)   # same tests
        expect_equal(got$p_adjusted, p.adjust(none$p_value, method = m), tolerance = 1e-12)
    }
})


test_that("unadjusted multiple comparisons are flagged where the user can see it", {
    # The only mention of correction lived in the Statistical Assumptions panel, which is
    # showAssumptions: false by default, so ten unadjusted p-values came with no visible caveat.
    d <- five_groups()
    n <- rg_run(d, show_stats = TRUE, p_adjust_method = "none")$results$notices$content
    expect_match(n, "Unadjusted p-values")
    expect_match(n, "10 pairwise comparisons", fixed = TRUE)
    expect_match(n, "40%", fixed = TRUE)               # 1 - 0.95^10 = 0.401

    # silent once a correction is chosen, and silent when there is only one comparison
    adj <- rg_run(d, show_stats = TRUE, p_adjust_method = "holm")$results$notices$content
    expect_false(grepl("Unadjusted p-values", adj, fixed = TRUE))
    set.seed(3)
    two <- data.frame(v = rnorm(60, 10, 2), g = factor(rep(c("A", "B"), each = 30)))
    one_cmp <- rg_run(two, show_stats = TRUE, p_adjust_method = "none")$results$notices$content
    expect_false(grepl("Unadjusted p-values", one_cmp, fixed = TRUE))
})


test_that("the copy-ready report names the tests actually run, not the one requested", {
    # .performSingleTest switches a comparison to Wilcoxon whenever its normality or
    # equal-variance check fails. On five groups drawn from ONE N(10, 2), a single chance
    # Shapiro result (p = 0.031) switched 4 of the 10 comparisons -- while the copy-ready
    # paragraph still read "Method: Parametric". That text is offered for pasting into
    # manuscripts, so it must not misdescribe the analysis.
    a <- rg_run(five_groups(), show_stats = TRUE)
    methods <- a$results$tests$asDF$method
    expect_gt(length(unique(methods)), 1)               # the mix really does occur

    rep_txt <- a$results$reportSummary$content
    expect_match(rep_txt, "mixed", fixed = TRUE)
    expect_match(rep_txt, "t-test", fixed = TRUE)
    expect_match(rep_txt, "Wilcoxon", fixed = TRUE)
    expect_false(grepl("Method: Parametric</p>", rep_txt, fixed = TRUE))
    expect_match(rep_txt, "not every comparison used the same test")

    # the completion notice says so too
    expect_match(a$results$notices$content, "switched to Wilcoxon")

    # when no switch occurs the wording stays simple
    set.seed(5)
    clean <- data.frame(v = c(rnorm(40, 10, 1), rnorm(40, 10, 1)),
                        g = factor(rep(c("A", "B"), each = 40)))
    b <- rg_run(clean, show_stats = TRUE)
    if (length(unique(b$results$tests$asDF$method)) == 1)
        expect_false(grepl("mixed", b$results$reportSummary$content, fixed = TRUE))
})


test_that("pairwise p-values reproduce the underlying stats:: tests", {
    d <- five_groups()
    tab <- rg_run(d, show_stats = TRUE, p_adjust_method = "none")$results$tests$asDF
    for (i in seq_len(nrow(tab))) {
        parts <- strsplit(tab$comparison[i], " vs ", fixed = TRUE)[[1]]
        x1 <- d$v[d$g == parts[1]]; x2 <- d$v[d$g == parts[2]]
        ref <- if (grepl("Wilcoxon", tab$method[i], fixed = TRUE))
                   suppressWarnings(wilcox.test(x1, x2, conf.int = TRUE)$p.value)
               else t.test(x1, x2)$p.value              # Welch, as t.test() defaults
        expect_equal(tab$p_value[i], ref, tolerance = 1e-10, info = tab$comparison[i])
    }
})
