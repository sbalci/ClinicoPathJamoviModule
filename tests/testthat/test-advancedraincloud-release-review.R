# Release review of advancedraincloud.
#
# This analysis carries clinical-trial vocabulary - MCID, responder rate,
# intention-to-treat, effect sizes - so most of the risk is a number or a label
# that means something different from what it says. Every block below asserts
# what the user is shown.

arc_two <- function(seed = 4) {
    set.seed(seed)
    data.frame(y = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
               g = factor(rep(c("Control", "Treated"), each = 30)))
}
arc_long <- function(seed = 6) {
    set.seed(seed)
    data.frame(y  = c(rnorm(25, 50, 5), rnorm(25, 60, 5)),
               g  = factor(rep(c("Baseline", "Week12"), each = 25),
                           levels = c("Baseline", "Week12")),
               id = factor(rep(1:25, 2)))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
arc_txt <- function(x) gsub("\\s+", " ", gsub("<[^>]*>", " ", x %||% ""))
# table cells, so a numeric result can be compared instead of regex-hunting prose
arc_cells <- function(html) {
    m <- regmatches(html, gregexpr("(?<=>)[^<>]*(?=</td>)", html, perl = TRUE))[[1]]
    trimws(m[nzchar(trimws(m))])
}


# ---- effect sizes ----------------------------------------------------------

test_that("Cohen's d and Hedges' g match the published formulas", {
    d <- arc_two()
    x <- d$y[d$g == "Control"]; y <- d$y[d$g == "Treated"]
    n1 <- length(x); n2 <- length(y)
    sp <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
    dd <- (mean(x) - mean(y)) / sp
    gg <- dd * (1 - 3 / (4 * (n1 + n2 - 2) - 1))     # Hedges' small-sample J

    for (ty in c("cohens_d", "hedges_g")) {
        res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                                 show_effect_size = TRUE, effect_size_type = ty)
        cells <- arc_cells(res$effect_sizes$content)
        expect_true(any(cells == sprintf("%.3f", round(if (ty == "cohens_d") dd else gg, 3))),
                    info = paste(ty, "->", paste(cells, collapse = " | ")))
    }
})

test_that("Glass's delta confidence interval uses the Hedges & Olkin standard error", {
    # The SE was sqrt(n1/(n1*n2) + d^2/(2*n2)). n1/(n1*n2) simplifies to 1/n2, so
    # the treatment group's sampling variance was dropped entirely and the second
    # denominator used 2*n2 instead of 2*(n2-1). Measured at n1=n2=20, delta=0.8:
    # SE 0.2569 against the correct 0.3418 - an interval only 75% as wide.
    d <- arc_two()
    x <- d$y[d$g == "Control"]; y <- d$y[d$g == "Treated"]
    n1 <- length(x); n2 <- length(y)
    delta <- (mean(x) - mean(y)) / sd(y)
    se <- sqrt(var(x) / (n1 * var(y)) + 1 / n2 + delta^2 / (2 * (n2 - 1)))

    res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             show_effect_size = TRUE, effect_size_type = "glass_delta")
    cells <- arc_cells(res$effect_sizes$content)
    expect_true(any(cells == sprintf("%.3f", round(delta, 3))), info = paste(cells, collapse = " | "))
    expect_true(any(cells == sprintf("[%s, %s]", round(delta - 1.96 * se, 3),
                                     round(delta + 1.96 * se, 3))),
                info = paste(cells, collapse = " | "))
})

test_that("effect size magnitude bands ignore the sign", {
    # Cohen's bands describe magnitude; comparing a SIGNED value against
    # 0.2/0.5/0.8 makes every negative effect "Negligible".
    f <- ClinicoPath:::advancedraincloudClass$private_methods$.interpret_effect_size
    env <- new.env(); environment(f) <- env
    for (v in c(0.9, -0.9, 1.5, -1.5)) expect_equal(f(v), "Large", info = as.character(v))
    for (v in c(0.3, -0.3)) expect_equal(f(v), "Small", info = as.character(v))
    expect_equal(f(0.05), "Negligible")
    expect_equal(f(NA_real_), "Not estimable")
})


# ---- claims the analysis cannot support ------------------------------------

test_that("the change panel reports direction, not benefit", {
    # "Improved" for any increase is wrong for tumour size, pain scores, LDL and
    # most biomarkers, where an increase is the patient getting worse.
    res <- advancedraincloud(data = arc_long(), y_var = "y", x_var = "g", id_var = "id",
                             show_change_scores = TRUE, baseline_group = "Baseline",
                             responder_threshold = 20)
    t <- arc_txt(res$change_analysis$content)
    expect_match(t, "Increased from baseline")
    expect_match(t, "Decreased from baseline")
    expect_false(grepl("Improved:", t, fixed = TRUE))
    expect_false(grepl("Declined:", t, fixed = TRUE))
})

test_that("an ITT label on a complete-case analysis is contradicted out loud", {
    d <- arc_two(); d$y[1:9] <- NA
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             generate_report = TRUE, population_type = "itt")
    t <- arc_txt(res$clinical_report$content)
    expect_match(t, "as declared")
    expect_match(t, "51 of 60 supplied rows analysed")
    expect_match(t, "were excluded before analysis")
    expect_match(t, "COMPLETE-CASE")
})

test_that("with no rows dropped the ITT contradiction is not raised", {
    res <- advancedraincloud(data = arc_two(), y_var = "y", x_var = "g",
                             generate_report = TRUE, population_type = "itt")
    expect_false(grepl("COMPLETE-CASE", arc_txt(res$clinical_report$content), fixed = TRUE))
})

test_that("log-transformed statistics are labelled as log scale", {
    # The transform is applied before every statistic, so the table numbers are
    # in log units. The plot axis said so; the table did not, and the Methods
    # panel that mentions it is off by default.
    d <- arc_two(); d$y <- abs(d$y) + 1
    on  <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             show_statistics = TRUE, log_transform = TRUE)
    off <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             show_statistics = TRUE, log_transform = FALSE)
    expect_match(arc_txt(on$statistics$content), "log scale")
    expect_false(grepl("log scale", arc_txt(off$statistics$content), fixed = TRUE))
})


# ---- data handling ---------------------------------------------------------

test_that("non-finite values are excluded and disclosed", {
    d <- arc_two(); d$y[1] <- Inf
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g")
    expect_match(arc_txt(res$analysisNotes$content), "non-finite")
})

test_that("a log transform on non-positive data is rejected, not silently NaN'd", {
    d <- arc_two(); d$y[1] <- -5
    expect_error(advancedraincloud(data = d, y_var = "y", x_var = "g", log_transform = TRUE),
                 "non-positive")
})

test_that("group comparisons stay non-parametric and disclose the omnibus limit", {
    d3 <- rbind(arc_two(),
                data.frame(y = rnorm(30, 14, 2), g = factor(rep("Third", 30))))
    d3$g <- factor(d3$g)
    res <- advancedraincloud(data = d3, y_var = "y", x_var = "g", show_comparisons = TRUE)
    t <- arc_txt(res$comparisons$content)
    expect_match(t, "Kruskal-Wallis")
    expect_match(t, "omnibus test only")
    expect_match(t, "multiplicity")

    kw <- stats::kruskal.test(y ~ g, data = d3)
    expect_true(any(arc_cells(res$comparisons$content) ==
                    paste0("χ² = ", round(unname(kw$statistic), 4),
                           ", df = ", unname(kw$parameter))))
})

test_that("two groups use Wilcoxon and match stats::wilcox.test", {
    d <- arc_two()
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g", show_comparisons = TRUE)
    w <- suppressWarnings(stats::wilcox.test(d$y[d$g == "Control"], d$y[d$g == "Treated"]))
    expect_match(arc_txt(res$comparisons$content), "Wilcoxon")
    expect_true(any(arc_cells(res$comparisons$content) ==
                    paste0("W = ", round(unname(w$statistic), 4))))
})
