# Module-wide numerical and data-integrity regressions (September 2026).
mr_backend <- function(name, data, ...) {
    ns <- asNamespace("ClinicoPath")
    options <- do.call(get(paste0(name, "Options"), ns)$new, list(...))
    a <- get(paste0(name, "Class"), ns)$new(options = options, data = data)
    a$init()
    a
}
mr_private <- function(a) a$.__enclos_env__$private

test_that("modified Z scores use the unscaled MAD once", {
    d <- data.frame(x = c(0:5, 14), g = factor(rep("A", 7)))
    a <- mr_backend("raincloud", d, dep_var = "x", group_var = "g", outlier_method = "modified_zscore")
    raw_mad <- median(abs(d$x - median(d$x)))
    expect_equal(sum(abs(0.6745 * (d$x - median(d$x)) / raw_mad) > 3.5), 1L)
    html <- mr_private(a)$.generate_outlier_analysis(d, "x", "g")
    expect_match(html, "1 outliers detected", fixed = TRUE)
})

test_that("wide repeated data ignore unrelated missing columns and preserve reserved names", {
    set.seed(84)
    d <- data.frame(rowid = rnorm(12), post = rnorm(12), unrelated = NA_real_)
    a <- mr_backend("jjwithinstats", d, dep1 = "rowid", dep2 = "post")
    z <- mr_private(a)$.prepareData()
    expect_equal(nrow(z), 24L)
    expect_equal(z$value[z$measurement == "rowid"], d$rowid)
    expect_equal(as.integer(table(z$rowid)), rep(2L, 12))
})

test_that("automatic repeated plots pair by ID and sample whole subjects", {
    set.seed(76)
    d <- expand.grid(id = seq_len(550), g = c("Before", "After"))
    d$y <- rep(rnorm(550), 2) + rnorm(1100)
    d <- d[sample.int(nrow(d)), ]
    info <- list(dep_var = "y", group_var = "g", grvar = NULL,
        plot_type = "repeated_factor_continuous", distribution = "parametric", alluvsty = "t1")
    a <- mr_backend("statsplot2", d, dep = "y", group = "g", direction = "repeated",
        subjectID = "id", sampleLarge = TRUE, sampleThreshold = 1000L, sampleSize = 100L)
    set.seed(943); old_seed <- .Random.seed
    z <- mr_private(a)$.prepareDataForPlot(info)
    expect_identical(.Random.seed, old_seed)
    expect_equal(nrow(z$data), 100L)
    expect_true(all(table(z$data$id) == 2L))
    expect_equal(z$subject_id, "id")
    plot <- mr_private(a)$.plotWithinStats(z)
    before <- z$data[z$data$g == "Before", ]; before <- before[order(before$id), ]
    after <- z$data[z$data$g == "After", ]; after <- after[order(after$id), ]
    oracle <- stats::t.test(before$y, after$y, paired = TRUE)
    expect_match(paste(deparse(plot$labels$subtitle), collapse = ""),
        sprintf("%.2f", unname(oracle$statistic)), fixed = TRUE)

    b <- mr_backend("statsplot2", d, dep = "y", group = "g", direction = "repeated")
    expect_error(mr_private(b)$.prepareDataForPlot(info), "Subject ID")
    dup <- rbind(d, d[1, ])
    b <- mr_backend("statsplot2", dup, dep = "y", group = "g", direction = "repeated", subjectID = "id")
    expect_error(mr_private(b)$.prepareDataForPlot(info), "duplicate")
})

test_that("change response excludes undefined percentages and preserves threshold sign", {
    d <- data.frame(id = rep(1:3, 2), g = factor(rep(c("Pre", "Post"), each = 3),
        levels = c("Pre", "Post")), y = c(0, 100, 100, 20, 50, 90))
    a <- mr_backend("advancedraincloud", d, y_var = "y", x_var = "g", id_var = "id")
    p <- mr_private(a)
    z <- p$.generate_change_analysis(d, "y", "g", "id", "Pre", -20)
    expect_equal(z$summary$n_total, 3L)
    expect_equal(z$summary$n_evaluable, 2L)
    expect_equal(z$summary$n_responders, 1L)
    expect_match(z$html, "<=-20", fixed = TRUE)
    expect_match(z$html, "1 / 2 evaluable (50%)", fixed = TRUE)
    expect_null(p$.generate_change_analysis(rbind(d, d[1, ]), "y", "g", "id", "Pre", -20)$summary)
    expect_null(p$.generate_comparisons(d, "y", "g", NULL)$stats)
    expect_match(p$.generate_effect_sizes(d, "y", "g", "cohens_d"), "paired effect-size")
})

test_that("Glass interval includes the observed variance ratio", {
    x <- c(0, 3, 6, 9, 12); y <- c(2, 3, 4, 5, 6)
    a <- mr_backend("advancedraincloud", data.frame(y = c(x, y), g = rep(1:2, each = 5)),
        y_var = "y", x_var = "g")
    z <- mr_private(a)$.calculate_effect_size(x, y, "glass_delta")
    delta <- (mean(x) - mean(y)) / sd(y)
    variance <- var(x) / (length(x) * var(y)) + 1 / length(y) + delta^2 / (2 * (length(y) - 1))
    expect_equal(z$ci_lower, delta - qnorm(.975) * sqrt(variance))
    expect_equal(z$ci_upper, delta + qnorm(.975) * sqrt(variance))
})

test_that("lollipop sorting and sample information survive aggregation", {
    d <- data.frame(y = 1:12, g = factor(rep(c("Z", "A"), each = 6), levels = c("Z", "A")))
    a <- mr_backend("lollipop", d, dep = "y", group = "g", highlight = NULL,
        aggregation = "mean", sortBy = "group_alpha")
    p <- mr_private(a); z <- p$.cleanData()
    expect_equal(levels(z$group), c("A", "Z"))
    expect_equal(z$dependent, c(9.5, 3.5))
    expect_equal(attr(z, "source_n"), 12L)
    s <- p$.calculateSummary(z)
    expect_match(p$.generateClinicalSummary(s, "y", "g"), "cannot be inferred")
    d$y[d$g == "A"] <- NA_real_
    a <- mr_backend("lollipop", d, dep = "y", group = "g", highlight = NULL, aggregation = "mean")
    expect_error(mr_private(a)$.cleanData(), "2 groups")
})

test_that("density uses distinct edges even when multiple edges are retained", {
    d <- data.frame(s = c("A", "A", "B"), t = c("B", "B", "C"))
    a <- mr_backend("jjarcdiagram", d, source = "s", target = "t", aggregateEdges = FALSE)
    z <- mr_private(a)$.prepareNetworkData()
    expect_equal(z$density, 2 / 3)
    expect_equal(z$n_edges, 3L)
})

test_that("segmented counts cannot become valid by summing fractional records", {
    d <- data.frame(x = factor(c("A", "A", "B", "B")),
        g = factor(c("C", "C", "D", "D")), y = rep(.5, 4))
    a <- mr_backend("jjsegmentedtotalbar", d, x_var = "x", y_var = "y", fill_var = "g", y_is_count = TRUE)
    p <- mr_private(a); p$.processData(d, "x", "y", "g", NULL)
    expect_null(p$.processed_data)
    expect_match(paste(vapply(p$.noticeList, function(x) x$content, character(1)), collapse = " "), "whole numbers")
})

test_that("histograms and correlation matrices treat infinity as unavailable", {
    d <- data.frame(x = c(1:8, Inf), y = c(8:1, 9))
    a <- mr_backend("jjhistostats", d[1:8, ], dep = "x")
    f <- mr_private(a)$.prepareData
    environment(f) <- list2env(list(self = list(data = d, options = a$options)), parent = environment(f))
    expect_true(is.na(f()$x[9]))
    b <- mr_backend("jjcorrmat", d[1:8, ], dep = c("x", "y"), naHandling = "listwise")
    f <- mr_private(b)$.prepareData
    environment(f) <- list2env(list(self = list(data = d, options = b$options)), parent = environment(f))
    expect_equal(nrow(f()), 8L)
})

test_that("fractional waffle weights are not rounded into fake counts", {
    d <- data.frame(g = c("A", "B"), w = c(.25, .75))
    a <- mr_backend("jwaffle", d, groups = "g", counts = "w")
    p <- mr_private(a)
    z <- p$.aggregateData(d, "g", counts_var = "w")
    expect_equal(sum(z$count), 1)
    expect_match(p$.generateSummary(z, "g", total_cases = 1, is_weighted = TRUE), "n=0.25", fixed = TRUE)
})

test_that("exchanging continuous and categorical roles preserves the group comparison", {
    set.seed(313)
    d <- data.frame(g = factor(rep(c("A", "B"), each = 30)), x = c(rnorm(30, 10), rnorm(30, 11)))
    a <- mr_backend("statsplot2", d, dep = "g", group = "x")
    z <- list(data = d, dep = "g", group = "x", distribution = "parametric")
    p <- mr_private(a)$.plotDotplotStats(z)
    oracle <- stats::t.test(d$x[d$g == "A"], d$x[d$g == "B"])
    expect_match(paste(deparse(p$labels$subtitle), collapse = ""),
        sprintf("%.2f", unname(oracle$statistic)), fixed = TRUE)
})

test_that("sampling and subject changes invalidate cached automatic plots", {
    for (changed in c("sampleSize", "sampleThreshold", "subjectID")) {
        opts <- ClinicoPath:::statsplot2Options$new(dep = "y", group = "g")
        fresh <- ClinicoPath:::statsplot2Results$new(opts)
        fresh$plot$fromProtoBuf(list(state = raw(), image = list(path = "previous.png")),
            oChanges = changed, vChanges = character())
        expect_null(fresh$plot$.__enclos_env__$private$.filePath, info = changed)
    }
})

test_that("bar reports name the sparse-table test", {
    d <- data.frame(y = factor(rep(c("Yes", "No", "Yes", "No"), c(1, 3, 5, 1))),
        g = factor(rep(c("A", "A", "B", "B"), c(1, 3, 5, 1))))
    a <- mr_backend("jjbarstats", d, dep = "y", group = "g", resultssubtitle = TRUE)
    p <- mr_private(a)
    expect_match(p$.methodDescription(d), "Fisher")
    d$w <- rep(.5, nrow(d))
    b <- mr_backend("jjbarstats", d, dep = "y", group = "g", counts = "w")
    expect_error(mr_private(b)$.validateVariables(), "whole numbers")
})

test_that("pie narrative follows paired and preset methods", {
    d <- data.frame(y = factor(rep(c("Yes", "No"), each = 10)),
        g = factor(rep(c("Yes", "No"), 10)))
    a <- mr_backend("jjpiestats", d, dep = "y", group = "g", paired = TRUE)
    expect_match(mr_private(a)$.generateAssumptionsContent(), "Pairs must be independent")
    expect_match(mr_private(a)$.generateReportContent(), "McNemar")
    b <- mr_backend("jjpiestats", d, dep = "y", group = "g", clinicalpreset = "treatment", typestatistics = "bayes")
    expect_false(grepl("Bayesian", mr_private(b)$.generateReportContent()))
})

test_that("hull separation does not depend on the units chosen for one axis", {
    d <- data.frame(x = rep(seq(-1, 1, length.out = 20), 2),
        y = c(seq(-1, 1, length.out = 20), seq(3, 5, length.out = 20)),
        g = factor(rep(c("A", "B"), each = 20)))
    a <- mr_backend("hullplot", d, x_var = "x", y_var = "y", group_var = "g")
    p <- mr_private(a)
    descriptor <- function(x) regmatches(x, regexpr("described as [^;]+", x))
    h1 <- p$.generate_natural_summary(d, "x", "y", "g")
    d$x <- d$x * 10000
    h2 <- p$.generate_natural_summary(d, "x", "y", "g")
    expect_identical(descriptor(h1), descriptor(h2))
    expect_match(h2, "does not establish biological differences")
})
