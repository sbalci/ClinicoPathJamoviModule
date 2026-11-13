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
    expect_match(t, "51 of 60 supplied rows analyzed")
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

test_that("incomplete longitudinal trajectories are disclosed in analysis notes", {
    d <- arc_long()
    # Remove one observation so one subject has an incomplete trajectory
    d <- d[-1, ]
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g", id_var = "id",
                             show_longitudinal = TRUE)
    expect_match(arc_txt(res$analysisNotes$content), "missing intermediate or follow-up")
})

test_that("population declaration is disclosed in analysis notes", {
    d <- arc_two()
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             population_type = "pp")
    expect_match(arc_txt(res$analysisNotes$content), "Per-Protocol")
})

test_that("clinical cutoff line rendering logic executes without error for 0 and non-zero", {
    d <- arc_two()
    res1 <- advancedraincloud(data = d, y_var = "y", x_var = "g", clinical_cutoff = 11.5)
    expect_s3_class(res1, "advancedraincloudResults")

    res0 <- advancedraincloud(data = d, y_var = "y", x_var = "g", clinical_cutoff = 0)
    expect_s3_class(res0, "advancedraincloudResults")
})


# ---- pre-release audit additions -------------------------------------------

test_that("ITT contradiction is surfaced in always-visible analysisNotes even when generate_report is FALSE", {
    d <- arc_two(); d$y[1:6] <- NA
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             generate_report = FALSE, population_type = "itt")
    notes <- arc_txt(res$analysisNotes$content)
    expect_match(notes, "6 of 60 supplied row")
    expect_match(notes, "Intention-to-Treat")
    expect_match(notes, "complete-case")
})

test_that("pooled outlier handling discloses modified/excluded counts and between-group attenuation caveat", {
    d <- arc_two()
    d$y[c(1, 60)] <- c(-25, 85)

    res_win <- advancedraincloud(data = d, y_var = "y", x_var = "g", outlier_method = "winsorize")
    expect_match(arc_txt(res_win$analysisNotes$content), "Winsorization clipped")
    expect_match(arc_txt(res_win$analysisNotes$content), "pooled sample")

    res_trim <- advancedraincloud(data = d, y_var = "y", x_var = "g", outlier_method = "trim")
    expect_match(arc_txt(res_trim$analysisNotes$content), "5th/95th percentile trimming")
    expect_match(arc_txt(res_trim$analysisNotes$content), "pooled sample")

    res_iqr <- advancedraincloud(data = d, y_var = "y", x_var = "g", outlier_method = "iqr")
    expect_match(arc_txt(res_iqr$analysisNotes$content), "1.5 x IQR filtering")
    expect_match(arc_txt(res_iqr$analysisNotes$content), "pooled sample")
})

test_that("small-sample groups emit explicit rank-test discreteness and normal-z CI warnings", {
    d_tiny <- data.frame(
        y = c(10, 11, 12, 20, 21, 22),
        g = factor(rep(c("A", "B"), each = 3))
    )
    res_tiny <- advancedraincloud(data = d_tiny, y_var = "y", x_var = "g",
                                  show_comparisons = TRUE, show_effect_size = TRUE)
    notes_tiny <- arc_txt(res_tiny$analysisNotes$content)
    expect_match(notes_tiny, "both groups have n (&lt;|<)= 3")
    expect_match(notes_tiny, "0\\.10")

    d_small <- data.frame(
        y = c(10, 11, 12, 13, 20, 21, 22, 23, 24, 25),
        g = factor(c(rep("A", 4), rep("B", 6)))
    )
    res_small <- advancedraincloud(data = d_small, y_var = "y", x_var = "g",
                                   show_effect_size = TRUE)
    expect_match(arc_txt(res_small$analysisNotes$content), "minimum group n = 4 (&lt;|<) 5")
})

test_that("negative clinical thresholds are rejected when log_transform is enabled", {
    d <- arc_two(); d$y <- abs(d$y) + 1
    expect_error(
        advancedraincloud(data = d, y_var = "y", x_var = "g",
                          log_transform = TRUE, show_clinical_cutoff = TRUE,
                          clinical_cutoff = -2),
        "non-negative"
    )
    expect_error(
        advancedraincloud(data = d, y_var = "y", x_var = "g",
                          log_transform = TRUE, show_reference_range = TRUE,
                          reference_range_min = -1, reference_range_max = 5),
        "non-negative"
    )
})

test_that("CV bands on non-positive group means emit a ratio-scale warning", {
    d <- data.frame(
        y = c(-6, -5, -4, -3, 4, 5, 6, 7),
        g = factor(rep(c("NegativeMean", "PositiveMean"), each = 4))
    )
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g", show_cv_bands = TRUE)
    expect_match(arc_txt(res$analysisNotes$content), "strictly positive ratio-scale")
})

test_that("effect_sizes avoids duplicate Cohen (1988) footnote and comparisons omits orphan star legend", {
    d <- arc_two()
    res <- advancedraincloud(data = d, y_var = "y", x_var = "g",
                             show_comparisons = TRUE, show_effect_size = TRUE,
                             effect_size_type = "cohens_d", include_methods = TRUE,
                             outlier_method = "winsorize", log_transform = TRUE)
    es_txt <- arc_txt(res$effect_sizes$content)
    expect_equal(length(gregexpr("Cohen \\(1988\\)", es_txt)[[1]]), 1L)
    expect_match(es_txt, "Cohen's d")
    expect_false(grepl("cohens_d", es_txt, fixed = TRUE))

    comp_txt <- arc_txt(res$comparisons$content)
    expect_false(grepl("\\*\\*\\* p < 0.001", comp_txt))
    expect_match(comp_txt, "without significance stars")

    m_txt <- arc_txt(res$methods_text$content)
    expect_match(m_txt, "log\\(y \\+ 0.01\\)")
    expect_match(m_txt, "5th/95th percentile Winsorization")
    expect_match(m_txt, "Wilcoxon rank-sum")
    expect_match(m_txt, "Cohen's d")
    expect_false(grepl("cohens_d", m_txt, fixed = TRUE))
})

test_that("asSource escapes adversarial option strings and drops list/vector/undeclared options", {
    f_names <- setdiff(names(formals(advancedraincloud)), "data")
    payloads <- c(
        'foo", system("id"), x = "',
        "trail\\",
        'multi\nline"quote'
    )

    for (opt_name in f_names) {
        for (pl in payloads) {
            opts <- advancedraincloudOptions$new()
            o <- opts$option(opt_name)
            o$value <- pl
            an <- advancedraincloudClass$new(options = opts, data = data.frame())
            parsed <- parse(text = an$asSource())
            expect_length(parsed, 1L)
            mc <- match.call(advancedraincloud, parsed[[1]])
            arg_names <- setdiff(names(as.list(mc))[-1], "data")
            expect_identical(arg_names, opt_name)
            expect_identical(mc[[opt_name]], pl)
        }

        opts_l <- advancedraincloudOptions$new()
        o_l <- opts_l$option(opt_name)
        o_l$value <- list(a = quote(stop("RCE")))
        an_l <- advancedraincloudClass$new(options = opts_l, data = data.frame())
        mc_l <- match.call(advancedraincloud, parse(text = an_l$asSource())[[1]])
        expect_length(setdiff(names(as.list(mc_l))[-1], "data"), 0L)

        opts_v <- advancedraincloudOptions$new()
        o_v <- opts_v$option(opt_name)
        o_v$value <- c("a", 'b", stop("RCE")')
        an_v <- advancedraincloudClass$new(options = opts_v, data = data.frame())
        mc_v <- match.call(advancedraincloud, parse(text = an_v$asSource())[[1]])
        expect_length(setdiff(names(as.list(mc_v))[-1], "data"), 0L)
    }

    opts_u <- advancedraincloudOptions$new()
    evil_opt <- jmvcore::OptionString$new("injected_arg", value = 'foo", stop("RCE"), bar = "')
    opts_u$.addOption(evil_opt)
    an_u <- advancedraincloudClass$new(options = opts_u, data = data.frame())
    mc_u <- match.call(advancedraincloud, parse(text = an_u$asSource())[[1]])
    expect_false("injected_arg" %in% names(as.list(mc_u)))
})

test_that("adversarial column names, factor levels, and tampered enum fallbacks are HTML-escaped and formula-safe", {
    df_adv <- data.frame(
        check.names = FALSE,
        "<img src=x onerror=alert(1)>" = c(1.2, 2.4, 3.1, 4.5, 5.2, 6.1, 2.0, 3.8, 4.9),
        'stop(paste(collapse=", ", system("id", intern=TRUE)))' = factor(
            rep(c("<script>alert(1)</script>", 'weird"level\\name', "G3 & <b onmouseover=x>"), each = 3)
        ),
        "id<b>1</b>" = factor(rep(c("P<1>", 'P"2"', "P&3"), times = 3))
    )

    res_adv <- advancedraincloud(
        data = df_adv,
        y_var = "<img src=x onerror=alert(1)>",
        x_var = 'stop(paste(collapse=", ", system("id", intern=TRUE)))',
        id_var = "id<b>1</b>",
        show_statistics = TRUE,
        show_comparisons = TRUE,
        show_interpretation = TRUE,
        show_effect_size = TRUE,
        show_change_scores = TRUE,
        baseline_group = "<script>alert(1)</script>",
        generate_report = TRUE,
        show_missing_info = TRUE
    )

    for (nm in c("statistics", "comparisons", "interpretation", "effect_sizes",
                 "change_analysis", "clinical_report", "analysisNotes")) {
        expect_false(
            grepl("<(script|img|svg|b\\b)", res_adv[[nm]]$content),
            info = nm
        )
    }

    opts_fb <- advancedraincloudOptions$new(
        y_var = "<img src=x onerror=alert(1)>",
        x_var = 'stop(paste(collapse=", ", system("id", intern=TRUE)))',
        show_interpretation = TRUE,
        include_methods = TRUE,
        show_effect_size = TRUE
    )
    an_fb <- advancedraincloudClass$new(options = opts_fb, data = df_adv)
    an_fb$init()
    o_rs <- opts_fb$option("rain_side"); o_rs$value <- "<svg onload=alert(1)>"
    o_om <- opts_fb$option("outlier_method"); o_om$value <- "<img src=x onerror=alert(2)>"
    o_es <- opts_fb$option("effect_size_type"); o_es$value <- "<script>alert(3)</script>"
    an_fb$.__enclos_env__$private$.run()
    combined_fb <- paste(an_fb$results$interpretation$content, an_fb$results$methods_text$content)
    expect_false(grepl("<(script|img|svg)", combined_fb))
    expect_match(combined_fb, "&lt;svg onload=alert\\(1\\)&gt;")
    expect_match(combined_fb, "&lt;img src=x onerror=alert\\(2\\)&gt;")
    expect_match(combined_fb, "&lt;script&gt;alert\\(3\\)&lt;/script&gt;")
})

test_that("non-finite or non-scalar numeric options reject cleanly and early return clears cached state", {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6), g = factor(rep(c("A", "B"), each = 3)))
    for (bad_val in list(NA_real_, NaN, Inf, numeric(0), c(1, 2))) {
        opts_na <- advancedraincloudOptions$new(y_var = "y", x_var = "g")
        o_ps <- opts_na$option("point_size")
        o_ps$value <- bad_val
        an_na <- advancedraincloudClass$new(options = opts_na, data = d)
        expect_error(an_na$.__enclos_env__$private$.run(), "Point size must be a numeric value")
    }

    opts_run <- advancedraincloudOptions$new(y_var = "y", x_var = "g")
    an_run <- advancedraincloudClass$new(options = opts_run, data = d)
    an_run$run()
    expect_false(is.null(an_run$.__enclos_env__$private$.analysis_data))

    o_xv <- opts_run$option("x_var")
    o_xv$value <- NULL
    an_run$.__enclos_env__$private$.run()
    expect_null(an_run$.__enclos_env__$private$.analysis_data)
    expect_null(an_run$results$plot$state)
})

test_that("duplicate variable roles reject cleanly and plot/effect-size edge cases preserve state and clear labels", {
    d <- data.frame(
        y = c(10, 12, 14, 20, 22, 24),
        x_num = c(1, 1, 1, 2, 2, 2),
        id = c(1, 2, 3, 1, 2, NA)
    )
    expect_error(
        advancedraincloud(data = d, y_var = "y", x_var = "y"),
        "distinct from the grouping"
    )
    expect_error(
        advancedraincloud(data = d, y_var = "y", x_var = "x_num", fill_var = "y"),
        "distinct from the grouping"
    )
    expect_error(
        advancedraincloud(data = d, y_var = "y", x_var = "x_num", id_var = "x_num"),
        "longitudinal ID variable must be distinct"
    )

    # Singleton group (n = 1) reports sample-size requirement rather than zero variance
    d_n1 <- data.frame(
        y = c(10, 20, 22, 24, 26),
        g = factor(c("Singleton", rep("Reference", 4)), levels = c("Singleton", "Reference"))
    )
    res_n1 <- advancedraincloud(data = d_n1, y_var = "y", x_var = "g", show_effect_size = TRUE)
    expect_match(arc_txt(res_n1$effect_sizes$content), "at least 2 observations")
    expect_false(grepl("no variation", arc_txt(res_n1$effect_sizes$content), fixed = TRUE))

    # Longitudinal plot with NA IDs filters before ggplot() and preserves comparison attribute on cached state
    d_long_na <- data.frame(
        y = c(10, 12, 14, 20, 22, 24, 30),
        g = factor(c("T1", "T1", "T1", "T2", "T2", "T2", "T2"), levels = c("T1", "T2")),
        subj = factor(c("S1", "S2", "S3", "S1", "S2", "S3", NA))
    )
    opts_ln <- advancedraincloudOptions$new(
        y_var = "y", x_var = "g", id_var = "subj",
        show_longitudinal = FALSE, show_comparisons = TRUE
    )
    an_ln <- advancedraincloudClass$new(options = opts_ln, data = d_long_na)
    an_ln$run()
    # Now enable longitudinal connections on state-only redraw where one row has NA subj
    o_sl <- opts_ln$option("show_longitudinal")
    o_sl$value <- TRUE
    f_pdf <- tempfile(fileext = ".pdf")
    grDevices::pdf(f_pdf, width = 8, height = 6)
    on.exit({
        try(grDevices::dev.off(), silent = TRUE)
        unlink(f_pdf)
    }, add = TRUE)
    ok_ln <- suppressWarnings(an_ln$.__enclos_env__$private$.plot(
        an_ln$results$plot, ggtheme = ggplot2::theme_classic(), theme = NULL
    ))
    p_last <- ggplot2::last_plot()
    expect_true(ok_ln)
    expect_equal(nrow(p_last$data), 6L)

    # Zero-variance 3-group tied Kruskal-Wallis comparison (p = NaN) renders 'p: not estimable' rather than 'p = NA'
    d_const <- data.frame(
        y = rep(5, 9),
        g = factor(rep(c("A", "B", "C"), each = 3))
    )
    res_const <- suppressMessages(suppressWarnings(advancedraincloud(
        data = d_const, y_var = "y", x_var = "g",
        show_comparisons = TRUE, p_value_position = "legend"
    )))
    suppressWarnings(print(res_const$plot))
    p_const <- ggplot2::last_plot()
    expect_match(p_const$labels$caption, "p: not estimable")
    expect_false(grepl("p = NA", p_const$labels$caption, fixed = TRUE))
})




