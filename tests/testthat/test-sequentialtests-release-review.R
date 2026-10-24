# Regression tests from the `sequentialtests` release review.
#
# Each case corresponds to a defect confirmed in the shipped code, or to a property that was
# verified independently and must not silently change. The accuracy figures are checked against a
# simulation of each strategy's actual decision rule, not against the module's own algebra.

run_st <- function(...) {
    args <- utils::modifyList(
        list(test1_sens = 0.95, test1_spec = 0.70,
             test2_sens = 0.80, test2_spec = 0.98,
             prevalence = 0.10, strategy = "serial_positive"),
        list(...))
    do.call(ClinicoPath::sequentialtests, args)
}

summary_of <- function(res) res$summary_table$asDF

table_notes <- function(res) {
    n <- res$summary_table$.__enclos_env__$private$.notes
    stats::setNames(vapply(n, function(e) get("note", envir = e), character(1)), names(n))
}

private_st <- function(...) {
    o <- ClinicoPath:::sequentialtestsOptions$new(...)
    a <- ClinicoPath:::sequentialtestsClass$new(options = o, data = data.frame(x = 1))
    a$init()
    list(a = a, o = o, p = a$.__enclos_env__$private)
}


test_that("each strategy matches a simulation of its own decision rule", {
    # serial positive  = positive iff BOTH tests positive
    # serial negative  = T1 positive, or T1 negative and the retest positive
    # parallel         = positive iff EITHER test positive
    skip_on_cran()
    s1 <- 0.95; p1 <- 0.70; s2 <- 0.80; p2 <- 0.98; prev <- 0.10

    set.seed(42)
    N <- 3e5
    D  <- rbinom(N, 1, prev)
    T1 <- ifelse(D == 1, rbinom(N, 1, s1), rbinom(N, 1, 1 - p1))
    T2 <- ifelse(D == 1, rbinom(N, 1, s2), rbinom(N, 1, 1 - p2))

    rules <- list(
        serial_positive = (T1 == 1) & (T2 == 1),
        serial_negative = (T1 == 1) | ((T1 == 0) & (T2 == 1)),
        parallel        = (T1 == 1) | (T2 == 1))

    for (nm in names(rules)) {
        P <- rules[[nm]]
        d <- summary_of(run_st(strategy = nm))
        expect_equal(d$combined_sens[1], mean(P[D == 1]), tolerance = 0.01, label = paste(nm, "sens"))
        expect_equal(d$combined_spec[1], mean(!P[D == 0]), tolerance = 0.01, label = paste(nm, "spec"))
    }

    # and the exact closed forms
    d <- summary_of(run_st(strategy = "serial_positive"))
    expect_equal(d$combined_sens[1], s1 * s2, tolerance = 1e-12)
    expect_equal(d$combined_spec[1], p1 + (1 - p1) * p2, tolerance = 1e-12)
})


test_that("combined PPV and NPV are Bayes on the combined operating point", {
    s1 <- 0.95; p1 <- 0.70; s2 <- 0.80; p2 <- 0.98; prev <- 0.10
    cs <- s1 * s2
    cp <- p1 + (1 - p1) * p2
    d <- summary_of(run_st(strategy = "serial_positive"))

    expect_equal(d$combined_ppv[1], (prev * cs) / (prev * cs + (1 - prev) * (1 - cp)), tolerance = 1e-12)
    expect_equal(d$combined_npv[1], ((1 - prev) * cp) / ((1 - prev) * cp + prev * (1 - cs)), tolerance = 1e-12)
    expect_equal(d$combined_ppv[1], 0.933660934, tolerance = 1e-7)
    expect_equal(d$combined_npv[1], 0.973873285, tolerance = 1e-7)
})


test_that("serial-negative and parallel are identical, and the output says so", {
    # sens1 + (1 - sens1) * sens2 is the same number as sens1 + sens2 - sens1 * sens2: both rules
    # call a subject positive if EITHER test is positive. They differ only in how many second
    # tests are performed. Nothing in the output mentioned this, so a user comparing the two saw
    # byte-identical rows with no explanation.
    a <- summary_of(run_st(strategy = "serial_negative"))
    b <- summary_of(run_st(strategy = "parallel"))
    for (col in c("combined_sens", "combined_spec", "combined_ppv", "combined_npv"))
        expect_equal(a[[col]][1], b[[col]][1], tolerance = 1e-12, label = col)

    expect_match(table_notes(run_st(strategy = "serial_negative"))[["equivalence_note"]],
                 "same sensitivity, specificity, PPV and NPV")
    expect_match(table_notes(run_st(strategy = "parallel"))[["equivalence_note"]],
                 "same sensitivity, specificity, PPV and NPV")
    # not claimed for serial-positive, where it is false
    expect_false("equivalence_note" %in% names(table_notes(run_st(strategy = "serial_positive"))))
})


test_that("the only real difference between them is the number of second tests", {
    n2 <- function(st) {
        d <- run_st(strategy = st, show_cost_analysis = TRUE, population_size = 1000,
                    test1_cost = 1, test2_cost = 1)$cost_analysis_table$asDF
        d$number_tests[d$item == "Test 2: Confirmatory Test"]
    }
    # parallel tests everyone; serial-negative tests only the first-test negatives
    expect_equal(n2("parallel"), 1000L)
    expect_lt(n2("serial_negative"), 1000L)
    expect_lt(n2("serial_positive"), n2("serial_negative"))
})


test_that("the independence assumption is stated for every strategy, in matching words", {
    # The note was set only when strategy == "parallel", and summary_table declares no clearWith,
    # so switching to a serial strategy left "Parallel testing calculations assume ..." sitting
    # under a row labelled "Serial Testing".
    for (st in c("serial_positive", "serial_negative", "parallel")) {
        nt <- table_notes(run_st(strategy = st))
        expect_true("independence_warning" %in% names(nt), label = paste(st, "has the note"))
        expect_match(nt[["independence_warning"]], "conditionally independent")
        expect_false(grepl("[Pp]arallel", nt[["independence_warning"]]),
                     label = paste(st, "note is not parallel-specific"))
    }
    # and it is also an unconditional notice
    expect_match(run_st()$notices$content, "Independence Assumption")

    serial_positive <- run_st(strategy = "serial_positive")$notices$content
    expect_match(serial_positive, "specificity above is too high")
    expect_match(serial_positive, "sensitivity too low")

    for (st in c("serial_negative", "parallel")) {
        notice <- run_st(strategy = st)$notices$content
        expect_match(notice, "sensitivity above is too high", info = st)
        expect_match(notice, "specificity too low", info = st)
    }
    expect_false(grepl("combined performance may be overestimated", serial_positive,
                       ignore.case = TRUE))
})


test_that("the summary table states that its inputs carry no uncertainty", {
    for (st in c("serial_positive", "serial_negative", "parallel")) {
        nt <- table_notes(run_st(strategy = st))
        expect_true("fixed_inputs" %in% names(nt), label = st)
        expect_match(nt[["fixed_inputs"]], "treated as exact")
        expect_match(nt[["fixed_inputs"]], "no</i> confidence interval")
    }
})


test_that("notes do not go stale when the strategy changes on the same analysis object", {
    h <- private_st(strategy = "parallel")
    keys <- function() names(h$a$results$summary_table$.__enclos_env__$private$.notes)

    h$p$.run()
    expect_true("equivalence_note" %in% keys())

    op <- h$o$option("strategy"); op$value <- "serial_positive"
    h$p$.run()
    expect_equal(h$a$results$summary_table$asDF$strategy_name[1], "Serial Testing (Test positives)")
    expect_false("equivalence_note" %in% keys())
})


test_that("teaching examples are applied when the analysis is called from R", {
    # Examples are applied in the GUI by jamovi/js/sequentialtests.events.js. Nothing runs that
    # JavaScript from R, so `preset =` was silently ignored and the panel defaults were analysed
    # -- a user asking for the HIV scenario got 0.95/0.70 + 0.80/0.98 at 10% prevalence.
    d <- summary_of(ClinicoPath::sequentialtests(preset = "hiv_screening_confirmation"))
    expect_equal(d$first_test[1], "HIV Ag/Ab Assay")
    expect_equal(d$second_test[1], "Differentiation Assay")
    expect_equal(d$prevalence[1], 0.02)
    expect_equal(d$combined_sens[1], 0.98 * 0.99, tolerance = 1e-12)
    expect_equal(d$combined_spec[1], 0.95 + 0.05 * 0.99, tolerance = 1e-12)

    # a preset also carries its strategy
    expect_equal(summary_of(ClinicoPath::sequentialtests(
        preset = "mi_emergency_parallel"))$strategy_name[1], "Parallel Testing (Test all)")
    expect_equal(summary_of(ClinicoPath::sequentialtests(
        preset = "prostate_screening_exclusion"))$strategy_name[1], "Serial Testing (Test negatives)")

    # custom leaves the supplied values alone
    expect_equal(summary_of(run_st(preset = "custom"))$first_test[1], "Screening Test")
})


test_that("the R teaching-example table and the JavaScript table agree", {
    # The values are necessarily duplicated: the GUI path is JavaScript and the R path is not.
    # This test is the guard that keeps the two from drifting.
    js <- paste(readLines("../../jamovi/js/sequentialtests.events.js", warn = FALSE), collapse = "\n")
    # (?s) so "." crosses newlines -- without it this matched nothing and the test passed vacuously
    block <- regmatches(js, regexpr("(?s)SEQUENTIAL_PRESET_CONFIGS\\s*=\\s*\\{.*?\\n\\};", js, perl = TRUE))
    expect_true(nzchar(block))

    # split into "name: { ... }" entries
    entries <- regmatches(block, gregexpr("\\n    ([a-z0-9_]+):\\s*\\{[^}]*\\}", block, perl = TRUE))[[1]]
    js_presets <- list()
    for (e in entries) {
        nm <- sub("^\\n    ([a-z0-9_]+):.*$", "\\1", e)
        kv <- regmatches(e, gregexpr("([a-z0-9_]+):\\s*('[^']*'|[0-9.]+)", e, perl = TRUE))[[1]]
        kv <- kv[-1]                                    # drop the preset name itself
        vals <- list()
        for (pair in kv) {
            k <- sub(":.*$", "", pair)
            v <- trimws(sub("^[a-z0-9_]+:\\s*", "", pair))
            if (k == "guidance") next
            vals[[k]] <- if (grepl("^'", v)) gsub("'", "", v) else as.numeric(v)
        }
        js_presets[[nm]] <- vals
    }
    js_presets$custom <- NULL

    prv <- private_st()$p
    a_yaml <- paste(readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE), collapse = "\n")

    expect_gt(length(js_presets), 0)
    for (nm in names(js_presets)) {
        r <- prv$.getPresetValues(nm)
        expect_false(is.null(r), label = paste("R table has", nm))
        for (k in names(js_presets[[nm]]))
            expect_equal(r[[k]], js_presets[[nm]][[k]],
                         label = sprintf("%s$%s (R %s vs JS %s)", nm, k, r[[k]], js_presets[[nm]][[k]]))
        # and the preset is offered in the option list
        expect_match(a_yaml, paste0("name: ", nm))
    }
})


test_that("teaching examples are labelled as examples, not clinical guidance", {
    # The example numbers are rounded teaching values with no study population behind them.
    # Every user-facing location must prevent them being mistaken for a clinical protocol.
    presets <- c("covid_screening_confirmation", "breast_cancer_screening",
                 "mi_emergency_parallel", "tb_screening_confirmation",
                 "prostate_screening_exclusion", "hiv_screening_confirmation",
                 "stroke_emergency_parallel")
    for (ps in presets) {
        n <- ClinicoPath::sequentialtests(preset = ps)$notices$content
        expect_match(n, "Teaching Example Values Are Not Clinical Guidance", info = ps)
        expect_match(n, "NOT validated clinical parameters", info = ps)
        expect_match(n, "Strong warning: Teaching Example Values Are Not Clinical Guidance",
                     fixed = TRUE, info = ps)
        expect_match(n, "not clinical guidance", ignore.case = TRUE, info = ps)
    }
    # but it does not nag when the user supplied their own numbers
    expect_false(grepl("Teaching Example Values", run_st()$notices$content))

    # The same caveat is carried in the option panel and result guidance.
    js <- paste(readLines("../../jamovi/js/sequentialtests.events.js", warn = FALSE), collapse = " ")
    a_yaml <- paste(readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE), collapse = " ")
    u_yaml <- paste(readLines("../../jamovi/sequentialtests.u.yaml", warn = FALSE), collapse = " ")
    # The guidance item is visible only with show_explanation and is now built only then.
    guidance <- run_st(show_explanation = TRUE)$clinical_guidance$content
    expect_match(a_yaml, "not clinical guidance", ignore.case = TRUE)
    expect_match(u_yaml, "Not Clinical Guidance", fixed = TRUE)
    expect_match(guidance, "Teaching examples only", fixed = TRUE)
    expect_false(grepl("evidence-based\\s+test parameters", a_yaml))
    expect_false(grepl("Western Blot", paste(a_yaml, js, guidance), fixed = TRUE))
})


test_that("dependence guidance explains direction and cites its source", {
    guidance <- run_st(show_explanation = TRUE)$clinical_guidance$content

    expect_match(guidance, "Why conditional dependence matters", fixed = TRUE)
    expect_match(guidance, "specificity too high and combined sensitivity too low",
                 fixed = TRUE)
    expect_match(guidance, "sensitivity too high and combined specificity too low",
                 fixed = TRUE)
    expect_match(guidance, "Gardner et al. (2000)", fixed = TRUE)
    expect_match(
        guidance,
        "Conditional dependence between tests affects the diagnosis and surveillance of animal diseases",
        fixed = TRUE
    )

    r_yaml <- paste(
        readLines("../../jamovi/sequentialtests.r.yaml", warn = FALSE),
        collapse = "\n"
    )
    expect_match(r_yaml, "ConditionalDependenceDiagnosticTests", fixed = TRUE)
})


test_that("performance plot supports duplicate display names", {
    h <- private_st(test1_name = "Same", test2_name = "Same", show_plots = TRUE)
    h$p$.run()
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(h$p$.plot_performance(
        h$a$results$plot_performance,
        ggtheme = ggplot2::theme_bw()
    ))
})


test_that("cost table reports and charges for the same expected test count", {
    res <- run_st(
        test1_sens = 0.87, test1_spec = 0.82,
        test2_sens = 0.83, test2_spec = 0.94,
        prevalence = 0.123, population_size = 101,
        test1_cost = 10, test2_cost = 100,
        show_cost_analysis = TRUE
    )
    d <- res$cost_analysis_table$asDF
    second <- d[d$item == "Test 2: Confirmatory Test", ]
    total <- d[d$item == "Total Protocol Cost", ]

    expect_false(second$number_tests == floor(second$number_tests))
    expect_equal(second$total_cost, second$number_tests * second$unit_cost, tolerance = 1e-12)
    expect_equal(total$number_tests, sum(d$number_tests[1:2]), tolerance = 1e-12)
    expect_equal(total$total_cost, sum(d$total_cost[1:2]), tolerance = 1e-12)
})


test_that("boundary warnings are reachable and severity is preserved", {
    low <- run_st(prevalence = 0.001)$notices$content
    high_accuracy <- run_st(test1_sens = 0.99, test2_spec = 0.99)$notices$content

    expect_match(low, "Strong warning: Prevalence Very Low", fixed = TRUE)
    expect_match(high_accuracy, "Warning: Test 1 Sensitivity High", fixed = TRUE)
    expect_match(high_accuracy, "Warning: Test 2 Specificity High", fixed = TRUE)
})


test_that("dead JavaScript guidance sink and debug logging are absent", {
    js <- paste(readLines("../../jamovi/js/sequentialtests.events.js", warn = FALSE), collapse = "\n")
    expect_false(grepl("ui.clinical_guidance", js, fixed = TRUE))
    expect_false(grepl("console.log", js, fixed = TRUE))
    expect_false(grepl("guidanceHtml", js, fixed = TRUE))
})


test_that("documentation and references match the repaired analysis", {
    header <- readLines("../../R/sequentialtests.h.R", warn = FALSE)
    r_yaml <- paste(readLines("../../jamovi/sequentialtests.r.yaml", warn = FALSE), collapse = "\n")
    a_yaml <- paste(readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE), collapse = "\n")
    u_yaml <- paste(readLines("../../jamovi/sequentialtests.u.yaml", warn = FALSE), collapse = "\n")
    refs <- paste(readLines("../../jamovi/00refs.yaml", warn = FALSE), collapse = "\n")

    expect_false(any(grepl("^#' @param [A-Za-z0-9_]+ \\.$", header)))
    expect_match(r_yaml, "ConditionalDependenceDiagnosticTests", fixed = TRUE)
    expect_match(r_yaml, "CDC_HIV_Testing_2023", fixed = TRUE)
    expect_true(grepl(
        "name: clinical_guidance[\\s\\S]*?visible: \\(show_explanation\\)",
        r_yaml, perl = TRUE
    ))
    expect_lt(
        regexpr("- ClinicoPathJamoviModule", r_yaml, fixed = TRUE)[1],
        regexpr("- ConditionalDependenceDiagnosticTests", r_yaml, fixed = TRUE)[1]
    )
    expect_match(a_yaml, "title: Diagnostic plots", fixed = TRUE)
    expect_true(grepl(
        "type: CollapseBox\\n    label: Display Options\\n    collapsed: true",
        u_yaml, perl = TRUE
    ))
    expect_false(grepl("    - Fagan", r_yaml, fixed = TRUE))
    expect_false(grepl("    - sensspecwiki", r_yaml, fixed = TRUE))
    expect_match(refs, "10.1016/S0167-5877(00)00119-7", fixed = TRUE)
    expect_match(refs, "https://stacks.cdc.gov/view/cdc/129018", fixed = TRUE)
})


test_that("the bundled demonstration datasets are documented as illustrative", {
    doc <- paste(readLines("../../R/data-sequentialtests.R", warn = FALSE), collapse = "\n")
    expect_match(doc, "not clinically accurate")
    # every shipped dataset is covered by the block
    rda <- sub("\\.rda$", "", basename(Sys.glob("../../data/sequentialtests_*.rda")))
    expect_gt(length(rda), 0)
    for (nm in rda)
        expect_match(doc, paste0('"', nm, '"'), fixed = TRUE, info = nm)
})


test_that("an unrecognised preset is reported rather than silently ignored", {
    prv <- private_st()$p
    expect_null(prv$.getPresetValues("no_such_preset"))
})


test_that("population flow is internally consistent with the combined operating point", {
    for (st in c("serial_positive", "serial_negative", "parallel")) {
        res <- run_st(strategy = st, population_size = 10000)
        d <- summary_of(res)
        flow <- res$population_flow_table$asDF
        final <- flow[nrow(flow), ]

        expect_equal(final$true_pos + final$false_neg, final$disease_pos, tolerance = 1e-6)
        expect_equal(final$true_neg + final$false_pos, final$disease_neg, tolerance = 1e-6)
        expect_equal(final$test_pos + final$test_neg, final$total_n, tolerance = 1e-6)
        # the counts must reproduce the reported sensitivity and specificity
        expect_equal(final$true_pos / final$disease_pos, d$combined_sens[1], tolerance = 1e-9,
                     label = paste(st, "flow sens"))
        expect_equal(final$true_neg / final$disease_neg, d$combined_spec[1], tolerance = 1e-9,
                     label = paste(st, "flow spec"))
    }
})


test_that("repeated runs do not duplicate rows or notices", {
    h <- private_st(show_cost_analysis = TRUE)
    seen <- lapply(1:3, function(i) {
        h$p$.run()
        c(summary    = h$a$results$summary_table$rowCount,
          individual = h$a$results$individual_tests_table$rowCount,
          flow       = h$a$results$population_flow_table$rowCount,
          cost       = h$a$results$cost_analysis_table$rowCount,
          notices    = length(h$p$.noticeList))
    })
    expect_equal(seen[[2]], seen[[1]])
    expect_equal(seen[[3]], seen[[1]])
    expect_equal(unname(seen[[1]]["individual"]), 3L)
    expect_equal(unname(seen[[1]]["flow"]), 3L)
    expect_equal(unname(seen[[1]]["cost"]), 3L)
})


test_that("every diagnostic plot renders", {
    h <- private_st(show_plots = TRUE)
    h$p$.run()
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    for (nm in c("plot_flow_diagram", "plot_performance", "plot_probability",
                 "plot_population_flow", "plot_sensitivity_analysis"))
        expect_true(h$p[[paste0(".", nm)]](h$a$results[[nm]], ggtheme = ggplot2::theme_bw()),
                    label = nm)
})


test_that("each probability path spans only the steps its own subjects receive", {
    # Until 2026-09-21 both paths were drawn across all three x positions and the branch that
    # LEAVES the pathway after test 1 was terminated on the pooled Combined_PPV / Combined_NPV.
    # A pooled predictive value is a mixture over two subgroups with different posteriors, so it
    # is not the continuation of either branch; drawing it moved a line at an x position labelled
    # for a test those subjects never received. With sens = 0.60 and spec = 0.70 on both tests
    # and prevalence = 0.20:
    #   serial positive -- final negatives are {T1-}, P(D) = 12.50%, and {T1+,T2-}, P(D) = 22.22%.
    #     The pooled 1 - NPV = 14.95% is neither, and the T1- line was drawn rising 12.50 -> 14.95.
    #   serial negative -- final positives are {T1+}, P(D) = 33.33%, and {T1-,T2+}, P(D) = 22.22%.
    #     The pooled PPV = 29.17% is neither, and the T1+ line was drawn falling 33.33 -> 29.17.
    # Where a final class IS a single branch the two coincide exactly (serial positive: final
    # positive == {T1+,T2+}; serial negative: final negative == {T1-,T2-}), and the endpoint must
    # still equal the pooled value -- that identity is asserted below.
    prev <- 0.20; s1 <- 0.60; p1 <- 0.70; s2 <- 0.60; p2 <- 0.70

    # Independent oracle: P(D | pattern) straight from Bayes, not from the module's LR chaining.
    pd <- function(lik_d, lik_h) (prev * lik_d) / (prev * lik_d + (1 - prev) * lik_h)
    p_t1pos <- pd(s1, 1 - p1)                              # 0.3333333333
    p_t1neg <- pd(1 - s1, p1)                              # 0.1250000000
    p_pospos <- pd(s1 * s2, (1 - p1) * (1 - p2))           # 0.5000000000
    p_negneg <- pd((1 - s1) * (1 - s2), p1 * p2)           # 0.0754716981
    p_either <- pd(1 - (1 - s1) * (1 - s2), 1 - p1 * p2)   # 0.2916666667

    render_probability_data <- function(strategy) {
        h <- private_st(
            strategy = strategy,
            test1_sens = s1, test1_spec = p1,
            test2_sens = s2, test2_spec = p2,
            prevalence = prev, show_plots = TRUE
        )
        h$p$.run()
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off(), add = TRUE)
        expect_true(h$p$.plot_probability(
            h$a$results$plot_probability,
            ggtheme = ggplot2::theme_bw()
        ))
        list(
            data = ggplot2::last_plot()$data,
            caption = ggplot2::last_plot()$labels$caption,
            state = h$a$results$plot_probability$state
        )
    }

    # An NA or NaN here would reach the rendered plot as a missing point.
    check_no_na <- function(d) expect_false(anyNA(d$Probability))
    # The caption is strwrap()ped for the plot, so match against a single-spaced copy.
    flat_caption <- function(x) gsub("[[:space:]]+", " ", x)

    serial_positive <- render_probability_data("serial_positive")
    check_no_na(serial_positive$data)
    # positive path: pre -> after T1+ -> after T2+ ; negative path stops at test 1.
    expect_equal(
        serial_positive$data$Probability,
        100 * c(prev, p_t1pos, p_pospos, prev, p_t1neg),
        tolerance = 1e-12
    )
    expect_equal(serial_positive$data$x, c(1, 2, 3, 1, 2))
    expect_equal(
        serial_positive$data$Probability[3],
        100 * serial_positive$state$Combined_PPV,
        tolerance = 1e-12
    )
    expect_false(isTRUE(all.equal(
        serial_positive$data$Probability[5],
        100 * (1 - serial_positive$state$Combined_NPV))))
    # ... and because it is NOT that pooled number, the plot has to say so. Until 2026-09-22 the
    # pane shipped 12.5% on the line and 14.95% in the tables for "probability after a negative
    # result" with nothing to tell them apart. The final-negative class is {T1-} (P(D) = 12.50%,
    # weight 0.640/0.856 = 0.7477) plus {T1+,T2-} (P(D) = 22.22%, weight 0.216/0.856 = 0.2523,
    # i.e. 21.6% of the cohort); 0.7477*0.125 + 0.2523*0.2222 = 0.1495, weights summing to 1.
    sp_cap <- flat_caption(serial_positive$caption)
    expect_true(length(sp_cap) == 1L && nzchar(sp_cap))
    expect_match(sp_cap, "Negative Path ends at the first-test-negative subgroup", fixed = TRUE)
    expect_match(sp_cap, "P(disease | Test 1 negative) = 12.5%", fixed = TRUE)
    expect_match(sp_cap, "1 - combined NPV = 15.0%", fixed = TRUE)
    expect_match(sp_cap, "Test 1 positive / Test 2 negative subgroup (P(disease) = 22.2%)", fixed = TRUE)

    serial_negative <- render_probability_data("serial_negative")
    check_no_na(serial_negative$data)
    # negative path: pre -> after T1- -> after T2- ; positive path stops at test 1.
    expect_equal(
        serial_negative$data$Probability,
        100 * c(prev, p_t1pos, prev, p_t1neg, p_negneg),
        tolerance = 1e-12
    )
    expect_equal(serial_negative$data$x, c(1, 2, 1, 2, 3))
    expect_equal(
        serial_negative$data$Probability[5],
        100 * (1 - serial_negative$state$Combined_NPV),
        tolerance = 1e-12
    )
    expect_false(isTRUE(all.equal(
        serial_negative$data$Probability[2],
        100 * serial_negative$state$Combined_PPV)))
    # Mirror image of the serial-positive case: final positives are {T1+} (P(D) = 33.33%, weight
    # 0.360/0.576 = 0.625) plus {T1-,T2+} (P(D) = 22.22%, weight 0.216/0.576 = 0.375);
    # 0.625*0.3333 + 0.375*0.2222 = 0.2917, weights summing to 1.
    sn_cap <- flat_caption(serial_negative$caption)
    expect_true(length(sn_cap) == 1L && nzchar(sn_cap))
    expect_match(sn_cap, "Positive Path ends at the first-test-positive subgroup", fixed = TRUE)
    expect_match(sn_cap, "P(disease | Test 1 positive) = 33.3%", fixed = TRUE)
    expect_match(sn_cap, "combined PPV = 29.2%", fixed = TRUE)
    expect_match(sn_cap, "Test 1 negative / Test 2 positive subgroup (P(disease) = 22.2%)", fixed = TRUE)

    # Parallel applies both tests at once, so there is no intermediate step and neither branch
    # is truncated: each endpoint is the POOLED value the tables report, which is why both
    # identities below hold. Only the negative endpoint is also a single pattern, {T1-,T2-},
    # P(D) = 7.55%. The positive endpoint 29.17% is a genuine three-way mixture of {T1+,T2+}
    # (P(D) = 50.00%, weight 0.250), {T1+,T2-} (22.22%, 0.375) and {T1-,T2+} (22.22%, 0.375):
    # 0.250*0.5 + 0.375*0.2222 + 0.375*0.2222 = 0.2917, weights summing to 1. The plot shows
    # that mixture rather than one of its parts, so nothing here contradicts the tables.
    parallel <- render_probability_data("parallel")
    check_no_na(parallel$data)
    expect_equal(
        parallel$data$Probability,
        100 * c(prev, p_either, prev, p_negneg),
        tolerance = 1e-12
    )
    expect_equal(parallel$data$x, c(1, 2, 1, 2))
    expect_equal(parallel$data$Probability[2], 100 * parallel$state$Combined_PPV, tolerance = 1e-12)
    expect_equal(parallel$data$Probability[4], 100 * (1 - parallel$state$Combined_NPV), tolerance = 1e-12)
    # Nothing is truncated here, so there is no discrepancy to caption.
    expect_null(parallel$caption)
})


test_that("translated templates interpolate dynamic values without ellipses", {
    res <- run_st(
        show_explanation = TRUE,
        show_formulas = TRUE,
        show_cost_analysis = TRUE,
        population_size = 1234
    )
    rendered <- paste(
        res$notices$content,
        res$plain_summary$content,
        res$explanation_text$content,
        res$formulas_text$content,
        paste(res$cost_analysis_table$asDF$item, collapse = " ")
    )
    expect_false(grepl("\u2026", rendered, fixed = TRUE))
    expect_match(rendered, "Test 2: Confirmatory Test", fixed = TRUE)
    expect_match(rendered, "combined specificity above is too high", fixed = TRUE)
    # jmvcore::format() ignores underscored placeholder names, so {final_pos}-style tokens
    # shipped as literal braces in the explanation and formulas panels until 2026-09-02.
    expect_false(grepl("[{][A-Za-z_]+[}]", rendered))
    expect_match(res$explanation_text$content, "1,234", fixed = TRUE)
    expect_match(res$explanation_text$content, "have a final positive classification", fixed = TRUE)
    expect_match(res$formulas_text$content, "Denominator = P", fixed = TRUE)
})

test_that("plot subtitles show the translated strategy label, not the internal key", {
    pr <- private_st(strategy = "serial_positive", show_plots = TRUE)
    pr$p$.run()
    st <- pr$a$results$plot_performance$state
    expect_equal(st$Strategy, "serial_positive")
    expect_match(st$StrategyLabel, "Serial Testing", fixed = TRUE)
})


test_that("the analysis does not advertise a Fagan nomogram it does not have", {
    # description.main promised "Fagan nomograms"; the word appeared nowhere else in the module --
    # no option, no results item, no code. That text becomes the roxygen description in
    # man/sequentialtests.Rd and the description shown in jamovi.
    src <- c(readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE),
             readLines("../../jamovi/sequentialtests.r.yaml", warn = FALSE))
    expect_false(any(grepl("nomogram", src, ignore.case = TRUE)))
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/sequentialtests.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("self\\$options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


test_that("release metadata versions agree", {
    description_version <- unname(read.dcf("../../DESCRIPTION", fields = "Version")[1, 1])
    analysis_version <- as.character(
        yaml::read_yaml("../../jamovi/sequentialtests.a.yaml")$version)
    module_version <- as.character(yaml::read_yaml("../../jamovi/0000.yaml")$version)
    citation_version <- as.character(yaml::read_yaml("../../CITATION.cff")$version)

    # The analysis version is stamped by _updateModules.R from `new_version` in its config, not
    # from the umbrella DESCRIPTION, so the two drift apart between releases (1.0.83 vs 1.0.81.01
    # on 2026-10-08) and the old DESCRIPTION comparison failed on a correct tree.
    shipped_version <- as.character(
        yaml::read_yaml("../../_updateModules_config.yaml")$new_version)
    expect_identical(analysis_version, sub("^([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", shipped_version))
    expect_identical(module_version, description_version)
    expect_identical(citation_version, description_version)
})


test_that("an out-of-range entry is rejected, never silently clamped in the GUI", {
    # Same defect as cotest: events.js clamped "80" (meaning 80%) to 0.99 and ran on it.
    js <- gsub("//[^\n]*", "", paste(readLines("../../jamovi/js/sequentialtests.events.js",
                                               warn = FALSE), collapse = "\n"))
    expect_false(grepl("clamp|Math\\.(min|max)|prevalence = 0\\.0", js))
    u <- readLines("../../jamovi/sequentialtests.u.yaml", warn = FALSE)
    expect_identical(trimws(grep("change:", u, value = TRUE)),
                     "change: ./sequentialtests.events::onChange_preset")

    expect_error(run_st(test1_sens = 80), "between 0.01 and 0.99")
    expect_error(run_st(prevalence = 10), "between 0.001 and 0.999")
})


test_that("a test name with markup characters survives every table cell", {
    # jamovi parses table cells as HTML, so "<LOD assay" opened a tag and rendered blank.
    res <- run_st(test1_name = "<LOD assay", test2_name = "Culture <b>", show_cost_analysis = TRUE)
    st <- summary_of(res)
    expect_identical(st$first_test, "< LOD assay")
    expect_identical(st$second_test, "Culture < b>")
    it <- res$individual_tests_table$asDF
    expect_identical(it$test_name[1:2], c("< LOD assay", "Culture < b>"))
    ct <- res$cost_analysis_table$asDF
    expect_true(all(c("Test 1: < LOD assay", "Test 2: Culture < b>") %in% ct$item))
    # "<5" opens no tag and is left alone
    expect_identical(summary_of(run_st(test1_name = "CRP <5"))$first_test, "CRP <5")
})


# ---- /check-function-full 2026-10-08 ---------------------------------------------------------

test_that("the teaching-example notice names the example, not its option key", {
    hiv <- run_st(preset = "hiv_screening_confirmation")$notices$content
    expect_match(hiv, '"HIV Ag/Ab + Differentiation Assay (Illustrative)" teaching example', fixed = TRUE)
    expect_false(grepl("hiv_screening_confirmation", hiv, fixed = TRUE))
})

test_that("an imperfect combined value is never printed as 100.0% or 0.0%", {
    p <- private_st()$p
    expect_identical(p$.formatPercent(0.9999), "99.99%")
    expect_identical(p$.formatPercent(0.0001), "0.01%")
    expect_identical(p$.formatPercent(0.5), "50.0%")
    expect_identical(p$.formatPercent(1), "100.0%")
    # PPV/NPV are not bounded away from the ends: prevalence 0.999 drives PPV to 0.9999999
    expect_identical(p$.formatPercent(0.9999999), ">99.99%")
    expect_identical(p$.formatPercent(1e-7), "<0.01%")
    # HIV example: serial-positive specificity 0.95 + 0.05 x 0.99 = 0.9995
    res <- run_st(preset = "hiv_screening_confirmation", show_explanation = TRUE)
    expect_false(grepl("100.0%", res$notices$content, fixed = TRUE))
    expect_false(grepl("100.0% specificity", res$plain_summary$content, fixed = TRUE))
})

test_that("name similarity is a WARNING, not a STRONG WARNING", {
    n <- run_st(test1_name = "PCR", test2_name = "RT-PCR")$notices$content
    expect_match(n, "(^|\n)Warning: Test Correlation Risk")
    expect_false(grepl("Strong warning: Test Correlation Risk", n, fixed = TRUE))
})

test_that("an empty test name falls back to a label instead of blocking the analysis", {
    st <- summary_of(run_st(test1_name = "  ", test2_name = ""))
    expect_identical(c(st$first_test, st$second_test), c("Test 1", "Test 2"))
    expect_false(is.na(st$combined_sens))
    # the two fallback labels are one edit apart and must not trip the similarity notice
    expect_false(grepl("Test Correlation Risk",
                       run_st(test1_name = "", test2_name = "")$notices$content, fixed = TRUE))
})

test_that("the cost table explains an all-zero cost column", {
    notes_of <- function(res) names(res$cost_analysis_table$.__enclos_env__$private$.notes)
    expect_true("zero_costs" %in% notes_of(run_st(show_cost_analysis = TRUE)))
    expect_false("zero_costs" %in% notes_of(run_st(show_cost_analysis = TRUE, test2_cost = 50)))
})

test_that("Html names are escaped only where HTML needs it", {
    html <- run_st(test1_name = "HIV Ag/Ab <x>", show_explanation = TRUE)$plain_summary$content
    expect_match(html, "HIV Ag/Ab &lt;x&gt;", fixed = TRUE)
    expect_false(grepl("&#x", html, fixed = TRUE))
})

test_that("plots: schematics have no axes, labels are clean, facets are ordered", {
    # A ggtheme that sets the child axis elements explicitly, as jamovi's does: blanking the
    # parent axis.text alone left coordinate axes on the schematics.
    jtheme <- ggplot2::theme_bw() + ggplot2::theme(
        axis.text.x = ggplot2::element_text(), axis.text.y = ggplot2::element_text(),
        axis.line = ggplot2::element_line(), panel.grid.major = ggplot2::element_line())
    h <- private_st(preset = "hiv_screening_confirmation", show_plots = TRUE)
    h$p$.run()
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    draw <- function(nm) {
        expect_true(h$p[[paste0(".", nm)]](h$a$results[[nm]], ggtheme = jtheme), label = nm)
        ggplot2::last_plot()
    }
    for (nm in c("plot_flow_diagram", "plot_population_flow")) {
        th <- draw(nm)$theme
        for (el in c("axis.text.x", "axis.text.y", "axis.line", "panel.grid.major"))
            expect_true(inherits(th[[el]], "element_blank"), label = paste(nm, el))
    }
    pr <- draw("plot_probability")
    txt <- Filter(function(l) inherits(l$geom, "GeomText"), pr$layers)
    expect_true(length(txt) == 1 && isFALSE(txt[[1]]$show.legend))
    pf <- draw("plot_performance")
    expect_identical(levels(pf$data$Metric), c("Sensitivity", "Specificity", "PPV", "NPV"))
    expect_identical(pf$theme$legend.position, "none")
})

test_that("no .() msgid carries a line break, and cost options clear only the cost table", {
    # A "\n" inside .() never reached the translation catalog; layout is applied in R instead.
    src <- readLines("../../R/sequentialtests.b.R", warn = FALSE)
    expect_false(any(grepl("\\.\\((\"|')[^\"']*\\\\n", src)))
    ry <- yaml::read_yaml("../../jamovi/sequentialtests.r.yaml")
    for (it in ry$items)
        if (it$name != "cost_analysis_table")
            expect_false(any(c("test1_cost", "test2_cost") %in% it$clearWith), label = it$name)
})

test_that("every teaching example stays inside the option bounds", {
    # The direct PPV/NPV/LR divisions rely on these bounds, and a preset bypasses the option check.
    p <- private_st()$p
    for (nm in c("covid_screening_confirmation", "breast_cancer_screening", "mi_emergency_parallel",
                 "tb_screening_confirmation", "prostate_screening_exclusion",
                 "hiv_screening_confirmation", "stroke_emergency_parallel")) {
        v <- p$.getPresetValues(nm)
        expect_true(all(unlist(v[c("test1_sens", "test1_spec", "test2_sens", "test2_spec")]) >= 0.01 &
                        unlist(v[c("test1_sens", "test1_spec", "test2_sens", "test2_spec")]) <= 0.99), label = nm)
        expect_true(v$prevalence >= 0.001 && v$prevalence <= 0.999, label = nm)
        expect_true(nzchar(v$title), label = nm)
    }
})


# ---- /review-function 2026-10-08: findings IR-01 to IR-08 of the independent review ----------

test_that("test order changes no combined figure, and the notice says so (IR-01)", {
    # Both serial rules are symmetric in the two tests. The old WARNING ("Test 2 specificity is
    # below Test 1 ... may not improve specificity as much as expected") fired on one order only,
    # and on the one that performs FEWER second tests.
    for (s in c("serial_positive", "serial_negative")) {
        a <- run_st(strategy = s, show_cost_analysis = TRUE)
        b <- run_st(strategy = s, test1_sens = 0.80, test1_spec = 0.98,
                    test2_sens = 0.95, test2_spec = 0.70, show_cost_analysis = TRUE)
        cols <- c("combined_sens", "combined_spec", "combined_ppv", "combined_npv")
        expect_equal(unlist(summary_of(a)[cols]), unlist(summary_of(b)[cols]), tolerance = 1e-12, label = s)
        for (r in list(a, b))
            expect_false(grepl("Assumption Check", r$notices$content, fixed = TRUE), label = s)
        # The notice appears only in the order whose swap would need fewer second tests, and its
        # counts are the cost table's second-test count and the swapped one.
        n2 <- c(a = a$cost_analysis_table$asDF$number_tests[2], b = b$cost_analysis_table$asDF$number_tests[2])
        has <- c(a = grepl("Test Order", a$notices$content, fixed = TRUE),
                 b = grepl("Test Order", b$notices$content, fixed = TRUE))
        expect_identical(unname(has), unname(n2 > rev(n2)), label = s)
        noisy <- if (has[["a"]]) a else b
        expect_match(noisy$notices$content,
                     sprintf("about %s per 1,000 people in this order, about %s if the two tests were swapped",
                             round(max(n2)), round(min(n2))), fixed = TRUE, label = s)
        expect_false(grepl("Warning: Test Order", noisy$notices$content, fixed = TRUE))
    }
    # Parallel testing gives everyone both tests: order means nothing, so no notice.
    expect_false(grepl("Test Order", run_st(strategy = "parallel")$notices$content, fixed = TRUE))
})

test_that("no 'N of every 100' sentence claims 0 or 100 for an interior value (IR-02)", {
    p <- private_st()$p
    expect_identical(p$.formatPer100(0.9995), "more than 99")
    expect_identical(p$.formatPer100(0.995), "more than 99")
    expect_identical(p$.formatPer100(0.004), "fewer than 1")
    expect_identical(p$.formatPer100(0.97), "97")
    expect_identical(p$.formatPer100(1), "100")
    # HIV example: Sp = .95 + .05 x .99 = .9995; serial-negative .95/.95: Se = .9975
    hiv <- run_st(preset = "hiv_screening_confirmation", show_explanation = TRUE)$plain_summary$content
    sn <- run_st(strategy = "serial_negative", test1_sens = .95, test2_sens = .95,
                 show_explanation = TRUE)$plain_summary$content
    for (h in list(hiv, sn)) expect_false(grepl("100 of every 100", h, fixed = TRUE))
    expect_match(hiv, "rules out more than 99 of every 100 healthy", fixed = TRUE)
    expect_match(sn, "detects more than 99 of every 100 diseased", fixed = TRUE)
    # The NNS sentence keeps its space now that the template carries it.
    expect_match(hiv, "(NPV). You would need to screen", fixed = TRUE)
})

test_that("the formulas panel never rounds an interior PPV to 1", {
    p <- private_st()$p
    expect_identical(p$.formatProb(0.99999), "0.99999")
    expect_identical(p$.formatProb(0.9999999), "0.9999999")
    expect_identical(p$.formatProb(0.9999), "0.9999")
    expect_identical(p$.formatProb(1), "1")
    # Se .9801, Sp .9999 at prevalence .999: PPV = 0.99999990
    f <- run_st(test1_sens = .99, test1_spec = .99, test2_sens = .99, test2_spec = .99,
                prevalence = .999, show_formulas = TRUE)$formulas_text$content
    expect_false(grepl("= 1</li>", f, fixed = TRUE))
    expect_match(f, "PPV = numerator/denominator = [0-9.]+/[0-9.]+ = 0\\.99999+</li>")
    expect_false(grepl(".(\"<ul>\")", paste(readLines("../../R/sequentialtests.b.R"), collapse = "\n"), fixed = TRUE))
})

test_that("each table says what its numbers are (IR-03, IR-06)", {
    res <- run_st()
    notes <- function(tab) vapply(tab$.__enclos_env__$private$.notes,
                                  function(e) get("note", envir = e), character(1))
    expect_match(notes(res$individual_tests_table)[["standalone"]], "used on its own at the entered prevalence", fixed = TRUE)
    expect_match(notes(res$population_flow_table)[["expected_counts"]], "expected values", fixed = TRUE)
})

test_that("expected counts below 10 are not rounded to 0 (IR-05)", {
    h <- private_st(population_size = 100, prevalence = .001, show_explanation = TRUE, show_plots = TRUE)
    h$p$.run()
    ex <- h$a$results$explanation_text$content
    expect_false(grepl("Among 0 expected", ex, fixed = TRUE))
    expect_match(ex, "Among 0.1 expected people with disease, 0.076 are correctly identified", fixed = TRUE)
    expect_identical(h$p$.formatCount(9.96), "10")
    expect_identical(h$p$.formatCount(1e-5), "0.00001")
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(h$p$.plot_population_flow(h$a$results$plot_population_flow, ggtheme = ggplot2::theme_bw()))
    labels <- unlist(lapply(ggplot2::last_plot()$layers, function(l) l$aes_params$label))
    expect_true(any(grepl("Disease+\n(n=0.1)", labels, fixed = TRUE)))   # .wrapLabel(, 8)
    expect_false(any(grepl("(n=0)", labels, fixed = TRUE)))
    # Box labels were white on orange/red at alpha .7 (1.8:1, 2.6:1 contrast).
    cols <- unlist(lapply(ggplot2::last_plot()$layers, function(l)
        if (inherits(l$geom, "GeomText")) l$aes_params$colour))
    expect_false("white" %in% cols)
})

test_that("the probability axis stops at 100% (IR-07)", {
    h <- private_st(preset = "hiv_screening_confirmation", show_plots = TRUE)
    h$p$.run()
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(h$p$.plot_probability(h$a$results$plot_probability, ggtheme = ggplot2::theme_bw()))
    built <- ggplot2::ggplot_build(ggplot2::last_plot())
    expect_lte(max(built$layout$panel_params[[1]]$y.range), 100)
    expect_false(anyNA(built$data[[2]]$y))   # no point dropped by the new limit
})

test_that("the 5% low-prevalence edge is presented as a convention (IR-08)", {
    n <- run_st(prevalence = .02)$notices$content
    expect_match(n, "This analysis flags pre-test probabilities below 5%", fixed = TRUE)
    expect_false(grepl("PPV is dominated by specificity", n, fixed = TRUE))
})

test_that("the performance plot and the table share one PPV/NPV formula", {
    h <- private_st(show_plots = TRUE, prevalence = .03)
    h$p$.run()
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    expect_true(h$p$.plot_performance(h$a$results$plot_performance, ggtheme = ggplot2::theme_bw()))
    d <- ggplot2::last_plot()$data
    tab <- h$a$results$individual_tests_table$asDF
    expect_equal(d$Value[d$Metric == "PPV"], tab$ppv * 100, tolerance = 1e-12)
    expect_equal(d$Value[d$Metric == "NPV"], tab$npv * 100, tolerance = 1e-12)
})

test_that("a table cell jamovi would show as 100.0% or 0.0% carries its value (RR-01)", {
    # The jamovi client prints a `pc` column as (100 * x).toFixed(1) at its default number format,
    # and no column option changes that: the COVID and HIV examples' combined specificity 0.9995
    # read "100.0 %" in the summary table while every sentence beside it said 99.95%.
    fn <- function(tab, col, row = 1) tab$getCell(rowNo = row, col = col)$footnotes
    res <- run_st(preset = "hiv_screening_confirmation")
    expect_match(fn(res$summary_table, "combined_spec"), "99.95%", fixed = TRUE)
    expect_match(fn(res$individual_tests_table, "npv", 1), "99.96%", fixed = TRUE)
    expect_length(fn(res$summary_table, "combined_sens"), 0)

    res <- run_st(test1_sens = .01, test1_spec = .01, test2_sens = .01, test2_spec = .01,
                  prevalence = .001)
    expect_match(fn(res$summary_table, "combined_ppv"), "<0.01%", fixed = TRUE)

    res <- run_st()
    for (col in c("combined_sens", "combined_spec", "combined_ppv", "combined_npv"))
        expect_length(fn(res$summary_table, col), 0)

    # A rerun whose value no longer rounds to an endpoint drops the footnote.
    h <- private_st(preset = "hiv_screening_confirmation")
    h$p$.run()
    expect_length(fn(h$a$results$summary_table, "combined_spec"), 1)
    op <- h$o$option("preset"); op$value <- "custom"
    h$p$.run()
    expect_length(fn(h$a$results$summary_table, "combined_spec"), 0)
})

test_that("the formulas panel prints decimals, never scientific notation (RR-02)", {
    p <- private_st()$p
    expect_identical(p$.formatProb(1e-4), "0.0001")
    expect_identical(p$.formatProb(9801), "9801")
    f <- run_st(test1_sens = .01, test1_spec = .01, test2_sens = .01, test2_spec = .01,
                prevalence = .001, show_formulas = TRUE)$formulas_text$content
    expect_false(grepl("[0-9]e[-+][0-9]", f))
})

test_that("asSource() escapes test names so the syntax pane cannot be broken out of (SEC-01)", {
    # jmvcore pastes a String option between quotes unescaped: `test1_name = "A "q""` did not
    # parse, and `" |> paste0(Sys.getpid()) == "` parsed with Sys.getpid() as live code.
    src_of <- function(n1, n2) {
        a <- ClinicoPath:::sequentialtestsClass$new(
            options = ClinicoPath:::sequentialtestsOptions$new(test1_name = n1, test2_name = n2),
            data = data.frame(x = 1))
        a$asSource()
    }
    names <- list(c('A "q"', "C:\\x"),
                  c('" |> paste0(Sys.getpid()) == "', '", x = file.exists("/etc/passwd"), y = "'))
    for (nm in names) {
        call <- parse(text = src_of(nm[1], nm[2]))[[1]]
        expect_identical(call$test1_name, nm[1])
        expect_identical(call$test2_name, nm[2])
        expect_setequal(all.names(call, functions = TRUE),
                        c("::", "ClinicoPath", "sequentialtests"))
    }
    # default names are not exported
    expect_false(grepl("test1_name", src_of("Screening Test", "Confirmatory Test"), fixed = TRUE))
})

test_that("a long test name is sized to fit its flow-diagram box (FF-01)", {
    # "Immunohistochemistry" (20 characters) ran out of its box and across the incoming arrow.
    # A line longer than the 12-character wrap width is drawn at 3.2 x 12 / its length.
    for (st in c("serial_positive", "serial_negative", "parallel")) {
        h <- private_st(show_plots = TRUE, strategy = st,
                        test1_name = "Immunohistochemistry", test2_name = "CK7")
        h$p$.run()
        grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
        expect_true(h$p$.plot_flow_diagram(h$a$results$plot_flow_diagram, ggtheme = ggplot2::theme_bw()))
        size_of <- function(lbl) {
            hit <- Filter(function(l) identical(l$aes_params$label, lbl), ggplot2::last_plot()$layers)
            hit[[1]]$aes_params$size
        }
        expect_equal(size_of("Immunohistochemistry"), 3.2 * 12 / 20)
        expect_equal(size_of("CK7"), 3.2)   # short names keep the original size
    }
})
