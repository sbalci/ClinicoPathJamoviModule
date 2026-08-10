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


test_that("clinical presets are applied when the analysis is called from R", {
    # Presets are applied in the GUI by jamovi/js/sequentialtests.events.js. Nothing runs that
    # JavaScript from R, so `preset =` was silently ignored and the panel defaults were analysed
    # -- a user asking for the HIV scenario got 0.95/0.70 + 0.80/0.98 at 10% prevalence.
    d <- summary_of(ClinicoPath::sequentialtests(preset = "hiv_screening_confirmation"))
    expect_equal(d$first_test[1], "ELISA")
    expect_equal(d$second_test[1], "Western Blot")
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


test_that("the R preset table and the JavaScript preset table agree", {
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


test_that("presets are labelled as demonstration values, not clinical parameters", {
    # The preset numbers are rounded teaching values with no citation and no population behind
    # them. The option description called them "evidence-based ... from medical literature",
    # which invites a user to treat the output as applicable to a real protocol.
    presets <- c("covid_screening_confirmation", "breast_cancer_screening",
                 "mi_emergency_parallel", "tb_screening_confirmation",
                 "prostate_screening_exclusion", "hiv_screening_confirmation",
                 "stroke_emergency_parallel")
    for (ps in presets) {
        n <- ClinicoPath::sequentialtests(preset = ps)$notices$content
        expect_match(n, "Demonstration Only", info = ps)
        expect_match(n, "NOT validated clinical parameters", info = ps)
        expect_match(n, "WARNING: Preset Values Are For Demonstration Only", fixed = TRUE, info = ps)
    }
    # but it does not nag when the user supplied their own numbers
    expect_false(grepl("Demonstration Only", run_st()$notices$content))

    # the same caveat is carried in the GUI (events JS) and in the option documentation
    js <- paste(readLines("../../jamovi/js/sequentialtests.events.js", warn = FALSE), collapse = " ")
    expect_match(js, "Demonstration values only")
    a_yaml <- paste(readLines("../../jamovi/sequentialtests.a.yaml", warn = FALSE), collapse = " ")
    expect_match(a_yaml, "demonstration and teaching only")
    expect_false(grepl("evidence-based\\s+test parameters", a_yaml))
})


test_that("the bundled demonstration datasets are documented as illustrative", {
    doc <- paste(readLines("../../R/sequentialtests_data.R", warn = FALSE), collapse = "\n")
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
