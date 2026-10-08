# Regression tests from /validate-function sequentialtests depth=exhaustive (2026-10-08).
# Report: development-ideas/sequentialtests-validation-2026-10-08.md
# Evidence script: development-scripts/validate_sequentialtests.R
#
# Oracles are independent of R/sequentialtests.b.R: hand arithmetic, an enumeration over the
# 8-cell joint distribution of (disease, test 1, test 2) with each strategy's decision rule applied
# cell by cell, and epiR (rsu.dxtest, epi.tests), which the analysis does not call.
# The VAL-sequentialtests-NN blocks were skip-guarded open defects; fixed 2026-10-08, now live.

val_run <- function(...) suppressWarnings(suppressMessages(ClinicoPath::sequentialtests(...)))
val_row <- function(df, key) {
    r <- df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
    stopifnot(nrow(r) == 1L)
    r
}
val_notes <- function(tbl) {
    n <- tbl$.__enclos_env__$private$.notes
    stats::setNames(vapply(n, function(e) get("note", envir = e), character(1)), names(n))
}
# Enumeration oracle. Cells (T1, T2) = ++, +-, -+, --; covD / covN = conditional covariances of the
# positive indicators among diseased / non-diseased (Gardner et al. 2000), 0 = independence.
val_oracle <- function(s1, p1, s2, p2, prev, strategy, N = 1000, covD = 0, covN = 0) {
    jD <- c(s1 * s2 + covD, s1 * (1 - s2) - covD, (1 - s1) * s2 - covD, (1 - s1) * (1 - s2) + covD)
    f1 <- 1 - p1; f2 <- 1 - p2
    jN <- c(f1 * f2 + covN, f1 * (1 - f2) - covN, (1 - f1) * f2 - covN, (1 - f1) * (1 - f2) + covN)
    t1 <- c(TRUE, TRUE, FALSE, FALSE); t2 <- c(TRUE, FALSE, TRUE, FALSE)
    pos <- switch(strategy, serial_positive = t1 & t2,
                  serial_negative = t1 | (!t1 & t2), parallel = t1 | t2)
    gets2 <- switch(strategy, serial_positive = t1, serial_negative = !t1, parallel = rep(TRUE, 4))
    pD <- prev * jD; pN <- (1 - prev) * jN; pA <- pD + pN
    list(sens = sum(jD[pos]), spec = sum(jN[!pos]),
         ppv = sum(pD[pos]) / sum(pA[pos]), npv = sum(pN[!pos]) / sum(pA[!pos]),
         t1_ppv = sum(pD[t1]) / sum(pA[t1]), t1_npv = sum(pN[!t1]) / sum(pA[!t1]),
         t2_ppv = sum(pD[t2]) / sum(pA[t2]), t2_npv = sum(pN[!t2]) / sum(pA[!t2]),
         f = N * c(sum(pD[pos]), sum(pN[pos]), sum(pD[!pos]), sum(pN[!pos])),
         n_t2 = N * sum(pA[gets2]),
         pd_t1pos = sum(pD[t1]) / sum(pA[t1]), pd_t1neg = sum(pD[!t1]) / sum(pA[!t1]),
         pd_pn = pD[2] / pA[2], pd_np = pD[3] / pA[3], pd_nn = pD[4] / pA[4])
}


test_that("VAL C04-C08 C19 C21: hand-calculated known answers for every strategy", {
    # Se1 .95 Sp1 .70 Se2 .80 Sp2 .98 P .10 N 1000; oracle: arithmetic written out below
    hand <- list(
        serial_positive = c(sens = .95 * .80, spec = .70 + .30 * .98, ppv = .076 / .0814,
                            npv = .8946 / .9186, nns = 14, tp = 76, fp = 5.4, n2 = 365),
        serial_negative = c(sens = .99, spec = .686, ppv = .099 / .3816, npv = .6174 / .6184,
                            nns = 11, tp = 99, fp = 282.6, n2 = 635),
        parallel        = c(sens = .99, spec = .686, ppv = .099 / .3816, npv = .6174 / .6184,
                            nns = 11, tp = 99, fp = 282.6, n2 = 1000))
    for (s in names(hand)) {
        e <- hand[[s]]
        r <- val_run(strategy = s, show_cost_analysis = TRUE, test1_cost = 1, test2_cost = 1)
        d <- r$summary_table$asDF
        expect_equal(d$combined_sens, e[["sens"]], tolerance = 1e-12, label = paste(s, "sens"))
        expect_equal(d$combined_spec, e[["spec"]], tolerance = 1e-12, label = paste(s, "spec"))
        expect_equal(d$combined_ppv, e[["ppv"]], tolerance = 1e-12, label = paste(s, "PPV"))
        expect_equal(d$combined_npv, e[["npv"]], tolerance = 1e-12, label = paste(s, "NPV"))
        expect_equal(d$nnt, e[["nns"]], label = paste(s, "NNS"))
        f2 <- val_row(r$population_flow_table$asDF, "after_test2")
        expect_equal(c(f2$true_pos, f2$false_pos), unname(e[c("tp", "fp")]), tolerance = 1e-12, label = paste(s, "flow"))
        expect_equal(val_row(r$cost_analysis_table$asDF, "test2")$number_tests, e[["n2"]],
                     tolerance = 1e-12, label = paste(s, "second tests"))
    }
})


test_that("VAL C10-C13: single-test PPV, NPV and likelihood ratios (hand calculation)", {
    it <- val_run()$individual_tests_table$asDF
    t1 <- val_row(it, "test1"); t2 <- val_row(it, "test2")
    expect_equal(c(t1$ppv, t1$npv, t1$plr, t1$nlr), c(.095 / .365, .63 / .635, .95 / .30, .05 / .70), tolerance = 1e-12)
    expect_equal(c(t2$ppv, t2$npv, t2$plr, t2$nlr), c(.08 / .098, .882 / .902, 40, .2 / .98), tolerance = 1e-12)
})


test_that("VAL C04-C07 C15-C16: parity with epiR::rsu.dxtest and epiR::epi.tests", {
    skip_if_not_installed("epiR")
    for (s in c("serial_positive", "serial_negative", "parallel")) {
        d <- val_run(strategy = s, test1_sens = .62, test1_spec = .91, test2_sens = .87, test2_spec = .73)$summary_table$asDF
        rs <- epiR::rsu.dxtest(se = matrix(c(.62, .87), ncol = 1), sp = matrix(c(.91, .73), ncol = 1),
                               covar.pos = 0, covar.neg = 0, tconf.int = 0.95,
                               interpretation = if (s == "serial_positive") "series" else "parallel")$independent
        expect_equal(c(d$combined_sens, d$combined_spec), rs$est, tolerance = 1e-12, label = s)
    }
    # expected 2x2 of the serial-positive rule at N = 1e7 (exact integer cells)
    ep <- epiR::epi.tests(as.table(matrix(c(760000, 54000, 240000, 8946000), 2, byrow = TRUE)))$detail
    r <- val_run()
    d <- r$summary_table$asDF; cb <- val_row(r$individual_tests_table$asDF, "combined")
    expect_equal(c(d$combined_ppv, d$combined_npv, cb$plr, cb$nlr),
                 ep$est[match(c("pv.pos", "pv.neg", "lr.pos", "lr.neg"), ep$statistic)], tolerance = 1e-10)
})


test_that("VAL C04-C22: every table cell equals the cell-enumeration oracle on a random grid", {
    set.seed(20261008)
    for (i in 1:12) {
        v <- round(runif(4, .01, .99), 3); P <- round(runif(1, .001, .999), 4); N <- sample(100:100000, 1)
        for (s in c("serial_positive", "serial_negative", "parallel")) {
            r <- val_run(test1_sens = v[1], test1_spec = v[2], test2_sens = v[3], test2_spec = v[4],
                         prevalence = P, population_size = N, strategy = s, show_cost_analysis = TRUE,
                         test1_cost = 3, test2_cost = 7)
            o <- val_oracle(v[1], v[2], v[3], v[4], P, s, N = N)
            d <- r$summary_table$asDF; it <- r$individual_tests_table$asDF
            f2 <- val_row(r$population_flow_table$asDF, "after_test2")
            lab <- paste(s, paste(c(v, P), collapse = "/"))
            expect_equal(c(d$combined_sens, d$combined_spec, d$combined_ppv, d$combined_npv),
                         c(o$sens, o$spec, o$ppv, o$npv), tolerance = 1e-9, label = lab)
            expect_equal(c(val_row(it, "test1")$ppv, val_row(it, "test1")$npv, val_row(it, "test2")$ppv, val_row(it, "test2")$npv),
                         c(o$t1_ppv, o$t1_npv, o$t2_ppv, o$t2_npv), tolerance = 1e-9, label = lab)
            expect_equal(unlist(f2[c("true_pos", "false_pos", "false_neg", "true_neg")], use.names = FALSE), o$f,
                         tolerance = 1e-9, label = lab)
            expect_equal(val_row(r$cost_analysis_table$asDF, "total")$total_cost, 3 * N + 7 * o$n_t2,
                         tolerance = 1e-9, label = lab)
        }
    }
})


test_that("VAL MR2: relabelling disease and positivity maps serial-positive onto parallel", {
    # serial_positive(Se, Sp, P) == parallel(Sp, Se, 1 - P) with sens <-> spec, PPV <-> NPV, LR+ <-> 1/LR-
    a <- val_run(strategy = "serial_positive", test1_sens = .62, test1_spec = .91, test2_sens = .87, test2_spec = .73, prevalence = .27)
    b <- val_run(strategy = "parallel", test1_sens = .91, test1_spec = .62, test2_sens = .73, test2_spec = .87, prevalence = .73)
    da <- a$summary_table$asDF; db <- b$summary_table$asDF
    expect_equal(c(da$combined_sens, da$combined_spec, da$combined_ppv), c(db$combined_spec, db$combined_sens, db$combined_npv), tolerance = 1e-12)
    expect_equal(val_row(a$individual_tests_table$asDF, "combined")$plr,
                 1 / val_row(b$individual_tests_table$asDF, "combined")$nlr, tolerance = 1e-12)
})


test_that("VAL MR6 G: sequential Bayes and the flow table reproduce the summary", {
    odds <- function(p) p / (1 - p); prob <- function(o) o / (1 + o)
    r <- val_run(strategy = "serial_positive", test1_sens = .62, test1_spec = .91, test2_sens = .87, test2_spec = .73,
                 prevalence = .27, population_size = 2500)
    it <- r$individual_tests_table$asDF; d <- r$summary_table$asDF
    expect_equal(prob(odds(val_row(it, "test1")$ppv) * val_row(it, "test2")$plr), d$combined_ppv, tolerance = 1e-12)
    f2 <- val_row(r$population_flow_table$asDF, "after_test2")
    expect_equal(c(f2$true_pos / f2$disease_pos, f2$true_neg / f2$disease_neg, f2$true_pos / f2$test_pos, f2$true_neg / f2$test_neg),
                 c(d$combined_sens, d$combined_spec, d$combined_ppv, d$combined_npv), tolerance = 1e-12)
    expect_equal(d$nnt, ceiling(2500 / f2$true_pos))
})


test_that("VAL C26: the stated dependence directions hold under positive conditional covariance", {
    # Gardner et al. 2000 (PMID 10802336): serial-positive Se = Se1Se2 + covD, Sp = 1-(1-Sp1)(1-Sp2) - covN;
    # parallel Se = 1-(1-Se1)(1-Se2) - covD, Sp = Sp1Sp2 + covN. The analysis shows the covD = covN = 0 values.
    for (s in c("serial_positive", "parallel")) {
        d <- val_run(strategy = s, test1_sens = .80, test1_spec = .85, test2_sens = .75, test2_spec = .90)$summary_table$asDF
        truth <- val_oracle(.80, .85, .75, .90, .1, s, covD = .04, covN = .03)
        if (s == "serial_positive") {
            expect_lt(d$combined_sens, truth$sens)    # "combined sensitivity too low"
            expect_gt(d$combined_spec, truth$spec)    # "combined specificity too high"
        } else {
            expect_gt(d$combined_sens, truth$sens)    # "combined sensitivity too high"
            expect_lt(d$combined_spec, truth$spec)    # "combined specificity too low"
        }
    }
    skip_if_not_installed("epiR")
    rs <- epiR::rsu.dxtest(se = matrix(c(.80, .75), ncol = 1), sp = matrix(c(.85, .90), ncol = 1),
                           covar.pos = .04, covar.neg = .03, tconf.int = 0.95, interpretation = "series")$dependent
    truth <- val_oracle(.80, .85, .75, .90, .1, "serial_positive", covD = .04, covN = .03)
    expect_equal(rs$est, c(truth$sens, truth$spec), tolerance = 1e-12)
})


test_that("VAL C51-C52: probability paths, caption and PPV/NPV curves are the right posteriors", {
    skip_if_not_installed("ggplot2")
    for (s in c("serial_positive", "serial_negative", "parallel")) {
        r <- val_run(strategy = s, show_plots = TRUE, test1_sens = .62, test1_spec = .91, test2_sens = .87,
                     test2_spec = .73, prevalence = .27)
        o <- val_oracle(.62, .91, .87, .73, .27, s)
        r$plot_probability$saveAs(tempfile(fileext = ".png"))
        p <- ggplot2::last_plot()
        expv <- switch(s,
            serial_positive = c(.27, o$pd_t1pos, o$ppv, .27, o$pd_t1neg),
            serial_negative = c(.27, o$pd_t1pos, .27, o$pd_t1neg, o$pd_nn),
            parallel        = c(.27, o$ppv, .27, 1 - o$npv))
        expect_equal(p$data$Probability, 100 * expv, tolerance = 1e-10, label = s)
        if (s != "parallel") {
            cap <- gsub("\n", " ", p$labels$caption)
            nums <- as.numeric(regmatches(cap, gregexpr("[0-9]+\\.[0-9]+(?=%)", cap, perl = TRUE))[[1]])
            want <- if (s == "serial_positive") c(o$pd_t1neg, 1 - o$npv, o$pd_pn) else c(o$pd_t1pos, o$ppv, o$pd_np)
            expect_equal(nums, round(100 * want, 1), label = paste(s, "caption"))
        }
        r$plot_sensitivity_analysis$saveAs(tempfile(fileext = ".png"))
        d <- ggplot2::last_plot()$data; dd <- d[grepl("^PPV", d$Metric), ]
        cs <- o$sens; cp <- o$spec
        expect_equal(dd$Value, dd$Prevalence * cs / (dd$Prevalence * cs + (1 - dd$Prevalence) * (1 - cp)), tolerance = 1e-10)
    }
})


test_that("VAL C39-C45: guard thresholds fire on the side the code states", {
    fires <- function(title, ...) grepl(title, val_run(...)$notices$content, fixed = TRUE)
    expect_equal(c(fires("Prevalence Very Low", prevalence = .009), fires("Prevalence Very Low", prevalence = .01),
                   fires("Prevalence Very Low", prevalence = .011)), c(TRUE, TRUE, FALSE))
    expect_equal(c(fires("STRONG WARNING: Prevalence Low", prevalence = .049),
                   fires("STRONG WARNING: Prevalence Low", prevalence = .05)), c(TRUE, FALSE))
    expect_equal(c(fires("Prevalence Very High", prevalence = .899), fires("Prevalence Very High", prevalence = .9)), c(FALSE, TRUE))
    expect_equal(c(fires("Test 1 Low Performance", test1_sens = .49), fires("Test 1 Low Performance", test1_sens = .5)), c(TRUE, FALSE))
    expect_equal(c(fires("Illustrative Population Too Small", prevalence = .099, population_size = 100),
                   fires("Illustrative Population Too Small", prevalence = .1, population_size = 100)), c(TRUE, FALSE))
    expect_error(val_run(test1_sens = 1), "between 0.01 and 0.99")
    expect_error(val_run(prevalence = 0), "between 0.001 and 0.999")
})


# ---- defects confirmed 2026-10-08 and fixed the same day: these lock the fixes in -----------

test_that("VAL-sequentialtests-01: 'Number Needed to Screen' is defined beside the table", {
    # Rembold 1998 BMJ 317:307 (PMID 9685274): NNS = number screened to prevent one death/adverse event.
    note <- val_notes(val_run()$summary_table)[["nns_scope"]]
    expect_match(note, "per true-positive case detected", fixed = TRUE)
    expect_match(note, "1 / (prevalence", fixed = TRUE)
    expect_match(note, "not the screening-benefit statistic", fixed = TRUE)
})

test_that("VAL-sequentialtests-02: NNS is the exact ceiling at integer boundaries", {
    # P = .625, Se = .2 + .75 - .15 = .8 exactly -> 1 / .5 = 2
    expect_equal(val_run(prevalence = .625, test1_sens = .2, test2_sens = .75, strategy = "parallel")$summary_table$asDF$nnt, 2)
    expect_equal(val_run(prevalence = .125, test1_sens = .15, test2_sens = .2, strategy = "parallel")$summary_table$asDF$nnt, 25)
})

test_that("VAL-sequentialtests-03: low-prevalence notice does not overstate the PPV sensitivity to specificity", {
    n <- val_run(prevalence = .02)$notices$content
    expect_false(grepl("tens of points", n, fixed = TRUE))
    # the bound it now states must be reachable: 1 point of specificity at P = .0141, Se .99
    claimed <- as.numeric(sub(".*by more than ([0-9]+) percentage points.*", "\\1", n))
    ppv <- function(sp) val_row(val_run(prevalence = .0141, test1_sens = .99, test1_spec = sp)$individual_tests_table$asDF, "test1")$ppv
    expect_gt(100 * (ppv(.99) - ppv(.98)), claimed)
})

test_that("VAL-sequentialtests-04: dependence direction is qualified to dependence in both disease groups", {
    expect_match(val_notes(val_run()$summary_table)[["independence_warning"]], "in both disease-status groups", fixed = TRUE)
    expect_match(val_run()$notices$content, "in both disease-status groups", fixed = TRUE)
    expect_match(val_run(show_explanation = TRUE)$plain_summary$content, "in both disease-status groups", fixed = TRUE)
})

test_that("VAL-sequentialtests-05: parallel plain summary does not describe a sequence", {
    expect_false(grepl("followed by", val_run(strategy = "parallel", show_explanation = TRUE)$plain_summary$content, fixed = TRUE))
    expect_match(val_run(strategy = "parallel", show_explanation = TRUE)$plain_summary$content,
                 "Screening Test and Confirmatory Test applied together", fixed = TRUE)
    expect_match(val_run(strategy = "serial_negative", show_explanation = TRUE)$plain_summary$content,
                 "Screening Test followed by Confirmatory Test", fixed = TRUE)
})

test_that("VAL-sequentialtests-06: one-letter test names are not reported as similar", {
    expect_false(grepl("Test Correlation Risk", val_run(test1_name = "A", test2_name = "B")$notices$content, fixed = TRUE))
    # still fires on identical names, on containment and on a one-character variant of a longer name
    for (pr in list(c("PCR", "PCR"), c("PCR", "RT-PCR"), c("CA-125", "CA 125")))
        expect_match(val_run(test1_name = pr[1], test2_name = pr[2])$notices$content, "Test Correlation Risk",
                     fixed = TRUE, info = paste(pr, collapse = " / "))
})

test_that("VAL-sequentialtests-07: population size 100000 is printed in full", {
    expect_equal(val_run(show_cost_analysis = TRUE, population_size = 100000)$cost_analysis_table$title,
                 "Cost Analysis (Per 100,000 People)")
})

test_that("VAL-sequentialtests-08: Strategy Notes are populated only when shown", {
    expect_false(nzchar(val_run(show_explanation = FALSE)$clinical_guidance$content))
})
