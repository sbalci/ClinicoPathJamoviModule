# Wiring of the "Pathologist Grading Analysis" section of agreement (.b.R layer only;
# the numbers are oracle-tested in test-agreement-latent-helpers.R). Checks what fills,
# what is refused, and that every refusal leaves an empty cell plus a note.

grading_run <- function(dat, vars, ...) {
    do.call(agreement, c(list(data = dat, vars = vars, gradingDesign = TRUE,
                              perPathologist = TRUE, severityModel = TRUE,
                              latentModel = TRUE), list(...)))
}

# Table$asDF row names are the deparsed row keys ("\"cases\""); strip the quotes.
by_key <- function(df) {
    rownames(df) <- gsub('"', "", rownames(df), fixed = TRUE)
    df
}

grading_sim <- function(I = 150, J = 6, seed = 20260924) {
    set.seed(seed)
    u <- stats::rnorm(I, 0, sqrt(1.5))
    v <- c(0.6, rep(0, J - 1))
    labs <- paste0("G", 1:5)
    d <- as.data.frame(lapply(seq_len(J), function(j)
        factor(as.character(cut(u + v[j] + stats::rnorm(I), c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf),
                                labels = labs)), levels = labs, ordered = TRUE)))
    names(d) <- paste0("P", seq_len(J))
    d
}

test_that("ordinal 150 x 6 x 5: every grading table fills", {
    skip_on_cran()
    d <- grading_sim()
    r <- grading_run(d, names(d))

    des <- by_key(r$designSummaryTable$asDF)
    expect_equal(des["cases", "value"], "150")
    expect_equal(des["connected", "value"], "Yes")
    expect_equal(des["model_scope", "value"], "Every model-based estimate, with confidence intervals.")

    dist <- r$categoryDistributionTable$asDF
    expect_equal(nrow(dist), 7L)                       # 6 pathologists + all
    expect_equal(dist$n[7], 900L)
    expect_equal(sum(unlist(dist[7, paste0("cat", 1:5, "_n")])), 900)

    pt <- r$pathologistTable$asDF
    expect_equal(pt$pathologist, names(d))             # vars order, not ranked
    expect_equal(sum(pt$shift), 0, tolerance = 1e-8)   # centred on the panel average
    expect_gt(pt$shift[1], 0)                          # P1 simulated +0.6: grades higher
    expect_equal(pt$tendency[1], "Grades higher than the panel average")
    expect_true(all(is.finite(pt$p_holm)))

    expect_equal(nrow(r$boundaryTable$asDF), 6L * 4L)  # 4 boundaries per pathologist

    lat <- by_key(r$latentModelTable$asDF)
    expect_true(all(is.finite(lat[c("rho", "kappa_m"), "ci_lower"])))
    expect_lt(lat["kappa_m", "estimate"], lat["rho", "estimate"])
    expect_equal(lat["kappa_m", "estimate"],
                 agreement_kappa_m(lat["rho", "estimate"], 5L), tolerance = 1e-8)
    expect_false(is.null(r$severityPlot$state))
    # The conservative label quotes the simulated region; the 95%-only and no-institution
    # caveats appear only when they apply (95% here, no cluster variable).
    ln <- agreement_notes(r$latentModelTable)
    expect_true(grepl("at least 100 cases and 3 pathologists", ln[["ci_conservative"]], fixed = TRUE))
    expect_false(any(c("ci_level", "ci_cluster") %in% names(ln)))
    expect_false(grepl("seconds", ln[["model"]], fixed = TRUE))   # no run-dependent timing
})

test_that("nominal ratings: model parts refused with a note, model-free tables fill", {
    skip_on_cran()
    data("agreement_hierarchical", package = "ClinicoPath", envir = environment())
    v <- grep("Rater", names(agreement_hierarchical), value = TRUE)
    r <- grading_run(agreement_hierarchical, v)
    expect_equal(nrow(r$boundaryTable$asDF), 6L * 4L)
    expect_true(all(is.finite(r$pathologistTable$asDF$pct_exact)))
    expect_true(all(is.na(r$pathologistTable$asDF$shift)))
    expect_true(all(is.na(r$latentModelTable$asDF$estimate)))
    expect_match(r$latentModelTable$notes$refuse_model$note, "nominal")
    expect_match(r$pathologistTable$notes$refuse_severity$note, "nominal")
    expect_null(r$severityPlot$state)
})

test_that("2 raters and binary with 4 raters: latent model refused with the design reason", {
    skip_on_cran()
    d <- grading_sim()
    r2 <- grading_run(d, c("P1", "P2"))
    expect_true(all(is.na(r2$latentModelTable$asDF$estimate)))
    expect_match(r2$latentModelTable$notes$refuse_model$note, "this study has 2")

    set.seed(7)
    u <- stats::rnorm(100, 0, 1.2)
    b <- as.data.frame(lapply(1:4, function(j) factor(ifelse(u + stats::rnorm(100) > 0.3, "pos", "neg"))))
    names(b) <- paste0("R", 1:4)
    r4 <- grading_run(b, names(b))
    expect_true(all(is.na(r4$latentModelTable$asDF$estimate)))
    expect_match(r4$latentModelTable$notes$refuse_model$note, "two categories")
    expect_match(r4$pathologistTable$notes$refuse_severity$note, "unordered")
})

# --- Review fixes, 2026-09-24 (second adversarial pass) ------------------------

test_that("grading guide quotes the gate constants, not stale figures", {
    d <- grading_sim(I = 20, J = 3)
    r <- agreement_run(d, vars = names(d), showGradingGuide = TRUE)$results
    html <- r$gradingGuide$content
    G <- AGREEMENT_LATENT_GATES
    expect_true(grepl(sprintf("Model-based kappa: at least %d pathologists", G$kappa_m_min_raters), html, fixed = TRUE))
    expect_true(grepl(sprintf("Latent ICC: %d cases, or %d when", G$rho_min_cases, G$rho_min_cases_incomplete), html, fixed = TRUE))
    expect_true(grepl(sprintf("Their confidence intervals: %d cases and %d pathologists",
                              G$rho_ci_min_cases, G$rho_ci_min_raters), html, fixed = TRUE))
    expect_true(grepl(sprintf("at least %d cases and %d pathologists", G$severity_min_cases, G$severity_min_raters),
                      html, fixed = TRUE))
    # The figures the old guide quoted and the gates never used.
    expect_false(grepl("from 30 cases", html, fixed = TRUE))
    expect_false(grepl("median of at least 6", html, fixed = TRUE))
})

test_that("incomplete design below its case limit: kappa_m shown, latent ICC refused with its own reason", {
    skip_on_cran()
    G <- AGREEMENT_LATENT_GATES
    d <- grading_sim(I = G$rho_min_cases_incomplete - 30L, J = 8)
    set.seed(7)
    for (i in seq_len(nrow(d))) d[i, sample(8, 5)] <- NA      # 3 of 8 read each case
    r <- agreement_run(d, vars = names(d), latentModel = TRUE)$results
    lat <- by_key(r$latentModelTable$asDF)
    expect_true(is.finite(lat["kappa_m", "estimate"]))
    expect_true(is.na(lat["rho", "estimate"]))
    notes <- agreement_notes(r$latentModelTable)
    expect_true(grepl("Not every pathologist read every case", notes[["refuse_rho"]], fixed = TRUE))
})

test_that("undetermined category order: boundary table has one row per category, not per boundary", {
    lv <- c("A", "B", "C")
    set.seed(3)
    x <- sample(lv, 60, TRUE)
    y <- ifelse(stats::runif(60) < 0.8, x, sample(lv, 60, TRUE))
    z <- ifelse(stats::runif(60) < 0.8, x, sample(lv, 60, TRUE))
    d <- data.frame(P1 = factor(x, lv, ordered = TRUE),
                    P2 = factor(y, rev(lv), ordered = TRUE),   # declares the reverse order
                    P3 = factor(z, lv, ordered = TRUE))
    r <- agreement_run(d, vars = names(d), perPathologist = TRUE, gradingDesign = TRUE)$results
    expect_equal(nrow(r$boundaryTable$asDF), 3L * 3L)
    expect_true(grepl("do not declare one common order", by_key(r$designSummaryTable$asDF)["scale", "value"], fixed = TRUE))
    expect_true(grepl("order is not determined", agreement_notes(r$boundaryTable)[["excess"]], fixed = TRUE))
})

test_that("single-read cases do not lift a small study over the case minimum (runtime review repro)", {
    skip_on_cran()
    G <- AGREEMENT_LATENT_GATES
    d <- grading_sim(I = 30 + 25, J = 4)
    for (i in 31:55) d[i, sample(4, 3)] <- NA                      # 25 cases read by one pathologist
    r <- agreement_run(d, vars = names(d), latentModel = TRUE, severityModel = TRUE)$results
    lat <- by_key(r$latentModelTable$asDF)
    expect_true(is.na(lat["kappa_m", "estimate"]))
    note <- agreement_notes(r$latentModelTable)[["refuse_model"]]
    expect_true(grepl("this study has 30", note, fixed = TRUE))
    expect_true(grepl("25 cases here were read by one", note, fixed = TRUE))
    expect_true(all(is.na(r$pathologistTable$asDF$shift)))
})

test_that("a large panel: grading tendency refused before fitting, with the runtime reason", {
    d <- grading_sim(I = 500, J = 20)                              # 10,000 ratings x 20 > ceiling
    t0 <- proc.time()[["elapsed"]]
    r <- agreement_run(d, vars = names(d), severityModel = TRUE)$results
    expect_lt(proc.time()[["elapsed"]] - t0, 60)                  # the 20 x 500 fit takes minutes
    expect_true(grepl("would take minutes to fit", agreement_notes(r$pathologistTable)[["refuse_severity"]], fixed = TRUE))
    expect_true(all(is.na(r$pathologistTable$asDF$shift)))
})
