# Residual-defect regression tests for jjcorrmat.
#
# One block per defect fixed in the residual-findings pass. Each asserts the
# USER-VISIBLE outcome (rendered panel text, table row count, the marker size
# ggcorrplot actually draws), not the internal mechanism.

library(testthat)

cmr_data <- function(n = 60, seed = 7) {
    set.seed(seed)
    d <- data.frame(a = rnorm(n))
    d$b <- 0.6 * d$a + rnorm(n, 0, 0.8)
    d$c <- -0.4 * d$a + rnorm(n, 0, 0.9)
    d
}

cmr_run <- function(data = cmr_data(), dep = c("a", "b", "c"), ...) {
    opts <- do.call(ClinicoPath:::jjcorrmatOptions$new,
                    utils::modifyList(list(dep = dep), list(...)))
    a <- ClinicoPath:::jjcorrmatClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}

# --- C1: the non-significance cross must not be drawn on top of the number ----
# ggstatsplot's own default is pch.cex = 14; the module passes its own
# ggcorrplot.args, which REPLACES that list, and ggcorrplot's formal is 5 - small
# enough to land on the decimal point ("0.02" rendered as "0X2").
test_that("ggcorrplot.args keeps ggstatsplot's pch.cex so the cross spans the cell", {
    skip_if_not_installed("ggstatsplot")
    a <- cmr_run()
    gargs <- a$.__enclos_env__$private$.prepareOptions()$ggcorrplot.args
    expect_equal(gargs$pch.cex, 14)

    p <- ggstatsplot::ggcorrmat(data = cmr_data(), cor.vars = c("a", "b", "c"),
                                pch = "cross", ggcorrplot.args = gargs)
    sizes <- vapply(p$layers, function(l) {
        if (identical(l$aes_params$shape, "cross")) l$aes_params$size else NA_real_
    }, numeric(1))
    expect_true(any(!is.na(sizes)))
    expect_equal(max(sizes, na.rm = TRUE), 14)
})

# --- C10: pairwise deletion must report the per-pair N, not nrow() ------------
test_that("pairwise deletion reports per-pair N and warns", {
    d <- cmr_data()
    d$b[1:30] <- NA
    a <- cmr_run(d, naHandling = "pairwise", showexplanations = TRUE)

    summary_txt <- a$results$summary$content
    expect_match(summary_txt, "per pair")
    # nrow(d) is 60; no correlation was computed on 60 rows.
    expect_false(grepl("60 observations<", summary_txt, fixed = TRUE))

    expect_match(a$results$warnings$content, "Pairwise deletion")

    tbl_n <- as.data.frame(a$results$table)$n
    expect_true(min(tbl_n) < nrow(d))
})

test_that("listwise deletion still reports a single N", {
    d <- cmr_data()
    d$b[1:30] <- NA
    a <- cmr_run(d, naHandling = "listwise", showexplanations = TRUE)
    expect_match(a$results$summary$content, "30 observations")
    expect_false(grepl("Pairwise deletion", a$results$warnings$content, fixed = TRUE))
})

# --- C9: a constant variable must be called out ------------------------------
test_that("a zero-variance variable produces a warning naming it", {
    d <- cmr_data()
    d$flat <- 1
    a <- cmr_run(d, dep = c("a", "b", "flat"), showexplanations = TRUE)
    expect_match(a$results$warnings$content, "flat")
    expect_match(a$results$warnings$content, "no variation")
    # The completion notice must count coefficients actually produced (1),
    # not table rows (3, two of which are all-NA).
    expect_match(a$results$warnings$content, "Computed 1 ")
})

# --- C9: the same variable selected twice ------------------------------------
test_that("a duplicated variable does not create a structural r = 1 row", {
    a <- cmr_run(dep = c("a", "a", "b"))
    tbl <- as.data.frame(a$results$table)
    expect_equal(nrow(tbl), 1L)
    expect_false(any(abs(tbl$r - 1) < 1e-9))
})

# --- C10: panels must name the method, not the option token ------------------
test_that("summary and assumptions print the method name, not the raw token", {
    for (ts in c("parametric", "nonparametric", "robust", "bayes")) {
        a <- cmr_run(typestatistics = ts, showexplanations = TRUE)
        s <- a$results$summary$content
        asm <- a$results$assumptions$content
        expect_false(grepl(paste0("<b>", ts, " correlation"), s, fixed = TRUE),
                     info = ts)
        expect_false(grepl(paste0("For ", ts, " correlation"), asm, fixed = TRUE),
                     info = ts)
    }

    a <- cmr_run(typestatistics = "robust", showexplanations = TRUE)
    expect_match(a$results$summary$content, "Winsorized Pearson correlation")
    expect_match(a$results$assumptions$content, "For Winsorized Pearson correlation")
    # Only the selected method's assumption bullet is shown.
    expect_false(grepl("Spearman", a$results$assumptions$content, fixed = TRUE))
})

# --- C12: the two plot methods were the only paths not resolving column names -
# .init, .populateTable, .generateInterpretation and .computeCorrelations all
# route column lookups through .resolveName (jamovi may hand back B64-encoded
# names); .plot and .plot2 indexed the prepared frame with the RAW option names.
# .plotFrame is the shared resolver, and it must also restore the user-facing
# names so the figure's labels match the table's Variable columns.
test_that(".plotFrame resolves column names and restores the user-facing ones", {
    p <- cmr_run()$.__enclos_env__$private
    fr <- p$.plotFrame(p$.prepareData(), p$.prepareOptions()$myvars)
    expect_identical(names(fr), c("a", "b", "c"))
    expect_equal(nrow(fr), nrow(cmr_data()))

    d <- cmr_data()
    d$g <- factor(rep(c("X", "Y"), length.out = nrow(d)))
    pg <- cmr_run(d, grvar = "g")$.__enclos_env__$private
    frg <- pg$.plotFrame(pg$.prepareData(), pg$.prepareOptions()$myvars, extra = "g")
    expect_identical(names(frg), c("a", "b", "c", "g"))
})

# --- C13: the interpretation panel is gated on showexplanations --------------
# It sits beside about/summary/assumptions, which are all visible:
# (showexplanations), but carried no gate of its own and was additionally
# regenerated by .plot and .plot2 - re-running the whole correlation engine,
# per group, on every render.
test_that("the clinical interpretation follows the showexplanations switch", {
    expect_false(nzchar(cmr_run()$results$interpretation$content))
    expect_true(nzchar(cmr_run(showexplanations = TRUE)$results$interpretation$content))
})

# --- C14: an empty correlation result must say so ----------------------------
# correlation::correlation does not error on a subset with no usable rows - it
# returns a full set of rows carrying n = 0, r = NA and the method string
# "NA correlation". Those were added to the table as blank junk rows, and
# nothing was written to the notices panel either way.
test_that("a group that yields no correlations is dropped and named in the notices", {
    d <- cmr_data()
    d$g <- factor(rep(c("X", "Y"), length.out = nrow(d)))
    d[d$g == "Y", c("a", "b", "c")] <- NA_real_
    a <- cmr_run(d, grvar = "g", naHandling = "pairwise")

    expect_match(a$results$warnings$content, "Y", fixed = TRUE)
    # only the three usable pairs from group X survive
    expect_equal(a$results$table$rowCount, 3L)
    groups <- vapply(a$results$table$asDF$group, as.character, character(1))
    expect_true(all(groups == "X"))
    expect_false(any(is.na(a$results$table$asDF$r)))
})

# --- C15: unadjusted multiplicity must be surfaced as a notice ---------------
# A correlation matrix runs one test per PAIR: 15 variables is 105 tests. On
# pure noise with no correction about six clear p < 0.05 and read as findings.
# Only a table footnote mentioned it.
test_that("running many uncorrected tests raises a strong warning", {
    set.seed(4)
    d <- as.data.frame(matrix(rnorm(40 * 15), 40))
    names(d) <- paste0("v", seq_len(15))
    a <- cmr_run(d, dep = names(d), padjustmethod = "none")
    txt <- gsub("<[^>]+>", "", a$results$warnings$content)

    expect_match(txt, "STRONG WARNING")
    expect_match(txt, "105 correlation tests", fixed = TRUE)
    expect_match(txt, "5.2", fixed = TRUE)          # 105 * 0.05 expected by chance

    # an applied correction, and the Bayesian branch, must stay quiet
    expect_false(grepl("expected to reach significance",
                       cmr_run(d, dep = names(d), padjustmethod = "holm")$results$warnings$content))
    # a single pair has no multiplicity to warn about
    expect_false(grepl("expected to reach significance",
                       cmr_run(dep = c("a", "b"), padjustmethod = "none")$results$warnings$content))
})

# --- C16: the notices panel is not left as an empty heading ------------------
# The <2-variable branch returned before .displayWarnings(), so the welcome
# message sat beside an empty "Warnings and Notices" heading.
test_that("the notices panel is hidden while the welcome message shows", {
    a <- cmr_run(dep = "a")
    expect_false(a$results$warnings$visible)
})

# --- C17: a Bayesian interval is a CREDIBLE interval -------------------------
# conf_low/conf_high sat under one static superTitle "Confidence interval" for
# every method. Under `bayes` those numbers are a credible interval from the
# posterior - a different claim - so the table now carries a second, separately
# titled column pair and shows only the one that applies.
test_that("Bayesian results report a credible interval, not a confidence interval", {
    # asDF returns only the VISIBLE columns, so the pairs are mutually exclusive:
    # that is precisely the gating under test.
    b <- cmr_run(typestatistics = "bayes")$results$table$asDF
    expect_true(all(c("cred_low", "cred_high") %in% names(b)))
    expect_false(any(c("conf_low", "conf_high") %in% names(b)))
    expect_false(any(is.na(b$cred_low)))

    f <- cmr_run()$results$table$asDF   # parametric: the mirror image
    expect_true(all(c("conf_low", "conf_high") %in% names(f)))
    expect_false(any(c("cred_low", "cred_high") %in% names(f)))

    a <- cmr_run()  # parametric
    expect_match(gsub("<[^>]+>", "", a$results$table$notes$padj$note), "correction")
    expect_match(gsub("<[^>]+>", "", cmr_run(typestatistics = "bayes")$results$table$notes$padj$note),
                 "credible interval")
})

# --- C18: the Bayesian seed is user-controllable -----------------------------
# The median posterior estimate and the credible interval come from an MCMC
# draw and genuinely move with the seed; BF10 is computed analytically by
# BayesFactor and is seed-invariant, so it is deliberately NOT asserted to move.
test_that("bayesseed changes the sampled estimate but not the analytic BF", {
    p <- cmr_run(typestatistics = "bayes", bayesseed = 777)$results$table$asDF
    q <- cmr_run(typestatistics = "bayes", bayesseed = 12345)$results$table$asDF
    expect_false(isTRUE(all.equal(p$r, q$r)))
    expect_false(isTRUE(all.equal(p$cred_low, q$cred_low)))
    expect_equal(p$bf, q$bf)

    # same seed twice must be identical
    r1 <- cmr_run(typestatistics = "bayes", bayesseed = 42)$results$table$asDF
    r2 <- cmr_run(typestatistics = "bayes", bayesseed = 42)$results$table$asDF
    expect_equal(r1$r, r2$r)
})

# --- C19: the interpretation phrases are whole, translatable sentences -------
# strength and direction used to be two independent one-word msgids ("strong",
# "positive") spliced by sprintf, which no translator can render and no
# inflecting language can agree.
test_that("the interpretation renders a single combined strength phrase", {
    txt <- gsub("<[^>]+>", "", cmr_run(showexplanations = TRUE)$results$interpretation$content)
    expect_match(txt, "strong positive|moderate positive|strong negative|moderate negative")
    expect_false(grepl("A  correlation", txt, fixed = TRUE))  # no empty splice
})
