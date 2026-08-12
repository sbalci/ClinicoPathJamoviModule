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
