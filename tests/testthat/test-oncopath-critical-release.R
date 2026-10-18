# Regressions for the October 2026 independent release review.
critical_notes <- function(x) paste(vapply(x$notes, function(n) n$note, ""), collapse = "\n")
critical_dm <- function(d, ...) diagnosticmeta(d, study = "study", true_positives = "tp",
    false_negatives = "fn", false_positives = "fp", true_negatives = "tn",
    covariate = "cov", meta_regression = TRUE, bivariate_analysis = FALSE,
    method = "fixed", ...)
critical_studies <- function() data.frame(study = paste0("S", 1:12),
    tp = rep(c(30, 50, 80), each = 4), fn = rep(c(70, 50, 20), each = 4),
    fp = 10:21, tn = 100 - (10:21), cov = factor(rep(c("A", "B", "C"), each = 4)))

test_that("diagnostic notes describe the retained reference of each fitted margin", {
    d <- critical_studies(); d$tp[1:4] <- 0; d$fn[1:4] <- 100
    r <- suppressWarnings(critical_dm(d))
    n <- critical_notes(r$metaregression)
    expect_match(n, "Sensitivity:.*reference level 'B'")
    expect_match(n, "Specificity:.*reference level 'A'")
    t <- r$metaregression$asDF
    expect_equal(t$estimate[t$measure == "Sensitivity" & t$parameter == "cov C"], log(4))
    d <- d[12:1, ]; r2 <- suppressWarnings(critical_dm(d))
    expect_equal(r2$metaregression$asDF, r$metaregression$asDF)
})

test_that("ordered and custom contrasts retain their model and report actual coding", {
    for (coding in c("ordered", "sum", "custom", "second-reference")) {
        d <- critical_studies()
        if (coding == "ordered") d$cov <- ordered(d$cov)
        if (coding == "sum") contrasts(d$cov) <- contr.sum(3)
        if (coding == "custom") contrasts(d$cov) <- cbind(c(1, -1, 0), c(1, 1, -2))
        if (coding == "second-reference") contrasts(d$cov) <- contr.treatment(3, base = 2)
        r <- suppressWarnings(critical_dm(d)); n <- critical_notes(r$metaregression)
        # Independent weighted normal equations on the exact R model matrix.
        X <- model.matrix(~ cov, d); w <- with(d, 1 / (1 / tp + 1 / fn))
        beta <- solve(crossprod(X, w * X), crossprod(X, w * with(d, log(tp / fn))))
        t <- r$metaregression$asDF
        expect_equal(t$estimate[t$measure == "Sensitivity"], as.numeric(beta), tolerance = 1e-10)
        if (coding == "second-reference") {
            expect_match(n, "reference level 'B'")
        } else {
            expect_match(n, "contrast coding, not level-versus-reference")
            expect_match(n, "Fitted design columns")
            expect_false(grepl("reference level '", n, fixed = TRUE))
        }
    }
})

test_that("swimmer rejects conflicting patient groups regardless of order and size", {
    for (count in c(8L, 600L)) {
        d <- data.frame(id = rep(seq_len(count), each = 2), start = rep(c(0, 2), count),
                        end = rep(c(1, 3), count), group = rep(c("A", "B"), count))
        for (idx in list(seq_len(nrow(d)), rev(seq_len(nrow(d))))) {
            expect_error(swimmerplot(d[idx, ], patientID = "id", startTime = "start",
                                    endTime = "end", groupVar = "group"),
                         "conflicting non-missing group values")
        }
    }
    d <- data.frame(id = rep(1:8, each = 2), start = rep(c(0, 2), 8),
                    end = rep(c(1, 3), 8), group = rep(c(NA, "A"), 8))
    expect_no_error(swimmerplot(d, patientID = "id", startTime = "start", endTime = "end", groupVar = "group"))
})

test_that("raw waterfall baseline calculation tolerates common user column names", {
    d <- data.frame(id = rep(letters[1:6], each = 2), time = rep(0:1, 6), burden = rep(c(100, 50), 6))
    run <- function(x, id = "id", response = "burden", time = "time") do.call(waterfall,
        list(data = x, patientID = id, timeVar = time, responseVar = response, inputType = "raw"))
    reference <- run(d)$summaryTable$asDF
    x <- d; names(x)[3] <- "baseline"
    expect_equal(run(x, response = "baseline")$summaryTable$asDF, reference)
    x <- d; x$baseline <- 999
    expect_equal(run(x)$summaryTable$asDF, reference)
    x <- d; names(x)[1] <- "baseline"
    expect_equal(run(x, id = "baseline")$summaryTable$asDF, reference)
    x <- d; names(x)[2] <- "baseline"
    expect_equal(run(x, time = "baseline")$summaryTable$asDF, reference)
})

test_that("unavailable diagnostic statistics use native empty cells", {
    d <- critical_studies()
    r <- diagnosticmeta(d, study = "study", true_positives = "tp", false_negatives = "fn",
                        false_positives = "fp", true_negatives = "tn", hsroc_analysis = TRUE)
    for (key in c("plr", "nlr", "dor")) {
        expect_null(r$bivariateresults$getRow(rowKey = key)$p_value$value)
    }
    for (key in c("taus_sq", "auc")) {
        expect_null(r$hsrocresults$getRow(rowKey = key)$z_value$value)
        expect_null(r$hsrocresults$getRow(rowKey = key)$p_value$value)
    }
})


test_that("unavailable IHC confidence limits use native empty cells", {
    d <- data.frame(a = seq(10, 90, length.out = 24))
    d$b <- d$a + rep(c(-1, 0, 1), 8)
    d$c <- d$a + rep(c(1, -1, 0), 8)
    r <- ihcheterogeneity(d, biopsy1 = "a", biopsy2 = "b", biopsy3 = "c")
    for (key in c(3, 4, 5)) {
        row <- r$reproducibilitytable$getRow(rowKey = key)
        expect_true(is.finite(row$value$value))
        expect_null(row$ci_lower$value)
        expect_null(row$ci_upper$value)
        expect_true(row$ci_lower$isEmpty)
    }
})
