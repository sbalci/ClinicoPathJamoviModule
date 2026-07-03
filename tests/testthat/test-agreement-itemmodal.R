# Tests for the Per-Category Item-Modal Agreement table in `agreement`.
#
# For each rating category c, the table reports the mean within-case agreement
# rate across raters over the cases whose modal rating is c. Cases with no
# unique mode (e.g. a 2-2 split) are excluded.

testthat::skip_if_not_installed("irr")

test_that("all-identical ratings give mean agreement 1 for every category", {
    n <- 30
    set.seed(11)
    v <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
    df <- data.frame(R1 = v, R2 = v, R3 = v)
    res <- agreement(data = df, vars = names(df),
                     itemModalCategoryAgreement = TRUE)
    tab <- res$itemModalAgreementTable$asDF
    expect_true(nrow(tab) >= 1)
    expect_true(all(tab$mean_agreement == 1))
    expect_true(all(tab$ci_lower == 1 & tab$ci_upper == 1))
})

test_that("a single dominant modal category yields one row", {
    n <- 30
    df <- data.frame(
        R1 = rep("A", n),
        R2 = rep("A", n),
        R3 = c(rep("A", n - 2), "B", "B")  # modal is always A (2 of 3)
    )
    df[] <- lapply(df, factor, levels = c("A", "B"))
    res <- agreement(data = df, vars = names(df),
                     itemModalCategoryAgreement = TRUE)
    tab <- res$itemModalAgreementTable$asDF
    expect_equal(nrow(tab), 1L)
    expect_equal(as.character(tab$category[1]), "A")
    expect_equal(tab$n_cases[1], n)
    # 28 cases fully agree (1.0), 2 cases agree 2/3 -> mean below 1
    expect_lt(tab$mean_agreement[1], 1)
    expect_gt(tab$mean_agreement[1], 0.9)
})

test_that("2-2 tie cases are excluded from the item-modal table", {
    clearA <- data.frame(R1 = "A", R2 = "A", R3 = "A", R4 = "A")[rep(1, 12), ]
    ties   <- data.frame(R1 = "A", R2 = "A", R3 = "B", R4 = "B")[rep(1, 8), ]
    df <- rbind(clearA, ties)
    df[] <- lapply(df, factor, levels = c("A", "B"))
    rownames(df) <- NULL
    res <- agreement(data = df, vars = names(df),
                     itemModalCategoryAgreement = TRUE)
    tab <- res$itemModalAgreementTable$asDF
    # Only category A has a unique mode; the 8 ties are dropped.
    expect_equal(nrow(tab), 1L)
    expect_equal(as.character(tab$category[1]), "A")
    expect_equal(tab$n_cases[1], 12L)
    expect_equal(tab$mean_agreement[1], 1)
})

test_that("known mixed profile reproduces hand-computed per-category means", {
    # Category A modal cases: two 4/4 (agree 1) and two 3/4 (agree .75) -> mean .875
    # Category B modal cases: one 4/4 (agree 1) and one 3/4 (agree .75) -> mean .875
    rows <- list(
        c("A", "A", "A", "A"),
        c("A", "A", "A", "A"),
        c("A", "A", "A", "B"),
        c("A", "A", "A", "B"),
        c("B", "B", "B", "B"),
        c("B", "B", "B", "A")
    )
    df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(df) <- paste0("R", 1:4)
    df[] <- lapply(df, factor, levels = c("A", "B"))
    res <- agreement(data = df, vars = names(df),
                     itemModalCategoryAgreement = TRUE)
    tab <- res$itemModalAgreementTable$asDF
    a <- tab[as.character(tab$category) == "A", ]
    b <- tab[as.character(tab$category) == "B", ]
    expect_equal(a$n_cases, 4L)
    expect_equal(b$n_cases, 2L)
    expect_equal(a$mean_agreement, 0.875, tolerance = 1e-8)
    expect_equal(b$mean_agreement, 0.875, tolerance = 1e-8)
})
