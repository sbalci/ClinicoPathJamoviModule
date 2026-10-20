# Regression tests for the `/fix-function decisioncombine` pass (2026-10-01).
# One test_that() per claim; each description starts with its item ID (FX-n). Every FX test
# except FX-03 and FX-04 (guards against the new notice over-firing) fails on the pre-fix file.

# Patient-level data from pattern counts (names "+/+", "+/-", ...); factors as jamovi sends them.
.fx_mk <- function(cD, cN) {
    rows <- list()
    for (g in c("D", "N")) {
        cnt <- if (g == "D") cD else cN
        for (p in names(cnt)) {
            n <- cnt[[p]]
            if (n == 0) next
            s <- strsplit(p, "/", fixed = TRUE)[[1]]
            rows[[length(rows) + 1L]] <- data.frame(
                gold = g, t1 = if (s[1] == "+") "pos" else "neg",
                t2 = if (s[2] == "+") "pos" else "neg", stringsAsFactors = FALSE)[rep(1L, n), ]
        }
    }
    d <- do.call(rbind, rows)
    rownames(d) <- NULL
    d$gold <- factor(d$gold, levels = c("D", "N"))
    d$t1 <- factor(d$t1, levels = c("pos", "neg"))
    d$t2 <- factor(d$t2, levels = c("pos", "neg"))
    d
}

# Every Level argument passed explicitly (they are required arguments of the wrapper), through
# do.call(): the wrapper reads a bare symbol such as `test1 = test1` as a column NAME.
.fx_run <- function(d, gold = "gold", test1 = "t1", t2 = "t2") {
    suppressMessages(do.call(ClinicoPath::decisioncombine, list(
        data = d, gold = gold, goldPositive = if (is.null(gold)) NULL else "D",
        test1 = test1, test1Positive = if (is.null(test1)) NULL else "pos",
        test2 = t2, test2Positive = if (is.null(t2)) NULL else "pos",
        test3 = NULL, test3Positive = NULL)))
}

# Notices as rendered: severity word, title, body.
.fx_notices <- function(res) {
    h <- res$notices$content
    blocks <- regmatches(h, gregexpr("<strong>[^<]*</strong><br><span[^>]*>[^<]*</span>", h))[[1]]
    unescape <- function(x) {
        x <- gsub("&lt;", "<", x, fixed = TRUE)
        gsub("&amp;", "&", gsub("&gt;", ">", x, fixed = TRUE), fixed = TRUE)
    }
    heads <- unescape(sub("^<strong>([^<]*)</strong>.*$", "\\1", blocks))
    data.frame(severity = sub("^[^ ]* ", "", sub(": .*$", "", heads)),
               title = sub("^[^:]*: ", "", heads),
               content = unescape(sub("^.*<span[^>]*>([^<]*)</span>$", "\\1", blocks)),
               stringsAsFactors = FALSE)
}

DISCRIMINATION <- c("Poor Discrimination", "Discrimination Not Established",
                    "Discrimination May Be Poor")

# Test 2 positive only where Test 1 is ("-/+" empty); no disease-absent case is "+/+", a
# sampling zero.
FX_D <- c("+/+" = 40, "+/-" = 12, "-/+" = 0, "-/-" = 8)
FX_N <- c("+/+" = 0, "+/-" = 10, "-/+" = 0, "-/-" = 80)


test_that("FX-01 a test positive in every case is named, with what it does to the rules", {
    d <- .fx_mk(FX_D, FX_N)
    d$t1[] <- "pos"                                    # level "neg" still declared
    res <- .fx_run(d)
    nt <- .fx_notices(res)
    hit <- nt$title == "Test 1 Has Only Positive Results"
    expect_identical(nt$severity[hit], "Serious warning")
    expect_match(nt$content[hit], paste0(
        "Every recorded result of Test 1 (t1) is \"pos\", the level chosen as positive, so Test 1 ",
        "cannot separate disease-present from disease-absent cases in these data: on its own it ",
        "reads sensitivity 100% and specificity 0% by construction."), fixed = TRUE)
    expect_match(nt$content[hit], "Every combination that uses it either reduces to a rule on the other tests alone",
                 fixed = TRUE)
    # No number changes: Test 1 alone and Parallel call every case positive.
    ct <- as.data.frame(res$combinationTable$asDF)
    for (rule in c("Test 1 alone", "Parallel (>=1 pos)")) {
        expect_equal(c(ct$sens[ct$pattern == rule], ct$spec[ct$pattern == rule]), c(1, 0))
    }
    # Those two rows have no test-negative case: structural zeros, not sparse sampling (FX-05).
    sparse <- nt$content[nt$title == "Sparse Cell Counts"]
    expect_false(any(grepl("Test 1 alone", sparse, fixed = TRUE)))
    expect_false(any(grepl("Parallel", sparse, fixed = TRUE)))
})

test_that("FX-02 one test never positive: named, and no discrimination or sparse-cell notice", {
    # The positive level is declared but never occurs: the analysis ran with a serious "Poor
    # Discrimination" advising that "the likelihood ratios still show how far each result moves
    # the probability of disease" -- of a test with one result -- and "Sparse Cell Counts".
    d <- .fx_mk(FX_D, FX_N)
    d$t1[] <- "neg"
    nt <- .fx_notices(.fx_run(d, t2 = NULL))
    hit <- nt$title == "Test 1 Has No Positive Results"
    expect_identical(nt$severity[hit], "Serious warning")
    expect_match(nt$content[hit], paste0(
        "No recorded result of Test 1 (t1) is \"pos\", the level chosen as positive, so Test 1 cannot ",
        "separate disease-present from disease-absent cases in these data: on its own it reads ",
        "sensitivity 0% and specificity 100% by construction. Check the level chosen as positive ",
        "and any filter applied to the data."), fixed = TRUE)
    expect_false(grepl("Every combination", nt$content[hit], fixed = TRUE))   # one test
    expect_false(any(DISCRIMINATION %in% nt$title))
    expect_false("Sparse Cell Counts" %in% nt$title)
})

test_that("FX-03 a test left with one result by missing values is left to the missingness notice", {
    # Test 2 recorded only after a positive Test 1 (reflex testing): every complete case is Test
    # 1-positive, but Test 1 has both results where it was recorded. The missingness notice
    # names the cause; "check the level chosen as positive" would misname it.
    d <- .fx_mk(c("+/+" = 30, "+/-" = 10, "-/+" = 5, "-/-" = 15),
                c("+/+" = 5, "+/-" = 10, "-/+" = 10, "-/-" = 75))
    d$t2[d$t1 == "neg"] <- NA
    nt <- .fx_notices(.fx_run(d))
    expect_identical(nt$severity[nt$title == "Missing Results Follow Another Result"],
                     "Serious warning")
    expect_false(any(grepl("Positive Results", nt$title, fixed = TRUE)))
})

test_that("FX-04 with one reference outcome the one-outcome notice speaks alone", {
    d <- .fx_mk(FX_D, FX_N)
    d <- d[d$gold == "N", ]                             # level "D" still declared
    d$t1[] <- "pos"
    nt <- .fx_notices(.fx_run(d))
    expect_true("Gold Standard Has Only One Outcome" %in% nt$title)
    expect_false(any(grepl("Positive Results", nt$title, fixed = TRUE)))
})

test_that("FX-05 an empty pattern is not reported as sparse; a sampling zero still is", {
    res <- .fx_run(.fx_mk(FX_D, FX_N))
    ct <- as.data.frame(res$combinationTable$asDF)
    empty <- ct[ct$pattern == "-/+", ]
    expect_identical(c(empty$tp, empty$fp), c(0L, 0L))  # nobody shows "-/+"
    expect_true(is.na(empty$lrPos) && is.na(empty$dor) && empty$lrNeg == 1)
    # It used to head the list as "(smallest cell 0): +/+, -/+, ...". "+/+", Test 2 alone and
    # Serial have fp = 0 with every margin filled: corrected, and genuinely sparse.
    nt <- .fx_notices(res)
    expect_match(nt$content[nt$title == "Sparse Cell Counts"],
                 "below 5 (smallest cell 0): +/+, Test 2 alone, Serial (all pos).", fixed = TRUE)
})

test_that("FX-06 'Not Established' no longer says every interval includes 0", {
    # Codex's counterexample: the top rule, Test 2 alone (J 0.20), has an interval reaching 0.40,
    # no rule's interval is wholly above 0, but "+/-" (J -0.30) lies wholly BELOW it.
    res <- .fx_run(.fx_mk(c("+/+" = 12, "+/-" = 3, "-/+" = 6, "-/-" = 9),
                          c("+/+" = 9, "+/-" = 12, "-/+" = 3, "-/-" = 6)))
    ct <- as.data.frame(res$combinationTable$asDF)
    expect_lt(ct$youdenUpper[ct$pattern == "+/-"], 0)
    nt <- .fx_notices(res)
    body <- nt$content[nt$title == "Discrimination Not Established"]
    expect_length(body, 1L)
    expect_match(body, paste0("No rule scored here has a 95% confidence interval wholly above 0, so ",
                              "this sample does not show that any of them separates"), fixed = TRUE)
    expect_false(grepl("includes 0", body, fixed = TRUE))
})

test_that("FX-07 the collective inversion notice no longer says sensitivity and specificity swap", {
    # A reversed level reports 1 - sensitivity and 1 - specificity, not the two exchanged.
    res <- .fx_run(.fx_mk(c("+/+" = 10, "+/-" = 20, "-/+" = 20, "-/-" = 50),
                          c("+/+" = 10, "+/-" = 32, "-/+" = 32, "-/-" = 26)))
    nt <- .fx_notices(res)
    body <- nt$content[nt$title == "Positive Levels May Be Inverted"]
    expect_length(body, 1L)
    expect_match(body, paste0("the sensitivity and specificity reported for each rule then describe ",
                              "results read the wrong way round, and the best-performing rule is one ",
                              "no clinician would apply"), fixed = TRUE)
    expect_false(grepl("swapped", body, fixed = TRUE))
})

test_that("FX-08 a selection started with Test 2 says what is missing, as one started with Test 1 does", {
    d <- .fx_mk(FX_D, FX_N)
    for (start in list(list(test1 = "t1", t2 = NULL), list(test1 = NULL, t2 = "t2"))) {
        nt <- .fx_notices(.fx_run(d, gold = NULL, test1 = start$test1, t2 = start$t2))
        expect_identical(nt$severity[nt$title == "No Gold Standard"], "Note")
    }
})
