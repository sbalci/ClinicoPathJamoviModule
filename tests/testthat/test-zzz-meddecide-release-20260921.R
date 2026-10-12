# Regression guards for the defects found by the 2026-09-21 meddecide release check.
# Each block fails if its specific defect comes back. Source-level where the defect is a
# source pattern, numeric where the defect was an arithmetic one.

ROOT <- normalizePath("../..")
read_b <- function(fn) readLines(file.path(ROOT, "R", paste0(fn, ".b.R")), warn = FALSE)
MEDDECIDE <- c("agreement", "cotest", "decision", "decisioncalculator", "decisioncombine",
               "decisioncompare", "decisioncurve", "enhancedROC", "kappaSizeCI",
               "kappaSizeFixedN", "kappaSizePower", "lassologistic", "nogoldstandard",
               "psychopdaROC", "sequentialtests")


test_that("Output columns pair setValues() with setRowNums(<row identities>)", {
    # lassologistic had setValues() with NO setRowNums at all; agreement had
    # setRowNums(1:n_cases), which is positional and mis-assigns every row under a filter.
    for (fn in MEDDECIDE) {
        src <- read_b(fn)
        code <- src[!grepl("^\\s*#", src)]
        setv <- grep("\\$setValues\\(", code)
        if (!length(setv)) next
        expect_true(any(grepl("\\$setRowNums\\(", code)),
                    info = paste(fn, "calls setValues() but never setRowNums()"))
        expect_false(any(grepl("\\$setRowNums\\(\\s*1:", code)),
                     info = paste(fn, "uses positional setRowNums(1:n); pass rownames(<data>)"))
    }
})


test_that("ICC sample size follows Walter, Eliasziw & Donner (1998), not a Wald form", {
    # The shipped formula was a Wald sample size carrying Walter's citation; it returned
    # n = 9 where Walter gives n = 23 (rho0 = .70, rho1 = .90, k = 2, two-sided, 80% power).
    walter <- function(rho0, rho1, k, alpha = 0.05, power = 0.80) {
        za <- qnorm(1 - alpha / 2); zb <- qnorm(power)
        cf <- function(r) 1 + k * r / (1 - r)
        ceiling(1 + 2 * k * (za + zb)^2 / ((k - 1) * (log(cf(rho1) / cf(rho0)))^2))
    }
    expect_equal(walter(0.60, 0.80, 2), 49)
    expect_equal(walter(0.70, 0.90, 2), 23)

    src <- read_b("agreement")
    expect_true(any(grepl("c_of <- function(rho) 1 + k * rho / (1 - rho)", src, fixed = TRUE)),
                info = "agreement no longer uses Walter's log transform for the ICC sample size")
    expect_false(any(grepl("2 * (z_alpha + z_beta)^2 * (1 - rho1)^2", src, fixed = TRUE)),
                 info = "the Wald ICC sample-size formula is back")
})


test_that("mean correlation across rater pairs is the plain arithmetic mean", {
    # Superseded 2026-09-25 (release review): the Fisher-z mean this test used to demand
    # fell back to the arithmetic mean only when a pair reached |r| = 1, so one adjacent
    # swap moved (0.9999, 0.608, 0.605) from 0.972 to (1, 0.608, 0.605) = 0.739. The plain
    # mean has no such jump, and for Spearman without ties it equals (mW - 1) / (m - 1).
    fz <- function(r) tanh(mean(atanh(pmin(pmax(r, -0.999999), 0.999999))))
    expect_gt(fz(c(0.9999, 0.608, 0.605)) - mean(c(1, 0.608, 0.605)), 0.2)   # the jump

    src <- read_b("agreement")
    code <- src[!grepl("^\\s*#", src)]
    expect_length(grep("tanh(mean(atanh(", code, fixed = TRUE), 0)
    expect_true(any(grepl("^\\s*mean_r <- mean\\(pairwise_r\\)", src)))
    expect_true(any(grepl("^\\s*mean_rho <- mean\\(pairwise_rho\\)", src)))
})


test_that("psychopdaROC dichotomises one-vs-rest instead of dropping the other levels", {
    src <- read_b("psychopdaROC")
    expect_true(any(grepl(".ovrResponse = function(class, positiveClass)", src, fixed = TRUE)))
    # the old form kept only ONE of the non-positive levels, so pROC silently dropped the
    # rest. Comments are stripped first: the helper documents the old form verbatim.
    code <- src[!grepl("^\\s*#", src)]
    expect_false(any(grepl("levels = c(setdiff(", code, fixed = TRUE)),
                 info = "a setdiff()-based levels= is back; it is not one-vs-rest")

    skip_if_not_installed("pROC")
    ovr <- function(class, pos) {
        other <- ".__rest__"; cls <- as.character(class)
        factor(ifelse(is.na(cls), NA_character_, ifelse(cls == pos, pos, other)),
               levels = c(other, pos))
    }
    set.seed(1)
    class <- factor(rep(c("Benign", "Borderline", "Malignant"), each = 30))
    x <- c(rnorm(30, 2), rnorm(30, 4), rnorm(30, 6))
    r <- pROC::roc(response = ovr(class, "Malignant"), predictor = x,
                   direction = "<", quiet = TRUE)
    expect_equal(length(r$cases) + length(r$controls), 90)   # was 60: Borderline was dropped
})


test_that("getCell() values are unwrapped with $value", {
    # getCell() returns a jmvcore Cell R6 object; round() on it raises
    # "non-numeric argument to mathematical function".
    for (fn in MEDDECIDE) {
        code <- read_b(fn)
        code <- code[!grepl("^\\s*#", code)]
        bad <- grep("\\$getCell\\([^)]*\\)\\s*(,|$|\\))", code, value = TRUE)
        bad <- bad[!grepl("\\$getCell\\([^)]*\\)\\$value", bad)]
        expect_length(bad, 0)
    }
})


test_that("no fabricated constant stands in for an undefined statistic", {
    src <- read_b("psychopdaROC")
    expect_false(any(grepl("bootstrap_aucs[i] <- 0.5", src, fixed = TRUE)),
                 info = "a degenerate resample is being scored as chance (0.5) again")
    ag <- read_b("agreement")
    expect_false(any(grepl("conc_prec <- if ((conc_tp + conc_fp) > 0) conc_tp / (conc_tp + conc_fp) else 0",
                           ag, fixed = TRUE)),
                 info = "undefined precision is being displayed as 0 again")
})


# library-audit 2026-09-16 meddecide [LOW] DONE: a .() msgid is a whole translatable unit -
#   leading/trailing spaces, commas and colons are assembled outside it, never inside.
test_that("no .() msgid carries a braced \\u{XXXX} escape or stray padding", {
    # A braced escape is resolved by R at parse time but stored literally in the catalog,
    # so the lookup can never match and the string is permanently untranslatable.
    for (fn in MEDDECIDE) {
        src <- read_b(fn)
        src <- src[!grepl("^\\s*#", src)]
        expect_length(grep('\\.\\(\\s*"[^"]*\\\\u\\{', src), 0)
        expect_length(grep('\\.\\(\\s*"(?:[\\s,;:])', src, perl = TRUE), 0)
        expect_length(grep('\\.\\(\\s*"[^"\n]*\\s"\\s*[,)]', src), 0)
        # " [..]" ANYWHERE in a msgid, not only at the end. jmvcore's Translator
        # splits on " \\[(.*)\\]" and, in any language with no catalog entry, drops
        # everything from " [" onward, so interval bounds vanish. An end-anchored
        # check missed a live blocker in enhancedROC on 2026-09-21.
        expect_length(grep('\\.\\(\\s*"(?:[^"\\\\]|\\\\.)* \\[[^"]*\\]', src, perl = TRUE), 0)
    }
})


test_that("notice renderers stay theme-safe", {
    # An opaque pastel fill plus an inherited (light) body colour is white-on-white on
    # jamovi's dark theme; a fixed title hue falls to ~2.7:1 there.
    for (fn in MEDDECIDE) {
        src <- read_b(fn)
        expect_length(grep("bgcolor = \"#", src, fixed = TRUE), 0)
        expect_length(grep("<strong style='color: \", style$color", src, fixed = TRUE), 0)
    }
})


test_that("tr.po keeps every sprintf conversion of its msgid", {
    po <- file.path(ROOT, "jamovi", "i18n", "tr.po")
    skip_if_not(file.exists(po))
    txt <- readLines(po, warn = FALSE, encoding = "UTF-8")
    fmt <- "%(?:[0-9]+\\$)?[-+0#]*[0-9]*(?:\\.[0-9]+)?[sdifeEgGxX]"
    join <- function(i, key) {
        out <- sub(paste0("^", key, "\\s+\""), "", txt[i]); out <- sub("\"\\s*$", "", out)
        j <- i + 1
        while (j <= length(txt) && grepl("^\\s*\"", txt[j])) {
            out <- paste0(out, sub("\"\\s*$", "", sub("^\\s*\"", "", txt[j]))); j <- j + 1
        }
        out
    }
    ids <- grep("^msgid\\s+\"", txt)
    offenders <- character(0)
    for (i in ids) {
        mid <- join(i, "msgid")
        k <- grep("^msgstr\\s+\"", txt[seq(i, min(i + 40, length(txt)))])
        if (!length(k)) next
        mstr <- join(i + k[1] - 1, "msgstr")
        if (!nzchar(mstr) || !grepl(fmt, mid)) next
        a <- regmatches(mid, gregexpr(fmt, mid))[[1]]
        b <- regmatches(mstr, gregexpr(fmt, mstr))[[1]]
        if (length(a) != length(b)) next                       # a different defect class
        # positional markers make order irrelevant; without them the sequence must match
        if (!any(grepl("\\$", b)) && !identical(a, b)) offenders <- c(offenders, substr(mid, 1, 60))
    }
    known_other_modules <- 11L   # chisqposttest, venn, jsurvival landmark, ... not meddecide
    expect_lte(length(offenders), known_other_modules)
    expect_false(any(grepl("ROC analysis was performed", offenders)))
    expect_false(any(grepl("Penalized logistic regression completed", offenders)))
})


# ---------------------------------------------------------------------------
# Guards for the four CRITICALs found by the 2026-09-21 deep module audit.
# ---------------------------------------------------------------------------

test_that("weighted kappa is computed on the declared ordinal scale", {
    # Two defects, one row. (1) Each rater column was converted with as.integer()
    # INDEPENDENTLY, so a label got a different code per column whenever the level
    # sets differed. (2) irr::kappa2 then sorted those codes as CHARACTERS, so a
    # >=10-category scale ran 1,10,11,12,2,3,... The CI beside the estimate came
    # from vcd via the factor levels, so the row printed a point estimate outside
    # its own interval.
    skip_if_not_installed("irr"); skip_if_not_installed("vcd")
    src <- readLines(file.path(ROOT, "R", "agreement.b.R"), warn = FALSE)
    expect_true(any(grepl(".kappa2Ordered = function", src, fixed = TRUE)))
    expect_true(any(grepl(".orderedLevelsInfo = function", src, fixed = TRUE)))
    expect_false(any(grepl("as.data.frame(lapply(ratings_k, function(x)", src, fixed = TRUE)),
                 info = "per-column as.integer() recoding is back")

    ordered_levels <- function(sub) {
        l1 <- levels(as.factor(sub[[1]])); l2 <- levels(as.factor(sub[[2]]))
        if (length(l2) > length(l1)) { t <- l1; l1 <- l2; l2 <- t }
        union(l1, l2)
    }
    k_new <- function(sub, w) {
        lv <- ordered_levels(sub)
        sub <- as.data.frame(lapply(sub, function(x) {
            m <- match(as.character(x), lv); o <- rep(NA_character_, length(m))
            o[!is.na(m)] <- sprintf("%04d", m[!is.na(m)]); o
        }), stringsAsFactors = FALSE)
        irr::kappa2(sub, weight = w)$value
    }
    k_truth <- function(sub, w) {
        lv <- ordered_levels(sub)
        tab <- table(factor(as.character(sub[[1]]), levels = lv),
                     factor(as.character(sub[[2]]), levels = lv))
        keep <- rowSums(tab) + colSums(tab) > 0
        unname(vcd::Kappa(tab[keep, keep, drop = FALSE],
               weights = if (w == "squared") "Fleiss-Cohen" else "Equal-Spacing")$Weighted["value"])
    }

    # (1) rater B never assigns the middle grade -> his column has 2 levels
    lv <- c("G1", "G2", "G3"); set.seed(7); n <- 80
    a <- factor(sample(lv, n, TRUE, c(.35, .30, .35)), levels = lv)
    bc <- ifelse(as.character(a) == "G2", sample(c("G1", "G3"), n, TRUE), as.character(a))
    bc[sample(n, 8)] <- sample(c("G1", "G3"), 8, TRUE)
    d1 <- data.frame(a = a, b = factor(bc))
    expect_equal(k_new(d1, "squared"), k_truth(d1, "squared"))
    expect_equal(round(k_new(d1, "squared"), 4), 0.7059)   # the old path gave 0.4706

    # (2) 12-category ordinal scale -> lexical order 1,10,11,12,2,...
    set.seed(21); n <- 150; lv12 <- as.character(1:12)
    x <- sample(1:12, n, TRUE); y <- pmin(12, pmax(1, x + sample(c(-1, 0, 0, 0, 1), n, TRUE)))
    d2 <- data.frame(A = factor(as.character(x), levels = lv12),
                     B = factor(as.character(y), levels = lv12))
    expect_equal(k_new(d2, "squared"), k_truth(d2, "squared"))
    expect_equal(round(k_new(d2, "squared"), 4), 0.9865)   # the old path gave 0.8632

    # unweighted kappa is order-invariant and must not have moved
    expect_equal(irr::kappa2(d2, "unweighted")$value, 0.6572, tolerance = 1e-3)
})


test_that("a failed marker does not contribute AUC 0 to the pooled estimate", {
    # `aucs[i] <- NA` inside `error = function(e)` assigns in the HANDLER frame, so
    # the outer pre-allocated vector kept its initial 0. Zero AUC with zero SE has
    # infinite inverse-variance weight, so the pooled AUC collapsed to ~0 while the
    # per-marker rows above it still read 0.85 / 0.79 / 0.81.
    src <- readLines(file.path(ROOT, "R", "psychopdaROC.b.R"), warn = FALSE)
    joined <- paste(src, collapse = "\n")
    expect_false(grepl("error = function(e) {\n              aucs[i] <- NA", joined, fixed = TRUE),
                 info = "handler-frame assignment is back")

    # demonstrate the mechanism so this guard cannot pass vacuously
    demo <- function(superassign) {
        aucs <- numeric(3)
        for (i in 1:3) tryCatch({
            if (i == 2) stop("fail")
            aucs[i] <- 0.8
        }, error = function(e) if (superassign) aucs[i] <<- NA_real_ else aucs[i] <- NA_real_)
        aucs
    }
    expect_equal(demo(FALSE)[2], 0)            # the defect
    expect_true(is.na(demo(TRUE)[2]))          # the shape a correct fix must have
})


test_that("criterion/prevalence/dot plots register against their own itemKeys", {
    # All three were added inside `if (!var %in% self$results$resultsTable$itemKeys)`.
    # Once a variable is registered in resultsTable on the first run that guard is
    # permanently false, so ticking one of those plots later drew nothing.
    src <- paste(readLines(file.path(ROOT, "R", "psychopdaROC.b.R"), warn = FALSE), collapse = "\n")
    for (p in c("criterionPlot", "prevalencePlot", "dotPlot")) {
        expect_true(grepl(paste0(p, "$itemKeys"), src, fixed = TRUE),
                    info = paste(p, "is not guarded on its own itemKeys"))
    }
})
