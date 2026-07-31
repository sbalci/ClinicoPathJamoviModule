# Regression tests for the two defects in TODO-adjusted-cox-consistency.md.
#
# ISSUE 1 -- two models in one report. finalfit::finalfit() silently
# as.factor()s any numeric explanatory variable with fewer than cont_cut
# (default 5) distinct values and then fits its OWN coxph on the mutated frame.
# An ordinal score entered as continuous (performance status 0/1/2) therefore
# produced a factor fit in the main multivariable table and a linear-trend fit
# in "Adjusted Cox Model Results": two rows vs one, LR df 7 vs 6, and -- because
# the adjustment sets differed -- every SHARED coefficient moved as well.
# .final_fit2() now passes cont_cut = 0.
#
# ISSUE 2 -- the rows of summary(coxph)$coefficients are design-matrix columns,
# not variables, and were printed raw: "For stageIV ... 653.4 % increase in
# hazard for each unit increase in stageIV". .coefTerms() recovers variable /
# level / reference from $assign, $contrasts and $xlevels instead.

.msacc_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("multisurvival", envir = .cand, inherits = FALSE)) {
            .msacc_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.msacc_ns), "multisurvival not available in this distribution")

.msacc_quiet <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

# performance_status is the variable that triggered the report: an ordinal score
# delivered to R as a plain numeric with 3 distinct values, i.e. below finalfit's
# cont_cut. treatment's level text repeats its variable name ("Treatment A"),
# which is what makes prefix-stripping the coefficient string unsafe.
.msacc_data <- function() {
    set.seed(42); n <- 500
    stage <- factor(sample(c("I", "II", "III"), n, TRUE), levels = c("I", "II", "III"))
    grade <- factor(sample(c("Good", "Poor"), n, TRUE), levels = c("Good", "Poor"))
    treatment <- factor(sample(c("Control", "Treatment A"), n, TRUE),
                        levels = c("Control", "Treatment A"))
    performance_status <- sample(0:2, n, TRUE)
    age <- round(stats::rnorm(n, 60, 10), 1)
    lp <- 0.6 * (stage == "III") + 0.3 * (grade == "Poor") -
          0.3 * performance_status + 0.01 * (age - 60)
    data.frame(t = round(stats::rexp(n, 0.02 * exp(lp)), 1) + 0.1,
               ev = stats::rbinom(n, 1, 0.85),
               stage = stage, grade = grade, treatment = treatment,
               performance_status = performance_status, age = age)
}

.msacc_run <- function(d, ...) {
    .msacc_quiet(do.call(get("multisurvival", envir = .msacc_ns),
        c(list(data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
               explanatory = c("stage", "grade", "treatment"),
               contexpl = c("age", "performance_status"),
               dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
               ac = TRUE, adjexplanatory = "stage", ac_summary = TRUE),
          list(...))))
}

.msacc_text <- function(x) {
    if (is.null(x)) return("")
    gsub("<[^>]*>", "", gsub("<br\\s*/?>", "\n", x, perl = TRUE))
}

# The multivariable estimate is the LAST cell of each finalfit row; the
# univariable column has an identical format, so it cannot be matched by shape.
# (?s) is required -- knitr::kable puts a newline inside every <tr>.
.msacc_ff_multi <- function(res) {
    h <- res$text$content
    rows <- regmatches(h, gregexpr("(?s)<tr>.*?</tr>", h, perl = TRUE))[[1]]
    last <- vapply(rows, function(r) {
        tds <- regmatches(r, gregexpr("(?s)<td[^>]*>.*?</td>", r, perl = TRUE))[[1]]
        if (!length(tds)) return(NA_character_)
        trimws(gsub("<[^>]*>", "", tds[length(tds)]))
    }, character(1), USE.NAMES = FALSE)
    last[!is.na(last) & grepl("^[0-9.]+ \\([0-9.]+-[0-9.]+", last)]
}

.msacc_df <- function(txt, pattern) {
    m <- regmatches(txt, regexpr(pattern, txt, perl = TRUE))
    if (!length(m)) return(NA_integer_)
    as.integer(sub(pattern, "\\1", m, perl = TRUE))
}

# ---------------------------------------------------------------- ISSUE 1 ----

test_that("the main table and the Adjusted Cox table are the same model", {
    res <- .msacc_run(.msacc_data())

    adj <- as.data.frame(res$adjustedCoxTable)
    ff  <- .msacc_ff_multi(res)

    # Same covariate set: one adjusted row per non-reference finalfit estimate.
    # Before the fix this was 6 vs 7 (performance_status contributed 2 rows to
    # the finalfit fit and 1 here).
    expect_gt(nrow(adj), 0)
    expect_equal(length(ff), nrow(adj))

    # Same HRs and CIs, to the precision both tables print.
    expect_equal(sort(sub(", *p[&<=][^)]*", "", ff)), sort(adj$HR))

    # Same likelihood ratio df.
    df_main <- .msacc_df(.msacc_text(res$text2$content),
                         "Likelihood ratio test = [0-9.]+ \\(df = ([0-9]+)")
    df_adj  <- .msacc_df(.msacc_text(res$adjustedCoxText$content),
                         "Likelihood ratio test = [0-9.]+, df = ([0-9]+)")
    expect_false(is.na(df_main))
    expect_equal(df_adj, df_main)

    # Same p-values. finalfit prints 3 dp with a "<0.001" floor.
    p_ff  <- sort(sub("^.*p([&<=][^)]*)\\)$", "\\1", ff))
    p_adj <- sort(vapply(adj$Pvalue, function(p)
        if (p < 0.0005) "&lt;0.001" else sprintf("=%.3f", p), character(1)))
    expect_equal(p_adj, p_ff)
})

test_that("an ordinal covariate in contexpl is one linear-trend row in BOTH tables", {
    res <- .msacc_run(.msacc_data())

    adj <- as.data.frame(res$adjustedCoxTable)
    expect_equal(sum(grepl("performance_status", adj$Variable, fixed = TRUE)), 1L)

    # finalfit describes it as a continuous variable ("Mean (SD)") rather than
    # listing its levels -- the visible proof that cont_cut = 0 took effect.
    main <- .msacc_text(res$text$content)
    expect_true(grepl("performance_status", main, fixed = TRUE))
    expect_match(main, "performance_status\\s*\\n?\\s*Mean \\(SD\\)")

    # And the user is told which specification was fitted, rather than it being
    # decided silently by a third-party default.
    expect_match(.msacc_text(res$infoMessages$content),
                 "performance_status \\(3 distinct values\\)")
})

test_that("a genuine factor in contexpl is refused, not coerced, before any model is fitted", {
    d <- .msacc_data()
    d$performance_status <- factor(d$performance_status)

    err <- tryCatch({ .msacc_run(d); NULL },
                    error = function(e) conditionMessage(e))
    # jmvcore validates options before .init()/.run(), so neither table exists:
    # the two paths cannot disagree because neither one runs.
    expect_false(is.null(err))
    expect_match(err, "contexpl")
    expect_match(err, "numeric")
})

# ---------------------------------------------------------------- ISSUE 2 ----

test_that("factor contrasts are described against their reference level, never per unit", {
    res <- .msacc_run(.msacc_data())
    prose <- .msacc_text(res$adjustedCoxSummary$content)

    # One statement per <br><br>-separated block, which .msacc_text turns into
    # newlines. Splitting on ". " would cut inside "0.99 (0.79-1.25, 95% CI)".
    sentences <- trimws(unlist(strsplit(prose, "\n", fixed = TRUE)))
    sentences <- sentences[nzchar(sentences)]

    factor_s <- grep("^For (stage|grade|treatment) =", sentences, value = TRUE)
    expect_equal(length(factor_s), 4L)   # stage II, stage III, grade, treatment

    for (s in factor_s) {
        expect_false(grepl("unit increase", s, fixed = TRUE))
        expect_true(grepl("compared with", s, fixed = TRUE))
    }

    # The reference level is named, not merely implied.
    expect_true(any(grepl("For stage = II compared with stage = I,", factor_s, fixed = TRUE)))
    expect_true(any(grepl("For stage = III compared with stage = I,", factor_s, fixed = TRUE)))
    expect_true(any(grepl("For grade = Poor compared with grade = Good,", factor_s, fixed = TRUE)))
    expect_true(any(grepl("For treatment = Treatment A compared with treatment = Control,",
                          factor_s, fixed = TRUE)))

    # Continuous predictors keep the per-unit reading, which is where it belongs.
    expect_true(any(grepl("For age, the adjusted hazard ratio is", sentences, fixed = TRUE) &
                    grepl("per 1-unit increase in age", sentences, fixed = TRUE)))

    # The inflated percentage gloss is gone: HR 7.53 used to be reported as a
    # "653.4 % increase in hazard" attached to a comparison that was never named.
    expect_false(grepl("% increase in hazard", prose, fixed = TRUE))
    expect_false(grepl("% decrease in hazard", prose, fixed = TRUE))
})

test_that("no raw coefficient name reaches the table or the prose", {
    res <- .msacc_run(.msacc_data())
    prose <- .msacc_text(res$adjustedCoxSummary$content)
    labels <- as.data.frame(res$adjustedCoxTable)$Variable

    for (raw in c("stageII", "stageIII", "gradePoor", "treatmentTreatment A")) {
        expect_false(grepl(raw, prose, fixed = TRUE))
        expect_false(any(grepl(raw, labels, fixed = TRUE)))
    }

    expect_true(all(c("stage: II (vs I)", "stage: III (vs I)",
                      "grade: Poor (vs Good)",
                      "treatment: Treatment A (vs Control)",
                      "age (per 1-unit increase)") %in% labels))
})

test_that(".coefTerms resolves the cases that break string parsing", {
    gen <- get("multisurvivalClass", envir = .msacc_ns)
    coefTerms <- gen$private_methods$.coefTerms
    skip_if(is.null(coefTerms), ".coefTerms not present")

    set.seed(3); n <- 400
    d <- data.frame(
        t = stats::rexp(n, 0.05) + 0.1,
        ev = stats::rbinom(n, 1, 0.8),
        stage = factor(sample(c("I", "II", "IV"), n, TRUE), levels = c("I", "II", "IV")),
        treatment = factor(sample(c("Control", "Treatment A"), n, TRUE),
                           levels = c("Control", "Treatment A")),
        ord = factor(sample(c("low", "mid", "high"), n, TRUE),
                     levels = c("low", "mid", "high"), ordered = TRUE),
        lgl = sample(c(TRUE, FALSE), n, TRUE),
        age = stats::rnorm(n, 60, 10))
    d[["my stage"]] <- d$stage
    m <- survival::coxph(
        survival::Surv(t, ev) ~ stage + treatment + ord + lgl + age +
            stage:treatment + `my stage`,
        data = d, x = TRUE, y = TRUE, model = TRUE)

    nms <- rownames(summary(m)$coefficients)
    tt  <- coefTerms(m, display = c(stage = "Tumour stage"))
    names(tt) <- nms
    expect_equal(length(tt), length(nms))

    # Level text that repeats the term name: positional mapping, not prefix strip.
    expect_equal(tt[["treatmentTreatment A"]]$level, "Treatment A")
    expect_equal(tt[["treatmentTreatment A"]]$ref, "Control")

    # display map replaces janitor's cleaned name with the clinician's own.
    expect_equal(tt[["stageIV"]]$var, "Tumour stage")
    expect_equal(tt[["stageIV"]]$level, "IV")
    expect_equal(tt[["stageIV"]]$ref, "I")

    expect_equal(tt[["age"]]$kind, "continuous")

    # ORDERED factor -> contr.poly. length(idx) == nlevels-1 still holds, so a
    # positional lookup would confidently print the wrong reference level. Must
    # come back as an unnamed contrast instead.
    expect_equal(tt[["ord.L"]]$kind, "contrast")
    expect_equal(tt[["ord.L"]]$suffix, ".L")
    expect_null(tt[["ord.L"]]$ref)

    # Logical predictor: has a $contrasts entry but NO $xlevels entry.
    expect_equal(tt[["lglTRUE"]]$kind, "level")
    expect_equal(tt[["lglTRUE"]]$level, "TRUE")
    expect_equal(tt[["lglTRUE"]]$ref, "FALSE")

    # Interaction: neither template applies, and it must not be read as a level.
    # The crossed LEVELS must appear -- the two coefficients of stage:treatment
    # share one term, so a term-only label prints the same string twice next to
    # different hazard ratios.
    expect_equal(tt[["stageII:treatmentTreatment A"]]$kind, "interaction")
    expect_equal(tt[["stageII:treatmentTreatment A"]]$var,
                 "Tumour stage: II \u{00D7} treatment: Treatment A")
    expect_false(identical(tt[["stageII:treatmentTreatment A"]]$var,
                           tt[["stageIV:treatmentTreatment A"]]$var))

    # Non-syntactic name: $assign keys are backticked, $xlevels keys are not.
    # Without the gsub guard this silently degrades to "contrast".
    expect_equal(tt[["`my stage`IV"]]$kind, "level")
    expect_equal(tt[["`my stage`IV"]]$level, "IV")
    expect_equal(tt[["`my stage`IV"]]$ref, "I")
})
