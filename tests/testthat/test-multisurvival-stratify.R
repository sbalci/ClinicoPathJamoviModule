# Regression tests for the multisurvival stratified-Cox defects (MS-02 / MS-03)
# and for the two contradictory significance summaries.
#
# Symptom that prompted these: one report simultaneously stated "The Cox model is
# stratified by treatment and stage" and printed hazard ratios *for* treatment and
# stage, with a likelihood-ratio df that counted the strata as covariates. A
# stratified variable is absorbed into the baseline hazard and has no coefficient,
# so both cannot be true.

.ms_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("multisurvival", envir = .cand, inherits = FALSE)) {
            .ms_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.ms_ns), "multisurvival not available in this distribution")

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }

.ms_data <- function() {
    set.seed(13); n <- 400
    site <- factor(sample(c("S1", "S2", "S3"), n, TRUE))
    grp  <- factor(sample(c("A", "B"), n, TRUE))
    age  <- rnorm(n, 60, 10)
    base <- c(S1 = 0.02, S2 = 0.05, S3 = 0.09)[as.character(site)]
    data.frame(t   = round(rexp(n, base * exp(0.8 * (grp == "B") + 0.03 * (age - 60))), 1) + 0.1,
               ev  = rbinom(n, 1, 0.85),
               grp = grp, age = age, site = site)
}

.ms_run <- function(d, expl, ...) {
    quiet(do.call(get("multisurvival", envir = .ms_ns),
                  c(list(data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
                         explanatory = expl, contexpl = "age",
                         dod = NULL, dooc = NULL, awd = NULL, awod = NULL), list(...))))
}

# Split the kable HTML the analysis emits into per-row character vectors.
.ms_cells <- function(html) {
    rows <- unlist(strsplit(as.character(html), "<tr>", fixed = TRUE))
    lapply(rows, function(r) trimws(gsub("<[^>]*>", "", unlist(strsplit(r, "</td>", fixed = TRUE)))))
}

test_that("a stratified fit reports the stratified hazard ratio and drops the strata rows", {
    d <- .ms_data()

    # Reference values from survival itself.
    unstrat <- survival::coxph(survival::Surv(t, ev) ~ grp + age, d)
    strat   <- survival::coxph(survival::Surv(t, ev) ~ grp + age + strata(site), d)
    hr_unstrat <- unname(exp(stats::coef(unstrat))["grpB"])
    hr_strat   <- unname(exp(stats::coef(strat))["grpB"])
    expect_false(isTRUE(all.equal(hr_unstrat, hr_strat, tolerance = 1e-3)))  # the two must differ

    r <- .ms_run(d, c("grp", "site"), use_stratify = TRUE, stratvar = "site")
    cs <- .ms_cells(r$text$content)

    # The multivariable HR for grp level B must match the STRATIFIED reference.
    hit <- Filter(function(x) length(x) >= 5 && identical(x[2], "B"), cs)
    expect_gt(length(hit), 0)
    shown <- suppressWarnings(as.numeric(
        regmatches(hit[[1]][5], regexpr("^[0-9]+\\.?[0-9]*", hit[[1]][5]))))
    expect_equal(shown, round(hr_strat, 2), tolerance = 0.02)

    # ... and must NOT match the unstratified one, which is what used to be shown.
    expect_false(isTRUE(all.equal(shown, round(hr_unstrat, 2), tolerance = 1e-3)))

    # The stratification variable keeps its UNIVARIABLE hazard ratio -- dropping
    # it from the table entirely would hide information the unstratified report
    # showed -- but it must have NO multivariable estimate, because a stratified
    # variable is absorbed into the baseline hazard.
    strata_rows <- Filter(function(x) length(x) >= 5 && grepl("^S[23]$", x[2]), cs)
    expect_gt(length(strata_rows), 0)
    for (sr in strata_rows) {
        expect_match(sr[4], "[0-9]")          # univariable HR present
        expect_identical(sr[5], "-")          # multivariable HR absent
    }

    # Likelihood-ratio df must count only the covariates (grp, age), not the strata.
    metrics <- as.character(r$text2$content)
    expect_match(metrics, "df = 2")
})

test_that("an unstratified fit still counts every covariate in its df", {
    d <- .ms_data()
    r <- .ms_run(d, c("grp", "site"))
    expect_match(as.character(r$text2$content), "df = 4")   # grp 1 + site 2 + age 1
})

test_that("the two significance summaries agree and name the strongest predictor", {
    d <- .ms_data()
    r <- .ms_run(d, "grp")

    txt <- paste(vapply(names(r), function(z) {
        y <- try(r[[z]]$content, silent = TRUE)
        if (inherits(y, "try-error") || is.null(y)) "" else as.character(y)
    }, character(1)), collapse = " ")
    txt <- gsub("<[^>]*>", " ", txt)

    # Both summaries answer "how many predictors were significant". They used to
    # disagree in the same report -- "1 out of 8" beside "5 out of 8".
    counts <- unlist(regmatches(txt, gregexpr("[0-9]+ out of [0-9]+", txt)))
    expect_gt(length(counts), 0)
    expect_equal(length(unique(counts)), 1)

    # The strongest predictor used to render with an empty name, because finalfit
    # writes the variable name only on the first row of each block and the winning
    # row was a later level.
    if (grepl("Strongest predictor", txt)) {
        nm <- regmatches(txt, regexpr("Strongest predictor:\\s*\\S+", txt))
        expect_gt(length(nm), 0)
        expect_false(grepl("Strongest predictor:\\s*was associated", txt))
    }
})

test_that(".summariseCoxSignificance counts variables, not coefficient rows", {
    ms <- get("multisurvivalClass", envir = .ms_ns)
    f <- ms$private_methods$.summariseCoxSignificance
    skip_if(is.null(f), "helper not present in this build")

    # Called for real as private$.summariseCoxSignificance(), so jmvcore's .()
    # translation helper resolves `self` from the R6 enclosing environment.
    # Pulled out of the generator like this there is no `self`, so supply one.
    env <- new.env(parent = environment(f))
    assign(".", function(x, ...) x, envir = env)
    assign("self", list(), envir = env)
    environment(f) <- env

    # Mimic a finalfit table: the variable name appears only on its first row.
    tab <- data.frame(
        var   = c("stage", "", "", "grade", ""),
        level = c("I", "III", "IV", "Low", "High"),
        all   = rep("", 5),
        `HR (univariable)`   = rep("-", 5),
        `HR (multivariable)` = c("-", "3.30 (1.89-5.76, p<0.001)",
                                 "7.53 (3.98-14.24, p<0.001)",
                                 "-", "1.10 (0.90-1.30, p=0.400)"),
        check.names = FALSE, stringsAsFactors = FALSE)

    s <- f(tab)
    expect_equal(s$n_sig_rows, 2)   # two significant LEVELS
    expect_equal(s$n_sig_vars, 1)   # but only ONE significant VARIABLE
    expect_equal(s$n_total_vars, 2)
    expect_equal(s$strongest_hr, 7.53)
    expect_match(s$strongest_label, "stage")   # not "" -- the forward-fill fix
    expect_match(s$strongest_label, "IV")
})
