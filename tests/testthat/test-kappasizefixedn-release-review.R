# Regression tests from the `kappaSizeFixedN` release review.
#
# Since the 2026-10-16 fix (development-ideas/kappaSizeFixedN-validation-2026-10-16.md) the bound is
# solved directly, and with three or more raters or categories it is the lower of the fixed-proportion
# and restricted-ML bounds; kappaSize's own search is kept in the Summary pane. Tests that pinned the
# module to kappaSize's figure now pin the Summary pane to it.

fn_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", kappa0 = 0.60, props = "0.30, 0.70",
             raters = "2", alpha = 0.05, n = 100),
        list(...))
    do.call(ClinicoPath::kappaSizeFixedN, args)
}
fn_line <- function(res) trimws(strsplit(res$text1$content, "\n")[[1]][1])
# text2 is wrapped at ~78 columns at render time; compare with line breaks collapsed
flat <- function(x) gsub("\\s+", " ", x)


test_that("the Summary pane reproduces kappaSize's own figure", {
    skip_if_not_installed("kappaSize")
    expect_match(fn_run()$text_summary$content,
                 sprintf("expected lower limit for kappa of %s",
                         kappaSize::FixedNBinary(kappa0 = 0.60, n = 100, props = c(0.30, 0.70),
                                                 alpha = 0.05, raters = 2)$kappaL),
                 fixed = TRUE)
    expect_match(fn_run(outcome = "3", props = "0.20, 0.30, 0.50")$text_summary$content,
                 sprintf("expected lower limit for kappa of %s",
                         kappaSize::FixedN3Cats(kappa0 = 0.60, n = 100,
                                                props = c(0.20, 0.30, 0.50),
                                                alpha = 0.05, raters = 2)$kappaL),
                 fixed = TRUE)
})


test_that("n = Inf is refused instead of hanging the engine", {
    # The old guard was `is.na(n) || n < 2 || n != round(n)`, and Inf passes all three
    # (is.na(Inf) FALSE, Inf < 2 FALSE, Inf != round(Inf) FALSE). Inside the engine the test
    # statistic becomes NaN, the while loop never terminates, and jamovi cannot abort it.
    t0 <- Sys.time()
    expect_error(fn_run(n = Inf), "between 11 and 1e\\+06")
    expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)

    # A whole-number check is still the backend's job: `n` is a Number option, so 100.5 is
    # inside the compiled bounds and only the backend stops it before the engine sees it.
    expect_error(fn_run(n = 100.5), "integer")        # type: Integer, enforced by jmvcore
    expect_error(fn_run(n = 2e6), "between 11 and 1e\\+06")

    # NA/NaN die in jmvcore's own range check on `if (value < min)` with "missing value where
    # TRUE/FALSE needed". That message is poor but jmvcore-wide, not this analysis's to fix.
    expect_error(fn_run(n = NA_real_))
    expect_error(fn_run(n = NaN))
})


test_that("n below the engine's floor is refused with a reason, not a vendor string", {
    # Every kappaSize FixedN* engine contains
    #   if (n <= 10) stop("Sorry, your study should enroll at least 10 subjects.")
    # while the option allowed n down to 2, so 2..10 reached the engine only to bounce back as
    # a raw vendor message (whose own wording is off by one).
    # The compiled `min: 11` now stops these at the option layer, so the message a user sees is
    # jmvcore's; the backend clause below it remains as a backstop if the bound is ever relaxed.
    for (bad in c(2, 5, 10))
        expect_error(fn_run(n = bad), "between 11 and", info = paste("n =", bad))
    expect_no_error(fn_run(n = 11))
    # never the raw vendor string, whose own wording is off by one ("at least 10")
    expect_false(grepl("Sorry", tryCatch(fn_run(n = 5), error = conditionMessage), fixed = TRUE))

    # `n` is quoted in the yaml ('n'), because a bare n is a YAML 1.1 boolean and R's yaml
    # package hands back FALSE where jamovi's js-yaml hands back the string. Accept either form
    # so this pins the bound, not the quoting style.
    a_yaml <- paste(readLines("../../jamovi/kappaSizeFixedN.a.yaml", warn = FALSE), collapse = "\n")
    blk <- regmatches(a_yaml, regexpr("(?s)    - name: '?n'?\\n.*?(?=\\n    - name: |\\Z)",
                                      a_yaml, perl = TRUE))
    expect_length(blk, 1L)
    expect_match(blk, "min: 11", fixed = TRUE)

    # and the compiled wrapper actually carries it
    h <- paste(readLines("../../R/kappaSizeFixedN.h.R", warn = FALSE), collapse = "\n")
    expect_match(h, "min=11", fixed = TRUE)
})


test_that("kappaSize's walk past the model floor is never shown as the bound", {
    # kappaSize decrements rho from kappa0 by 0.001 with no floor, so an underpowered design
    # walks straight out of the common-correlation model, whose "all raters positive" cell
    # p^r (1 - rho) + rho p turns negative below -p^(r-1) / (1 - p^(r-1)): prevalence 0.02,
    # n 11, alpha 0.001 -> -23.78; three raters, prevalence 0.02, kappa0 0.01, n 100, alpha 0.2
    # -> -0.841 against a model floor of -0.0004. Those designs used to be refused. The bound is
    # now solved inside the model, where the chi-square grows without limit at the floor, so the
    # true root exists and is reported (2026-10-16 fix); kappaSize's figure stays in the Summary
    # pane, labelled as not a valid limit.
    skip_if_not_installed("kappaSize")
    bound_of <- function(res) as.numeric(sub(".*kappa of (\\S+?)\\.$", "\\1", fn_line(res)))
    raw <- kappaSize::FixedNBinary(kappa0 = 0.01, n = 11, props = c(0.02, 0.98),
                                   alpha = 0.001, raters = 2)$kappaL
    expect_lt(raw, -1)                                  # the engine really does return it
    res <- fn_run(kappa0 = 0.01, n = 11, props = "0.02, 0.98", alpha = 0.001)
    # two raters: floor -p / (1 - p) = -0.020408; root -0.020387 (independent bisection), shown -0.020
    expect_equal(bound_of(res), -0.020)
    expect_match(res$notices$content, "cannot demonstrate agreement")
    expect_match(flat(res$text_summary$content), "stepped past the lowest agreement the model allows")
    raw3 <- kappaSize::FixedNBinary(kappa0 = 0.01, n = 100, props = 0.02,
                                    alpha = 0.2, raters = 3)$kappaL
    expect_gt(raw3, -1)                                 # inside (-1, 0) but outside the model
    res3 <- fn_run(kappa0 = 0.01, n = 100, props = "0.02", alpha = 0.2, raters = "3")
    b3 <- bound_of(res3)
    expect_gte(b3, -0.02^2 / (1 - 0.02^2))              # three raters: floor -p^2 / (1 - p^2)
    expect_lt(b3, 0)
    expect_false(grepl("-0.841", fn_line(res3), fixed = TRUE))
    expect_match(flat(res3$text_summary$content), "stepped past the lowest agreement the model allows")

    # a legitimately NEGATIVE bound (inside the model: floor is -0.3/0.7 = -0.43 here) is
    # still reported, with an explanation, and its sparse check is evaluated AT the bound.
    # kappaSize's walk gave -0.293; the solved root is -0.2925, shown to nearest as -0.292.
    res <- fn_run(kappa0 = 0.20, n = 20, props = "0.30, 0.70", alpha = 0.001)
    expect_match(flat(res$text2$content), "-0.292", fixed = TRUE)
    expect_match(flat(res$text2$content), "no better than chance")
    expect_match(res$notices$content, "cannot demonstrate agreement")
    expect_false(grepl("less extreme category distribution", res$notices$content))
})


test_that("stale results do not survive a rejected re-run", {
    o <- ClinicoPath:::kappaSizeFixedNOptions$new(outcome = "2", kappa0 = 0.60,
             props = "0.30, 0.70", raters = "2", alpha = 0.05, n = 100)
    a <- ClinicoPath:::kappaSizeFixedNClass$new(options = o, data = data.frame(x = 1))
    p <- a$.__enclos_env__$private
    a$init()
    p$.run()
    expect_match(a$results$text1$content, "lower limit for kappa")

    op <- o$option("props"); op$value <- "0.30, 0.60"     # no longer sums to 1
    expect_error(p$.run(), "sum to 1")
    expect_equal(a$results$text1$content, "")             # previous numbers must be gone
    expect_equal(a$results$text2$content, "")
})


test_that("a decimal comma is diagnosed as such, not as the wrong count", {
    # "0,30 0,70" splits into 0, 30, 0, 70 and used to be reported as four proportions.
    expect_error(fn_run(props = "0,30 0,70"), "decimal point, not a decimal comma")
    expect_error(fn_run(props = "0,5 0,5"), "decimal point, not a decimal comma")
})


test_that("the explanation states the answer and reads correctly for one prevalence", {
    two <- flat(fn_run(props = "0.30, 0.70")$text2$content)
    expect_match(two, "The expected lower bound for kappa is")
    expect_match(two, "proportions of the outcome categories are 0.30 and 0.70", fixed = TRUE)
    # a lower bound is the smallest kappa NOT ruled out; the old sentence said the opposite
    expect_match(two, "still be unable to rule out")
    expect_false(grepl("lowest value of kappa that the study can expect to rule out", two))
    # and it fits the non-wrapping Preformatted pane
    expect_lte(max(nchar(strsplit(fn_run(props = "0.30, 0.70")$text2$content, "\n")[[1]])), 80L)
    expect_lte(max(nchar(strsplit(fn_run(kappa0 = 0.20, n = 20, alpha = 0.001)$text2$content,
                                  "\n")[[1]])), 80L)

    one <- flat(fn_run(props = "0.30")$text2$content)
    expect_match(one, "prevalence of the trait is 0.30", fixed = TRUE)
    expect_false(grepl("proportions of the outcome categories", one, fixed = TRUE))
    # both entry styles are the same computation
    expect_equal(fn_line(fn_run(props = "0.30")), fn_line(fn_run(props = "0.30, 0.70")))
})


test_that("alpha is bounded consistently with the compiled option", {
    # The backend permitted (0,1) while the option compiles to 0.001-0.20, so an R caller could
    # reach the engine with alpha = 0.5 and get "missing value where TRUE/FALSE needed".
    expect_error(fn_run(alpha = 0.5), "between")
    expect_error(fn_run(alpha = 0.0005), "between")
    expect_no_error(fn_run(alpha = 0.001))
    expect_no_error(fn_run(alpha = 0.20))
})


test_that("the proportions sum tolerance matches the engine's", {
    # The module used all.equal(tolerance = 1e-3), which accepts |sum - 1| <= 0.001, while every
    # kappaSize engine rejects at abs(sum(props) - 1) >= 0.001. The module now uses the engine's
    # own predicate, so no sum can slip past the clear message and hit the vendor one.
    expect_error(fn_run(props = "0.302, 0.70"), "sum to 1")     # 1.002
    expect_error(fn_run(props = "0.31, 0.70"), "sum to 1")      # 1.010
    expect_no_error(fn_run(props = "0.3004, 0.70"))             # 1.0004, inside both tolerances
    expect_no_error(fn_run(props = "0.3005, 0.6995"))           # exactly 1

    # 1.001 is the knife edge and lands on the accept side of BOTH predicates, because
    # abs(1.001 - 1) is 0.0009999999999998899 in binary floating point. Module and engine agree,
    # which is the property that matters; pin it so a "tidy-up" to > or to a rounded compare
    # cannot silently reintroduce the divergence.
    expect_no_error(fn_run(props = "0.301, 0.70"))
    skip_if_not_installed("kappaSize")
    expect_no_error(kappaSize::FixedNBinary(kappa0 = 0.60, n = 100, props = c(0.301, 0.70),
                                            alpha = 0.05, raters = 2))
})


test_that("proportions parse on every separator the family accepts", {
    # kappaSizeCI takes "[,;|[:space:]]+"; this analysis took "[,;[:space:]]+", so a pipe-
    # separated list that worked in one member of the family failed in the next.
    base <- fn_line(fn_run(props = "0.30, 0.70"))
    for (ps in c("0.30 0.70", "0.30;0.70", "0.30|0.70", "0.30  ,  0.70", "0.30\t0.70",
                 "0.30\u00A00.70", "0.30\u202F0.70"))   # NBSP and narrow NBSP (macOS/FR)
        expect_equal(fn_line(fn_run(props = ps)), base, label = ps)
})


test_that("the Notes panel states the method and the kappa0 hazard", {
    # kappa0 means the ANTICIPATED kappa here and the NULL kappa in kappaSizePower. Two
    # analyses in the same menu taking an identically-named argument with different meanings is
    # exactly the kind of thing that produces a confidently wrong sample size.
    a <- ClinicoPath:::kappaSizeFixedNClass$new(
        options = ClinicoPath:::kappaSizeFixedNOptions$new(
            outcome = "2", kappa0 = 0.60, props = "0.30, 0.70",
            raters = "2", alpha = 0.05, n = 100),
        data = data.frame(x = 1))
    build <- a$.__enclos_env__$private$.buildNotices

    # the method statement is its own panel at the foot of the results (2026-10-17)
    plain <- a$.__enclos_env__$private$.buildMethodology(outcome = 2, raters = 2, alpha = 0.05)
    expect_match(plain, "Methodology")
    expect_match(plain, "intraclass \\(Fleiss-type\\) kappa")
    expect_match(plain, "steps of 0.001")
    expect_match(plain, "kappa0 here is the agreement you anticipate")
    expect_match(plain, "kappaSizePower", fixed = TRUE)
    expect_false(grepl("Sparse categories", plain, fixed = TRUE))
    expect_false(grepl("cannot demonstrate agreement", plain, fixed = TRUE))
    expect_identical(build(0.44, sparse_cells = FALSE), "")   # no warning, no panel text

    expect_match(build(0.44, sparse_cells = TRUE, sparse_min = 0.11, sparse_below5 = 2L,
                       sparse_total = 7L),
                 "smallest expected count is 0.11, and the number of cells below 5 is 2 of 7")

    # a bound at or below zero is the decisive clinical case and gets its own red block
    expect_match(build(-0.29), "cannot demonstrate agreement")
    expect_match(build(0), "cannot demonstrate agreement")
    expect_false(grepl("cannot demonstrate agreement", build(0.001), fixed = TRUE))
})


test_that("sparse agreement-pattern cells at the lower bound are flagged, not just sparse categories", {
    skip_if_not_installed("kappaSize")
    run <- function(...) {
        args <- utils::modifyList(list(outcome = "2", kappa0 = 0.4, props = "0.30, 0.70",
                                       raters = "2", alpha = 0.05, n = 100), list(...))
        do.call(ClinicoPath::kappaSizeFixedN, args)
    }
    # kappaSize's own warning checks props[i] * n, which is exactly 5 here and stays silent
    # (it prints into the Summary pane), while the pooled cells of the 1-df test at the bound
    # include an all-positive cell below 1.
    six <- run(props = "0.05", raters = "6")
    expect_false(grepl("expected cell count", six$text_summary$content, fixed = TRUE))
    expect_match(six$notices$content, "Sparse categories")
    expect_match(six$notices$content, "goodness-of-fit cells")
    expect_match(six$notices$content, "enriching the case series")
    expect_match(six$notices$content, "the number of cells below 5 is 1 of 3")
    # Cochran's rule on the POOLED cells (all negative / any disagreement / all positive), the
    # cells of the published test, as kappaSizeCI and kappaSizePower judge them (VAL-02, 2026-10-16).
    # With four raters at the default prevalence the all-positive cell is about 4 of 3 cells, so
    # it is flagged now; the unpooled rule had passed it on one cell of 1.85 out of five.
    expect_match(run(kappa0 = 0.4, props = "0.20, 0.80", raters = "4")$notices$content,
                 "Sparse categories")
    # and a dense multi-rater design is not flagged: the unpooled rule fired here on an interior
    # "exactly 5 of 6" cell of 0.51 while the pooled cells are 622 / 244 / 134
    expect_false(grepl("Sparse categories", run(kappa0 = 0.7, props = "0.2", raters = "6",
                                                  n = 1000)$notices$content))
    expect_match(run(kappa0 = 0.4, props = "0.20, 0.80", raters = "5")$notices$content,
                 "Sparse categories")
    # n >= 1e5 is printed as a number, not 1e+05
    expect_match(run(n = 100000)$text1$content, "100000 subjects", fixed = TRUE)
    # multi-category with several raters: all-agree cells from the Dirichlet-multinomial product
    four <- run(outcome = "4", raters = "5", kappa0 = 0.3, props = "0.40, 0.30, 0.06, 0.24",
                n = 120)
    expect_match(four$notices$content, "Sparse categories")
    expect_match(four$notices$content, "collapsing rare categories")
    # the engine's repeated per-category warning is kept once in the raw panes
    five <- run(outcome = "5", kappa0 = 0.6, props = "0.01, 0.04, 0.15, 0.30, 0.50", n = 20)
    expect_equal(sum(grepl("expected cell count",
                           strsplit(five$text_summary$content, "\n")[[1]])), 1L)
    # a dense design stays clean
    expect_false(grepl("Sparse categories", run(n = 500)$notices$content))
})


test_that("the sparse-cell caveat is detected from kappaSize's own summary text", {
    skip_if_not_installed("kappaSize")
    res <- kappaSize::FixedN5Cats(kappa0 = 0.60, n = 20, props = c(0.01, 0.04, 0.15, 0.30, 0.50),
                                  alpha = 0.05, raters = 2)
    txt <- paste(utils::capture.output(summary(res)), collapse = "\n")
    expect_true(grepl("expected cell count is less than five", txt, fixed = TRUE))

    dense <- paste(utils::capture.output(summary(
        kappaSize::FixedNBinary(kappa0 = 0.60, n = 500, props = c(0.30, 0.70),
                                alpha = 0.05, raters = 2))), collapse = "\n")
    expect_false(grepl("expected cell count is less than five", dense, fixed = TRUE))
})


test_that("the notices item is declared in .r.yaml with the same clearWith as the rest", {
    # Adding it requires jmvtools::prepare(); until that runs the analysis errors with
    # "'notices' does not exist in this results element", so this pins the yaml side.
    # The bool handlers matter: this analysis has an option literally named `n`, which is a
    # YAML 1.1 boolean, so R's yaml package (libyaml, 1.1) hands back FALSE where jamovi's
    # js-yaml (1.2) hands back the string "n". Without them this test compares against FALSE.
    y <- yaml::read_yaml("../../jamovi/kappaSizeFixedN.r.yaml",
                         handlers = list("bool#yes" = function(x) x, "bool#no" = function(x) x))
    items <- setNames(y$items, vapply(y$items, function(i) i$name, character(1)))
    expect_true("notices" %in% names(items))
    expect_equal(items$notices$type, "Html")
    opts <- c("outcome", "kappa0", "props", "raters", "alpha", "n")
    for (nm in names(items))
        expect_setequal(items[[nm]]$clearWith, opts)
})


test_that("every declared option is read by the backend", {
    a_yaml <- readLines("../../jamovi/kappaSizeFixedN.a.yaml", warn = FALSE)
    declared <- sub("^    - name: ", "", grep("^    - name: [A-Za-z0-9_]+$", a_yaml, value = TRUE))
    backend <- paste(readLines("../../R/kappaSizeFixedN.b.R", warn = FALSE), collapse = "\n")
    unread <- declared[!vapply(declared, function(o)
        grepl(paste0("options\\$", o, "\\b"), backend), logical(1))]
    expect_equal(unread, character(0))
})


# --- second release-review pass -----------------------------------------------------------

test_that("every pane renders the bound identically", {
    # The explanation used signif(, 4) while text1 printed 7 significant digits, so the same
    # quantity read differently on one screen whenever kappa0 carried more than three decimals.
    # Both now come from one formatter (.fmtBound: rounded down to three decimals).
    b_of <- function(x) sub(".*(?:kappa of|kappa is) (\\S+?)\\.(?: |$).*", "\\1", flat(x), perl = TRUE)
    for (res in list(fn_run(kappa0 = 0.61234, props = "0.30, 0.70"),
                     fn_run(kappa0 = 0.45, props = "0.07", raters = "5", alpha = 0.02, n = 25))) {
        expect_identical(b_of(res$text1$content), b_of(res$text2$content))
        expect_match(b_of(res$text1$content), "^-?[0-9]+\\.[0-9]{3}$")
        # no floating-point residue of kappaSize's 0.001 walk anywhere on screen
        expect_false(grepl("99999|00000[1-9]", paste(res$text1$content, res$text2$content,
                                                     res$text_summary$content)))
    }
})


test_that("the Notes panel says the bound is a best case, not a guarantee", {
    # kappaL is what the study reaches only if the observed agreement lands exactly on
    # kappa0; about half of such studies do worse. Without this a planner reads the bound
    # as a floor the study is guaranteed to clear.
    notes <- fn_run()$methodology$content
    expect_match(notes, "lands exactly on kappa0", fixed = TRUE)
    expect_match(notes, "planning expectation rather than a guarantee", fixed = TRUE)
})


test_that("the methodological sources are cited, not just named in prose", {
    # The Methodology note credits Donner & Eliasziw and Rotondi & Donner; both belong in
    # refs so the reference list a user exports actually contains them.
    r_yaml <- paste(readLines("../../jamovi/kappaSizeFixedN.r.yaml", warn = FALSE), collapse = "\n")
    for (key in c("donnerEliasziwKappaGOF", "rotondiDonnerKappaCI"))
        expect_match(r_yaml, key, fixed = TRUE)

    refs <- paste(readLines("../../jamovi/00refs.yaml", warn = FALSE), collapse = "\n")
    for (key in c("donnerEliasziwKappaGOF:", "rotondiDonnerKappaCI:"))
        expect_match(refs, key, fixed = TRUE)
})


test_that("the shipped example file runs and its quoted bounds are the real ones", {
    skip_if_not_installed("kappaSize")
    path <- "../../inst/examples/kappasizefixedn_example.R"
    skip_if_not(file.exists(path))
    lines <- readLines(path, warn = FALSE)
    ex <- parse(path, keep.source = TRUE)
    srcs <- utils::getSrcref(ex)

    calls <- list(); starts <- integer()
    for (i in seq_along(ex)) {
        e <- ex[[i]]
        cal <- if (is.call(e) && identical(e[[1]], as.name("kappaSizeFixedN"))) e
               else if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
                        identical(e[[3]][[1]], as.name("kappaSizeFixedN"))) e[[3]] else NULL
        if (!is.null(cal)) {
            calls[[length(calls) + 1]] <- cal
            starts <- c(starts, as.integer(srcs[[i]])[3])
        }
    }
    expect_gt(length(calls), 10)

    quoted <- grep("^# Result: kappaL = ", lines)
    for (i in seq_along(calls)) {
        obj <- eval(calls[[i]], envir = new.env(parent = globalenv()))
        got <- sub("[.] *$", "", sub(".*kappa of ", "",
                                     strsplit(trimws(obj$text1$content), "\n")[[1]][1]))
        q <- quoted[quoted > starts[i]][1]
        if (!is.na(q))
            expect_identical(got, sub("^# Result: kappaL = ", "", lines[q]),
                             info = paste("example call", i))
        # the bound is a confidence limit around kappa0 and can never sit above it
        expect_lt(as.numeric(got), as.numeric(as.list(calls[[i]])$kappa0))
    }
})


test_that("the shipped example file does not resurrect the power framing", {
    path <- "../../inst/examples/kappasizefixedn_example.R"
    skip_if_not(file.exists(path))
    txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # kappa0 is the ANTICIPATED value here, and nothing is tested; the previous version of
    # this file called it a null hypothesis and the bound a "minimum detectable kappa",
    # which inverts the direction of the answer (kappaL is always BELOW kappa0).
    # The file now names those phrases only to forbid them, so match the assertive forms
    # the old version actually used rather than any mention of the words.
    expect_false(grepl("Null hypothesis", txt, fixed = TRUE))
    expect_false(grepl("Output: Lowest detectable kappa", txt, fixed = TRUE))
    expect_false(grepl("returns the LOWEST kappa value detectable", txt, fixed = TRUE))
    expect_match(txt, "It is NOT a null", fixed = TRUE)
    expect_match(txt, "ALWAYS BELOW kappa0", fixed = TRUE)
})


test_that("the shipped teaching datasets do not teach the power framing", {
    # kappasizefixedn_power_cases$teaching_point used to assert, among other things, that
    # "More categories reduce detection capability". At balanced proportions the opposite is
    # true for this method (n=100, kappa0=0.50, 2 raters: 2 cats -> 0.345, 5 cats -> 0.397),
    # so the datasets now quote the computed bound instead of claiming a direction.
    for (nm in c("kappasizefixedn_scenarios_comprehensive",
                 "kappasizefixedn_power_cases",
                 "kappasizefixedn_validation_cases")) {
        f <- system.file("..", "data", paste0(nm, ".rda"), package = "ClinicoPath")
        if (!file.exists(f)) f <- file.path("../../data", paste0(nm, ".rda"))
        skip_if_not(file.exists(f), paste("missing", nm))
        e <- new.env(); load(f, envir = e); d <- get(ls(e)[1], envir = e)
        chr <- names(d)[vapply(d, is.character, TRUE)]
        offenders <- unlist(lapply(chr, function(cc)
            d[[cc]][grepl("detectable|can we detect|null hypothes", d[[cc]], ignore.case = TRUE)]))
        expect_length(offenders, 0L)
    }
})


test_that("kappasizefixedn_power_cases quotes the bound this function actually returns", {
    skip_if_not_installed("kappaSize")
    f <- "../../data/kappasizefixedn_power_cases.rda"
    skip_if_not(file.exists(f))
    e <- new.env(); load(f, envir = e); d <- as.data.frame(get(ls(e)[1], envir = e))
    for (i in seq_len(nrow(d))) {
        res <- ClinicoPath::kappaSizeFixedN(
            outcome = as.character(d$outcome[i]), kappa0 = d$kappa0[i],
            props = d$proportions[i], raters = as.character(d$raters[i]),
            alpha = d$alpha[i], n = as.integer(d$n[i]))
        got <- as.numeric(sub("[.] *$", "", sub(".*kappa of ", "",
                          strsplit(trimws(res$text1$content), "\n")[[1]][1])))
        expect_identical(
            d$expected_pattern[i],
            sprintf("Lower bound %s; %s below kappa0",
                    formatC(got, format = "f", digits = 3),
                    formatC(d$kappa0[i] - got, format = "f", digits = 3)),
            info = d$case_name[i])
    }
})


test_that("expected counts never reach the reader in scientific notation", {
    # This file had no .fmtCount at all while both siblings did: signif(sparse_min, 2) went
    # straight into the sentence, and 64 of 120 swept designs told a pathologist that "the
    # smallest expected count is 2.4e-06". Pinned directly so the floor cannot drift -- the
    # same three assertions live in test-kappasizeci- and test-kappasizepower-, and the three
    # .fmtCount implementations must stay identical.
    # .fmtCount calls .() for its two literal returns, and jmvcore's .() looks up `self` in the
    # calling frame -- lifted out of $private_methods it is unbound and throws "object 'self'
    # not found". Bind it off an instance instead.
    an  <- ClinicoPath:::kappaSizeFixedNClass$new(
        options = ClinicoPath:::kappaSizeFixedNOptions$new())
    fmt <- an$.__enclos_env__$private$.fmtCount
    expect_equal(fmt(8.9e-06), "below 0.0001")
    expect_equal(fmt(0.0013),  "0.0013")
    expect_equal(fmt(0.013),   "0.013")
    expect_equal(fmt(4.9),     "4.9")
    expect_equal(fmt(NA_real_), "unavailable")

    # and the notice it feeds renders the same way
    build <- an$.__enclos_env__$private$.buildNotices
    sparse <- build(0.44, sparse_cells = TRUE, sparse_min = 2.4e-06, sparse_below5 = 6L,
                    sparse_total = 7L)
    expect_match(sparse, "smallest expected count is below 0.0001", fixed = TRUE)
    expect_false(grepl("count is [0-9.]+e[-+]", sparse))
})


test_that("the syntax pane escapes props: a crafted value cannot inject an argument (security audit 2026-10-16)", {
    # jmvcore quotes a String without escaping it, so a props value carried in a shared .omv exported a call
    # with extra arguments that ran their code when pasted into R. Parse only, never evaluate.
    src_of <- function(props) ClinicoPath:::kappaSizeFixedNClass$new(
        options = ClinicoPath:::kappaSizeFixedNOptions$new(props = props))$asSource()
    for (bad in c('0.2", alpha = {stop("INJECTED"); 0.05}, outcome = "2',
                  '0.2\\", 0.8\\', '0.2"\nstop("x")\n"')) {
        ex <- parse(text = src_of(bad))[[1]]
        expect_identical(ex$props, bad)
        expect_identical(names(ex)[-1], "props")
    }
    expect_match(src_of("0.30, 0.70"), 'props = "0.30, 0.70"', fixed = TRUE)
    expect_false(grepl("props", src_of("0.20, 0.80"), fixed = TRUE))   # default still omitted
})


test_that("decimal-comma advice only when the comma reading parses in full (2026-10-16)", {
    # The check dropped tokens it could not read, so "1.2, 0.8" ("1.2," unreadable, 0.8 fine) was told to use
    # the decimal point it already used. Shared rule: R/utils-kappasize.R kappaSizePropsLookDecimalComma().
    for (p in c("1.2, 0.8", "2, 0.8"))
        expect_error(fn_run(props = p), "strictly between 0 and 1", fixed = TRUE, info = p)
    for (p in c("0,30 0,70", "0,3, 0,7", "0,3|0,7"))
        expect_error(fn_run(props = p), "decimal point, not a decimal comma", fixed = TRUE, info = p)
})


test_that("the significance level is grouped under 'Significance Level', not 'Alpha' twice (2026-10-16)", {
    ui <- testthat::test_path("..", "..", "jamovi", "kappaSizeFixedN.u.yaml")
    skip_if_not(file.exists(ui))
    groups <- vapply(yaml::read_yaml(ui)$children, function(x) if (is.null(x$label)) "" else x$label, "")
    expect_true("Significance Level" %in% groups)
    expect_false("Alpha" %in% groups)
})


# ---- /check-function-full follow-ups (2026-10-17) -------------------------------------------------

test_that("IR-03 the proportions are re-estimated where the warm start used to leave the model", {
    # One 0.01 step below the fixed root the anticipated proportions were outside the model; the fit
    # gave up and the bound fell back to -0.016. Independent multi-start oracle: re-estimated root
    # -0.10067 (Codex, and development-ideas/kappaSizeFixedN-independent-review-2026-10-16.md).
    res <- fn_run(outcome = "3", kappa0 = 0.20, props = "0.02, 0.4426, 0.5374", raters = "2",
                  alpha = 0.05, n = 11)
    expect_false(grepl("could not be re-estimated", res$methodology$content, fixed = TRUE))
    expect_match(res$methodology$content, "The bound shown is the lower of two", fixed = TRUE)
    expect_identical(fn_line(res), "A total of 11 subjects can produce an expected lower limit for kappa of -0.101.")
})

test_that("IR-03 the re-estimated statistic does not depend on what was fitted before", {
    # A warm start from the previous fit gave -2.54 at this rho when fitted first and +3.83 after a fit
    # at 0.0392, so uniroot saw no change of sign and the whole re-estimation was abandoned.
    an <- ClinicoPath:::kappaSizeFixedNClass$new(options = ClinicoPath:::kappaSizeFixedNOptions$new(),
                                                 data = data.frame())
    p <- c(0.005, 0.0342, 0.3743, 0.3951, 0.1914); p <- p / sum(p)
    fit <- an$.__enclos_env__$private$.rmlFitter(5, 6, p, 0.05)
    first <- fit(0.049215)$stat
    invisible(fit(0.039215))
    expect_identical(fit(0.049215)$stat, first)
})

test_that("IR-04 the amber remedy is offered only when a design within the option limits reaches 0.40", {
    amber <- function(...) fn_run(...)$notices$content
    # kappa0 0.4001: 0.399 even at n 1e6, 6 raters, alpha 0.20 - "enrol more" cannot be followed
    hi <- amber(kappa0 = 0.4001, props = "0.5", raters = "6", alpha = 0.2, n = 1000000)
    expect_match(hi, "This design cannot support a reliability claim.", fixed = TRUE)
    expect_false(grepl("Enrol more subjects", hi, fixed = TRUE))
    expect_match(hi, "the largest this analysis allows", fixed = TRUE)
    # the reported bound is the lower of two: the fixed one reaches 0.40 (0.40005) but the re-estimated
    # one does not (0.39999), so this is not reachable either (Codex counterexample)
    rml <- amber(outcome = "3", kappa0 = 0.4005, props = "0.11, 0.70, 0.19", raters = "6",
                 alpha = 0.2, n = 1000000)
    expect_match(rml, "the largest this analysis allows", fixed = TRUE)
    # reachable: kappa0 0.41, 30 subjects
    ok <- amber(kappa0 = 0.41, props = "0.5", n = 30)
    expect_match(ok, "This design cannot support a reliability claim.", fixed = TRUE)
    expect_match(ok, "Enrol more subjects, add raters, or relax the significance level.", fixed = TRUE)
    # kappa0 at or below 0.40 keeps its own explanation
    expect_match(amber(kappa0 = 0.40, props = "0.5", n = 200), "is what binds here", fixed = TRUE)
})

test_that("IR-04 .liftable() tests the limits the options actually have", {
    # .liftable() hard-codes n 1e6, alpha 0.20 and 6 raters; this fails if the .a.yaml limits move
    y <- yaml::read_yaml("../../jamovi/kappaSizeFixedN.a.yaml",
                         handlers = list("bool#yes" = function(x) x, "bool#no" = function(x) x))
    o <- setNames(y$options, vapply(y$options, function(i) i$name, ""))
    expect_equal(o$n$max, 1000000)
    expect_equal(o$alpha$max, 0.20)
    expect_identical(tail(unlist(o$raters$options), 1), "6")
    backend <- paste(readLines("../../R/kappaSizeFixedN.b.R", warn = FALSE), collapse = "\n")
    expect_match(backend, "n <- 1e6", fixed = TRUE)
    expect_match(backend, "crit <- stats::qchisq(1 - 2 * 0.20, 1)", fixed = TRUE)
    expect_match(backend, "for (r in raters:6)", fixed = TRUE)
})

test_that("the Methodology panel sits at the foot and the Notes panel carries warnings only", {
    y <- yaml::read_yaml("../../jamovi/kappaSizeFixedN.r.yaml",
                         handlers = list("bool#yes" = function(x) x, "bool#no" = function(x) x))
    items <- vapply(y$items, function(i) i$name, "")
    expect_identical(items[1], "notices")
    expect_identical(items[length(items)], "methodology")
    res <- fn_run()   # no warning at these settings
    # never "" after a run: the jamovi client skips an empty Html update and keeps the old warnings
    expect_identical(res$notices$content, "<div></div>")
    expect_false(grepl("Methodology", res$notices$content, fixed = TRUE))
    expect_match(res$methodology$content, "Methodology", fixed = TRUE)
    warned <- fn_run(kappa0 = 0.2, props = "0.5", n = 30)
    expect_match(warned$notices$content, "This sample size cannot demonstrate agreement.", fixed = TRUE)
    expect_false(grepl("Methodology", warned$notices$content, fixed = TRUE))
})

test_that("the Summary-panel cross-reference appears only when kappaSize printed its cell-count line", {
    line <- "The Summary panel's line 'At least one expected cell count is less than five'"
    sparse <- fn_run(outcome = "5", kappa0 = 0.60, props = "0.01, 0.04, 0.15, 0.30, 0.50", n = 20)
    expect_match(sparse$text_summary$content, "expected cell count is less than five", fixed = TRUE)
    expect_match(sparse$methodology$content, line, fixed = TRUE)
    dense <- fn_run(kappa0 = 0.60, props = "0.30, 0.70", n = 500)
    expect_false(grepl("expected cell count", dense$text_summary$content, fixed = TRUE))
    expect_false(grepl(line, dense$methodology$content, fixed = TRUE))
    expect_false(grepl("If the Summary panel also prints", dense$methodology$content, fixed = TRUE))
})

test_that("the notice builder follows its arguments, not self$options", {
    # the analysis object carries kappa0 = 0.60; the remedy must follow the kappa0 passed in
    an <- ClinicoPath:::kappaSizeFixedNClass$new(
        options = ClinicoPath:::kappaSizeFixedNOptions$new(kappa0 = 0.60), data = data.frame())
    build <- an$.__enclos_env__$private$.buildNotices
    expect_match(build(0.30, kappa0 = 0.35), "is what binds here", fixed = TRUE)
    expect_match(build(0.30, kappa0 = 0.50), "Enrol more subjects", fixed = TRUE)
    expect_match(build(0.30, kappa0 = 0.4001, liftable = FALSE), "the largest this analysis allows", fixed = TRUE)
    meth <- an$.__enclos_env__$private$.buildMethodology
    expect_false(grepl("lower of two", meth(outcome = 2, raters = 2, alpha = 0.05), fixed = TRUE))
    expect_match(meth(outcome = 2, raters = 3, alpha = 0.05), "lower of two", fixed = TRUE)
    expect_match(meth(outcome = 3, raters = 2, alpha = 0.05, rml = "failed"), "could not be re-estimated", fixed = TRUE)
})
