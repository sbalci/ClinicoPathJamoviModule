# An unchosen Female/Male level is a waiting state in agepyramid (user decision, 2026-10-18).
#
# Without allowNone the jamovi client pre-filled BOTH Level options with the gender variable's
# first level, which then hit "Female and Male levels must be different". The backend, given
# NULLs from R, guessed instead: from the level names, else by level ORDER (first = female),
# else filled the empty side from the remaining levels. Now nothing is guessed: until the levels
# are chosen the analysis lists the levels found (with counts) in an INFO notice and draws
# nothing. The one exception is a single-gender cohort: one side chosen and no other observed
# level, which is drawn one-sided as before.
#
# Level options have no default, so they are required wrapper arguments: these tests pass NULL
# explicitly.

library(testthat)

# Level names the old code read as genders, so a waiting state here proves no name-guess.
# Female ages 0-9 (30 rows), Male ages 10-29 (40 rows): with bin width 10 the sides differ in
# every band, so a swapped or guessed assignment cannot match the expected table.
pg_data <- function(levels = c("Female", "Male")) {
    data.frame(age = c(rep(0:9, 3), rep(10:29, 2)),
               sex = factor(c(rep("Female", 30), rep("Male", 40)), levels = levels))
}

pg_run <- function(d, female, male, ...) {
    agepyramid(data = d, age = "age", gender = "sex", female = female, male = male,
               bin_width = 10, ...)
}

pg_expect_nothing_drawn <- function(res, info = NULL) {
    expect_equal(res$pyramidTable$rowCount, 0L, info = info)
    expect_null(res$plot$state, info = info)
    expect_false(grepl("Data Summary", res$dataInfo$content %||% "", fixed = TRUE), info = info)
}

pg_bands <- function(res) {
    df <- res$pyramidTable$asDF
    df[, c("Pop", "Female", "Male", "Female_Pct", "Male_Pct")]
}

pg_expected <- data.frame(
    Pop = c("20-29", "10-19", "0-9", "Total"),
    Female = c(0, 0, 30, 30), Male = c(20, 20, 0, 40),
    Female_Pct = c(0, 0, 100, 100), Male_Pct = c(50, 50, 0, 100),
    stringsAsFactors = FALSE)

test_that("female and male allow none and carry no default (schema)", {
    skip_if_not_installed("yaml")
    path <- testthat::test_path("..", "..", "jamovi", "agepyramid.a.yaml")
    skip_if_not(file.exists(path))
    opts <- yaml::read_yaml(path)$options
    for (nm in c("female", "male")) {
        o <- Filter(function(x) identical(x$name, nm), opts)[[1]]
        expect_equal(o$type, "Level", info = nm)
        expect_true(isTRUE(o$allowNone), info = nm)
        expect_null(o$default, info = nm)
    }
})

test_that("both levels empty: INFO waiting with counts, nothing drawn, no name or order guess", {
    cases <- list(
        list(d = pg_data(), counts = "Female (n = 30), Male (n = 40)"),
        # male-first level order: the old fallback put "Male" on the female side
        list(d = pg_data(c("Male", "Female")), counts = "Male (n = 40), Female (n = 30)"))
    for (cs in cases) for (unset in list(NULL, "", NA_character_)) {
        info <- paste(cs$counts, deparse(unset))
        expect_no_error(res <- pg_run(cs$d, female = unset, male = unset))
        notices <- res$notices$content
        expect_match(notices, "NOTE: Choose the Female and Male levels", fixed = TRUE, info = info)
        expect_match(notices, "'Female level'", fixed = TRUE, info = info)
        expect_match(notices, "'Male level'", fixed = TRUE, info = info)
        expect_match(notices, cs$counts, fixed = TRUE, info = info)
        expect_false(grepl("ERROR", notices, fixed = TRUE), info = info)
        expect_false(grepl("WARNING", notices, fixed = TRUE), info = info)
        pg_expect_nothing_drawn(res, info)
    }
})

test_that("one level chosen while another observed level remains: waiting, never filled in", {
    d3 <- pg_data()
    d3$sex <- factor(c(rep("Female", 30), rep("Male", 35), rep("Unknown", 5)))
    cases <- list(
        list(d = pg_data(), female = "Female", male = NULL,
             title = "NOTE: Choose the Male level", others = "Male (n = 40)"),
        list(d = pg_data(), female = NULL, male = "Male",
             title = "NOTE: Choose the Female level", others = "Female (n = 30)"),
        list(d = d3, female = "Female", male = NULL,
             title = "NOTE: Choose the Male level", others = "Male (n = 35), Unknown (n = 5)"))
    for (cs in cases) {
        info <- cs$title
        expect_no_error(res <- pg_run(cs$d, female = cs$female, male = cs$male))
        notices <- res$notices$content
        expect_match(notices, cs$title, fixed = TRUE, info = info)
        expect_match(notices, cs$others, fixed = TRUE, info = info)
        expect_false(grepl("ERROR", notices, fixed = TRUE), info = info)
        pg_expect_nothing_drawn(res, info)
    }
})

test_that("a single-level variable with one side chosen runs as a single-gender cohort", {
    female_only <- data.frame(age = rep(0:9, 3), sex = rep("Female", 30))
    res <- pg_run(female_only, female = "Female", male = NULL)
    tab <- res$pyramidTable$asDF
    expect_equal(tab$Female[tab$Pop == "Total"], 30)
    expect_equal(tab$Male[tab$Pop == "Total"], 0)
    expect_match(res$dataInfo$content, "Single-gender (Female)", fixed = TRUE)
    expect_equal(unique(res$plot$state$Gender), "Female")

    # An unused factor level is not an observed level: still single-gender.
    male_only <- data.frame(age = rep(10:29, 2),
                            sex = factor(rep("Male", 40), levels = c("Female", "Male")))
    res2 <- pg_run(male_only, female = NULL, male = "Male")
    tab2 <- res2$pyramidTable$asDF
    expect_equal(tab2$Male[tab2$Pop == "Total"], 40)
    expect_equal(tab2$Female[tab2$Pop == "Total"], 0)
    expect_match(res2$dataInfo$content, "Single-gender (Male)", fixed = TRUE)

    # ...but with BOTH sides empty it waits: the side is never read from the label.
    res3 <- pg_run(female_only, female = NULL, male = NULL)
    expect_match(res3$notices$content, "Female (n = 30)", fixed = TRUE)
    pg_expect_nothing_drawn(res3)
})

test_that("both levels chosen: the table follows the user's assignment", {
    res <- pg_run(pg_data(), female = "Female", male = "Male")
    expect_equal(pg_bands(res), pg_expected, ignore_attr = TRUE)
    expect_false(grepl("Choose the", res$notices$content, fixed = TRUE))
    expect_match(res$dataInfo$content, "Data Summary", fixed = TRUE)

    # Swapped on purpose: obeyed, not corrected from the names.
    swapped <- pg_bands(pg_run(pg_data(), female = "Male", male = "Female"))
    expect_equal(swapped$Female, pg_expected$Male)
    expect_equal(swapped$Male, pg_expected$Female)
})

test_that("the same level on both sides keeps the existing error and draws nothing", {
    res <- pg_run(pg_data(), female = "Female", male = "Female")
    expect_match(res$notices$content, "ERROR: Female and Male levels must be different", fixed = TRUE)
    pg_expect_nothing_drawn(res)
})

test_that("a chosen level the variable does not have is an error, never a reason to wait", {
    # Codex review 2026-10-18: a stale selection used to wait (one side) or silently empty a side.
    for (args in list(list(female = "Missing", male = NULL), list(female = "Female", male = "Missing"))) {
        res <- do.call(pg_run, c(list(d = pg_data()), args))
        notices <- res$notices$content
        expect_match(notices, "ERROR: Gender level not found", fixed = TRUE)
        expect_match(notices, "The level 'Missing' chosen under", fixed = TRUE)
        expect_false(grepl("NOTE: Choose the", notices, fixed = TRUE))
        pg_expect_nothing_drawn(res)
    }
    # a declared but unused level is still a level: a male-only cohort may name both sides
    male_only <- data.frame(age = rep(10:29, 2), sex = factor(rep("Male", 40), levels = c("Female", "Male")))
    res <- pg_run(male_only, female = "Female", male = "Male")
    expect_false(grepl("Gender level not found", res$notices$content, fixed = TRUE))
    expect_gt(res$pyramidTable$rowCount, 0L)
})

test_that("a vector of levels is not one level: it waits instead of being recycled", {
    res <- pg_run(pg_data(), female = c("Female", "Male"), male = NULL)
    expect_match(res$notices$content, "NOTE: Choose the Female and Male levels", fixed = TRUE)
    pg_expect_nothing_drawn(res)
})

test_that("ready -> waiting -> ready on one analysis object leaves no stale numbers", {
    ns <- asNamespace("ClinicoPath")
    o <- get("agepyramidOptions", ns)$new(age = "age", gender = "sex", female = "Female",
                                          male = "Male", bin_width = 10, enableGGCharts = TRUE)
    a <- get("agepyramidClass", ns)$new(options = o, data = pg_data())
    priv <- a$.__enclos_env__$private
    # Two steps: the option is an R6 object, so assigning through the local binding
    # mutates the one `o` holds (jamovi's clearWith does not run on an R6 re-run).
    male_opt <- o$option("male")
    female_opt <- o$option("female")

    priv$.run()
    expect_equal(pg_bands(a$results), pg_expected, ignore_attr = TRUE)
    expect_false(is.null(a$results$plotGGCharts$state))

    male_opt$value <- NULL
    priv$.run()
    expect_match(a$results$notices$content, "Choose the Male level", fixed = TRUE)
    pg_expect_nothing_drawn(a$results, "male cleared")
    expect_null(a$results$plotGGCharts$state)
    expect_null(a$results$pyramidTable$notes$pct)

    male_opt$value <- "Male"
    priv$.run()
    expect_equal(pg_bands(a$results), pg_expected, ignore_attr = TRUE)
    expect_false(grepl("Choose the", a$results$notices$content, fixed = TRUE))

    # same through the existing equal-levels error
    female_opt$value <- "Male"
    priv$.run()
    expect_match(a$results$notices$content, "must be different", fixed = TRUE)
    pg_expect_nothing_drawn(a$results, "equal levels")

    female_opt$value <- "Female"
    priv$.run()
    expect_equal(pg_bands(a$results), pg_expected, ignore_attr = TRUE)
})
