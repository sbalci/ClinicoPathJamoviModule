# Regression coverage for the five defects found by the post-fix audit pass.
# Each block reproduces the exact user-visible symptom, not the code path.

test_that("REG-1 jjcorrmat does not blame pairwise deletion for a group split", {
    set.seed(1)
    d <- data.frame(
        a = rnorm(60), b = rnorm(60), c = rnorm(60),
        g = factor(rep(c("x", "y"), times = c(15, 45)))
    )
    expect_false(anyNA(d))

    res <- jjcorrmat(data = d, dep = c("a", "b", "c"), grvar = "g",
                     showexplanations = TRUE)
    txt <- paste0(res$interpretation$content, " ", res$summary$content)

    # A complete dataset split into unequal groups is not pairwise deletion.
    expect_false(grepl("pairwise deletion", txt, fixed = TRUE))
    expect_true(grepl("per group", txt, fixed = TRUE))
})

test_that("REG-1b the pairwise-deletion wording still appears when it is true", {
    set.seed(2)
    d <- data.frame(a = rnorm(60), b = rnorm(60), c = rnorm(60))
    d$b[1:20] <- NA

    res <- jjcorrmat(data = d, dep = c("a", "b", "c"), naHandling = "pairwise",
                     showexplanations = TRUE)
    txt <- paste0(res$interpretation$content, " ", res$summary$content)
    expect_true(grepl("pairwise deletion", txt, fixed = TRUE))
})

test_that("REG-3 jjscatterstats method-substitution notice reaches the panel", {
    set.seed(3)
    d <- data.frame(x = rnorm(80), y = rnorm(80))

    # robust has no ggpubr analogue, so the panel silently draws an ordinary
    # Pearson r. The disclosure used to be written from .plot(), by which time
    # jamovi had already composed the results panel, so it went nowhere.
    res <- jjscatterstats(
        data = d, dep = "x", group = "y",
        typestatistics = "robust",
        addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE
    )
    warn <- res$warnings$content
    expect_false(is.null(warn))
    expect_true(grepl("not showing your analysis type", warn, fixed = TRUE))
})

test_that("REG-3b no notice when the panel agrees with the analysis", {
    set.seed(4)
    d <- data.frame(x = rnorm(80), y = rnorm(80))
    res <- jjscatterstats(
        data = d, dep = "x", group = "y",
        typestatistics = "parametric",
        addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE,
        ggpubrCorrMethod = "pearson"
    )
    warn <- res$warnings$content
    if (!is.null(warn))
        expect_false(grepl("not showing your analysis type", warn, fixed = TRUE))
    else
        succeed()
})

test_that("REG-4 jjridges Ridge Height Scale changes the violin plot", {
    set.seed(5)
    d <- data.frame(
        v = c(rnorm(60, 0), rnorm(60, 3)),
        g = factor(rep(c("a", "b"), each = 60))
    )
    render <- function(sc) {
        res <- jjridges(data = d, x_var = "v", y_var = "g",
                        plot_type = "violin_ridges", scale = sc)
        f <- tempfile(fileext = ".svg")
        svglite::svglite(f, width = 6, height = 4); on.exit(unlink(f), add = TRUE)
        print(res$plot); grDevices::dev.off()
        readLines(f, warn = FALSE)
    }
    expect_false(identical(render(0.5), render(3)))
})

test_that("REG-5 jjwithinstats gives an actionable message for a text measurement", {
    set.seed(6)
    n <- 30
    d <- data.frame(
        t1 = as.character(round(rnorm(n, 10, 1), 1)),
        t2 = as.character(round(rnorm(n, 12, 1), 1)),
        stringsAsFactors = FALSE
    )
    # Must not be the raw "non-numeric argument to binary operator" from quantile().
    err <- tryCatch({ jjwithinstats(data = d, dep1 = "t1", dep2 = "t2"); NA_character_ },
                    error = function(e) conditionMessage(e))
    expect_false(is.na(err))
    expect_false(grepl("non-numeric argument to binary operator", err, fixed = TRUE))
    expect_true(grepl("numeric", err, fixed = TRUE))
})

test_that("REG-5b jjwithinstats still runs on ordinary numeric measurements", {
    set.seed(7)
    n <- 30
    d <- data.frame(t1 = rnorm(n, 10, 1), t2 = rnorm(n, 12, 1))
    res <- jjwithinstats(data = d, dep1 = "t1", dep2 = "t2")
    expect_true(inherits(res$plot, "Image"))
})

test_that("C9 jwaffle rejects one real category padded with NA", {
    d <- data.frame(g = factor(c(rep("a", 40), rep(NA, 10)), levels = c("a", "b")))
    expect_error(jwaffle(data = d, groups = "g"), "Only one category")

    d0 <- data.frame(g = factor(rep(NA_character_, 50), levels = c("a", "b")))
    expect_error(jwaffle(data = d0, groups = "g"), "no non-missing values")

    ok <- data.frame(g = factor(rep(c("a", "b"), 25)))
    expect_silent_ok <- jwaffle(data = ok, groups = "g")
    expect_true(inherits(expect_silent_ok$plot, "Image"))
})
