# Checks for the jjstatsplot module that follow up the jamovi library review (one test_that per class).

# A result that changes with the random seed names that seed next to it, and the seed is an option the
# user can change (it used to be a constant inside five analyses). Parametric results show no seed.
test_that("seed-dependent jjstatsplot statistics show the seed that drew them", {
  skip_if_not(exists("jjbetweenstatsClass") && exists("jjridgesClass"))
  skip_if_not_installed("ggstatsplot")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  shown <- "Random seed: 777"
  run <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    quiet(a$init())
    quiet(a$run())
    a
  }
  # Every caption the renderer hands to ggplot_build() while drawing its main plot.
  captions <- function(analysis) {
    seen <- character()
    suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                           tracer = bquote(assign("seen", c(get("seen", envir = .(environment())),
                                                            as.character(plot$labels$caption)),
                                                  envir = .(environment())))))
    on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    quiet(analysis$.__enclos_env__$private$.plot(analysis$results$plot, ggplot2::theme_grey(), list()))
    seen
  }

  # an existing caption keeps its own line above the seed
  has_seed <- function(caps) any(grepl(shown, caps, fixed = TRUE))

  set.seed(3)
  n <- 60
  g <- factor(rep(c("a", "b", "c"), each = n / 3))
  d <- data.frame(g, y = stats::rnorm(n, as.integer(g)), x = stats::rnorm(n))
  d$z <- d$x + stats::rnorm(n, 0, 0.5)
  d$before <- stats::rnorm(n, 10)
  d$after <- d$before + stats::rnorm(n, 0.5)

  expect_true(has_seed(captions(run("jjbetweenstats", d, dep = "y", group = "g",
                                      typestatistics = "robust", resultssubtitle = TRUE, seed = 777))))
  expect_false(has_seed(captions(run("jjbetweenstats", d, dep = "y", group = "g",
                                       typestatistics = "parametric", resultssubtitle = TRUE, seed = 777))))
  expect_true(has_seed(captions(run("jjwithinstats", d, dep1 = "before", dep2 = "after",
                                      typestatistics = "robust", resultssubtitle = TRUE, seed = 777))))
  expect_true(has_seed(captions(run("jjhistostats", d, dep = "y",
                                      typestatistics = "robust", resultssubtitle = TRUE, seed = 777))))
  expect_true(has_seed(captions(run("jjdotchart", d, dep = "y", group = "g", typestatistics = "robust", seed = 777))))
  expect_true(has_seed(captions(run("statsplot2", d, dep = "y", group = "g", distribution = "r", seed = 777))))
  skip_if_not_installed("BayesFactor")
  expect_true(has_seed(captions(run("jjscatterstats", d, dep = "x", group = "z",
                                      typestatistics = "bayes", resultssubtitle = TRUE, seed = 777))))
  expect_true(shown %in% unname(vapply(run("jjcorrmat", d, dep = c("x", "z", "y"), typestatistics = "bayes",
                                           bayesseed = 777)$results$table$notes, function(x) x$note, "")))
  two <- droplevels(d[d$g != "c", ])
  expect_match(run("jjdotplotstats", two, dep = "y", group = "g", typestatistics = "bayes", seed = 777,
                   showexplanation = TRUE)$results$explanation$content,
               shown, fixed = TRUE)

  ridges <- run("jjridges", d, x_var = "y", y_var = "g", show_stats = TRUE, effsize_type = "cliff_delta", seed = 777)
  expect_true(shown %in% unname(vapply(ridges$results$tests$notes, function(x) x$note, "")))
})

# Two more surfaces carry seed-dependent numbers besides the subtitle: the Bayes-factor caption a
# PARAMETRIC test draws when bfmessage is on, and centrality_description's bootstrap interval, which
# jjdotchart prints as Lower/Upper. The contingency analyses (bar, pie) were unseeded entirely.
test_that("the Bayes-factor caption, contingency plots and centrality intervals name their seed", {
  skip_if_not(exists("jjbarstatsClass") && exists("jjpiestatsClass") && exists("jjdotchartClass"))
  skip_if_not_installed("BayesFactor")
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  shown <- "Random seed: 777"
  run <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    quiet(a$init())
    quiet(a$run())
    a
  }
  captions <- function(analysis, renderer = ".plot") {
    seen <- character()
    suppressMessages(trace("ggplot_build", where = asNamespace("ggplot2"), print = FALSE,
                           tracer = bquote(assign("seen", c(get("seen", envir = .(environment())),
                                                            as.character(plot$labels$caption)),
                                                  envir = .(environment())))))
    on.exit(suppressMessages(untrace("ggplot_build", where = asNamespace("ggplot2"))))
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    image <- if (renderer == ".plot1") analysis$results$plot1 else analysis$results$plot
    quiet(analysis$.__enclos_env__$private[[renderer]](image, ggplot2::theme_grey(), list()))
    seen
  }
  has_seed <- function(caps) any(grepl(shown, caps, fixed = TRUE))

  set.seed(4)
  n <- 80
  d <- data.frame(g = factor(rep(c("a", "b"), each = n / 2)),
                  y = stats::rnorm(n) + rep(c(0, 0.6), each = n / 2),
                  f = factor(sample(c("p", "q", "r"), n, TRUE)))

  # contingency tables: Bayesian subtitle, and the Bayes-factor caption of a frequentist run
  expect_true(has_seed(captions(run("jjbarstats", d, dep = "f", group = "g",
                                    typestatistics = "bayes", seed = 777))))
  expect_true(has_seed(captions(run("jjbarstats", d, dep = "f", group = "g",
                                    typestatistics = "parametric", bfmessage = TRUE, seed = 777))))
  expect_true(has_seed(captions(run("jjpiestats", d, dep = "f", group = "g",
                                    typestatistics = "bayes", seed = 777), ".plot1")))

  # the Bayes-factor caption of a parametric two-group comparison is sampled too
  expect_true(has_seed(captions(run("jjbetweenstats", d, dep = "y", group = "g",
                                    typestatistics = "parametric", resultssubtitle = TRUE,
                                    bfmessage = TRUE, seed = 777))))

  # centrality_description bootstraps its interval for every test, parametric included
  dot <- run("jjdotchart", d, dep = "y", group = "f", typestatistics = "parametric", seed = 777)
  expect_true(shown %in% unname(vapply(dot$results$summary$notes, function(x) x$note, "")))
  expect_true(has_seed(captions(dot)))

  # and a two-group nonparametric comparison is analytic: it must claim no seed
  two <- run("jjdotplotstats", d, dep = "y", group = "g", typestatistics = "nonparametric",
             seed = 777, showexplanation = TRUE)
  expect_false(grepl("random seed", two$results$explanation$content, ignore.case = TRUE))
})

# library-audit 2026-09-16 meddecide [LOW] DONE (same class): tables whose row set is fixed by the code or by
#   an option have their rows before .run(), under stable named keys (these three also accumulated rows
#   on every rerun, because nothing cleared them)
test_that("fixed-row jjstatsplot tables are scaffolded before .run()", {
  skip_if_not(exists("jjsegmentedtotalbarClass"))
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  set.seed(9)
  n <- 30
  d <- data.frame(g = factor(sample(c("a", "b", "c"), n, TRUE)), f = factor(sample(c("p", "q"), n, TRUE)),
                  v = stats::runif(n, 1, 10), x = stats::rnorm(n), yb = factor(sample(c("no", "yes"), n, TRUE)))
  st <- after_init("jjsegmentedtotalbar", d, x_var = "g", y_var = "v", fill_var = "f")
  expect_identical(st("detailed_stats"),
                   c("categories", "segments", "total", "min_pct", "max_pct", "mean_pct", "most_variable"))
})

# The same class in two analyses that are still pending (menuGroup ...P), so they ship in no module yet and
# this block runs in the umbrella only. jjcoefstats also had two statistical bugs fixed with it: a logistic
# model inherits "lm", so every glm fell into the linear branch and errored on a NULL R-squared; and the
# Cox "Concordance" row showed model$concordance[1], the count of concordant PAIRS, not the C-index.
test_that("fixed-row tables in the pending jjcoefstats and jjoncoplot are scaffolded before .run()", {
  skip_if_not(exists("jjcoefstatsClass") && exists("jjoncoplotClass"), "pending analyses: umbrella only")
  after_init <- function(name, data, ...) {
    a <- get(paste0(name, "Class"))$new(options = get(paste0(name, "Options"))$new(...), data = data)
    suppressWarnings(suppressMessages(a$init()))
    function(table) unlist(a$results[[table]]$rowKeys)
  }
  set.seed(9)
  n <- 30
  d <- data.frame(v = stats::runif(n, 1, 10), x = stats::rnorm(n), yb = factor(sample(c("no", "yes"), n, TRUE)))
  # the metric rows follow the model type the user chose
  lm_keys <- after_init("jjcoefstats", d, inputMode = "fitmodel", outcome = "v", predictors = "x", modelType = "lm")
  expect_identical(lm_keys("modelMetrics"), c("r_squared", "adj_r_squared", "aic"))
  glm_keys <- after_init("jjcoefstats", d, inputMode = "fitmodel", outcome = "yb", predictors = "x", modelType = "glm")
  expect_identical(glm_keys("modelMetrics"), c("aic", "pseudo_r_squared"))
  onco <- data.frame(sample = sprintf("S%02d", 1:10), TP53 = rbinom(10, 1, 0.5), KRAS = rbinom(10, 1, 0.5))
  oc <- after_init("jjoncoplot", onco, sampleVar = "sample", geneVars = c("TP53", "KRAS"))
  expect_identical(oc("plotInfo"), c("samples", "genes", "plot_type", "color_scheme", "width", "height"))
})
