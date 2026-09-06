# Regression cover for the 2026-09 audit fixes to alluvial. Each block fails
# against the pre-audit backend or schema.

library(testthat)

al <- function(d, ...) {
  args <- list(data = d, condensationvar = NULL, fillGgalluvial = NULL, weight = NULL)
  do.call(alluvial, utils::modifyList(args, list(...)))
}
al_txt <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(x)))

al_obj <- function(d, ...) {
  ns <- asNamespace("ClinicoPath")
  get("alluvialClass", ns)$new(options = get("alluvialOptions", ns)$new(...), data = d)
}
al_priv <- function(d, ...) al_obj(d, ...)$.__enclos_env__$private

# Both renderers catch every error and draw an explanation into the image, so a
# plain "returned TRUE" proves nothing: turn that fallback into an error.
draw <- function(a, item, fn) {
  priv <- a$.__enclos_env__$private
  unlockBinding(".messagePlot", priv)
  priv$.messagePlot <- function(text) stop("render fell back to a message plot: ", text)
  lockBinding(".messagePlot", priv)
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  on.exit(grDevices::dev.off(), add = TRUE)
  priv[[fn]](a$results[[item]], ggtheme = ggplot2::theme_gray(), theme = list())
}

x_labels <- function(p) ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
stratum_x <- function(p, levels) {
  d <- ggplot2::ggplot_build(p)$data[[2]]   # geom_stratum layer
  unique(d$x[as.character(d$stratum) %in% levels])
}

set.seed(31); n <- 80
d3 <- data.frame(S = factor(sample(c("s1", "s2"), n, TRUE)),
                 G = factor(sample(c("g1", "g2", "g3"), n, TRUE)),
                 R = factor(sample(c("r1", "r2"), n, TRUE)))

test_that("vars is optional for programmatic callers (default: NULL)", {
  expect_no_error(res <- alluvial(data = d3))
  expect_match(al_txt(res$todo$content), "Alluvial Diagrams")
})

test_that("GG Alluvial axes carry the variable names, not continuous ticks", {
  priv <- al_priv(d3, vars = c("S", "G", "R"), engine = "ggalluvial",
                  condensationvar = NULL, fillGgalluvial = NULL, weight = NULL)
  p <- priv$.createGgalluvialPlot(d3, vars = c("S", "G", "R"), fill_var = "S")
  expect_equal(x_labels(p), c("S", "G", "R"))
  expect_equal(stratum_x(p, c("s1", "s2")), 1)
  # a reversed order moves the first variable to the far axis AND labels it there
  p_rev <- priv$.createGgalluvialPlot(d3, vars = c("R", "G", "S"), fill_var = "S")
  expect_equal(x_labels(p_rev), c("R", "G", "S"))
  expect_equal(stratum_x(p_rev, c("s1", "s2")), 3)
})

test_that("flow direction resolves to reverse/flip once, with the legacy orientation shortcut", {
  priv <- al_priv(d3, vars = c("S", "G"), condensationvar = NULL, fillGgalluvial = NULL, weight = NULL)
  r <- priv$.resolveFlowDirection
  expect_equal(r("vert", "left_right"), list(reverse = FALSE, flip = FALSE))
  expect_equal(r("vert", "right_left"), list(reverse = TRUE, flip = FALSE))
  expect_equal(r("vert", "top_bottom"), list(reverse = TRUE, flip = TRUE))
  expect_equal(r("vert", "bottom_top"), list(reverse = FALSE, flip = TRUE))
  expect_equal(r("horr", "left_right"), list(reverse = TRUE, flip = TRUE))   # legacy shortcut
  expect_equal(r("horr", "right_left"), list(reverse = TRUE, flip = FALSE))  # flowDirection wins
})

test_that("a variable literally named x renders under GG Alluvial in every direction", {
  dx <- d3; names(dx)[1] <- "x"
  for (dir in c("left_right", "right_left", "top_bottom", "bottom_top")) {
    a <- al_obj(dx, vars = c("x", "G", "R"), engine = "ggalluvial", flowDirection = dir,
                condensationvar = NULL, fillGgalluvial = NULL, weight = NULL)
    a$.__enclos_env__$private$.run()
    expect_false(is.null(a$results$plot$state), info = dir)
    expect_no_error(draw(a, "plot", ".plot"))
  }
})

test_that("the condensation panel draws numeric variables as their recorded values", {
  dn <- d3; dn$Gn <- as.numeric(dn$G)   # grade coded 1/2/3
  a <- al_obj(dn, vars = c("S", "Gn", "R"), condensationvar = "S",
              fillGgalluvial = NULL, weight = NULL)
  a$.__enclos_env__$private$.run()
  st <- a$results$plot2$state
  expect_false(is.null(st))
  expect_true(all(vapply(st$data, is.factor, logical(1))))
  expect_equal(levels(st$data$Gn), c("1", "2", "3"))
  # plot_condensation() no longer bins, so its "bins ... are empty" warning is gone
  expect_no_warning(draw(a, "plot2", ".plot2"))
})

test_that("axis and condensation variables share one continuous-variable cutoff", {
  set.seed(5); m <- 200
  dc <- data.frame(A = factor(sample(c("a", "b"), m, TRUE)),
                   B = factor(sample(c("p", "q"), m, TRUE)),
                   k15 = sample(1:15, m, TRUE), k25 = sample(1:25, m, TRUE))
  # 15 distinct numbers: accepted as an axis, so accepted as condensation variable too
  res <- al(dc, vars = c("A", "B"), condensationvar = "k15")
  expect_false(is.null(res$plot2$state))
  expect_match(al_txt(res$notices$content), "Too Many Categories")
  # 25 distinct numbers: refused in both roles
  res25 <- al(dc, vars = c("A", "B"), condensationvar = "k25")
  expect_null(res25$plot2$state)
  expect_match(al_txt(res25$condensationWarning$content), "appears continuous")
  expect_null(al(dc, vars = c("A", "k25"))$plot$state)
})

test_that("a high-cardinality fill variable is flagged", {
  df <- d3; df$site <- factor(sample(sprintf("site%02d", 1:12), n, TRUE))
  df$few <- factor(sample(c("u", "v"), n, TRUE))
  many <- al_txt(al(df, vars = c("S", "G"), engine = "ggalluvial", fillGgalluvial = "site")$notices$content)
  expect_match(many, "Too Many Fill Categories")
  expect_match(many, "site")
  few <- al_txt(al(df, vars = c("S", "G"), engine = "ggalluvial", fillGgalluvial = "few")$notices$content)
  expect_false(grepl("Too Many Fill Categories", few))
})

test_that("schema: flow table clears on the fill variable and marginal plots are gated by engine only", {
  r <- yaml::read_yaml("../../jamovi/alluvial.r.yaml")
  ft <- Filter(function(i) i$name == "flowTable", r$items)[[1]]
  expect_true("fillGgalluvial" %in% ft$clearWith)
  u <- readLines("../../jamovi/alluvial.u.yaml")
  i <- grep("name: marg$", u)
  expect_match(u[i + 1], "enable: \\(engine:easyalluvial\\)$")
})
