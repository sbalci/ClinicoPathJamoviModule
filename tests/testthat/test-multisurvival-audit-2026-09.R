# Regressions for the 2026-09 audit remediation of multisurvival.
#
# Behavioural tests drive the REAL .cox_model_impl() taken off the R6
# generator against a recording stub (same harness as
# test-singlearm-median-zero-event.R), so the assertions are about the
# notices the shipped code actually emits. Source-level tests pin the HTML
# escaping and the finalfit wiring that cannot be exercised without jamovi.
# Everything skips (never fails) when R/*.b.R is absent (installed-package
# check).

suppressWarnings(suppressMessages({
  library(testthat)
  library(survival)
}))

.msrc <- function(file) {
  for (p in c(file.path("../../R", file), file.path("../R", file), file.path("R", file)))
    if (file.exists(p)) return(p)
  testthat::skip(paste0("R/", file, " not available (installed-package check)"))
}

.b_lines <- function() readLines(.msrc("multisurvival.b.R"), warn = FALSE)

# ---------------------------------------------------------------------------
# Harness: run the shipped .cox_model_impl() on a data frame
# ---------------------------------------------------------------------------
.cox_run <- function(df, explanatory = character(0), contexpl = character(0)) {
  src <- .msrc("multisurvival.b.R")
  e <- new.env(parent = globalenv())
  for (f in c("utils.R", "survival_utils.R", "multisurvival-interactions.R", "multisurvival.b.R"))
    suppressWarnings(suppressMessages(sys.source(.msrc(f), envir = e)))
  # CRAN jmvcore has no asFormula(); see test-multisurvival-interactions.R.
  if (!exists("asFormula", envir = asNamespace("jmvcore"), inherits = FALSE))
    assign(".asSurvivalFormula", function(x, env = parent.frame()) stats::as.formula(x), envir = e)

  pm <- e$multisurvivalClass$private_methods
  rec <- new.env(parent = emptyenv()); rec$msgs <- character()

  stub <- new.env(parent = e)
  stub$. <- function(x, ...) x
  stub$self <- list(
    options = list(use_stratify = FALSE, stratvar = NULL,
                   explanatory = if (length(explanatory)) explanatory else NULL,
                   contexpl = if (length(contexpl)) contexpl else NULL,
                   interactions = list(), multievent = FALSE),
    results = list(interactionTest = list(rowCount = 0), subgroupHR = list(rowCount = 0))
  )
  stub$private <- list(
    .cleandata = function() list(
      cleanData = df, mydata_labelled = df,
      myexplanatory_labelled = explanatory, mycontexpl_labelled = contexpl,
      mystratvar_labelled = NULL),
    .addHtmlMessage = function(type, title, message)
      rec$msgs <- c(rec$msgs, paste0(type, " | ", title, " | ", message)),
    .isCompetingRisk = function(...) FALSE,
    .checkpoint = function(...) invisible(NULL)
  )
  f <- pm$.cox_model_impl
  environment(f) <- stub
  fit <- f()
  list(fit = fit, msgs = rec$msgs)
}

test_that("central coxph convergence warning reaches the results pane as a strongWarning naming the term", {
  set.seed(11)
  n <- 120
  df <- data.frame(
    mytime = round(rexp(n, 0.2) + 1, 2),
    grp = factor(rep(c("A", "B"), each = n / 2))
  )
  # Every event is in group A: monotone likelihood for grpB.
  df$myoutcome <- ifelse(df$grp == "A", rbinom(n, 1, 0.6), 0L)

  out <- .cox_run(df, explanatory = "grp")
  expect_s3_class(out$fit, "coxph")
  conv <- grep("^strongWarning \\| Cox model convergence problem", out$msgs, value = TRUE)
  expect_length(conv, 1L)
  expect_match(conv, "grpB", fixed = TRUE)
  expect_match(conv, "coefficient may be infinite", fixed = TRUE)
})

test_that("PH violation is a strongWarning that names the term and its p-value", {
  set.seed(7)
  n <- 300
  g <- factor(rep(c("A", "B"), each = n / 2))
  df <- data.frame(
    mytime = ifelse(g == "A", rexp(n, 1), 2 + rexp(n, 2)),
    myoutcome = 1L, g = g
  )
  out <- .cox_run(df, explanatory = "g")
  ph <- grep("^strongWarning \\| Proportional hazards violation", out$msgs, value = TRUE)
  expect_length(ph, 1L)
  expect_match(ph, "g (p", fixed = TRUE)
  expect_match(ph, "p [<=] ")
  expect_false(any(grepl("^warning \\| Proportional hazards violation", out$msgs)))
})

test_that("a well-behaved model emits neither convergence nor PH strong warnings", {
  set.seed(3)
  n <- 200
  x <- rnorm(n)
  df <- data.frame(mytime = rexp(n, exp(0.3 * x)), myoutcome = rbinom(n, 1, 0.8), x = x)
  out <- .cox_run(df, contexpl = "x")
  expect_s3_class(out$fit, "coxph")
  expect_false(any(grepl("Cox model convergence problem|Proportional hazards violation", out$msgs)))
})

# ---------------------------------------------------------------------------
# finalfit wiring: interaction terms only in the multivariable model
# ---------------------------------------------------------------------------
test_that("finalfit receives interaction terms via explanatory_multi only", {
  src <- .b_lines()
  # No longer appended to the univariable `explanatory` vector ...
  expect_false(any(grepl("explanatory_formula <- c\\(\\s*$", src) &
                     grepl("interactionTermsForFinalfit", c(src[-1], ""))))
  # ... but present in both explanatory_multi calls.
  expect_true(any(grepl("explanatory_multi = c(explanatory_formula, interaction_ff)", src, fixed = TRUE)))
  expect_true(any(grepl("explanatory_multi = c(covars_multi, interaction_ff,", src, fixed = TRUE)))
})

test_that("finalfit keeps the interaction row with '-' in the univariable column (contract the fix relies on)", {
  skip_if_not_installed("finalfit")
  d <- survival::lung
  d$status <- d$status - 1
  d$sex <- factor(d$sex, labels = c("M", "F"))
  d$ph <- factor(d$ph.ecog > 0, labels = c("G0", "G1plus"))
  d <- d[!is.na(d$ph), ]
  main <- c("sex", "ph")
  tb <- suppressWarnings(suppressMessages(finalfit::finalfit(
    d, "Surv(time, status)", explanatory = main,
    explanatory_multi = c(main, "sex:ph"), cont_cut = 0, metrics = TRUE)))[[1]]
  tb[is.na(tb)] <- "-"
  ref <- suppressWarnings(suppressMessages(finalfit::finalfit(
    d, "Surv(time, status)", explanatory = main, cont_cut = 0)))
  irow <- grepl(":", tb[[1]])
  expect_equal(sum(irow), 1L)
  expect_equal(tb[["HR (univariable)"]][irow], "-")
  # Main-effect univariable HRs are untouched by the interaction.
  expect_equal(tb[["HR (univariable)"]][!irow], ref[["HR (univariable)"]])
})

# ---------------------------------------------------------------------------
# Source-level pins: HTML sinks escape user data; dead schema removed
# ---------------------------------------------------------------------------
test_that("factor-level text is HTML-escaped at every Html sink flagged by the audit", {
  src <- .b_lines()
  hit <- function(pat) any(grepl(pat, src, fixed = TRUE))
  expect_true(hit("level = htmltools::htmlEscape(row$strata)"))
  expect_true(hit("variable = htmltools::htmlEscape(adj_var), level = htmltools::htmlEscape(r$factor)"))
  expect_true(hit("terms = htmltools::htmlEscape(paste(na_coefs, collapse = \", \"))"))
  expect_true(hit("htmltools::htmlEscape(section_name)"))
  expect_true(hit("paste(htmltools::htmlEscape(sections[[section_name]]), collapse=\"<br>\")"))
  expect_true(hit("paste(htmltools::htmlEscape(tech_details), collapse=\"<br>\")"))
})

test_that("stratified nomogram path no longer hides the summary it then writes to", {
  src <- .b_lines()
  i <- grep("for (nm in c(\"nomogramHeading\", \"plot_nomogram\", \"nomogram_display\",", src, fixed = TRUE)
  expect_length(i, 1L)
  expect_false(grepl("nomogramSummary", src[i + 1L], fixed = TRUE))
})

test_that("dead schema is gone: risk_score_analysis output, 'marginal' ac_method branch, .plot_adj dead formula", {
  src <- .b_lines()
  expect_false(any(grepl("results$risk_score_analysis$", src, fixed = TRUE)))
  expect_false(any(grepl("identical(method, \"marginal\")", src, fixed = TRUE)))
  expect_false(any(grepl("\"marginal\" = .(", src, fixed = TRUE)))
  live <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("myformula <- .asSurvivalFormula(myformula)", live, fixed = TRUE)))

  ry <- for (p in c("../../jamovi", "../jamovi", "jamovi"))
    if (file.exists(file.path(p, "multisurvival.r.yaml"))) break
  ryp <- file.path(p, "multisurvival.r.yaml")
  skip_if_not(file.exists(ryp), "jamovi/multisurvival.r.yaml not available")
  ry <- yaml::read_yaml(ryp)
  nms <- vapply(ry$items, function(it) it$name, character(1))
  expect_false("risk_score_analysis" %in% nms)
  expect_true("risk_score_analysis2" %in% nms)
  refs <- lapply(ry$items[nms %in% c("survMetricsTable", "survMetricsPlot")], `[[`, "refs")
  expect_true(all(vapply(refs, function(r) "riskRegression" %in% unlist(r), logical(1))))
})
