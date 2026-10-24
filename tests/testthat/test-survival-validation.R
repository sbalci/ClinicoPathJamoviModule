# Independent numerical contracts and regression cases from the 2026-10-23 / 2026-10-24 audits.
.sv_env <- local({
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  if (!file.exists(file.path(root, "R", "survival.b.R"))) root <- getwd()
  if (file.exists(file.path(root, "R", "survival.b.R"))) {
    suppressWarnings(suppressPackageStartupMessages({
      library(jmvcore); library(R6); library(survival); library(ggplot2)
      library(magrittr); library(lubridate); library(labelled); library(cmprsk); library(flexsurv)
    }))
    e <- new.env(parent = globalenv())
    e[[".packageName"]] <- "ClinicoPath"
    for (f in c("utils.R", "utils-formula.R", "utils-eventindicator.R",
                "utils-observed-cif.R", "survival-competingrisks.R",
                "survival.h.R", "survival.b.R")) {
      sys.source(file.path(root, "R", f), envir = e)
    }
    e
  } else {
    NULL
  }
})
.sv_validation_data <- function() {
  set.seed(242)
  data.frame(time = round(rexp(120, .05), 2) + .1,
    status = rbinom(120, 1, .7),
    grp = factor(rep(c("B", "A"), 60), levels = c("B", "A")),
    age = runif(120, 35, 85))
}
.sv_validation_run <- function(d, ...) {
  args <- modifyList(list(data = d, elapsedtime = "time", outcome = "status",
    explanatory = "grp", outcomeLevel = "1", dod = NULL, dooc = NULL,
    awd = NULL, awod = NULL, cutp = "0,2,4,6"), list(...), keep.null = TRUE)
  fn <- if (!is.null(.sv_env)) .sv_env$survival else ClinicoPath::survival
  suppressWarnings(suppressMessages(do.call(fn, args)))
}

test_that("survival KM uses tied event risk sets and excludes missing rows", {
  d <- data.frame(time = c(2, 2, 4, 4, 7, 9, 2, 3, 5, 6, 8, 10, NA),
    status = c(1,0,1,1,0,1,1,0,1,1,0,1,1),
    grp = factor(c(rep("A",6),rep("B",6),"A")))
  r <- .sv_validation_run(d)
  st <- r$survTable$asDF
  expect_equal(st$surv[st$strata=="A" & st$time==4], 5/12)
  expect_equal(st$surv[st$strata=="B" & st$time==6], 5/12)
  expect_equal(sum(r$medianTable$asDF$records), 12)
})

test_that("survival copy-ready median sentences retain estimable confidence limits", {
  r <- .sv_validation_run(.sv_validation_data(), showSummaries = TRUE)
  expect_true(all(is.finite(r$medianTable$asDF$x0_95lcl)))
  expect_true(all(is.finite(r$medianTable$asDF$x0_95ucl)))
  expect_false(grepl("NA to NA", r$copyReadySentencesExplanation$content, fixed = TRUE))
  expect_match(r$copyReadySentencesExplanation$content,"95% CI: 11.92 to 28.2 months",fixed=TRUE)
})

test_that("survival copy-ready text contains its estimated Cox hazard ratio", {
  r <- .sv_validation_run(.sv_validation_data(), showSummaries = TRUE)
  expect_true(any(r$coxTable$asDF$HR_univariable != "-"))
  expect_match(r$copyReadySentencesExplanation$content, "Cox regression estimated", fixed = TRUE)
  expect_match(r$copyReadySentencesExplanation$content,"grp: A vs B",fixed=TRUE)
  expect_match(r$copyReadySentencesExplanation$content,"N = 120 observations",fixed=TRUE)
})

test_that("survival landmark with attained age equals explicitly rebased entry age", {
  d <- .sv_validation_data()
  expected_data <- d[d$time >= 12, ]
  expected_data$age_entry <- expected_data$age + 1
  expected_data$age_exit <- expected_data$age + expected_data$time/12
  oracle <- survival::coxph(survival::Surv(age_entry,age_exit,status) ~ grp,
    data = expected_data)
  actual <- .sv_validation_run(d,uselandmark=TRUE,landmark=12,
    age_adjustment=TRUE,age_variable="age",age_time_scale=TRUE)
  expect_equal(actual$ageTimeScaleTable$asDF$hr,unname(exp(coef(oracle))),tolerance=1e-6)
})

test_that("survival direct standardization refuses unsupported age bands", {
  d <- .sv_validation_data(); d$age <- ifelse(d$grp=="B",40,80)
  actual <- .sv_validation_run(d,age_adjustment=TRUE,age_variable="age",
    age_standardization=TRUE,age_standardization_method="direct")
  tb <- actual$ageStandardizationTable$asDF
  expect_true(nrow(tb)==0 || all(is.na(tb$smr)))
})

test_that("survival age cutpoints retain their documented upper-closed convention", {
  d <- .sv_validation_data(); d$age <- rep(c(49,50,64,65,74,75),20)
  band <- cut(d$age,c(-Inf,50,65,75,Inf),right=TRUE)
  rate <- tapply(d$status,band,sum)/tapply(d$time,band,sum)
  expected <- vapply(levels(d$grp),function(g) {
    i <- d$grp==g
    sum(d$status[i])/sum(d$time[i]*rate[band[i]])
  },numeric(1))
  actual <- .sv_validation_run(d,age_adjustment=TRUE,age_variable="age",age_standardization=TRUE)
  expect_equal(actual$ageStandardizationTable$asDF$smr,unname(round(expected,3)))
})

test_that("survival documents its existing age bands and natural spline knots", {
  path <- testthat::test_path("..", "..", "jamovi", "survival.a.yaml")
  skip_if_not(file.exists(path), "Option source is not installed")
  opts <- yaml::read_yaml(path)$options
  desc <- function(name) opts[[which(vapply(opts,function(x) x$name==name,logical(1)))]]$description
  expect_match(desc("age_group_cutpoints"), "upper cutpoint is included in the preceding group", fixed=TRUE)
  expect_match(desc("rcs_knots"), "interior knots at equally spaced quantiles", fixed=TRUE)
  expect_match(desc("rcs_knots"), "observed minimum and maximum", fixed=TRUE)
  expect_false(grepl("Harrell",desc("rcs_knots"),fixed=TRUE))
})

test_that("survival natural spline results retain their fitted knot convention", {
  d <- .sv_validation_data()
  oracle <- survival::coxph(survival::Surv(time,status) ~ splines::ns(age,df=3)+grp,data=d)
  actual <- .sv_validation_run(d,rcs_analysis=TRUE,rcs_variable="age",rcs_knots=4)
  expect_equal(actual$rcsTestTable$asDF$loglik[2],tail(oracle$loglik,1),tolerance=1e-8)
})

test_that("survival preserves literal NA labels, order and caller data", {
  d <- .sv_validation_data()
  d$grp <- ordered(rep(c("B","A","NA"),40),levels=c("B","NA","A"))
  d$grp[1:3] <- NA; d$grp<-addNA(d$grp)
  attr(d$grp,"label") <- "Treatment label"
  original <- d
  actual <- .sv_validation_run(d)
  expect_equal(sum(actual$medianTable$asDF$records),117)
  expect_setequal(as.character(actual$medianTable$asDF$factor),c("B","A","NA"))
  expect_true(is.ordered(actual$plot$state$cleanData$grp))
  expect_identical(d,original)
})

test_that("survival excludes genuine missing groups encoded as factor levels", {
  d <- .sv_validation_data(); d$grp[1:3] <- NA; d$grp <- addNA(d$grp)
  actual <- .sv_validation_run(d)
  expect_equal(sum(actual$medianTable$asDF$records),117)
})

test_that("survival row labels never change patient age alignment", {
  d <- .sv_validation_data()
  oracle <- survival::coxph(survival::Surv(age,age+time/12,status) ~ grp,data=d)
  rownames(d) <- rev(seq_len(nrow(d)))
  actual <- .sv_validation_run(d,age_adjustment=TRUE,age_variable="age",age_time_scale=TRUE)
  expect_equal(actual$ageTimeScaleTable$asDF$hr,unname(exp(coef(oracle))),tolerance=1e-6)
})

test_that("survival keeps a selected row_names column separate from its positional key", {
  d <- .sv_validation_data(); d$row_names <- d$grp
  expected <- .sv_validation_run(d)
  actual <- .sv_validation_run(d,explanatory="row_names")
  expect_equal(actual$medianTable$asDF$median,expected$medianTable$asDF$median)
  expect_identical(actual$plot$state$cleanData$source_row_index,seq_len(nrow(d)))
  expect_identical(actual$plot$state$cleanData$row_names,rownames(d))
})

test_that("survival aligns age and spline covariates after exclusions with text row labels", {
  d <- .sv_validation_data(); rownames(d)<-paste0("patient-",seq_len(nrow(d)))
  d$time[2]<-NA; d$age[4]<-NA
  keep <- complete.cases(d)
  expected_age <- survival::coxph(survival::Surv(age,age+time/12,status) ~ grp,data=d[keep,])
  expected_spline <- survival::coxph(survival::Surv(time,status) ~ splines::ns(age,df=3)+grp,data=d[keep,])
  actual <- .sv_validation_run(d,age_adjustment=TRUE,age_variable="age",age_time_scale=TRUE,
    rcs_analysis=TRUE,rcs_variable="age")
  expect_equal(actual$ageTimeScaleTable$asDF$hr,unname(exp(coef(expected_age))),tolerance=1e-6)
  expect_equal(actual$rcsTestTable$asDF$loglik[2],tail(expected_spline$loglik,1),tolerance=1e-8)
  expect_identical(actual$plot$state$cleanData$row_names,rownames(d)[!is.na(d$time)])
})

test_that("survival Cox sentences identify aliased references and ordered contrasts", {
  d <- .sv_validation_data()
  d$grp<-factor(d$grp,levels=c("Unused","B","A"))
  actual <- .sv_validation_run(d,showSummaries=TRUE)
  expect_match(actual$copyReadySentencesExplanation$content,"(model contrast)",fixed=TRUE)
  expect_false(grepl("vs Unused",actual$copyReadySentencesExplanation$content,fixed=TRUE))
  d <- .sv_validation_data()
  d$grp<-ordered(rep(c("Low","Middle","High"),40),levels=c("Low","Middle","High"))
  actual <- .sv_validation_run(d,showSummaries=TRUE)
  expect_match(actual$copyReadySentencesExplanation$content,"(model contrast)",fixed=TRUE)
  expect_false(grepl("vs Low",actual$copyReadySentencesExplanation$content,fixed=TRUE))
})

test_that("survival residual diagnostics work independently of ph_cox and respect stratified_cox (IR-01, IR-05)", {
  d <- .sv_validation_data()
  d$band <- factor(ifelse(d$age <= 60, "Young", "Old"))
  r_indep <- .sv_validation_run(d, residual_diagnostics = TRUE, ph_cox = FALSE)
  expect_gt(nrow(r_indep$residualsTable$asDF), 0L)
  expect_false(is.null(r_indep$residualsPlot$state))

  r_strat <- .sv_validation_run(d, stratified_cox = TRUE, strata_variable = "band",
                                ph_cox = TRUE, residual_diagnostics = TRUE)
  mod_strat <- survival::coxph(survival::Surv(time, status) ~ grp + strata(band), data = d)
  expect_equal(r_strat$residualsTable$asDF$martingale[1:5],
               round(unname(residuals(mod_strat, type = "martingale")[1:5]), 4),
               tolerance = 1e-6)
  strat_lrt <- round(summary(mod_strat)$logtest["test"], 2)
  expect_match(r_strat$tCoxtext2$content, paste0("Likelihood ratio test = ", strat_lrt), fixed = TRUE)
})

test_that("survival calibration works on unequal 2-group factors and respects active covariates (IR-02)", {
  d <- .sv_validation_data()
  # Make unequal 2-level factor (80 vs 40)
  d$grp <- factor(c(rep("B", 80), rep("A", 40)), levels = c("B", "A"))
  r_two <- .sv_validation_run(d, calibration_curves = TRUE)
  expect_true(any(!is.na(r_two$calibrationTable$asDF$value)))
  expect_equal(nrow(r_two$calibrationGroupTable$asDF), 2L)

  r_age <- .sv_validation_run(d, calibration_curves = TRUE, age_adjustment = TRUE, age_variable = "age")
  expect_equal(nrow(r_age$calibrationGroupTable$asDF), 5L)

  r_rcs_off <- .sv_validation_run(d, calibration_curves = TRUE, rcs_analysis = FALSE, rcs_variable = "age")
  expect_equal(nrow(r_rcs_off$calibrationGroupTable$asDF), 2L)
})

test_that("survival UI enables timetypeoutput when tint is FALSE and ageAdjustedCox retains spaced names (IR-03, IR-06)", {
  u_path <- testthat::test_path("..", "..", "jamovi", "survival.u.yaml")
  skip_if_not(file.exists(u_path), "UI YAML source not installed")
  u_str <- paste(readLines(u_path), collapse = "\n")
  expect_false(grepl("name: timetypeoutput\\s+label: Output time type\\s+enable: \\(tint\\)", u_str))

  d <- .sv_validation_data()
  d[["Age (years)"]] <- d$age
  r_spaced <- .sv_validation_run(d, age_adjustment = TRUE, age_variable = "Age (years)")
  expect_equal(nrow(r_spaced$ageAdjustedCoxTable$asDF), 2L)
})

test_that("survival low-event gating, competing-risks prose, row-order invariance, and 1-group RMST work (IR-07..IR-10)", {
  d_low <- .sv_validation_data()
  d_low$status <- as.integer(seq_len(nrow(d_low)) <= 6L)
  r_low <- .sv_validation_run(d_low, age_adjustment = TRUE, age_variable = "age",
                              age_interaction = TRUE, age_time_scale = TRUE,
                              rcs_analysis = TRUE, rcs_variable = "age")
  expect_equal(nrow(r_low$coxTable$asDF), 0L)
  expect_equal(nrow(r_low$ageAdjustedCoxTable$asDF), 0L)
  expect_equal(nrow(r_low$ageInteractionTable$asDF), 0L)
  expect_equal(nrow(r_low$ageTimeScaleTable$asDF), 0L)
  expect_true(all(is.na(r_low$rcsTestTable$asDF$loglik)))

  d_cr <- .sv_validation_data()
  d_cr$status_cr <- factor(
    ifelse(d_cr$grp == "B" & seq_len(nrow(d_cr)) <= 70, "DOD",
           ifelse(seq_len(nrow(d_cr)) %% 3 == 0, "DOOC", "AWOD")),
    levels = c("AWOD", "DOOC", "DOD")
  )
  r_cr <- .sv_validation_run(d_cr, outcome = "status_cr", outcomeLevel = NULL,
                             multievent = TRUE, analysistype = "compete",
                             dod = "DOD", dooc = "DOOC", awd = NULL, awod = "AWOD",
                             showSummaries = TRUE)
  expect_false(grepl("Greenwood", r_cr$medianSummary$content, fixed = TRUE))
  expect_false(grepl("NA to NA", r_cr$copyReadySentencesExplanation$content, fixed = TRUE))

  d_perm <- d_cr[c(which(d_cr$grp == "A"), which(d_cr$grp == "B")), ]
  r_pt1 <- .sv_validation_run(d_cr, outcome = "status", person_time = TRUE, time_intervals = "5, 10")
  r_pt2 <- .sv_validation_run(d_perm, outcome = "status", person_time = TRUE, time_intervals = "5, 10")
  expect_identical(r_pt1$personTimeTable$asDF$interval, r_pt2$personTimeTable$asDF$interval)

  d_one <- d_cr
  d_one$grp <- factor("SingleArm")
  r_one <- .sv_validation_run(d_one, outcome = "status", rmst_analysis = TRUE, rmst_tau = 10)
  expect_equal(nrow(r_one$rmstTable$asDF), 1L)
})

test_that("survival bootstrap validation respects stratified_cox and strata_variable", {
  d_boot <- .sv_validation_data()
  d_boot$strata_grp <- factor(rep(c("S1", "S2", "S3"), length.out = nrow(d_boot)))
  r_strat <- .sv_validation_run(
    d_boot,
    bootstrapValidation = TRUE,
    bootstrapValN = 50,
    stratified_cox = TRUE,
    strata_variable = "strata_grp"
  )
  ref_fit <- survival::coxph(
    survival::Surv(time, status) ~ grp + survival::strata(strata_grp),
    data = d_boot
  )
  ref_c <- round(survival::concordance(ref_fit)$concordance, 4)
  tab <- r_strat$bootstrapValidationTable$asDF
  expect_equal(tab$apparent[1], ref_c, tolerance = 1e-4)
  expect_true(is.finite(tab$corrected[1]))
})

test_that("survival .sourcifyOption safely serializes string options and ageAdjustedInterpretation escapes HTML", {
  d <- .sv_validation_data()
  d$age <- seq(40, 80, length.out = nrow(d))
  d[["grp<script>alert(1)</script>"]] <- d$grp
  opts <- .sv_env$survivalOptions$new(
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1",
    explanatory = "grp<script>alert(1)</script>",
    cutp = '2, 4, 6", system("id"), "',
    age_adjustment = TRUE,
    age_variable = "age",
    showExplanations = TRUE
  )
  an <- .sv_env$survivalClass$new(options = opts, data = d)
  src <- an$asSource()
  expect_true(is.expression(parse(text = src)))

  fake_opt <- list(name = "not_a_real_option); system('id') #", value = "x", asSource = function() "x")
  expect_identical(an$.__enclos_env__$private$.sourcifyOption(fake_opt), "")

  an$run()
  interp <- an$results$ageAdjustedInterpretation$content
  expect_false(grepl("<script>", interp, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;", interp, fixed = TRUE))
})
