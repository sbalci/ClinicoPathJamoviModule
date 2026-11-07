# Regression tests for the 2026-10-30 independent review of multisurvival (MS-01..11),
# the cross-analysis findings it shares, and the stale-panel engine finding.
library(testthat)
library(survival)

.rr_ms <- function(d, ..., elapsedtime = "time", outcome = "status") {
  args <- list(data = d, elapsedtime = elapsedtime, outcome = outcome, ...)
  for (k in c("outcomeLevel", "dod", "dooc", "awd", "awod"))
    if (is.null(args[[k]])) args[k] <- list(NULL)
  do.call(ClinicoPath::multisurvival, args)
}
.rr_plain <- function(x) gsub("\\s+", " ", gsub("</?[a-zA-Z][^>]*>", " ", x))  # "p < 0.05" is not a tag
.rr_notes <- function(tbl) vapply(tbl$notes, function(n) n$note, character(1))

.rr_data <- function(n = 300, seed = 1, ord = FALSE) {
  set.seed(seed)
  d <- data.frame(age = round(rnorm(n, 60, 10)),
    grade = factor(sample(c("G1", "G2", "G3"), n, TRUE), levels = c("G1", "G2", "G3"), ordered = ord),
    arm = factor(sample(c("A", "B"), n, TRUE)), site = factor(sample(c("X", "Y"), n, TRUE)))
  lp <- .03 * (d$age - 60) + .5 * (as.integer(d$grade) - 1) - .4 * (d$arm == "B")
  t <- rexp(n, .02 * exp(lp)); cen <- runif(n, 10, 100)
  d$time <- pmin(t, cen); d$status <- as.integer(t <= cen)
  d
}
.rr_cr <- function(n = 500, seed = 2) {
  set.seed(seed)
  d <- data.frame(age = round(rnorm(n, 60, 10)), arm = factor(sample(c("A", "B"), n, TRUE)),
                  site = factor(sample(c("X", "Y"), n, TRUE)))
  t1 <- rexp(n, .02 * exp(.03 * (d$age - 60) - .5 * (d$arm == "B")))
  t2 <- rexp(n, .01 * exp(.5 * (d$arm == "B"))); cen <- runif(n, 10, 120)
  d$time <- pmin(t1, t2, cen)
  d$status <- factor(ifelse(cen < pmin(t1, t2), "AWOD", ifelse(t1 < t2, "DOD", "DOOC")))
  d
}
.rr_crargs <- list(multievent = TRUE, analysistype = "compete", dod = "DOD", dooc = "DOOC", awod = "AWOD")
.rr_dates <- function(d) {
  d$dx <- as.Date("2015-01-01") + (seq_len(nrow(d)) %% 300)
  d$fu <- d$dx + round(d$time * 30.44) + 1
  d
}

test_that("MS-01: an Ordinal column gives the same results as a Nominal one", {
  args <- list(explanatory = c("grade", "arm"), contexpl = "age", interactions = list(c("grade", "arm")),
               ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE)
  rn <- do.call(.rr_ms, c(list(.rr_data()), args))
  ro <- do.call(.rr_ms, c(list(.rr_data(ord = TRUE)), args))
  expect_identical(.rr_plain(ro$text$content), .rr_plain(rn$text$content))
  expect_identical(ro$adjustedCoxTable$asDF, rn$adjustedCoxTable$asDF)
  expect_identical(ro$subgroupHR$asDF, rn$subgroupHR$asDF)
  expect_false(grepl("contrast", paste(ro$adjustedCoxTable$asDF$Variable, collapse = " ")))
})

test_that("MS-02: a main effect under an interaction states where it applies", {
  d <- .rr_data()
  r <- .rr_ms(d, explanatory = "arm", contexpl = "age", interactions = list(c("arm", "age")),
              ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE)
  fit <- coxph(Surv(time, status) ~ arm + age + arm:age, d)
  txt <- .rr_plain(r$adjustedCoxSummary$content)
  expect_match(txt, "the adjusted hazard ratio is [0-9.]+ \\([0-9.-]+, 95% CI\\) at age = 0", perl = TRUE)
  expect_match(txt, "per 1-unit increase in age at arm = A")
  expect_false(grepl("holding the other covariates", txt, fixed = TRUE))
  # The row is the HR at age 0, not at the cohort's mean age: the wording is the point.
  expect_gt(abs(coef(fit)["armB"] - (coef(fit)["armB"] + mean(d$age) * coef(fit)["armB:age"])), 0.1)

  tb <- r$adjustedCoxTable
  expect_true(any(grepl("at age = 0", tb$asDF$Variable, fixed = TRUE)))
  for (notes in list(.rr_notes(tb), .rr_notes(r$interactionTest))) {
    expect_true(any(grepl("arm: at age = 0", notes, fixed = TRUE)))
    expect_true(any(grepl("outside the observed range", notes, fixed = TRUE)))
  }
  expect_match(.rr_plain(r$text$content), "arm: at age = 0", fixed = TRUE)
})

test_that("MS-02: a categorical partner is named by its reference level", {
  r <- .rr_ms(.rr_data(), explanatory = c("arm", "site"), interactions = list(c("arm", "site")),
              ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE)
  txt <- .rr_plain(r$adjustedCoxSummary$content)
  expect_match(txt, "at site = X", fixed = TRUE)
  expect_match(txt, "at arm = A", fixed = TRUE)
  expect_false(any(grepl("outside the observed range", .rr_notes(r$adjustedCoxTable), fixed = TRUE)))
})

test_that("MS-02: a variable in an interaction is never the strongest predictor", {
  set.seed(4); n <- 600
  d <- data.frame(arm = factor(sample(c("A", "B"), n, TRUE)), site = factor(sample(c("X", "Y"), n, TRUE)),
                  grade = factor(sample(c("G1", "G2"), n, TRUE)))
  t <- rexp(n, .02 * exp(2.5 * (d$arm == "B") - 2.5 * (d$arm == "B") * (d$site == "Y") + .5 * (d$grade == "G2")))
  cen <- runif(n, 10, 100); d$time <- pmin(t, cen); d$status <- as.integer(t <= cen)
  r <- .rr_ms(d, explanatory = c("arm", "site", "grade"), interactions = list(c("arm", "site")))
  main <- .rr_plain(r$multivariableCoxSummary$content)
  clin <- .rr_plain(r$text$content)
  for (txt in list(main, clin)) {
    expect_match(txt, "Strongest predictor: grade", perl = TRUE)
    expect_match(txt, "are not ranked here", fixed = TRUE)
  }
})

test_that("MS-03: a row sharing its focal with another interaction names the fixed value", {
  r <- .rr_ms(.rr_data(), explanatory = c("arm", "grade"), contexpl = "age",
              interactions = list(c("arm", "grade"), c("arm", "age")))
  sg <- r$subgroupHR$asDF
  rows <- sg[sg$interaction == "arm * grade", ]
  expect_true(all(grepl("(at age = 0)", rows$moderator_level, fixed = TRUE)))
  expect_true(any(grepl("age = 0", .rr_notes(r$subgroupHR), fixed = TRUE)))
})

test_that("MS-04: no focal contrast in a subgroup is not estimable, not another level's HR", {
  d <- .rr_data()
  d$g2 <- factor(ifelse(d$grade == "G1", "L", ifelse(d$grade == "G2", "M", "H")), levels = c("L", "M", "H"))
  d$arm[d$g2 == "H"] <- "A"
  sub <- ClinicoPath:::.computeSubgroupHRs(
    Surv(time, status) ~ arm + g2 + arm:g2, d, focal = "arm", moderator = "g2")
  expect_true(is.na(sub$hr[sub$moderator_level == "H"]))
  expect_true(is.na(sub$p[sub$moderator_level == "H"]))
  expect_false(is.na(sub$hr[sub$moderator_level == "M"]))

  r <- .rr_ms(d, explanatory = c("arm", "g2"), interactions = list(c("arm", "g2")))
  tb <- r$subgroupHR
  df <- tb$asDF
  h <- which(df$moderator_level == "H")
  expect_false(isTRUE(df$hr[h] == df$hr[df$moderator_level == "M"]))
  expect_match(paste(capture.output(print(tb)), collapse = "\n"), "Not estimable in this subgroup", fixed = TRUE)
})

test_that("MS-05: a standardized curve stops at the shortest stratum", {
  set.seed(11); n <- 400
  d <- data.frame(arm = factor(sample(c("A", "B"), n, TRUE)), st = factor(sample(c("S1", "S2"), n, TRUE)))
  t <- rexp(n, .04); cen <- ifelse(d$st == "S1", runif(n, 5, 24), runif(n, 5, 80))
  d$time <- pmin(t, cen); d$status <- as.integer(t <= cen)
  r <- .rr_ms(d, explanatory = "arm", use_stratify = TRUE, stratvar = "st", ac = TRUE,
              adjexplanatory = "arm", ac_summary = TRUE, cutp = "12,48")
  tb <- r$adjustedSurvTable
  expect_setequal(tb$asDF$time, 12)
  expect_true(any(grepl("shortest follow-up among the strata", .rr_notes(tb), fixed = TRUE)))
  expect_true(any(grepl("shortest follow-up among the strata", .rr_notes(r$adjustedMedianTable), fixed = TRUE)))
})

test_that("MS-06: Fine-Gray with strata matches a by-hand stratified fit", {
  d <- .rr_cr()
  r <- do.call(.rr_ms, c(list(d, explanatory = "arm", contexpl = "age",
                              use_stratify = TRUE, stratvar = "site"), .rr_crargs))
  d$st <- factor(ifelse(d$status == "AWOD", "c", ifelse(d$status == "DOD", "e", "o")), c("c", "e", "o"))
  d$id <- seq_len(nrow(d))
  # Expanded separately per stratum, so the censoring weights are per stratum too.
  fg <- do.call(rbind, lapply(split(d, d$site), function(x) {
    e <- finegray(Surv(time, st) ~ arm + age + id, x, etype = "e", id = id)
    e$site <- x$site[1]
    e
  }))
  f <- coxph(Surv(fgstart, fgstop, fgstatus) ~ arm + age + strata(site), fg, weights = fgwt, cluster = id)
  hr <- sprintf("%.2f", exp(coef(f)[["armB"]]))
  expect_match(.rr_plain(r$text$content), paste0("(vs A) ", hr, " ("), fixed = TRUE)
})

test_that("MS-07: a small rate is not rounded to zero", {
  d <- .rr_data()
  r <- .rr_ms(transform(d, time = time * 30.44), explanatory = "arm", person_time = TRUE,
              time_intervals = "365", rate_multiplier = 1, timetypeoutput = "days")
  pt <- r$personTimeTable$asDF
  truth <- sum(d$status) / sum(d$time * 30.44)
  expect_equal(pt$rate[1], truth, tolerance = 1e-10)
  expect_gt(pt$rate_ci_upper[1], truth)
  expect_match(.rr_plain(r$personTimeSummary$content),
               paste0("Overall incidence rate: ", base::format(signif(truth, 3), scientific = FALSE)), fixed = TRUE)
  expect_true(any(grepl("Rates are per 1 person-days.", .rr_notes(r$personTimeTable), fixed = TRUE)))
})

test_that("MS-08/F3: numeric dates that are not epoch seconds are refused", {
  d <- .rr_dates(.rr_data())
  d$dxn <- as.numeric(d$dx); d$fun <- as.numeric(d$fu)
  expect_error(
    .rr_ms(d, explanatory = "arm", tint = TRUE, dxdate = "dxn", fudate = "fun", elapsedtime = NULL),
    "dxn contains numbers that cannot be read as dates \\(for example [0-9]+\\)")
  d$dxs <- d$dxn * 86400; d$fus <- d$fun * 86400
  r <- .rr_ms(d, explanatory = "arm", tint = TRUE, dxdate = "dxs", fudate = "fus", elapsedtime = NULL)
  expect_gt(nchar(r$text$content), 100)
})

test_that("F4: a placeholder date is refused", {
  d <- .rr_dates(.rr_data())
  d$dx <- as.character(d$dx); d$fu <- as.character(d$fu); d$fu[3] <- "2199-12-31"
  expect_error(
    .rr_ms(d, explanatory = "arm", tint = TRUE, dxdate = "dx", fudate = "fu", elapsedtime = NULL),
    "1 dates in fu fall outside a plausible range \\(for example 2199-12-31\\)")
})

test_that("F5: unreadable dates are reported with their count", {
  d <- .rr_dates(.rr_data())
  d$dx <- as.character(d$dx); d$fu <- as.character(d$fu); d$fu[1:40] <- "not a date"
  r <- suppressWarnings(.rr_ms(d, explanatory = "arm", tint = TRUE, dxdate = "dx", fudate = "fu", elapsedtime = NULL))
  expect_match(.rr_plain(r$warnings$content),
               "40 of 600 dates could not be read, so those patients were excluded.", fixed = TRUE)
})

test_that("F2: the landmark keeps T > L strictly and says so", {
  d <- data.frame(time = rep(c(6, 12, 13, 24, 36, 48), 20), arm = factor(rep(c("A", "B"), 60)))
  d$status <- as.integer(seq_len(nrow(d)) %% 3 != 0)
  r <- .rr_ms(d, explanatory = "arm", uselandmark = TRUE, landmark = 12, person_time = TRUE)
  pt <- r$personTimeTable$asDF
  expect_equal(pt$person_time[1], sum(d$time[d$time > 12] - 12))
  expect_match(.rr_plain(r$text2$content), "at or before the landmark were excluded \\(n = 40 removed\\)")
  expect_true(any(grepl("Times are measured from the landmark at 12 months", .rr_notes(r$personTimeTable), fixed = TRUE)))
})

test_that("F2: missing times are counted apart from landmark exclusions", {
  d <- .rr_data(); d$time[1:5] <- NA
  r <- .rr_ms(d, explanatory = "arm", uselandmark = TRUE, landmark = 3)
  expect_match(.rr_plain(r$text2$content), "no follow-up time could not be assessed at the landmark and were also excluded (n = 5)", fixed = TRUE)
})

test_that("F6: saved columns keep every row, unshifted", {
  d <- .rr_dates(.rr_data())
  d$dx <- as.character(d$dx); d$fu <- as.character(d$fu)
  d$arm[1:3] <- NA
  o <- ClinicoPath:::multisurvivalOptions$new(
    tint = TRUE, dxdate = "dx", fudate = "fu", outcome = "status", outcomeLevel = "1",
    explanatory = "arm", uselandmark = TRUE, landmark = 6)
  o$.__enclos_env__$private$..calculatedtime$.__enclos_env__$private$.value <-
    list(value = TRUE, vars = "calculatedtime")
  an <- ClinicoPath:::multisurvivalClass$new(options = o, data = d)
  suppressWarnings(an$run())
  saved <- an$results$calculatedtime$.__enclos_env__$private
  truth <- lubridate::time_length(lubridate::interval(as.Date(d$dx), as.Date(d$fu)), "months")
  expect_equal(as.integer(saved$.rowNums), seq_len(nrow(d)))
  expect_equal(as.numeric(saved$.values[[1]]), truth, tolerance = 1e-8)
})

test_that("F1: a reference level emptied by exclusions is dropped and disclosed", {
  d <- .rr_data()
  d$ki67 <- rnorm(nrow(d)); d$ki67[d$grade == "G1"] <- NA
  r <- .rr_ms(d, explanatory = "grade", contexpl = "ki67", ac = TRUE, adjexplanatory = "grade", ac_summary = TRUE)
  fit <- coxph(Surv(time, status) ~ grade + ki67, droplevels(d[!is.na(d$ki67), ]))
  expect_match(.rr_plain(r$warnings$content),
               "grade: no analyzed patient has G1, so that level was dropped. Hazard ratios are compared with G2, not with G1.", fixed = TRUE)
  rows <- r$adjustedCoxTable$asDF
  expect_equal(nrow(rows), 2L)
  expect_match(rows$Variable[1], "grade: G3 (vs G2)", fixed = TRUE)
  expect_match(rows$HR[1], sprintf("^%.2f ", exp(coef(fit)[["gradeG3"]])))
})

test_that("F1: a level lost to exclusions is refused when one level remains", {
  d <- .rr_data(); d$time[d$arm == "A"] <- 1
  expect_error(.rr_ms(d, explanatory = "arm", uselandmark = TRUE, landmark = 3), "only one value")
})

test_that("F16: a column named after an R keyword runs", {
  d <- .rr_data(); d$IF <- d$site
  r <- .rr_ms(d, explanatory = c("arm", "IF"))
  expect_false(grepl("Error generating", r$text$content, fixed = TRUE))
  expect_match(.rr_plain(r$text$content), "IF", fixed = TRUE)
})

test_that("median follow-up does not count an other-cause death as still observed", {
  d <- .rr_cr()
  r <- .rr_ms(d, explanatory = "arm", multievent = TRUE, analysistype = "cause",
              dod = "DOD", dooc = "DOOC", awod = "AWOD")
  alive <- as.integer(d$status == "AWOD")
  fit <- survfit(Surv(time, alive) ~ 1, d)
  expect_match(.rr_plain(r$infoMessages$content),
               sprintf("median follow-up of %.1f months", unname(summary(fit)$table["median"])), fixed = TRUE)
  wrong <- survfit(Surv(time, as.integer(d$status != "DOD")) ~ 1, d)
  expect_false(isTRUE(all.equal(summary(fit)$table[["median"]], summary(wrong)$table[["median"]])))
})

test_that("MS-09: one empty cell leaves the joint test, on fewer df", {
  set.seed(5); d <- .rr_data(); d$m <- factor(sample(c("p", "q", "r"), 300, TRUE))
  d <- d[!(d$grade == "G3" & d$m == "r"), ]
  fit <- coxph(Surv(time, status) ~ grade + m + grade:m, d)
  jt <- ClinicoPath:::.interactionJointTests(fit)
  expect_equal(jt$df, 3L)
  idx <- which(!is.na(coef(fit)) & grepl(":", names(coef(fit)), fixed = TRUE))
  expect_equal(jt$chisq, as.numeric(t(coef(fit)[idx]) %*% solve(vcov(fit)[idx, idx]) %*% coef(fit)[idx]))
})

test_that("MS-10: a colon in a level is not an interaction", {
  d <- .rr_data(); levels(d$grade) <- c("Gr 1", "Gr:2", "Gr+3")
  r <- .rr_ms(d, explanatory = c("grade", "arm"), contexpl = "age", interactions = list(c("grade", "arm")),
              ac = TRUE, adjexplanatory = "arm", ac_summary = TRUE)
  expect_equal(nrow(r$interactionTest$asDF), 2L)
  expect_true(any(grepl("grade: Gr:2 × arm: B", r$adjustedCoxTable$asDF$Variable, fixed = TRUE)))
})

test_that("MS-11: the metrics line never prints p = 0.000", {
  r <- .rr_ms(.rr_data(), explanatory = c("arm", "grade"), contexpl = "age")
  expect_false(grepl("p = 0.000", r$text2$content, fixed = TRUE))
})

test_that("Engine F2: an early return leaves no previous model's text", {
  d <- .rr_cr()
  o <- ClinicoPath:::multisurvivalOptions$new(
    elapsedtime = "time", outcome = "status", explanatory = "arm", multievent = TRUE)
  an <- ClinicoPath:::multisurvivalClass$new(options = o, data = d)
  an$results$text$setContent("HR from the previous model")
  an$results$glossaryPanel$setContent("static explanation")
  an$run()
  expect_identical(an$results$text$content, "<div></div>")
  expect_identical(an$results$glossaryPanel$content, "static explanation")
  expect_match(an$results$infoMessages$content, "Assign outcome levels", fixed = TRUE)
})
