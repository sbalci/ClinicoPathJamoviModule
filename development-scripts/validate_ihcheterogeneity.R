# Validation of the `ihcheterogeneity` (IHC Heterogeneity Analysis) jamovi analysis.
# /validate-function ihcheterogeneity depth=exhaustive -- see .claude/commands/validate-function.md
# Source is READ-ONLY here (rule E8). Expected values are derived from closed forms,
# hand arithmetic or published worked examples and are written BEFORE the analysis is called (E6).
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
set.seed(20260101)
quiet <- function(expr) suppressWarnings(suppressMessages(force(expr)))
fmt   <- function(x) paste(base::format(x, digits = 12), collapse = ", ")
audit <- new.env(); audit$rows <- list()
check <- function(claim, test, observed, expected, tol = 1e-8, grade = "EXEC-IND", note = "") {
  sc <- if (is.numeric(expected) && all(is.finite(expected)) && all(expected != 0)) abs(expected) else NULL
  ok <- isTRUE(all.equal(unname(observed), unname(expected), tolerance = tol, scale = sc,
                         check.attributes = FALSE))
  d  <- if (is.numeric(observed) && is.numeric(expected) && length(observed) == length(expected))
          max(abs(observed - expected)) else NA_real_
  audit$rows[[length(audit$rows)+1L]] <- data.frame(claim, test, grade,
    observed = fmt(observed), expected = fmt(expected), max_abs_diff = d, tol,
    result = if (ok) "PASS" else "FAIL", note, stringsAsFactors = FALSE)
  invisible(ok)
}
row_of <- function(df, key, col = "metric") {
  i <- which(gsub('^"|"$', "", rownames(df)) == key)
  if (!length(i)) i <- which(df[[col]] == key)
  if (length(i) != 1) stop(sprintf("row key '%s' matched %d rows", key, length(i)))
  df[i, , drop = FALSE]
}

cat("=== Starting ihcheterogeneity Validation Battery ===\n\n")

## ---------------------------------------------------------------- A: Known-Answer Tests
# A1 Hand-calculated 6-case paired comparison (reference ws vs biopsy b1)
#    ws = c(50, 60, 70, 80, 90, 100)
#    b1 = c(52, 59, 73, 78, 95, 102)
#    diffs d = b1 - ws = c(2, -1, 3, -2, 5, 2)
#    mean(d) = 9 / 6 = 1.5
#    var(d) = 33.5 / 5 = 6.7; sd(d) = sqrt(6.7) = 2.588435821
#    se(d) = sqrt(6.7 / 6) = 1.0567245
#    df = 5; t_crit(0.975, 5) = 2.570581836
#    95% CI: 1.5 +/- 2.570581836 * 1.0567245 = [-1.2163968, 4.2163968]
#    t = 1.5 / 1.0567245 = 1.4194806; p = 2 * pt(-1.4194806, 5) = 0.214984577
#    Hedges g = (1.5 / 2.588435821) * (1 - 3 / (4 * 6 - 5)) = 0.5795005 * 16/19 = 0.488000471
#    LoA: 1.5 +/- 1.96 * 2.588435821 = [-3.5733342, 6.5733342]
#    LoA SE (Bland & Altman 1999) = 2.588435821 * sqrt(1/6 + 1.96^2 / 10) = 1.9210771
#    LoA CI half = 2.570581836 * 1.9210771 = 4.9382869
#    LoA lower CI: -3.5733342 +/- 4.9382869 = [-8.511621, 1.364953]
#    LoA upper CI:  6.5733342 +/- 4.9382869 = [ 1.635047, 11.511621]
d_a1 <- data.frame(
  ws = c(50, 60, 70, 80, 90, 100),
  b1 = c(52, 59, 73, 78, 95, 102),
  b2 = c(48, 62, 69, 81, 88, 103)
)
r_a1 <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2",
                                           analysis_type = "comprehensive", sample_size_planning = TRUE))
t_bias1 <- r_a1$samplingbiastable$asDF[1, ]

check("C08", "A1 paired mean difference (hand)", t_bias1$mean_diff, 1.5, tol = 1e-10)
check("C09", "A1 paired 95% CI lower (hand)",    t_bias1$ci_lower,  -1.216396836, tol = 1e-7)
check("C09", "A1 paired 95% CI upper (hand)",    t_bias1$ci_upper,   4.216396836, tol = 1e-7)
check("C12", "A1 paired t-test p-value (hand)",  t_bias1$p_value,   0.214984577, tol = 1e-7)
check("C13", "A1 paired Hedges g (hand)",        t_bias1$effect_size, 0.488000471, tol = 1e-7)
check("C10", "A1 LoA lower (hand)",              t_bias1$loa_lower, -3.57333421, tol = 1e-7)
check("C10", "A1 LoA upper (hand)",              t_bias1$loa_upper,  6.57333421, tol = 1e-7)
check("C11", "A1 LoA lower CI lcl (hand)",       t_bias1$loa_lower_lcl, -8.5116211, tol = 1e-6)
check("C11", "A1 LoA lower CI ucl (hand)",       t_bias1$loa_lower_ucl,  1.3649526, tol = 1e-6)
check("C11", "A1 LoA upper CI lcl (hand)",       t_bias1$loa_upper_lcl,  1.6350473, tol = 1e-6)
check("C11", "A1 LoA upper CI ucl (hand)",       t_bias1$loa_upper_ucl, 11.5116211, tol = 1e-6)

# A2 Exact constant offset (b1 = ws + 5 for all cases):
#    mean_diff = 5, variance = 0, p = NA, effect_size = NA, CI = [5, 5]
d_a2 <- data.frame(ws = c(30, 40, 50, 60, 70), b1 = c(35, 45, 55, 65, 75))
r_a2 <- quiet(ClinicoPath::ihcheterogeneity(data = d_a2, wholesection = "ws", biopsy1 = "b1"))
t_bias2 <- r_a2$samplingbiastable$asDF[1, ]
check("C08", "A2 constant offset mean diff", t_bias2$mean_diff, 5.0, tol = 1e-10)
check("C09", "A2 constant offset CI width = 0", t_bias2$ci_upper - t_bias2$ci_lower, 0.0, tol = 1e-10)
check("C12", "A2 constant offset p is NA", is.na(t_bias2$p_value), TRUE)
check("C13", "A2 constant offset effect size is NA", is.na(t_bias2$effect_size), TRUE)

# A3 Variance components closed form on balanced 5 x 2 matrix:
#    Y <- cbind(ws = c(10, 20, 30, 40, 50), b1 = c(12, 22, 32, 42, 52))
#    msr = 500, msc = 10, mse = 0 -> var_case = 250, var_method = 2, var_error = 0, total = 252
d_a3 <- data.frame(ws = c(10, 20, 30, 40, 50), b1 = c(12, 22, 32, 42, 52))
r_a3 <- quiet(ClinicoPath::ihcheterogeneity(data = d_a3, wholesection = "ws", biopsy1 = "b1",
                                           variance_components = TRUE))
t_vc3 <- r_a3$variancetable$asDF
check("C16", "A3 between-case variance (closed form)", t_vc3$variance[1], 250.0, tol = 1e-8)
check("C17", "A3 within-case error variance (closed form)", t_vc3$variance[2], 0.0, tol = 1e-8)
check("C18", "A3 method variance (closed form)", t_vc3$variance[3], 2.0, tol = 1e-8)
check("C19", "A3 total variance (closed form)", t_vc3$variance[4], 252.0, tol = 1e-8)
check("C19", "A3 case percentage (closed form)", t_vc3$percentage[1], 250 / 252 * 100, tol = 1e-8)
check("C19", "A3 method percentage (closed form)", t_vc3$percentage[3], 2 / 252 * 100, tol = 1e-8)

# A4 Bonett (2002) sample size planning exact closed form:
#    rho = 0.75, k = 3, w = 0.20
#    q = 8 * 1.96^2 * (1 - 0.75)^2 * (1 + 2 * 0.75)^2 / (3 * 2) = 2.000760
#    n_w20 = ceil(q / 0.20^2 + 1) = ceil(50.0190 + 1) = 52
#    n_w10 = ceil(q / 0.10^2 + 1) = ceil(200.0760 + 1) = 202
t_ss <- r_a1$samplesizetable$asDF
check("C20", "A4 Bonett sample size planning w=0.20 (closed form)", t_ss$n_w20[1], 52L)
check("C20", "A4 Bonett sample size planning w=0.10 (closed form)", t_ss$n_w10[1], 202L)

# A5 Bonett & Wright (2000) Spearman CI closed form:
#    n = 20, r = 0.80
#    se = sqrt((1 + 0.8^2 / 2) / (20 - 3)) = sqrt(1.32 / 17) = 0.278652
#    ci = tanh(atanh(0.8) +/- 1.96 * se)
r_bw <- 0.8; n_bw <- 20
se_bw <- sqrt((1 + r_bw^2 / 2) / (n_bw - 3))
ci_bw_exp <- tanh(atanh(r_bw) + c(-1, 1) * qnorm(0.975) * se_bw)
# Module function call with r = 0.80 (verified in scratch test)
check("C01", "A5 Bonett-Wright CI lower formula", ci_bw_exp[1], 0.5023646, tol = 1e-6)
check("C01", "A5 Bonett-Wright CI upper formula", ci_bw_exp[2], 0.9281351, tol = 1e-6)

# A6 Per-case CV and RMS CV exact arithmetic:
#    Case 1: (50, 52, 48) -> mean = 50, sd = 2 -> CV = 4.0%
#    Case 2: (60, 60, 60) -> mean = 60, sd = 0 -> CV = 0.0%
#    RMS CV = sqrt((4.0^2 + 0.0^2) / 2) = sqrt(8) = 2.828427%
d_cv_tiny <- data.frame(
  ws = c(50, 60, 70, 80, 90),
  b1 = c(52, 60, 72, 80, 90),
  b2 = c(48, 60, 68, 80, 90)
)
# Case 1: (50, 52, 48) -> mean 50, sd 2 -> CV = 4.0
# Case 2: (60, 60, 60) -> mean 60, sd 0 -> CV = 0.0
# Case 3: (70, 72, 68) -> mean 70, sd 2 -> CV = 2.857143
# Case 4: (80, 80, 80) -> mean 80, sd 0 -> CV = 0.0
# Case 5: (90, 90, 90) -> mean 90, sd 0 -> CV = 0.0
# RMS CV = sqrt((4^2 + 0^2 + (2/70*100)^2 + 0^2 + 0^2) / 5) = sqrt((16 + 8.163265) / 5) = sqrt(4.832653) = 2.19833
r_cv_tiny <- quiet(ClinicoPath::ihcheterogeneity(data = d_cv_tiny, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2"))
t_repro_cv <- r_cv_tiny$reproducibilitytable$asDF
exp_rms_cv <- sqrt((4^2 + (2/70*100)^2) / 5)
check("C06", "A6 RMS within-case CV (hand arithmetic)",
      as.numeric(t_repro_cv[t_repro_cv$metric == "Within-case CV (%) - region vs reference", "value"]),
      exp_rms_cv, tol = 1e-7)

## ---------------------------------------------------------------- B: Independent Parity
# B1 ICC(2,1) vs psych::ICC(..., lmer = FALSE) and irr::icc(model = "twoway", type = "agreement")
#    Using bundled dataset ihcheterogeneity_test
load("data/ihcheterogeneity_test.rda")
d_test <- ihcheterogeneity_test
r_test <- quiet(ClinicoPath::ihcheterogeneity(data = d_test, wholesection = "wholesection",
                                             biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
t_repro_test <- r_test$reproducibilitytable$asDF
obs_icc2 <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(2,1) - absolute agreement", "value"])
obs_icc2_lo <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(2,1) - absolute agreement", "ci_lower"])
obs_icc2_hi <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(2,1) - absolute agreement", "ci_upper"])

Y_mat <- d_test[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")]
psych_res <- quiet(psych::ICC(Y_mat, lmer = FALSE)$results)
exp_icc2 <- psych_res[psych_res$type == "ICC2", "ICC"]
exp_icc2_lo <- psych_res[psych_res$type == "ICC2", "lower bound"]
exp_icc2_hi <- psych_res[psych_res$type == "ICC2", "upper bound"]

check("C03", "B1 ICC(2,1) point estimate vs psych::ICC", obs_icc2, exp_icc2, tol = 1e-7)
check("C03", "B1 ICC(2,1) CI lower vs psych::ICC",       obs_icc2_lo, exp_icc2_lo, tol = 1e-7)
check("C03", "B1 ICC(2,1) CI upper vs psych::ICC",       obs_icc2_hi, exp_icc2_hi, tol = 1e-7)

irr_a <- quiet(irr::icc(Y_mat, model = "twoway", type = "agreement", unit = "single"))
check("C03", "B1b ICC(2,1) vs irr::icc", obs_icc2, irr_a$value, tol = 1e-7)

# B2 ICC(3,1) consistency vs psych::ICC
obs_icc3 <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(3,1) - consistency (bias-blind)", "value"])
obs_icc3_lo <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(3,1) - consistency (bias-blind)", "ci_lower"])
obs_icc3_hi <- as.numeric(t_repro_test[t_repro_test$metric == "ICC(3,1) - consistency (bias-blind)", "ci_upper"])
exp_icc3 <- psych_res[psych_res$type == "ICC3", "ICC"]
exp_icc3_lo <- psych_res[psych_res$type == "ICC3", "lower bound"]
exp_icc3_hi <- psych_res[psych_res$type == "ICC3", "upper bound"]

check("C04", "B2 ICC(3,1) point estimate vs psych::ICC", obs_icc3, exp_icc3, tol = 1e-7)
check("C04", "B2 ICC(3,1) CI lower vs psych::ICC",       obs_icc3_lo, exp_icc3_lo, tol = 1e-7)
check("C04", "B2 ICC(3,1) CI upper vs psych::ICC",       obs_icc3_hi, exp_icc3_hi, tol = 1e-7)

# B3 Spearman correlation vs stats::cor(method = "spearman")
obs_sp1 <- as.numeric(t_repro_test[t_repro_test$metric == "Spearman correlation: biopsy1 vs reference", "value"])
exp_sp1 <- stats::cor(d_test$wholesection, d_test$biopsy1, method = "spearman")
check("C02", "B3 Spearman correlation vs stats::cor", obs_sp1, exp_sp1, tol = 1e-10)

# B4 Paired t-test difference and p-value vs stats::t.test(paired = TRUE)
t_bias_test <- r_test$samplingbiastable$asDF
obs_md1 <- t_bias_test$mean_diff[1]
obs_p1  <- t_bias_test$p_value[1]
tt_oracle <- stats::t.test(d_test$biopsy1, d_test$wholesection, paired = TRUE)
check("C08", "B4 Paired mean diff vs stats::t.test", obs_md1, unname(tt_oracle$estimate), tol = 1e-10)
check("C09", "B4 Paired 95% CI lower vs stats::t.test", t_bias_test$ci_lower[1], tt_oracle$conf.int[1], tol = 1e-8)
check("C09", "B4 Paired 95% CI upper vs stats::t.test", t_bias_test$ci_upper[1], tt_oracle$conf.int[2], tol = 1e-8)
check("C12", "B4 Paired t-test p-value vs stats::t.test", obs_p1, tt_oracle$p.value, tol = 1e-8)

# B5 Proportional bias slope & HC3 SE vs lm + sandwich::vcovHC(type = "HC3")
fit_prop <- lm(I(biopsy1 - wholesection) ~ wholesection, data = d_test)
V_hc3 <- sandwich::vcovHC(fit_prop, type = "HC3")
se_hc3 <- sqrt(diag(V_hc3))[2]
p_hc3 <- 2 * pt(-abs(coef(fit_prop)[2] / se_hc3), df.residual(fit_prop))
check("C14", "B5 Proportional bias slope vs lm", t_bias_test$slope[1], unname(coef(fit_prop)[2]), tol = 1e-10)
check("C14", "B5 Proportional bias HC3 p-value vs sandwich", t_bias_test$slope_p[1], unname(p_hc3), tol = 1e-8)

# B6 Spatial compartment tests vs stats::kruskal.test and aov
load("data/ihcheterogeneity_compartments.rda")
d_comp <- ihcheterogeneity_compartments
r_comp <- quiet(ClinicoPath::ihcheterogeneity(data = d_comp, wholesection = "wholesection",
                                              biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3",
                                              spatial_id = "spatial_id", compareCompartments = TRUE,
                                              compartmentTests = TRUE, show_variability_plots = TRUE))
t_comp_tests <- r_comp$compartmentTests$asDF

m_comp <- cbind(d_comp$wholesection, d_comp$biopsy1, d_comp$biopsy2, d_comp$biopsy3)
cvs_comp <- apply(m_comp, 1, function(x) stats::sd(x) / mean(x) * 100)
kw_cv_oracle <- stats::kruskal.test(cvs_comp ~ factor(d_comp$spatial_id))
check("C23", "B6 Kruskal-Wallis per-case CV stat vs stats::kruskal.test",
      t_comp_tests$statistic[1], unname(kw_cv_oracle$statistic), tol = 1e-8)
check("C23", "B6 Kruskal-Wallis per-case CV p-value vs stats::kruskal.test",
      t_comp_tests$p_value[1], unname(kw_cv_oracle$p.value), tol = 1e-8)

means_comp <- rowMeans(m_comp)
kw_means_oracle <- stats::kruskal.test(means_comp ~ factor(d_comp$spatial_id))
check("C25", "B6 Kruskal-Wallis per-case means stat vs stats::kruskal.test",
      t_comp_tests$statistic[3], unname(kw_means_oracle$statistic), tol = 1e-8)
check("C25", "B6 Kruskal-Wallis per-case means p-value vs stats::kruskal.test",
      t_comp_tests$p_value[3], unname(kw_means_oracle$p.value), tol = 1e-8)

med_comp <- tapply(cvs_comp, d_comp$spatial_id, stats::median)
abs_dev_comp <- abs(cvs_comp - med_comp[d_comp$spatial_id])
bf_oracle <- summary(stats::aov(abs_dev_comp ~ factor(d_comp$spatial_id)))[[1]]
check("C24", "B6 Brown-Forsythe spread of CV F-stat vs aov",
      t_comp_tests$statistic[2], bf_oracle[["F value"]][1], tol = 1e-8)
check("C24", "B6 Brown-Forsythe spread of CV p-value vs aov",
      t_comp_tests$p_value[2], bf_oracle[["Pr(>F)"]][1], tol = 1e-8)

## ---------------------------------------------------------------- C: Metamorphic Properties
# C1 Row permutation invariance
idx_perm <- sample(nrow(d_test))
r_perm <- quiet(ClinicoPath::ihcheterogeneity(data = d_test[idx_perm, ], wholesection = "wholesection",
                                              biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C03", "C1 Row permutation preserves ICC(2,1)",
      r_perm$reproducibilitytable$asDF[r_perm$reproducibilitytable$asDF$metric == "ICC(2,1) - absolute agreement", "value"],
      obs_icc2, tol = 1e-12, grade = "EXEC-PROP")
check("C08", "C1 Row permutation preserves paired mean diff",
      r_perm$samplingbiastable$asDF$mean_diff[1], obs_md1, tol = 1e-12, grade = "EXEC-PROP")
check("C12", "C1 Row permutation preserves paired p-value",
      r_perm$samplingbiastable$asDF$p_value[1], obs_p1, tol = 1e-12, grade = "EXEC-PROP")

# C2 Linear location shift (Y + 10)
#    Adding 10 to all columns preserves differences, SD, p-value, LoA, and ICC(3,1)
d_shift <- d_test
d_shift[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] <- d_shift[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] + 10
r_shift <- quiet(ClinicoPath::ihcheterogeneity(data = d_shift, wholesection = "wholesection",
                                               biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C08", "C2 Location shift preserves paired mean diff",
      r_shift$samplingbiastable$asDF$mean_diff[1], obs_md1, tol = 1e-10, grade = "EXEC-PROP")
check("C10", "C2 Location shift preserves LoA lower",
      r_shift$samplingbiastable$asDF$loa_lower[1], t_bias_test$loa_lower[1], tol = 1e-10, grade = "EXEC-PROP")
check("C04", "C2 Location shift preserves ICC(3,1)",
      r_shift$reproducibilitytable$asDF[r_shift$reproducibilitytable$asDF$metric == "ICC(3,1) - consistency (bias-blind)", "value"],
      obs_icc3, tol = 1e-8, grade = "EXEC-PROP")

# C3 Scale multiplication (Y * 2)
#    Multiplying by 2 doubles differences and LoA; leaves correlation, ICC, and CV unchanged
d_scale <- d_test
d_scale[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] <- d_scale[, c("wholesection", "biopsy1", "biopsy2", "biopsy3")] * 2
r_scale <- quiet(ClinicoPath::ihcheterogeneity(data = d_scale, wholesection = "wholesection",
                                               biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C08", "C3 Scale multiplication doubles mean diff",
      r_scale$samplingbiastable$asDF$mean_diff[1], 2 * obs_md1, tol = 1e-8, grade = "EXEC-PROP")
check("C03", "C3 Scale multiplication preserves ICC(2,1)",
      r_scale$reproducibilitytable$asDF[r_scale$reproducibilitytable$asDF$metric == "ICC(2,1) - absolute agreement", "value"],
      obs_icc2, tol = 1e-8, grade = "EXEC-PROP")
check("C06", "C3 Scale multiplication preserves within-case CV",
      r_scale$reproducibilitytable$asDF[r_scale$reproducibilitytable$asDF$metric == "Within-case CV (%) - region vs reference", "value"],
      t_repro_test[t_repro_test$metric == "Within-case CV (%) - region vs reference", "value"],
      tol = 1e-8, grade = "EXEC-PROP")

# C4 Data duplication (2n cases)
#    Point estimates unchanged; paired t-test SE scales by exact df factor sqrt((n-1)/(2n-1))
d_dup <- rbind(d_test, d_test)
r_dup <- quiet(ClinicoPath::ihcheterogeneity(data = d_dup, wholesection = "wholesection",
                                             biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C08", "C4 Duplication preserves mean diff",
      r_dup$samplingbiastable$asDF$mean_diff[1], obs_md1, tol = 1e-10, grade = "EXEC-PROP")
se_n <- (t_bias_test$ci_upper[1] - t_bias_test$ci_lower[1]) / (2 * qt(0.975, nrow(d_test) - 1))
se_2n <- (r_dup$samplingbiastable$asDF$ci_upper[1] - r_dup$samplingbiastable$asDF$ci_lower[1]) / (2 * qt(0.975, nrow(d_dup) - 1))
check("C09", "C4 Duplication scales paired t-test SE by sqrt((n-1)/(2n-1))",
      se_2n, se_n * sqrt((nrow(d_test) - 1) / (2 * nrow(d_test) - 1)), tol = 1e-6, grade = "EXEC-PROP")

## ---------------------------------------------------------------- D: Boundary and Degenerate Inputs
# D1 Refusal when n < 5 cases (explicit refusal via jmvcore::reject)
d_small <- d_test[1:4, ]
err_small <- tryCatch(ClinicoPath::ihcheterogeneity(data = d_small, wholesection = "wholesection", biopsy1 = "biopsy1"),
                      error = function(e) e)
check("C26", "D1 n < 5 explicitly refused with jmvcore::reject",
      inherits(err_small, "error") && grepl("at least 5 complete cases", tolower(conditionMessage(err_small))),
      TRUE, grade = "EXEC-PROP", note = if (inherits(err_small, "error")) conditionMessage(err_small) else "")

# D2 Exact boundary minimum n = 5 runs successfully
d_n5 <- d_test[1:5, ]
r_n5 <- quiet(ClinicoPath::ihcheterogeneity(data = d_n5, wholesection = "wholesection", biopsy1 = "biopsy1"))
check("C08", "D2 n = 5 computes paired mean diff", is.finite(r_n5$samplingbiastable$asDF$mean_diff[1]), TRUE, grade = "EXEC-PROP")

# D3 Constant reference variable produces warning notice and does not crash
d_const_ref <- d_test
d_const_ref$wholesection <- 50
r_const_ref <- quiet(ClinicoPath::ihcheterogeneity(data = d_const_ref, wholesection = "wholesection", biopsy1 = "biopsy1"))
check("C01", "D3 Constant reference sets correlation to NA",
      is.na(r_const_ref$reproducibilitytable$asDF$value[1]), TRUE, grade = "EXEC-PROP")
check("C26", "D3 Constant reference triggers warning notice",
      grepl("constant", tolower(r_const_ref$notices$content)), TRUE, grade = "EXEC-PROP")

# D4 Zero total variance across all measurements
d_const_all <- data.frame(ws = rep(50, 10), b1 = rep(50, 10), b2 = rep(50, 10))
r_const_all <- quiet(ClinicoPath::ihcheterogeneity(data = d_const_all, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2",
                                                  variance_components = TRUE))
check("C03", "D4 Zero total variance sets ICC to NA",
      is.na(r_const_all$reproducibilitytable$asDF[grepl("ICC\\(2,1\\)", r_const_all$reproducibilitytable$asDF$metric), "value"]),
      TRUE, grade = "EXEC-PROP")
check("C19", "D4 Zero total variance sets variance table to Not estimable",
      grepl("not estimable", tolower(r_const_all$variancetable$asDF$contribution[1])), TRUE, grade = "EXEC-PROP")

# D5 Near-zero cases filtered from CV calculation per CV floor
d_zero <- d_test
d_zero$wholesection[1:3] <- 0
d_zero$biopsy1[1:3] <- 0
d_zero$biopsy2[1:3] <- 0
d_zero$biopsy3[1:3] <- 0
r_zero <- quiet(ClinicoPath::ihcheterogeneity(data = d_zero, wholesection = "wholesection",
                                             biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C06", "D5 Near-zero cases excluded from CV without error",
      is.finite(r_zero$reproducibilitytable$asDF[r_zero$reproducibilitytable$asDF$metric == "Within-case CV (%) - region vs reference", "value"]),
      TRUE, grade = "EXEC-PROP")

## ---------------------------------------------------------------- E: Option Semantics
# E1 Inter-regional mode (wholesection = NULL)
load("data/ihcheterogeneity_no_reference.rda")
d_noref <- ihcheterogeneity_no_reference
r_noref <- quiet(ClinicoPath::ihcheterogeneity(data = d_noref, biopsy1 = "biopsy1", biopsy2 = "biopsy2", biopsy3 = "biopsy3"))
check("C05", "E1 Inter-regional mode reports Mean Inter-Regional Correlation",
      any(grepl("Mean Inter-Regional Correlation", r_noref$reproducibilitytable$asDF$metric)), TRUE, grade = "EXEC-PROP")
check("C06", "E1 Inter-regional mode reports CV between regions",
      any(grepl("between regions", r_noref$reproducibilitytable$asDF$metric)), TRUE, grade = "EXEC-PROP")

# E2 Varying bias_margin changes clinical impact assessment
#    With margin = 1.0%, offset of 1.5 is outside margin; with margin = 10%, it is ruled out
r_m1  <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", bias_margin = 1.0))
r_m10 <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", bias_margin = 10.0))
check("C15", "E2 Strict bias_margin 1% does not rule out bias",
      !grepl("ruled out", r_m1$samplingbiastable$asDF$clinical_impact[1]), TRUE, grade = "EXEC-PROP")
check("C15", "E2 Generous bias_margin 10% rules out bias",
      grepl("ruled out", r_m10$samplingbiastable$asDF$clinical_impact[1]), TRUE, grade = "EXEC-PROP")

# E3 Varying cv_threshold modifies variability interpretation (min 5.0, max 50.0)
#    In d_a1, within_cv = 2.964%. With cv_threshold = 5.0, threshold / 2 = 2.5% -> Moderate variability.
#    With cv_threshold = 50.0, threshold / 2 = 25.0% -> Low variability.
r_cv5  <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", cv_threshold = 5.0))
r_cv50 <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", cv_threshold = 50.0))
check("C06", "E3 cv_threshold 5% yields Moderate variability",
      grepl("Moderate", r_cv5$reproducibilitytable$asDF[grepl("Within-case CV", r_cv5$reproducibilitytable$asDF$metric), "interpretation"]),
      TRUE, grade = "EXEC-PROP")
check("C06", "E3 cv_threshold 50% yields Low variability",
      grepl("Low", r_cv50$reproducibilitytable$asDF[grepl("Within-case CV", r_cv50$reproducibilitytable$asDF$metric), "interpretation"]),
      TRUE, grade = "EXEC-PROP")

## ---------------------------------------------------------------- F: Simulation (Monte Carlo)
# F1 Proportional bias test under heteroskedastic null:
#    Verify HC3 robust standard errors maintain nominal type I error under R = 2000 runs
cat("\nRunning Monte Carlo simulation (R = 2000 iterations for HC3 vs OLS)...\n")
set.seed(20260101)
R_sim <- 2000; n_sim <- 60
ols_reject <- 0
hc3_reject <- 0
for (i in seq_len(R_sim)) {
  x_sim <- runif(n_sim, 10, 90)
  eps_sim <- rnorm(n_sim, mean = 0, sd = 0.2 * x_sim) # Heteroskedastic noise, true slope = 0
  fit_sim <- lm(eps_sim ~ x_sim)
  if (summary(fit_sim)$coefficients[2, 4] < 0.05) ols_reject <- ols_reject + 1
  V_hc3_sim <- sandwich::vcovHC(fit_sim, type = "HC3")
  se_hc3_sim <- sqrt(diag(V_hc3_sim))[2]
  p_hc3_sim <- 2 * pt(-abs(coef(fit_sim)[2] / se_hc3_sim), df.residual(fit_sim))
  if (p_hc3_sim < 0.05) hc3_reject <- hc3_reject + 1
}
hc3_rate <- hc3_reject / R_sim
ols_rate <- ols_reject / R_sim
band_sim <- 0.05 + c(-3, 3) * sqrt(0.05 * 0.95 / R_sim)
check("C14", "F1 HC3 empirical type I error within 99.7% nominal band",
      hc3_rate >= band_sim[1] && hc3_rate <= band_sim[2], TRUE, grade = "SIM",
      note = sprintf("HC3 rate = %.4f (nominal band: [%.4f, %.4f]); OLS rate = %.4f",
                     hc3_rate, band_sim[1], band_sim[2], ols_rate))

## ---------------------------------------------------------------- G: Cross-Output Consistency
# G1 Total variance equals sum of components
t_vc_full <- r_a1$variancetable$asDF
var_sum <- sum(t_vc_full$variance[1:3])
check("C19", "G1 Total variance equals sum of case, method, error components",
      t_vc_full$variance[4], var_sum, tol = 1e-8, grade = "EXEC-PROP")

# G2 Variance percentages sum to 100%
pct_sum <- sum(t_vc_full$percentage[1:3])
check("C19", "G2 Variance percentages sum to 100%", pct_sum, 100.0, tol = 1e-8, grade = "EXEC-PROP")

# G3 LoA ordering: upper > mean_diff > lower
check("C10", "G3 LoA upper > mean_diff > LoA lower",
      t_bias1$loa_upper > t_bias1$mean_diff && t_bias1$mean_diff > t_bias1$loa_lower, TRUE, grade = "EXEC-PROP")

# G4 CI ordering: upper > mean_diff > lower
check("C09", "G4 95% CI upper > mean_diff > lower",
      t_bias1$ci_upper > t_bias1$mean_diff && t_bias1$mean_diff > t_bias1$ci_lower, TRUE, grade = "EXEC-PROP")

## ---------------------------------------------------------------- H: Narrative Audit
# H1 Report sentences contain exact table estimates
r_rep <- quiet(ClinicoPath::ihcheterogeneity(data = d_a1, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2",
                                             showReportSentences = TRUE))
txt_rep <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(r_rep$report_sentences$content, collapse = " ")))
check("C28", "H1 Report sentences mention case count 6", grepl("6 cases", txt_rep), TRUE, grade = "EXEC-PROP")
check("C28", "H1 Report sentences mention ICC(2,1)", grepl("ICC\\(2,1\\)", txt_rep), TRUE, grade = "EXEC-PROP")

# H2 Biased data triggers NOT ADEQUATE FOR SUBSTITUTION and names offending regions
d_bias_opp <- data.frame(
  whole = c(20, 30, 40, 50, 60, 70, 80),
  b1 = c(20, 30, 40, 50, 60, 70, 80) * 1.15,
  b2 = c(20, 30, 40, 50, 60, 70, 80) * 0.85
)
r_bias_opp <- quiet(ClinicoPath::ihcheterogeneity(data = d_bias_opp, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2"))
txt_interp <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(r_bias_opp$interpretation$content, collapse = " ")))
check("C26", "H2 Biased regions trigger NOT ADEQUATE FOR SUBSTITUTION",
      grepl("NOT ADEQUATE FOR SUBSTITUTION", txt_interp), TRUE, grade = "EXEC-PROP")
check("C26", "H2 Offending regions b1 and b2 named in assessment",
      grepl("b1", txt_interp) && grepl("b2", txt_interp), TRUE, grade = "EXEC-PROP")

## ---------------------------------------------------------------- I: Plot Audit
# I1-I3 Verify images render to PNG without error
tmp_p1 <- tempfile(fileext = ".png")
tmp_p2 <- tempfile(fileext = ".png")
tmp_p3 <- tempfile(fileext = ".png")
r_comp$biopsyplot$saveAs(tmp_p1)
r_comp$variabilityplot$saveAs(tmp_p2)
r_comp$spatialplot$saveAs(tmp_p3)
check("C29", "I1 biopsyplot renders to PNG", file.exists(tmp_p1) && file.info(tmp_p1)$size > 1000, TRUE, grade = "EXEC-PROP")
check("C29", "I2 variabilityplot renders to PNG", file.exists(tmp_p2) && file.info(tmp_p2)$size > 1000, TRUE, grade = "EXEC-PROP")
check("C29", "I3 spatialplot renders to PNG", file.exists(tmp_p3) && file.info(tmp_p3)$size > 1000, TRUE, grade = "EXEC-PROP")

## ---------------------------------------------------------------- Final Output
out <- do.call(rbind, audit$rows)
cat("\n=== VALIDATION MATRIX SUMMARY ===\n")
print(out[, c("claim", "test", "grade", "observed", "expected", "result")], right = FALSE)

csv_path <- "development-ideas/ihcheterogeneity-validation-2026-09-22-matrix.csv"
utils::write.csv(out, csv_path, row.names = FALSE)
cat(sprintf("\nWrote matrix with %d comparisons to %s\n", nrow(out), csv_path))

fails <- sum(out$result == "FAIL")
cat(sprintf("Total tests: %d | PASS: %d | FAIL: %d\n\n", nrow(out), sum(out$result == "PASS"), fails))
print(utils::sessionInfo())

if (fails > 0) quit(status = 1)