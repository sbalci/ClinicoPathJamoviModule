# ═══════════════════════════════════════════════════════════
# Validation regression tests: lassologistic (/validate-function, 2026-10-12)
# ═══════════════════════════════════════════════════════════
#
# Oracles are independent of glmnet and pROC: a Newton solve of the elastic-net
# objective (Friedman, Hastie & Tibshirani, J Stat Softw 2010;33(1), eq. 13 with the
# penalty of eq. 2) plus its KKT conditions; Mann-Whitney AUC; DeLong (1988) variance
# by structural components; exhaustive Youden search; hand Newton-Raphson logistic
# regression. VAL-* tests lock in the fixes for the findings of that validation.
# Evidence and report: development-scripts/validate_lassologistic.R,
# development-ideas/lassologistic-validation-2026-10-12.md

library(testthat)

# ── fixtures and oracles ─────────────────────────────────────────────────────
val_fixture <- function(n = 400, seed = 7) {
  set.seed(seed)
  d <- data.frame(
    p53 = factor(sample(c("wt", "mut"), n, TRUE), levels = c("wt", "mut")),
    Rb1 = factor(sample(c("intact", "lost"), n, TRUE), levels = c("intact", "lost")),
    grade = factor(sample(c("G1", "G2", "G3"), n, TRUE), levels = c("G1", "G2", "G3")),
    ki = round(stats::rnorm(n, 30, 15)), age = round(stats::rnorm(n, 60, 10)),
    noise1 = stats::rnorm(n))
  lp <- -2.2 + 1.5 * (d$p53 == "mut") + 1.0 * (d$Rb1 == "lost") + 0.6 * (d$grade == "G3") +
        0.035 * (d$ki - 30)
  d$dx <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(lp)) == 1, "NEC", "NET"),
                 levels = c("NET", "NEC"))
  names(d)[names(d) == "ki"] <- "Ki-67 (%)"
  d
}
VAL_VARS <- c("p53", "Rb1", "grade", "Ki-67 (%)", "age", "noise1")

val_run <- function(d, outcome = "dx", outcomeLevel = "NEC", explanatory = VAL_VARS, ...) {
  args <- list(data = d, outcome = outcome, outcomeLevel = outcomeLevel, explanatory = explanatory,
               nfolds = 10, random_seed = 123456, cv_plot = TRUE, coef_plot = FALSE, roc_plot = TRUE)
  suppressWarnings(do.call(ClinicoPath::lassologistic, utils::modifyList(args, list(...))))
}
val_key  <- function(df, key) df[gsub('^"|"$', "", rownames(df)) == key, , drop = FALSE]
val_stat <- function(res, s) { ms <- res$modelSummary$asDF; ms$value[ms$statistic == s] }
val_coef <- function(res) {
  cf <- res$coefficients$asDF
  list(terms = cf$variable[!cf$variable %in% c("(Intercept)", "No variables selected")],
       coef = stats::setNames(cf$coefficient, cf$variable),
       b0 = cf$coefficient[cf$variable == "(Intercept)"])
}
val_design <- function(d, vars) {
  X <- stats::model.matrix(~ ., d[vars])[, -1, drop = FALSE]
  colnames(X) <- gsub("`", "", colnames(X), fixed = TRUE); X
}
val_l1pe <- function(x) pmax(x, 0) + log1p(exp(-abs(x)))
# Newton on the module's active set, then KKT on the inactive set.
val_enet <- function(Z, y, lambda, alpha, active, signs) {
  n <- nrow(Z); ZA <- cbind(1, Z[, active, drop = FALSE]); k <- ncol(ZA)
  lin <- c(0, lambda * alpha * signs); quad <- c(0, rep(lambda * (1 - alpha), length(active)))
  obj <- function(b) { e <- as.numeric(ZA %*% b); -mean(y * e - val_l1pe(e)) + sum(lin * b) + sum(quad * b^2) / 2 }
  b <- c(stats::qlogis(mean(y)), rep(0, length(active)))
  for (it in 1:200) {
    p <- as.numeric(stats::plogis(ZA %*% b))
    g <- as.numeric(-crossprod(ZA, y - p) / n) + lin + quad * b
    H <- crossprod(ZA, ZA * (p * (1 - p))) / n + diag(quad, k)
    s <- as.numeric(solve(H, g)); f0 <- obj(b); t <- 1
    repeat { bn <- b - t * s; if (obj(bn) <= f0 - 1e-4 * t * sum(g * s) || t < 1e-12) break; t <- t / 2 }
    b <- bn; if (max(abs(t * s)) < 1e-14) break
  }
  p <- as.numeric(stats::plogis(ZA %*% b)); ina <- setdiff(colnames(Z), active)
  list(b0 = b[1], b = b[-1], p = p,
       kkt = if (alpha > 0 && length(ina)) max(abs(colMeans(Z[, ina, drop = FALSE] * (y - p)))) / (lambda * alpha) else 0)
}
val_coef_oracle <- function(res, X, y, alpha = 1, lam = "lambda_1se") {
  ct <- val_coef(res); ctr <- colMeans(X); sdv <- sqrt(colMeans(sweep(X, 2, ctr)^2))
  Z <- sweep(sweep(X, 2, ctr), 2, sdv, "/")
  o <- val_enet(Z, y, res$cv_plot$state[[lam]], alpha, ct$terms, sign(ct$coef[ct$terms]))
  list(b_mod = unname(ct$coef[ct$terms] * sdv[ct$terms]), b0_mod = ct$b0 + sum(ct$coef[ct$terms] * ctr[ct$terms]),
       o = o, ct = ct)
}
val_auc <- function(y, s) mean(outer(s[y == 1], s[y == 0], ">") + 0.5 * outer(s[y == 1], s[y == 0], "=="))
val_delong <- function(y, s) {
  psi <- outer(s[y == 1], s[y == 0], ">") + 0.5 * outer(s[y == 1], s[y == 0], "==")
  a <- mean(psi); v <- stats::var(rowMeans(psi)) / nrow(psi) + stats::var(colMeans(psi)) / ncol(psi)
  sprintf("%.3f (%.3f-%.3f)", a, a - stats::qnorm(0.975) * sqrt(v), a + stats::qnorm(0.975) * sqrt(v))
}
val_youden <- function(y, p) {
  u <- sort(unique(p)); th <- c(-Inf, (utils::head(u, -1) + utils::tail(u, -1)) / 2, Inf)
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  Jn <- vapply(th, function(t) sum(p[y == 1] > t) * n0 + sum(p[y == 0] <= t) * n1, 0)
  th[which(Jn == max(Jn))[1]]
}
val_logit <- function(X, y) {
  X <- cbind(1, X); b <- rep(0, ncol(X))
  for (i in 1:100) { p <- as.numeric(stats::plogis(X %*% b))
    s <- solve(crossprod(X, X * (p * (1 - p))), crossprod(X, y - p)); b <- b + as.numeric(s)
    if (max(abs(s)) < 1e-12) break }
  p <- as.numeric(stats::plogis(X %*% b))
  list(b = b, p = p, aic = -2 * sum(y * log(p) + (1 - y) * log(1 - p)) + 2 * ncol(X))
}
COEF_TOL <- 5e-4   # per-SD, absolute: glmnet's default thresh (1e-7) bounds its precision near here

# ── coefficients ─────────────────────────────────────────────────────────────
test_that("C14/C17 LASSO coefficients and intercept solve the penalized objective (Newton + KKT)", {
  # oracle: Newton solve of FHT 2010 eq. 13 on the module's active set; KKT on the rest
  skip_if_not_installed("glmnet")
  d <- val_fixture(); res <- val_run(d)
  X <- val_design(d, VAL_VARS); y <- as.integer(d$dx == "NEC")
  o <- val_coef_oracle(res, X, y)
  expect_gt(length(o$ct$terms), 0)
  expect_lte(max(abs(o$b_mod - unname(o$o$b))), COEF_TOL)
  expect_lte(abs(o$b0_mod - o$o$b0), COEF_TOL)
  expect_lte(o$o$kkt, 1 + 1e-3)
})

test_that("C14 ridge and elastic-net coefficients solve their objectives", {
  # oracle: same Newton/KKT with alpha = 0 and 0.5
  skip_if_not_installed("glmnet")
  d <- val_fixture(); X <- val_design(d, VAL_VARS); y <- as.integer(d$dx == "NEC")
  for (cfg in list(list(penalty = "ridge", a = 0), list(penalty = "elasticnet", a = 0.5))) {
    res <- val_run(d, penalty = cfg$penalty, alpha = 0.5)
    o <- val_coef_oracle(res, X, y, alpha = cfg$a)
    expect_lte(max(abs(o$b_mod - unname(o$o$b))), COEF_TOL, label = cfg$penalty)
    expect_lte(o$o$kkt, 1 + 1e-3)
  }
})

test_that("C14 MASS::birthwt coefficients solve the penalized objective", {
  # canonical data; oracle as above
  skip_if_not_installed("glmnet"); skip_if_not_installed("MASS")
  bw <- MASS::birthwt; bw$race <- factor(bw$race, levels = 1:3, labels = c("white", "black", "other"))
  v <- c("age", "lwt", "race", "smoke", "ptl", "ht", "ui", "ftv")
  res <- val_run(bw, outcome = "low", outcomeLevel = NULL, explanatory = v)
  expect_equal(val_stat(res, "Event class (positive)"), sprintf("1 (n=%d)", sum(bw$low == 1)))
  o <- val_coef_oracle(res, val_design(bw, v), bw$low)
  expect_lte(max(abs(o$b_mod - unname(o$o$b))), COEF_TOL)
  expect_lte(o$o$kkt, 1 + 1e-3)
})

# ── discrimination and classification ────────────────────────────────────────
test_that("C19/C20 apparent AUC and its DeLong interval match hand computation", {
  # oracle: Mann-Whitney pair count; DeLong 1988 structural-component variance
  d <- val_fixture(); res <- val_run(d, showModelComparison = TRUE)
  rs <- list(y = as.integer(d$dx == "NEC"), probabilities = res$predictions$.__enclos_env__$private$.values[[1]])
  expect_equal(val_key(res$modelComparison$asDF, "1")$auc, val_auc(rs$y, rs$probabilities), tolerance = 1e-12)
  expect_equal(val_key(res$performance$asDF, "auc")$value, val_delong(rs$y, rs$probabilities))
})

test_that("C21 AUC bands (Hosmer-Lemeshow 2013) land correctly at exact AUC 0.7 / 0.8 / 0.9", {
  # one binary marker (duplicated column): AUC = (sens + spec)/2 = k/50 exactly
  for (cs in list(list(k = 35, band = "Acceptable"), list(k = 40, band = "Excellent"), list(k = 45, band = "Outstanding"))) {
    x <- c(rep(1, cs$k), rep(0, 50 - cs$k), rep(0, cs$k), rep(1, 50 - cs$k))
    d <- data.frame(y = factor(rep(c("case", "ctl"), each = 50), levels = c("ctl", "case")), x1 = x, x2 = x)
    res <- val_run(d, outcome = "y", outcomeLevel = "case", explanatory = c("x1", "x2"))
    pf <- res$performance$asDF
    expect_equal(val_key(pf, "auc")$interpretation, cs$band)
    expect_equal(val_key(pf, "sensitivity")$value, sprintf("%.3f", cs$k / 50))
    expect_equal(val_key(pf, "specificity")$value, sprintf("%.3f", cs$k / 50))
  }
})

test_that("C22-C28 threshold, 2x2 metrics and Brier follow from the saved probabilities", {
  # oracle: exhaustive Youden search in integer arithmetic; counts by hand
  d <- val_fixture(); res <- val_run(d)
  y <- as.integer(d$dx == "NEC"); p <- res$predictions$.__enclos_env__$private$.values[[1]]
  t <- val_youden(y, p); pred <- p > t
  tp <- sum(pred & y == 1); fp <- sum(pred & y == 0); fn <- sum(!pred & y == 1); tn <- sum(!pred & y == 0)
  pf <- res$performance$asDF; f <- function(k) val_key(pf, k)$value
  expect_equal(f("threshold"), sprintf("%.3f", t))
  expect_equal(f("accuracy"), sprintf("%.3f", (tp + tn) / length(y)))
  expect_equal(f("sensitivity"), sprintf("%.3f", tp / (tp + fn)))
  expect_equal(f("specificity"), sprintf("%.3f", tn / (tn + fp)))
  expect_equal(f("precision"), sprintf("%.3f", tp / (tp + fp)))
  expect_equal(f("f1"), sprintf("%.3f", 2 * tp / (2 * tp + fp + fn)))
  expect_equal(f("brier"), sprintf("%.4f", mean((p - y)^2)))
})

test_that("C17/C55 the intercept and coefficients reproduce every saved prediction, filtered rows included", {
  # property: plogis(b0 + x'b) per row; Output rows keyed to the filtered data's row names
  d <- val_fixture()[-(1:20), ]; d[["Ki-67 (%)"]][c(1, 5)] <- NA
  res <- val_run(d)
  pv <- res$predictions$.__enclos_env__$private
  expect_equal(as.integer(pv$.rowNums), as.integer(rownames(d)))
  cc <- stats::complete.cases(d[c("dx", VAL_VARS)]); ct <- val_coef(res)
  X <- val_design(d[cc, ], VAL_VARS)
  expect_equal(pv$.values[[1]][cc], as.numeric(stats::plogis(ct$b0 + X[, ct$terms, drop = FALSE] %*% ct$coef[ct$terms])),
               tolerance = 1e-10)
  expect_true(all(is.na(pv$.values[[1]][!cc])))
})

test_that("C53 unpenalized refit AIC and AUC match a hand Newton fit on the raw scale", {
  # oracle: Newton-Raphson logistic on unscaled predictors (AIC is scale-invariant)
  d <- val_fixture(); res <- val_run(d, showModelComparison = TRUE)
  ct <- val_coef(res); X <- val_design(d, VAL_VARS); y <- as.integer(d$dx == "NEC")
  g <- val_logit(X[, ct$terms, drop = FALSE], y); row <- val_key(res$modelComparison$asDF, "2")
  expect_equal(row$aic, g$aic, tolerance = 1e-6)
  expect_equal(row$auc, val_auc(y, g$p), tolerance = 1e-9)
})

# ── bootstrap validation ─────────────────────────────────────────────────────
test_that("C46/C47 bootstrap table: corrected = apparent - optimism; apparent slope by hand", {
  # oracle: Newton-Raphson slope of y on logit(p); identity per row
  d <- val_fixture(); res <- val_run(d, bootstrapValidation = TRUE, bootstrapN = 50)
  vt <- res$validationTable$asDF
  for (k in c("auc", "brier", "slope"))
    expect_equal(val_key(vt, k)$corrected, val_key(vt, k)$apparent - val_key(vt, k)$optimism, tolerance = 1e-12)
  p <- res$predictions$.__enclos_env__$private$.values[[1]]; y <- as.integer(d$dx == "NEC")
  expect_equal(val_key(vt, "slope")$apparent, val_logit(matrix(stats::qlogis(p)), y)$b[2], tolerance = 1e-6)
  expect_equal(val_key(vt, "brier")$apparent, mean((p - y)^2), tolerance = 1e-12)
})

# ── scoring system ───────────────────────────────────────────────────────────
test_that("C31-C37 points, criteria and odds ratios follow the documented rules on the original scale", {
  # oracle: contribution = coef (dummy) or coef x (mean above - mean below the median);
  # max-scaled = round(contribution / max|contribution| x 10)
  d <- val_fixture(); y <- as.integer(d$dx == "NEC")
  ct <- val_coef(val_run(d))
  res <- val_run(d, scoringSystem = TRUE, scoringMethod = "maxscaled")
  st <- res$scoringTable$asDF; st <- st[match(ct$terms, st$variable), ]
  contrib <- vapply(ct$terms, function(t) {
    if (t %in% names(d)) { x <- d[[t]]; m <- stats::median(x); ct$coef[[t]] * (mean(x[x > m]) - mean(x[x <= m])) }
    else ct$coef[[t]] }, 0)
  pts <- round(contrib / max(abs(contrib)) * 10); pts[pts == 0] <- sign(contrib[pts == 0])
  expect_equal(st$oddsRatio, unname(exp(contrib)), tolerance = 1e-8)
  expect_equal(as.numeric(st$points), unname(as.numeric(pts)))
  if ("Ki-67 (%)" %in% ct$terms)
    expect_identical(as.numeric(sub("^> ", "", st$criterion[st$variable == "Ki-67 (%)"])),
                     stats::median(d[["Ki-67 (%)"]]))
})

# ── RNG hygiene ──────────────────────────────────────────────────────────────
test_that("C13 the analysis leaves the global RNG state untouched", {
  d <- val_fixture(); set.seed(99); before <- .Random.seed
  invisible(val_run(d)); expect_identical(.Random.seed, before)
})

# ═══════════════════════════════════════════════════════════
# Fixed defects (VAL-lassologistic-01..10, fixed 2026-10-12): regression tests
# ═══════════════════════════════════════════════════════════

test_that("VAL-lassologistic-01 an ordinal predictor is reported and scored per level", {
  # jamovi delivers Ordinal as an ordered factor; contr.poly gave grade.L/grade.Q and a
  # 'grade.Q present' rule that awarded grades 1 AND 3
  d <- val_fixture(); d$grade <- factor(d$grade, levels = c("G1", "G2", "G3"), ordered = TRUE)
  for (st in c(TRUE, FALSE)) {
    res <- val_run(d, standardize = st, scoringSystem = TRUE, scoringMethod = "maxscaled",
                   showVariableImportance = TRUE)
    vi <- res$variableImportance$asDF$variable
    expect_false(any(grepl("^grade\\.(L|Q|C)", vi)), label = paste("standardize", st))
    expect_true(all(c("gradeG2", "gradeG3") %in% vi))
    expect_match(res$coefficients$notes$ordinal_note$note, "grade")
    st_tab <- res$scoringTable$asDF
    if ("gradeG3" %in% st_tab$variable)
      expect_equal(st_tab$criterion[st_tab$variable == "gradeG3"], "present")
  }
  # an unordered fit of the same data gives identical numbers: coding, not data, changed
  d2 <- d; d2$grade <- factor(d2$grade, ordered = FALSE)
  expect_equal(val_coef(val_run(d))$coef, val_coef(val_run(d2))$coef, tolerance = 1e-12)
  expect_null(val_run(d2)$coefficients$notes$ordinal_note)
})

test_that("VAL-lassologistic-02 the divide-by-smallest method is not called Schneeweiss", {
  # Schneeweiss 2003: +1 point per 0.3 ln(OR); the computed rule is round(c / min|c|)
  d <- val_fixture()
  res <- val_run(d, scoringSystem = TRUE, scoringMethod = "smallest")
  note <- res$scoringTable$notes$method$note
  expect_false(grepl("Schneeweiss", note))
  expect_match(note, "Sullivan")
  st <- res$scoringTable$asDF; c <- log(st$oddsRatio)
  pts <- round(c / min(abs(c))); pts[pts == 0] <- sign(c[pts == 0])
  expect_equal(as.numeric(st$points_smallest), pts)
  expect_error(val_run(d, scoringSystem = TRUE, scoringMethod = "schneeweiss"))   # the old key is gone
})

test_that("VAL-lassologistic-03 Beta10 is attributed to Mehta 2016", {
  # Zhang 2017 rounds beta with no x10; "Beta10/integer" is Mehta et al. 2016's label
  res <- val_run(val_fixture(), scoringSystem = TRUE, scoringMethod = "beta10")
  note <- res$scoringTable$notes$method$note
  expect_false(grepl("Zhang", note))
  expect_match(note, "Mehta")
  mc <- val_run(val_fixture(), scoringSystem = TRUE, scoringMethod = "compare")$methodComparison$asDF
  expect_equal(val_key(mc, "beta10")$reference, "Mehta et al. 2016")
})

test_that("VAL-lassologistic-04 the copy-ready sentence names the strongest retained terms and says it is a subset", {
  skip_if_not_installed("MASS")
  bw <- MASS::birthwt; bw$race <- factor(bw$race, levels = 1:3, labels = c("white", "black", "other"))
  v <- c("age", "lwt", "race", "smoke", "ptl", "ht", "ui", "ftv")
  res <- val_run(bw, outcome = "low", outcomeLevel = NULL, explanatory = v, showSummary = TRUE)
  s <- res$summaryText$content; ct <- val_coef(res)
  expect_gt(length(ct$terms), 5)
  expect_match(s, "including the five")
  # the five named are the five largest |beta| x SD
  X <- val_design(bw, v)
  mag <- abs(ct$coef[ct$terms]) * apply(X[, ct$terms, drop = FALSE], 2, stats::sd)
  top5 <- names(sort(mag, decreasing = TRUE))[1:5]
  expect_true(all(vapply(top5, grepl, TRUE, x = s, fixed = TRUE)))
  # five or fewer: all named
  d <- val_fixture(); r2 <- val_run(d, showSummary = TRUE); t2 <- val_coef(r2)$terms
  if (length(t2) <= 5) expect_true(all(vapply(t2, grepl, TRUE, x = r2$summaryText$content, fixed = TRUE)))
})

test_that("VAL-lassologistic-05 a model that predicts the base rate is graded 'No better', not 'Marginal'", {
  # hand: intercept-only model, p = 7/12 for all, Brier skill exactly 0
  d <- data.frame(y = factor(c(rep("pos", 7), rep("neg", 5)), levels = c("neg", "pos")),
                  a = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8), b = c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5))
  res <- val_run(d, outcome = "y", outcomeLevel = "pos", explanatory = c("a", "b"), nfolds = 3)
  expect_equal(val_stat(res, "Terms selected"), "0")
  expect_equal(val_key(res$performance$asDF, "brier")$interpretation, "No better than predicting the base rate")
})

test_that("VAL-lassologistic-06 the AUC band agrees with the AUC as displayed", {
  # seed 394: raw apparent AUC 0.699722, shown as 0.700 -> 'Acceptable', and no Poor notice
  set.seed(394); n <- 120
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  d$y <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(1.2 * d$x1)) == 1, "ev", "no"), levels = c("no", "ev"))
  res <- val_run(d, outcome = "y", outcomeLevel = "ev", explanatory = c("x1", "x2"), showModelComparison = TRUE)
  raw <- val_key(res$modelComparison$asDF, "1")$auc
  skip_if_not(raw < 0.7 && round(raw, 3) == 0.7, "fixture no longer lands just below 0.700")
  row <- val_key(res$performance$asDF, "auc")
  expect_match(row$value, "^0\\.700 ")
  expect_equal(row$interpretation, "Acceptable")
  expect_false(grepl("Poor Discrimination", res$notices$content))
})

test_that("VAL-lassologistic-07 the notes do not promise inference from bootstrap validation", {
  res <- val_run(val_fixture(), showMethodologyNotes = TRUE)
  expect_false(grepl("Use bootstrap validation for inference", res$methodologyNotes$content, fixed = TRUE))
  expect_match(res$methodologyNotes$content, "no coefficient inference")
  expect_false(grepl("unpenalized estimates", res$coefficients$notes$ci_note$note))   # that table has none
})

test_that("VAL-lassologistic-08 an explicit NA factor level is treated as missing, not as a category", {
  set.seed(5); d <- data.frame(y = factor(rep(c("ev", "no"), 30), levels = c("no", "ev")), x1 = stats::rnorm(60), x2 = stats::rnorm(60))
  g <- as.character(factor(sample(c("a", "b"), 60, TRUE))); g[1:6] <- NA
  d$g <- addNA(factor(g, levels = c("a", "b", "unused")))       # an unused level must survive as well
  res <- val_run(d, outcome = "y", outcomeLevel = "ev", explanatory = c("x1", "x2", "g"), showVariableImportance = TRUE)
  expect_equal(val_stat(res, "Complete cases analyzed"), "54")
  expect_true("gb" %in% res$variableImportance$asDF$variable)       # the real level is still a term
  expect_false(any(grepl("NA$", res$variableImportance$asDF$variable)))
  # a missing OUTCOME behind an NA level is reported as missing
  d2 <- d[, c("y", "x1", "x2")]; yy <- as.character(d2$y); yy[1:3] <- NA
  d2$y <- addNA(factor(yy, levels = c("no", "ev")))
  ms <- val_run(d2, outcome = "y", outcomeLevel = "ev", explanatory = c("x1", "x2"))$modelSummary$asDF
  expect_equal(ms$value[ms$statistic == "  - outcome missing"], "3")
  expect_false(any(ms$statistic == "  - outcome level not modeled"))
})

test_that("VAL-lassologistic-09 a predictor raised from 0 to 1 point is disclosed", {
  # Max-scaled with 3 points: a contribution under 1/6 of the largest rounds to 0 and is raised to 1
  set.seed(3); n <- 400
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  d$y <- factor(ifelse(stats::rbinom(n, 1, stats::plogis(1.5 * d$x1 + 0.25 * d$x2)) == 1, "ev", "no"), levels = c("no", "ev"))
  res <- val_run(d, outcome = "y", outcomeLevel = "ev", explanatory = c("x1", "x2"), lambda = "lambda.min",
                 scoringSystem = TRUE, scoringMethod = "maxscaled", scoringMaxPoints = 3)
  st <- res$scoringTable$asDF; c <- log(st$oddsRatio)
  raised <- st$variable[round(c / max(abs(c)) * 3) == 0]
  expect_gt(length(raised), 0)                                       # the fixture must exercise the rule
  expect_equal(abs(st$points[st$variable %in% raised]), rep(1L, length(raised)))
  expect_match(res$scoringTable$notes$promoted$note, raised[1], fixed = TRUE)
  # and no note when nothing was raised
  r2 <- val_run(d, outcome = "y", outcomeLevel = "ev", explanatory = c("x1", "x2"), lambda = "lambda.min",
                scoringSystem = TRUE, scoringMethod = "maxscaled", scoringMaxPoints = 20)
  c2 <- log(r2$scoringTable$asDF$oddsRatio)
  if (all(round(c2 / max(abs(c2)) * 20) != 0)) expect_null(r2$scoringTable$notes$promoted)
})

test_that("VAL-lassologistic-10 the apparent AUC interval is qualified as in-sample DeLong", {
  # simulation (validation script section F): coverage 83% of the true AUC; 8.45% null exclusions of 0.5
  res <- val_run(val_fixture())
  row <- val_key(res$performance$asDF, "auc")
  expect_match(row$metric, "DeLong 95% CI", fixed = TRUE)
  expect_match(res$performance$notes$auc_ci_note$note, "does not account for fitting")
  # no caveat where there is nothing to qualify: a constant score's 0.5-0.5 is exact
  d <- data.frame(y = factor(c(rep("pos", 7), rep("neg", 5)), levels = c("neg", "pos")),
                  a = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8), b = c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5))
  r0 <- val_run(d, outcome = "y", outcomeLevel = "pos", explanatory = c("a", "b"), nfolds = 3)
  expect_null(r0$performance$notes$auc_ci_note)
})

test_that("VAL-lassologistic-11 the ROC plot draws no second, wrong diagonal", {
  # abline(0, 1) on pROC's specificity axis drew sensitivity = specificity (the
  # anti-diagonal) beside pROC's own chance line; found when the plot was inspected
  body_txt <- paste(deparse(body(ClinicoPath:::lassologisticClass$private_methods$.rocPlot)), collapse = "\n")
  expect_match(body_txt, "abline(a = 1, b = -1", fixed = TRUE)
  expect_false(grepl("pROC::plot.roc", body_txt, fixed = TRUE))
  res <- val_run(val_fixture())
  f <- tempfile(fileext = ".png"); res$roc_plot$saveAs(f)
  expect_gt(file.size(f), 1000)
})
