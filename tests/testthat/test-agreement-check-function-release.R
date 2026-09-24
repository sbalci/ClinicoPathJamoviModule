# Regression tests from /check-function agreement --profile release (2026-09-23)
# and the adversarial review of that pass (2026-09-24).
#   * 'Kappa interval method' (kappaCIMethod) is honoured by every asymptotic
#     Cohen's kappa interval - headline, All-Pairs and intra-rater - and each
#     scale is what the option describes;
#   * CI column titles follow the confidence level;
#   * the hierarchical tables declare no CI columns they never fill;
#   * Gwet's AC p-value is two-sided whatever irrCAC version is installed;
#   * the directional discordance test counts complete pairs, not rows;
#   * Light's kappa carries no p-value (irr's variance formula is invalid);
#   * All-Pairs rows cannot collide on rater names;
#   * the continuous cluster test is boundary-corrected and labelled as what it tests.
#
# Every p-value test below asserts that the old and the new value DIFFER on its
# fixture before comparing: a strong-signal fixture underflows every p to 0, and
# the first version of the Gwet test compared 0 with 0 (CHANGELOG 2026-09-23).
#
# The package under test provides the wrapper; do NOT source() R/agreement.*.R
# here (see test-agreement-validation.R for why).
agreement <- ClinicoPath::agreement

.three_raters <- function() {
  g <- c("G1", "G2", "G3")
  n <- c(20, 3, 1, 4, 18, 3, 1, 2, 15)   # 67 cases; A rows, B columns
  A <- rep(c("G1", "G1", "G1", "G2", "G2", "G2", "G3", "G3", "G3"), n)
  B <- rep(c("G1", "G2", "G3", "G1", "G2", "G3", "G1", "G2", "G3"), n)
  C <- A
  C[c(2, 9, 30, 41, 60)] <- c("G2", "G3", "G1", "G3", "G1")
  data.frame(A = factor(A, levels = g), B = factor(B, levels = g),
             C = factor(C, levels = g))
}

.closed_form <- function(k, se, level, method) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (identical(method, "fisherz")) {
    tanh(atanh(k) + c(-1, 1) * z * se / (1 - k^2))
  } else {
    c(max(-1, k - z * se), min(1, k + z * se))
  }
}

.note_text <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " ")

test_that("the headline kappa interval follows kappaCIMethod and matches the closed forms", {
  skip_if_not_installed("vcd")
  d <- .three_raters()[, c("A", "B")]
  se <- unname(vcd::Kappa(table(d$A, d$B))$Unweighted[["ASE"]])
  w <- agreement(data = d, vars = vars(A, B), kappaCIMethod = "wald")$irrtable$asDF
  f <- agreement(data = d, vars = vars(A, B), kappaCIMethod = "fisherz")$irrtable$asDF
  k <- w$kappa[1]
  expect_true(is.finite(k) && is.finite(se) && se > 0)
  expect_equal(f$kappa[1], k)                       # the estimate does not move
  expect_equal(c(w$ci_lower[1], w$ci_upper[1]), .closed_form(k, se, 0.95, "wald"),
               tolerance = 1e-8)
  expect_equal(c(f$ci_lower[1], f$ci_upper[1]), .closed_form(k, se, 0.95, "fisherz"),
               tolerance = 1e-8)
  # Wald is symmetric about kappa (unclamped here); Fisher z is not, and stays inside (-1, 1)
  expect_equal(w$ci_upper[1] - k, k - w$ci_lower[1], tolerance = 1e-8)
  expect_lt(f$ci_upper[1] - k, k - f$ci_lower[1])
  expect_true(all(abs(c(f$ci_lower[1], f$ci_upper[1])) < 1))
})

test_that("the All-Pairs table uses the same interval scale and titles its CI columns from confLevel", {
  skip_if_not_installed("vcd")
  d <- .three_raters()
  se <- unname(vcd::Kappa(table(d$A, d$B))$Unweighted[["ASE"]])
  r <- agreement(data = d, vars = vars(A, B, C), allPairsKappa = TRUE,
                 kappaCIMethod = "fisherz", confLevel = 0.90)
  tb <- r$allPairsKappaTable$asDF
  ab <- tb[tb$rater_a == "A" & tb$rater_b == "B", ]
  expect_equal(nrow(ab), 1L)
  expect_equal(c(ab$ci_lower, ab$ci_upper), .closed_form(ab$kappa, se, 0.90, "fisherz"),
               tolerance = 1e-8)
  expect_identical(r$allPairsKappaTable$getColumn("ci_lower")$title, "90% CI Lower")
  expect_identical(r$allPairsKappaTable$getColumn("ci_upper")$title, "90% CI Upper")
})

test_that("the intra-rater table uses the interval kappaCIMethod selects, not a rebuilt Wald one", {
  skip_if_not_installed("vcd")
  t3 <- .three_raters()
  d <- data.frame(A_1 = t3$A, A_2 = t3$B, B_1 = t3$C, B_2 = t3$A)
  se <- unname(vcd::Kappa(table(d$A_1, d$A_2))$Unweighted[["ASE"]])
  r <- agreement(data = d, vars = names(d), interIntraRater = TRUE,
                 kappaCIMethod = "fisherz")
  tb <- r$interIntraRaterIntraTable$asDF
  a <- tb[tb$rater == "A", ]
  expect_equal(nrow(a), 1L)
  k <- a$value
  expect_true(is.finite(k) && abs(k) < 1)
  fz <- .closed_form(k, se, 0.95, "fisherz")
  wd <- .closed_form(k, se, 0.95, "wald")
  expect_gt(max(abs(fz - wd)), 1e-3)                # the two scales differ here
  expect_equal(c(a$ci_lower, a$ci_upper), fz, tolerance = 1e-8)
})

test_that("the hierarchical tables declare no confidence-interval columns", {
  d <- .three_raters()
  r <- agreement(data = d, vars = vars(A, B))
  for (nm in c("hierarchicalOverallTable", "hierarchicalICCTable")) {
    # $columns lists every DECLARED column, hidden or not; asDF drops hidden
    # ones, so an asDF check also passed when the columns were declared and hidden.
    cols <- names(r[[nm]]$columns)
    expect_true(length(cols) > 0, info = nm)
    expect_false(any(c("ci_lower", "ci_upper") %in% cols), info = nm)
  }
})

# Gwet's AC: irrCAC 1.0 returned a two-sided p and irrCAC 1.4 a one-sided one, so
# the module recomputes the two-sided p from the coefficient and SE on n - 1 df.
.gwet_case <- function(d, lv) {
  d[] <- lapply(d, factor, levels = lv)
  r <- agreement(data = d, vars = names(d), gwet = TRUE)
  up <- irrCAC::gwet.ac1.raw(as.matrix(d), weights = "unweighted",
                             categ.labels = lv, conflev = 0.95, N = Inf)$est
  list(tb = r$gwetTable$asDF, up = up,
       oracle = 2 * stats::pt(-abs(up$coeff.val / up$coeff.se), nrow(d) - 1))
}

test_that("Gwet's AC p-value is two-sided near chance, where one- and two-sided differ", {
  skip_if_not_installed("irrCAC")
  lv <- c("neg", "pos", "equiv")
  d <- data.frame(
    A = c("neg","pos","neg","equiv","pos","neg","pos","equiv","neg","pos","neg","pos","equiv","neg","pos","neg","equiv","pos","neg","pos"),
    B = c("neg","pos","pos","equiv","neg","neg","pos","neg","neg","equiv","neg","pos","pos","neg","pos","equiv","equiv","neg","neg","pos"),
    C = c("pos","pos","neg","neg","pos","neg","equiv","equiv","neg","pos","pos","pos","equiv","equiv","pos","neg","neg","pos","neg","neg"))
  g <- .gwet_case(d, lv)
  expect_equal(g$tb$coefficient[1], g$up$coeff.val, tolerance = 1e-8)
  expect_gt(g$up$coeff.val, 0)
  # Separation guard: the value this test rejects (irrCAC's own p) is far from the oracle.
  expect_gt(abs(g$oracle - g$up$p.value), 1e-3)
  expect_equal(g$tb$p[1], g$oracle, tolerance = 1e-8)
})

test_that("Gwet's AC p-value is small for strong disagreement, not 1", {
  skip_if_not_installed("irrCAC")
  lv <- c("neg", "pos", "equiv")
  A <- rep(c("neg", "pos", "equiv"), length.out = 18)
  B <- rep(c("pos", "equiv", "neg"), length.out = 18)
  C <- rep(c("equiv", "neg", "pos"), length.out = 18)
  B[c(1, 7)] <- A[c(1, 7)]
  g <- .gwet_case(data.frame(A = A, B = B, C = C), lv)
  expect_lt(g$up$coeff.val, 0)
  expect_gt(abs(g$oracle - g$up$p.value), 0.5)      # irrCAC 1.4 says 1 here
  expect_equal(g$tb$p[1], g$oracle, tolerance = 1e-8)
  expect_lt(g$tb$p[1], 0.05)
})

test_that("the directional discordance test refuses fewer than 3 complete pairs", {
  # irr::rater.bias() reads a square input as a table of COUNTS, so 2 x 2 data was
  # scored as a count table; and it drops incomplete pairs itself, so a row with
  # one rating missing must not count toward the 3.
  g <- c("Low", "High")
  two <- data.frame(A = factor(c("Low", "High"), levels = g, ordered = TRUE),
                    B = factor(c("High", "Low"), levels = g, ordered = TRUE))
  partial <- data.frame(A = factor(c("Low", "High", "Low"), levels = g, ordered = TRUE),
                        B = factor(c("High", "Low", NA), levels = g, ordered = TRUE))
  for (d in list(two, partial)) {
    tb <- agreement(data = d, vars = vars(A, B), raterBias = TRUE)$raterBiasTable
    expect_true(grepl("At least 3 cases", .note_text(tb), fixed = TRUE))
    df <- tb$asDF
    expect_true(nrow(df) == 0 || all(is.na(df$chisq)))
  }
})

test_that("Light's kappa reports no p-value and says why", {
  r <- agreement(data = .three_raters(), vars = vars(A, B, C), lightKappa = TRUE)
  tb <- r$lightKappaTable
  expect_false("p" %in% names(tb$columns))
  expect_true(is.finite(tb$asDF$kappa[1]))
  expect_true(grepl("kappam.light", .note_text(tb), fixed = TRUE))
})

test_that("All-Pairs rows are keyed by position, so rater names cannot collide", {
  # Joining names with "__" gave ("A", "B__C") and ("A__B", "C") the same key,
  # and the second row silently replaced the first.
  t3 <- .three_raters()
  d <- data.frame(t3$A, t3$B, t3$C, t3$A, check.names = FALSE)
  names(d) <- c("A", "A__B", "B__C", "C")
  d[[4]][c(3, 11, 25)] <- c("G2", "G3", "G1")
  tb <- agreement(data = d, vars = names(d), allPairsKappa = TRUE)$allPairsKappaTable$asDF
  expect_equal(nrow(tb), 6L)
  expect_setequal(paste(tb$rater_a, tb$rater_b, sep = " | "),
                  apply(utils::combn(names(d), 2), 2, paste, collapse = " | "))
})

test_that("rater names with punctuation and non-ASCII letters reach the table verbatim", {
  t3 <- .three_raters()
  nm <- c("Ki-67 (%)", "A+B", "Patölog Ç")
  d <- data.frame(t3$A, t3$B, t3$C, check.names = FALSE)
  names(d) <- nm
  # vars = names(d), not vars = nm: the generated wrapper resolves a bare symbol as a
  # column NAME, so `nm` would be looked up as a column called "nm".
  tb <- agreement(data = d, vars = names(d), allPairsKappa = TRUE)$allPairsKappaTable$asDF
  expect_equal(nrow(tb), 3L)
  expect_setequal(unique(c(tb$rater_a, tb$rater_b)), nm)
  expect_true(all(is.finite(tb$kappa)))
})

test_that("the continuous cluster test is boundary-corrected and labelled as a test of rating level", {
  skip_if_not_installed("lme4")
  set.seed(21); n <- 120
  lab <- factor(rep(paste0("Lab", 1:4), each = 30))
  d <- data.frame(r1 = rnorm(n, 50, 15) + c(0, 3, 6, 9)[as.integer(lab)],
                  r2 = NA_real_, lab = lab)
  d$r2 <- d$r1 + rnorm(n, 0, 5)
  r <- agreement(data = d, vars = c("r1", "r2"), hierarchicalKappa = TRUE,
                 clusterVariable = "lab", testClusterHomogeneity = TRUE)
  tb <- r$homogeneityTestTable
  row <- tb$asDF
  expect_equal(nrow(row), 1L)
  expect_true(grepl("institution variance", row$test_name[1], fixed = TRUE))
  expect_false(grepl("agreement differs by", row$conclusion[1], fixed = TRUE))
  expect_true(grepl("Stram and Lee", .note_text(tb), fixed = TRUE))
  # The p is the 50:50 chi-square(0):chi-square(1) mixture, half the naive value.
  stat <- row$statistic[1]
  expect_true(is.finite(stat) && stat > 0)
  naive <- stats::pchisq(stat, df = 1, lower.tail = FALSE)
  expect_gt(naive / 2, 1e-4)                        # separation guard: not underflowed
  expect_equal(row$p_value[1], naive / 2, tolerance = 1e-10)
})
