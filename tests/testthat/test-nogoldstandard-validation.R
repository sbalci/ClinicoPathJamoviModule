# Numerical validation, 2026-10-03. Independent estimands, not backend helper calls.
ngsv_data <- function(counts = c(2, 1, 1, 2, 1, 1, 1, 2)) {
  patterns <- expand.grid(t1 = 0:1, t2 = 0:1, t3 = 0:1)
  x <- patterns[rep(seq_len(8), counts), ]
  as.data.frame(lapply(x, function(v) factor(ifelse(v == 1, 'pos', 'neg'),
                                           levels = c('pos', 'neg'))))
}
ngsv_run <- function(d, method = 'composite', ...) {
  args <- list(data = d, method = method,
    test1 = names(d)[1], test1Positive = 'pos',
    test2 = names(d)[2], test2Positive = 'pos',
    test3 = names(d)[3], test3Positive = 'pos',
    test4 = NULL, test4Positive = NULL, test5 = NULL, test5Positive = NULL)
  do.call(ClinicoPath::nogoldstandard, utils::modifyList(args, list(...), keep.null = TRUE))
}
ngsv_wilson <- function(success, total, alpha = .05) {
  # Score-test inversion: total*(phat-p)^2 <= qnorm(1-alpha/2)^2*p*(1-p).
  z2 <- qnorm(1 - alpha / 2)^2
  a <- total + z2; b <- -(2 * success + z2); c <- success^2 / total
  sort(c((-b - sqrt(b^2 - 4*a*c)) / (2*a),
         (-b + sqrt(b^2 - 4*a*c)) / (2*a)))
}

test_that('C01-C10 rule proportions and agreement match independent contingency arithmetic', {
  d <- ngsv_data()
  x <- as.matrix(d == 'pos')
  for (method in c('composite', 'all_positive', 'any_positive')) {
    ref <- switch(method, composite = rowSums(x) >= 2,
                  all_positive = rowSums(x) == 3, any_positive = rowSums(x) >= 1)
    r <- ngsv_run(d, method)
    expect_equal(r$prevalence$asDF$estimate, mean(ref), tolerance = 1e-12)
    expect_equal(unname(unlist(r$prevalence$asDF[c('ci_lower', 'ci_upper')])),
                 ngsv_wilson(sum(ref), length(ref)), tolerance = 1e-12)
    for (j in 1:3) {
      tp <- sum(x[,j] & ref); tn <- sum(!x[,j] & !ref)
      expected <- c(tp / sum(ref), tn / sum(!ref), tp / sum(x[,j]), tn / sum(!x[,j]))
      if (method == 'all_positive') expected[c(1, 4)] <- NA_real_
      if (method == 'any_positive') expected[c(2, 3)] <- NA_real_
      expect_equal(unname(unlist(r$test_metrics$asDF[j,
        c('sensitivity', 'specificity', 'ppv', 'npv')])), expected, tolerance = 1e-12)
    }
  }
})

test_that('C11-C13 pairwise kappa and its null test agree with independent binary formulas', {
  skip_if_not_installed('irr')
  d <- ngsv_data(); x <- as.matrix(d == 'pos')
  tab <- ngsv_run(d)$agreement_stats$asDF
  pairs <- combn(3, 2)
  for (j in 1:3) {
    a <- x[,pairs[1,j]]; b <- x[,pairs[2,j]]
    pa <- mean(a); pb <- mean(b); po <- mean(a == b)
    pe <- pa * pb + (1 - pa) * (1 - pb)
    z <- (mean(a*b) - pa*pb) * sqrt(nrow(x)) / sqrt(pa*(1-pa)*pb*(1-pb))
    expect_equal(tab$kappa[j], (po-pe)/(1-pe), tolerance = 1e-12)
    expect_equal(tab$p_value[j], 2*pnorm(-abs(z)), tolerance = 1e-12)
    expect_equal(tab$agreement[j], po, tolerance = 1e-12)
  }
})

test_that('C14-C16 and C39 complete response counts agree with the heatmap', {
  r <- ngsv_run(ngsv_data())
  expect_equal(sum(r$crosstab$asDF$count), 11)
  expect_equal(sum(r$crosstab$asDF$percentage), 1, tolerance = 1e-12)
  expect_equal(nrow(r$crosstab$asDF), 8L)
  expect_equal(diag(r$agreement_plot$state$agreement_matrix), rep(1, 3),
               ignore_attr = TRUE)
  expect_equal(r$agreement_plot$state$agreement_matrix[1,2],
               r$agreement_stats$asDF$agreement[1], tolerance = 1e-12)
})

test_that('C17-C21 latent estimates match the independent three-test moment solution', {
  skip_if_not_installed('poLCA')
  counts <- c(305, 55, 85, 95, 135, 65, 55, 205)
  pat <- as.matrix(expand.grid(0:1, 0:1, 0:1)); p <- counts / sum(counts)
  mu <- colSums(pat*p); centered <- sweep(pat, 2, mu, '-')
  covariance <- crossprod(centered, centered*p)
  c12 <- covariance[1,2]; c13 <- covariance[1,3]; c23 <- covariance[2,3]
  ratio <- sum(p*centered[,1]*centered[,2]*centered[,3]) / sqrt(c12*c13*c23)
  prevalence <- (1-ratio/sqrt(ratio^2+4))/2
  delta <- sqrt(c(c12*c13/c23, c12*c23/c13, c13*c23/c12) /
                  (prevalence*(1-prevalence)))
  positive <- mu+(1-prevalence)*delta; negative <- 1-mu+prevalence*delta
  r <- ngsv_run(ngsv_data(counts), 'latent_class')
  expect_equal(r$prevalence$asDF$estimate, prevalence, tolerance = 1e-6)
  expect_equal(r$test_metrics$asDF$sensitivity, positive, tolerance = 1e-6,
               ignore_attr = TRUE)
  expect_equal(r$test_metrics$asDF$specificity, negative, tolerance = 1e-6,
               ignore_attr = TRUE)
})

test_that('C22 regular three-test likelihood reaches the independently saturated value', {
  skip_if_not_installed('poLCA')
  counts <- c(305, 55, 85, 95, 135, 65, 55, 205)
  # This fixture has an interior moment solution, so multinomial entropy is the maximum.
  expected <- sum(counts * log(counts / sum(counts)))
  r <- ngsv_run(ngsv_data(counts), 'latent_class')
  fit <- r$model_fit$asDF
  expect_equal(fit$value[fit$statistic == 'Log-Likelihood'], expected, tolerance = 1e-9)
})

test_that('C30 bootstrap prevalence matches independent case resampling', {
  d <- ngsv_data(); x <- as.matrix(d == 'pos')
  # Percentile interval uses R quantile type 7; resample whole patient rows.
  expected <- withr::with_seed(23, replicate(100, {
    index <- sample.int(nrow(x), nrow(x), replace = TRUE)
    mean(rowSums(x[index, , drop = FALSE]) >= 2)
  }))
  r <- ngsv_run(d, bootstrap = TRUE, nboot = 100, seed = 23)
  expect_equal(unname(unlist(r$prevalence$asDF[c('ci_lower', 'ci_upper')])),
               unname(quantile(expected, c(.025, .975), type = 7)), tolerance = 1e-12)
})

test_that('C36-C38 ordinary missing cases and selected factor ordering are respected', {
  d <- ngsv_data(); d[11,1] <- NA
  r <- ngsv_run(d)
  expect_equal(sum(r$crosstab$asDF$count), 10)
  expect_match(r$notices$content, 'Excluded 1 case')
  swapped <- as.data.frame(lapply(d, function(x) factor(x, levels = rev(levels(x)))))
  expect_equal(ngsv_run(swapped)$prevalence$asDF, r$prevalence$asDF)
})

test_that('C37 explicit NA factor levels are excluded, VAL-nogoldstandard-01', {
  d <- ngsv_data(); d[11,1] <- NA; d[] <- lapply(d, addNA)
  r <- ngsv_run(d)
  expect_equal(sum(r$crosstab$asDF$count), 10)
  expect_equal(r$prevalence$asDF$estimate, 5/10, tolerance = 1e-12)
  expect_match(r$notices$content, 'Excluded 1 case')
})

test_that('C35 three-test output does not claim universal exact fit, VAL-nogoldstandard-02', {
  # Allman et al., arXiv:1710.01696, Theorem 2 and Proposition 5.
  r <- ngsv_run(ngsv_data(c(1,25,25,1,25,1,1,1)), 'latent_class')
  notes <- paste(vapply(r$model_fit$notes, function(x) x$note, ''), collapse = ' ')
  expect_false(grepl('reproduces the observed table exactly', notes, fixed = TRUE))
  fit <- r$model_fit$asDF
  # Independent constrained optimum from the validation oracle; nominal df is zero.
  expect_equal(fit$value[fit$statistic == 'G-squared'], 55.13284931525104,
               tolerance = 1e-6)
  expect_equal(nrow(r$conditional_dependence$asDF), 3L)
})

test_that('C09-C10 predictive values retain defined boundary ratios, VAL-nogoldstandard-03', {
  # All cases rule-negative: NPV = TN/(TN+FN) = 20/20, despite undefined sensitivity.
  r <- ngsv_run(ngsv_data(c(20,0,0,0,0,0,0,0)))
  expect_equal(r$test_metrics$asDF$npv, rep(1,3), tolerance = 1e-12)
})

test_that('C46 regular penalized EM finishes, VAL-nogoldstandard-05', {
  r <- ngsv_run(ngsv_data(c(305,55,85,95,135,65,55,205)), 'bayesian',
                verbose = TRUE, bootstrap = TRUE, nboot = 100, seed = 9)
  expect_false(grepl('The estimation did not converge', r$notices$content, fixed = TRUE))
  expect_equal(r$prevalence$asDF$estimate, 0.40121306868618939, tolerance = 1e-8)
  expect_true(all(is.finite(unlist(r$prevalence$asDF))))
  expect_true(all(is.finite(r$test_metrics$asDF$sens_ci_lower)))
  expect_true(all(is.finite(r$test_metrics$asDF$spec_ci_upper)))
  expect_match(r$notices$content, 'coverage is approximate')
})

test_that('C49 rule-group notice uses correct denominator terminology, VAL-nogoldstandard-06', {
  r <- ngsv_run(ngsv_data(c(2,2,1,1,1,0,0,1)))
  expect_equal(r$test_metrics$asDF$ppv[1], 2/4, tolerance = 1e-12)
  expect_false(grepl('Every percentage in the table below is computed on one of these two counts',
                     r$notices$content, fixed = TRUE))
})

test_that('C12 very small p-values retain stable tail accuracy, VAL-nogoldstandard-07', {
  d <- ClinicoPath::nogoldstandard_pathology[, c('Pathologist1', 'Pathologist2', 'Pathologist3')]
  d <- as.data.frame(lapply(d, function(x) factor(ifelse(is.na(x), NA,
    ifelse(x == 'Malignant', 'pos', 'neg')), levels = c('pos', 'neg'))))
  d <- d[complete.cases(d), ]
  x <- as.matrix(d == 'pos')
  tab <- table(x[,1], x[,3])
  expected <- pchisq(unname(chisq.test(tab, correct = FALSE)$statistic), 1,
                     lower.tail = FALSE)
  actual <- ngsv_run(d)$agreement_stats$asDF$p_value[2]
  expect_lt(abs(actual / expected - 1), 1e-8)
})

test_that('C50 a uniform table does not identify unique latent parameters, VAL-nogoldstandard-08', {
  # pi=.2,q1=(.9,.5,.5),q0=(.4,.5,.5) and pi=.4,q1=(.8,.5,.5),q0=(.3,.5,.5)
  # both yield probability 1/8 for every response pattern, with oriented classes.
  r <- ngsv_run(ngsv_data(rep(100,8)), 'latent_class')
  expect_true(is.na(r$prevalence$asDF$estimate))
  expect_match(r$notices$content, 'not identifiable|non-identif|not estimable',
               ignore.case = TRUE)
})

test_that('uninformative extra tests do not create latent identification', {
  for (k in 3:5) {
    patterns <- as.matrix(expand.grid(rep(list(0:1), k)))
    d <- as.data.frame(lapply(as.data.frame(patterns[rep(seq_len(2^k), 20), ]),
      function(x) factor(ifelse(x == 1, 'pos', 'neg'), levels = c('pos', 'neg'))))
    names(d) <- paste0('t', seq_len(k))
    args <- list()
    if (k >= 4) args <- c(args, list(test4 = 't4', test4Positive = 'pos'))
    if (k == 5) args <- c(args, list(test5 = 't5', test5Positive = 'pos'))
    for (method in c('latent_class', 'bayesian')) {
      r <- do.call(ngsv_run, c(list(d, method, bootstrap = TRUE, nboot = 100), args))
      expect_true(all(is.na(unlist(r$prevalence$asDF))))
      expect_true(all(is.na(unlist(r$test_metrics$asDF[-1]))))
      expect_match(r$notices$content, 'not estimable')
      expect_match(r$notices$content, 'bootstrap refitting was skipped')
      expect_equal(sum(r$crosstab$asDF$count), nrow(d))
      expect_equal(nrow(r$agreement_stats$asDF), choose(k, 2))
    }
  }
})

test_that('identification distinguishes two from three informative tests', {
  patterns <- as.matrix(expand.grid(rep(list(0:1), 5)))
  make_tab <- function(q1) {
    probabilities <- apply(sweep(patterns, 2, q1,
      function(x, p) p^x * (1-p)^(1-x)), 1, prod) * .4 +
      apply(sweep(patterns, 2, rep(.5, 5),
        function(x, p) p^x * (1-p)^(1-x)), 1, prod) * .6
    list(patterns = patterns, counts = probabilities * 10000, n = 10000, k = 5)
  }
  q2 <- c(.9, .8, .5, .5, .5)
  q3 <- c(.9, .8, .7, .5, .5)
  identify <- ClinicoPath:::nogoldstandard_identification
  expect_false(identify(make_tab(q2), c(.4, q2, rep(.5, 5)))$identified)
  expect_true(identify(make_tab(q3), c(.4, q3, rep(.5, 5)))$identified)
})

test_that('penalized fitting explores asymmetric test response patterns', {
  counts <- c(1, 25, 25, 1, 25, 1, 1, 1)
  r <- ngsv_run(ngsv_data(counts), 'bayesian')
  par <- c(r$prevalence$asDF$estimate, r$test_metrics$asDF$sensitivity,
           1 - r$test_metrics$asDF$specificity)
  patterns <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  objective <- function(p) {
    component <- function(q) apply(sweep(patterns, 2, q,
      function(x, prob) prob^x * (1-prob)^(1-x)), 1, prod)
    sum(counts * log(p[1]*component(p[2:4]) + (1-p[1])*component(p[5:7]))) +
      sum(log(p[2:4])) + sum(log1p(-p[5:7]))
  }
  expect_true(all(is.finite(par)))
  # This explicitly feasible point is better than the exchangeable-start solution.
  expect_gt(objective(par), objective(c(1/3, .99, .1, .1, .02, .49, .49)))
})

test_that('near-boundary refinement never evaluates invalid Hessian perturbations', {
  patterns <- as.matrix(expand.grid(0:1, 0:1, 0:1))
  par <- c(.4, 1e-5, .8, .7, 1e-6, .2, .3)
  prob <- ClinicoPath:::nogoldstandard_probabilities(par, patterns)$prob
  counts <- round(1e8 * prob)
  tab <- list(patterns = patterns, counts = counts, n = sum(counts), k = 3)
  expect_warning(fit <- ClinicoPath:::nogoldstandard_refine(tab, par, TRUE), NA)
  expect_true(all(is.finite(fit$par)))
  # A strict score failure is allowed here, and must not be called convergence.
  expect_identical(fit$converged, fit$score < 1e-7)
})

test_that('finite bootstrap cutoff is explicit and preserves the 20-value boundary', {
  d <- ngsv_data()
  options <- ClinicoPath:::nogoldstandardOptions$new(
    test1 = 't1', test1Positive = 'pos', test2 = 't2', test2Positive = 'pos',
    test3 = 't3', test3Positive = 'pos', test4 = NULL, test4Positive = NULL,
    test5 = NULL, test5Positive = NULL, method = 'composite')
  private <- ClinicoPath:::nogoldstandardClass$new(options = options, data = d)$.__enclos_env__$private
  expect_true(is.na(private$.bootCI(seq_len(19), .05)$lower))
  expect_equal(unlist(private$.bootCI(seq_len(20), .05)),
               setNames(as.numeric(quantile(seq_len(20), c(.025, .975))), c('lower', 'upper')))
})

test_that('C34 suppressed latent estimates have a nonnumeric summary', {
  r <- ngsv_run(ngsv_data(rep(10L, 8)), 'latent_class', showSummary = TRUE)
  expect_true(is.na(r$prevalence$asDF$estimate))
  expect_match(r$clinical_summary$content, 'not estimable')
  expect_false(grepl('NA%|NaN%', r$clinical_summary$content))
})

test_that('unused poLCA standard errors do not affect fitted values or emit warnings', {
  d <- as.data.frame(lapply(ngsv_data(), function(x) as.integer(x == 'pos') + 1L))
  f <- cbind(t1, t2, t3) ~ 1
  fit <- function(se) withr::with_seed(100, poLCA::poLCA(f, d, nclass = 2,
    graphs = FALSE, verbose = FALSE, nrep = 1, maxiter = 1000, calc.se = se))
  original <- suppressWarnings(fit(TRUE))
  expect_warning(revised <- fit(FALSE), NA)
  fields <- c('P', 'probs', 'llik', 'Gsq', 'Chisq', 'numiter', 'eflag', 'predcell')
  expect_identical(revised[fields], original[fields])
  expect_warning(ngsv_run(ngsv_data(), 'latent_class', showSummary = TRUE), NA)
})
