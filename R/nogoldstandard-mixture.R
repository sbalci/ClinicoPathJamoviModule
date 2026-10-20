# Probability-scale likelihood utilities for nogoldstandard's binary two-class model.
# Parameter order: class weight, P(test+ | class 1), P(test+ | class 0).
nogoldstandard_orientation <- function(positive, negative) {
  contrast <- mean(positive) - mean(negative)
  # Numerical label guard on the probability scale, not a significance test.
  tied <- !is.finite(contrast) || abs(contrast) <= 1e-6
  list(contrast = contrast, tied = tied)
}

nogoldstandard_pattern_data <- function(data) {
  k <- ncol(data)
  patterns <- as.matrix(expand.grid(rep(list(0:1), k)))
  index <- 1L + as.integer(as.matrix(data) %*% 2^(seq_len(k) - 1L))
  list(patterns = patterns, counts = tabulate(index, nbins = 2^k), n = nrow(data), k = k)
}

nogoldstandard_probabilities <- function(par, patterns) {
  k <- ncol(patterns)
  # apply(x, 1, prod) was ~60% of penalized-EM bootstrap time (the refinement Hessian
  # calls this hundreds of times per resample); k <= 5 columns, so loop over columns.
  rowprod <- function(x) {
    out <- rep(1, nrow(x))
    for (j in seq_len(ncol(x))) out <- out * x[, j]
    out
  }
  terms <- lapply(list(par[1L + seq_len(k)], par[1L + k + seq_len(k)]), function(q) {
    sweep(patterns, 2, q, function(x, p) ifelse(x == 1, p, 1 - p))
  })
  component <- lapply(terms, rowprod)
  jacobian <- matrix(0, nrow(patterns), 1L + 2L * k)
  jacobian[, 1] <- component[[1]] - component[[2]]
  for (j in seq_len(k)) {
    sign <- 2 * patterns[, j] - 1
    jacobian[, 1L + j] <- par[1] * sign * rowprod(terms[[1]][, -j, drop = FALSE])
    jacobian[, 1L + k + j] <- (1 - par[1]) * sign * rowprod(terms[[2]][, -j, drop = FALSE])
  }
  list(prob = par[1] * component[[1]] + (1 - par[1]) * component[[2]],
       component = component, jacobian = jacobian)
}

nogoldstandard_refine <- function(tab, start, penalized = FALSE, checkpoint = function() {}) {
  k <- tab$k
  lower <- rep(1e-12, length(start))
  upper <- 1 - lower
  positive <- 1L + seq_len(k)
  negative <- 1L + k + seq_len(k)
  used <- tab$counts > 0
  last_par <- NULL
  last_value <- NULL
  evaluate <- function(par) {
    if (identical(par, last_par)) return(last_value)
    z <- nogoldstandard_probabilities(par, tab$patterns)
    p <- pmax(z$prob, .Machine$double.xmin)
    value <- -sum(tab$counts[used] * log(p[used]))
    gradient <- -colSums(z$jacobian[used, , drop = FALSE] * (tab$counts[used] / p[used]))
    if (penalized) {
      value <- value - sum(log(par[positive])) - sum(log1p(-par[negative]))
      gradient[positive] <- gradient[positive] - 1 / par[positive]
      gradient[negative] <- gradient[negative] + 1 / (1 - par[negative])
    }
    last_par <<- par
    last_value <<- list(value = value / tab$n, gradient = gradient / tab$n)
    last_value
  }
  objective <- function(par) evaluate(par)$value
  gradient <- function(par) evaluate(par)$gradient
  start <- pmin(upper, pmax(lower, start))
  checkpoint()
  fit <- stats::nlminb(start, objective, gradient, lower = lower, upper = upper,
    control = list(iter.max = 1000L, eval.max = 2000L, rel.tol = 1e-13, x.tol = 1e-12))
  par <- if (is.finite(fit$objective) && fit$objective <= objective(start) + 1e-13)
    fit$par else start

  # Likelihood increments can vanish before parameters settle. Polish the free
  # coordinates on an interior or boundary face, accepting only feasible descent.
  for (i in seq_len(4L)) {
    checkpoint()
    free <- which(par > lower + 1e-7 & par < upper - 1e-7)
    if (!length(free)) break
    free_objective <- function(theta) {
      candidate <- par
      candidate[free] <- theta
      objective(candidate)
    }
    free_gradient <- function(theta) {
      candidate <- par
      candidate[free] <- theta
      gradient(candidate)[free]
    }
    step_size <- pmin(1e-5, (par[free] - lower[free]) / 4,
                      (upper[free] - par[free]) / 4)
    hessian <- stats::optimHess(par[free], free_objective, free_gradient,
                              control = list(ndeps = step_size))
    if (any(!is.finite(hessian))) break
    curvature <- eigen(hessian, symmetric = TRUE, only.values = TRUE)$values
    if (min(curvature) <= 1e-9) break
    step <- numeric(length(par))
    step[free] <- as.vector(solve(hessian, gradient(par)[free]))
    if (max(abs(step)) < 1e-12) break
    accepted <- FALSE
    for (scale in 2^-(0:12)) {
      candidate <- par - scale * step
      if (all(candidate >= lower & candidate <= upper) &&
          objective(candidate) <= objective(par) + 1e-14) {
        par <- candidate
        accepted <- TRUE
        break
      }
    }
    if (!accepted) break
  }
  score <- gradient(par)
  score[par <= lower + 1e-9 & score > 0] <- 0
  score[par >= upper - 1e-9 & score < 0] <- 0
  list(par = par, objective = -tab$n * objective(par),
       converged = all(is.finite(par)) && max(abs(score)) < 1e-7,
       score = max(abs(score)), iterations = fit$iterations,
       probabilities = nogoldstandard_probabilities(par, tab$patterns)$prob)
}

nogoldstandard_em_start <- function(tab, par, checkpoint = function() {}) {
  k <- tab$k
  positive <- 1L + seq_len(k)
  negative <- 1L + k + seq_len(k)
  # EM supplies a stable starting point; the score-checked optimizer finishes it.
  for (iter in seq_len(200L)) {
    if (iter %% 50L == 1L) checkpoint()
    q1 <- par[positive]
    q0 <- par[negative]
    log_odds <- stats::qlogis(par[1]) + as.vector(
      tab$patterns %*% log(q1 / q0) +
      (1 - tab$patterns) %*% log((1 - q1) / (1 - q0)))
    weight <- stats::plogis(log_odds)
    n1 <- sum(tab$counts * weight)
    n0 <- tab$n - n1
    next_par <- c(n1 / tab$n,
      (colSums(tab$patterns * (tab$counts * weight)) + 1) / (n1 + 1),
      colSums(tab$patterns * (tab$counts * (1 - weight))) / (n0 + 1))
    next_par <- pmin(1 - 1e-12, pmax(1e-12, next_par))
    difference <- max(abs(next_par - par))
    par <- next_par
    if (difference < 1e-8) break
  }
  par
}

nogoldstandard_identification <- function(tab, par) {
  if (is.null(tab$k) || tab$k < 2L) {
    return(list(identified = FALSE, weak = TRUE, ratio = NA_real_, exact_factorization = FALSE))
  }
  z <- nogoldstandard_probabilities(par, tab$patterns)
  singular_values <- svd(z$jacobian, nu = 0, nv = 0)$d
  ratio <- min(singular_values) / max(singular_values)

  # A two-test block and independent remaining tests is generally unidentified.
  # Deterministic boundary cases can be uniquely decomposable but nonregular:
  # use this as a conservative reporting guard, not a proof of global nonuniqueness.
  # It also catches a MAP point displaced away from an unpenalized likelihood ridge.
  empirical <- tab$counts / tab$n
  marginal <- colSums(tab$patterns * empirical)
  factorized <- FALSE
  for (pair in utils::combn(tab$k, 2, simplify = FALSE)) {
    code <- 1L + tab$patterns[, pair[1]] + 2L * tab$patterns[, pair[2]]
    pair_prob <- vapply(1:4, function(i) sum(empirical[code == i]), numeric(1))
    implied <- pair_prob[code]
    for (j in setdiff(seq_len(tab$k), pair)) {
      implied <- implied * ifelse(tab$patterns[, j] == 1, marginal[j], 1 - marginal[j])
    }
    if (max(abs(implied - empirical)) < 1e-12) factorized <- TRUE
  }
  list(identified = tab$k >= 3L && !factorized && is.finite(ratio) && ratio > 1e-8,
       weak = is.finite(ratio) && ratio < 1e-4,
       ratio = ratio, exact_factorization = factorized)
}

nogoldstandard_update_lca <- function(model, tab, fit) {
  par <- fit$par
  k <- tab$k
  model$P <- c(par[1], 1 - par[1])
  for (j in seq_len(k)) {
    q <- c(par[1 + j], par[1 + k + j])
    model$probs[[j]][, 1] <- 1 - q
    model$probs[[j]][, 2] <- q
  }
  expected <- tab$n * fit$probabilities
  observed <- tab$counts
  used <- observed > 0
  model$llik <- sum(observed[used] * log(fit$probabilities[used]))
  model$aic <- -2 * model$llik + 2 * (2 * k + 1)
  model$bic <- -2 * model$llik + log(tab$n) * (2 * k + 1)
  model$Gsq <- 2 * sum(observed[used] * log(observed[used] / expected[used]))
  model$Chisq <- sum((observed - expected)^2 / pmax(expected, .Machine$double.xmin))
  model$probs.se <- model$P.se <- NULL
  model$posterior <- model$predclass <- NULL
  model$predcell <- as.data.frame(tab$patterns[used, , drop = FALSE] + 1)
  names(model$predcell) <- names(model$probs)
  model$predcell$observed <- observed[used]
  model$predcell$expected <- expected[used]
  model
}
