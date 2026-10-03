.paReaderRatio <- function(a, b) if (b > 0) a / b else NA_real_

.paReaderAccuracy <- function(x, gold) {
  ok <- !is.na(x) & !is.na(gold)
  x <- x[ok]; gold <- gold[ok]
  counts <- c(tp = sum(x & gold), fn = sum(!x & gold),
              tn = sum(!x & !gold), fp = sum(x & !gold))
  se <- .paReaderRatio(counts[["tp"]], sum(gold))
  sp <- .paReaderRatio(counts[["tn"]], sum(!gold))
  c(n = length(x), counts, sensitivity = se, specificity = sp,
    balanced_accuracy = (se + sp) / 2)
}

.paReaderAgreement <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  a <- sum(x & y); b <- sum(x & !y); c <- sum(!x & y); d <- sum(!x & !y)
  n <- length(x)
  po <- .paReaderRatio(a + d, n)
  pe <- .paReaderRatio((a + b) * (a + c) + (c + d) * (b + d), n^2)
  c(n = n, both_positive = a, first_positive = b, second_positive = c,
    both_negative = d, observed_agreement = po, discordance = .paReaderRatio(b + c, n),
    kappa = if (is.finite(pe) && pe < 1) (po - pe) / (1 - pe) else NA_real_,
    positive_agreement = .paReaderRatio(2 * a, 2 * a + b + c),
    negative_agreement = .paReaderRatio(2 * d, 2 * d + b + c))
}

.paReaderOrdinal <- function(x, y, weight) {
  k <- nlevels(x)
  tab <- table(x, y)
  n <- sum(tab)
  if (!n || k < 2L) return(NA_real_)
  distance <- abs(outer(seq_len(k), seq_len(k), "-")) / (k - 1)
  w <- switch(weight, equal = 1 - distance, squared = 1 - distance^2, diag(k))
  observed <- sum(w * tab) / n
  expected <- sum(w * outer(rowSums(tab), colSums(tab))) / n^2
  if (expected >= 1) NA_real_ else (observed - expected) / (1 - expected)
}

.paReaderComposite <- function(ba, kappa) {
  if (!is.finite(ba) || !is.finite(kappa) || ba < 0 || ba > 1 || kappa < 0 || kappa > 1) {
    return(NA_real_)
  }
  sqrt(ba * kappa)
}

.paReaderExactCI <- function(success, n, confidence) {
  if (n <= 0) return(c(NA_real_, NA_real_))
  unname(stats::binom.test(success, n, conf.level = confidence)$conf.int[1:2])
}
