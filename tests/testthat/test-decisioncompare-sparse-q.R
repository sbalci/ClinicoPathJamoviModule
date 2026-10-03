# Independent exhaustive conditional permutation oracle, not the DP recurrence.
dc_q_brute <- function(sums) {
    assignments <- as.matrix(expand.grid(rep(list(1:3), length(sums))))
    matrices <- lapply(seq_len(nrow(assignments)), function(i) {
        z <- matrix(0, length(sums), 3)
        z[cbind(seq_along(sums), assignments[i, ])] <- 1
        z[sums == 2, ] <- 1 - z[sums == 2, , drop = FALSE]
        z
    })
    scores <- vapply(matrices, function(z) sum(colSums(z)^2), numeric(1))
    list(matrices = matrices, scores = scores)
}

dc_q_analysis <- function(m = 3) {
    gold <- rep(c('P', 'N'), each = 60)
    b <- gold; b[seq_len(m)] <- 'N'
    opts <- ClinicoPath:::decisioncompareOptions$new(
        gold='gold', goldPositive='P', test1='a', test1Positive='P',
        test2='b', test2Positive='P', test3='c', test3Positive='P',
        statComp=TRUE, showSummary=TRUE, showReportSentence=TRUE, showExplanations=TRUE)
    a <- ClinicoPath:::decisioncompareClass$new(options=opts,
        data=data.frame(gold=factor(gold), a=factor(gold), b=factor(b), c=factor(b)))
    a$run()
    a
}

test_that('exact Q agrees with exhaustive permutations of mixed row sums', {
    a <- dc_q_analysis()
    exact <- a$.__enclos_env__$private$.cochranExactP
    for (m in 1:5) {
        for (n_two in 0:m) {
            oracle <- dc_q_brute(c(rep(2, n_two), rep(1, m - n_two)))
            # Every attainable score, including ties and both row-sum types.
            idx <- which(!duplicated(oracle$scores))
            got <- vapply(idx, function(i) exact(oracle$matrices[[i]]), numeric(1))
            want <- vapply(idx, function(i) mean(oracle$scores >= oracle$scores[i]), numeric(1))
            expect_equal(got, want, tolerance=1e-12)
            # Conditional rejection probability does not exceed nominal alpha.
            weights <- vapply(idx, function(i) mean(oracle$scores == oracle$scores[i]), numeric(1))
            expect_lte(sum(weights[got <= .05]), .05 + 1e-12)
        }
    }
})

test_that('sparse global inference, narratives and method notes use the exact tail', {
    a <- dc_q_analysis()
    q <- a$results$mcnemarTable$asDF[1, ]
    expect_equal(q$stat, 6)
    expect_equal(q$p, 1/9, tolerance=1e-12)
    expect_true(is.na(q$df))
    expect_match(q$interpretation, 'No significant overall difference', fixed=TRUE)
    for (panel in c('summaryReport', 'reportSentence')) {
        text <- a$results[[panel]]$content
        expect_match(text, 'did not detect a significant difference', fixed=TRUE)
        expect_match(text, 'exact conditional p-value based on 3 informative cases', fixed=TRUE)
    }
    exact <- a$.__enclos_env__$private$.cochranExactP
    z <- rbind(c(1,0,0),c(0,1,1),c(1,0,0))
    expect_equal(exact(z), exact(z[,3:1]), tolerance=1e-12)
    expect_equal(exact(z), exact(1-z), tolerance=1e-12)
    expect_equal(exact(z), exact(rbind(z, c(0,0,0),c(1,1,1))), tolerance=1e-12)
})

test_that('Q method selection counts informative cases and clears notes on rerun', {
    at_limit <- dc_q_analysis(30)
    expect_equal(at_limit$results$mcnemarTable$asDF$p[1] / 3^(-29), 1, tolerance=1e-12)
    expect_true(is.na(at_limit$results$mcnemarTable$asDF$df[1]))
    a <- dc_q_analysis(31)
    expect_equal(a$results$mcnemarTable$asDF$p[1] / pchisq(62,2,lower.tail=FALSE),
                 1, tolerance=1e-12)
    expect_equal(a$results$mcnemarTable$asDF$df[1], 2)
    expect_match(a$results$reportSentence$content, 'chi-square approximation with 31 informative cases', fixed=TRUE)
    private <- a$.__enclos_env__$private
    private$.data$b <- private$.data$a
    private$.data$c <- private$.data$a
    a$run()
    expect_null(a$results$mcnemarTable$notes$cochran_method)
    expect_identical(private$.cochran_method_note, '')
    expect_false(grepl('chi-square approximation with', a$results$reportSentence$content, fixed=TRUE))
})
