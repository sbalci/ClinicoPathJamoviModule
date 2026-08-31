# Tests for summarydata().
#
# This file had drifted badly from the implementation and every block failed:
#   * it selected "age_normal" from a data frame whose column is "age";
#   * it asserted "Mean of <strong>age</strong> is:", "Min:", "Max:", "Skewness",
#     "Kurtosis" - none of which the backend has ever emitted (the real strings
#     are "<strong>age</strong> (N = ...): Mean ...", "minimum:", "maximum:",
#     "skewness =", "kurtosis =");
#   * expect_s3_class(results, "jmvcore::Results") used a namespaced string that
#     is not a class name;
#   * its all-NA and non-numeric columns were logical/character, which jmvcore
#     rejects at the wrapper, so the backend branches they aimed at were never
#     reached.
# Rewritten against the actual API and output strings.

set.seed(123)
test_data <- data.frame(
    age = rnorm(100, mean = 50, sd = 10),
    biomarker1 = rlnorm(100, meanlog = 2, sdlog = 0.5),
    biomarker2 = c(rnorm(95, mean = 10, sd = 2), 50, 52, 55, 58, 60),
    all_na = as.numeric(rep(NA, 100)),   # numeric, or jmvcore rejects it first
    single_value = rep(5, 100)
)

test_that("summarydata - Basic functionality", {
    results <- summarydata(data = test_data, vars = c("age", "biomarker1"))

    expect_s3_class(results, "summarydataResults")
    expect_true(!is.null(results$text1))
    expect_true(nzchar(as.character(results$text$content)))

    # both selected variables must appear - a silently dropped variable is the
    # failure mode worth catching here
    out <- as.character(results$text$content)
    expect_match(out, "age")
    expect_match(out, "biomarker1")
})

test_that("summarydata - Statistical correctness", {
    results <- summarydata(data = test_data, vars = "age", decimal_places = 3)
    out <- as.character(results$text$content)

    expect_match(out, "<strong>age</strong>")
    expect_match(out, "Mean")
    expect_match(out, "Median:")
    expect_match(out, "minimum:")
    expect_match(out, "maximum:")

    # The printed numbers must be the real ones, at the requested precision.
    expect_match(out, sprintf("Mean %s", formatC(mean(test_data$age), format = "f", digits = 3)),
                 fixed = TRUE)
    expect_match(out, formatC(sd(test_data$age), format = "f", digits = 3), fixed = TRUE)
    expect_match(out, formatC(median(test_data$age), format = "f", digits = 3), fixed = TRUE)
    expect_match(out, sprintf("N = %d", sum(!is.na(test_data$age))))
})

test_that("summarydata - Distribution diagnostics", {
    results <- summarydata(data = test_data, vars = "age", distr = TRUE)
    out <- as.character(results$text$content)

    expect_match(out, "Shapiro-Wilk p-value")
    expect_match(out, "skewness =")
    expect_match(out, "kurtosis =")

    # values must match the moments package, which reports g1 and b2 (normal = 3)
    expect_match(out, sprintf("skewness = %s", round(moments::skewness(test_data$age), 2)),
                 fixed = TRUE)
    expect_match(out, sprintf("kurtosis = %s", round(moments::kurtosis(test_data$age), 2)),
                 fixed = TRUE)
})

test_that("summarydata - Outlier detection", {
    results <- summarydata(data = test_data, vars = "biomarker2", outliers = TRUE)
    outlier_report <- as.character(results$outlierReport$content)

    expect_match(outlier_report, "5 observation(s) flagged", fixed = TRUE)
    # values are now formatted at the user's decimal_places (default 2) so that
    # they match the precision of every other number in the output
    expect_match(outlier_report, "50.00, 52.00, 55.00, 58.00, 60.00", fixed = TRUE)

    # bounds agree with a hand-computed IQR fence
    x <- test_data$biomarker2
    q <- quantile(x, c(0.25, 0.75))
    expect_match(outlier_report,
                 formatC(q[[1]] - 1.5 * diff(q), format = "f", digits = 2), fixed = TRUE)
})

test_that("summarydata - non-numeric and all-missing selections", {
    # jmvcore enforces `permitted: [numeric]` at the wrapper, so a character
    # column never reaches .run(). (The backend keeps its own non-numeric guard
    # as defence in depth; it is not reachable through this path.)
    d <- test_data
    d$non_numeric <- "a"
    expect_error(summarydata(data = d, vars = "non_numeric"), "numeric variable")

    # A numeric all-NA column reaches the backend, which reports one plain-text
    # notice and leaves the data-information panel empty.
    res_na <- summarydata(data = test_data, vars = "all_na")
    expect_match(as.character(res_na$notices$content),
                 "every value in it is missing", fixed = TRUE)
    expect_equal(as.character(res_na$todo$content), "")
    expect_equal(as.character(res_na$text$content), "")
})

test_that("summarydata - a constant variable reports zero variance", {
    results <- summarydata(data = test_data, vars = "single_value", distr = TRUE)
    out <- as.character(results$text$content)

    expect_match(out, "<strong>single_value</strong>")
    expect_match(out, "Mean 5.00")
    # Shapiro-Wilk is undefined without variance; the analysis must say so rather
    # than print NaN
    expect_match(out, "constant")
    expect_false(grepl("NaN", out))
})
