test_that("aeplot patient mode computes incidence and builds a table", {
    d <- read.csv(testthat::test_path("..", "..", "data", "aeplot_test_data.csv"))
    expect_error(
        r <- ClinicoPath::aeplot(data = d, inputMode = "patient", subjectID = "SubjectID",
              aeTerm = "AETerm", armVar = "Arm", gradeVar = "Grade", gradeThreshold = 3),
        NA)
    expect_gt(r$freqTable$rowCount, 0)
    # incidence percentages must be within [0, 100]
    ag <- r$freqTable$asDF$allGrade
    expect_true(all(ag >= 0 & ag <= 100))
})

test_that("aeplot summary mode accepts pre-computed percentages", {
    s <- data.frame(AE = c("Fatigue", "Nausea"), tAll = c(40, 25),
                    tHi = c(8, 4), cAll = c(30, 20), cHi = c(5, 3))
    expect_error(
        r <- ClinicoPath::aeplot(data = s, inputMode = "summary", aeTermS = "AE",
              testAll = "tAll", testHigh = "tHi", controlAll = "cAll", controlHigh = "cHi"),
        NA)
    expect_equal(r$freqTable$rowCount, 4)  # 2 terms x 2 arms
})

test_that("aeplot topN limits the number of terms shown", {
    d <- read.csv(testthat::test_path("..", "..", "data", "aeplot_test_data.csv"))
    r <- ClinicoPath::aeplot(data = d, inputMode = "patient", subjectID = "SubjectID",
          aeTerm = "AETerm", gradeVar = "Grade", topN = 3)
    # single arm x 3 terms = 3 rows
    expect_equal(r$freqTable$rowCount, 3)
})
