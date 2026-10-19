# Regression tests for the waterfall findings of the 2026-10-19 pre-release critical review
# (OncoPath docs/audit/release-review-20261019/review_waterfall.md).

wr2 <- function(d, ...) suppressWarnings(ClinicoPath::waterfall(data = d, ...))
wr2_metric <- function(res, pattern) {
    m <- res$clinicalMetrics$asDF
    m$value[grepl(pattern, m$metric, fixed = TRUE)][1]
}

test_that("WF-1: a supplied category counts a patient with no measured value", {
    d <- data.frame(id = paste0("p", 1:6), v = c(-50, -40, 10, 5, NA, NA),
                    cat = c(NA, NA, NA, NA, "PD", "PD"))
    r <- wr2(d, patientID = "id", responseVar = "v", responseCategoryVar = "cat")
    s <- r$summaryTable$asDF
    expect_equal(s$n[s$category == "PD"], 2)
    # oracle: RECIST counts both early progressors - ORR 2/6, DCR 4/6
    expect_match(wr2_metric(r, "Objective Response Rate (CR+PR)"), "33.3%", fixed = TRUE)
    expect_match(wr2_metric(r, "Disease Control Rate (CR+PR+SD)"), "66.7%", fixed = TRUE)
    # the long-format layout of the same patients gives the same answer
    d2 <- data.frame(id = c("a","a","b","b","c","c","d","d","e","f"), t = c(0,2,0,2,0,2,0,2,0,0),
                     v = c(0,-50, 0,-40, 0,10, 0,5, 0,0), cat = c(rep(NA,8),"PD","PD"))
    r2 <- wr2(d2, patientID = "id", responseVar = "v", timeVar = "t", responseCategoryVar = "cat")
    expect_identical(wr2_metric(r, "Disease Control Rate (CR+PR+SD)"),
                     wr2_metric(r2, "Disease Control Rate (CR+PR+SD)"))
})

test_that("WF-2: a supplied NE is not evaluable, not a threshold responder", {
    d <- data.frame(id = paste0("p", 1:5), v = c(-50, -45, 0, 10, 30), cat = c("NE", NA, NA, NA, NA))
    r <- wr2(d, patientID = "id", responseVar = "v", responseCategoryVar = "cat")
    expect_match(wr2_metric(r, "Objective Response Rate (CR+PR)"), "25.0%", fixed = TRUE)   # 1/4
    expect_false(grepl("OVERRIDE IGNORED", paste(r$notices$content, collapse = " "), fixed = TRUE))
})

test_that("WF-4: an all-patients DCR row and an exclusion count beside the categories", {
    d <- data.frame(id = paste0("p", 1:6), v = c(-50, -40, 10, 5, NA, NA))
    r <- wr2(d, patientID = "id", responseVar = "v")
    expect_match(wr2_metric(r, "Disease Control Rate, all 6 patients"), "66.7%", fixed = TRUE)
    notes <- vapply(r$summaryTable$notes, function(n) n$note, "")
    expect_true(any(grepl("2 further patient(s) could not be analysed", notes, fixed = TRUE)))
})

test_that("WF-5: raw sizes entered as percentage changes stop the run", {
    d <- data.frame(id = rep(paste0("p", 1:3), each = 2), t = rep(c(0, 2), 3), v = c(50, 20, 30, 40, 20, 0))
    expect_error(ClinicoPath::waterfall(data = d, patientID = "id", responseVar = "v", timeVar = "t"),
                 "these look like raw tumour measurements", fixed = TRUE)
})

test_that("WF-3: the ongoing flag documentation matches the code (last value)", {
    a <- yaml::read_yaml(test_path("..", "..", "jamovi", "waterfall.a.yaml"))
    opt <- Filter(function(o) identical(o$name, "ongoingVar"), a$options)[[1]]
    expect_match(opt$description$R, "latest recorded value", fixed = TRUE)
})
